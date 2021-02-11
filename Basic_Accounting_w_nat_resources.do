**************************************************************************************
*** Intro:  This do-file generates standard income accounting results using PWT 10.0 data 
* See lecture notes for details on the data construction and outcomes.
* STEG Lecture 2 Julieta Caunedo
* V1 February 11/2021
***************************************************************************************

*** Housekeeping

clear all
capture log close
set more off
global root         = "/Users/jdc364admin/Dropbox/Teaching/STEG_Dev_accounting"
global root         = "E:/Dropbox/Teaching/STEG_Dev_accounting"
global workingdata  = "$root/Workingdata"
global tables       = "$root/Tables"
global figures      = "$root/Figures"
global dofiles      = "$root/Dofiles"


***************************************************************************************
*** Read PWT Data
***************************************************************************************

use "$workingdata/pwt_10_dload_02032021.dta", clear
**natural_resources adjustment
merge 1:1 year countrycode using "$workingdata/natural_resources.dta"
drop if _merge==2




************************
**relevant variables (IMPORTANT: adjustment for natural resources on GDP. PWT capital corresponds to reproducible capital)

* real PPP (in mil 2017US$)

* Output: rgdpo
* Human Capital (per worker): hc
* Labor Inputs/Workers: emp
* Average hours avh
* Capital stock: cn
* Capital denominated at current PPP $ so capital-output ratio computed using cgdpo
* Can use Capital services instead of stocks is desired.

************************
**parameters of interest
***********************
*capital share H&Jones
local alpha=0.33
*relevant year of study
local year_base=2017
*adjustment GDP (assuming one uses the same deflator for GDP and nat_res)
replace rgdpo=(1-natural_res/100)*rgdpo
replace cgdpo=(1-natural_res/100)*cgdpo
**************************


*********Initializing variables************
g output_per_worker=ln(rgdpo/(emp*avh))

g output_per_worker_PPPc=ln(cgdpo/(emp))
g output_per_worker_XRc=ln(cgdpo*pl_gdpo/(emp))

g capital_output_ratio=ln(cn/cgdpo)*`alpha'/(1-`alpha')

g human_capital=ln(hc)

g tfp_resid=output_per_worker-capital_output_ratio-human_capital

g capital_output_ratio_sh=(1-labsh)/(labsh)*ln(cn/cgdpo)

g tfp_resid_labsh=output_per_worker-capital_output_ratio_sh-human_capital

g us_level=cn if countrycode=="USA"
bys year: egen us_cap_level=mean(cn)
g capserv_output_ratio=ln(us_cap_level*ck/cgdpo)*(`alpha'/(1-`alpha'))

************************
**normalized variables, LEVELS USA=1
************************
foreach i in output_per_worker capital_output_ratio capital_output_ratio_sh capserv_output_ratio human_capital tfp_resid tfp_resid_labsh{
replace `i'=exp(`i')
g output_=`i' if countrycode=="USA" 
bys year: egen output_norm=mean(output_)

g norm_`i'=`i'/output_norm
drop output_ output_norm

}

/*
********************************
* Balassa-Samuelson effect, L=employment in the slides
********************************
preserve
keep if year==`year_base'

keep if countrycode=="ARG" | countrycode=="CHN" | countrycode=="FIN"
egen countryid=group(country) 

graph bar output_per_worker_PPPc output_per_worker_XRc, over(country) legend (label(1 "PPP") label(2 "Exchange rates")) ytitle("Log Output per worker `year_base', current 2017 US$ (USA=1)") graphregion(margin(right) fcolor(white) lcolor(white)) plotregion(margin(medlarge))
graph export "$figures/Penn_effect.pdf",as(pdf) replace
su output_per_worker_PPPc,d

su output_per_worker_XRc,d

restore
*/
*****************************
**contributions
*****************************
g overall_diff=1/norm_output_per_worker
g contrib_capital=1/norm_capital_output_ratio
g contrib_capital_sh=1/norm_capital_output_ratio_sh
g contrib_capital_serv=1/norm_capserv_output_ratio
g contrib_labor=1/norm_human_capital
g contrib_TFP=1/norm_tfp_resid

g share_due_to_TFP=contrib_TFP/(contrib_TFP+(contrib_labor*contrib_capital))
replace share_due_to_TFP=. if countrycode=="USA"
sort output_per_worker

/*
*preserve
**Jones uses employment measures, not average hours(were not available back then). Adjust line 57 to replicate slides
*******
keep if year==`year_base'
* generate average contribution full sample
foreach i in output_per_worker capital_output_ratio human_capital tfp_resid{
egen mean_norm`i'=mean(norm_`i')
g mean_contrib`i'=1/mean_norm`i'
}

**Countries in Table , Jones '10.
keep if countrycode=="USA" | countrycode=="HKG" | countrycode=="SGP" | countrycode=="FRA" |countrycode=="DEU" | countrycode=="GBR" |countrycode=="JPN" |countrycode=="KOR" |countrycode=="ARG" |countrycode=="MEX" |countrycode=="BWA" | countrycode=="ZAF" |countrycode=="BRA" |countrycode=="THA" | countrycode=="CHN" | countrycode=="IDN" | countrycode=="IND" | countrycode=="KEN" |countrycode=="MWI" 
edit countrycode country year norm_output_per_worker norm_capital_output_ratio norm_human_capital norm_tfp_resid share_due_to_TFP mean_contrib* mean_norm* if year==`year_base'

keep countrycode country year norm_output_per_worker norm_capital_output_ratio norm_human_capital norm_tfp_resid share_due_to_TFP  mean_contrib* mean_norm*
save "$tables/income_account_basics_Jones_sample.dta" replace
*restore
return
*/

preserve
*Outliers, resources not properly accounted for
drop if countrycode=="MAC" | countrycode=="IRL"

corr share_due_to_TFP norm_output_per_worker if year==`year_base'
local corr0: display %5.4f r(rho)
reg share_due_to_TFP norm_output_per_worker if year==`year_base'
*mat V=e(V)
local std0: display %5.4f _b[norm_output_per_worker]/_se[norm_output_per_worker]

tw (scatter share_due_to_TFP norm_output_per_worker  if year==`year_base', msize(small) mlabel(countrycode) mlabsize(vsmall) mcolor(pink) mlabcolor(black) mlabposition(6)), xscale(log) xticks(0.015 0.031 0.0625 0.125 0.25 0.5 1 2) xlabel(0.015 "1/64" 0.031 "1/32" 0.0625 "1/16" 0.125 "1/8" 0.25 "1/4" 0.5 "1/2" 1 "1" 2 "2")  xtitle("Output per worker `year_base', PPP current 2017 US$ (USA=1)") ytitle("Share due to TFP") legend(off) graphregion(margin(right) fcolor(white) lcolor(white)) plotregion(margin(medlarge)) note(Correlation=`corr0' t-stat=`std0', position(7) ring(0)) legend(off)
graph export "$figures/Income_Accounting_Baseline.pdf",as(pdf) replace

restore


******some other interesting correlations*****
/*
tw (scatter norm_tfp_resid norm_output_per_worker  if year==`year_base') (lfit norm_tfp_resid norm_output_per_worker  if year==`year_base'), xtitle("Output per worker `year_base', PPP current 2017 US$") ytitle("Total Factor Productivity") legend(off)

tw (scatter norm_capital_output_ratio norm_output_per_worker  if year==`year_base') (lfit norm_capital_output_ratio norm_output_per_worker  if year==`year_base'), xtitle("Output per worker `year_base', PPP current 2017 US$") ytitle("Capital-output ratio") legend(off)

tw (scatter norm_capital_output_ratio_sh norm_output_per_worker  if year==`year_base') (lfit norm_capital_output_ratio_sh norm_output_per_worker  if year==`year_base'), xtitle("Output per worker `year_base', PPP current 2017 US$") ytitle("Capital-output ratio, share PWT") legend(off)

tw (scatter norm_human_capital norm_output_per_worker  if year==`year_base') (lfit norm_human_capital norm_output_per_worker  if year==`year_base'), xtitle("Output per worker `year_base', PPP current 2017 US$") ytitle("Human capital") legend(off)
*/

*********************************
* Median Contribution of TFP through time
*********************************
bys year: egen median_contrib_tfp=median(share_due_to_TFP) 

tw (scatter median_contrib_tfp year if year>=1970), xtitle("year") ytitle("Median contribution of TFP") legend(off) graphregion(margin(right) fcolor(white) lcolor(white)) plotregion(margin(medlarge)) 
graph export "$figures/Median_TFP_contribution_time.pdf",as(pdf) replace



*********************************
* Alternative measure of TFP: ctfp
* Torqvinst index
* More volatility, disparities in the 80s and 90s.
*****************************

g contrib_cTFP=1/ctfp
g share_due_to_cTFP=contrib_cTFP/overall_diff
bys year: egen median_contrib_ctfp=median(share_due_to_cTFP) 
tw (scatter median_contrib_tfp year if year>=1970) (scatter median_contrib_ctfp year if year>=1970)



*
*********************************
* Contribution of capital, Jones measures, issue: not additive.
* Here I describe the share of the variation coming from inputs account by each term
*********************************

g share_due_to_hcap=contrib_labor/(contrib_labor+contrib_capital)*(1-share_due_to_TFP)
bys year: egen median_contrib_hcap=median(share_due_to_hcap) 

corr share_due_to_hcap norm_output_per_worker if year==`year_base'
local corr0: display %5.4f r(rho)
reg share_due_to_hcap norm_output_per_worker if year==`year_base'
*mat V=e(V)
local std0: display %5.4f _b[norm_output_per_worker]/_se[norm_output_per_worker]
local year_base=2017
tw (scatter share_due_to_hcap norm_output_per_worker  if year==`year_base', msize(small) mlabel(countrycode) mlabsize(vsmall) mcolor(pink) mlabcolor(black) mlabposition(6)), xscale(log) xticks(0.015 0.031 0.0625 0.125 0.25 0.5 1 2) xlabel(0.015 "1/64" 0.031 "1/32" 0.0625 "1/16" 0.125 "1/8" 0.25 "1/4" 0.5 "1/2" 1 "1" 2 "2")  xtitle("Output per worker `year_base', PPP current 2017 US$ (USA=1)") ytitle("Share due to Human Capital") legend(off) graphregion(margin(right) fcolor(white) lcolor(white)) plotregion(margin(medlarge)) note(Correlation=`corr0' t-stat=`std0', position(7) ring(0)) legend(off)
graph export "$figures/Income_Accounting_HumanCapital.pdf",as(pdf) replace

g share_due_to_cap=contrib_capital/(contrib_capital+contrib_labor)*(1-share_due_to_TFP)
bys year: egen median_contrib_cap=median(share_due_to_cap) 

corr share_due_to_cap norm_output_per_worker if year==`year_base'
local corr0: display %5.4f r(rho)
reg share_due_to_cap norm_output_per_worker if year==`year_base'
*mat V=e(V)
local std0: display %5.4f _b[norm_output_per_worker]/_se[norm_output_per_worker]

tw (scatter share_due_to_cap norm_output_per_worker  if year==`year_base', msize(small) mlabel(countrycode) mlabsize(vsmall) mcolor(pink) mlabcolor(black) mlabposition(6)), xscale(log) xticks(0.015 0.031 0.0625 0.125 0.25 0.5 1 2) xlabel(0.015 "1/64" 0.031 "1/32" 0.0625 "1/16" 0.125 "1/8" 0.25 "1/4" 0.5 "1/2" 1 "1" 2 "2")  xtitle("Output per worker `year_base', PPP current 2017 US$ (USA=1)") ytitle("Share due to Physical Capital") legend(off) graphregion(margin(right) fcolor(white) lcolor(white)) plotregion(margin(medlarge)) note(Correlation=`corr0' t-stat=`std0', position(7) ring(0)) legend(off)
graph export "$figures/Income_Accounting_PhysicalCapital.pdf",as(pdf) replace

*/



tw (scatter median_contrib_tfp year if year>1970) (scatter median_contrib_cap year if year>1970) (scatter median_contrib_hcap year if year>1970), xtitle("year") ytitle("Median contribution of TFP") legend(label(1 "TFP") label(2 "Physical Capital") label( 3 "Human Capital")) graphregion(margin(right) fcolor(white) lcolor(white)) plotregion(margin(medlarge)) 
graph export "$figures/Median_TFP_contribution_time_types.pdf",as(pdf) replace



******************
* Caselli measures of the contribution
******************
preserve
keep if year==2017
foreach i in output_per_worker capital_output_ratio human_capital{
replace `i'=ln(`i')
su `i',d
local var_`i'=r(Var)
}
g output_factor=human_capital+capital_output_ratio
su output_factor,d
local var_output_factor=r(Var)

cap drop success*
gen successcap=`var_capital_output_ratio'/`var_output_per_worker'
su successcap

gen successhcap=`var_human_capital'/`var_output_per_worker'
su successhcap

gen successoutput=`var_output_factor'/`var_output_per_worker'
su successoutput
restore




********************************
* Alternative variables
* Capital services: ck
* Labor share: labsh

/*
*********************************
* Alternative measure of TFP: feeding labor shares
*****************************

g contrib_labshTFP=1/norm_tfp_resid_labsh
g share_due_to_labshTFP=contrib_labshTFP/(contrib_TFP+(contrib_labor*contrib_capital_sh))
bys year: egen median_contrib_labshtfp=median(share_due_to_labshTFP) 

tw (scatter median_contrib_tfp year) (scatter median_contrib_labshtfp year)

