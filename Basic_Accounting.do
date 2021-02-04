**************************************************************************************
*** Intro:  This do-file generates an income accounting exercise using PWT data 
* STEG Lecture 2 Julieta Caunedo
***************************************************************************************

*** Housekeeping

clear all
capture log close
set more off
global root         = "E:\Dropbox\Teaching\STEG_Dev_accounting"
global workingdata  = "$root/Workingdata"
global tables       = "$root/Tables"
global figures      = "$root/Figures"
global dofiles      = "$root/Dofiles"


***************************************************************************************
*** Read PWT Data
***************************************************************************************

use "$workingdata/pwt_10_dload_02032021.dta", clear

************************
**relevant variables, replication Hall \& Jones '99 (IMPORTANT: no adjustment for natural resources!)

* current PPP (in mil 2017US$)

* Output: cgdpo
* Human Capital (per worker): hc
* Labor Inputs/Workers: emp
* Capital stock: cn
************************
**parameters of interest
***********************
*capital share H&Jones
local alpha=0.33
*relevant year of study
local year_base=2017
**************************

*********Initializing variables************
g output_per_worker=ln(cgdpo/(emp*avh))

g capital_output_ratio=ln(cn/cgdpo)*`alpha'/(1-`alpha')

g human_capital=ln(hc)

g tfp_resid=output_per_worker-capital_output_ratio-human_capital

g capital_output_ratio_sh=(1-labsh)/(labsh)*ln(cn/cgdpo)

g tfp_resid_labsh=output_per_worker-capital_output_ratio_sh-human_capital

g us_level=cn if countrycode=="USA"
bys year: egen us_cap_level=mean(cn)
g capserv_output_ratio=(us_cap_level*ck/cgdpo)^(`alpha'/(1-`alpha'))

************************
**normalized variables, USA=1
************************
foreach i in output_per_worker capital_output_ratio capital_output_ratio_sh capserv_output_ratio human_capital tfp_resid tfp_resid_labsh{
replace `i'=exp(`i')
g output_=`i' if countrycode=="USA" 
bys year: egen output_norm=mean(output_)

g norm_`i'=`i'/output_norm
drop output_ output_norm
}

sort output_per_worker

edit countrycode country year norm_* if year==`year_base'

*10th percentile
replace norm_capital_output_ratio_sh=. if norm_capital_output_ratio_sh>8



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

sort output_per_worker

edit countrycode country year norm_* share_due_to_TFP if year==`year_base'

tw (scatter share_due_to_TFP norm_output_per_worker  if year==`year_base') (lfit share_due_to_TFP norm_output_per_worker  if year==`year_base'), xtitle("Output per worker `year_base', PPP current 2017 US$") ytitle("Share due to TFP") legend(off)

/*
tw (scatter norm_tfp_resid norm_output_per_worker  if year==`year_base') (lfit norm_tfp_resid norm_output_per_worker  if year==`year_base'), xtitle("Output per worker `year_base', PPP current 2017 US$") ytitle("Total Factor Productivity") legend(off)

tw (scatter norm_capital_output_ratio norm_output_per_worker  if year==`year_base') (lfit norm_capital_output_ratio norm_output_per_worker  if year==`year_base'), xtitle("Output per worker `year_base', PPP current 2017 US$") ytitle("Capital-output ratio") legend(off)

tw (scatter norm_capital_output_ratio_sh norm_output_per_worker  if year==`year_base') (lfit norm_capital_output_ratio_sh norm_output_per_worker  if year==`year_base'), xtitle("Output per worker `year_base', PPP current 2017 US$") ytitle("Capital-output ratio, share PWT") legend(off)

tw (scatter norm_human_capital norm_output_per_worker  if year==`year_base') (lfit norm_human_capital norm_output_per_worker  if year==`year_base'), xtitle("Output per worker `year_base', PPP current 2017 US$") ytitle("Human capital") legend(off)
*/
bys year: egen median_contrib_tfp=median(share_due_to_TFP) 

*********************************
* Alternative measure of TFP: ctfp
*****************************

g contrib_cTFP=1/ctfp
g share_due_to_cTFP=contrib_cTFP/overall_diff
bys year: egen median_contrib_ctfp=median(share_due_to_cTFP) 

tw (scatter median_contrib_tfp year) (scatter median_contrib_ctfp year)

*********************************
* Contribution of capital, Jones measures, issue: not additive.
*********************************
g share_due_to_hcap=contrib_labor/(contrib_labor+(contrib_TFP*contrib_capital))
bys year: egen median_contrib_hcap=median(share_due_to_hcap) 

g share_due_to_cap=contrib_capital/(contrib_capital+(contrib_TFP*contrib_labor))
bys year: egen median_contrib_cap=median(share_due_to_cap) 

tw (scatter median_contrib_tfp year) (scatter median_contrib_cap year) (scatter median_contrib_hcap year)


tw (scatter share_due_to_TFP year if countrycode=="CAN") (scatter share_due_to_cap year if countrycode=="CAN") (scatter share_due_to_hcap year if countrycode=="CAN")

tw (scatter share_due_to_TFP year if countrycode=="DEU") (scatter share_due_to_cap year if countrycode=="DEU") (scatter share_due_to_hcap year if countrycode=="DEU")


g share_due_to_capserv=contrib_capital_serv/(contrib_capital_serv+(contrib_TFP*contrib_labor))
bys year: egen median_contrib_capserv=median(share_due_to_capserv) 
tw (scatter median_contrib_capserv year)  (scatter median_contrib_cap year) 

******************
* Caselli measures of the contribution
******************
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

*


*