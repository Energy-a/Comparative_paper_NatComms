
/*----------------------------------------------------------------------------------
 
  Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)
 
 This do-file:
   1) exploits CDD-wet bulb 24 deg
   2) conducts logit regressions for Brazil using 2017 wave
   3) project future ownership rates
   4) run intensive margin regressions: electricity expenditure on climate + covariates

----------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"

* Load data
use "input_data_files\Household.dta", clear
	 
* Select the country
keep if country=="Brazil"

* Keep only those with access to ely
keep if ely_access == 1

* Keep only last wave
keep if year == 2017

* Generate logs
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
gen ln_ely_q=log(ely_q)

* Keep only urban stratums
keep if stratum_groups=="capital" | stratum_groups=="other_urban" 

* Merge data with projections dataframe
merge m:1 country state2 using "input_data_files\Projections.dta"
drop if _merge == 2
drop _merge

* Merge data with regional historical (1986-2005) wet CDD dataframe
merge m:1 country state3 using "input_data_files\CDD_wb_hist.dta"
drop if _merge == 2
drop _merge


/*--------------------------------------------------------------- 

              Extensive margin projections at 2040

---------------------------------------------------------------*/

** Logistic regression of AC on covariates
quietly logit ac c.mean_CDD_wb c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_wb n_members sh_under16 i.ownership_d i.edu_head_2 ///
		i.housing_index_lab age_head i.sex_head i.region3, vce(robust) nolog 

** Predicted probabilities
predict phat0_obs if e(sample)

** Predictions using future data
* First compute percent change
foreach rcp in rcp45 rcp85 {
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {
gen pct_cdd_`rcp'_`ssp' = (mean_CDD_2021_2060_wb_`rcp'_`ssp' - mean_CDD_wb_hist)/mean_CDD_wb_hist
}
}

foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {
gen pct_texp_`ssp' = (GDPpc_mean_2020_2060_`ssp' - GDPpc_2010)/GDPpc_2010
}

* Duplicate variables that we want to replace for projections - climate & also total expenditure
clonevar mean_CDD_c = mean_CDD_wb
clonevar ln_total_exp_usd_2011_c=ln_total_exp_usd_2011

* Compute future AC penetration projections
foreach rcp in rcp45 rcp85 {
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

* Run logit regression
quietly logit ac c.mean_CDD_wb c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_wb n_members sh_under16 i.ownership_d i.edu_head_2 ///
			  i.housing_index_lab age_head i.sex_head i.region3, vce(robust) nolog

* Substitute the new CDD and total exp by multiplying historical one for the regional % change
replace mean_CDD_wb = mean_CDD_wb*(1+pct_cdd_`rcp'_`ssp')
replace ln_total_exp_usd_2011 =log(total_exp_usd_2011*(1+pct_texp_`ssp'))

* Re-fit and compute the new predicted probabilities
predict phat_fut_`rcp'_`ssp' if e(sample) 

* Reset the historical values
replace mean_CDD_wb = mean_CDD_c
replace ln_total_exp_usd_2011 = ln_total_exp_usd_2011_c

}
}

** Find old and new HHs classified as owning an AC (NB: using ROC curve we have seen that we are GOOD at predicting those who have AC)
* Current
gen ac_obs = 0 if phat0_obs != .
replace ac_obs = 1 if phat0_obs > 0.5 & phat0_obs != .

* Future
foreach rcp in rcp45 rcp85 {
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen ac_fut_`rcp'_`ssp' = 0 if phat_fut_`rcp'_`ssp' != .
replace ac_fut_`rcp'_`ssp' = 1 if phat_fut_`rcp'_`ssp' > 0.5 & phat_fut_`rcp'_`ssp' != .

}
}


/*--------------------------------------------------------------- 

              Intensive margin projections at 2040

---------------------------------------------------------------*/

* Run regression of log ely consumption on linear CDD-wb and total exp
quietly reg ln_ely_q c.mean_CDD_wb c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_wb n_members sh_under16 i.ownership_d i.edu_head_2  ///
			i.housing_index_lab age_head i.sex_head i.region3, vce(robust)

* Find fitted values
predict ely_hat0_q if e(sample)
replace ely_hat0_q = exp(ely_hat0) if e(sample)

** Predictions using future data
foreach rcp in rcp45 rcp85 {
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

* Run regression of log ely quantity on linear CDD-wb and total exp (INT)
quietly reg ln_ely_q c.mean_CDD_wb c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_wb n_members sh_under16 i.ownership_d i.edu_head_2 ///
			i.housing_index_lab age_head i.sex_head i.region3, vce(robust)

* Substitute the new CDD and total exp by multiplying historical one for the regional % change
replace mean_CDD_wb = mean_CDD_wb*(1+pct_cdd_`rcp'_`ssp') 
replace ln_total_exp_usd_2011 =log(total_exp_usd_2011*(1+pct_texp_`ssp')) 

* Find future fitted values
predict ely_hat_fut_int_`rcp'_`ssp'_q if e(sample)
replace ely_hat_fut_int_`rcp'_`ssp'_q = exp(ely_hat_fut_int_`rcp'_`ssp'_q) if e(sample)

* Reset the historical values
replace mean_CDD_wb = mean_CDD_c 
replace ln_total_exp_usd_2011 = ln_total_exp_usd_2011_c 

* Compute growth in electricity quantity in both intensive
gen ely_ratio_int_`rcp'_`ssp'_q = ely_hat_fut_int_`rcp'_`ssp'_q/ely_hat0_q 
gen ely_gro_int_`rcp'_`ssp'_q = (ely_hat_fut_int_`rcp'_`ssp'_q/ely_hat0_q)-1 

}
}

** Create income decile
gsort total_exp_usd_2011
xtile dec_inc = total_exp_usd_2011 [pweight= weight], nq(10)
	
	   
** Save
* Keep variables for next steps
keep hhid year country* state* district* weight ac ac_fut_* mean_CDD_wb mean_CDD_db exp_cap_usd_2011 ln_total_exp_usd_2011 ely_q ely_hat_fut* ely_gro_* dec_inc pct_texp_*

* Save
save "output_data_files\projections\second_stage_BRA_wb.dta", replace
	   
************ STOP ************
