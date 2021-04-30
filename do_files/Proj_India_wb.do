
/*----------------------------------------------------------------------------------
 
 This do-file:
   1) exploits CDD-wet bulb
   2) conducts logit regressions for India using 2011-2012 wave
   3) project future ownership rates
   4) run intensive margin regressions: electricity expenditure on climate + covariates

----------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"

* Load data
use "input_data_files\Household.dta", clear

* Keep only vars of interest to speed up computations
keep hhid ac fan refrigerator mean_CDD_wb* mean_CDD_db* ln_exp_cap_usd exp_cap_usd_2011 exp_cap_usd_2010 ely_exp_usd_2011 ely_exp_usd_2010 urban n_members sh_under16 ///
     ownership edu_head_2 occupation_head age_head sex_head state* district* year country* wave ely_access housing_index* ///
	 ownership_d walls_d roof_d lighting_d water_d ely_q total* weight ely_price mean_CDD_1970_2010_*
	 
* Select the country
keep if country=="India"
drop if occupation_head == 1 & country == "India" // 64 obs: empty category in the regression -> we drop it otherwise it does not allow for computing the marginal effect of occupation.
	 	 
* Keep only those with access to ely
keep if ely_access == 1

* Keep only last wave
drop if year == 2005

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
gen ln_exp_cap_usd_2011= log(exp_cap_usd_2011)
gen ln_ely_q=log(ely_q)

* Merge data with projections dataframe
merge m:m hhid country state state_district using "input_data_files\Projections.dta"
drop if _merge == 2
drop _merge

* Merge data with regional historical (1986-2005) wet CDD dataframe
merge m:1 country state3 using "input_data_files\HH_4countries_CDD_wb_hist.dta"
drop if _merge == 2
drop _merge


/*--------------------------------------------------------------- 

              Extensive margin projections at 2040

---------------------------------------------------------------*/

** Logistic regression of AC on covariates
quietly logit ac c.mean_CDD_wb c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_wb i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
		age_head i.sex_head i.state, vce(cluster state_district) nolog 

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
quietly logit ac c.mean_CDD_wb c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_wb i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
			  age_head i.sex_head i.state, vce(cluster state_district) nolog

* Substitute the new CDD and total exp by multiplying historical one for the regional % change
replace mean_CDD_wb = mean_CDD_wb*(1+pct_cdd_`rcp'_`ssp') 
replace ln_total_exp_usd_2011 =log(total_exp_usd_2011*(1+pct_texp_`ssp')) 

* Re-fit and compute the new predicted probabilities
predict phat_fut_`rcp'_`ssp' if e(sample)

* Reset the historical CDD
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
quietly reg ln_ely_q c.mean_CDD_wb c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_wb i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head  ///
			age_head i.sex_head i.state, vce(cluster state_district)

estimates store IND_Full
outreg2 IND_Full using "output_table_figures\IND_ely_wb", excel ctitle(IND Full) replace 

* Find fitted values
predict ely_hat0_q if e(sample)
replace ely_hat0_q = exp(ely_hat0_q) if e(sample)

** Predictions using future data
foreach rcp in rcp45 rcp85 {
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

** Run regression of log ely quantity on linear CDD-wb and total exp (INT)
quietly reg ln_ely_q c.mean_CDD_wb c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_wb i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head  ///
			age_head i.sex_head i.state, vce(cluster state_district)
	
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
xtile dec_inc=total_exp_usd_2011 [pweight= weight], nq(10)


*** Export
** Growth
* Ely_q - RCP 8.5	  
sutex2 ely_gro_int_rcp85_SSP1_q ely_gro_int_rcp85_SSP2_q ely_gro_int_rcp85_SSP3_q ely_gro_int_rcp85_SSP4_q ely_gro_int_rcp85_SSP5_q, ///
	   replace saving("output_table_figures\IND_ely_q_intensive_extensive_rcp85_wb.tex") ///
	   minmax percentiles(25 50 75)
	   
* Ely_q - RCP 4.5
sutex2 ely_gro_int_rcp45_SSP1_q ely_gro_int_rcp45_SSP2_q ely_gro_int_rcp45_SSP3_q ely_gro_int_rcp45_SSP4_q ely_gro_int_rcp45_SSP5_q, ///
	   replace saving("output_table_figures\IND_ely_q_intensive_extensive_rcp45_wb.tex") ///
	   minmax percentiles(25 50 75)

** Save
* Keep variables for next steps
keep hhid year country* state* district* weight ac ac_fut_* mean_CDD_wb mean_CDD_db ln_exp_cap_usd_2011 exp_cap_usd_2011 ln_total_exp_usd_2011 ely_q ely_hat_fut* ely_gro_* dec_inc pct_*

* Save
save "output_data_files\projections\second_stage_IND_wb.dta", replace

************ STOP ************
