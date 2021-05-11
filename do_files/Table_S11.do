
/*----------------------------------------------------------------------------------------------------------

 Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)

 This do-file:
   1) conducts logit regressions for each country separately using: either two waves or last wave or only
	  CDD and expenditure as covariates
   2) AC is the only appliance analysed through regressions, using either wet
   3) save results 
   
----------------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"

/*---------------------------------------------------------------

		Different speficiation for AC ownership regression

---------------------------------------------------------------*/


** REGRESSION 1: POF - BRAZIL

foreach y in wb db {

* Load data
use "input_data_files\Household.dta", clear
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only Brazil
keep if country == "Brazil"
drop if year == 2002

* keep only urban stratum
keep if stratum_groups=="capital" | stratum_groups=="other_urban" 

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 

* Total effect - last wave - no covariates
quietly logit ac c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.region3 if country == "Brazil" & year == 2017, robust nolog

quietly margins, dydx(mean_CDD_`y' ln_total_exp_usd_2011) atmeans post
eststo pof_`y'

* Total effect - last wave - full model	  
quietly logit ac c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 n_members sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab age_head i.sex_head i.region3 if country == "Brazil" & year == 2017, robust nolog	  

quietly margins, dydx(mean_CDD_`y' ln_total_exp_usd_2011) atmeans post
eststo pof_`y'_last

* Total effect - two waves - full model	  
quietly logit ac c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 n_members sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog
	  
quietly margins, dydx(mean_CDD_`y' ln_total_exp_usd_2011) atmeans post
eststo pof_`y'_cov

}



** REGRESSION 2: NSS - INDIA

foreach y in wb db {

* Load data
use "input_data_files\Household.dta", clear
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only India
keep if country == "India"
drop if occupation_head == 1 & country == "India" // 64 obs: empty category in the regression -> we drop it otherwise it does not allow for computing the marginal effect of occupation.

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 

* Total effect - last wave - no covariates
quietly logit ac c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.state if country == "India" & year == 2012, vce(cluster state_district) nolog

quietly margins, dydx(mean_CDD_`y' ln_total_exp_usd_2011) atmeans post	  
eststo nss_`y'

* Total effect - last wave - full model			  
quietly logit ac c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      age_head i.sex_head i.state if country == "India" & year == 2012, vce(cluster state_district) nolog

quietly margins, dydx(mean_CDD_`y' ln_total_exp_usd_2011) atmeans post	  
eststo nss_`y'_last
		  
* Total effect - two waves - full model			  
quietly logit ac c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog

quietly margins, dydx(mean_CDD_`y' ln_total_exp_usd_2011) atmeans post	  
eststo nss_`y'_cov

}


* REGRESSION 4: ENIGH - MEXICO

foreach y in wb db {

* Load data
use "input_data_files\Household.dta", clear
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only Mexico
keep if country == "Mexico"
drop if year == 2004

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 

* Total effect - last wave - no covariates
quietly logit ac c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.state if country == "Mexico" & year == 2016, vce(cluster state_district) nolog

quietly margins, dydx(mean_CDD_`y' ln_total_exp_usd_2011) atmeans post	  
eststo enigh_`y'

* Total effect - last wave - full model	
quietly logit ac c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab age_head i.sex_head i.state if country == "Mexico" & year == 2016, vce(cluster state_district) nolog

quietly margins, dydx(mean_CDD_`y' ln_total_exp_usd_2011) atmeans post	  
eststo enigh_`y'_last

* Total effect - two waves - full model			  
quietly logit ac c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog

quietly margins, dydx(mean_CDD_`y' ln_total_exp_usd_2011) atmeans post	  
eststo enigh_`y'_cov

}


** Table S11
esttab pof_wb pof_wb_last pof_wb_cov enigh_wb enigh_wb_last enigh_wb_cov nss_wb nss_wb_last nss_wb_cov ///
using "output_table_figures\Supplementary_Info\table_S11.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N, fmt(0) labels("Obs.")) se ///
title("Marginal Effects for Air-conditioning adoption from logit models using different specification - wet bulb") ///
mtitles("AC-Brazil" "AC-Brazil" "AC-Brazil" "AC-Mexico" "AC-Mexico" "AC-Mexico" "AC-India" "AC-India" "AC-India") ///
addnotes("Clustered standard errors at district level for MEX, IDN, and IND, and robust standard errors for Brazil in parentheses. ***p<0.001; **p<0.05; *p<0.1.") 
