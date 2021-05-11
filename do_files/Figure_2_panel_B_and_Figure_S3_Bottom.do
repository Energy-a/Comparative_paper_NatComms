
/*-----------------------------------------------------------------------------------------------------------------

 Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)

 This do-file:
   1) save predicted probabilities for each appliances and countries
   2) predicted probabilities are used create in R both Figure 2 - Panel B (Main) and Figure S3 - Bottom (SI)
   3) we use both waves
   4) the x-axis is CDD wet-bulb (or dry-bulb)
   5) the y-axis is predicted probabilities
   
-----------------------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver"


* Load data
use "input_data_files\Household.dta", clear


/*--------------------------------------------------------------- 

         Predicted probabilites from logit regressions

---------------------------------------------------------------*/


**** REGRESSION: POF - BRAZIL

foreach y in wb db {
foreach app in ac fan refrigerator {

* Load data
use "input_data_files\Household.dta", clear

	 
* Only who has electricity access
keep if ely_access == 1


* Keep only POF 2008 and 2017
keep if country == "Brazil"
drop if year == 2002


* keep only urban strata
keep if stratum_groups=="capital" | stratum_groups=="other_urban"  

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0


* Run logit regression
logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_`y' n_members sh_under16 i.ownership i.edu_head_2 ///
      i.housing_index_lab age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog

	  
* Predicted probabilities
margins, at(c.mean_CDD_`y'=(0(125)1250)) saving("output_data_files\Figure_2\POF_`app'_CDD_`y'_pred_prob", replace)


}
}


**** REGRESSION: NSS - INDIA

foreach y in wb db {
foreach app in ac fan refrigerator {

* Load data
use "input_data_files\Household.dta", clear

	
* Only who has electricity access
keep if ely_access == 1


* Keep only India
keep if country == "India"
drop if occupation_head == 1 & country == "India" // 64 obs: empty category in the regression


* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 


* Run logit regression
logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_`y' i.urban n_members sh_under16 i.ownership i.edu_head_2 i.occupation_head ///
      age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog
	  

* Predicted probabilities
margins, at(c.mean_CDD_`y'=(0(125)1250)) saving("output_data_files\Figure_2\NSS_`app'_CDD_`y'_pred_prob", replace)

}
}


**** REGRESSION: ENIGH - MEXICO

foreach y in wb db {
foreach app in ac fan refrigerator {

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


* Run logit regression
logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011#c.mean_CDD_`y' i.urban n_members sh_under16 i.ownership i.edu_head_2 i.occupation_head ///
       i.housing_index_lab age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog
	   

* Predicted probabilities
margins, at(c.mean_CDD_`y'=(0(125)1250)) saving("output_data_files\Figure_2\ENIGH_`app'_CDD_`y'_pred_prob", replace)

}
}


** Create database for plotting in R

foreach y in wb db {

* Mexico
use "output_data_files\Figure_2\ENIGH_ac_CDD_`y'_pred_prob", clear
gen appl = "AC"
append using "output_data_files\Figure_2\ENIGH_fan_CDD_`y'_pred_prob"
replace appl = "FAN" if appl == ""
append using "output_data_files\Figure_2\ENIGH_refrigerator_CDD_`y'_pred_prob"
replace appl = "REF" if appl == ""
gen country = "Mexico"

* India
append using "output_data_files\Figure_2\NSS_ac_CDD_`y'_pred_prob"
replace appl = "AC" if appl == ""
append using "output_data_files\Figure_2\NSS_fan_CDD_`y'_pred_prob"
replace appl = "FAN" if appl == ""
append using "output_data_files\Figure_2\NSS_refrigerator_CDD_`y'_pred_prob"
replace appl = "REF" if appl == ""
replace country = "India" if country == ""

* Brazil
append using "output_data_files\Figure_2\POF_ac_CDD_`y'_pred_prob"
replace appl = "AC" if appl == ""
append using "output_data_files\Figure_2\POF_fan_CDD_`y'_pred_prob"
replace appl = "FAN" if appl == ""
append using "output_data_files\Figure_2\POF_refrigerator_CDD_`y'_pred_prob"
replace appl = "REF" if appl == ""
replace country = "Brazil" if country == ""


* Keep predicted probabilities, CIs and CDD level
keep _margin _at1 _ci_lb _ci_ub country appl
rename _margin margin
rename _ci_lb ci_lb
rename _ci_ub ci_ub
rename _at1 mean_CDD


* Save
save "output_data_files\Figure_2\APPL_CDD_`y'_pred_prob", replace

}
