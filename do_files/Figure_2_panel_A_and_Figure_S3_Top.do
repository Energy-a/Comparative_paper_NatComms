
/*-------------------------------------------------------------------------------------------------------

 Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)

This do-file:
   1) Save marginal effects of CDD by expenditure level for AC, fan and refrigerator, and for all countries 
   2) marginal effects are used create in R both Figure 2 - Panel A (Main) and Figure S3 - Top (SI)
   3) we use both waves
   4) the x-axis is CDD wet-bulb (or dry-bulb)
   5) the y-axis is predicted probabilities
   
-------------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver"


* Load data
use "input_data_files\Household.dta", clear

/*--------------------------------------------------------------- 

    M. Effects of CDD wrt expenditure from logit regressions

---------------------------------------------------------------*/

**** REGRESSION: POF - BRAZIL

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


** Regression + save margins
foreach app in ac fan refrigerator {
foreach y in wb db {

* Run logit regression
quietly logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 n_members sh_under16 i.ownership_d i.edu_head_2 ///
			  i.housing_index_lab age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog
			  
* Marginal effect CDD by expenditure level
margins, dydx(mean_CDD_`y') at(c.ln_total_exp_usd_2011=(1(0.5)12)) saving("output_data_files\Figure_2\POF_`app'_CDD_`y'_exp", replace) // APE of mean_CDD_wb on pAC by log exp

}
}


**** REGRESSION: NSS - INDIA

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
 

** Regression + save margins
foreach app in ac fan refrigerator {
foreach y in wb db {

* Run logit regression
quietly logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
			  age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog
			  
* Marginal effect CDD by expenditure level
margins, dydx(mean_CDD_`y') at(c.ln_total_exp_usd_2011=(1(0.5)12)) saving("output_data_files\Figure_2\NSS_`app'_CDD_`y'_exp", replace) // APE of mean_CDD_wb on pAC by log exp

}
}


**** REGRESSION: ENIGH - MEXICO

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


** Regression + save margins
foreach app in ac fan refrigerator {
foreach y in wb db {

* Run logit regression
quietly logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog
			  
* Marginal effect CDD by expenditure level
margins, dydx(mean_CDD_`y') at(c.ln_total_exp_usd_2011=(1(0.5)12)) saving("output_data_files\Figure_2\ENIGH_`app'_CDD_`y'_exp", replace) // APE of mean_CDD_wb on pAC by log exp

}
}


**** Create database for plotting in R

foreach app in ac fan refrigerator {
foreach y in wb db {

use "output_data_files\Figure_2\ENIGH_`app'_CDD_`y'_exp", clear
gen country = "Mexico"

append using "output_data_files\Figure_2\NSS_`app'_CDD_`y'_exp"
replace country = "India" if country == ""

append using "output_data_files\Figure_2\POF_`app'_CDD_`y'_exp"
replace country = "Brazil" if country == ""

keep _margin _at2 _ci_lb _ci_ub country
rename _margin margin
rename _ci_lb ci_lb
rename _ci_ub ci_ub
rename _at2 ln_total_exp

* Save
tempfile `app'_`y'
save ``app'_`y'', replace

}
}

* Append
foreach y in wb db {

use "`ac_`y''"
gen appl = "AC"
append using "`fan_`y''"
replace appl = "FAN" if appl == ""
append using "`refrigerator_`y''"
replace appl = "REF" if appl == ""

* Save
save "output_data_files\Figure_2\CDD_`y'_exp", replace

}
 
