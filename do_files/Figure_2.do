
/*-------------------------------------------------------------------------------------------------------

This do-file:
   1) Save predictive probabilities for each appliances and countries to create plots in R
   
-------------------------------------------------------------------------------------------------------*/

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


* Keep only vars of interest to speed up computations
keep hhid year wave country* state* district* stratum* ac fan refrigerator mean_CDD_wb* mean_CDD_db* ln_exp_cap_usd exp_cap_usd_2011 exp_cap_usd_2010 /// 
	 ely_exp_usd_2011 ely_exp_usd_2010 urban n_members sh_under16 ownership edu_head_2 occupation_head age_head sex_head ely_access housing_index* ///
	 ownership_d walls_d roof_d lighting_d water_d ely_q total*

	 
* Only who has electricity access
keep if ely_access == 1


* Keep only POF 2008 and 2017
keep if country == "Brazil"
drop if year == 2002


* keep only urban strata
keep if stratum_groups=="capital" | stratum_groups=="other_urban"  


* Generate macro-region variable
gen region=""
replace region="North" if state3=="Acre"
replace region="North" if state3=="Amapá"
replace region="North" if state3=="Amazonas"
replace region="North" if state3=="Pará"
replace region="North" if state3=="Rondônia"
replace region="North" if state3=="Roraima"
replace region="North" if state3=="Tocantins"

replace region="Northeast" if state3=="Alagoas"
replace region="Northeast" if state3=="Bahia"
replace region="Northeast" if state3=="Ceará"
replace region="Northeast" if state3=="Maranhão"
replace region="Northeast" if state3=="Pernambuco"
replace region="Northeast" if state3=="Paraíba"
replace region="Northeast" if state3=="Piauí"
replace region="Northeast" if state3=="Rio Grande do Norte"
replace region="Northeast" if state3=="Sergipe"

replace region="South" if state3=="Paraná"
replace region="South" if state3=="Rio Grande do Sul"
replace region="South" if state3=="Santa Catarina"

replace region="Southeast" if state3=="Espírito Santo"
replace region="Southeast" if state3=="Minas Gerais"
replace region="Southeast" if state3=="Rio de Janeiro"
replace region="Southeast" if state3=="São Paulo"

replace region="Midwest" if state3=="Distrito Federal"
replace region="Midwest" if state3=="Goiás"
replace region="Midwest" if state3=="Mato Grosso"
replace region="Midwest" if state3=="Mato Grosso do Sul"

encode region, gen(region3)


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

* Keep only vars of interest to speed up computations
keep hhid year wave country* state* district* stratum* ac fan refrigerator mean_CDD_wb* mean_CDD_db* ln_exp_cap_usd exp_cap_usd_2011 exp_cap_usd_2010 /// 
	 ely_exp_usd_2011 ely_exp_usd_2010 urban n_members sh_under16 ownership edu_head_2 occupation_head age_head sex_head ely_access housing_index* ///
	 ownership_d walls_d roof_d lighting_d water_d ely_q total*
	
	
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


* Keep only vars of interest to speed up computations
keep hhid year wave country* state* district* stratum* ac fan refrigerator mean_CDD_wb* mean_CDD_db* ln_exp_cap_usd exp_cap_usd_2011 exp_cap_usd_2010 /// 
	 ely_exp_usd_2011 ely_exp_usd_2010 urban n_members sh_under16 ownership edu_head_2 occupation_head age_head sex_head ely_access housing_index* ///
	 ownership_d walls_d roof_d lighting_d water_d ely_q total*
	 
	 
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
