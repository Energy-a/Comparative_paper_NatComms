
/*----------------------------------------------------------------------------------
 
 This do-file:
   1) run the standardised logit regressions (TABLE 1)
   2) Save marginal effects

----------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"


* Load data
use "input_data_files\Household.dta", clear


/*--------------------------------------------------------------- 

              Standardised logit regressions for:
			  
			  1) BRA 08-17, 
			  2) IND 05/06-11/12 
			  3) IDN 12-17
			  4) MEX 12-16

---------------------------------------------------------------*/

** REGRESSION 1: POF - BRAZIL

foreach y in wb db {
foreach app in ac fan refrigerator {

* Load data
use "input_data_files\Household.dta", clear

* Keep only vars of interest to speed up computations
keep hhid year wave country* state* district* stratum* ac fan refrigerator mean_CDD_wb* mean_CDD_db* ln_exp_cap_usd exp_cap_usd_2011 exp_cap_usd_2010 /// 
	 ely_exp_usd_2011 ely_exp_usd_2010 urban n_members sh_under16 edu_head_2 occupation_head age_head sex_head ely_access housing_index* ///
	 ownership_d walls_d roof_d lighting_d water_d ely_q total*
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only Brazil
keep if country == "Brazil"
drop if year == 2002

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

* keep only urban stratum
keep if stratum_groups=="capital" | stratum_groups=="other_urban" 

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 

* Standardise vars
logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 n_members sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog
	  
foreach x of varlist mean_CDD_`y' ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	  
		  
* Run standardised regressions		  
logit `app' c.std_mean_CDD_`y' c.std_ln_total_exp_usd_2011 c.std_mean_CDD_`y'#c.std_ln_total_exp_usd_2011 std_n_members std_sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab std_age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog

* Compute marginal effects
estpost margins, dydx(std_mean_CDD_wb std_ln_total_exp_usd_2011) atmeans
eststo pof_`app'_`y'

}
}


** REGRESSION 2: NSS - INDIA

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
drop if occupation_head == 1 & country == "India" // 64 obs: empty category in the regression -> we drop it otherwise it does not allow for computing the marginal effect of occupation.

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 

* Standardise vars
logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog

foreach x of varlist mean_CDD_`y' ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	  
		  
* Total effect
logit `app' c.std_mean_CDD_`y' c.std_ln_total_exp_usd_2011 c.std_mean_CDD_`y'#c.std_ln_total_exp_usd_2011 i.urban std_n_members std_sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      std_age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog
	  
* Compute marginal effects
estpost margins, dydx(std_mean_CDD_wb std_ln_total_exp_usd_2011) atmeans
eststo nss`app'_`y' 


}
}


* REGRESSION 3: SUSENAS - INDONESIA

foreach y in wb db {
foreach app in ac refrigerator {

* Load data
use "input_data_files\Household.dta", clear

* Keep only vars of interest to speed up computations
keep hhid year wave country* state* district* stratum* ac fan refrigerator mean_CDD_wb* mean_CDD_db* ln_exp_cap_usd exp_cap_usd_2011 exp_cap_usd_2010 /// 
	 ely_exp_usd_2011 ely_exp_usd_2010 urban n_members sh_under16 ownership edu_head_2 occupation_head age_head sex_head ely_access housing_index* ///
	 ownership_d walls_d roof_d lighting_d water_d ely_q total*
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only Indonesia
keep if country == "Indonesia"
drop if year == 2004

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 

* Standardise vars
logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 ///
       i.housing_index_lab age_head i.sex_head i.state i.year if country == "Indonesia", vce(cluster state_district) nolog

foreach x of varlist mean_CDD_`y' ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	 

* Total effect
logit `app' c.std_mean_CDD_`y' c.std_ln_total_exp_usd_2011 c.std_mean_CDD_`y'#c.std_ln_total_exp_usd_2011 i.urban std_n_members std_sh_under16 i.ownership_d i.edu_head_2 ///
       i.housing_index_lab std_age_head i.sex_head i.state i.year if country == "Indonesia", vce(cluster state_district) nolog
	   
* Compute marginal effects
estpost margins, dydx(std_mean_CDD_wb std_ln_total_exp_usd_2011) atmeans
eststo susenas_`app'_`y'

}
}


* REGRESSION 4: ENIGH - MEXICO

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

* Standardise vars
logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog

foreach x of varlist mean_CDD_`y' ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	  

* Total effect
logit `app' c.std_mean_CDD_`y' c.std_ln_total_exp_usd_2011 c.std_mean_CDD_`y'#c.std_ln_total_exp_usd_2011 i.urban std_n_members std_sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab std_age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog
	   
* Compute marginal effects
estpost margins, dydx(std_mean_CDD_wb std_ln_total_exp_usd_2011) atmeans
eststo enigh_`app'_`y'

}
}

* Export
esttab pof_*_wb enigh_*_wb nss_*_wb susenas_*_wb using "output_table_figures\Table_1\std_reg.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N, fmt(0) labels("Obs.")) se ///
title("Total Marginal Effects for CDDs wet-bulbs and total expenditure from standardized logit models" \label{std_reg}) ///
mtitles("AC" "FAN" "REF" "AC" "FAN" "REF" "AC" "FAN" "REF" "AC" "REF") ///
addnotes("Clustered standard errors at district level for MEX, IDN, and IND, and robust standard errors for Brazil in parentheses. State- and year-fixed effects for MEX, IDN, and IND and region- and year-fixed effect for BRA. ***p<0.001; **p<0.05; *p<0.1.") ///
indicate("State FE = *.state" "Macro-Region FE = *.region3" "Time FE = *.year") ///
substitute(\begin{table}[htbp]\centering \begin{table}[htbp]\centering\footnotesize{ \end{tabular} \end{tabular}})

*********** STOP ***********
