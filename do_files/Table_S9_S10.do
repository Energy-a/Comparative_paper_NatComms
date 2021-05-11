
/*----------------------------------------------------------------------------------
 
  Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)
 
 This do-file:
   1) run the OLS regressions of electricity quantity (kwH) for all countries
   2) only for the last wave
   3) save coefficients results -> Table 9 (SI) and Table 10 (SI)

----------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"


* Load data
use "input_data_files\Household.dta", clear


/*--------------------------------------------------------------- 

              Standardised logit regressions for:
			  
			  1) BRA 17, 
			  2) IND 11/12 
			  3) IDN 17
			  4) MEX 16

---------------------------------------------------------------*/

** REGRESSION 1: POF - BRAZIL

foreach y in wb db {

* Load data
use "input_data_files\Household.dta", clear
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only Brazil
keep if country == "Brazil"
keep if year == 2017

* keep only urban stratum
keep if stratum_groups=="capital" | stratum_groups=="other_urban" 

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 
gen ln_total_exp_usd_2011_CDD_`y' = ln_total_exp_usd_2011*mean_CDD_`y'
gen ln_ely_q=log(ely_q)
		  
* Run standardised regressions		  
quietly reg ln_ely_q c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011_CDD_`y' n_members sh_under16 i.ownership_d i.edu_head_2  ///
			i.housing_index_lab age_head i.sex_head i.region3, vce(robust)
			
* Compute marginal effects
estpost margins, dydx(*) atmeans
eststo pof_ely_`y'

}



** REGRESSION 2: NSS - INDIA

foreach y in wb db {

* Load data
use "input_data_files\Household.dta", clear
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only India
keep if country == "India"
keep if year == 2012
drop if occupation_head == 1 & country == "India" // 64 obs: empty category in the regression -> we drop it otherwise it does not allow for computing the marginal effect of occupation.

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 
gen ln_total_exp_usd_2011_CDD_`y' = ln_total_exp_usd_2011*mean_CDD_`y'
gen ln_ely_q=log(ely_q)

* Run regression of log ely consumption on linear CDD-wb and total exp
quietly reg ln_ely_q c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011_CDD_`y' i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head  ///
			age_head i.sex_head i.state, vce(cluster state_district)

	  
* Compute marginal effects
estpost margins, dydx(*) atmeans
eststo nss_ely_`y' 


}



* REGRESSION 4: ENIGH - MEXICO

foreach y in wb db {

* Load data
use "input_data_files\Household.dta", clear
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only Mexico
keep if country == "Mexico"
keep if year == 2016

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0
gen ln_total_exp_usd_2011_CDD_`y' = ln_total_exp_usd_2011*mean_CDD_`y' 
gen ln_ely_q=log(ely_q)

* Run regression of log ely consumption on linear CDD-wb and total exp
quietly reg ln_ely_q c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.ln_total_exp_usd_2011_CDD_`y' i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head  ///
			i.housing_index_lab age_head i.sex_head i.state, vce(cluster state_district)
	   
* Compute marginal effects
estpost margins, dydx(*) atmeans
eststo enigh_ely_`y'

}


** Table S9
esttab pof_ely_wb nss_ely_wb enigh_ely_wb using "output_table_figures\Supplementary_Info\table_S9.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N r2, fmt(0 3) labels("Obs." "R-squared")) se ///
title("OLS regression models for electricity quantity with state fixed effects using wet-bulb CDDs." \label{ely_reg_wb}) ///
addnotes("Clustered standard errors at district level for MEX and IND, and robust standard errors for Brazil in parentheses. State- and year-fixed effects for MEX and IND, and region- and year-fixed effect for BRA. ***p<0.001; **p<0.05; *p<0.1.") ///
indicate("State FE = *.state" "Macro-Region FE = *.region3") ///
substitute(\begin{table}[htbp]\centering \begin{table}[htbp]\centering\footnotesize{ \end{tabular} \end{tabular}})

** Table S10
esttab pof_ely_db nss_ely_db enigh_ely_db using "output_table_figures\Supplementary_Info\table_S10.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N r2, fmt(0 3) labels("Obs." "R-squared")) se ///
title("OLS regression models for electricity quantity with state fixed effects using dry-bulb CDDs." \label{ely_reg_db}) ///
addnotes("Clustered standard errors at district level for MEX and IND, and robust standard errors for Brazil in parentheses. State- and year-fixed effects for MEX and IND, and region- and year-fixed effect for BRA. ***p<0.001; **p<0.05; *p<0.1.") ///
indicate("State FE = *.state" "Macro-Region FE = *.region3") ///
substitute(\begin{table}[htbp]\centering \begin{table}[htbp]\centering\footnotesize{ \end{tabular} \end{tabular}})
