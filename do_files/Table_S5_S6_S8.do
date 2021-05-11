
/*----------------------------------------------------------------------------------
 
  Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)
 
 This do-file:
   1) run the two-wave logit regression
   2) saves marginal effects
   3) creates Table S5 (SI), S6 (SI) and S8 (SI)
   4) 24 deg wet- and dry- bulb regression

----------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"


* Load data
use "input_data_files\Household.dta", clear


/*--------------------------------------------------------------- 

              Logit regressions for:
			  
			  1) BRA 08-17, 
			  2) IND 05/06-11/12 
			  3) IDN 12-17
			  4) MEX 12-16

---------------------------------------------------------------*/

** REGRESSION 1: POF - BRAZIL

foreach y in wb db {
foreach app in ac fan refrigerator {

* Load 4countries database
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
gen ln_total_exp_usd_2011_CDD_`y' = ln_total_exp_usd_2011*mean_CDD_`y'

* Run logit regression
quietly logit `app' mean_CDD_`y' ln_total_exp_usd_2011 ln_total_exp_usd_2011_CDD_`y' n_members sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog

quietly margins, dydx(*) atmeans post
eststo pof_`app'_`y'

* Total effect		  
quietly logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 n_members sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog

quietly margins, dydx(*) atmeans post
eststo pof_`app'_`y'_tot

}
}


** REGRESSION 2: NSS - INDIA

foreach y in wb db {
foreach app in ac fan refrigerator {

* Load 4countries database
use "input_data_files\Household.dta", clear
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only India
keep if country == "India"
drop if occupation_head == 1 & country == "India" // 64 obs: empty category in the regression -> we drop it otherwise it does not allow for computing the marginal effect of occupation.

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 
gen ln_total_exp_usd_2011_CDD_`y' = ln_total_exp_usd_2011*mean_CDD_`y'

* Run logit regression
quietly logit `app' mean_CDD_`y' ln_total_exp_usd_2011 ln_total_exp_usd_2011_CDD_`y' i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog

quietly margins, dydx(*) atmeans post
eststo nss_`app'_`y' 
		  
* Total effect		  
quietly logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog

quietly margins, dydx(*) atmeans post	  
eststo nss_`app'_`y'_tot

}
}


* REGRESSION 4: ENIGH - MEXICO

foreach y in wb db {
foreach app in ac fan refrigerator {

* Load 4countries database
use "input_data_files\Household.dta", clear
	 
* Only who has electricity access
keep if ely_access == 1

* Keep only Mexico
keep if country == "Mexico"
drop if year == 2004

* Generate logs and interactions
gen ln_total_exp_usd_2011 = log(total_exp_usd_2011)
replace ln_total_exp_usd_2011 = 0 if total_exp_usd_2011 == 0 
gen ln_total_exp_usd_2011_CDD_`y' = ln_total_exp_usd_2011*mean_CDD_`y'

* Run logit regression
quietly logit `app' mean_CDD_`y' ln_total_exp_usd_2011 ln_total_exp_usd_2011_CDD_`y' i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog

quietly margins, dydx(*) atmeans post
eststo enigh_`app'_`y'

* Total effect		  
quietly logit `app' c.mean_CDD_`y' c.ln_total_exp_usd_2011 c.mean_CDD_`y'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog

quietly margins, dydx(*) atmeans post	  
eststo enigh_`app'_`y'_tot

}
}



** Table S5
esttab pof_*_wb enigh_*_wb nss_*_wb using "output_table_figures\Supplementary_Info\table_S5.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N, fmt(0) labels("Obs.")) se ///
title("Marginal Effects from logit models - wet bulb") ///
mtitles("AC-Brazil" "FAN-Brazil" "REF-Brazil" "AC-Mexico" "FAN-Mexico" "REF-Mexico" "AC-India" "FAN-India" "REF-India") ///
addnotes("Clustered standard errors at district level for MEX and IND, and robust standard errors for Brazil in parentheses. State- and year-fixed effects for MEX and IND and region- and year-fixed effect for BRA. ***p<0.001; **p<0.05; *p<0.1.") ///
indicate("State FE = *.state" "Macro-Region FE = *.region3" "Time FE = *.year") 

** Table S6
esttab pof_*_wb_tot enigh_*_wb_tot nss_*_wb_tot using "output_table_figures\Supplementary_Info\table_S6.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N, fmt(0) labels("Obs.")) se ///
title("Total Marginal Effects from logit models - wet bulb") ///
mtitles("AC-Brazil" "FAN-Brazil" "REF-Brazil" "AC-Mexico" "FAN-Mexico" "REF-Mexico" "AC-India" "FAN-India" "REF-India") ///
addnotes("Clustered standard errors at district level for MEX and IND, and robust standard errors for Brazil in parentheses. State- and year-fixed effects for MEX and IND and region- and year-fixed effect for BRA. ***p<0.001; **p<0.05; *p<0.1.") ///
indicate("State FE = *.state" "Macro-Region FE = *.region3" "Time FE = *.year") 

** Table S8
esttab pof_*_db enigh_*_db nss_*_db using "output_table_figures\Supplementary_Info\table_S8.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N, fmt(0) labels("Obs.")) se ///
title("Marginal Effects from logit models - dry bulb")  ///
mtitles("AC-Brazil" "FAN-Brazil" "REF-Brazil" "AC-Mexico" "FAN-Mexico" "REF-Mexico" "AC-India" "FAN-India" "REF-India") ///
addnotes("Clustered standard errors at district level for MEX and IND, and robust standard errors for Brazil in parentheses. State- and year-fixed effects for MEX and IND and region- and year-fixed effect for BRA. ***p<0.001; **p<0.05; *p<0.1.") ///
indicate("State FE = *.state" "Macro-Region FE = *.region3" "Time FE = *.year") 

** Table S8 but with total effect
esttab pof_*_db_tot enigh_*_db_tot nss_*_db_tot using "output_table_figures\Supplementary_Info\table_S8_alt.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N, fmt(0) labels("Obs.")) se ///
title("Total Marginal Effects from logit models - dry bulb") ///
mtitles("AC-Brazil" "FAN-Brazil" "REF-Brazil" "AC-Mexico" "FAN-Mexico" "REF-Mexico" "AC-India" "FAN-India" "REF-India") ///
addnotes("Clustered standard errors at district level for MEX and IND, and robust standard errors for Brazil in parentheses. State- and year-fixed effects for MEX and IND and region- and year-fixed effect for BRA. ***p<0.001; **p<0.05; *p<0.1.") ///
indicate("State FE = *.state" "Macro-Region FE = *.region3" "Time FE = *.year") 

*********** STOP ***********

