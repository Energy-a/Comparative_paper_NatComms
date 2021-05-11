
/*-------------------------------------------------------------------------------------------------------

 Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)

This do-file:
   1) saves data for Figure S1 (SI)
   2) create weighted measures at district and state level for AC, FAN, REF, Urbanisation, Electricity 
	  Access and Expenditure per adult equivalent
   
-------------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"

* Load data
use "input_data_files\Household.dta", clear

/*--------------------------------------------------------------- 

						State level

---------------------------------------------------------------*/

preserve

** Collapse using weights
collapse (mean) ac fan refrigerator urban ely_access exp_cap_usd_2011 [pw=weight] , by(year country state state2 state3) /* recall that collapse ignore missing values */

* Multiply by 100 to get shares
gen ac_sh_state_wgt = ac*100
gen fan_sh_state_wgt = fan*100
gen ref_sh_state_wgt = refrigerator*100
gen urban_sh_state_wgt = urban*100
gen ely_access_sh_state_wgt = ely_access*100

drop ac fan refrigerator urban ely_access

keep year country state* ac* fan* ref* urban* ely_access* exp_cap_usd_2011

** Save 
save "output_data_files\Figure_S1\HH_4countries_agg_state.dta", replace

restore

/*--------------------------------------------------------------- 

					   District level

---------------------------------------------------------------*/

preserve

* Drop Brazil which has only state as geographic unit
drop if country == "Brazil"

** Collapse using weights
collapse (mean) ac fan refrigerator urban ely_access exp_cap_usd_2011 [pw=weight] , by(year country state state2 state3 district district2 state_district) /* recall that collapse ignore missing values */

* Multiply by 100 to get shares
gen ac_sh_state_wgt = ac*100
gen fan_sh_state_wgt = fan*100
gen ref_sh_state_wgt = refrigerator*100
gen urban_sh_state_wgt = urban*100
gen ely_access_sh_state_wgt = ely_access*100

drop ac fan refrigerator urban ely_access

keep year country state* district* state_district ac* fan* ref* urban* ely_access* exp_cap_usd_2011

** Save 
save "output_data_files\Figure_S1\HH_4countries_agg_district.dta", replace

restore
