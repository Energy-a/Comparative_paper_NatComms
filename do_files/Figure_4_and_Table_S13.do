
/*------------------------------------------------------------------------------------------------------------------------------------------
 
  Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)
 
 This do-file: 
   1) Multiply population-weighted CDDs by population to get a measure of the total CDD exposure in each country, state and district.
	  This is the total number of CDDs experienced annually by a country’s population. 
   2) Compute the Adaptation Cooling gap
   3) Data for Figure 4 (Main) 
   4) Creates Table 13 (SI)
   5) we have also added the possibility of to partially replicates Figure 4 using STATA -> lines 227-259
   
------------------------------------------------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"

/*--------------------------------------------------------------- 

              Prepare the dataset at state-level

---------------------------------------------------------------*/

* Load projections data
use "output_data_files\projections\second_stage_MEX_wb.dta", clear
append using "output_data_files\projections\second_stage_IND_wb.dta"
append using "output_data_files\projections\second_stage_BRA_wb.dta"

tempfile data
save "`data'", replace

* Collapse obtaining total population represented in each state and country
* CDD-wb at state level
use "input_data_files\Household.dta", clear
collapse (mean) mean_CDD_wb, by(year country state3)
tempfile cdd
save "`cdd'", replace
 
* Projected CDD-wb at state level
use "input_data_files\Projections.dta", clear
tempfile cdd_fut
save "`cdd_fut'", replace

* Projected change in population at state level
use "input_data_files\Projections.dta", clear
keep country state2 Pop*
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen pct_pop_`ssp' = (Pop_mean_2020_2060_`ssp' - Pop_2010)/Pop_2010

}
tempfile fut_pop
save "`fut_pop'", replace

* Merge with projections data
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

use "`data'", clear
merge m:1 country state2 using "`fut_pop'"
keep if _merge == 3
drop _merge

* Count future number of households
bysort year country state3: gen hh_state_fut_`ssp'=_n
gen weight_`ssp' = weight*(1+pct_pop_`ssp')
collapse (count) hh_state_fut_`ssp' [pw=weight_`ssp'] , by(year country state3 state2) /* This gives the weighted sum of all families */
tempfile fut_pop_`ssp'
save "`fut_pop_`ssp''", replace

}

** Merge all state level datasets
* Current and future AC shares at state level
use "`data'", clear
bysort year country state3: gen hh_state=_n
collapse (mean) ac ac_fut_* (count) hh_state [pw=weight] , by(year country state3 state2)

* Merge with CDD
merge 1:1 year country state3 using "`cdd'"
keep if _merge == 3
drop _merge

* Merge with future CDD 
merge 1:1 country state2 using "`cdd_fut'"
keep if _merge == 3
drop _merge

* Merge with population
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

merge 1:1 country state2 using "`fut_pop_`ssp''"
keep if _merge == 3
drop _merge

}


/*--------------------------------------------------------------- 

                    Adaptation Cooling Gap

---------------------------------------------------------------*/

** 1) Compute No AC shares by state
gen no_ac = 1 - ac // No AC share current

foreach rcp in rcp45 rcp85 { 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen no_ac_fut_sta_`rcp'_`ssp' = 1 - ac_fut_`rcp'_`ssp' // No AC share fut

}
}

** 2a) Number of HHs without AC and with AC by state
gen n_without_ac = no_ac*hh_state/1000000 // No AC population current
label var n_without_ac "Number of HHs without AC (state level) in millions"

gen n_with_ac = ac*hh_state/1000000 // AC population current
label var n_with_ac "Number of HHs with AC (state level) in millions"

** 2b) Projected Number of HHs without AC and with AC by state
foreach rcp in rcp45 rcp85 { 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen n_without_ac_`rcp'_`ssp' = no_ac_fut_sta_`rcp'_`ssp'*hh_state_fut_`ssp'/1000000 // No AC population fut
label var n_without_ac_`rcp'_`ssp' "Number of HHs without AC in `rcp'-`ssp' (state level) in millions"

gen n_with_ac_`rcp'_`ssp' = ac_fut_`rcp'_`ssp'*hh_state_fut_`ssp'/1000000 // AC population fut
label var n_with_ac_`rcp'_`ssp' "Number of HHs with AC in `rcp'-`ssp' (state level) in millions"

}
}

** 3a) Average total number of CDDs experienced in the last 30 years by a state’s population (EXPOSURE)
gen CDDwb_pop_curr_state = hh_state*mean_CDD_wb/1000000 // CDD exposure by state current
label var CDDwb_pop_curr_state "Average total number of annual CDDs experienced in the last 30 years (state level)"


** 3b) Average total number of CDDs that will be experienced in 2040 by a state’s population (EXPOSURE)
foreach rcp in rcp45 rcp85 { 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen CDDwb_pop_`rcp'_`ssp'_state = hh_state_fut_`ssp'*mean_CDD_2021_2060_wb_`rcp'_`ssp'/1000000 // CDD exposure by state fut
label var CDDwb_pop_`rcp'_`ssp'_state "Average total number of annual CDDs experienced up to 2040 in `rcp'-`ssp'(state level)"

}
}

** 4) Generate the cooling gap indexes
gen adapt_CDD_exposure = n_with_ac*mean_CDD_wb
label var adapt_CDD_exposure "Adapted CDD exposure (state level)"

gen unmet_cool_dem = n_without_ac*mean_CDD_wb
label var unmet_cool_dem "Unmet Cooling Demand: Unadapted CDD exposure (state level)"

foreach rcp in rcp45 rcp85 { 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen adapt_CDD_exposure_`rcp'_`ssp' = n_with_ac_`rcp'_`ssp'*mean_CDD_2021_2060_wb_`rcp'_`ssp'
label var adapt_CDD_exposure_`rcp'_`ssp' "Adapted CDD exposure in `rcp'-`ssp' (state level)"

gen unmet_cool_dem_`rcp'_`ssp' = n_without_ac_`rcp'_`ssp'*mean_CDD_2021_2060_wb_`rcp'_`ssp'
label var unmet_cool_dem_`rcp'_`ssp' "Unmet Cooling Demand: Unadapted CDD exposure in `rcp'-`ssp' (state level)"


}
}

** 5) Normalise by country average the exposure
* Compute country-level current variables
bysort country: egen tot_pop = sum(hh_state) // total n°HHs represented
bysort country: gen tot_pop_mn = tot_pop/1000000 // total n°HHs represented in millions
label var tot_pop "Total number of HHs in the country"

bysort country: egen CDD_wb_country = median(mean_CDD_wb) // Median CDD by country
label var CDD_wb_country "Country median of the mean annual CDD in the last 30 years" 

bysort country: egen CDDwb_pop_curr_country = median(CDDwb_pop_curr_state)
label var CDDwb_pop_curr_country "Country median CDD exposure experienced in the last 30 years" // CDD exposure by country

bysort country: gen exposure_ratio = CDDwb_pop_curr_state/CDDwb_pop_curr_country // ratio current
label var exposure_ratio "CDD exposure relative to country average"

* Compute country-level future variables
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

bysort country: egen tot_pop_fut_`ssp' = sum(hh_state_fut_`ssp') // total n°HHs represented
bysort country: gen tot_pop_mn_fut_`ssp' = tot_pop_fut_`ssp'/1000000 // total n°HHs represented in millions
label var tot_pop_fut_`ssp' "Total number of HHs in the country in 2040 `ssp'"

foreach rcp in rcp45 rcp85 { 

bysort country: egen CDD_wb_country_`rcp'_`ssp' = median(mean_CDD_2021_2060_wb_`rcp'_`ssp')
label var CDD_wb_country_`rcp'_`ssp' "Country average of the mean annual CDD in 2040 `rcp'-`ssp'"

bysort country: egen CDDwb_pop_country_`rcp'_`ssp' = median(CDDwb_pop_`rcp'_`ssp'_state)
label var CDDwb_pop_country_`rcp'_`ssp' "Country average CDD exposure in 2040 `rcp'-`ssp'"

bysort country: gen exposure_ratio_`rcp'_`ssp' = CDDwb_pop_`rcp'_`ssp'_state/CDDwb_pop_country_`rcp'_`ssp'
label var exposure_ratio_`rcp'_`ssp' "CDD exposure relative to country average in 2040 `rcp'-`ssp'"

}
}

** 6) Normalise by country average the AC penetration rate
* Compute country-level current variables
bysort country: egen ac_country = median(ac) // Average AC by country
label var ac_country "Country average AC adoption (current)" 

bysort country: gen ac_ratio = ac/ac_country // AC ratio current
label var ac_ratio "AC adoption relative to country average"

* Compute country-level future variables
foreach rcp in rcp45 rcp85 { 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

bysort country: egen ac_country_`rcp'_`ssp' = median(ac_fut_`rcp'_`ssp')
label var ac_country_`rcp'_`ssp' "Country average AC adoption in 2040 `rcp'-`ssp'"

bysort country: gen ac_ratio_`rcp'_`ssp' = ac_fut_`rcp'_`ssp'/ac_country_`rcp'_`ssp'
label var ac_ratio_`rcp'_`ssp' "AC adoption relative to country average in 2040 `rcp'-`ssp'"

}
} 

** Make plots - Similar to FIGURE 4
/*foreach country in Brazil India Mexico {

#delimit;	
twoway (scatter ac_ratio exposure_ratio [weight = hh_state] if country == "`country'", mlabsize(textsizestyle) 
		msymbol(circle_hollow) 
		mlcolor(dknay) 
		mfcolor(gs14) 
		msize(vsmall) 
		mlwidth(thin))
	   (scatter ac_ratio exposure_ratio [weight = hh_state] if country == "`country'",  mlabel(state3) mlabsize(tiny) msym(i)), 
		xline(1) 
		yline(1) 
		legend(off)
		graphregion(color(white));
		
graph export "output_table_figures\Figure_4\AC_CDD_exposure_ratio_`country'_new.png", width(1500) replace;  

#delimit;	
twoway (scatter ac_ratio_rcp85_SSP5 exposure_ratio_rcp85_SSP5 [weight = hh_state_fut_SSP5] if country == "`country'", mlabsize(textsizestyle) 
		msymbol(circle_hollow) 
		mlcolor(dknay) 
		mfcolor(gs14) 
		msize(vsmall) 
		mlwidth(thin))
	   (scatter ac_ratio_rcp85_SSP5 exposure_ratio_rcp85_SSP5 [weight = hh_state_fut_SSP5] if country == "`country'",  mlabel(state3) mlabsize(tiny) msym(i)), 
		xline(1) 
		yline(1) 
		legend(off)
		graphregion(color(white));
		
graph export "output_table_figures\Figure_4\AC_CDD_exposure_ratio_rcp85_SSP5_`country'_new.png", width(1500) replace;  
		
};*/

*** 6) Compute number of HH in each quadrants for each country and total
** Current level
* Q1
bysort country: egen N_HH_q1 = sum(n_without_ac) if ac_ratio > 1 & exposure_ratio > 1 // Q1 top-right: high-exposure, high-adoption
bysort country: sum N_HH_q1

* Q2
bysort country: egen N_HH_q2 = sum(n_without_ac) if ac_ratio > 1 & exposure_ratio < 1 // Q2 top-left: low-exposure, high-adoption
bysort country: sum N_HH_q2

* Q3
bysort country: egen N_HH_q3 = sum(n_without_ac) if ac_ratio < 1 & exposure_ratio < 1 // Q3 bottom-left: low-exposure, low-adoption
bysort country: sum N_HH_q3

* Q4
bysort country: egen N_HH_q4 = sum(n_without_ac) if ac_ratio < 1 & exposure_ratio > 1 // Q4 bottom-right: high-exposure, low-adoption
bysort country: sum N_HH_q4

** Future level
* Q1
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5{
foreach rcp in rcp45 rcp85 {
bysort country: egen N_HH_q1_fut_`rcp'_`ssp' = sum(n_without_ac_`rcp'_`ssp') if ac_ratio_`rcp'_`ssp' > 1 & exposure_ratio_`rcp'_`ssp' > 1 // top-right: high-exposure, high-adoption
bysort country: sum N_HH_q1_fut_`rcp'_`ssp'
}
}

* Q2
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5{
foreach rcp in rcp45 rcp85 {
bysort country: egen N_HH_q2_fut_`rcp'_`ssp' = sum(n_without_ac_`rcp'_`ssp') if ac_ratio_`rcp'_`ssp' > 1 & exposure_ratio_`rcp'_`ssp' < 1 //   top-left: low-exposure, high-adoption
bysort country: sum N_HH_q2_fut_`rcp'_`ssp'
}
}

* Q3
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5{
foreach rcp in rcp45 rcp85 {
bysort country: egen N_HH_q3_fut_`rcp'_`ssp' = sum(n_without_ac_`rcp'_`ssp') if ac_ratio_`rcp'_`ssp' < 1 & exposure_ratio_`rcp'_`ssp' < 1 //  bottom-left: low-exposure, low adoption
bysort country: sum N_HH_q3_fut_`rcp'_`ssp'
}
}

* Q4
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {
foreach rcp in rcp45 rcp85 {
bysort country: egen N_HH_q4_fut_`rcp'_`ssp' = sum(n_without_ac_`rcp'_`ssp') if ac_ratio_`rcp'_`ssp' < 1 & exposure_ratio_`rcp'_`ssp' > 1 // bottom-right: high-exposure, low-adoption
bysort country: sum N_HH_q4_fut_`rcp'_`ssp'
}
}


** Sum in all countries
* Q4 - Current
egen TOT_N_HH_q4 = sum(n_without_ac) if ac_ratio < 1 & exposure_ratio > 1 // Q4 bottom-right: high-exposure, low-adoption

* Q4 - Future
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5{
foreach rcp in rcp45 rcp85 {
egen TOT_N_HH_q4_fut_`rcp'_`ssp' = sum(n_without_ac_`rcp'_`ssp') if ac_ratio_`rcp'_`ssp' < 1 & exposure_ratio_`rcp'_`ssp' > 1
}
}

tabstat TOT_N_HH_q4_fut_*, columns(s) // numbers cited in the paper but they do not count for Indonesia (so they are lower)

** 9) Number of HHs with at least one AC

* Tot HHs with AC - today 
by country, sort: egen hh_ac = sum(n_with_ac) 
by country, sort: egen hh_no_ac = sum(n_without_ac) 

* Tot HHs with AC - future 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5{
foreach rcp in rcp45 rcp85 {

by country, sort: egen hh_ac_`rcp'_`ssp' = sum(n_with_ac_`rcp'_`ssp') 
by country, sort: egen hh_no_ac_`rcp'_`ssp' = sum(n_without_ac_`rcp'_`ssp')

}
}

* Table S13
bysort country: tabstat hh_ac hh_ac_rcp45_SSP*, columns(s)
bysort country: tabstat hh_ac hh_ac_rcp85_SSP*, columns(s)


** Save
save "output_data_files\projections\Figure_4.dta", replace


**********************************************************************************						
************************************** END ***************************************
**********************************************************************************

