
/*-------------------------------------------------------------------------------------------------------

 Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)

This do-file:
   1) creates Table S12 (SI) -> compare elasticities across CDDs and temperature thresholds
   2) conducts standardised logit regressions for each country on TWO waves using
		a) 22 deg wb CDDs
		b) 22 deg db CDDs
		c) 24 deg wb CDDs
		d) 24 deg db CDDs
   
-------------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"

/*--------------------------------------------------------------- 

				  Wet-bulb 22 deg vs 24 deg

---------------------------------------------------------------*/

** REGRESSION 1: POF - BRAZIL

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

preserve

*** Merge with 22deg CDDs data
use "input_data_files/pop_wt_dta_files_using_GLDAS_CDDwb_hist/22_deg/BRA_statistics_census_tract_level_CDD_wetbulb_22degC_1970_2016_POP_wght.dta", clear

* recode stratum_groups -> equal to the names in HH_Brazil_200*
replace stratum_groups = "MetRegion" if stratum_groups == "Met.Region"
replace stratum_groups = "other_rural" if stratum_groups == "rural"
replace stratum_groups = "other_urban" if stratum_groups == "urban"

* rename Region_* in survey var state*
rename Region_ID state
tostring state, gen(state2)
order state*

qui ds state* stratum_groups, not
collapse `r(varlist)', by(state* stratum_groups)  

** create CDD mean for the other waves
* 2008
egen mean_CDD_1970_2008_wb_22deg = rowmean(CDD_1970 CDD_1971 CDD_1972 CDD_1973 CDD_1974 CDD_1975 CDD_1976 CDD_1977 CDD_1978 CDD_1979 CDD_1980 CDD_1981 CDD_1982 CDD_1983 CDD_1984 CDD_1985 ///
									 CDD_1986 CDD_1987 CDD_1988 CDD_1989 CDD_1990 CDD_1991 CDD_1992 CDD_1993 CDD_1994 CDD_1995 CDD_1996 CDD_1997 CDD_1998 CDD_1999 CDD_2000 CDD_2001 ///
									 CDD_2002 CDD_2003 CDD_2004 CDD_2005 CDD_2006 CDD_2007 CDD_2008)							  

* create other climate vars to add in the dataframe								  
clonevar mean_CDD_wb_22deg = mean_CDD_1970_2016 // This var has the same name in all POF waves, but CDD means are clearly computed differently in each wave					 				  

* Keep
keep stratum_groups state_code mean_CDD_wb_22deg mean_CDD_1970_2008_wb_22deg

* Save
tempfile cdd
save `cdd', replace	

restore				  

rename mean_CDD_wb mean_CDD_wb_24deg

* Merge
merge m:1 state_code stratum_groups using "`cdd'"
drop if _merge == 1 | _merge == 2
drop _merge		

replace mean_CDD_wb_22deg = mean_CDD_1970_2008_wb_22deg if year == 2008			

** Regression
foreach deg in 22 24 {
foreach app in ac {

preserve  
								  
* Standardise vars
quietly logit `app' c.mean_CDD_wb_`deg'deg c.ln_total_exp_usd_2011 c.mean_CDD_wb_`deg'deg#c.ln_total_exp_usd_2011 n_members sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog
	  
foreach x of varlist mean_CDD_wb_`deg'deg ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	  

* Total effect
quietly logit `app' c.std_mean_CDD_wb_`deg'deg c.std_ln_total_exp_usd_2011 c.std_mean_CDD_wb_`deg'deg#c.std_ln_total_exp_usd_2011 std_n_members std_sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab std_age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog

quietly margins, dydx(std_mean_CDD_wb_`deg'deg std_ln_total_exp_usd_2011) atmeans post
eststo pof_`app'_`deg'_wb

restore

}
}


** REGRESSION 2: NSS - INDIA

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

preserve

*** Merge with CDDs
use "input_data_files/pop_wt_dta_files_using_GLDAS_CDDwb_hist/22_deg/IND_statistics_district_level_CDD_wetbulb_22degC_1970_2011_POP_wght.dta", clear

rename District_Name district3
rename District_ID district2
rename Region_ID state2

order state2 district2 district3

*2011
rename mean_CDD_1970_2011 mean_CDD_1970_2011_wb_22deg

*2004								  
egen mean_CDD_1970_2004_wb_22deg = rowmean(CDD_1970 CDD_1971 CDD_1972 CDD_1973 CDD_1974 CDD_1975 CDD_1976 CDD_1977 CDD_1978 CDD_1979 CDD_1980 CDD_1981 CDD_1982 CDD_1983 CDD_1984 CDD_1985 ///
									 CDD_1986 CDD_1987 CDD_1988 CDD_1989 CDD_1990 CDD_1991 CDD_1992 CDD_1993 CDD_1994 CDD_1995 CDD_1996 CDD_1997 CDD_1998 CDD_1999 CDD_2000 CDD_2001 ///
									 CDD_2002 CDD_2003 CDD_2004)

** create other climate vars to add in the dataframe								  
clonevar mean_CDD_wb_22deg = mean_CDD_1970_2011_wb_22deg

* Keep
keep state2 district2 mean_CDD_wb_22deg mean_CDD_1970_2004_wb_22deg

* Save
tempfile cdd
save `cdd', replace	

restore				  

rename mean_CDD_wb mean_CDD_wb_24deg

* Merge
merge m:1 state2 district2 using "`cdd'"
drop if _merge == 1 | _merge == 2
drop _merge

replace mean_CDD_wb_22deg = mean_CDD_1970_2004_wb_22deg if year == 2005

** Regression
foreach deg in 22 24 {
foreach app in ac {

preserve

* Standardise vars
quietly logit `app' c.mean_CDD_wb_`deg' c.ln_total_exp_usd_2011 c.mean_CDD_wb_`deg'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog

foreach x of varlist mean_CDD_wb_`deg' ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	  

* Total effect
quietly logit `app' c.std_mean_CDD_wb_`deg' c.std_ln_total_exp_usd_2011 c.std_mean_CDD_wb_`deg'#c.std_ln_total_exp_usd_2011 i.urban std_n_members std_sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      std_age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog

quietly margins, dydx(std_mean_CDD_wb_`deg' std_ln_total_exp_usd_2011) atmeans post	  
eststo nss_`app'_`deg'_wb

restore

}
}


** REGRESSION 4: ENIGH - MEXICO

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

preserve

*** Merge with 22-deg CDDs
use "input_data_files/pop_wt_dta_files_using_GLDAS_CDDwb_hist/22_deg/MEX_statistics_district_level_CDD_wetbulb_22degC_1970_2016_POP_wght.dta", clear

** decode Region_Name (long format) in a str var (state3)
rename Region_ID state2
rename District_ID district2
rename District_Name district3

** create CDD mean for the other waves

*2016
rename mean_CDD_1970_2016 mean_CDD_wb_22deg

*2012
egen mean_CDD_1970_2012_wb_22deg = rowmean(CDD_1970 CDD_1971 CDD_1972 CDD_1973 CDD_1974 CDD_1975 CDD_1976 CDD_1977 CDD_1978 CDD_1979 CDD_1980 CDD_1981 CDD_1982 CDD_1983 CDD_1984 CDD_1985 ///
									 CDD_1986 CDD_1987 CDD_1988 CDD_1989 CDD_1990 CDD_1991 CDD_1992 CDD_1993 CDD_1994 CDD_1995 CDD_1996 CDD_1997 CDD_1998 CDD_1999 CDD_2000 CDD_2001 ///
									 CDD_2002 CDD_2003 CDD_2004 CDD_2005 CDD_2006 CDD_2007 CDD_2008 CDD_2009 CDD_2010 CDD_2011 CDD_2012)
									 

* Keep
keep state2 district2 mean_CDD_wb_22deg mean_CDD_1970_2012_wb_22deg

* Save
tempfile cdd
save `cdd', replace	

restore				  

rename mean_CDD_wb mean_CDD_wb_24deg

* Merge
merge m:1 state2 district2 using "`cdd'"
drop if _merge == 2
drop _merge

replace mean_CDD_wb_22deg = mean_CDD_1970_2012_wb_22deg if year == 2012

** Regression
foreach deg in 22 24 {
foreach app in ac {

preserve

* Standardise vars
quietly logit `app' c.mean_CDD_wb_`deg' c.ln_total_exp_usd_2011 c.mean_CDD_wb_`deg'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog

foreach x of varlist mean_CDD_wb_`deg' ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	  

* Total effect
quietly logit `app' c.std_mean_CDD_wb_`deg' c.std_ln_total_exp_usd_2011 c.std_mean_CDD_wb_`deg'#c.std_ln_total_exp_usd_2011 i.urban std_n_members std_sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab std_age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog
	   
quietly margins, dydx(std_mean_CDD_wb_`deg' std_ln_total_exp_usd_2011) atmeans post
eststo enigh_`app'_`deg'_wb

restore

}
}


/*--------------------------------------------------------------- 

				  Dry-bulb 22 deg vs 24 deg

---------------------------------------------------------------*/

** REGRESSION 1: POF - BRAZIL

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

preserve

*** Merge with 22deg CDDs data
use "input_data_files/pop_wt_dta_files_using_GLDAS_CDD_hist/22_deg/BRA_statistics_census_tract_level_CDD_22degC_1970_2016_POP_wght.dta", clear

* recode stratum_groups -> equal to the names in HH_Brazil_200*
replace stratum_groups = "MetRegion" if stratum_groups == "Met.Region"
replace stratum_groups = "other_rural" if stratum_groups == "rural"
replace stratum_groups = "other_urban" if stratum_groups == "urban"

* rename Region_* in survey var state*
rename Region_ID state
tostring state, gen(state2)
order state*

qui ds state* stratum_groups, not
collapse `r(varlist)', by(state* stratum_groups)  

** create CDD mean for the other waves
* 2008
egen mean_CDD_1970_2008_db_22deg = rowmean(CDD_1970 CDD_1971 CDD_1972 CDD_1973 CDD_1974 CDD_1975 CDD_1976 CDD_1977 CDD_1978 CDD_1979 CDD_1980 CDD_1981 CDD_1982 CDD_1983 CDD_1984 CDD_1985 ///
									 CDD_1986 CDD_1987 CDD_1988 CDD_1989 CDD_1990 CDD_1991 CDD_1992 CDD_1993 CDD_1994 CDD_1995 CDD_1996 CDD_1997 CDD_1998 CDD_1999 CDD_2000 CDD_2001 ///
									 CDD_2002 CDD_2003 CDD_2004 CDD_2005 CDD_2006 CDD_2007 CDD_2008)							  

* create other climate vars to add in the dataframe								  
clonevar mean_CDD_db_22deg = mean_CDD_1970_2016 // This var has the same name in all POF waves, but CDD means are clearly computed differently in each wave					 				  

* Keep
keep stratum_groups state_code mean_CDD_db_22deg mean_CDD_1970_2008_db_22deg

* Save
tempfile cdd
save `cdd', replace	

restore				  

rename mean_CDD_db mean_CDD_db_24deg

* Merge
merge m:1 state_code stratum_groups using "`cdd'"
drop if _merge == 1 | _merge == 2
drop _merge		

replace mean_CDD_db_22deg = mean_CDD_1970_2008_db_22deg if year == 2008			

** Regression
foreach deg in 22 24 {
foreach app in ac {

preserve  
								  
* Standardise vars
quietly logit `app' c.mean_CDD_db_`deg'deg c.ln_total_exp_usd_2011 c.mean_CDD_db_`deg'deg#c.ln_total_exp_usd_2011 n_members sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog
	  
foreach x of varlist mean_CDD_db_`deg'deg ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	  

* Total effect
quietly logit `app' c.std_mean_CDD_db_`deg'deg c.std_ln_total_exp_usd_2011 c.std_mean_CDD_db_`deg'deg#c.std_ln_total_exp_usd_2011 std_n_members std_sh_under16 i.ownership_d i.edu_head_2 ///
      i.housing_index_lab std_age_head i.sex_head i.region3 i.year if country == "Brazil", robust nolog

quietly margins, dydx(std_mean_CDD_db_`deg'deg std_ln_total_exp_usd_2011) atmeans post
eststo pof_`app'_`deg'_db

restore

}
}


** REGRESSION 2: NSS - INDIA

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

preserve

*** Merge with CDDs
use "input_data_files/pop_wt_dta_files_using_GLDAS_CDD_hist/22_deg/IND_statistics_district_level_CDD_22degC_1970_2011_POP_wght.dta", clear

rename District_Name district3
rename District_ID district2
rename Region_ID state2

order state2 district2 district3

*2011
rename mean_CDD_1970_2011 mean_CDD_1970_2011_db_22deg

*2004								  
egen mean_CDD_1970_2004_db_22deg = rowmean(CDD_1970 CDD_1971 CDD_1972 CDD_1973 CDD_1974 CDD_1975 CDD_1976 CDD_1977 CDD_1978 CDD_1979 CDD_1980 CDD_1981 CDD_1982 CDD_1983 CDD_1984 CDD_1985 ///
									 CDD_1986 CDD_1987 CDD_1988 CDD_1989 CDD_1990 CDD_1991 CDD_1992 CDD_1993 CDD_1994 CDD_1995 CDD_1996 CDD_1997 CDD_1998 CDD_1999 CDD_2000 CDD_2001 ///
									 CDD_2002 CDD_2003 CDD_2004)

** create other climate vars to add in the dataframe								  
clonevar mean_CDD_db_22deg = mean_CDD_1970_2011_db_22deg

* Keep
keep state2 district2 mean_CDD_db_22deg mean_CDD_1970_2004_db_22deg

* Save
tempfile cdd
save `cdd', replace	

restore				  

rename mean_CDD_db mean_CDD_db_24deg

* Merge
merge m:1 state2 district2 using "`cdd'"
drop if _merge == 1 | _merge == 2
drop _merge

replace mean_CDD_db_22deg = mean_CDD_1970_2004_db_22deg if year == 2005

** Regression
foreach deg in 22 24 {
foreach app in ac {

preserve

* Standardise vars
quietly logit `app' c.mean_CDD_db_`deg' c.ln_total_exp_usd_2011 c.mean_CDD_db_`deg'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog

foreach x of varlist mean_CDD_db_`deg' ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	  

* Total effect
quietly logit `app' c.std_mean_CDD_db_`deg' c.std_ln_total_exp_usd_2011 c.std_mean_CDD_db_`deg'#c.std_ln_total_exp_usd_2011 i.urban std_n_members std_sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
      std_age_head i.sex_head i.state i.year if country == "India", vce(cluster state_district) nolog

quietly margins, dydx(std_mean_CDD_db_`deg' std_ln_total_exp_usd_2011) atmeans post	  
eststo nss_`app'_`deg'_db

restore

}
}


** REGRESSION 4: ENIGH - MEXICO

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

preserve

*** Merge with 22-deg CDDs
use "input_data_files/pop_wt_dta_files_using_GLDAS_CDD_hist/22_deg/MEX_statistics_district_level_CDD_22degC_1970_2016_POP_wght.dta", clear

** decode Region_Name (long format) in a str var (state3)
rename Region_ID state2
rename District_ID district2
rename District_Name district3

** create CDD mean for the other waves

*2016
rename mean_CDD_1970_2016 mean_CDD_db_22deg

*2012
egen mean_CDD_1970_2012_db_22deg = rowmean(CDD_1970 CDD_1971 CDD_1972 CDD_1973 CDD_1974 CDD_1975 CDD_1976 CDD_1977 CDD_1978 CDD_1979 CDD_1980 CDD_1981 CDD_1982 CDD_1983 CDD_1984 CDD_1985 ///
									 CDD_1986 CDD_1987 CDD_1988 CDD_1989 CDD_1990 CDD_1991 CDD_1992 CDD_1993 CDD_1994 CDD_1995 CDD_1996 CDD_1997 CDD_1998 CDD_1999 CDD_2000 CDD_2001 ///
									 CDD_2002 CDD_2003 CDD_2004 CDD_2005 CDD_2006 CDD_2007 CDD_2008 CDD_2009 CDD_2010 CDD_2011 CDD_2012)
									 

* Keep
keep state2 district2 mean_CDD_db_22deg mean_CDD_1970_2012_db_22deg

* Save
tempfile cdd
save `cdd', replace	

restore				  

rename mean_CDD_db mean_CDD_db_24deg

* Merge
merge m:1 state2 district2 using "`cdd'"
drop if _merge == 2
drop _merge

replace mean_CDD_db_22deg = mean_CDD_1970_2012_db_22deg if year == 2012

** Regression
foreach deg in 22 24 {
foreach app in ac {

preserve

* Standardise vars
quietly logit `app' c.mean_CDD_db_`deg' c.ln_total_exp_usd_2011 c.mean_CDD_db_`deg'#c.ln_total_exp_usd_2011 i.urban n_members sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog

foreach x of varlist mean_CDD_db_`deg' ln_total_exp_usd_2011 n_members sh_under16 /// 
          age_head {
		  
		  egen std_`x' = std(`x') if e(sample)
		  
		  }	  

* Total effect
quietly logit `app' c.std_mean_CDD_db_`deg' c.std_ln_total_exp_usd_2011 c.std_mean_CDD_db_`deg'#c.std_ln_total_exp_usd_2011 i.urban std_n_members std_sh_under16 i.ownership_d i.edu_head_2 i.occupation_head ///
       i.housing_index_lab std_age_head i.sex_head i.state i.year if country == "Mexico", vce(cluster state_district) nolog
	   
quietly margins, dydx(std_mean_CDD_db_`deg' std_ln_total_exp_usd_2011) atmeans post
eststo enigh_`app'_`deg'_db

restore

}
}




*** Table S12
* Wet-bulb - total effect 22 deg vs 24 deg
esttab pof_*_wb enigh_*_wb nss_*_wb using "output_table_figures\Supplementary_Info\table_S12_wb.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N, fmt(0) labels("Obs.")) se ///
title("Total Marginal Effects from standardized logit models - wet bulb") ///
mtitles("AC-Brazil" "AC-Mexico" "AC-India") ///
addnotes("Clustered standard errors at district level for MEX and IND, and robust standard errors for Brazil in parentheses. State- and year-fixed effects for MEX and IND and region- and year-fixed effect for BRA. ***p<0.001; **p<0.05; *p<0.1.")

* Dry-bulb - total effect 22 deg vs 24 deg
esttab pof_*_db enigh_*_db nss_*_db using "output_table_figures\Supplementary_Info\table_S12_db.tex", replace ///
star(* 0.10 ** 0.05 *** 0.01) b(a3) se(5) nonum nogaps ///
stats(N, fmt(0) labels("Obs.")) se ///
title("Total Marginal Effects from standardized logit models - dry bulb") ///
mtitles("AC-Brazil" "AC-Mexico" "AC-India") ///
addnotes("Clustered standard errors at district level for MEX and IND, and robust standard errors for Brazil in parentheses. State- and year-fixed effects for MEX and IND and region- and year-fixed effect for BRA. ***p<0.001; **p<0.05; *p<0.1.")

*********** STOP ***********
