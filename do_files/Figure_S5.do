
/*-------------------------------------------------------------------------------------------------------

 Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)

This do-file:
   1) saves data for Figure S5 (SI)
   2) computes kernel density
   3) ownership-expenditure relationship is estimated by the means of a weighted local regression
   4) plots are then created in R
   5) expenditure and ac data are those obtained from the projections
   6) we have also added the possibility of creating the plots using STATA -> lines 87-125
   7) the x-axis is the household's pae expenditure (log scale)
   8) the y-axis is the share of household with the appliance
   
-------------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"


/*--------------------------------------------------------------- 

			   Local Regression from projections

---------------------------------------------------------------*/

foreach country in Brazil India Mexico {

** Load first stage data

* Load projections data
use "output_data_files\projections\second_stage_MEX_wb.dta", clear
append using "output_data_files\projections\second_stage_IND_wb.dta"
append using "output_data_files\projections\second_stage_BRA_wb.dta"

keep hhid year country* weight ac mean_CDD_wb mean_CDD_db exp_cap_usd_2011 ln_total_exp_usd_2011 ac_fut_* pct_texp_*
bysort country: gen ln_exp_cap_usd_2011 =log(exp_cap_usd_2011)

** Expenditure projections

foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

bysort country: gen ln_exp_cap_usd_2011_`ssp' =log(exp_cap_usd_2011*(1+pct_texp_`ssp'))
bysort country: gen exp_cap_usd_2011_`ssp' =exp_cap_usd_2011*(1+pct_texp_`ssp')

}

* Country
keep if country == "`country'"

* Sample reduction
set seed 1
sample 10 // reduce sample size to allow lowess working quickly

* Local Reg. for baseline AC
lowess ac ln_exp_cap_usd_2011 if ac !=., mean generate(local_ac) nograph adjust
sort local_ac 

* Local Reg. for projected AC (SSP1)
lowess ac_fut_rcp85_SSP1 ln_exp_cap_usd_2011_SSP1 if ac_fut_rcp85_SSP1 !=., mean generate(local_ac_rcp85_SSP1) nograph adjust
sort local_ac_rcp85_SSP1

* Local Reg. for projected AC (SSP1)
lowess ac_fut_rcp85_SSP2 ln_exp_cap_usd_2011_SSP2 if ac_fut_rcp85_SSP2 !=., mean generate(local_ac_rcp85_SSP2) nograph adjust
sort local_ac_rcp85_SSP2

* Local Reg. for projected AC (SSP3)
lowess ac_fut_rcp85_SSP3 ln_exp_cap_usd_2011_SSP3 if ac_fut_rcp85_SSP3 !=., mean generate(local_ac_rcp85_SSP3) nograph adjust
sort local_ac_rcp85_SSP3

* Local Reg. for projected AC (SSP4)
lowess ac_fut_rcp85_SSP4 ln_exp_cap_usd_2011_SSP4 if ac_fut_rcp85_SSP4 !=., mean generate(local_ac_rcp85_SSP4) nograph adjust
sort local_ac_rcp85_SSP4

* Local Reg. for projected AC (SSP5)
lowess ac_fut_rcp85_SSP5 ln_exp_cap_usd_2011_SSP5 if ac_fut_rcp85_SSP5 !=., mean generate(local_ac_rcp85_SSP5) nograph adjust
sort local_ac_rcp85_SSP5

* Kernel density at baseline
kdensity ln_exp_cap_usd_2011 [aweight = weight], generate(kernel_ln_exp density) nograph n(1000)
gen kernel_exp = exp(kernel_ln_exp)

* Save
tempfile local_proj_`country'
save `local_proj_`country'', replace

/** Plot
#delimit;
local xtitle = "Annual Expenditure Per Adult Equivalent (Log Scale)";
graph twoway 
	(line local_ac exp_cap_usd_2011 if exp_cap_usd_2011 > 0,
		clwidth(thick )
		lpattern(solid dash_dot)
		xscale(log))
	(line local_ac_rcp85_SSP1 exp_cap_usd_2011_SSP1 if exp_cap_usd_2011_SSP1 > 0,
		clwidth(thick )
		lpattern(solid dash_dot)
		xscale(log))		
	(line local_ac_rcp85_SSP2 exp_cap_usd_2011_SSP2 if exp_cap_usd_2011_SSP2 > 0,
		clwidth(thick )
		lpattern(solid dash_dot)
		xscale(log))
	(line local_ac_rcp85_SSP3 exp_cap_usd_2011_SSP3 if exp_cap_usd_2011_SSP3 > 0,
		clwidth(thick )
		lpattern(solid dash_dot)
		xscale(log))	
	(line local_ac_rcp85_SSP4 exp_cap_usd_2011_SSP4 if exp_cap_usd_2011_SSP4 > 0,
		clwidth(thick )
		lpattern(solid dash_dot)
		xscale(log))	
	(line local_ac_rcp85_SSP5 exp_cap_usd_2011_SSP5 if exp_cap_usd_2011_SSP5 > 0,
		clwidth(thick )
		lpattern(solid dash_dot)
		xscale(log))	
	||(line density kernel_exp [aweight = weight], clwidth(thin) lpattern(dash_dot)),
		yscale(range(0 1))
		ylabel(#5, nogrid labsize(small))
		xscale(log)
		xscale(range(500 1000000))
		xlabel(1000 10000 100000 1000000, labsize(small))
		xtitle("Annual Expenditure per Adult Equivalent in 2011 USD (Log Scale)")
		ytitle("Fraction of Households with AC")
		graphregion(color(white))	
	legend(order(1 "CURRENT" 2 "RCP8.5-SSP1" 3 "RCP8.5-SSP2" 4 "RCP8.5-SSP3" 5 "RCP8.5-SSP4" 6 "RCP8.5-SSP5"  7 "HH Density") cols(3));*/
	
*graph export "Comparative_paper\R_plots\Other_plots\proj_local_reg_`country'.png", width(1200) replace; 
 

}

* Append the four countries

use "`local_proj_Mexico'", clear
append using "`local_proj_India'"
append using "`local_proj_Brazil'"

* Save
save "output_data_files\projections\local_reg_proj.dta", replace
