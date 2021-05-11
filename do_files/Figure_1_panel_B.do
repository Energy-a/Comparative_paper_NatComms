
/*--------------------------------------------------------------------------------------------------

 Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)
 
 This do-file:
   1) saves data for Figure 1 - Bottom (Main)
   2) computes kernel density
   3) ownership-expenditure relationship is estimated by the means of a weighted local regression
   4) plots are then created in R
   5) we use only the second wave so we have wave-years that are near among the four countries
	  (2008 BRA, 2012 MEX and IDN, 2011/2012 IND)
   6) we have also added the possibility of creating the plots using STATA -> lines 62-86
   7) the x-axis is the household's pae expenditure (log scale)
   8) the y-axis is the share of household with the appliance

--------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"


* Load data
use "input_data_files\Household.dta", clear


/*--------------------------------------------------------------- 

              Local regression and Kernel density

---------------------------------------------------------------*/

foreach country in Mexico Brazil India {

* Load
use "input_data_files\Household.dta", clear
keep if country == "`country'" & wave == 2

* Set seed
set seed 1
sample 10 // reduce sample size to allow lowess working quickly

* AC ownership
lowess ac ln_exp_cap_usd if ac !=., mean generate(local_ac) nograph adjust
sort local_ac 

* FAN ownership
lowess fan ln_exp_cap_usd if fan !=., mean generate(local_fan) nograph adjust
sort local_fan

* REF ownership
lowess refrigerator ln_exp_cap_usd if refrigerator !=., mean generate(local_ref) nograph adjust
sort local_ref

* Kernel density
kdensity ln_exp_cap_usd [aweight = weight], generate(kernel_ln_exp density) nograph n(1000)
gen kernel_exp = exp(kernel_ln_exp)

* Keep vars of interest
keep year wave hhid country* state* district* weight local* ln_exp_cap_usd exp_cap_* density kernel_*

* Save output
tempfile kdens_`country'
save `kdens_`country'', replace

/* Plot
#delimit;
local xtitle = "Annual Expenditure Per Adult Equivalent (Log Scale)";
graph twoway 
	(line local_ac exp_cap_usd_2011 if exp_cap_usd_2011 > 0,
		clwidth(thick )
		lpattern(solid dash_dot)
		xscale(log))
	(line local_fan exp_cap_usd_2011 if exp_cap_usd_2011 > 0,
		clwidth(thick )
		lpattern(solid dash_dot)
		xscale(log))
	(line local_ref exp_cap_usd_2011 if exp_cap_usd_2011 > 0,
		clwidth(thick )
		lpattern(solid dash_dot)
		xscale(log))
	||(line density kernel_exp [aweight=weight], clwidth(thin) lpattern(dash_dot)),
		yscale(range(0 1))
		ylabel(#5, nogrid labsize(small))
		xscale(log)
		xscale(range(500 1000000))
		xlabel(1000 10000 100000 1000000, labsize(small))
		xtitle("Annual Expenditure per Adult Equivalent in 2011 USD (Log Scale)")
		ytitle("Fraction of households with AC/FAN/REF")
		graphregion(color(white))	
	legend(order(1 "AC" 2 "FAN" 3 "REF" 4 "HH Density") cols(3));
*/	
}


* Append the output
use "`kdens_Mexico'", clear
append using "`kdens_India'"
append using "`kdens_Brazil'"

* Save
save "output_data_files\Figure_1\kdens_local_reg_own.dta", replace
