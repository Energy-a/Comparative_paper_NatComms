
/*----------------------------------------------------------------------------------
 
 This do-file:
   1) creates ownership-density plots for the second wave (FIGURE 1 PANEL B)
   2) compute kernel density
   3) ownership is estimated by the means of a weighted local regression

----------------------------------------------------------------------------------*/

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
save "output_data_files\Figure_1_Panel_B\kdens_`country'_`wave'.dta", replace

* Plot
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
	
};


* Append the output
use "output_data_files\Figure_1_Panel_B\kdens_Mexico_2.dta", clear
append using "output_data_files\Figure_1_Panel_B\kdens_Indonesia_2.dta"
append using "output_data_files\Figure_1_Panel_B\kdens_India_2.dta"
append using "output_data_files\Figure_1_Panel_B\kdens_Brazil_2.dta"

* Save
save "output_data_files\Figure_1_Panel_B\kdens_local_reg_own.dta", replace
