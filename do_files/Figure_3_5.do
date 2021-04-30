
/*------------------------------------------------------------------------------------------------------------------------------------------
 
 This do-file: 
   1) Creates district level projections
   2) Creates income decile projected ac levels
   3) Creates state level projections
   3) Data for Figure 3 and Figure 5
   
------------------------------------------------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"


/*--------------------------------------------------------------- 

          District level dataset -> for Maps in Appendix

---------------------------------------------------------------*/

* Load projections data
use "output_data_files\projections\second_stage_MEX_wb.dta", clear
append using "output_data_files\projections\second_stage_IND_wb.dta"

* Collapse at district level and save
collapse (mean) ac ac_fut_* ely_q ely_hat_fut* ely_gro_* [pw=weight] , by(year country state state2 state3 district district2 state_district) /* recall that collapse ignore missing values */

* Multiply by 100 to have in %
gen ac_sh_dst_wgt = ac*100

foreach rcp in rcp45 rcp85 { 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen ac_fut_dst_`rcp'_`ssp' = ac_fut_`rcp'_`ssp'*100

}
}

* Save
save "output_data_files\projections\HH_4countries_proj_district.dta", replace


/*--------------------------------------------------------------- 

		   Dataset by income deciles -> for Figure 5

---------------------------------------------------------------*/

* Load projections data
use "output_data_files\projections\second_stage_MEX_wb.dta", clear
append using "output_data_files\projections\second_stage_IND_wb.dta"
append using "output_data_files\projections\second_stage_BRA_wb.dta"

* Generate AC ratio and percentage change
foreach rcp in rcp45 rcp85 {
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {
gen ac_ratio_`rcp'_`ssp' = ac_fut_`rcp'_`ssp'/ac
}
}

foreach rcp in rcp45 rcp85 {
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {
gen ac_pc_`rcp'_`ssp' = (ac_fut_`rcp'_`ssp'-ac)/ac
}
}

* Generate ely and ac deltas (projected-current level)
foreach rcp in rcp45 rcp85 {
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {
gen ely_ratio_int_`rcp'_`ssp' = ely_gro_int_`rcp'_`ssp' + 1
gen ely_diff_int_`rcp'_`ssp' = ely_hat_fut_int_`rcp'_`ssp'_q - ely_q
gen ac_diff_`rcp'_`ssp' = ac_fut_`rcp'_`ssp' - ac
}
}

* Collapse
collapse (mean) ac ac_diff_rcp* ac_fut_* ely_hat_fut* ely_q ely_diff_int_* , by (state3 country dec_inc)
drop if ac==0

* Figure 5A
graph box ac ac_fut_rcp45_SSP2, nooutsides ///
over(dec_inc,label(angle(0)labsize(medium))) ///
over(country,label(angle(0)labsize(medium))) ///
ytitle("AC share", size(medium))legend(cols(4)position(12) ring(1)) ///
legend(size(small)) ylabel(, labsize(small))xsize(9)/**/ graphregion(fcolor(white)) ///
plotregion(fcolor(white)) 

graph export "output_table_figures\Figure_5\ac_fut_dec_inc.png", width(1500) replace

* Figure 5B
graph box ely_q ely_hat_fut_int_rcp45_SSP2_q, nooutsides ///
over(dec_inc,label(angle(0)labsize(medium))) ///
over(country,label(angle(0)labsize(medium))) ///
ytitle("Kwh", size(medium))legend(cols(4)position(12) ring(1)) ///
legend(size(small)) ylabel(, labsize(small))xsize(9)/**/ graphregion(fcolor(white)) ///
plotregion(fcolor(white)) 

graph export "output_table_figures\Figure_5\ely_fut_dec_inc.png", width(1500) replace

** Save
save "output_data_files\projections\HH_4countries_proj_dec.dta", replace

* Collapse at state - income decile level
collapse (mean) ac ac_diff_rcp* ely_q ely_diff_int_* , by (state3 country dec_inc)

* Some other graphs
graph bar ely_ratio_int_ext_rcp85_SSP5, nooutsides ///
over(dec_inc,label(angle(0)labsize(medium))) ///
over(country,label(angle(0)labsize(medium))) ///
ytitle("Electricity growth factor, ratio", size(medium))legend(cols(4)position(12) ring(1)) ///
legend(size(small)) ylabel(, labsize(small))xsize(9)/**/ graphregion(fcolor(white)) ///
plotregion(fcolor(white)) 

graph bar ely_diff_int_ext_rcp85_SSP5, nooutsides ///
over(dec_inc,label(angle(0)labsize(medium))) ///
over(country,label(angle(0)labsize(medium))) ///
ytitle("Electricity change, kWh", size(medium))legend(cols(4)position(12) ring(1)) ///
legend(size(small)) ylabel(, labsize(small))xsize(9)/**/ graphregion(fcolor(white)) ///
plotregion(fcolor(white)) 

graph bar ely_hat_fut_int_ext_rcp85_SSP5 ely_q, nooutsides ///
over(dec_inc,label(angle(0)labsize(medium))) ///
over(country,label(angle(0)labsize(medium))) ///
ytitle("Electricity, kWh", size(medium))legend(cols(4)position(12) ring(1)) ///
legend(size(small)) ylabel(, labsize(small))xsize(9)/**/ graphregion(fcolor(white)) ///
plotregion(fcolor(white)) 


/*--------------------------------------------------------------- 

               State level dataset -> for Figure 3

---------------------------------------------------------------*/

* Load projections data
use "output_data_files\projections\second_stage_MEX_wb.dta", clear
append using "output_data_files\projections\second_stage_IND_wb.dta"
append using "output_data_files\projections\second_stage_BRA_wb.dta"

* Generate total number of households in each state
bysort year country state3: gen hh_state=_n
bys year country state3: egen n_hh_state=max(hh_state)

* Collapse at state level and save
collapse (mean) ac ac_fut_* ely_q ely_hat_fut* ely_gro_* (count) hh_state [pw=weight] , by(year country state3) /* recall that collapse ignore missing values */
bysort country: egen tot_pop = sum(hh_state) // total population represented

* Multiply by 100 to have in %
gen ac_sh_state = ac*100

foreach rcp in rcp45 rcp85 { 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen ac_fut_sta_`rcp'_`ssp' = ac_fut_`rcp'_`ssp'*100

}
}

* Save
save "Comparative_paper\modified_data\for_descriptives\HH_4countries_proj_state.dta", replace



**********************************************************************************						
************************************** END ***************************************
**********************************************************************************


