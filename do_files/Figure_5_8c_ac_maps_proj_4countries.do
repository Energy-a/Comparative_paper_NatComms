
/*------------------------------------------------------------------------------------------------------------------------------------------
 
 This do-file: 
   1) Creates district level projections
   2) Creates income decile projected ac levels
   3) Creates state level projections
   3) Data for Figure 3 and Figure 5
   
------------------------------------------------------------------------------------------------------------------------------------------*/

* Set directory
*cd "C:\Users\decian\Google Drive (enrica.decian@unive.it)\ENERGYA (1)\5-CountryExpertsExchange\"
cd "G:\Il mio Drive\ENERGYA\5-CountryExpertsExchange\"
*cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\"

/*--------------------------------------------------------------- 

          District level dataset -> for Maps in Appendix

---------------------------------------------------------------*/

* Load projections data
use "Comparative_paper\modified_data\for_descriptives\second_stage_MEX_wb.dta", clear
append using "Comparative_paper\modified_data\for_descriptives\second_stage_IND_wb.dta"
append using "Comparative_paper\modified_data\for_descriptives\second_stage_IDN_wb.dta"

* Collapse at district level and save
collapse (mean) ac ac_fut_* ely_q ely_hat_fut* ely_gro_* [pw=weight] , by(year country state state2 state3 district district2 state_district) /* recall that collapse ignore missing values */
gen ac_sh_dst_wgt = ac*100

* Multiply by 100 to have in %
foreach rcp in rcp45 rcp85 { 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen ac_fut_dst_`rcp'_`ssp' = ac_fut_`rcp'_`ssp'*100

}
}

collapse (mean) ac ac_fut_* ely_q ely_hat_fut* ely_gro_*, by(country) /* recall that collapse ignore missing values */

* Save
save "Comparative_paper\modified_data\for_descriptives\HH_4countries_proj_district.dta", replace


/*--------------------------------------------------------------- 

		   Dataset by income deciles -> for Figure 5

---------------------------------------------------------------*/

* Load projections data
use "Comparative_paper\modified_data\for_descriptives\second_stage_MEX_wb.dta", clear
append using "Comparative_paper\modified_data\for_descriptives\second_stage_IND_wb.dta"
append using "Comparative_paper\modified_data\for_descriptives\second_stage_IDN_wb.dta"
append using "Comparative_paper\modified_data\for_descriptives\second_stage_BRA_wb.dta"

*drop if state3 == "Morelos"

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
gen ely_ratio_int_ext_`rcp'_`ssp' = ely_gro_int_ext_`rcp'_`ssp' + 1
gen ely_diff_int_ext_`rcp'_`ssp' = ely_hat_fut_int_ext_`rcp'_`ssp'_q - ely_q
gen ely_diff_int_`rcp'_`ssp' = ely_hat_fut_int_`rcp'_`ssp'_q - ely_q
gen ac_diff_`rcp'_`ssp' = ac_fut_`rcp'_`ssp' - ac
}
}

foreach rcp in rcp45 rcp85 {
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {
gen ely_ratio_int_`rcp'_`ssp' = ely_gro_int_`rcp'_`ssp' + 1
}
}

by country, sort: summ ely_ratio_int_*


* Collapse
collapse (mean) ac ac_diff_rcp* ac_fut_* ely_hat_fut* ely_q ely_diff_int_* , by (state3 country dec_inc)
drop if ac==0
*drop if ac<=0.01
sum ac

*To compare variation across SSPs and RCPs
*collapse (mean) ely_ratio* , by (country)


* Figure 5A
graph box ac ac_fut_rcp45_SSP2, nooutsides ///
over(dec_inc,label(angle(0)labsize(medium))) ///
over(country,label(angle(0)labsize(medium))) ///
ytitle("AC share", size(medium))legend(cols(4)position(12) ring(1)) ///
legend(size(small)) ylabel(, labsize(small))xsize(9)/**/ graphregion(fcolor(white)) ///
plotregion(fcolor(white)) 

* Figure 5B
graph box ely_q ely_hat_fut_int_rcp45_SSP2_q, nooutsides ///
over(dec_inc,label(angle(0)labsize(medium))) ///
over(country,label(angle(0)labsize(medium))) ///
ytitle("Kwh", size(medium))legend(cols(4)position(12) ring(1)) ///
legend(size(small)) ylabel(, labsize(small))xsize(9)/**/ graphregion(fcolor(white)) ///
plotregion(fcolor(white)) 


** Save
save "Comparative_paper\modified_data\for_descriptives\HH_4countries_proj_dec.dta", replace

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
gen ac_sh_state = ac*100

* Multiply by 100 to have in %
foreach rcp in rcp45 rcp85 { 
foreach ssp in SSP1 SSP2 SSP3 SSP4 SSP5 {

gen ac_fut_sta_`rcp'_`ssp' = ac_fut_`rcp'_`ssp'*100

}
}

save "Comparative_paper\modified_data\for_descriptives\HH_4countries_proj_state.dta", replace


* Other stuff
use "Comparative_paper\modified_data\for_descriptives\HH_4countries_proj_state.dta", clear

gen pc_ch= (ac_fut_rcp85_SSP5/ac-1)*100
gen pc_ch_rcp45_ssp4= (ac_fut_rcp45_SSP4/ac-1)*100
gen pc_ch_rcp45_ssp1= (ac_fut_rcp45_SSP1/ac-1)*100
gen pc_ch_rcp45_ssp3= (ac_fut_rcp45_SSP3/ac-1)*100

tabstat ac ac_fut_rcp85_SSP5 ac_fut_rcp45_SSP4 ac_fut_rcp45_SSP1 ac_fut_rcp45_SSP3, by(country) stat(mean) 


gen pc_factor= (ac_fut_rcp85_SSP5/ac)
gen pc_fr_rcp45_ssp4= (ac_fut_rcp45_SSP4/ac)
gen pc_fr_rcp45_ssp1= (ac_fut_rcp45_SSP1/ac)
gen pc_fr_rcp45_ssp3= (ac_fut_rcp45_SSP3/ac)
gen pc_fr_rcp45_ssp5= (ac_fut_rcp45_SSP5/ac)

tabstat pc_factor pc_fr_rcp45_ssp5 pc_fr_*, by(country) stat(mean) 

tabstat ac_fut_rcp85_SSP5 ac_fut_rcp45_SSP4 ac_fut_rcp45_SSP1 ac_fut_rcp85_SSP1 ac_fut_rcp45_SSP3, by(country) stat(mean) 


gen pc_change= (ac_fut_rcp85_SSP5-ac)

tabstat pc_factor ac ac_fut_rcp85_SSP5 n_hh_state if country=="Brazil", by(state3) stat(mean) 
tabstat pc_factor ac ac_fut_rcp85_SSP5 n_hh_state if country=="India", by(state3) stat(mean) 
tabstat pc_factor ac ac_fut_rcp85_SSP5 n_hh_state if country=="Indonesia", by(state3) stat(mean) 
tabstat pc_factor ac ac_fut_rcp85_SSP5 n_hh_state if country=="Mexico", by(state3) stat(mean) 

tabstat ac ac_fut_rcp85_SSP5, by(country) stat(iqr sd) 









