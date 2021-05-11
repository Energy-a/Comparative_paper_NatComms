
/*------------------------------------------------------------------------------------------------------------------------------------------
  
  Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)

 This do-file: 
   1) use state-level projection dataset
   2) recall that these projections are created using 24-deg CDD wb
   3) creates Tables S14 to S21 (SI)
   
------------------------------------------------------------------------------------------------------------------------------------------*/

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"

/*--------------------------------------------------------------- 

             Table S14 to S21 in Supp. Information

---------------------------------------------------------------*/

* Load data
use "output_data_files\projections\HH_4countries_proj_state.dta", clear

* Table S14
sutex2 ely_gro_int_rcp85_SSP*_q ely_gro_int_rcp45_SSP*_q if country=="Brazil", ///
	   replace saving("output_table_figures\Supplementary_Info\table_S14.tex") caption("Summary statistics") ///
	   minmax percentiles(25 50 75)

* Table S15
sutex2 ely_gro_int_rcp85_SSP*_q ely_gro_int_rcp45_SSP*_q if country=="Mexico", ///
	   replace saving("output_table_figures\Supplementary_Info\table_S15.tex") caption("Summary statistics") ///
	   minmax percentiles(25 50 75)

* Table S16
sutex2 ely_gro_int_rcp85_SSP*_q ely_gro_int_rcp45_SSP*_q if country=="India", ///
	   replace saving("output_table_figures\Supplementary_Info\table_S16.tex") caption("Summary statistics") ///
	   minmax percentiles(25 50 75)

* Table S18
sutex2 ac_fut_rcp85_SSP* ac_fut_rcp45_SSP* if country=="Brazil", ///
	   replace saving("output_table_figures\Supplementary_Info\table_S18.tex") caption("Summary statistics") ///
	   minmax percentiles(25 50 75)

* Table S19	   
sutex2 ac_fut_rcp85_SSP* ac_fut_rcp45_SSP* if country=="Mexico", ///
	   replace saving("output_table_figures\Supplementary_Info\table_S19.tex") caption("Summary statistics") ///
	   minmax percentiles(25 50 75)

* Table S20
sutex2 ac_fut_rcp85_SSP* ac_fut_rcp45_SSP* if country=="India", ///
	   replace saving("output_table_figures\Supplementary_Info\table_S20.tex") caption("Summary statistics") ///
	   minmax percentiles(25 50 75)
