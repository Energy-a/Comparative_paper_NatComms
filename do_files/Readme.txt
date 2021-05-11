
 Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)

Description of all .do files:

- Figure_1_panel_B

   	1) saves data for Figure 1 - Bottom (Main)
   	2) computes kernel density
   	3) ownership-expenditure relationship is estimated by the means of a weighted local regression
   	4) plots are then created in R
   	5) we use only the second wave so we have wave-years that are near among the four countries
	  (2008 BRA, 2012 MEX and IDN, 2011/2012 IND)
   	6) we have also added the possibility of creating the plots using STATA -> lines 62-86
   	7) the x-axis is the household's pae expenditure (log scale)
   	8) the y-axis is the share of household with the appliance

- Table_1_and_Table_S7

	1) run the standardised logit regressions for all appliances and countries
   	2) Save marginal effects results -> Table 1 (Main)

- Figure_2_panel_A_and Figure_S3_Top

	1) Save marginal effects of CDD by expenditure level for AC, fan and refrigerator, and for all countries 
   	2) marginal effects are used create in R both Figure 2 - Panel A (Main) and Figure S3 - Top (SI)
   	3) we use both waves
   	4) the x-axis is CDD wet-bulb (or dry-bulb)
   	5) the y-axis is predicted probabilities

- Figure_2_panel_B_and Figure_S3_Bottom

  	1) save predicted probabilities for each appliances and countries
   	2) predicted probabilities are used create in R both Figure 2 - Panel B (Main) and Figure S3 - Bottom (SI)
   	3) we use both waves
   	4) the x-axis is CDD wet-bulb (or dry-bulb)
   	5) the y-axis is predicted probabilities

- proj_*_wb

 	1) exploits CDD-wet bulb 24 deg
   	2) conducts logit regressions for each country last wave
   	3) project future ownership rates
   	4) run intensive margin regressions: electricity expenditure on climate + covariates

- proj_*_db

 	1) exploits CDD-dry bulb 24 deg
   	2) conducts logit regressions for each country last wave
   	3) project future ownership rates
   	4) run intensive margin regressions: electricity expenditure on climate + covariates

- proj_*_wb_22deg

 	1) exploits CDD-wet bulb 22 deg
   	2) conducts logit regressions for each country last wave
   	3) project future ownership rates
   	4) run intensive margin regressions: electricity expenditure on climate + covariates

- proj_*_db_22deg

 	1) exploits CDD-wet bulb 22 deg
   	2) conducts logit regressions for each country last wave
   	3) project future ownership rates
   	4) run intensive margin regressions: electricity expenditure on climate + covariates

- Figure_4_and_Table_S13
	
	1) Multiply population-weighted CDDs by population to get a measure of the total CDD exposure in each country, state and district.
	   This is the total number of CDDs experienced annually by a country’s population. 
   	2) Compute the Adaptation Cooling gap
  	3) Data for Figure 4 (Main) 
   	4) Creates Table 13 (SI)
   	5) we have also added the possibility of to partially replicates Figure 4 using STATA -> lines 227-259

- Figure_3_5_and_Figure_S4_S6_S7

	1) creates district level projection data -> this will be used in Figure S6 (SI)
   	2) creates income decile level projection data -> this will be used for Figure 5 (Main)
   	3) creates state level projections -> this will be used for Figure 3 (Main), S4 and S7 (SI)
   	4) saves data for Figure 3, 5, S6, S4 and S7 

- Figure_S1

	1) saves data for Figure S1 (SI)
   	2) create weighted measures at district and state level for AC, FAN, REF, Urbanisation, Electricity 
	   Access and Expenditure per adult equivalent

- Figure_S5

	1) saves data for Figure S5 (SI)
   	2) computes kernel density
   	3) ownership-expenditure relationship is estimated by the means of a weighted local regression
   	4) plots are then created in R
   	5) expenditure and ac data are those obtained from the projections
   	6) we have also added the possibility of creating the plots using STATA -> lines 87-125
   	7) the x-axis is the household's pae expenditure (log scale)
   	8) the y-axis is the share of household with the appliance

- Table_S4

	1) creates weighted descriptives statistics for comparative paper -> Table S4
   	2) we proceed selecting a Country and a Year for each repetition
   	3) we use sum up because it allows to run statistics by country and year and does not create problems with missing variables (e.g. ac, fan, energy expenditure in Indonesia)

- Table_S5_S6_S8

	1) run the two-wave logit regression
   	2) saves marginal effects
   	3) creates Table S5 (SI), S6 (SI) and S8 (SI)
   	4) 24 deg wet- and dry- bulb regression

- Table_S9_S10

	1) run the OLS regressions of electricity quantity (kwH) for all countries
   	2) only for the last wave
   	3) save coefficients results -> Table 9 (SI) and Table 10 (SI)

- Table_S11

	1) conducts logit regressions for each country separately using: either two waves or last wave or only
	   CDD and expenditure as covariates
   	2) AC is the only appliance analysed through regressions, using either wet
   	3) save results 

- Table_S12
	
	1) creates Table S12 (SI) -> compare elasticities across CDDs and temperature thresholds
  	2) conducts standardised logit regressions for each country on TWO waves using
		a) 22 deg wb CDDs
		b) 22 deg db CDDs
		c) 24 deg wb CDDs
		d) 24 deg db CDDs

- Table_S14_to_S21 

	1) use state-level projection dataset
   	2) recall that these projections are created using 24-deg CDD wb
   	3) creates Tables S14 to S21 (SI)