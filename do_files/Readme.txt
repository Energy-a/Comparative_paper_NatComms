
Description of all .do files:

1) Figure_1_panel_B
	- creates data for ownership-density plots for the second wave -> move to Rscript for "FIGURE 1 PANEL B"
   	- computes kernel density
   	- ownership is estimated by the means of a weighted local regression

2) Table_1
	- run the standardised logit regressions (TABLE 1)
   	- saves marginal effects

3) Figure_2
	- saves predictive probabilities for each appliances and countries to create plots in R -> move to Rscript for "FIGURE 2"

4) proj_*_wb
	- exploits CDD-wet bulb
	- conducts logit regressions for each country last wave
   	- projects future ownership rates of AC
   	- run intensive margin regressions: electricity expenditure on climate + covariates
	- computes projections of ely quantity

5) Figure_4
	- multiplies population-weighted CDDs by population to get a measure of the total CDD exposure in each country, state and district.
	  This is the total number of CDDs experienced annually by a country’s population. 
   	- computes the cooling gap index
   	- creates data for Figure 4

6) Figure_3_5
	- creates district level projections
   	- creates income decile projected ac levels
	- creates state level projections
   	- creates data for Figure 3 and Figure 5