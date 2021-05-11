
/*-------------------------------------------------------------------------------------------------------

 Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
 (DOI of paper to be included upon acceptance)

This do-file:
   1) creates weighted descriptives statistics for comparative paper -> Table S4
   2) we proceed selecting a Country and a Year for each repetition
   3) we use sum up because it allows to run statistics by country and year and does not create problems with missing variables (e.g. ac, fan, energy expenditure in Indonesia)
   
-------------------------------------------------------------------------------------------------------*/

* Variable for the descriptives
global var "ac fan refrigerator urban total_exp_usd_2011 exp_cap_usd_2011 energy_exp_usd_2011 ely_exp_usd_2011 medical_exp_usd_2011 food_exp_usd_2011 ely_q n_members sh_under16 sh_infants literacy_head ownership_d ely_access housing_index_lab car tv"

* Set directory
cd "C:\Users\Standard\Google Drive\5-CountryExpertsExchange\Comparative_Paper\Data_codes_to_be_submitted_for_online_ver\"

* Load data
use "input_data_files\Household.dta", clear

*** Table S4
sumup $var [aw = weight], by(country2 year)
bysort country2 year: sum mean_CDD_wb mean_CDD_db // 24 deg CDDs
