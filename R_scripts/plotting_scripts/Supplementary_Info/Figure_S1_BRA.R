
####### Replication code for Fig. S1 - Four countries maps at state level (Brazil 2008) ####### 

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

### Maps for the four countries (MEX, IDN, IND and BRA) using both survey (2012, for BRA 2008) and Climate dataframes ###
### Here we map the share of Air Conditioning, Fan, Ref, HH, Ely access and urbanisation by State in each country ###
### Each share is given by dividing: (n°households with AC in state i/n°households in state i)*100 ###

### Here we plot only Brazil 2008! ###

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc()                  # frees up memory resources

## Install required R packages if required before loading

library(lazyeval)
library(rgdal)
library(raster)
library(gdata)
library(foreign)
library(plyr)
library(dplyr)
library(readstata13)
library(haven)
library(tidyr)
library(latticeExtra)
library(sp)
library(grid)
library(gridExtra)
library(fields)
library(stringr)


## Set path to work directory

user <- 'username'

if (user=='username') {
  stub <- 'C:/Users/Standard/Google Drive/5-CountryExpertsExchange/Comparative_paper/'
}

dta_dir              <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_data_files/Figure_S1/', sep='')
map_plot_dir          <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Supplementary_Info/', sep='')
shpf_dir             <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/shape_files/', sep='')

### Read .dta files ###

FOUR_COUNTRIES               <- read.dta13(paste(dta_dir,'/HH_4countries_agg_state.dta', sep='')) ### Library readStata13
BRA_POF                       <- FOUR_COUNTRIES %>% filter(country == "Brazil")

BRA                               <- c('BRA')

## load GADM administrative boundaries

BRA_spdf   <- shapefile(paste(shpf_dir,'Brazil/Adm_level_1_states/BRA_adm1.shp', sep='')) # Brazil

BRA_df     <- BRA_spdf@data
colnames(BRA_spdf@data)

## We rename state names as they coincide with those ones of the GADM administrative boundaries
BRA_POF_2008 <- BRA_POF %>% filter(year == "2008")

# Define an univoque colour palette we'll use it later for the maps
col <- c("#dadaeb", "#bcbddc", "#9e9ac8", "#756bb1", "#54278f")


######## CREATE THE DATABASE FOR MAPS ######## 

# BRA
BRA_POF_2008_small <- aggregate(cbind(ac_sh_state_wgt, fan_sh_state_wgt, ref_sh_state_wgt, 
                                      urban_sh_state_wgt, ely_access_sh_state_wgt, exp_cap_usd_2011) ~ state3, 
                                data = BRA_POF_2008, FUN = mean)


# Use sp::merge to combine the survey dataframes with spatial polygon object. Note the sequency of arguments
# The all.x=TRUE ensures that all rows in *_spdf (irrespective of whether there is a match in survey dataframes) are retained
# So wherever survey dataframes does not have data for regions, that region will have NA in the merged spatial polygons DF.
# This is imp, otherwise during plotting, those regions (polygons) will dissappear completely and the country 
# map will look strange.
# by.x and by.y refer to the column names in the two dataframes being merged. NAME_1 is the states (of survey dataframes)

BRA_sh_spdf   <- sp::merge(BRA_spdf[,c(1,5)], BRA_POF_2008_small, all.x=TRUE, duplicateGeoms = TRUE, 
                           by.x=c("NAME_1"), by.y=c("state3"))

# For plotting the variables, round off to two signif digits

BRA_sh_spdf@data[,c(3:8)]     <- round(BRA_sh_spdf@data[,c(3:8)], digits=2)

# check the dataframes
BRA_sh_df     <- BRA_sh_spdf@data



## Now, we create the breaks and we cut *_sh_state_* using those breaks, so we generate a categorical vars of intervals

at <- c(0, 10, 20, 50, 75, 100) # Breaks as in Davis and Gertler
at_2 <- c(0, 50, 70, 80, 90, 100) # Breaks for ely_access_sh_state
at_3 <- c(0, 20, 40, 60, 80, 100) # Breaks for urban_sh_state
at_4 <- c(0, 1000, 2000, 5000, 10000, 25000) # Breaks for total exp per adult equiv

# AC
BRA_sh_spdf@data$ac_cut <- cut(BRA_sh_spdf@data$ac_sh_state_wgt, breaks = c(at), include.lowest = TRUE)

# FAN
BRA_sh_spdf@data$fan_cut <- cut(BRA_sh_spdf@data$fan_sh_state_wgt, breaks = c(at), include.lowest = TRUE)

# REF
BRA_sh_spdf@data$REF_cut <- cut(BRA_sh_spdf@data$ref_sh_state_wgt, breaks = c(at), include.lowest = TRUE)

# URBAN
BRA_sh_spdf@data$URB_cut <- cut(BRA_sh_spdf@data$urban_sh_state_wgt, breaks = c(at_3), include.lowest = TRUE)

# ELY_ACCESS
BRA_sh_spdf@data$ELY_ACC_cut <- cut(BRA_sh_spdf@data$ely_access_sh_state, breaks = c(at_2), include.lowest = TRUE)

# EXP CAP
BRA_sh_spdf@data$exp_cut <- cut(BRA_sh_spdf@data$exp_cap_usd_2011, breaks = c(at_4), include.lowest = TRUE)

## Now plot using spplot. Note that the top layer is the variable being plotted. If only this
## layer is plotted, then those polygons (regions) which don't have data (i.e. NA) will be transparent ####
## Thus we plot the full map as a layer underneath to ensure that the regions with missing data appear as white ##
## layer_ comes from library(latticeExtra)

############## COUNTRY PLOTS FOR EXPENDITURE PAE BY STATE ############## 

EXP_BRA <- spplot(BRA_sh_spdf, "exp_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE, 
                  main="")+
  layer_(sp.polygons(BRA_spdf, fill='white'))

png(paste0(map_plot_dir, 'Exp_', BRA,'_POF.png',sep=''), height=6.75, width=7.5, units='in', res=1200)
print(EXP_BRA)
dev.off()


############## COUNTRY PLOTS FOR AC SHARE BY STATE ############## 

AC_BRA <- spplot(BRA_sh_spdf, "ac_cut", col.regions=col, 
                 par.settings=list(axis.line=list(col='transparent')), 
                 colorkey = FALSE, 
                 main="")+
                 layer_(sp.polygons(BRA_spdf, fill='white'))

png(paste0(map_plot_dir, 'Share_of_AC_', BRA,'_POF.png',sep=''), height=6.75, width=7.5, units='in', res=1200)
print(AC_BRA)
dev.off()

############## COUNTRY PLOTS FOR FAN SHARE BY STATE ############## 

FAN_BRA <- spplot(BRA_sh_spdf, "fan_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE, 
                  main="")+
                  layer_(sp.polygons(BRA_spdf, fill='white'))

png(paste0(map_plot_dir, 'Share_of_FAN_', BRA,'_POF.png',sep=''), height=6.75, width=7.5, units='in', res=1200)
print(FAN_BRA)
dev.off()


############## COUNTRY PLOTS FOR REFRIGERATORS' SHARE BY STATE ##############

REF_BRA <- spplot(BRA_sh_spdf, "REF_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE, 
                  main="")+
                  layer_(sp.polygons(BRA_spdf, fill='white'))

png(paste0(map_plot_dir, 'Share_of_REF_', BRA,'_POF.png',sep=''), height=6.75, width=7.5, units='in', res=1200)
print(REF_BRA)
dev.off()


############## COUNTRY PLOTS FOR ELY ACCESS' SHARE BY STATE ##############

ELY_ACC_BRA <- spplot(BRA_sh_spdf, "ELY_ACC_cut", col.regions=col, 
                      par.settings=list(axis.line=list(col='transparent')), 
                      colorkey = FALSE, 
                      main="")+
                      layer_(sp.polygons(BRA_spdf, fill='white'))

png(paste0(map_plot_dir, 'Share_of_ELY_ACC_', BRA,'_POF.png',sep=''), height=6.75, width=7.5, units='in', res=1200)
print(ELY_ACC_BRA)
dev.off()

############## COUNTRY PLOTS FOR URBAN SHARE BY STATE ##############

URB_BRA <- spplot(BRA_sh_spdf, "URB_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE,
                  main="")+
                  layer_(sp.polygons(BRA_spdf, fill='white'))

png(paste0(map_plot_dir, 'Share_of_URB_', BRA,'_POF.png',sep=''), height=6.75, width=7.5, units='in', res=1200)
print(URB_BRA)
dev.off()

#### Legends
# AC/FAN/REF
image.plot(zlim=c(0.1,1), legend.only=TRUE, horizontal =FALSE, add = TRUE,
           legend.shrink = 0.5, smallplot = c(.05,.07,.40,.60),
           breaks = c(0,1,2,3,4,5), nlevel = 5, col = col,   
           axis.args = list(at = 0:5, cex.axis = 0.9, 
                            labels=c("0%", "10%", 
                                     "20%", "50%", "75%", "100%")))
# Urban
image.plot(zlim=c(0.5,1), legend.only=TRUE, horizontal =TRUE, add = TRUE,
           legend.shrink = 1, smallplot = c(.40,.80, .15,.17),
           breaks = c(0,1,2,3,4,5), nlevel = 5, col = col,   
           axis.args = list(at = 0:5, cex.axis = 0.8, 
                            labels=c("0%", "20%", 
                                     "40%", "60%", "80%", "100%")))
# Ely Acc
image.plot(zlim=c(0.5,1), legend.only=TRUE, horizontal =TRUE, add = TRUE,
           legend.shrink = 1, smallplot = c(.40,.80, .15,.17),
           breaks = c(0,1,2,3,4,5), nlevel = 5, col = col,   
           axis.args = list(at = 0:5, cex.axis = 0.8, 
                            labels=c("0%", "50%", 
                                     "70%", "80%", "90%", "100%")))

# Exp cap
image.plot(zlim=c(0.5,1), legend.only=TRUE, horizontal =TRUE, add = TRUE,
           legend.shrink = 1, smallplot = c(.40,.80, .15,.17),
           breaks = c(0,1,2,3,4,5), nlevel = 5, col = col,   
           axis.args = list(at = 0:5, cex.axis = 0.8, 
                            labels=c("0", "1000", 
                                     "2000", "5000", "10000", "25000")))
