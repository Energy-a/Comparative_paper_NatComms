
####### Replication code for Fig. S6 - District maps of future AC level for the four countries (MEX, IDN, IND and BRA) ####### 

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)


## Read the database after projections and merge each country with boundaries shapefile at district level. 
## Then we plot the AC future shares by district

## For Brazil we merge at state level

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
library(tidyr)
library(latticeExtra)
library(sp)
library(grid)
library(gridExtra)
library(fields)
library(stringr)

user <- 'username'

if (user=='username') {
  stub <- 'C:/Users/Standard/Google Drive/5-CountryExpertsExchange/Comparative_paper/'
}

dta_dir              <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_data_files/projections', sep='')
SI_plot_dir          <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Supplementary_Info/', sep='')
shpf_dir             <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/shape_files/', sep='')


### Read .dta file ###

FOUR_COUNTRIES_state                   <- read.dta13(paste(dta_dir,'/HH_4countries_proj_state.dta', sep='')) ### Library readStata13
FOUR_COUNTRIES_dist                    <- read.dta13(paste(dta_dir,'/HH_4countries_proj_district.dta', sep='')) ### Library readStata13
MEX_ENIGH                              <- FOUR_COUNTRIES_dist %>% filter(country == "Mexico")
IND_NSS                                <- FOUR_COUNTRIES_dist %>% filter(country == "India")
BRA_POF                                <- FOUR_COUNTRIES_state %>% filter(country == "Brazil")

### Read district .shp files ###
# NB: Since for India there are changes in district borders, I used two different shapefiles
#     namely shp 2001 for NSS 2004; shp 2011 for NSS 2012

MEX_spdf   <- shapefile(paste(shpf_dir,'Mexico/Adm_level_2_municipalities/Muni_2012gw.shp', sep='')) # Mexico

IND_spdf   <- shapefile(paste(shpf_dir,'India/Adm_level_2_districts/2001_Dist.shp', sep='')) # India

BRA_spdf   <- shapefile(paste(shpf_dir,'Brazil/Adm_level_1_states/BRA_adm1.shp', sep='')) # Brazil


# Save shapefile data to check state and district ID and names

MEX_df     <- MEX_spdf@data
colnames(MEX_spdf@data) # CVE_ENT and CVE_MUN (Entities codes and municipalities codes)
IND_df     <- IND_spdf@data
colnames(IND_spdf@data) # ST_CEN_CD and DT_CEN_CD (Entities codes and municipalities codes)
BRA_df     <- BRA_spdf@data
colnames(BRA_spdf@data) # CVE_ENT and CVE_MUN (Entities codes and municipalities codes)


## Combine entity and municip. codes since for each entity municipalities to have an equal format column in both the 
## the shapefile and survey data

# Mexico
MEX_spdf@data$state_district <- paste(MEX_spdf@data$CVE_ENT, MEX_spdf@data$CVE_MUN, sep=" - ")

# India
IND_spdf@data$state_district <- paste(IND_spdf@data$ST_CEN_CD, IND_spdf@data$DT_CEN_CD, sep=" - ")


### NB: ALL SHARES ARE ALREADY WEIGHTED! ###


######## CREATE THE DATABASE FOR MAPS ######## 

at <- c(0, 10, 20, 50, 75, 100) # Breaks
col <- c("#dadaeb", "#bcbddc", "#9e9ac8", "#756bb1", "#54278f")

####### MEXICO - ENIGH #######

MEX_ENIGH_proj <- aggregate(cbind(ac_sh_dst_wgt, ac_fut_dst_rcp85_SSP5) ~ state_district, data = MEX_ENIGH, FUN = mean)
names(MEX_ENIGH_proj)       <- c("state_district", "AC 2016", "AC RCP8.5-SSP5")

# Use sp::merge to combine the MEX_ENIGH_* with spatial polygon object. Note the sequency of arguments
# The all.x=TRUE ensures that all rows in MEX_spdf (irrespective of whether there is a match in MEX_ENIGH_*) are retained
# So wherever MEX_ENIGH_* does not have data for regions, that region will have NA in the merged spatial polygons DF.
# This is imp, otherwise during plotting, those regions (polygons) will dissappear completely and the country 
# map will look strange.
# by.x and by.y refer to the column names in the two dataframes being merged. MEX_spdf's state_district is the state_district of MEX_ENIGH


MEX_AC_spdf   <- sp::merge(MEX_spdf[,c(1,7)], MEX_ENIGH_proj, all.x=TRUE, duplicateGeoms = TRUE, 
                           by.x=c("state_district"), by.y=c("state_district"))


# For plotting the variables, round off to two signif digits
MEX_AC_df     <- MEX_AC_spdf@data
MEX_AC_spdf@data[,c(3:4)] <- round(MEX_AC_spdf@data[,c(3:4)], digits=2)

# AC cuts
MEX_AC_spdf@data$ac_cut <- cut(MEX_AC_spdf@data$`AC 2016`, breaks = c(at), include.lowest = TRUE)
MEX_AC_spdf@data$ac_cut_proj <- cut(MEX_AC_spdf@data$`AC RCP8.5-SSP5`, breaks = c(at), include.lowest = TRUE)

# MAP (2016)
AC_MEX <- spplot(MEX_AC_spdf, "ac_cut", col.regions=col, 
                 par.settings=list(axis.line=list(col='transparent')), 
                 colorkey = FALSE, 
                 main="")+
                 layer_(sp.polygons(MEX_spdf, fill='white'))

AC_MEX

png(paste0(SI_plot_dir, 'AC_MEX_2016.png',sep=''), height=6.75, width=7.5, units='in',res=1200)
print(AC_MEX)
dev.off()

# MAP (RCP8.5-SSP5)
AC_MEX_PROJ <- spplot(MEX_AC_spdf, "ac_cut_proj", col.regions=col, 
                      par.settings=list(axis.line=list(col='transparent')), 
                      colorkey = FALSE, 
                      main="")+
                      layer_(sp.polygons(MEX_spdf, fill='white'))

AC_MEX_PROJ

png(paste0(SI_plot_dir, 'AC_MEX_RCP85_SSP5.png',sep=''), height=6.75, width=7.5, units='in',res=1200)
print(AC_MEX_PROJ)
dev.off()


####### INDIA - NSS #######

### We create the .dta database we will use for the merge with shp file
### We reduce .dta size for each waves
### Namely the NSS INDIA district shares of AC

IND_NSS_proj <- aggregate(cbind(ac_sh_dst_wgt, ac_fut_dst_rcp85_SSP5) ~ state_district, data = IND_NSS, FUN = mean)
names(IND_NSS_proj)       <- c("state_district", "AC 2012", "AC RCP8.5-SSP5")

# Use sp::merge to combine the IND_NSS_* with spatial polygon object. Note the sequency of arguments
# The all.x=TRUE ensures that all rows in MEX_spdf (irrespective of whether there is a match in IND_NSS_*) are retained
# So wherever IND_NSS_* does not have data for regions, that region will have NA in the merged spatial polygons DF.
# This is imp, otherwise during plotting, those regions (polygons) will dissappear completely and the country 
# map will look strange.
# by.x and by.y refer to the column names in the two dataframes being merged. MEX_spdf's state_district is the state_district of IND_NSS


IND_AC_spdf   <- sp::merge(IND_spdf[,c(1,5)], IND_NSS_proj, all.x=TRUE, duplicateGeoms = TRUE, 
                           by.x=c("state_district"), by.y=c("state_district"))

# For plotting the variables, round off to two signif digits

IND_AC_df     <- IND_AC_spdf@data
IND_AC_spdf@data[,c(3:4)] <- round(IND_AC_spdf@data[,c(3:4)], digits=2)

# AC cuts
IND_AC_spdf@data$ac_cut <- cut(IND_AC_spdf@data$`AC 2012`, breaks = c(at), include.lowest = TRUE)
IND_AC_spdf@data$ac_cut_proj <- cut(IND_AC_spdf@data$`AC RCP8.5-SSP5`, breaks = c(at), include.lowest = TRUE)

# MAP (2012)
AC_IND <- spplot(IND_AC_spdf, "ac_cut", col.regions=col, 
                 par.settings=list(axis.line=list(col='transparent')), 
                 colorkey = FALSE, 
                 main="")+
                 layer_(sp.polygons(IND_spdf, fill='white'))

AC_IND

png(paste0(SI_plot_dir, 'AC_IND_2012.png',sep=''), height=6.75, width=7.5, units='in',res=1200)
print(AC_IND)
dev.off()

# MAP (RCP8.5-SSP5)
AC_IND_PROJ <- spplot(IND_AC_spdf, "ac_cut_proj", col.regions=col, 
                      par.settings=list(axis.line=list(col='transparent')), 
                      colorkey = FALSE, 
                      main="")+
                      layer_(sp.polygons(IND_spdf, fill='white'))

AC_IND_PROJ

png(paste0(SI_plot_dir, 'AC_IND_RCP85_SSP5.png',sep=''), height=6.75, width=7.5, units='in',res=1200)
print(AC_IND_PROJ)
dev.off()


####### BRAZIL - POF #######

### We create the .dta database we will use for the merge with shp file
### We reduce .dta size for each waves
### Namely the POF BRA district shares of AC

BRA_POF_proj <- aggregate(cbind(ac_sh_state, ac_fut_sta_rcp85_SSP5) ~ state3, data = BRA_POF, FUN = mean)
names(BRA_POF_proj)       <- c("state3", "AC 2017",  "AC RCP8.5-SSP5")

# Use sp::merge to combine the BRA_POF_* with spatial polygon object. Note the sequency of arguments
# The all.x=TRUE ensures that all rows in MEX_spdf (irrespective of whether there is a match in BRA_POF_*) are retained
# So wherever BRA_POF_* does not have data for regions, that region will have NA in the merged spatial polygons DF.
# This is imp, otherwise during plotting, those regions (polygons) will dissappear completely and the country 
# map will look strange.
# by.x and by.y refer to the column names in the two dataframes being merged. MEX_spdf's state_district is the state_district of BRA_POF

BRA_AC_spdf   <- sp::merge(BRA_spdf[,c(1,4)], BRA_POF_proj, all.x=TRUE, duplicateGeoms = TRUE, 
                           by.x=c("NAME_1"), by.y=c("state3"))


# For plotting the variables, round off to two signif digits
BRA_AC_df     <- BRA_AC_spdf@data
BRA_AC_spdf@data[,c(3:4)]     <- round(BRA_AC_spdf@data[,c(3:4)], digits=2)

# AC cuts
BRA_AC_spdf@data$ac_cut <- cut(BRA_AC_spdf@data$`2017`, breaks = c(at), include.lowest = TRUE)
BRA_AC_spdf@data$ac_cut_proj <- cut(BRA_AC_spdf@data$`RCP8.5-SSP5`, breaks = c(at), include.lowest = TRUE)

# MAP (2008)
AC_BRA <- spplot(BRA_AC_spdf, "ac_cut", col.regions=col, 
                 par.settings=list(axis.line=list(col='transparent')), 
                 colorkey = FALSE, 
                 main="")+
                 layer_(sp.polygons(BRA_spdf, fill='white'))

AC_BRA

png(paste0(SI_plot_dir, 'AC_BRA_2017.png',sep=''), height=6.75, width=7.5, units='in',res=1200)
print(AC_BRA)
dev.off()

# MAP (RCP8.5-SSP5)
AC_BRA_PROJ  <- spplot(BRA_AC_spdf, "ac_cut_proj", col.regions=col, 
                       par.settings=list(axis.line=list(col='transparent')), 
                       colorkey = FALSE, 
                       main="")+
                      layer_(sp.polygons(BRA_spdf, fill='white'))

AC_BRA_PROJ

png(paste0(SI_plot_dir, 'AC_BRA_RCP85_SSP5.png',sep=''), height=6.75, width=7.5, units='in',res=1200)
print(AC_BRA_PROJ)
dev.off()

################################################## END ###########################################################