
####### Replication code for Fig. S1 - Four countries maps at district level (Mexico 2012 and India 2012) ####### 

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

### Maps for the four countries (MEX, IDN, IND and BRA) using both survey (2012, for BRA 2008) and Climate dataframes ###
### Here we map the share of Air Conditioning, Fan, Ref, HH, Ely access and urbanisation by State in each country ###
### Each share is given by dividing: (n°households with AC in state i/n°households in state i)*100 ###

### Here we plot only Mexico 2012 and India 2012! ###

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
SI_plot_dir          <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Supplementary_Info/', sep='')
shpf_dir             <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/shape_files/', sep='')

### Read .dta file ###

FOUR_COUNTRIES                         <- read.dta13(paste(dta_dir,'/HH_4countries_agg_district.dta', sep='')) ### Library readStata13
MEX_ENIGH                              <- FOUR_COUNTRIES %>% filter(country == "Mexico")
IND_NSS                                <- FOUR_COUNTRIES %>% filter(country == "India")

### Read district .shp files ###
MEX_spdf   <- shapefile(paste(shpf_dir,'Mexico/Adm_level_2_municipalities/Muni_2012gw.shp', sep='')) # Mexico

IND_spdf   <- shapefile(paste(shpf_dir,'India/Adm_level_2_districts/2001_Dist.shp', sep='')) # India

# Save shapefile data to check state and district ID and names

MEX_df     <- MEX_spdf@data
colnames(MEX_spdf@data) # CVE_ENT and CVE_MUN (Entities codes and municipalities codes)
IND_df     <- IND_spdf@data
colnames(IND_spdf@data) # ST_CEN_CD and DT_CEN_CD (Entities codes and municipalities codes)

## Combine entity and municip. codes since for each entity municipalities to have an equal format column in both the 
## the shapefile and survey data

# Mexico
MEX_spdf@data$state_district <- paste(MEX_spdf@data$CVE_ENT, MEX_spdf@data$CVE_MUN, sep=" - ")

# India
IND_spdf@data$state_district <- paste(IND_spdf@data$ST_CEN_CD, IND_spdf@data$DT_CEN_CD, sep=" - ")

### NB: ALL SHARES ARE ALREADY WEIGHTED! ###


######## CREATE THE DATABASE FOR MAPS ######## 

# Create breaks and colour
at <- c(0, 10, 20, 50, 75, 100) # Breaks as in Davis and Gertler
at_2 <- c(0, 50, 70, 80, 90, 100) # Breaks for ely_access_sh_state
at_3 <- c(0, 20, 40, 60, 80, 100) # Breaks for urban_sh_state
at_4 <- c(0, 1000, 2000, 5000, 10000, 25000) # Breaks for total exp per adult equiv
col <- c("#dadaeb", "#bcbddc", "#9e9ac8", "#756bb1", "#54278f")

####### MEXICO - ENIGH #######

### We create the .dta database we will use for the merge with shp file
### We reduce .dta size for each waves
### Namely the ENIGH MEX district shares of AC
# 2012

MEX_ENIGH_2012       <- MEX_ENIGH %>% filter(year == 2012)
MEX_ENIGH_2012_small <- aggregate(cbind(ac_sh_state_wgt, fan_sh_state_wgt, ref_sh_state_wgt, 
                                        urban_sh_state_wgt, exp_cap_usd_2011, ely_access_sh_state_wgt) 
                                  ~ state_district,
                                  data = MEX_ENIGH_2012, FUN = mean)
names(MEX_ENIGH_2012_small) <- c("state_district", "AC_2012", "Fan_2012", "Ref_2012", "Urb_2012", 
                                 "Exp_cap_2012", "Ely_Access_2012")

# Use sp::merge to combine the MEX_ENIGH_* with spatial polygon object. Note the sequency of arguments
# The all.x=TRUE ensures that all rows in MEX_spdf (irrespective of whether there is a match in MEX_ENIGH_*) are retained
# So wherever MEX_ENIGH_* does not have data for regions, that region will have NA in the merged spatial polygons DF.
# This is imp, otherwise during plotting, those regions (polygons) will dissappear completely and the country 
# map will look strange.
# by.x and by.y refer to the column names in the two dataframes being merged. MEX_spdf's state_district is the state_district of MEX_ENIGH


#MEX_AC_FAN_REF_spdf   <- sp::merge(MEX_spdf[,c(1,7)], MEX_ENIGH_small, all.x=TRUE, duplicateGeoms = TRUE, 
#                                   by.x=c("state_district"), by.y=c("state_district"))
MEX_AC_FAN_REF_spdf   <- sp::merge(MEX_spdf[,c(1,7)], MEX_ENIGH_2012_small, all.x=TRUE, duplicateGeoms = TRUE, 
                                   by.x=c("state_district"), by.y=c("state_district"))


# For plotting the variables, round off to two signif digits

MEX_AC_FAN_REF_df     <- MEX_AC_FAN_REF_spdf@data

MEX_AC_FAN_REF_spdf@data[,c(3:8)]     <- round(MEX_AC_FAN_REF_spdf@data[,c(3:8)], digits=2)

# Create Map categories
MEX_AC_FAN_REF_spdf@data$ac_cut     <- cut(MEX_AC_FAN_REF_spdf@data$AC_2012, breaks = c(at), include.lowest = TRUE) # AC
MEX_AC_FAN_REF_spdf@data$fan_cut    <- cut(MEX_AC_FAN_REF_spdf@data$Fan_2012, breaks = c(at), include.lowest = TRUE) # FAN
MEX_AC_FAN_REF_spdf@data$ref_cut    <- cut(MEX_AC_FAN_REF_spdf@data$Ref_2012, breaks = c(at), include.lowest = TRUE) # REF
MEX_AC_FAN_REF_spdf@data$urb_cut    <- cut(MEX_AC_FAN_REF_spdf@data$Urb_2012, breaks = c(at_3), include.lowest = TRUE) # URB
MEX_AC_FAN_REF_spdf@data$ely_ac_cut <- cut(MEX_AC_FAN_REF_spdf@data$Ely_Access_2012, breaks = c(at_2), include.lowest = TRUE) # ELY_ACC
MEX_AC_FAN_REF_spdf@data$exp_cap_cut <- cut(MEX_AC_FAN_REF_spdf@data$Exp_cap_2012, breaks = c(at_4), include.lowest = TRUE) # ELY_ACC


## AC
AC_MEX <- spplot(MEX_AC_FAN_REF_spdf, "ac_cut", col.regions=col, 
                 par.settings=list(axis.line=list(col='transparent')), 
                 colorkey = FALSE, main="")+
                layer_(sp.polygons(MEX_spdf, fill='white'))

AC_MEX

png(paste0(SI_plot_dir, 'AC_ENIGH.png',sep=''), height=6.75, width=7.5, units='in', res=1200)
print(AC_MEX)
dev.off()

## FAN
FAN_MEX <- spplot(MEX_AC_FAN_REF_spdf, "fan_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE, main="")+
                  layer_(sp.polygons(MEX_spdf, fill='white'))

FAN_MEX

## REF
REF_MEX <- spplot(MEX_AC_FAN_REF_spdf, "ref_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE, main="")+
                  layer_(sp.polygons(MEX_spdf, fill='white'))

REF_MEX

## URB
URB_MEX <- spplot(MEX_AC_FAN_REF_spdf, "urb_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE, main="")+
                  layer_(sp.polygons(MEX_spdf, fill='white'))

URB_MEX

## ELY_ACC
ELY_AC_MEX <- spplot(MEX_AC_FAN_REF_spdf, "ely_ac_cut", col.regions=col, 
                     par.settings=list(axis.line=list(col='transparent')), 
                     colorkey = FALSE, main="")+
                     layer_(sp.polygons(MEX_spdf, fill='white'))

ELY_AC_MEX

## EXP_CAP
EXP_CAP_MEX <- spplot(MEX_AC_FAN_REF_spdf, "exp_cap_cut", col.regions=col, 
                      par.settings=list(axis.line=list(col='transparent')), 
                      colorkey = FALSE, main="")+
                      layer_(sp.polygons(MEX_spdf, fill='white'))

EXP_CAP_MEX


####### INDIA - NSS #######

### We create the .dta database we will use for the merge with shp file
### We reduce .dta size for each waves
### Namely the NSS INDIA district shares of AC

# 2012

IND_NSS_2012                    <- IND_NSS %>% filter(year == 2012)
IND_NSS_2012_small <- aggregate(cbind(ac_sh_state_wgt, fan_sh_state_wgt, ref_sh_state_wgt, 
                                      urban_sh_state_wgt, exp_cap_usd_2011, ely_access_sh_state_wgt) 
                                ~ state_district,
                                  data = IND_NSS_2012, FUN = mean)
names(IND_NSS_2012_small) <- c("state_district", "AC_2012", "Fan_2012", "Ref_2012", "Urb_2012", 
                               "Exp_cap_2012", "Ely_Access_2012")

# Use sp::merge to combine the IND_NSS_* with spatial polygon object. Note the sequency of arguments
# The all.x=TRUE ensures that all rows in MEX_spdf (irrespective of whether there is a match in IND_NSS_*) are retained
# So wherever IND_NSS_* does not have data for regions, that region will have NA in the merged spatial polygons DF.
# This is imp, otherwise during plotting, those regions (polygons) will dissappear completely and the country 
# map will look strange.
# by.x and by.y refer to the column names in the two dataframes being merged. MEX_spdf's state_district is the state_district of IND_NSS

IND_AC_FAN_REF_spdf   <- sp::merge(IND_spdf[,c(1,5)], IND_NSS_2012_small, all.x=TRUE, duplicateGeoms = TRUE, 
                                   by.x=c("state_district"), by.y=c("state_district"))

# For plotting the variables, round off to two signif digits

IND_AC_FAN_REF_df     <- IND_AC_FAN_REF_spdf@data

#IND_AC_FAN_REF_spdf@data[,c(3:10)]     <- round(IND_AC_FAN_REF_spdf@data[,c(3:10)], digits=2)
IND_AC_FAN_REF_spdf@data[,c(3:8)]     <- round(IND_AC_FAN_REF_spdf@data[,c(3:8)], digits=2)

# Create Map categories
IND_AC_FAN_REF_spdf@data$ac_cut     <- cut(IND_AC_FAN_REF_spdf@data$AC_2012, breaks = c(at), include.lowest = TRUE) # AC
IND_AC_FAN_REF_spdf@data$fan_cut    <- cut(IND_AC_FAN_REF_spdf@data$Fan_2012, breaks = c(at), include.lowest = TRUE) # FAN
IND_AC_FAN_REF_spdf@data$ref_cut    <- cut(IND_AC_FAN_REF_spdf@data$Ref_2012, breaks = c(at), include.lowest = TRUE) # REF
IND_AC_FAN_REF_spdf@data$urb_cut    <- cut(IND_AC_FAN_REF_spdf@data$Urb_2012, breaks = c(at_3), include.lowest = TRUE) # URB
IND_AC_FAN_REF_spdf@data$ely_ac_cut <- cut(IND_AC_FAN_REF_spdf@data$Ely_Access_2012, breaks = c(at_2), include.lowest = TRUE) # ELY_ACC
IND_AC_FAN_REF_spdf@data$exp_cap_cut <- cut(IND_AC_FAN_REF_spdf@data$Exp_cap_2012, breaks = c(at_4), include.lowest = TRUE) # ELY_ACC


## AC
AC_IND <- spplot(IND_AC_FAN_REF_spdf, "ac_cut", col.regions=col, 
                 par.settings=list(axis.line=list(col='transparent')), 
                 colorkey = FALSE, main="")+
                 layer_(sp.polygons(IND_spdf, fill='white'))

AC_IND

## FAN
FAN_IND <- spplot(IND_AC_FAN_REF_spdf, "fan_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE, main="")+
                  layer_(sp.polygons(IND_spdf, fill='white'))

FAN_IND

## REF
REF_IND <- spplot(IND_AC_FAN_REF_spdf, "ref_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE, main="")+
                  layer_(sp.polygons(IND_spdf, fill='white'))

REF_IND

## URB
URB_IND <- spplot(IND_AC_FAN_REF_spdf, "urb_cut", col.regions=col, 
                  par.settings=list(axis.line=list(col='transparent')), 
                  colorkey = FALSE, main="")+
                  layer_(sp.polygons(IND_spdf, fill='white'))

URB_IND

## ELY_ACC
ELY_AC_IND <- spplot(IND_AC_FAN_REF_spdf, "ely_ac_cut", col.regions=col, 
                     par.settings=list(axis.line=list(col='transparent')), 
                     colorkey = FALSE, main="")+
                     layer_(sp.polygons(IND_spdf, fill='white'))

ELY_AC_IND

## EXP_CAP
EXP_CAP_IND <- spplot(IND_AC_FAN_REF_spdf, "exp_cap_cut", col.regions=col, 
                      par.settings=list(axis.line=list(col='transparent')), 
                      colorkey = FALSE, main="")+
                      layer_(sp.polygons(IND_spdf, fill='white'))

EXP_CAP_IND
