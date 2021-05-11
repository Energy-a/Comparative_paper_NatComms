
####### Replication code for Fig. S8 Top panel: Predicted air-conditioning adoption rates ####### 
####### in 2040  (between 0 and 1, full adoption). Bottom: Predicted growth rates of      #######  
####### electricity demand.                                                               ####### 

###  Sensitivity to temperature thresholds and to temperature measurement

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)


rm(list=ls(all=TRUE)) # Removes all previously created variables
gc()                  # frees up memory resources

## Install required R packages if required before loading

library(tidyverse)
library(gdata)
library(readstata13)
library(reshape2)
library(RColorBrewer)
library(openxlsx)
library(ggplot2)

## Set path to work directory

user <- 'username'

if (user=='username') {
  stub <- 'C:/Users/Standard/Google Drive/5-CountryExpertsExchange/Comparative_paper/'
}

dta_dir           <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_data_files/projections', sep='')
SI_plot_dir       <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Supplementary_Info/', sep='')



### Read .dta file ###

mex_wb_24  <- read.dta13(paste(dta_dir,'/second_stage_MEX_wb.dta', sep='')) ### Library readStata13
bra_wb_24  <- read.dta13(paste(dta_dir,'/second_stage_BRA_wb.dta', sep='')) ### Library readStata13
ind_wb_24  <- read.dta13(paste(dta_dir,'/second_stage_IND_wb.dta', sep='')) ### Library readStata13

mex_db_24  <- read.dta13(paste(dta_dir,'/second_stage_MEX_db.dta', sep='')) ### Library readStata13
bra_db_24  <- read.dta13(paste(dta_dir,'/second_stage_BRA_db.dta', sep='')) ### Library readStata13
ind_db_24  <- read.dta13(paste(dta_dir,'/second_stage_IND_db.dta', sep='')) ### Library readStata13

mex_wb_22  <- read.dta13(paste(dta_dir,'/second_stage_MEX_wb_22.dta', sep='')) ### Library readStata13
bra_wb_22  <- read.dta13(paste(dta_dir,'/second_stage_BRA_wb_22.dta', sep='')) ### Library readStata13
ind_wb_22  <- read.dta13(paste(dta_dir,'/second_stage_IND_wb_22.dta', sep='')) ### Library readStata13

mex_db_22  <- read.dta13(paste(dta_dir,'/second_stage_MEX_db_22.dta', sep='')) ### Library readStata13
bra_db_22  <- read.dta13(paste(dta_dir,'/second_stage_BRA_db_22.dta', sep='')) ### Library readStata13
ind_db_22  <- read.dta13(paste(dta_dir,'/second_stage_IND_db_22.dta', sep='')) ### Library readStata13




library(dplyr)

mex_wb_24 <- mex_wb_24 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))
bra_wb_24 <- bra_wb_24 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))
ind_wb_24 <- ind_wb_24 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))

mex_db_24 <- mex_db_24 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))
bra_db_24 <- bra_db_24 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))
ind_db_24 <- ind_db_24 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))

mex_wb_22 <- mex_wb_22 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))
bra_wb_22 <- bra_wb_22 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))
ind_wb_22 <- ind_wb_22 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))

mex_db_22 <- mex_db_22 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))
bra_db_22 <- bra_db_22 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))
ind_db_22 <- ind_db_22 %>% select(country, state3, starts_with("ely_gro_int"), starts_with("ac_fut"))

mex_db_24 <- mex_db_24 %>% select(-starts_with("ely_gro_int_ext"))
ind_db_24 <- ind_db_24 %>% select(-starts_with("ely_gro_int_ext"))



#Collapse by state & country

mex_wb_24 <- mex_wb_24 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))
bra_wb_24 <- bra_wb_24 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))
ind_wb_24 <- ind_wb_24 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))

mex_db_24 <- mex_db_24 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))
bra_db_24 <- bra_db_24 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))
ind_db_24 <- ind_db_24 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))

mex_wb_22 <- mex_wb_22 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))
bra_wb_22 <- bra_wb_22 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))
ind_wb_22 <- ind_wb_22 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))

mex_db_22 <- mex_db_22 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))
bra_db_22 <- bra_db_22 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))
ind_db_22 <- ind_db_22 %>% group_by(country, state3) %>% summarise_all(funs(mean(., na.rm = TRUE)))


#Collapse by  country
mex_wb_24 <- mex_wb_24 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))
bra_wb_24 <- bra_wb_24 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))
ind_wb_24 <- ind_wb_24 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))

mex_db_24 <- mex_db_24 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))
bra_db_24 <- bra_db_24 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))
ind_db_24 <- ind_db_24 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))

mex_wb_22 <- mex_wb_22 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))
bra_wb_22 <- bra_wb_22 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))
ind_wb_22 <- ind_wb_22 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))

mex_db_22 <- mex_db_22 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))
bra_db_22 <- bra_db_22 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))
ind_db_22 <- ind_db_22 %>% group_by(country) %>% summarise_all(funs(median(., na.rm = TRUE)))



library(tidyr)
mex_wb_24 <- mex_wb_24 %>% gather(Var, value, -c(country))
bra_wb_24 <- bra_wb_24 %>% gather(Var, value, -c(country))
ind_wb_24 <- ind_wb_24 %>% gather(Var, value, -c(country))

mex_db_24 <- mex_db_24 %>% gather(Var, value, -c(country))
bra_db_24 <- bra_db_24 %>% gather(Var, value, -c(country))
ind_db_24 <- ind_db_24 %>% gather(Var, value, -c(country))

mex_wb_22 <- mex_wb_22 %>% gather(Var, value, -c(country))
bra_wb_22 <- bra_wb_22 %>% gather(Var, value, -c(country))
ind_wb_22 <- ind_wb_22 %>% gather(Var, value, -c(country))

mex_db_22 <- mex_db_22 %>% gather(Var, value, -c(country))
bra_db_22 <- bra_db_22 %>% gather(Var, value, -c(country))
ind_db_22 <- ind_db_22 %>% gather(Var, value, -c(country))

mex_wb_24<-mex_wb_24[-1,]
bra_wb_24<-bra_wb_24[-1,]
ind_wb_24<-ind_wb_24[-1,]

mex_db_24<-mex_db_24[-1,]
bra_db_24<-bra_db_24[-1,]
ind_db_24<-ind_db_24[-1,]

mex_wb_22<-mex_wb_22[-1,]
bra_wb_22<-bra_wb_22[-1,]
ind_wb_22<-ind_wb_22[-1,]

mex_db_22<-mex_db_22[-1,]
bra_db_22<-bra_db_22[-1,]
ind_db_22<-ind_db_22[-1,]



mex_wb_24 <- mex_wb_24 %>% mutate("WB_24deg")
bra_wb_24 <- bra_wb_24 %>% mutate("WB_24deg")
ind_wb_24 <- ind_wb_24 %>% mutate("WB_24deg")

mex_db_24 <- mex_db_24 %>% mutate("DB_24deg")
bra_db_24 <- bra_db_24 %>% mutate("DB_24deg")
ind_db_24 <- ind_db_24 %>% mutate("DB_24deg")

mex_wb_22 <- mex_wb_22 %>% mutate("WB_22deg")
bra_wb_22 <- bra_wb_22 %>% mutate("WB_22deg")
ind_wb_22 <- ind_wb_22 %>% mutate("WB_22deg")

mex_db_22 <- mex_db_22 %>% mutate("DB_22deg")
bra_db_22 <- bra_db_22 %>% mutate("DB_22deg")
ind_db_22 <- ind_db_22 %>% mutate("DB_22deg")


#######AC
ac_mex_wb_24 <- mex_wb_24 %>% filter(str_detect(Var, 'ac') )
ac_bra_wb_24 <- bra_wb_24 %>% filter(str_detect(Var, 'ac') )
ac_ind_wb_24 <- ind_wb_24 %>% filter(str_detect(Var, 'ac') )

ac_mex_db_24 <- mex_db_24 %>% filter(str_detect(Var, 'ac') )
ac_bra_db_24 <- bra_db_24 %>% filter(str_detect(Var, 'ac') )
ac_ind_db_24 <- ind_db_24 %>% filter(str_detect(Var, 'ac') )

ac_mex_wb_22 <- mex_wb_22 %>% filter(str_detect(Var, 'ac') )
ac_bra_wb_22 <- bra_wb_22 %>% filter(str_detect(Var, 'ac') )
ac_ind_wb_22 <- ind_wb_22 %>% filter(str_detect(Var, 'ac') )

ac_mex_db_22 <- mex_db_22 %>% filter(str_detect(Var, 'ac') )
ac_bra_db_22 <- bra_db_22 %>% filter(str_detect(Var, 'ac') )
ac_ind_db_22 <- ind_db_22 %>% filter(str_detect(Var, 'ac') )

ac_mex_wb_24<- ac_mex_wb_24 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")
ac_bra_wb_24<- ac_bra_wb_24 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")
ac_ind_wb_24<- ac_ind_wb_24 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")

ac_mex_db_24<- ac_mex_db_24 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")
ac_bra_db_24<- ac_bra_db_24 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")
ac_ind_db_24<- ac_ind_db_24 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")

ac_mex_wb_22<- ac_mex_wb_22 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")
ac_bra_wb_22<- ac_bra_wb_22 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")
ac_ind_wb_22<- ac_ind_wb_22 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")

ac_mex_db_22<- ac_mex_db_22 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")
ac_bra_db_22<- ac_bra_db_22 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")
ac_ind_db_22<- ac_ind_db_22 %>%  separate(Var, c("var","var1","rcp","ssp"), "_")

#Rename last col
names(ac_mex_wb_24)[7] <- "CDDs"
names(ac_bra_wb_24)[7] <- "CDDs"
names(ac_ind_wb_24)[7] <- "CDDs"

names(ac_mex_db_24)[7] <- "CDDs"
names(ac_bra_db_24)[7] <- "CDDs"
names(ac_ind_db_24)[7] <- "CDDs"

names(ac_mex_db_22)[7] <- "CDDs"
names(ac_bra_db_22)[7] <- "CDDs"
names(ac_ind_db_22)[7] <- "CDDs"

names(ac_mex_wb_22)[7] <- "CDDs"
names(ac_bra_wb_22)[7] <- "CDDs"
names(ac_ind_wb_22)[7] <- "CDDs"

#######Append

ac<-rbind(ac_mex_wb_24, ac_bra_wb_24, ac_ind_wb_24,
          ac_mex_db_24, ac_bra_db_24, ac_ind_db_24,
          ac_mex_wb_22, ac_bra_wb_22, ac_ind_wb_22,
          ac_mex_db_22, ac_bra_db_22, ac_ind_db_22)

is.numeric(ac$value)
ac$value <- as.numeric(as.character(ac$value))


#######PLOT AC

ac_sens <- ggplot(ac, aes(x = CDDs, y = value)) +
  geom_boxplot() +
  theme_classic()+facet_wrap(~ country)

png(paste0(SI_plot_dir, 'Figure_S8_panel_A.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(ac_sens)
dev.off()


#######ELY
ely_mex_wb_24 <- mex_wb_24 %>% filter(str_detect(Var, 'ely') )
ely_bra_wb_24 <- bra_wb_24 %>% filter(str_detect(Var, 'ely') )
ely_ind_wb_24 <- ind_wb_24 %>% filter(str_detect(Var, 'ely') )

ely_mex_db_24 <- mex_db_24 %>% filter(str_detect(Var, 'ely') )
ely_bra_db_24 <- bra_db_24 %>% filter(str_detect(Var, 'ely') )
ely_ind_db_24 <- ind_db_24 %>% filter(str_detect(Var, 'ely') )

ely_mex_wb_22 <- mex_wb_22 %>% filter(str_detect(Var, 'ely') )
ely_bra_wb_22 <- bra_wb_22 %>% filter(str_detect(Var, 'ely') )
ely_ind_wb_22 <- ind_wb_22 %>% filter(str_detect(Var, 'ely') )

ely_mex_db_22 <- mex_db_22 %>% filter(str_detect(Var, 'ely') )
ely_bra_db_22 <- bra_db_22 %>% filter(str_detect(Var, 'ely') )
ely_ind_db_22 <- ind_db_22 %>% filter(str_detect(Var, 'ely') )


ely_mex_wb_24<- ely_mex_wb_24 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")
ely_bra_wb_24<- ely_bra_wb_24 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")
ely_ind_wb_24<- ely_ind_wb_24 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")

ely_mex_db_24<- ely_mex_db_24 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")
ely_bra_db_24<- ely_bra_db_24 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")
ely_ind_db_24<- ely_ind_db_24 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")

ely_mex_wb_22<- ely_mex_wb_22 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")
ely_bra_wb_22<- ely_bra_wb_22 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")
ely_ind_wb_22<- ely_ind_wb_22 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")

ely_mex_db_22<- ely_mex_db_22 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")
ely_bra_db_22<- ely_bra_db_22 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")
ely_ind_db_22<- ely_ind_db_22 %>%  separate(Var, c("var","var1","var2","rcp","ssp","q"), "_")


#Rename last col
names(ely_mex_wb_24)[9] <- "CDDs"
names(ely_bra_wb_24)[9] <- "CDDs"
names(ely_ind_wb_24)[9] <- "CDDs"

names(ely_mex_db_24)[9] <- "CDDs"
names(ely_bra_db_24)[9] <- "CDDs"
names(ely_ind_db_24)[9] <- "CDDs"

names(ely_mex_db_22)[9] <- "CDDs"
names(ely_bra_db_22)[9] <- "CDDs"
names(ely_ind_db_22)[9] <- "CDDs"

names(ely_mex_wb_22)[9] <- "CDDs"
names(ely_bra_wb_22)[9] <- "CDDs"
names(ely_ind_wb_22)[9] <- "CDDs"

#######Append

ely<-rbind(ely_mex_wb_24, ely_bra_wb_24, ely_ind_wb_24,
           ely_mex_db_24, ely_bra_db_24, ely_ind_db_24,
           ely_mex_wb_22, ely_bra_wb_22, ely_ind_wb_22, 
           ely_mex_db_22, ely_bra_db_22, ely_ind_db_22)

is.numeric(ely$value)
ely$value <- as.numeric(as.character(ely$value))


#######PLOT ely


ely_sens <- ggplot(ely, aes(x = CDDs, y = value)) +
  geom_boxplot() +
  theme_classic()+facet_wrap(~ country)

png(paste0(SI_plot_dir, 'Figure_S8_panel_B.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(ely_sens)
dev.off()


################################################## END ###########################################################

