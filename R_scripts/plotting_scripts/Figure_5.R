
####### Replication code for Fig. 5 - Historical and projected air-conditioning adoption rates (panel a) ####### 
####### and total final electricity use (panel b) by income decile in the SSP5 RCP8.5 scenario            ####### 

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)
 
# Historical values refer to the latest available wave, Brazil, 2017; India, 2012; Indonesia, 2017; Mexico, 2016). 

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc()                  # frees up memory resources

## Install required R packages if required before loading

library(readxl)
library(readr)
library(plotly)
library(plyr)
library(dplyr)
library(tidyr)
library(xlsx)
library(ggpubr)
library(ggthemes)
library(readstata13)
library(data.table)
library(lattice)
library(latticeExtra)
library(readstata13)
library(scales)
library(fields)
library(grid)
library(gridExtra)
library(fields)
library(weights)
library(reshape)
library(reshape2)
library(ggplot2)

## Set path to work directory

user <- 'username'

if (user=='username') {
  stub <- 'C:/Users/Standard/Google Drive/5-CountryExpertsExchange/Comparative_paper/'
}


dta_dir           <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_data_files/projections', sep='')
plot_dir          <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Figure_5/', sep='')

### Read .dta file ###
FOUR_COUNTRIES_dec                   <- read.dta13(paste(dta_dir,'/HH_4countries_proj_dec.dta', sep='')) ### Library readStata13

##################################################################################################
### Reshape in long format and prepare data for plotting median AC shares by income decile ###
##################################################################################################

FOUR_COUNTRIES_dec.reshape <- melt(FOUR_COUNTRIES_dec,id.vars=c('country','dec_inc'), 
                               measure.vars=c('ac','ac_fut_rcp85_SSP5'))
FOUR_COUNTRIES_dec.reshape$value2<-FOUR_COUNTRIES_dec.reshape$value*100
FOUR_COUNTRIES_dec.reshape$variable <- revalue(FOUR_COUNTRIES_dec.reshape$variable,
                                           c('ac' = 'HIST', 'ac_fut_rcp85_SSP5' = 'RCP85-SSP5'))

BRA_dec.reshape <- filter(FOUR_COUNTRIES_dec.reshape, country=="Brazil")
IND_dec.reshape <- filter(FOUR_COUNTRIES_dec.reshape, country=="India")
MEX_dec.reshape <- filter(FOUR_COUNTRIES_dec.reshape, country=="Mexico")

## Compute median values for each country by decile (hist and fut). 

BRA_dec.reshape_median               <- ddply(BRA_dec.reshape, .(dec_inc, variable), 
                                              summarise, median_value2 = median(value2, na.rm = T))

BRA_dec.reshape_median$median_value2 <- round(BRA_dec.reshape_median$median_value2, digits=2)

IND_dec.reshape_median               <- ddply(IND_dec.reshape, .(dec_inc, variable), 
                                              summarise, median_value2 = median(value2, na.rm = T))

IND_dec.reshape_median$median_value2 <- round(IND_dec.reshape_median$median_value2, digits=2)

MEX_dec.reshape_median               <- ddply(MEX_dec.reshape, .(dec_inc, variable), 
                                              summarise, median_value2 = median(value2, na.rm = T))

MEX_dec.reshape_median$median_value2 <- round(MEX_dec.reshape_median$median_value2, digits=2)


#BRA
ac_diff_bra  <- BRA_dec.reshape_median %>%
                ggplot(aes(y = median_value2, x = factor(dec_inc), fill = factor(variable, levels = c("HIST", "RCP45-SSP2")))) +
                geom_point(mapping = aes(x = factor(dec_inc), y = median_value2, shape = factor(variable), 
                                         group = factor(variable), color = factor(variable)))+
                scale_shape_manual(values = c(1, 16)) +
                scale_color_manual(values=c("darkgray", "#7f7fff")) +
                ylab("")+
                coord_cartesian(ylim = c(0, 100), clip="off") +
                scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
                theme_bw() +
                theme(plot.margin= margin(t = 0.0, r = 0, b = 3.0, l = 0.5, unit = "cm"),
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_line(color="white"),
                      panel.grid.minor=element_blank(),
                      plot.title = element_text(), 
                      text = element_text(), 
                      legend.title = element_blank(),
                      axis.title.x = element_blank(),
                      axis.ticks=element_blank(),
                      axis.text.x=element_blank(),
                      legend.position = "none")

png(paste0(plot_dir, 'Figure_5_panel_A_BRA.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(ac_diff_bra)
dev.off()

## IND
ac_diff_ind  <- IND_dec.reshape_median %>%
                ggplot(aes(y = median_value2, x = factor(dec_inc), fill = factor(variable, levels = c("HIST", "RCP85-SSP5")))) +
                geom_point(mapping = aes(x = factor(dec_inc), y = median_value2, shape = factor(variable), 
                                         group = factor(variable), color = factor(variable)))+
                scale_shape_manual(values = c(1, 16)) +
                scale_color_manual(values=c("darkgray", "#e8bb00")) +
                ylab("")+
                coord_cartesian(ylim = c(0, 100), clip="off") +
                scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
                theme_bw() +
                theme(plot.margin= margin(t = 0.0, r = 0, b = 3.0, l = 0.5, unit = "cm"),
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_line(color="white"),
                      panel.grid.minor=element_blank(),
                      plot.title = element_text(), 
                      text = element_text(), 
                      legend.title = element_blank(),
                      axis.title.x = element_blank(),
                      axis.ticks=element_blank(),
                      axis.text.x=element_blank(),
                      legend.position = "none")

png(paste0(plot_dir, 'Figure_5_panel_A_IND.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(ac_diff_ind)
dev.off()

## MEX
ac_diff_mex  <- MEX_dec.reshape_median %>%
                ggplot(aes(y = median_value2, x = factor(dec_inc), fill = factor(variable, levels = c("HIST", "RCP85-SSP5")))) +
                geom_point(mapping = aes(x = factor(dec_inc), y = median_value2, shape = factor(variable), 
                                         group = factor(variable), color = factor(variable)))+
                scale_shape_manual(values = c(1, 16)) +
                scale_color_manual(values=c("lightgray", "#ff7fff")) +
                ylab("")+
                coord_cartesian(ylim = c(0, 100), clip="off") +
                scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
                theme_bw() +
                theme(plot.margin= margin(t = 0.0, r = 0, b = 3.0, l = 0.5, unit = "cm"),
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_line(color="white"),
                      panel.grid.minor=element_blank(),
                      plot.title = element_text(), 
                      text = element_text(), 
                      legend.title = element_blank(),
                      axis.title.x = element_blank(),
                      axis.ticks=element_blank(),
                      axis.text.x=element_blank(),
                      legend.position = "none")

png(paste0(plot_dir, 'Figure_5_panel_A_MEX.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(ac_diff_mex)
dev.off()

##################################################################################################
### Reshape in long format and prepare data for plotting median ely use (kWh) by income decile ###
##################################################################################################


FOUR_COUNTRIES_dec.reshape <- melt(FOUR_COUNTRIES_dec,id.vars=c('country','dec_inc'), 
                                   measure.vars=c('ely_q','ely_hat_fut_int_rcp85_SSP5_q'))

FOUR_COUNTRIES_dec.reshape$variable <- revalue(FOUR_COUNTRIES_dec.reshape$variable,
                                               c('ely_q' = 'HIST', 'ely_hat_fut_int_rcp85_SSP5_q' = 'RCP85-SSP5'))

BRA_dec.reshape <- filter(FOUR_COUNTRIES_dec.reshape, country=="Brazil")
IND_dec.reshape <- filter(FOUR_COUNTRIES_dec.reshape, country=="India")
MEX_dec.reshape <- filter(FOUR_COUNTRIES_dec.reshape, country=="Mexico")


BRA_dec.reshape_median               <- ddply(BRA_dec.reshape, .(dec_inc, variable), 
                                              summarise, median_value = median(value, na.rm = T))

BRA_dec.reshape_median$median_value <- round(BRA_dec.reshape_median$median_value, digits=2)

IND_dec.reshape_median               <- ddply(IND_dec.reshape, .(dec_inc, variable), 
                                              summarise, median_value = median(value, na.rm = T))

IND_dec.reshape_median$median_value <- round(IND_dec.reshape_median$median_value, digits=2)

MEX_dec.reshape_median               <- ddply(MEX_dec.reshape, .(dec_inc, variable), 
                                              summarise, median_value = median(value, na.rm = T))

MEX_dec.reshape_median$median_value <- round(MEX_dec.reshape_median$median_value, digits=2)

## BRA
ely_diff_bra <- BRA_dec.reshape_median %>%
                ggplot(aes(y = median_value, x = factor(dec_inc), fill = factor(variable, levels = c("HIST", "RCP85-SSP5")))) +
                geom_point(mapping = aes(x = factor(dec_inc), y = median_value, shape = factor(variable), 
                                         group = factor(variable), color = factor(variable)))+
                scale_shape_manual(values = c(1, 16)) +
                scale_color_manual(values=c("darkgray", "#7f7fff")) +
                ylab("")+
                coord_cartesian(ylim = c(0, 10000), clip="off") + 
                theme_bw() +
                theme(plot.margin= margin(t = 0.0, r = 0, b = 3.0, l = 0.5, unit = "cm"),
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_line(color="white"),
                      panel.grid.minor=element_blank(),
                      plot.title = element_text(), 
                      text = element_text(), 
                      legend.title = element_blank(),
                      axis.title.x = element_blank(),
                      axis.ticks=element_blank(),
                      axis.text.x=element_blank(),
                      legend.position = "none")

png(paste0(plot_dir, 'Figure_5_panel_B_BRA.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(ely_diff_bra)
dev.off()

## IND
ely_diff_ind <- IND_dec.reshape_median %>%
                ggplot(aes(y = median_value, x = factor(dec_inc), fill = factor(variable, levels = c("HIST", "RCP85-SSP5")))) +
                geom_point(mapping = aes(x = factor(dec_inc), y = median_value, shape = factor(variable), 
                                         group = factor(variable), color = factor(variable)))+
                scale_shape_manual(values = c(1, 16)) +
                scale_color_manual(values=c("darkgray", "#e8bb00")) +
                ylab("")+
                coord_cartesian(ylim = c(0, 10000), clip="off") + 
                theme_bw() +
                theme(plot.margin= margin(t = 0.0, r = 0, b = 3.0, l = 0.5, unit = "cm"),
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_line(color="white"),
                      panel.grid.minor=element_blank(),
                      plot.title = element_text(), 
                      text = element_text(), 
                      legend.title = element_blank(),
                      axis.title.x = element_blank(),
                      axis.ticks=element_blank(),
                      axis.text.x=element_blank(),
                      legend.position = "none")

png(paste0(plot_dir, 'Figure_5_panel_B_IND.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(ely_diff_ind)
dev.off()

## MEX
ely_diff_mex <- MEX_dec.reshape_median %>%
                ggplot(aes(y = median_value, x = factor(dec_inc), fill = factor(variable, levels = c("HIST", "RCP85-SSP5")))) +
                geom_point(mapping = aes(x = factor(dec_inc), y = median_value, shape = factor(variable), 
                                         group = factor(variable), color = factor(variable)))+
                scale_shape_manual(values = c(1, 16)) +
                scale_color_manual(values=c("darkgray", "#ff7fff")) +
                ylab("")+
                coord_cartesian(ylim = c(0, 10000), clip="off") + 
                theme_bw() +
                theme(plot.margin= margin(t = 0.0, r = 0, b = 3.0, l = 0.5, unit = "cm"),
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_line(color="white"),
                      panel.grid.minor=element_blank(),
                      plot.title = element_text(), 
                      text = element_text(), 
                      legend.title = element_blank(),
                      axis.title.x = element_blank(),
                      axis.ticks=element_blank(),
                      axis.text.x=element_blank(),
                      legend.position = "none")

png(paste0(plot_dir, 'Figure_5_panel_B_MEX.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(ely_diff_mex)
dev.off()


################################################## END ###########################################################



