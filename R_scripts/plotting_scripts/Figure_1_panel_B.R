
####### Replication code for Fig. 1 - Panel B Local regression plots for the  ####### 
####### four countries (MEX, IDN, IND and BRA) using survey data (second wave)####### 

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

## Notice that the data here loaded have been created using Figure_1_panel_B.do file in STATA

## Expenditure is computed in costant 2011 dollars ($) using current PPP for each survey and US consumer price index (CPI)
## We have to use two different aes -> note inherit.aes is fundamental to plot two different kind of plots (line and density)
## We use na.omit to avoid possible NAs. NB. Stata usually does it by itself, R not!
## We use the same different colours for the three appliances AC, FAN and REF
## As we have to identify the three vars in the same plot (look at the below one), 
## we use #FF1E42 (AC), #003943 (FAN) and #00AC8B (REF)
## http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=6


rm(list=ls(all=TRUE)) # Removes all previously created variables
gc()                  # frees up memory resources

## Install required R packages if required before loading

library(data.table)
library(plyr)
library(dplyr) 
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(ggpubr)
library(plotly)
library(readstata13)
library(scales)
library(fields)
library(grid)
library(gridExtra)

## Set path to work directory

user <- 'username'

if (user=='username') {
    stub <- 'C:/Users/Standard/Google Drive/5-CountryExpertsExchange/Comparative_paper/'
}

dta_dir              <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_data_files/Figure_1/', sep='')
line_plot_dir        <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Figure_1/', sep='')


### Read .dta file from Figure_1_panel_B.do ###
FOUR_COUNTRIES                <- read.dta13(paste(dta_dir,'/kdens_local_reg_own.dta', sep='')) ### Library readStata13


######## AC ownership - Fan ownership - Household density - by EXPENDITURE ########

### AC-FAN-REF cumulative + kernel density (by country) in 2012 (and 2008)

# BRA
BRA <- FOUR_COUNTRIES %>% filter(country == "Brazil") %>% filter(year == 2008)
BRA_local_AC_FAN_REF_kd  <- ggplot(data = na.omit(subset(BRA, 
                                                         select = c(exp_cap_usd_2011, country, local_ac))),
                                   aes(x = exp_cap_usd_2011, colour = "AC")) + 
                            geom_line(aes(y = local_ac), size = 2) +
                            geom_line(data = na.omit(subset(BRA, 
                                                            select = c(exp_cap_usd_2011, country, local_fan))), 
                                      aes(x = exp_cap_usd_2011, y = local_fan, 
                                          colour = "Fan"), inherit.aes = FALSE, size = 2) +
                            geom_line(data = na.omit(subset(BRA, 
                                                            select = c(exp_cap_usd_2011, country, local_ref))), 
                                      aes(x = exp_cap_usd_2011, y = local_ref, 
                                          colour = "Refrigerator"), inherit.aes = FALSE, size = 2) +
                            geom_line(data = na.omit(subset(BRA, 
                                                            select = c(kernel_exp, density, country))), 
                                      aes(x = kernel_exp, y = density, 
                                          colour = "HH Density"), inherit.aes = FALSE, linetype = "dashed", size = 1) +
                            scale_color_manual(values = c('AC' = '#fc8d62', 'Fan' = '#66c2a5',
                                                          'Refrigerator' = '#8da0cb','HH Density' = "black")) +
                                  scale_x_log10(breaks = c(0, 1000, 10000, 100000), labels = comma) + 
                            coord_cartesian(xlim = c(500, 100000), ylim = c(0,1)) + # change scale for zooming in/out
                            scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)) +
                            xlab("") +
                            ylab("") +
                            # ggtitle("Ownership Rates by Consumption Level (Log scale): Air Conditioning, Fan and Refrigerator.") +
                            theme_bw() +
                            theme(panel.background=element_blank(),
                                  panel.border=element_blank(),
                                  panel.grid.major=element_line(color="lightgray"),
                                  panel.grid.minor=element_blank(),
                                  plot.title = element_text(size = 20, family = "Tahoma", hjust = 0.5), 
                                  text = element_text(size = 20, family = "Tahoma",color="black"),
                                  legend.title=element_blank(),
                                  legend.position = "none")
png(paste0(line_plot_dir, 'BRA_local_AC_FAN_REF_kd.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(BRA_local_AC_FAN_REF_kd)
dev.off()

#66c2a5 - FAN
#fc8d62 - ac
#8da0cb - REF


# IND
IND <- FOUR_COUNTRIES %>% filter(country == "India") %>% filter(year == 2012)
IND_local_AC_FAN_REF_kd  <- ggplot(data = na.omit(subset(IND, 
                                                         select = c(exp_cap_usd_2011, country, local_ac))),
                                   aes(x = exp_cap_usd_2011, colour = "AC")) + 
                            geom_line(aes(y = local_ac), size = 2) +
                            geom_line(data = na.omit(subset(IND, 
                                                            select = c(exp_cap_usd_2011, country, local_fan))), 
                                      aes(x = exp_cap_usd_2011, y = local_fan, 
                                          colour = "Fan"), inherit.aes = FALSE, size = 2) +
                            geom_line(data = na.omit(subset(IND, 
                                                            select = c(exp_cap_usd_2011, country, local_ref))), 
                                      aes(x = exp_cap_usd_2011, y = local_ref, 
                                          colour = "Refrigerator"), inherit.aes = FALSE, size = 2) +
                            geom_line(data = na.omit(subset(IND, 
                                                            select = c(kernel_exp, density, country))), 
                                      aes(x = kernel_exp, y = density, 
                                          colour = "HH Density"), inherit.aes = FALSE, linetype = "dashed", size = 1) +
                            scale_color_manual(values = c('AC' = '#fc8d62', 'Fan' = '#66c2a5',
                                                          'Refrigerator' = '#8da0cb','HH Density' = "black")) +
                            scale_x_log10(breaks = c(0, 1000, 10000, 100000), labels = comma) + 
                            coord_cartesian(xlim = c(500, 100000), ylim = c(0,1)) + # change scale for zooming in/out
                            scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)) +
                            xlab("") +
                            ylab("") +
                            #                           ggtitle("Ownership Rates by Consumption Level (Log scale): Air Conditioning, Fan and Refrigerator.") +
                            theme_bw() +
                            theme(panel.background=element_blank(),
                                  panel.border=element_blank(),
                                  panel.grid.major=element_line(color="lightgray"),
                                  panel.grid.minor=element_blank(),
                                  plot.title = element_text(size = 20, family = "Tahoma", hjust = 0.5), 
                                  text = element_text(size = 20, family = "Tahoma",color="black"),
                                  legend.title=element_blank(),
                                  legend.position = "none")


png(paste0(line_plot_dir, 'IND_local_AC_FAN_REF_kd.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(IND_local_AC_FAN_REF_kd)
dev.off()


# MEX
MEX <- FOUR_COUNTRIES %>% filter(country == "Mexico") %>% filter(year == 2012)
MEX_local_AC_FAN_REF_kd  <- ggplot(data = na.omit(subset(MEX, 
                                                         select = c(exp_cap_usd_2011, country, local_ac))),
                                   aes(x = exp_cap_usd_2011, colour = "AC")) + 
                            geom_line(aes(y = local_ac), size = 2) +
                            geom_line(data = na.omit(subset(MEX, 
                                                            select = c(exp_cap_usd_2011, country, local_fan))), 
                                      aes(x = exp_cap_usd_2011, y = local_fan, 
                                          colour = "Fan"), inherit.aes = FALSE, size = 2) +
                            geom_line(data = na.omit(subset(MEX, 
                                                            select = c(exp_cap_usd_2011, country, local_ref))), 
                                      aes(x = exp_cap_usd_2011, y = local_ref, 
                                          colour = "Refrigerator"), inherit.aes = FALSE, size = 2) +
                            geom_line(data = na.omit(subset(MEX, 
                                                            select = c(kernel_exp, density, country))), 
                                      aes(x = kernel_exp, y = density, 
                                          colour = "HH Density"), inherit.aes = FALSE, linetype = "dashed", size = 1) +
                            scale_color_manual(values = c('AC' = '#fc8d62', 'Fan' = '#66c2a5',
                                                          'Refrigerator' = '#8da0cb','HH Density' = "black")) +
                            scale_x_log10(breaks = c(0, 1000, 10000, 100000), labels = comma) + 
                            coord_cartesian(xlim = c(500, 100000), ylim = c(0,1)) + # change scale for zooming in/out
                            scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)) +
                            xlab("") +#Annual expenditure per Adult Equivalent  (PPP 2011 US$)
                            ylab("") +#Share of households (%)
                            #                           ggtitle("Ownership Rates by Consumption Level (Log scale): Air Conditioning, Fan and Refrigerator.") +
                            theme_bw() +
                            theme(panel.background=element_blank(),
                                  panel.border=element_blank(),
                                  panel.grid.major=element_line(color="lightgray"),
                                  panel.grid.minor=element_blank(),
                                  plot.title = element_text(size = 20, family = "Tahoma", hjust = 0.5), 
                                  text = element_text(size = 20, family = "Tahoma",color="black"),
                                  legend.title=element_blank(),
                                  legend.position = "none")



png(paste0(line_plot_dir, 'MEX_local_AC_FAN_REF_kd.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(MEX_local_AC_FAN_REF_kd)
dev.off()


#####################################################################################################################
######################################################## END ########################################################
#####################################################################################################################



