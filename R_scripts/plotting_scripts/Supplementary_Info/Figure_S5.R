
####### Replication code for Fig. S5 - Local regression plots for the four   ####### 
####### countries (MEX, IDN, IND and BRA): current (last wave) + projections ####### 

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

## Expenditure is computed in costant 2011 dollars ($) using current PPP for each survey and US consumer price index (CPI)
## We have to use two different aes -> note inherit.aes is fundamental to plot two different kind of plots (line and density)
## We use na.omit to avoid possible NAs. NB. Stata usually does it by itself, R not!


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

dta_dir              <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_data_files/projections', sep='')
SI_plot_dir          <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Supplementary_Info/', sep='')


### Read .dta files ###
FOUR_COUNTRIES                <- read.dta13(paste(dta_dir,'/local_reg_proj.dta', sep='')) ### Library readStata13


### AC local regs in last waves and projections + kernel density in last wave (by country)

# BRA
BRA <- FOUR_COUNTRIES %>% filter(country == "Brazil")
BRA_local_proj  <- ggplot(data = na.omit(subset(BRA, 
                                                select = c(exp_cap_usd_2011, country, local_ac))),
                                 aes(x = exp_cap_usd_2011, colour = "CURRENT")) + 
                          geom_line(aes(y = local_ac), size = 2) +
                          geom_line(data = na.omit(subset(BRA, 
                                                          select = c(exp_cap_usd_2011_SSP1, country, local_ac_rcp85_SSP1))), 
                                    aes(x = exp_cap_usd_2011_SSP1, y = local_ac_rcp85_SSP1, 
                                        colour = "RCP8.5-SSP1"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(BRA, 
                                                          select = c(exp_cap_usd_2011_SSP2, country, local_ac_rcp85_SSP2))), 
                                    aes(x = exp_cap_usd_2011_SSP2, y = local_ac_rcp85_SSP2, 
                                        colour = "RCP8.5-SSP2"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(BRA, 
                                                          select = c(exp_cap_usd_2011_SSP3, country, local_ac_rcp85_SSP3))), 
                                    aes(x = exp_cap_usd_2011_SSP3, y = local_ac_rcp85_SSP3, 
                                        colour = "RCP8.5-SSP3"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(BRA, 
                                                          select = c(exp_cap_usd_2011_SSP4, country, local_ac_rcp85_SSP4))), 
                                    aes(x = exp_cap_usd_2011_SSP4, y = local_ac_rcp85_SSP4, 
                                        colour = "RCP8.5-SSP4"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(BRA, 
                                                          select = c(exp_cap_usd_2011_SSP5, country, local_ac_rcp85_SSP2))), 
                                    aes(x = exp_cap_usd_2011_SSP5, y = local_ac_rcp85_SSP2, 
                                        colour = "RCP8.5-SSP5"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(BRA, 
                                                          select = c(kernel_exp, density, country))), 
                                    aes(x = kernel_exp, y = density, 
                                        colour = "HH Density"), inherit.aes = FALSE, linetype = "dashed", size = 1) +
                          scale_color_manual(values = c('CURRENT' = '#fb9a99', 'RCP8.5-SSP1' = '#dadaeb', 'RCP8.5-SSP2' = '#bcbddc', 
                                                        'RCP8.5-SSP3' = '#9e9ac8', 'RCP8.5-SSP4' = '#756bb1',
                                                        'RCP8.5-SSP5' = '#54278f', 'HH Density' = "black")) +
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
                                plot.title = element_text(size = 18, family = "Tahoma", hjust = 0.5), 
                                text = element_text(size = 18, family = "Tahoma"),
                                legend.title=element_blank(),
                                legend.position = "none")

png(paste0(SI_plot_dir, 'BRA_local_proj.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(BRA_local_proj)
dev.off()

# IND
IND <- FOUR_COUNTRIES %>% filter(country == "India")
IND_local_proj  <- ggplot(data = na.omit(subset(IND, 
                                                select = c(exp_cap_usd_2011, country, local_ac))),
                          aes(x = exp_cap_usd_2011, colour = "CURRENT")) + 
                          geom_line(aes(y = local_ac), size = 2) +
                          geom_line(data = na.omit(subset(IND, 
                                                          select = c(exp_cap_usd_2011_SSP1, country, local_ac_rcp85_SSP1))), 
                                    aes(x = exp_cap_usd_2011_SSP1, y = local_ac_rcp85_SSP1, 
                                        colour = "RCP8.5-SSP1"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(IND, 
                                                          select = c(exp_cap_usd_2011_SSP2, country, local_ac_rcp85_SSP2))), 
                                    aes(x = exp_cap_usd_2011_SSP2, y = local_ac_rcp85_SSP2, 
                                        colour = "RCP8.5-SSP2"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(IND, 
                                                          select = c(exp_cap_usd_2011_SSP3, country, local_ac_rcp85_SSP3))), 
                                    aes(x = exp_cap_usd_2011_SSP3, y = local_ac_rcp85_SSP3, 
                                        colour = "RCP8.5-SSP3"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(IND, 
                                                          select = c(exp_cap_usd_2011_SSP4, country, local_ac_rcp85_SSP4))), 
                                    aes(x = exp_cap_usd_2011_SSP4, y = local_ac_rcp85_SSP4, 
                                        colour = "RCP8.5-SSP4"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(IND, 
                                                          select = c(exp_cap_usd_2011_SSP5, country, local_ac_rcp85_SSP2))), 
                                    aes(x = exp_cap_usd_2011_SSP5, y = local_ac_rcp85_SSP2, 
                                        colour = "RCP8.5-SSP5"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(IND, 
                                                          select = c(kernel_exp, density, country))), 
                                    aes(x = kernel_exp, y = density, 
                                        colour = "HH Density"), inherit.aes = FALSE, linetype = "dashed", size = 1) +
                          scale_color_manual(values = c('CURRENT' = '#fb9a99', 'RCP8.5-SSP1' = '#dadaeb', 'RCP8.5-SSP2' = '#bcbddc', 
                                                        'RCP8.5-SSP3' = '#9e9ac8', 'RCP8.5-SSP4' = '#756bb1',
                                                        'RCP8.5-SSP5' = '#54278f', 'HH Density' = "black")) +
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
                                plot.title = element_text(size = 18, family = "Tahoma", hjust = 0.5), 
                                text = element_text(size = 18, family = "Tahoma"),
                                legend.title=element_blank(),
                                legend.position = "none")

png(paste0(SI_plot_dir, 'IND_local_proj.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(IND_local_proj)
dev.off()

# MEX
MEX <- FOUR_COUNTRIES %>% filter(country == "Mexico")
MEX_local_proj  <- ggplot(data = na.omit(subset(MEX, 
                                                select = c(exp_cap_usd_2011, country, local_ac))),
                          aes(x = exp_cap_usd_2011, colour = "CURRENT")) + 
                          geom_line(aes(y = local_ac), size = 2) +
                          geom_line(data = na.omit(subset(MEX, 
                                                          select = c(exp_cap_usd_2011_SSP1, country, local_ac_rcp85_SSP1))), 
                                    aes(x = exp_cap_usd_2011_SSP1, y = local_ac_rcp85_SSP1, 
                                        colour = "RCP8.5-SSP1"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(MEX, 
                                                          select = c(exp_cap_usd_2011_SSP2, country, local_ac_rcp85_SSP2))), 
                                    aes(x = exp_cap_usd_2011_SSP2, y = local_ac_rcp85_SSP2, 
                                        colour = "RCP8.5-SSP2"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(MEX, 
                                                          select = c(exp_cap_usd_2011_SSP3, country, local_ac_rcp85_SSP3))), 
                                    aes(x = exp_cap_usd_2011_SSP3, y = local_ac_rcp85_SSP3, 
                                        colour = "RCP8.5-SSP3"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(MEX, 
                                                          select = c(exp_cap_usd_2011_SSP4, country, local_ac_rcp85_SSP4))), 
                                    aes(x = exp_cap_usd_2011_SSP4, y = local_ac_rcp85_SSP4, 
                                        colour = "RCP8.5-SSP4"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(MEX, 
                                                          select = c(exp_cap_usd_2011_SSP5, country, local_ac_rcp85_SSP2))), 
                                    aes(x = exp_cap_usd_2011_SSP5, y = local_ac_rcp85_SSP2, 
                                        colour = "RCP8.5-SSP5"), inherit.aes = FALSE, size = 2) +
                          geom_line(data = na.omit(subset(MEX, 
                                                          select = c(kernel_exp, density, country))), 
                                    aes(x = kernel_exp, y = density, 
                                        colour = "HH Density"), inherit.aes = FALSE, linetype = "dashed", size = 1) +
                          scale_color_manual(values = c('CURRENT' = '#fb9a99', 'RCP8.5-SSP1' = '#dadaeb', 'RCP8.5-SSP2' = '#bcbddc', 
                                                        'RCP8.5-SSP3' = '#9e9ac8', 'RCP8.5-SSP4' = '#756bb1',
                                                        'RCP8.5-SSP5' = '#54278f', 'HH Density' = "black")) +
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
                                plot.title = element_text(size = 18, family = "Tahoma", hjust = 0.5), 
                                text = element_text(size = 18, family = "Tahoma"),
                                legend.title=element_blank(),
                                legend.position = "none")



png(paste0(SI_plot_dir, 'MEX_local_proj.png',sep=''), height=7.75, width=8.5, units='in', res=1200)
print(MEX_local_proj)
dev.off()

