
####### Replication code for Fig. 2 - Panel A Marginal effect plots of CDD by log total exp ####### 
####### for the four countries (MEX, IDN, IND and BRA) using survey data                    ####### 

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

## We use the same different colours for the three appliances AC, FAN and REF
## As we have to identify the three vars in the same plot (look at the below one), 
## we use #FF1E42 (AC), #003943 (FAN) and #00AC8B (REF)
## http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=6

## Install required R packages if required before loading

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc()                  # frees up memory resources

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

dta_dir              <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_data_files/Figure_2/', sep='')
plot_dir             <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Figure_2/', sep='')
SI_plot_dir          <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Supplementary_Info/', sep='')

### Read .dta files created in the do file Figure_2_panel_A_and_Figure_S3_Top ###

FOUR_COUNTRIES_dydx_wb         <- read.dta13(paste(dta_dir,'/CDD_wb_exp.dta', sep='')) ### Library readStata13
FOUR_COUNTRIES_dydx_db         <- read.dta13(paste(dta_dir,'/CDD_db_exp.dta', sep='')) ### Library readStata13


# Convert log total exp in levels -> in the graph we use log scale but x-axis express in levels

FOUR_COUNTRIES_dydx_wb <- FOUR_COUNTRIES_dydx_wb %>% mutate(total_exp = exp(ln_total_exp))
FOUR_COUNTRIES_dydx_db <- FOUR_COUNTRIES_dydx_db %>% mutate(total_exp = exp(ln_total_exp))

FOUR_COUNTRIES_dydx_wb$country_appl <- paste(FOUR_COUNTRIES_dydx_wb$country, FOUR_COUNTRIES_dydx_wb$appl, sep=" - ")
FOUR_COUNTRIES_dydx_db$country_appl <- paste(FOUR_COUNTRIES_dydx_db$country, FOUR_COUNTRIES_dydx_db$appl, sep=" - ")

## Create plots 

## Wet-bulb
dydx_wb    <- FOUR_COUNTRIES_dydx_wb %>%
              ggplot(aes(x = total_exp, y = margin, 
                         colour = as.factor(appl), fill = as.factor(appl), group = as.factor(country_appl), show.legend=F)) +
              geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub), linetype = 0, alpha=0.3) +
              geom_line(size = 0.5) +
              ylab("CDDs' marginal effect on adoption") +
              xlab("Total Expenditure 2011 USD (Log scale)") +
              scale_x_log10(breaks = c(0, 1000, 3000, 10000, 50000), labels = comma) +
              scale_y_continuous(labels = comma) +
              coord_cartesian(xlim = c(500, 50000)) + # change scale for zooming in/out
              theme_bw() +
              scale_fill_manual(values=c("#fc8d62", "#66c2a5", "#8da0cb")) +
              scale_colour_manual(values=c("#fc8d62", "#66c2a5", "#8da0cb")) +
  theme(panel.background=element_blank(),
                    axis.line = element_line(color="lightgray"),
                    panel.border=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
                    text = element_text(size = 14, family = "Tahoma"),
                    legend.title=element_blank(),
                    legend.position = "none") +
              facet_wrap(. ~ country, ncol=4)
dydx_wb

png(paste0(plot_dir, 'Figure_2_panelA_aug.png',sep=''), height=6.75, width=13, units='in', res=1200)
print(dydx_wb)
dev.off()


## Dry-bulb

dydx_db    <- FOUR_COUNTRIES_dydx_db %>%
              ggplot(aes(x = total_exp, y = margin, 
                         colour = as.factor(appl), fill = as.factor(appl), group = as.factor(country_appl), show.legend=F)) +
              geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub), linetype = 0, alpha=0.3) +
              geom_line(size = 0.5) +
              ylab("CDDs marginal effect on AC adoption") +
              xlab("Total Expenditure 2011 USD (Log scale)") +
              scale_x_log10(breaks = c(0, 1000, 3000, 10000, 50000), labels = comma) +
              scale_y_continuous(labels = comma) +
              coord_cartesian(xlim = c(500, 50000)) + # change scale for zooming in/out
              theme_bw() +
              scale_fill_manual(values=c("#fc8d62", "#66c2a5", "#8da0cb")) +
              scale_colour_manual(values=c("#fc8d62", "#66c2a5", "#8da0cb")) +
              theme(panel.background=element_blank(),
                                axis.line = element_line(color="lightgray"),
                                panel.border=element_blank(),
                                panel.grid.major=element_blank(),
                                panel.grid.minor=element_blank(),
                                plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
                                text = element_text(size = 14, family = "Tahoma"),
                                legend.title=element_blank(),
                                legend.position = "none") +
                          facet_wrap(. ~ country, ncol=4)
dydx_db

png(paste0(SI_plot_dir, 'Figure_S3_panelA.aug.png',sep=''), height=6.75, width=13, units='in', res=1200)
print(dydx_db)
dev.off()
