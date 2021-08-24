
####### Replication code for Fig. 2 - Panel B and for Fig. 3 - Panel B ####### 
####### Predictive probabilities plots for the four countries #######

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

## We add to the plot the average value of CDD in each country as a grey dashed vertical line

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

dta_dir              <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_data_files/Figure_2/', sep='')
plot_dir             <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Figure_2/', sep='')
SI_plot_dir          <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Supplementary_Info/', sep='')


### Read .dta files created in the do file Figure_2_panel_B_and_Figure_S3_Bottom ###
FOUR_COUNTRIES_APPL_db         <- read.dta13(paste(dta_dir,'/APPL_CDD_db_pred_prob.dta', sep='')) ### Library readStata13
FOUR_COUNTRIES_APPL_wb         <- read.dta13(paste(dta_dir,'/APPL_CDD_wb_pred_prob.dta', sep='')) ### Library readStata13


####### Prediction Probabilities plots #######

# Prepare for graphs -> averages across all the available waves for each country
# Set means: db
FOUR_COUNTRIES_APPL_db$mean_CDD_avg_2016 <- 377.0034
FOUR_COUNTRIES_APPL_db$mean_CDD_avg_2016[FOUR_COUNTRIES_APPL_db$country == "India"] <- 1040.71
FOUR_COUNTRIES_APPL_db$mean_CDD_avg_2016[FOUR_COUNTRIES_APPL_db$country == "Brazil"] <- 480.4042

# Set means: wb
FOUR_COUNTRIES_APPL_wb$mean_CDD_avg_2016 <- 115.5225
FOUR_COUNTRIES_APPL_wb$mean_CDD_avg_2016[FOUR_COUNTRIES_APPL_wb$country == "India"] <- 298.1859
FOUR_COUNTRIES_APPL_wb$mean_CDD_avg_2016[FOUR_COUNTRIES_APPL_wb$country == "Brazil"] <- 176.7097

# Create mean columns
mean_CDD_avg_2016_db <- FOUR_COUNTRIES_APPL_db %>%
  group_by(country) %>%
  summarise(MN = mean(mean_CDD_avg_2016))

mean_CDD_avg_2016_wb <- FOUR_COUNTRIES_APPL_wb %>%
  group_by(country) %>%
  summarise(MN = mean(mean_CDD_avg_2016))


#### GRAPHS ####

## Full sample by country

FOUR_COUNTRIES_APPL_db$country_appl <- paste(FOUR_COUNTRIES_APPL_db$country, FOUR_COUNTRIES_APPL_db$appl, sep=" - ")
FOUR_COUNTRIES_APPL_wb$country_appl <- paste(FOUR_COUNTRIES_APPL_wb$country, FOUR_COUNTRIES_APPL_wb$appl, sep=" - ")


FOUR_COUNTRIES_APPL_wb$margin<-FOUR_COUNTRIES_APPL_wb$margin*100
FOUR_COUNTRIES_APPL_wb$ci_lb<-FOUR_COUNTRIES_APPL_wb$ci_lb*100
FOUR_COUNTRIES_APPL_wb$ci_ub<-FOUR_COUNTRIES_APPL_wb$ci_ub*100
FOUR_COUNTRIES_APPL_wb <- FOUR_COUNTRIES_APPL_wb %>% filter(mean_CDD < 1001)

FOUR_COUNTRIES_APPL_db$margin<-FOUR_COUNTRIES_APPL_db$margin*100
FOUR_COUNTRIES_APPL_db$ci_lb<-FOUR_COUNTRIES_APPL_db$ci_lb*100
FOUR_COUNTRIES_APPL_db$ci_ub<-FOUR_COUNTRIES_APPL_db$ci_ub*100
FOUR_COUNTRIES_APPL_db <- FOUR_COUNTRIES_APPL_db %>% filter(mean_CDD < 1001)

  
# CDD WB
PRED_PROB_APPL_wb    <- FOUR_COUNTRIES_APPL_wb %>%
                        ggplot(aes(x = mean_CDD, y = margin, 
                                   colour = as.factor(appl), fill = as.factor(appl), group = as.factor(country_appl))) +
                        geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub), linetype = 0, alpha=0.3) +
                        geom_line(size = 0.5) +
                        ylab("Adoption (%)") +
                        xlab("Mean CDD") +
                        geom_vline(data = mean_CDD_avg_2016_wb, aes(xintercept = MN), color = "grey", linetype = "dashed",
                                   show.legend=T) + 
                        coord_cartesian(ylim = c(0, 100)) +
                        scale_y_continuous(breaks = seq(0, 100, by = 20)) +
                        scale_x_continuous(breaks = seq(0, 1000, by=250)) +
                        theme_bw() +
                        scale_fill_manual(values=c("#fc8d62", "#66c2a5", "#8da0cb")) +
                        scale_colour_manual(values=c("#fc8d62", "#66c2a5", "#8da0cb")) +
                        theme(panel.background=element_blank(),
                              axis.line = element_line(color="lightgray"),
                              panel.border=element_blank(),
                              panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),
                              plot.title = element_text(size = 16, family = "Tahoma", hjust = 0.5), 
                              text = element_text(size = 16, family = "Tahoma"),
                              legend.title=element_blank(),
                              legend.position = "bottom")+
                        facet_wrap(. ~ country, ncol=4)

png(paste0(plot_dir, 'Figure_2_PanelB.png',sep=''), height=6.75, width=13, units='in', res=1200)
print(PRED_PROB_APPL_wb)
dev.off()


# CDD DB
PRED_PROB_APPL_db    <- FOUR_COUNTRIES_APPL_db %>%
                        ggplot(aes(x = mean_CDD, y = margin, 
                                   colour = as.factor(appl), fill = as.factor(appl), group = as.factor(country_appl))) +
                        geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub), linetype = 0, alpha=0.3) +
                        geom_line(size = 0.5) +
                        ylab("Adoption (%))") +
                        xlab("Mean CDD") +
                        geom_vline(data = mean_CDD_avg_2016_db, aes(xintercept = MN), color = "grey", linetype = "dashed",
                                   show.legend=T) + 
                        coord_cartesian(ylim = c(0, 100)) +
                        scale_y_continuous(breaks = seq(0, 100, by = 20)) +
                        scale_x_continuous(breaks = seq(0, 1000, by=250)) +
                        theme_bw() +
                        scale_fill_manual(values=c("#fc8d62", "#66c2a5", "#8da0cb")) +
                        scale_colour_manual(values=c("#fc8d62", "#66c2a5", "#8da0cb")) +
                        theme(panel.background=element_blank(),
                              axis.line = element_line(color="lightgray"),
                              panel.border=element_blank(),
                              panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),
                              plot.title = element_text(size = 16, family = "Tahoma", hjust = 0.5), 
                              text = element_text(size = 16, family = "Tahoma"),
                              legend.title=element_blank(),
                              legend.position = "bottom")+
                        facet_wrap(. ~ country, ncol=4)


png(paste0(SI_plot_dir, 'Figure_S3_PanelB.png',sep=''), height=6.75, width=13, units='in', res=1200)
print(PRED_PROB_APPL_db)
dev.off()


################################################################################################################
################################################### END ########################################################
################################################################################################################
