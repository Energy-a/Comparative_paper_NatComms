
####### Replication code for  Fig. 4 BUBBLE PLOT (MEX, IDN, IND and BRA) WITH THREE DIMENSIONS ######

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

# https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html

## x-axis: CDD exposure_ratio
## y-axis: ac_ratio computed as ac_state/ac_country(median across ac state)
## bubble: hh_state(for bubble size)

## Plots (bubbles) are for historical and future (RCP 8.5_SSP5). Plots are created 
## separately for the two periods, as well as (hist & fut) combined on a single plot. 


## The required R packages will be installed (if not already installed) before loading ##

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc()                  # frees up memory resources

if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(readstata13)){install.packages("readstata13")}
if(!require(grid)){install.packages("grid")}
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(ggrepel)){install.packages("ggrepel")}

## Set path to main directory, input netcdf and output plot directories ##

user <- 'mnm_cmcc'

if (user=='mnm_cmcc') {
stub        <- 'G:/My Drive/'
}

dta_dir                       <- paste(stub,'5-CountryExpertsExchange/Comparative_paper/modified_data', sep='')
plot_dir                      <- paste(stub,'5-CountryExpertsExchange/Comparative_paper/R_plots/Bubble_plots/', sep='')

### Read .dta file ###

FOUR_COUNTRIES                <- read.dta13(paste(dta_dir,'/for_descriptives/cdd_pop_exposure_4countries.dta', sep='')) ### Library readStata13

names(FOUR_COUNTRIES)[2]      <- "Country"
names(FOUR_COUNTRIES)[3]      <- "State"

### Retain only the required columns ###

FOUR_COUNTRIES        <- FOUR_COUNTRIES[,c("Country", "State", "exposure_ratio", "exposure_ratio_rcp85_SSP5",
                                           "ac_ratio",  "ac_ratio_rcp85_SSP5", "hh_state", "hh_state_fut_SSP5")]

### Normalize the hh_state and hh_state_fut_SSP5

FOUR_COUNTRIES$hh_state_rescaled           <- ceiling((FOUR_COUNTRIES$hh_state-mean(FOUR_COUNTRIES$hh_state, na.rm = T))/
                                                        sd(FOUR_COUNTRIES$hh_state, na.rm = T) *100000000)
FOUR_COUNTRIES$hh_state_fut_SSP5_rescaled  <- ceiling((FOUR_COUNTRIES$hh_state_fut_SSP5-mean(FOUR_COUNTRIES$hh_state_fut_SSP5, na.rm=T))/
                                                        sd(FOUR_COUNTRIES$hh_state_fut_SSP5, na.rm = T) *100000000)


### Split DF by individual country for plotting (Colors shown in hex) ##

# http://applied-r.com/wp-content/uploads/2019/01/rcolors_byhex.png

Brazil      <- FOUR_COUNTRIES %>% filter(Country=="Brazil")      #0000FF
India       <- FOUR_COUNTRIES %>% filter(Country=="India")       #gold
Indonesia   <- FOUR_COUNTRIES %>% filter(Country=="Indonesia")   #FF6347
Mexico      <- FOUR_COUNTRIES %>% filter(Country=="Mexico")      #FF00FF

### For the sake of plotting, we show names of states only in the 4th quadrant (bottom right)
### To do this, the names of states with ac_ratio < 1 and exposure_ratio < 1 needs to be NULL

Brazil_hist      <- transform(Brazil, State = ifelse(exposure_ratio > 1 & ac_ratio < 1,  State, ""))
Brazil_fut       <- transform(Brazil, State = ifelse(exposure_ratio_rcp85_SSP5 > 1 & ac_ratio_rcp85_SSP5 < 1,  State, ""))

### First Plot individual periods ###

### Note: May give warnings related to font database, if the required font is not found
### Change the font in "axis.title.x" and other locations within the 'ggplot' block of code


Brazil_bubble_plot_hist <- ggplot(data=Brazil_hist) +

  geom_point(aes(x=exposure_ratio, y=ac_ratio, size = hh_state), 
             shape = 21, color = ifelse(Brazil_hist$State == "", "grey80", "#0000FF"), 
             fill = ifelse(Brazil_hist$State == "", "grey80", "#0000FF"), alpha = 0.5)+
  
  scale_size_area()+
  
  xlim(0,12)+ ylim(0, 1.7)+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +

geom_text_repel(data = Brazil_hist, mapping = aes(x=exposure_ratio, y=ac_ratio, label = State), 
                nudge_y = 0.3, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                segment.alpha	= 0.7, segment.color =	"#0000FF") +
  
  ggtitle("Historical period")+
  
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_text(size = 14, family = "Tahoma"),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        legend.position = "none") 

# Brazil_bubble_plot_hist

Brazil_bubble_plot_fut <- ggplot(data=Brazil_fut) +
  
  geom_point(aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, size = hh_state_fut_SSP5), 
             shape = 21, color = ifelse(Brazil_fut$State == "", "grey80", "#0000FF"), 
             fill = ifelse(Brazil_fut$State == "", "grey80", "#0000FF"), alpha = 0.5)+
  
  scale_size_area()+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +

  geom_text_repel(data = Brazil_fut, mapping = aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, label = State), 
                  nudge_y = 0.1, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                  segment.alpha	= 0.7, segment.color =	"#0000FF") +
  
  ggtitle("RCP 8.5 - SSP5")+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_text(size = 14, family = "Tahoma"),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        legend.position = "none") 

# Brazil_bubble_plot_fut

## Now future on common (historical) y-axis

Brazil_bubble_plot_fut_hist_y_axis <- ggplot(data=Brazil_fut) +

  
  geom_point(aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, size = hh_state_fut_SSP5), 
             shape = 21, color = ifelse(Brazil_fut$State == "", "grey80", "#0000FF"), 
             fill = ifelse(Brazil_fut$State == "", "grey80", "#0000FF"), alpha = 0.5)+
  
  scale_size_area()+
  
  xlim(0,12)+ ylim(0, 1.7)+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +

  geom_text_repel(data = Brazil_fut, mapping = aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, label = State), 
                  nudge_y = 0.2, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                  segment.alpha	= 0.7, segment.color =	"#0000FF") +
  
  ggtitle("RCP 8.5 - SSP5")+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "none") 

# Brazil_bubble_plot_fut_hist_y_axis

## Combine historical and future (on the same y-axis) side by side (Commented as not used in paper)
## The plots in Figure 4 (of paper) are actually combined using Adobe Illustrator

# Brazil_bubble_plot_hist_fut_on_same_y_axis <- grid.arrange(Brazil_bubble_plot_hist, 
#                                                            Brazil_bubble_plot_fut_hist_y_axis, ncol=2)

### Now historical and RCP8.5_SSP5 as before, but this time show the names of only those states which 
### are in the fourth quadrant in the future period (and show their names in the historical period to get
### and idea where they were in the past) ##

Brazil_hist_rev      <- transform(Brazil_hist, State = ifelse(exposure_ratio_rcp85_SSP5 > 1 & ac_ratio_rcp85_SSP5 < 1,  Brazil_fut$State, Brazil_hist$State))

Brazil_bubble_plot_hist_rev <- ggplot(data=Brazil_hist_rev) +
  
  geom_point(aes(x=exposure_ratio, y=ac_ratio, size = hh_state), 
             shape = 21, color = ifelse(Brazil_hist_rev$State == "", "grey80", "#0000FF"), 
             fill = ifelse(Brazil_hist$State == "", "grey80", "#0000FF"), alpha = 0.5)+
  
  scale_size_area()+
  
  xlim(0,12)+ ylim(0, 1.7)+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +

geom_text_repel(data = Brazil_hist_rev, mapping = aes(x=exposure_ratio, y=ac_ratio, label = State), 
                nudge_y = 0.3, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                segment.alpha	= 0.7, 
                segment.color =	ifelse(Brazil_hist_rev$ac_ratio > 1, "grey80", "#0000FF")) + ## not working
  
  ggtitle("Historical period")+
  
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_text(size = 14, family = "Tahoma"),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        legend.position = "none") 

# Brazil_bubble_plot_hist_rev

## Combine historical and future (on the same y-axis) side by side

Brazil_bubble_plot_hist_fut_on_same_y_axis_rev <- grid.arrange(Brazil_bubble_plot_hist_rev, 
                                                               Brazil_bubble_plot_fut_hist_y_axis, ncol=2)

### India ####

India_hist      <- transform(India, State = ifelse(exposure_ratio > 1 & ac_ratio < 1,  State, ""))
India_fut       <- transform(India, State = ifelse(exposure_ratio_rcp85_SSP5 > 1 & ac_ratio_rcp85_SSP5 < 1,  State, ""))

# ### If required, first plot individual periods ###
# 
# India_bubble_plot_hist <- ggplot(data=India_hist) +
#   
#   geom_point(aes(x=exposure_ratio, y=ac_ratio, size = hh_state), 
#              shape = 21, color = ifelse(India_hist$State == "", "grey80", "#40b8d0"), 
#              fill = ifelse(India_hist$State == "", "grey80", "#40b8d0"), alpha = 0.5)+
#   
#   scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
#   
#   xlim(0,12)+ ylim(0, 1.7)+
#   
#   xlab("CDD exposure relative to country average")+
#   ylab("AC adoption relative to country average")+
#   labs(color = "Country", size = "HH State" ) +
#   
#    geom_text_repel(data = India_hist, mapping = aes(x=exposure_ratio, y=ac_ratio, label = State), 
#                 nudge_y = 0.3, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
#                 segment.alpha	= 0.7, segment.color =	"#40b8d0") +
#   
#   ggtitle("Historical period")+
#   
#   theme(panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.ticks=element_blank(),
#         plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
#         axis.title.x = element_text(size = 12, family = "Tahoma"),
#         axis.title.y = element_text(size = 14, family = "Tahoma"),
#         axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         legend.position = "none") 
# 
# India_bubble_plot_hist
# 
# India_bubble_plot_fut <- ggplot(data=India_fut) +
#   
#   geom_point(aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, size = hh_state_fut_SSP5), 
#              shape = 21, color = ifelse(India_fut$State == "", "grey80", "#E7B800"), 
#              fill = ifelse(India_fut$State == "", "grey80", "#E7B800"), alpha = 0.5)+
#   
#   
#   scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
#   
#   xlab("CDD exposure relative to country average")+
#   ylab("AC adoption relative to country average")+
#   labs(color = "Country", size = "HH State" ) +
#   #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   
#   geom_text_repel(data = India_fut, mapping = aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, label = State), 
#                   nudge_y = 0.1, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
#                   segment.alpha	= 0.7, segment.color =	"#E7B800") +
#   
#   ggtitle("RCP 8.5 - SSP5")+
#   theme(panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.ticks=element_blank(),
#         plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
#         # text = element_text(size = 16, family = "Tahoma", face = "bold"),
#         axis.title.x = element_text(size = 12, family = "Tahoma"),
#         axis.title.y = element_text(size = 14, family = "Tahoma"),
#         axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         legend.position = "none") 
# 
# India_bubble_plot_fut

## Now future on common (historical) y-axis

India_bubble_plot_fut_hist_y_axis <- ggplot(data=India_fut) +
  
  geom_point(aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, size = hh_state_fut_SSP5),
             shape = 21, color = ifelse(India_fut$State == "", "grey80", "gold"),
             fill = ifelse(India_fut$State == "", "grey80", "gold"), alpha = 0.5)+
  
  # scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
  scale_size_area()+
  
  xlim(0,12)+ ylim(0, 1.7)+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +
  #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  
  geom_text_repel(data = India_fut, mapping = aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, label = State),
                  nudge_y = 0.2, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                  segment.alpha	= 0.7, segment.color =	"gold") +
  
  ggtitle("RCP 8.5 - SSP5")+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        # text = element_text(size = 16, family = "Tahoma", face = "bold"),
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "none")

# India_bubble_plot_fut_hist_y_axis

# ## Combine historical and future (on the same y-axis) side by side
# 
# India_bubble_plot_hist_fut_on_same_y_axis <- grid.arrange(India_bubble_plot_hist, 
#                                                            India_bubble_plot_fut_hist_y_axis, ncol=2)
# 
# plot(India_bubble_plot_hist_fut_on_same_y_axis)

### Now historical and RCP8.5_SSP5 as before, but this time show the names of only those states which 
### are in the fourth quadrant in the future period (and show their names in the historical period to get
### and idea where they were in the past) ##

India_hist_rev      <- transform(India_hist, State = ifelse(exposure_ratio_rcp85_SSP5 > 1 & ac_ratio_rcp85_SSP5 < 1,  India_fut$State, India_hist$State))

India_bubble_plot_hist_rev <- ggplot(data=India_hist_rev) +
  
  geom_point(aes(x=exposure_ratio, y=ac_ratio, size = hh_state), 
             shape = 21, color = ifelse(India_hist_rev$State == "", "grey80", "gold"), 
             fill = ifelse(India_hist$State == "", "grey80", "gold"), alpha = 0.5)+
  
  # scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
  scale_size_area()+
  
  xlim(0,12)+ ylim(0, 1.7)+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +
  #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  
  geom_text_repel(data = India_hist_rev, mapping = aes(x=exposure_ratio, y=ac_ratio, label = State), 
                  nudge_y = 0.3, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                  segment.alpha	= 0.7, 
                  segment.color =	ifelse(India_hist_rev$ac_ratio > 1, "grey80", "gold")) + ## not working
  
  ggtitle("Historical period")+
  
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
        # text = element_text(size = 16, family = "Tahoma", face = "bold"),
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_text(size = 14, family = "Tahoma"),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        # scale_fill_brewer(palette = "Accent") +
        # scale_fill_continuous(low = "plum1", high = "purple4") +
        legend.position = "none") 

# India_bubble_plot_hist_rev

## Combine historical and future (on the same y-axis) side by side

India_bubble_plot_hist_fut_on_same_y_axis_rev <- grid.arrange(India_bubble_plot_hist_rev, 
                                                              India_bubble_plot_fut_hist_y_axis, ncol=2)

### Indonesia ####

Indonesia_hist      <- transform(Indonesia, State = ifelse(exposure_ratio > 1 & ac_ratio < 1,  State, ""))
Indonesia_fut       <- transform(Indonesia, State = ifelse(exposure_ratio_rcp85_SSP5 > 1 & ac_ratio_rcp85_SSP5 < 1,  State, ""))

# ### First individual periods ###
# 
# Indonesia_bubble_plot_hist <- ggplot(data=Indonesia_hist) +
#   
#   geom_point(aes(x=exposure_ratio, y=ac_ratio, size = hh_state), 
#              shape = 21, color = ifelse(Indonesia_hist$State == "", "grey80", "#40b8d0"), 
#              fill = ifelse(Indonesia_hist$State == "", "grey80", "#40b8d0"), alpha = 0.5)+
#   
#   scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
#   
#   xlim(0,12)+ ylim(0, 1.7)+
#   
#   xlab("CDD exposure relative to country average")+
#   ylab("AC adoption relative to country average")+
#   labs(color = "Country", size = "HH State" ) +
#   #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   
#    geom_text_repel(data = Indonesia_hist, mapping = aes(x=exposure_ratio, y=ac_ratio, label = State), 
#                 nudge_y = 0.3, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
#                 segment.alpha	= 0.7, segment.color =	"#40b8d0") +
#   
#   ggtitle("Historical period")+
#   
#   theme(panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.ticks=element_blank(),
#         plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
#         # text = element_text(size = 16, family = "Tahoma", face = "bold"),
#         axis.title.x = element_text(size = 12, family = "Tahoma"),
#         axis.title.y = element_text(size = 14, family = "Tahoma"),
#         axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         # scale_fill_brewer(palette = "Accent") +
#         # scale_fill_continuous(low = "plum1", high = "purple4") +
#         legend.position = "none") 
# 
# Indonesia_bubble_plot_hist
# 
# Indonesia_bubble_plot_fut <- ggplot(data=Indonesia_fut) +
#   
#   geom_point(aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, size = hh_state_fut_SSP5), 
#              shape = 21, color = ifelse(Indonesia_fut$State == "", "grey80", "#E7B800"), 
#              fill = ifelse(Indonesia_fut$State == "", "grey80", "#E7B800"), alpha = 0.5)+
#   
#   
#   scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
#   
#   xlab("CDD exposure relative to country average")+
#   ylab("AC adoption relative to country average")+
#   labs(color = "Country", size = "HH State" ) +
#   #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   
#   geom_text_repel(data = Indonesia_fut, mapping = aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, label = State), 
#                   nudge_y = 0.1, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
#                   segment.alpha	= 0.7, segment.color =	"#E7B800") +
#   
#   ggtitle("RCP 8.5 - SSP5")+
#   theme(panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.ticks=element_blank(),
#         plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
#         # text = element_text(size = 16, family = "Tahoma", face = "bold"),
#         axis.title.x = element_text(size = 12, family = "Tahoma"),
#         axis.title.y = element_text(size = 14, family = "Tahoma"),
#         axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         legend.position = "none") 
# 
# Indonesia_bubble_plot_fut

## Now future on common (historical) y-axis

Indonesia_bubble_plot_fut_hist_y_axis <- ggplot(data=Indonesia_fut) +
  
  geom_point(aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, size = hh_state_fut_SSP5),
             shape = 21, color = ifelse(Indonesia_fut$State == "", "grey80", "#FF6347"),
             fill = ifelse(Indonesia_fut$State == "", "grey80", "#FF6347"), alpha = 0.5)+
  
  # scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
  scale_size_area()+
  
  xlim(0,12)+ ylim(0, 1.7)+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +
  #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  
  geom_text_repel(data = Indonesia_fut, mapping = aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, label = State),
                  nudge_y = 0.2, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                  segment.alpha	= 0.7, segment.color =	"#FF6347") +
  
  ggtitle("RCP 8.5 - SSP5")+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        # text = element_text(size = 16, family = "Tahoma", face = "bold"),
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "none")

# Indonesia_bubble_plot_fut_hist_y_axis

# ## Combine historical and future (on the same y-axis) side by side
# 
# Indonesia_bubble_plot_hist_fut_on_same_y_axis <- grid.arrange(Indonesia_bubble_plot_hist, 
#                                                            Indonesia_bubble_plot_fut_hist_y_axis, ncol=2)
# 
# plot(Indonesia_bubble_plot_hist_fut_on_same_y_axis)

### Now historical and RCP8.5_SSP5 as before, but this time show the names of only those states which 
### are in the fourth quadrant in the future period (and show their names in the historical period to get
### and idea where they were in the past) ##

Indonesia_hist_rev      <- transform(Indonesia_hist, State = ifelse(exposure_ratio_rcp85_SSP5 > 1 & ac_ratio_rcp85_SSP5 < 1,  Indonesia_fut$State, Indonesia_hist$State))

Indonesia_bubble_plot_hist_rev <- ggplot(data=Indonesia_hist_rev) +
  
  geom_point(aes(x=exposure_ratio, y=ac_ratio, size = hh_state), 
             shape = 21, color = ifelse(Indonesia_hist_rev$State == "", "grey80", "#FF6347"), 
             fill = ifelse(Indonesia_hist$State == "", "grey80", "#FF6347"), alpha = 0.5)+
  
  # scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
  scale_size_area()+
  
  xlim(0,12)+ ylim(0, 1.7)+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +
  #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  
  geom_text_repel(data = Indonesia_hist_rev, mapping = aes(x=exposure_ratio, y=ac_ratio, label = State), 
                  nudge_y = 0.3, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                  segment.alpha	= 0.7, 
                  segment.color =	ifelse(Indonesia_hist_rev$ac_ratio > 1, "grey80", "#FF6347")) + ## not working
  
  ggtitle("Historical period")+
  
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
        # text = element_text(size = 16, family = "Tahoma", face = "bold"),
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_text(size = 14, family = "Tahoma"),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        # scale_fill_brewer(palette = "Accent") +
        # scale_fill_continuous(low = "plum1", high = "purple4") +
        legend.position = "none") 

# Indonesia_bubble_plot_hist_rev

## Combine historical and future (on the same y-axis) side by side

Indonesia_bubble_plot_hist_fut_on_same_y_axis_rev <- grid.arrange(Indonesia_bubble_plot_hist_rev, 
                                                                  Indonesia_bubble_plot_fut_hist_y_axis, ncol=2)




### Mexico ####

Mexico_hist      <- transform(Mexico, State = ifelse(exposure_ratio > 1 & ac_ratio < 1,  State, ""))
Mexico_fut       <- transform(Mexico, State = ifelse(exposure_ratio_rcp85_SSP5 > 1 & ac_ratio_rcp85_SSP5 < 1,  State, ""))

# ### First individual periods ###
# 
# Mexico_bubble_plot_hist <- ggplot(data=Mexico_hist) +
#   
#   geom_point(aes(x=exposure_ratio, y=ac_ratio, size = hh_state), 
#              shape = 21, color = ifelse(Mexico_hist$State == "", "grey80", "#40b8d0"), 
#              fill = ifelse(Mexico_hist$State == "", "grey80", "#40b8d0"), alpha = 0.5)+
#   
#   scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
#   
#   xlim(0,12)+ ylim(0, 1.7)+
#   
#   xlab("CDD exposure relative to country average")+
#   ylab("AC adoption relative to country average")+
#   labs(color = "Country", size = "HH State" ) +
#   #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   
#    geom_text_repel(data = Mexico_hist, mapping = aes(x=exposure_ratio, y=ac_ratio, label = State), 
#                 nudge_y = 0.3, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
#                 segment.alpha	= 0.7, segment.color =	"#40b8d0") +
#   
#   ggtitle("Historical period")+
#   
#   theme(panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.ticks=element_blank(),
#         plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
#         # text = element_text(size = 16, family = "Tahoma", face = "bold"),
#         axis.title.x = element_text(size = 12, family = "Tahoma"),
#         axis.title.y = element_text(size = 14, family = "Tahoma"),
#         axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         # scale_fill_brewer(palette = "Accent") +
#         # scale_fill_continuous(low = "plum1", high = "purple4") +
#         legend.position = "none") 
# 
# Mexico_bubble_plot_hist
# 
# Mexico_bubble_plot_fut <- ggplot(data=Mexico_fut) +
#   
#   geom_point(aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, size = hh_state_fut_SSP5), 
#              shape = 21, color = ifelse(Mexico_fut$State == "", "grey80", "#E7B800"), 
#              fill = ifelse(Mexico_fut$State == "", "grey80", "#E7B800"), alpha = 0.5)+
#   
#   
#   scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
#   
#   xlab("CDD exposure relative to country average")+
#   ylab("AC adoption relative to country average")+
#   labs(color = "Country", size = "HH State" ) +
#   #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
#   
#   geom_text_repel(data = Mexico_fut, mapping = aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, label = State), 
#                   nudge_y = 0.1, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
#                   segment.alpha	= 0.7, segment.color =	"#E7B800") +
#   
#   ggtitle("RCP 8.5 - SSP5")+
#   theme(panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.ticks=element_blank(),
#         plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
#         # text = element_text(size = 16, family = "Tahoma", face = "bold"),
#         axis.title.x = element_text(size = 12, family = "Tahoma"),
#         axis.title.y = element_text(size = 14, family = "Tahoma"),
#         axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
#         legend.position = "none") 
# 
# Mexico_bubble_plot_fut

## Now future on common (historical) y-axis

Mexico_bubble_plot_fut_hist_y_axis <- ggplot(data=Mexico_fut) +
  
  geom_point(aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, size = hh_state_fut_SSP5),
             shape = 21, color = ifelse(Mexico_fut$State == "", "grey80", "#FF00FF"),
             fill = ifelse(Mexico_fut$State == "", "grey80", "#FF00FF"), alpha = 0.5)+
  
  # scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
  scale_size_area()+
  
  xlim(0,12)+ ylim(0, 1.7)+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +
  #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  
  geom_text_repel(data = Mexico_fut, mapping = aes(x=exposure_ratio_rcp85_SSP5, y=ac_ratio_rcp85_SSP5, label = State),
                  nudge_y = 0.2, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                  segment.alpha	= 0.7, segment.color =	"#FF00FF") +
  
  ggtitle("RCP 8.5 - SSP5")+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        # text = element_text(size = 16, family = "Tahoma", face = "bold"),
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "none")

# Mexico_bubble_plot_fut_hist_y_axis

# ## Combine historical and future (on the same y-axis) side by side
# 
# Mexico_bubble_plot_hist_fut_on_same_y_axis <- grid.arrange(Mexico_bubble_plot_hist, 
#                                                            Mexico_bubble_plot_fut_hist_y_axis, ncol=2)
# 
# plot(Mexico_bubble_plot_hist_fut_on_same_y_axis)

### Now historical and RCP8.5_SSP5 as before, but this time show the names of only those states which 
### are in the fourth quadrant in the future period (and show their names in the historical period to get
### and idea where they were in the past) ##

Mexico_hist_rev      <- transform(Mexico_hist, State = ifelse(exposure_ratio_rcp85_SSP5 > 1 & ac_ratio_rcp85_SSP5 < 1,  Mexico_fut$State, Mexico_hist$State))

Mexico_bubble_plot_hist_rev <- ggplot(data=Mexico_hist_rev) +
  
  geom_point(aes(x=exposure_ratio, y=ac_ratio, size = hh_state), 
             shape = 21, color = ifelse(Mexico_hist_rev$State == "", "grey80", "#FF00FF"), 
             fill = ifelse(Mexico_hist$State == "", "grey80", "#FF00FF"), alpha = 0.5)+
  
  # scale_size(range = c(1.5, 10)) +  # Adjust the range of points size
  scale_size_area()+

  xlim(0,12)+ ylim(0, 1.7)+
  
  xlab("CDD exposure relative to country average")+
  ylab("AC adoption relative to country average")+
  labs(color = "Country", size = "HH State" ) +
  #geom_vline(xintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  #geom_hline(yintercept = 1, color="red", size = 0.5, linetype = "dashed") +
  
  geom_text_repel(data = Mexico_hist_rev, mapping = aes(x=exposure_ratio, y=ac_ratio, label = State), 
                  nudge_y = 0.3, force = 12, direction = "x", angle = 0, vjust = 0,  segment.size = 0.8,
                  segment.alpha	= 0.7, 
                  segment.color =	ifelse(Mexico_hist_rev$ac_ratio > 1, "grey80", "#FF00FF")) + ## not working
  
  ggtitle("Historical period")+
  
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5), 
        # text = element_text(size = 16, family = "Tahoma", face = "bold"),
        axis.title.x = element_text(size = 12, family = "Tahoma"),
        axis.title.y = element_text(size = 14, family = "Tahoma"),
        axis.text.x=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        axis.text.y=element_text(size = 15, angle = 0, hjust = 0.5 , vjust = 0.5),
        # scale_fill_brewer(palette = "Accent") +
        # scale_fill_continuous(low = "plum1", high = "purple4") +
        legend.position = "none") 

# Mexico_bubble_plot_hist_rev

## Combine historical and future (on the same y-axis) side by side

Mexico_bubble_plot_hist_fut_on_same_y_axis_rev <- grid.arrange(Mexico_bubble_plot_hist_rev, 
                                                               Mexico_bubble_plot_fut_hist_y_axis, ncol=2)


#### Save plots ####

filename = c(paste0(plot_dir, "combined_hist_fut/zoom_y_axis/Brazil_bubble_plot_hist_fut_on_same_y_axis.png", sep=''))

png(filename = filename, res=1200, type="cairo", height=6.75, width=7.5, units="in", pointsize=12)

plot(Brazil_bubble_plot_hist_fut_on_same_y_axis_rev)

dev.off()

filename = c(paste0(plot_dir, "combined_hist_fut/zoom_y_axis/India_bubble_plot_hist_fut_on_same_y_axis.png", sep=''))

png(filename = filename, res=1200, type="cairo", height=6.75, width=7.5, units="in", pointsize=12)

plot(India_bubble_plot_hist_fut_on_same_y_axis_rev)

dev.off()

filename = c(paste0(plot_dir, "combined_hist_fut/zoom_y_axis/Indonesia_bubble_plot_hist_fut_on_same_y_axis.png", sep=''))

png(filename = filename, res=1200, type="cairo", height=6.75, width=7.5, units="in", pointsize=12)

plot(Indonesia_bubble_plot_hist_fut_on_same_y_axis_rev)

dev.off()

filename = c(paste0(plot_dir, "combined_hist_fut/zoom_y_axis/Mexico_bubble_plot_hist_fut_on_same_y_axis.png", sep=''))

png(filename = filename, res=1200, type="cairo", height=6.75, width=7.5, units="in", pointsize=12)

plot(Mexico_bubble_plot_hist_fut_on_same_y_axis_rev)

dev.off()


