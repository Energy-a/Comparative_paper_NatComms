
####### Replication code for  Fig. 3 (Heat Maps) ###################

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

# (i)   State level data for each of the 4 countries (Brazil, Mexico, India and Indonesia)
# (ii)  Growth factor in electricity use across states (Ely) - intensive margin
# (iii) under RCPs 4.5 and 8.5, with each SSP [1-V]

### Similarly, instead of intensive margin, the projections of AC adoption for the 
### RCPs-SSPs are also done as separate plots

### This version plots the Ely and AC panels separately, with the RCPs 4.5 and 8.5 plotted side by side ###

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc()                  # frees up memory resources

### Install pages if required before loading ###

library(plotly)
library(tidyverse)
library(gdata)
library(readstata13)
library(reshape2)
library(RColorBrewer)

##########

## Note: the traditional way of saving .png plots using png(type=cairo) does not work with plot_ly
## So below Cairo packages are commented. Instead, to save plotly plots, 'phantomjs' and 'orca' 
## need to be installed (see below)

# library(Cairo)
# library(cairoDevice)

# capabilities()
# packageVersion("Cairo")
###################

# (i) Step 1: Install phantomjs (uncomment below and run)

# webshot::install_phantomjs() ##required to install for saving plot_ly objects

# (ii) Step 2: Install 'orca' following instructions here..and set the path in user environment variable
# Will require to relaunch RStudio after the installation (or simply close RStudio before installing orca)

# https://github.com/plotly/orca  (see windows installation)
# https://plotly.com/r/static-image-export/
# https://www.rdocumentation.org/packages/plotly/versions/4.9.0/topics/orca

## Set path to directories 

user <- 'mnm'

if (user=='mnm') {
 stub        <- 'C:/Users/malcon.mistry/Google Drive/'
}

dta_dir          <- paste(stub,'5-CountryExpertsExchange/Comparative_paper/modified_data/for_descriptives/', sep='')
plot_dir         <- paste(stub,'5-CountryExpertsExchange/Comparative_paper/R_plots/Heat_maps/', sep='')

### Read .dta file ###

FOUR_COUNTRIES_state  <- read.dta13(paste(dta_dir,'/HH_4countries_proj_state.dta', sep='')) ### Library readStata13

### Subset required columns of AC and intensive margin (ely growth) for all RCPs and SSPs ##

# FOUR_COUNTRIES_state <- FOUR_COUNTRIES_state[,c(2,3,15, 36,38,40,42,44,46,48,50,52,54,56:66)]
FOUR_COUNTRIES_state <- FOUR_COUNTRIES_state[,c(2,3,36,38,40,42,44,46,48,50,52,54, 58:68)]

names(FOUR_COUNTRIES_state) <- c("Country", "State", 
                                 "ely_rcp45_SSP1", "ely_rcp45_SSP2" , "ely_rcp45_SSP3" , 
                                 "ely_rcp45_SSP4", "ely_rcp45_SSP5", "ely_rcp85_SSP1",
                                 "ely_rcp85_SSP2", "ely_rcp85_SSP3", "ely_rcp85_SSP4",
                                 "ely_rcp85_SSP5",
                                 "ac_hist", "ac_rcp45_SSP1",  "ac_rcp45_SSP2",       
                                 "ac_rcp45_SSP3", "ac_rcp45_SSP4", "ac_rcp45_SSP5", 
                                 "ac_rcp85_SSP1", "ac_rcp85_SSP2", "ac_rcp85_SSP3", 
                                 "ac_rcp85_SSP4", "ac_rcp85_SSP5")

### Split above to electricity RCPs 4.5 and 8.5 ###

FOUR_COUNTRIES_state_ely_rcp45 <- FOUR_COUNTRIES_state[,c(1:7)]
FOUR_COUNTRIES_state_AC_rcp45  <- FOUR_COUNTRIES_state[,c(1:2,13:18)]

FOUR_COUNTRIES_state_ely_rcp85 <- FOUR_COUNTRIES_state[,c(1:2,8:12)]
FOUR_COUNTRIES_state_AC_rcp85  <- FOUR_COUNTRIES_state[,c(1:2,13,19:23)]

rm(FOUR_COUNTRIES_state)

### Rename columns (DROP ely and ac from column names for plots ###

names(FOUR_COUNTRIES_state_ely_rcp45) <- c("Country","State", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
names(FOUR_COUNTRIES_state_AC_rcp45)  <- c("Country","State", "HIST", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

names(FOUR_COUNTRIES_state_ely_rcp85) <- c("Country","State", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
names(FOUR_COUNTRIES_state_AC_rcp85)  <- c("Country","State", "HIST", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

### Now change from wide to long format for plotting ###

# FOUR_COUNTRIES_state$State <- factor(FOUR_COUNTRIES_state$State)
# FOUR_COUNTRIES_state$State <- as.character(FOUR_COUNTRIES_state$State)

# RCP 4.5
FOUR_COUNTRIES_state_ely_reshape_rcp45 <- reshape2::melt(FOUR_COUNTRIES_state_ely_rcp45, id.vars=c("Country", "State"))
FOUR_COUNTRIES_state_AC_reshape_rcp45  <- reshape2::melt(FOUR_COUNTRIES_state_AC_rcp45, id.vars=c("Country", "State"))

# RCP 8.5
FOUR_COUNTRIES_state_ely_reshape_rcp85 <- reshape2::melt(FOUR_COUNTRIES_state_ely_rcp85, id.vars=c("Country", "State"))
FOUR_COUNTRIES_state_AC_reshape_rcp85  <- reshape2::melt(FOUR_COUNTRIES_state_AC_rcp85, id.vars=c("Country", "State"))

rm(FOUR_COUNTRIES_state_ely_rcp45, FOUR_COUNTRIES_state_AC_rcp45, 
   FOUR_COUNTRIES_state_ely_rcp85, FOUR_COUNTRIES_state_AC_rcp85)
 
### Subset to individual countries. First Brazil. Reorder the states by increasing value of historical
### electricity growth factor, and ac adoption ###

Brazil_state_ely_rcp45         <- FOUR_COUNTRIES_state_ely_reshape_rcp45 %>% filter(Country=="Brazil")
# Brazil_state_ely_rcp45         <- Brazil_state_ely_rcp45 %>% arrange(State)
Brazil_state_ely_rcp45         <- Brazil_state_ely_rcp45 %>% arrange(value)
Brazil_state_ely_rcp45         <- Brazil_state_ely_rcp45 %>% arrange(variable)
Brazil_state_ely_rcp45         <- Brazil_state_ely_rcp45[,-c(1)]

Brazil_state_AC_rcp45          <- FOUR_COUNTRIES_state_AC_reshape_rcp45 %>% filter(Country=="Brazil")
# Brazil_state_AC_rcp45          <- Brazil_state_AC_rcp45 %>% arrange(State)
Brazil_state_AC_rcp45          <- Brazil_state_AC_rcp45 %>% arrange(value)
Brazil_state_AC_rcp45          <- Brazil_state_AC_rcp45 %>% arrange(variable)
Brazil_state_AC_rcp45          <- Brazil_state_AC_rcp45[,-c(1)]

Brazil_state_ely_rcp85         <- FOUR_COUNTRIES_state_ely_reshape_rcp85 %>% filter(Country=="Brazil")
Brazil_state_ely_rcp85         <- Brazil_state_ely_rcp85 %>% arrange(State)
Brazil_state_ely_rcp85         <- Brazil_state_ely_rcp85 %>% arrange(value)
Brazil_state_ely_rcp85         <- Brazil_state_ely_rcp85 %>% arrange(variable)
Brazil_state_ely_rcp85         <- Brazil_state_ely_rcp85[,-c(1)]

Brazil_state_AC_rcp85          <- FOUR_COUNTRIES_state_AC_reshape_rcp85 %>% filter(Country=="Brazil")
# Brazil_state_AC_rcp85          <- Brazil_state_AC_rcp85 %>% arrange(State)
Brazil_state_AC_rcp85          <- Brazil_state_AC_rcp85 %>% arrange(value)
Brazil_state_AC_rcp85          <- Brazil_state_AC_rcp85 %>% arrange(variable)
Brazil_state_AC_rcp85          <- Brazil_state_AC_rcp85[,-c(1)]

## INDIA 

India_state_ely_rcp45          <- FOUR_COUNTRIES_state_ely_reshape_rcp45 %>% filter(Country=="India")
# India_state_ely_rcp45          <- India_state_ely_rcp45 %>% arrange(State)
India_state_ely_rcp45          <- India_state_ely_rcp45 %>% arrange(value)
India_state_ely_rcp45          <- India_state_ely_rcp45 %>% arrange(variable)
India_state_ely_rcp45          <- India_state_ely_rcp45[,-c(1)]

India_state_AC_rcp45           <- FOUR_COUNTRIES_state_AC_reshape_rcp45 %>% filter(Country=="India")
# India_state_AC_rcp45           <- India_state_AC_rcp45 %>% arrange(State)
India_state_AC_rcp45           <- India_state_AC_rcp45 %>% arrange(value)
India_state_AC_rcp45           <- India_state_AC_rcp45 %>% arrange(variable)
India_state_AC_rcp45           <- India_state_AC_rcp45[,-c(1)]

India_state_ely_rcp85          <- FOUR_COUNTRIES_state_ely_reshape_rcp85 %>% filter(Country=="India")
# India_state_ely_rcp85          <- India_state_ely_rcp85 %>% arrange(State)
India_state_ely_rcp85          <- India_state_ely_rcp85 %>% arrange(value)
India_state_ely_rcp85          <- India_state_ely_rcp85 %>% arrange(variable)
India_state_ely_rcp85          <- India_state_ely_rcp85[,-c(1)]

India_state_AC_rcp85           <- FOUR_COUNTRIES_state_AC_reshape_rcp85 %>% filter(Country=="India")
# India_state_AC_rcp85           <- India_state_AC_rcp85 %>% arrange(State)
India_state_AC_rcp85           <- India_state_AC_rcp85 %>% arrange(value)
India_state_AC_rcp85           <- India_state_AC_rcp85 %>% arrange(variable)
India_state_AC_rcp85           <- India_state_AC_rcp85[,-c(1)]

## Indonesia

Indonesia_state_ely_rcp45         <- FOUR_COUNTRIES_state_ely_reshape_rcp45 %>% filter(Country=="Indonesia")
# Indonesia_state_ely_rcp45         <- Indonesia_state_ely_rcp45 %>% arrange(State)
Indonesia_state_ely_rcp45         <- Indonesia_state_ely_rcp45 %>% arrange(value)
Indonesia_state_ely_rcp45         <- Indonesia_state_ely_rcp45 %>% arrange(variable)
Indonesia_state_ely_rcp45         <- Indonesia_state_ely_rcp45[,-c(1)]

Indonesia_state_AC_rcp45          <- FOUR_COUNTRIES_state_AC_reshape_rcp45 %>% filter(Country=="Indonesia")
# Indonesia_state_AC_rcp45          <- Indonesia_state_AC_rcp45 %>% arrange(State)
Indonesia_state_AC_rcp45          <- Indonesia_state_AC_rcp45 %>% arrange(value)
Indonesia_state_AC_rcp45          <- Indonesia_state_AC_rcp45 %>% arrange(variable)
Indonesia_state_AC_rcp45          <- Indonesia_state_AC_rcp45[,-c(1)]

Indonesia_state_ely_rcp85         <- FOUR_COUNTRIES_state_ely_reshape_rcp85 %>% filter(Country=="Indonesia")
# Indonesia_state_ely_rcp85         <- Indonesia_state_ely_rcp85 %>% arrange(State)
Indonesia_state_ely_rcp85         <- Indonesia_state_ely_rcp85 %>% arrange(value)
Indonesia_state_ely_rcp85         <- Indonesia_state_ely_rcp85 %>% arrange(variable)
Indonesia_state_ely_rcp85         <- Indonesia_state_ely_rcp85[,-c(1)]

Indonesia_state_AC_rcp85          <- FOUR_COUNTRIES_state_AC_reshape_rcp85 %>% filter(Country=="Indonesia")
# Indonesia_state_AC_rcp85          <- Indonesia_state_AC_rcp85 %>% arrange(State)
Indonesia_state_AC_rcp85          <- Indonesia_state_AC_rcp85 %>% arrange(value)
Indonesia_state_AC_rcp85          <- Indonesia_state_AC_rcp85 %>% arrange(variable)
Indonesia_state_AC_rcp85          <- Indonesia_state_AC_rcp85[,-c(1)]

## Mexico ##
## State Morelos has crazy high Ely numbers..so dropping them for now in both RCPs

Mexico_state_ely_rcp45            <- FOUR_COUNTRIES_state_ely_reshape_rcp45 %>% filter(Country=="Mexico")
# Mexico_state_ely_rcp45            <- Mexico_state_ely_rcp45 %>% arrange(State)
Mexico_state_ely_rcp45            <- Mexico_state_ely_rcp45 %>% arrange(value)
Mexico_state_ely_rcp45            <- Mexico_state_ely_rcp45 %>% arrange(variable)
Mexico_state_ely_rcp45            <- Mexico_state_ely_rcp45[,-c(1)]
Mexico_state_ely_rcp45            <- Mexico_state_ely_rcp45[!(Mexico_state_ely_rcp45$State=="Morelos"),]

Mexico_state_AC_rcp45             <- FOUR_COUNTRIES_state_AC_reshape_rcp45 %>% filter(Country=="Mexico")
# Mexico_state_AC_rcp45             <- Mexico_state_AC_rcp45 %>% arrange(State)
Mexico_state_AC_rcp45             <- Mexico_state_AC_rcp45 %>% arrange(value)
Mexico_state_AC_rcp45             <- Mexico_state_AC_rcp45 %>% arrange(variable)
Mexico_state_AC_rcp45             <- Mexico_state_AC_rcp45[,-c(1)]

Mexico_state_ely_rcp85            <- FOUR_COUNTRIES_state_ely_reshape_rcp85 %>% filter(Country=="Mexico")
# Mexico_state_ely_rcp85            <- Mexico_state_ely_rcp85 %>% arrange(State)
Mexico_state_ely_rcp85            <- Mexico_state_ely_rcp85 %>% arrange(value)
Mexico_state_ely_rcp85            <- Mexico_state_ely_rcp85 %>% arrange(variable)
Mexico_state_ely_rcp85            <- Mexico_state_ely_rcp85[,-c(1)]
Mexico_state_ely_rcp85            <- Mexico_state_ely_rcp85[!(Mexico_state_ely_rcp85$State=="Morelos"),]

Mexico_state_AC_rcp85             <- FOUR_COUNTRIES_state_AC_reshape_rcp85 %>% filter(Country=="Mexico")
# Mexico_state_AC_rcp85             <- Mexico_state_AC_rcp85 %>% arrange(State)
Mexico_state_AC_rcp85             <- Mexico_state_AC_rcp85 %>% arrange(value)
Mexico_state_AC_rcp85             <- Mexico_state_AC_rcp85 %>% arrange(variable)
Mexico_state_AC_rcp85             <- Mexico_state_AC_rcp85[,-c(1)]

rm(FOUR_COUNTRIES_state_AC_reshape_rcp45, FOUR_COUNTRIES_state_ely_reshape_rcp45,
   FOUR_COUNTRIES_state_AC_reshape_rcp85, FOUR_COUNTRIES_state_ely_reshape_rcp85)

### Begin plots ###
# https://stackoverflow.com/questions/56834676/r-plotly-cannot-sort-graph-by-value
# https://colorbrewer2.org/#type=sequential&scheme=Purples&n=3

# Palette definition
# palette <- colorRampPalette(c("#f0f0f0",  "#bdbdbd", "#636363"))   # grey

palette <- colorRampPalette(c("#efedf5",  "#bcbddc", "#756bb1"))  # indigo

## Brazil

Brazil_state_ely_rcp45_plot  <- plot_ly(Brazil_state_ely_rcp45, 
                                        x = ~variable, 
                                        y = ~State, 
                                        z = ~value, 
                                        type = "heatmap", width = 700, height = 800, 
                                        # colorscale='Viridis', # other colorscale='Reds' or ‘Bluered’ or �Portland��...
                                        colors = palette(10),
                                        zmin = min(Brazil_state_ely_rcp45$value), #set min and max equal for the two different plots 
                                        zmax = max(Brazil_state_ely_rcp45$value), # so that the scale of z is comparable
                                        showscale=F,
                                        colorbar = list(title = list(text = "Ratio", 
                                                                     side = "top",
                                                        font = list(size = 20)),
                                                        tickfont = list(size = 16)))%>%
    layout(title = list(text = "<b>Electricity Growth Factor<b>", align = "center", y='0.99'),
                        font=list(size=14),
           xaxis = list(type = 'category', title = list(text = "<b>RCP 4.5<b>", titlefont = list(size = 18)), 
                        tickfont = list(size = 16)), 
           yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Brazil_state_ely_rcp45$State))

# Note: If the y-axis is to be reordered from lowest to highest (from top to bottom), use rev(categoryarray = Brazil_state_ely_rcp45$State)

# Brazil_state_ely_rcp45_plot

Brazil_state_ely_rcp85_plot  <- plot_ly(Brazil_state_ely_rcp85, 
                                        x = ~variable, 
                                        y = ~State, 
                                        z = ~value, 
                                        type = "heatmap", width = 700, height = 800,  
                                        # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                        colors = palette(10),
                                        zmin = min(Brazil_state_ely_rcp85$value), #set min and max equal for the two different plots 
                                        zmax = max(Brazil_state_ely_rcp85$value), # so that the scale of z is comparable
                                        showscale=T,
                                        colorbar = list(title = list(text = "Ratio", 
                                                                     side = "top",
                                                                     font = list(size = 20)),
                                                        tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>Electricity Growth Factor<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 8.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Brazil_state_ely_rcp85$State))

Brazil_state_AC_plot_rcp45  <- plot_ly(Brazil_state_AC_rcp45, 
                                       x = ~variable, 
                                       y = ~State, 
                                       z = ~value, 
                                       type = "heatmap", width = 700, height = 800,  
                                       # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                       colors = palette(10),
                                       zmin = min(Brazil_state_AC_rcp45$value), #set min and max equal for the two different plots 
                                       zmax = max(Brazil_state_AC_rcp45$value), # so that the scale of z is comparable
                                       showscale=F,
                                       colorbar = list(title = list(text = "%", 
                                                                    side = "top",
                                                                    font = list(size = 20)),
                                                       tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>AC Adoption<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 4.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Brazil_state_AC_rcp45$State))

# Brazil_state_AC_plot_rcp45
           
Brazil_state_AC_plot_rcp85  <- plot_ly(Brazil_state_AC_rcp85, 
                                       x = ~variable, 
                                       y = ~State, 
                                       z = ~value, 
                                       # text = as.character(round(Brazil_state_AC_rcp85$value[1:26], digits =2)),
                                       type = "heatmap", width = 700, height = 800,  
                                       # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                       colors = palette(10),
                                       zmin = min(Brazil_state_AC_rcp85$value), #set min and max equal for the two different plots 
                                       zmax = max(Brazil_state_AC_rcp85$value), # so that the scale of z is comparable
                                       showscale=T,
                                       colorbar = list(title = list(text = "%", 
                                                                    side = "top",
                                                                    font = list(size = 20)),
                                                       tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>AC Adoption<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 8.5<b>", 
                                                      titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Brazil_state_AC_rcp85$State))

Brazil_state_AC_plot_rcp85

#### Combine plots (RCP 4.5 and 8.5) ###

Brazil_state_ely_plot_rcps_combined <- subplot(Brazil_state_ely_rcp45_plot, Brazil_state_ely_rcp85_plot, 
                                              nrows = 1, shareX = F, shareY = TRUE, titleX = T)
Brazil_state_ely_plot_rcps_combined


Brazil_state_AC_plot_rcps_combined <- subplot(Brazil_state_AC_plot_rcp45, Brazil_state_AC_plot_rcp85, 
                                              nrows = 1, shareX = F, shareY = TRUE, titleX = T)

Brazil_state_AC_plot_rcps_combined

#### Repeat for India ###

India_state_ely_rcp45_plot  <- plot_ly(India_state_ely_rcp45, 
                                       x = ~variable, 
                                       y = ~State, 
                                       z = ~value, 
                                       type = "heatmap", width = 700, height = 800,  
                                       # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                       colors = palette(10),
                                       zmin = min(India_state_ely_rcp45$value), #set min and max equal for the two different plots 
                                       zmax = max(India_state_ely_rcp45$value), # so that the scale of z is comparable
                                       showscale=F,
                                       colorbar = list(title = list(text = "Ratio", 
                                                                    side = "top",
                                                                    font = list(size = 20)),
                                                       tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>Electricity Growth Factor<b>", align = "center", y='0.99'),
         font=list(size=14),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 4.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = India_state_ely_rcp45$State))

# India_state_ely_rcp45_plot

India_state_ely_rcp85_plot  <- plot_ly(India_state_ely_rcp85, 
                                       x = ~variable, 
                                       y = ~State, 
                                       z = ~value, 
                                       type = "heatmap", width = 700, height = 800,  
                                       # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                       colors = palette(10),
                                       zmin = min(India_state_ely_rcp85$value), #set min and max equal for the two different plots 
                                       zmax = max(India_state_ely_rcp85$value), # so that the scale of z is comparable
                                       showscale=T,
                                       colorbar = list(title = list(text = "Ratio", 
                                                                    side = "top",
                                                                    font = list(size = 20)),
                                                       tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>Electricity Growth Factor<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 8.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = India_state_ely_rcp85$State))

India_state_AC_plot_rcp45  <- plot_ly(India_state_AC_rcp45, 
                                      x = ~variable, 
                                      y = ~State, 
                                      z = ~value, 
                                      type = "heatmap", width = 700, height = 800,  
                                      # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                      colors = palette(10),
                                      zmin = min(India_state_AC_rcp45$value), #set min and max equal for the two different plots 
                                      zmax = max(India_state_AC_rcp45$value), # so that the scale of z is comparable
                                      showscale=F,
                                      colorbar = list(title = list(text = "%", 
                                                                   side = "top",
                                                                   font = list(size = 20)),
                                                      tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>AC Adoption<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 4.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = India_state_AC_rcp45$State))

# India_state_AC_plot_rcp45

India_state_AC_plot_rcp85  <- plot_ly(India_state_AC_rcp85, 
                                      x = ~variable, 
                                      y = ~State, 
                                      z = ~value, 
                                      type = "heatmap", width = 700, height = 800,  
                                      # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                      colors = palette(10),
                                      zmin = min(India_state_AC_rcp85$value), #set min and max equal for the two different plots 
                                      zmax = max(India_state_AC_rcp85$value), # so that the scale of z is comparable
                                      showscale=T,
                                      colorbar = list(title = list(text = "%", 
                                                                   side = "top",
                                                                   font = list(size = 20)),
                                                      tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>AC Adoption<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 8.5<b>", 
                                                      titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = India_state_AC_rcp45$State))

# India_state_AC_plot_rcp85

#### Combine plots (RCP 4.5 and 8.5) ###

India_state_ely_plot_rcps_combined <- subplot(India_state_ely_rcp45_plot, India_state_ely_rcp85_plot, 
                                              nrows = 1, shareX = F, shareY = TRUE, titleX = T)
India_state_ely_plot_rcps_combined


India_state_AC_plot_rcps_combined <- subplot(India_state_AC_plot_rcp45, India_state_AC_plot_rcp85, 
                                             nrows = 1, shareX = F, shareY = TRUE, titleX = T)

India_state_AC_plot_rcps_combined

### Repeat for Indonesia ###

Indonesia_state_ely_rcp45_plot  <- plot_ly(Indonesia_state_ely_rcp45, 
                                           x = ~variable, 
                                           y = ~State, 
                                           z = ~value, 
                                           type = "heatmap", width = 700, height = 800,  
                                           # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                           colors = palette(10),
                                           zmin = min(Indonesia_state_ely_rcp45$value), #set min and max equal for the two different plots 
                                           zmax = max(Indonesia_state_ely_rcp45$value), # so that the scale of z is comparable
                                           showscale=F,
                                           colorbar = list(title = list(text = "Ratio", 
                                                                        side = "top",
                                                                        font = list(size = 20)),
                                                           tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>Electricity Growth Factor<b>", align = "center", y='0.99'),
         font=list(size=14),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 4.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Indonesia_state_ely_rcp45$State))

# Indonesia_state_ely_rcp45_plot

Indonesia_state_ely_rcp85_plot  <- plot_ly(Indonesia_state_ely_rcp85, 
                                           x = ~variable, 
                                           y = ~State, 
                                           z = ~value, 
                                           type = "heatmap", width = 700, height = 800,  
                                           # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                           colors = palette(10),
                                           zmin = min(Indonesia_state_ely_rcp85$value), #set min and max equal for the two different plots 
                                           zmax = max(Indonesia_state_ely_rcp85$value), # so that the scale of z is comparable
                                           showscale=T,
                                           colorbar = list(title = list(text = "Ratio", 
                                                                        side = "top",
                                                                        font = list(size = 20)),
                                                           tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>Electricity Growth Factor<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 8.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Indonesia_state_ely_rcp85$State))

Indonesia_state_AC_plot_rcp45  <- plot_ly(Indonesia_state_AC_rcp45, 
                                          x = ~variable, 
                                          y = ~State, 
                                          z = ~value, 
                                          type = "heatmap", width = 700, height = 800,  
                                          # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                          colors = palette(10),
                                          zmin = min(Indonesia_state_AC_rcp45$value), #set min and max equal for the two different plots 
                                          zmax = max(Indonesia_state_AC_rcp45$value), # so that the scale of z is comparable
                                          showscale=F,
                                          colorbar = list(title = list(text = "%", 
                                                                       side = "top",
                                                                       font = list(size = 20)),
                                                          tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>AC Adoption<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 4.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Indonesia_state_AC_rcp45$State))

# Indonesia_state_AC_plot_rcp45

Indonesia_state_AC_plot_rcp85  <- plot_ly(Indonesia_state_AC_rcp85, 
                                          x = ~variable, 
                                          y = ~State, 
                                          z = ~value, 
                                          type = "heatmap", width = 700, height = 800,  
                                          # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                          colors = palette(10),
                                          zmin = min(Indonesia_state_AC_rcp85$value), #set min and max equal for the two different plots 
                                          zmax = max(Indonesia_state_AC_rcp85$value), # so that the scale of z is comparable
                                          showscale=T,
                                          colorbar = list(title = list(text = "%", 
                                                                       side = "top",
                                                                       font = list(size = 20)),
                                                          tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>AC Adoption<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 8.5<b>", 
                                                      titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Indonesia_state_AC_rcp85$State))

# Indonesia_state_AC_plot_rcp85

#### Combine plots (RCP 4.5 and 8.5) ###

Indonesia_state_ely_plot_rcps_combined <- subplot(Indonesia_state_ely_rcp45_plot, Indonesia_state_ely_rcp85_plot, 
                                                  nrows = 1, shareX = F, shareY = TRUE, titleX = T)
Indonesia_state_ely_plot_rcps_combined


Indonesia_state_AC_plot_rcps_combined <- subplot(Indonesia_state_AC_plot_rcp45, Indonesia_state_AC_plot_rcp85, 
                                                 nrows = 1, shareX = F, shareY = TRUE, titleX = T)

Indonesia_state_AC_plot_rcps_combined

### Mexico ####

Mexico_state_ely_rcp45_plot  <- plot_ly(Mexico_state_ely_rcp45, 
                                        x = ~variable, 
                                        y = ~State, 
                                        z = ~value, 
                                        type = "heatmap", width = 700, height = 800,  
                                        # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                        colors = palette(10),
                                        zmin = min(Mexico_state_ely_rcp45$value), #set min and max equal for the two different plots 
                                        zmax = max(Mexico_state_ely_rcp45$value), # so that the scale of z is comparable
                                        showscale=F,
                                        colorbar = list(title = list(text = "Ratio", 
                                                                     side = "top",
                                                                     font = list(size = 20)),
                                                        tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>Electricity Growth Factor<b>", align = "center", y='0.99'),
         font=list(size=14),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 4.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Mexico_state_ely_rcp45$State))

# Mexico_state_ely_rcp45_plot

Mexico_state_ely_rcp85_plot  <- plot_ly(Mexico_state_ely_rcp85, 
                                        x = ~variable, 
                                        y = ~State, 
                                        z = ~value, 
                                        type = "heatmap", width = 700, height = 800,  
                                        # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                        colors = palette(10),
                                        zmin = min(Mexico_state_ely_rcp85$value), #set min and max equal for the two different plots 
                                        zmax = max(Mexico_state_ely_rcp85$value), # so that the scale of z is comparable
                                        showscale=T,
                                        colorbar = list(title = list(text = "Ratio", 
                                                                     side = "top",
                                                                     font = list(size = 20)),
                                                        tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>Electricity Growth Factor<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 8.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Mexico_state_ely_rcp85$State))

Mexico_state_AC_plot_rcp45  <- plot_ly(Mexico_state_AC_rcp45, 
                                       x = ~variable, 
                                       y = ~State, 
                                       z = ~value, 
                                       type = "heatmap", width = 700, height = 800,  
                                       # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                       colors = palette(10),
                                       zmin = min(Mexico_state_AC_rcp45$value), #set min and max equal for the two different plots 
                                       zmax = max(Mexico_state_AC_rcp45$value), # so that the scale of z is comparable
                                       showscale=F,
                                       colorbar = list(title = list(text = "%", 
                                                                    side = "top",
                                                                    font = list(size = 20)),
                                                       tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>AC Adoption<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 4.5<b>", titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Mexico_state_AC_rcp45$State))

# Mexico_state_AC_plot_rcp45

Mexico_state_AC_plot_rcp85  <- plot_ly(Mexico_state_AC_rcp85, 
                                       x = ~variable, 
                                       y = ~State, 
                                       z = ~value, 
                                       type = "heatmap", width = 700, height = 800,  
                                       # colorscale='Portland', # other colorscale='Reds' or ‘Bluered’ or ‘Viridis’...
                                       colors = palette(10),
                                       zmin = min(Mexico_state_AC_rcp85$value), #set min and max equal for the two different plots 
                                       zmax = max(Mexico_state_AC_rcp85$value), # so that the scale of z is comparable
                                       showscale=T,
                                       colorbar = list(title = list(text = "%", 
                                                                    side = "top",
                                                                    font = list(size = 20)),
                                                       tickfont = list(size = 16)))%>%
  layout(title = list(text = "<b>AC Adoption<b>", align = "center", y='0.99'),
         font=list(size=12.5),
         xaxis = list(type = 'category', title = list(text = "<b>RCP 8.5<b>", 
                                                      titlefont = list(size = 18)), 
                      tickfont = list(size = 16)), 
         yaxis = list(title = "", tickfont = list(size = 16), categoryorder = "array", categoryarray = Mexico_state_AC_rcp85$State))

# Mexico_state_AC_plot_rcp85

#### Combine plots (RCP 4.5 and 8.5) ###

Mexico_state_ely_plot_rcps_combined <- subplot(Mexico_state_ely_rcp45_plot, Mexico_state_ely_rcp85_plot, 
                                               nrows = 1, shareX = F, shareY = TRUE, titleX = T)
Mexico_state_ely_plot_rcps_combined


Mexico_state_AC_plot_rcps_combined <- subplot(Mexico_state_AC_plot_rcp45, Mexico_state_AC_plot_rcp85, 
                                              nrows = 1, shareX = F, shareY = TRUE, titleX = T)

Mexico_state_AC_plot_rcps_combined

#### Save Individual country plots ####

filename <- c(paste0(plot_dir, 'Ely/Brazil_Ely_growth_factor_rcp45.png',  sep = ""))

orca(Brazil_state_ely_rcp45_plot, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

filename <- c(paste0(plot_dir, 'Ely/Brazil_Ely_growth_factor_rcp85.png',  sep = ""))

orca(Brazil_state_ely_rcp85_plot, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)


filename <- c(paste0(plot_dir, 'AC/Brazil_AC_rcp45.png',  sep = ""))

orca(Brazil_state_AC_plot_rcp45, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

filename <- c(paste0(plot_dir, 'AC/Brazil_AC_rcp85.png',  sep = ""))

orca(Brazil_state_AC_plot_rcp85, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

## India ##

filename <- c(paste0(plot_dir, 'Ely/India_Ely_growth_factor_rcp45.png',  sep = ""))

orca(India_state_ely_rcp45_plot, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

filename <- c(paste0(plot_dir, 'Ely/India_Ely_growth_factor_rcp85.png',  sep = ""))

orca(India_state_ely_rcp85_plot, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)


filename <- c(paste0(plot_dir, 'AC/India_AC_rcp45.png',  sep = ""))

orca(India_state_AC_plot_rcp45, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

filename <- c(paste0(plot_dir, 'AC/India_AC_rcp85.png',  sep = ""))

orca(India_state_AC_plot_rcp85, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

## Indonesia ##

filename <- c(paste0(plot_dir, 'Ely/Indonesia_Ely_growth_factor_rcp45.png',  sep = ""))

orca(Indonesia_state_ely_rcp45_plot, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

filename <- c(paste0(plot_dir, 'Ely/Indonesia_Ely_growth_factor_rcp85.png',  sep = ""))

orca(Indonesia_state_ely_rcp85_plot, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)


filename <- c(paste0(plot_dir, 'AC/Indonesia_AC_rcp45.png',  sep = ""))

orca(Indonesia_state_AC_plot_rcp45, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

filename <- c(paste0(plot_dir, 'AC/Indonesia_AC_rcp85.png',  sep = ""))

orca(Indonesia_state_AC_plot_rcp85, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

## Mexico ##

filename <- c(paste0(plot_dir, 'Ely/Mexico_Ely_growth_factor_rcp45.png',  sep = ""))

orca(Mexico_state_ely_rcp45_plot, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

filename <- c(paste0(plot_dir, 'Ely/Mexico_Ely_growth_factor_rcp85.png',  sep = ""))

orca(Mexico_state_ely_rcp85_plot, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)


filename <- c(paste0(plot_dir, 'AC/Mexico_AC_rcp45.png',  sep = ""))

orca(Mexico_state_AC_plot_rcp45, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

filename <- c(paste0(plot_dir, 'AC/Mexico_AC_rcp85.png',  sep = ""))

orca(Mexico_state_AC_plot_rcp85, file = filename, format="png",
     height = 3 * 300, width = 7 * 300)

