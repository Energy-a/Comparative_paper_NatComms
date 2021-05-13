
####### Replication code for  Fig. 1 (panel A)  and Fig. S1 (in supplementary material) ###################

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

## The script uses annual wet-bulb CDDs (CDDwb, Units: deg-Celcius days), computed using GLDAS data @ 0.25 deg 
## global gridded res (Mistry M.N. 2019) and country-specific shape files, to produce Maps in Figure 1 
## of mean 1970-2012 CDDwb at grid-cell level, with regional scale (ADM-1 level) boundaries. 

## The input annual CDDwb (1970-2019) netcdf file is made available in the 'input_data_files/netdf' folder. 
## The data is documented in below dataset reference paper and can be also downloaded from the repository:
## Mistry, M.N. (2019). Historical global gridded degree-days: A high-spatial resolution database of CDD an
## HDD. Geosci Data J. 2019; 6: 214- 221. https://doi.org/10.1002/gdj3.83

## In addition to the global-gridded map (Fig. S1, top panel), maps are created separately for each of the following
## 4 countries (Brazil, India, Indonesia and Mexico) for Fig. 1 (panel A).

## The script can be modified to plot different time periods and/or different indicator (e.g. CDD instead of CDDwb)

## The raster resolution (the raster brick object which contains data from netcdf) will have blocks of grid-cell
## proportionate to 0.25 deg res. When plotting, a part of these grid-cells will tend to fall outside the polygon 
## shape of the regions/countries. The raster::crop and raster::mask functions essentially retain the grid-cell (pixels)
## whose centroid falls within the country boundary. Thus there will be a few grid-cells that will plot outside the 
## country boundary, especially for countries having number of small islands such as Indonesia

## For the sake of graphics, the resolution of raster is changed to finer (i.e. disaggregated) so that the pixels
## do not fall outside the country boundaries. 

### The individual country shape file from GADM is downloaded here and the shape objects
## (spatial polygon data frames - SPDF) are read inside the for loop. The SPDF include regional (admin) boundaries 

## The required R packages will be installed (if not already installed) before loading ##

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc()                  # frees up memory resources

if(!require(sp)){install.packages("sp")}
if(!require(rgdal)){install.packages("rgdal")}
if(!require(raster)){install.packages("raster")}
if(!require(rworldmap)){install.packages("rworldmap")} ## Will be used for plotting global gridded CDDwb
if(!require(grid)){install.packages("grid")}

## Set path to main directory, input netcdf and output plot directories ##

stub                         <- 'G:/My Drive/3-Research/Data/'
GLDAS_dir                    <- paste(stub,'GLDAS_hdd_cdd/netcdf/',sep='')
plot_dir                     <- paste(stub,'OECD/Plots/four_countries/',sep='')

## BRA = Brazil, MEX = Mexico, IND = India, IDN = Indonesia ##

target_country               <- c('MEX', 'BRA', 'IDN', 'IND')

#### Read CDDwb (Netcdf file) computed on 0.25 deg Global Gridded GLDAS data (Years 1970-2018) ###
#### Both dry- and wet- bulb CDDs are available from https://doi.pangaea.de/10.1594/PANGAEA.903123

netcdf_file                  <- c(paste0(GLDAS_dir, '/cdd_twetbulb/gldas_0p25_deg_wet_bulb_cdd_base_T_24C_1970_2019_ann.nc4', sep=''))

CDDwb                        <- brick(netcdf_file, lvar=3, values=TRUE, level=1,
                                      varname="wet_bulb_derived_CDD_annual") 

dim(CDDwb)                 ## 600 1440   50 (Lat, Lon, Time) !

bbox(CDDwb)                ## Displays geographical (spatial) coordinates extent
raster::crs(CDDwb)         ## Displays CRS. Can also be used to assign a new CRS. See documentation

## Projection string can also be assigned as follows

projection(CDDwb)            <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## !! To subset the raster brick to a different time period, do as below ###
## For the purpose of our study, we utilize 1970-2012 as long term climatology ##

CDDwb_1970_2012              <- raster::subset(CDDwb, 1:43) ##e.g. for yrs 1970-2012, because 2012 is the 43rd year

### Mean  CDDwb for raster brick. 'Calc' can apply a function for all layers in a 
### RasterBrick object (i.e. on each grid-cell for the years 1970-2012). The output object is a single raster layer.

mean_1970_2012_CDDwb         <- raster::calc(CDDwb, mean, na.rm=TRUE)  ### Mean (1970-2012) at each grid cell (pixel). 
                                                                       ### This converts the global raster brick object 
                                                                       ### to a single global raster layer 

## Now disaggregate for smoother plotting. The raster object is still global ###

mean_1970_2012_CDDwb_disagg  <- raster::disaggregate(mean_1970_2012_CDDwb, fact = 10, method = "bilinear")

res(mean_1970_2012_CDDwb_disagg)  # 0.025 x 0.025 deg

##### Plot maps for each country in a for loop ###########

for (country in target_country) {

  print(country)

  ### Download the shapes (actually spatial polygon data frames) in loop for the current country ###
  
  myURL        <- paste0('https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_', country, '_1_sp.rds', sep='')
 
  ISO_spdf     <- readRDS(url(myURL))
  
  # Crop/mask the earlier disaggregated global raster (mean 1970-2012 CDDwb at each grid-cell)
  # to the country boundary. These masked raster objects will be used for plotting CDDwb, 
  # overlaid by their respective country regional boundaries.

  mean_1970_2012_CDDwb_country_cropped    <- raster::crop(mean_1970_2012_CDDwb_disagg, extent(ISO_spdf)) 
  mean_1970_2012_CDDwb_country_rasterized <- raster::rasterize(ISO_spdf, mean_1970_2012_CDDwb_country_cropped)
  mean_1970_2012_CDDwb_country            <- raster::mask(x=mean_1970_2012_CDDwb_country_cropped, 
                                                          mask=mean_1970_2012_CDDwb_country_rasterized)
  
  print("CDDwb global cropped and masked to country boundary")

  ### Overlay the four_countries CDDwb on the map using spplot ###

  pols1 <- list("sp.lines", as(ISO_spdf, 'SpatialLines'), cex = 1.2) ### These are the borders of the regions which will be 
                                                                     ### overlaid on the map after plotting the mean CDDwb   

  plot_CDDwb_mean <- spplot(mean_1970_2012_CDDwb_country, colorkey = list(space = "bottom", height = 1.0, labels=list(cex=1.1)),
                            cex=0.4, pch=15, auto.key = F,
                            col.regions = colorRampPalette(c("skyblue1", "yellow", "red")),
                            par.settings=list(axis.line=list(col='transparent'), layout.heights = list(key.top = 1.7)),
                            # fontsize=list(text=20)), # fontsize changes not only legend text size, but also that of legend key
                            sp.layout=list(pols1))
  
  png(paste0(plot_dir, 'CDD_wetbulb/mean/mean_CDDwb_24degC_1970_2012_', country,'.png',sep=''),
      height=6.75, width=7.5, units='in',res=1200)
  print(plot_CDDwb_mean)
  grid.text("\u00B0C days", x=unit(0.5, "npc"), y=unit(0.15, "npc"), rot=0,  gp=gpar(fontsize=15, col="black"))
  dev.off()

} ### End of for loop 

#### Plot Global Gridded mean 1970-2012 CDDwb (Global Map shown in Figure A.1 in Supplementary material) ###
# Get global map with country boundaries, using package 'rworldmap'

worldmap               <- getMap(resolution = "coarse")

pols1                  <- list("sp.lines", as(worldmap, 'SpatialLines'), cex = 1.2) ### These are the country borders which will be 
                                                                                    ### overlaid on the map after plotting the mean CDDwb   

## Use the global disaggregated CDDwb raster 

plot_global_CDDwb_mean <- spplot(mean_1970_2012_CDDwb_disagg, colorkey = list(space = "bottom", height = 1.0, labels=list(cex=1.1)),
                                 cex=0.4, pch=15, auto.key = F,
                                 col.regions = colorRampPalette(c("white", "yellow", "orange", "red")),
                                 par.settings=list(axis.line=list(col='transparent'), layout.heights = list(key.top = 1.7)),
                                 sp.layout=list(pols1))

png(paste0(plot_dir, 'CDD_wetbulb/mean/mean_CDDwb_24degC_1970_2012_global.png',sep=''),
    height=6.75, width=7.5, units='in',res=1200)
print(plot_global_CDDwb_mean)
grid.text("\u00B0C days", x=unit(0.5, "npc"), y=unit(0.15, "npc"), rot=0,  gp=gpar(fontsize=15, col="black"))
dev.off()

############### End of script ####################





