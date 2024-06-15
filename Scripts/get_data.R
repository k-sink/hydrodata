# Katharine Sink 
# April 2024
# DOWNLOADING DATA 

# load libraries
library(tidyverse)
library(sf)
library(terra)
library(dataRetrieval) # USGS discharge data
library(nhdplusTools) # NWIS basin delineations
library(daymetr) # daymet data
library(FedData) # daymet data
library(lubridate)


########################################################################
## USGS DISCHARGE DATA RETRIEVAL ##
########################################################################
# obtain hydrologic data from USGS NWIS
# https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html

# get list of gauges as csv file (single column)
gauges = read.csv(file = "D://University of Texas at Dallas/CombinedDataset/Gaugelist.csv")
# imports as integer, convert to character
gauges = as.character(gauges$GaugeID)
# add leading zero to gauges with 7 characters to make it an 8 digit value
gauges = str_pad(gauges, max(nchar(gauges)), side = "left", pad = "0")

# obtain site information about the gauges (station name, drainage area, latitude, longitude)
siteinfo = readNWISsite(gauges)
# get available data for daily values (service), discharge (parameterCd), mean value (statCd)
available = whatNWISdata(siteNumber = gauges, service = "dv", parameterCd = "00060", statCd = "00003")

# loop through each gauge 
for (i in 1:length(gauges)){
# passing on the start and end date
startDate = as.Date("1980-01-01")
endDate <- as.Date("2023-12-31")
siteID = gauges[i]
discharge <- readNWISdata(sites = siteID,service = "dv",parameterCd = c("00060"), 
                         statCd = "00003", startDate = startDate, endDate = endDate)
if (length(discharge[,1])>0){ # checking if data is available
# saving the data in table format such that for "n" number of sites "n" number of files will be created
write.csv(discharge, paste0("D://University of Texas at Dallas/CombinedDataset/Discharge/", gauges[i], ".csv"), 
          row.names = FALSE, append = FALSE)
}
}

# discharge is in ft3/sec

########################################################################
## NHD BASIN DELINEATIONS ##
########################################################################
# basin delineations consistent with NWIS 
# https://doi-usgs.github.io/nhdplusTools/

nwis_site_ids = read.csv(file = "D://University of Texas at Dallas/CombinedDataset/NWIS_ID.csv")
# convert to character vector
nwis_site_ids = as.character(nwis_site_ids$GaugeID)

# create base directory to save shapefiles
base_dir = "D://University of Texas at Dallas/CombinedDataset/Shapefiles/"

# loop through each NWIS site ID
for (site_id in nwis_site_ids) {
  
  # create the NLDI query for the NWIS site
  nldi_query = list(featureSource = "nwissite", featureID = site_id)
  
  # get NLDI feature for the NWIS site
  #site = get_nldi_feature(nldi_query)
  
  # get NLDI basin for the NWIS site as sf dataframe, CRS is WGS 84
  basin = get_nldi_basin(nldi_feature = nldi_query)

  # create the file path for saving the shapefile
  shapefile_path = paste0(base_dir, "basin_", site_id, ".shp")
  
  # save the basin boundary as a shapefile
  st_write(basin, shapefile_path, append = FALSE)
  
  # print a message indicating the processing is complete for the current site
  cat("Processed NWIS site ID:", site_id, "\n")
}

########################################################################
## DAYMET DATA DOWNLOAD USING FEDDATA PACKAGE ##
########################################################################
# download Daymet using daymetr and FedData packages 
# https://cran.r-project.org/web/packages/daymetr/index.html
# daymet data consists of "tiles" which are 1kmx1km raster cells 

# this works for one variable at a time !
# dayl (during of daylight period in seconds per day), 
# prcp (daily total precip in mm per day, converted to water equivalent), 
# srad (incident shortwave radiation flux density in watts per square meter as average over daylight period of day)
# swe (snow water equivalent in kg per square meter, amount contained in snowpack)
# tmax (daily max 2-meter air temp in celcius), tmin
# vp (water vapor pressure in pascals, daily average partial pressure of water vapor)

# folder path containing shapefiles of basin boundaries downloaded from NLDI
folder_path = "D:/University of Texas at Dallas/CombinedDataset/Batch"

# create directory folder to save output csv files
folder = "D:/University of Texas at Dallas/CombinedDataset"
csv_folder = file.path(folder, "Daily_data")
dir.create(csv_folder, showWarnings = FALSE)

# get a list of shapefiles in the directory folder
shapefiles = list.files(path = folder_path, pattern = "\\.shp$", full.names = TRUE)

# loop through each shapefile
for (shapefile in shapefiles) {
  
   # read shapefile
  basin_shp = sf::st_read(shapefile)
  
  # make sure the shapefile is in the same CRS projection (Lambert Conformal Conic) as Daymet
  daymet_crs = "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  basin_shp = sf::st_transform(basin_shp, crs = daymet_crs)
  
  # get Daymet data, retrieves as spat raster
  day = FedData::get_daymet(template = basin_shp, label = "PRCP", 
                            elements = "prcp", years = 1980:2023, tempo = "day")
  
  # create as data frame
  day = as.data.frame(day)
  
  # extract day columns names (dates) using pattern 
  day_columns = grep("^prcp.", names(day), value = TRUE)
  
  # calculate column means since each column represents a day
  day_means = data.frame(Mean = colMeans(day))
 
  # combine dataframe of dates and column means
  result_df <- data.frame(Day = seq_along(day_columns), Mean = day_means)
  
  # change row name to column to get dates
  prcp <- tibble::rownames_to_column(result_df, "rn")
  
  # remove prcp 
  prcp$rn <- gsub("prcp.", "", as.character(prcp$rn))
  
  # create date column 
  prcp$DATE <- ymd(prcp$rn)
  prcp <- prcp %>% rename(PRCP = Mean) 
  prcp$PRCP <- round(prcp$PRCP, digits = 2)
  prcp <- prcp[,-c(1:2)]
  
  # write to CSV with shapefile ID as filename
  shapefile_id <- tools::file_path_sans_ext(basename(shapefile))
  csv_filename <- file.path(csv_folder, paste0(shapefile_id,"_Daymet_prcp.csv"))
  write.csv(prcp, file = csv_filename, row.names = FALSE)
  
  # print message indicating completion for the current shapefile
  cat("Processed shapefile:", shapefile_id, "\n")
  
  # remove files after each iteration to free memory
  rm(basin_shp, day, day_columns, day_means, result_df, prcp, shapefile_id, csv_filename)
  # garbage collector returns memory to OS
  gc()
}

########################################################################
## DAYMET DATA DOWNLOAD USING CLIMATER PACKAGE ##
########################################################################
library(climateR)

# loop through each shapefile
for (shapefile in shapefiles) {
  
   # read shapefile
  basin_shp = st_read(shapefile)
  
  # make sure the shapefile is in the same CRS (Lambert Conformal Conic) as Daymet
  daymet_crs = "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  basin_shp = st_transform(basin_shp, crs = daymet_crs)
  
  # get Daymet data, retrieves as spat raster
  day = climateR::getDaymet(AOI = basin_shp, varname = 'prcp', 
                            startDate = '1980-01-01', endDate = '2023-12-31')
  
}

####################################################
shp = sf::st_sfc(sf::st_point(c(-68.583, 47.237)), crs = 4326)

nldi_nwis = list(featureSource = "nwissite", featureID = "USGS-01013500")
point = discover_nhdplus_id(nldi_feature = nldi_nwis)

####################################################
raster_obj = day$prcp
raster_values = values(raster_obj)
raster_coordinates = xyFromCell(raster_obj, cell = 1:ncell(raster_obj))

raster_df <- data.frame(x = raster_coordinates[, 1],
                        y = raster_coordinates[, 2],
                        value = raster_values)

raster_columns = grep("^value.", names(raster_df), value = TRUE)

raster_means = data.frame(Mean = colMeans(raster_df))
 
###################################################
library(raster)
library(ncdf4)
library(exactextractr)

# daymet netcdf file for 1980 for north america
netcdf_file = "D:/University of Texas at Dallas/CombinedDataset/Daymet_gridded/daymet_v4_daily_na_prcp_1980.nc"
raster_data = raster::brick(netcdf_file)

shapefile = st_read("D:/University of Texas at Dallas/CombinedDataset/Batch/basin_USGS-05508000.shp")

if(st_crs(shapefile) != crs(raster_data)) {
  shapefile = st_transform(shapefile, crs = crs(raster_data))
}

extent_shapefile = extent(shapefile)
raster_cropped = crop(raster_data, extent_shapefile)
raster_masked = mask(raster_cropped, shapefile)
# values = exactextractr::exact_extract(raster, shapefile, 'mean')
values = exactextractr::exact_extract(raster, shapefile)
values_means = data.frame(Mean = colMeans(values))
