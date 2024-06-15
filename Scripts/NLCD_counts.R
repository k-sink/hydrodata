# Katharine Sink
# 6/4/2024
# National land cover unique values for raster 

library(terra)
library(tidyverse)

# path to clipped raster files
folder_path = "D:/University of Texas at Dallas/CombinedDataset/NLCD2006_clipped"

# loop through each raster
process_raster = function(file) {
  
   # read raster as spatraster
  basin_rast = terra::rast(file)
  
  # frequency (number of pixels) of each unique value in the raster
  basin_values = as.data.frame(freq(basin_rast))
  
  # get the basin id from the file name, remove the _clipped and .tif extension 
  basin_id = sub("_clipped\\.tif$", "", basename(file))
  
# create a data frame with BasinID and count columns 
  data = basin_values %>% 
   mutate(BasinID = basin_id) %>% 
   pivot_wider(names_from = value, values_from = count, values_fill = list(count = 0))
 
 return(data)
  
}

# get a list of rasters in the directory folder
raster_files = list.files(path = folder_path, pattern = "\\.tif$", full.names = TRUE)

# create an empty data frame to store the results
results = data.frame()

# loop through each file and process it
for (file in raster_files) {
  data = process_raster(file)
  results = bind_rows(results, data)
}

write.csv(results, "D:/University of Texas at Dallas/CombinedDataset/NLCD2006_counts.csv")  


###################################################################
## Land Cover Analyses with NLCD raster data
###################################################################
library(stars)

# open file as raster layer
NLCD_2006 = rast("D:/University of Texas at Dallas/NLCD/LC files/nlcd_2006_land_cover_l48_20210604/nlcd_2006_land_cover_l48_20210604.img")
st_crs(NLCD_2006)

basin = sf::st_read("D:/University of Texas at Dallas/CombinedDataset/Shapefiles/basin_USGS-01013500.shp")
st_crs(basin)

# albers conical equal area
prj = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

basin_prj = st_transform(basin, crs = prj)
lc2006 = terra::mask(NLCD_2006, basin_prj)
