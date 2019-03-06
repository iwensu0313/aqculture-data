## Shellfish Suitability
# Data Source
# Gentry et al 2017

library(tiff)
library(raster)

str_name<-'../gentry_2017/fish_sp_count.tiff' 

read_file <- readTIFF(str_name) 
imported_raster=raster(str_name)
