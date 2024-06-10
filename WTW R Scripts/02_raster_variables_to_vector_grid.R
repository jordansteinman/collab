library(raster)
library(sf)
library(exactextractr)

# Start timer
start <- Sys.time()

# Read-in grid
grid <- read_sf("AOI/NB_10ha.shp")

# ARA ----
ara_open_water <- raster("Regional/ARA_Open_Water.tif")
grid$ARA1 <- exact_extract(ara_open_water, grid, fun = "sum")
grid$ARA1  <- (grid$ARA1  * 900) / 10000 # convert m2 to ha

ara_flood_zone <- raster("Regional/ARA_Flood_Zone.tif")
grid$ARA2 <- exact_extract(ara_flood_zone, grid, fun = "sum")
grid$ARA2  <- (grid$ARA2  * 900) / 10000 # convert m2 to ha

ara_riparian_wetlands <- raster("Regional/ARA_Riparian_Wetlands.tif")
grid$ARA3 <- exact_extract(ara_riparian_wetlands, grid, fun = "sum")
grid$ARA3  <- (grid$ARA3  * 900) / 10000 # convert m2 to ha

ara_steep_slopes <- raster("Regional/ARA_Steep_Slopes.tif")
grid$ARA4 <- exact_extract(ara_steep_slopes, grid, fun = "sum")
grid$ARA4  <- (grid$ARA4  * 900) / 10000 # convert m2 to ha

# Upland ----
upland <- raster("Regional/NB_Elev_Steep.tif")
grid$Upland <- exact_extract(upland, grid, fun = "sum")
grid$Upland  <- (grid$Upland  * 900) / 10000 # convert m2 to ha

# Carbon ----
carbon <- raster("Regional/NB_WWF_CARBON.tif")
grid$Carbon <- exact_extract(carbon, grid, fun = "sum")

# TNC ----
TNC_13 <- raster("Regional/TNC_13.tif")
grid$TNC13 <- exact_extract(TNC_13, grid, fun = "sum")
grid$TNC13  <- (grid$TNC13  * 900) / 10000 # convert m2 to ha

TNC_12 <- raster("Regional/TNC_12.tif")
grid$TNC12 <- exact_extract(TNC_12, grid, fun = "sum")
grid$TNC12  <- (grid$TNC12  * 900) / 10000 # convert m2 to ha

TNC_11 <- raster("Regional/TNC_11.tif")
grid$TNC11 <- exact_extract(TNC_11, grid, fun = "sum")
grid$TNC11  <- (grid$TNC11  * 900) / 10000 # convert m2 to ha

# Round extractions to 1 decimal place
grid <- grid %>% mutate(across(3:11, round, 1))

# Write to disk
write_sf(grid, "REGIONAL/Extractions/o2_NB_PRZ_10HA.shp")

# End timer
end <- Sys.time()
print(end - start)
