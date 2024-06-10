library(sf)
library(dplyr)
library(raster)
library(fasterize)

# Start timer
start <- Sys.time()

# Read-in metadata csv
metadata <- read.csv("WTW/metadata/partner-metadata.csv")

# Read-in 10 ha vector grid with extracted variables
vector_grid <- read_sf("Regional/Extractions/o1_Fleming_PRZ_10HA.shp")
# vector_grid <- read_sf("Regional/Extractions/o2_NB_PRZ_10HA.shp")

# Read-in raster grid 
raster_grid <- raster("PU/PU0.tif")

# Get field names to rasterize
fields <- colnames(vector_grid)

# Remove fields that are not in metadata.csv
fields <- fields[fields %in% metadata$Shp_Name]

# Rasterize variables
for (i in seq_along(fields)) {
  print(paste0("Rasterize ", i, " of ", length(fields), ": ", fields[i]))
  ## extract row index from metadata csv
  idx <- which(metadata$Shp_Name == fields[i])
  ## extract shp name 
  tiff_name <- metadata[idx,]$File
  ## extract type
  threshold <- metadata[idx,]$Threshold
  ## rasterize variable
  x <- fasterize(vector_grid, raster_grid, field = fields[i])
  ## back fill 0 values
  tiff <- raster::mosaic(x, raster_grid, fun = "max")
  ## create binary variable
  if (!is.na(threshold)) {
    tiff[tiff < threshold] <- 0
    tiff[tiff >= threshold] <- 10
  }
  ## write to disk
  writeRaster(tiff, paste0("Tiffs/", tiff_name), overwrite = TRUE)
}

# End timer
end <- Sys.time()
print(end - start)
