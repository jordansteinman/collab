library(sf)
library(dplyr)
library(units)

# Start timer
start <- Sys.time()

# Read-in metadata csv
metadata <- read.csv("WTW/metadata/partner-metadata.csv")

# Read-in 10 ha vector grid
grid <- read_sf("PU/PU.shp")

# Read-in intersection layer metadata
regional <- st_layers("Intersections.gdb")

# Extract to the grid
for (i in 1:length(regional$name)) {
  print(paste0("Extracting ", i, " of ", length(regional$name), ": ", regional$name[i]))
  
  # extract row index from metadata csv
  idx <- which(metadata$Feature_Name == regional$name[i])
  ## extract shp name 
  shp_name <- metadata[idx,]$Shp_Name
  
  ## read-in feature class
  print("... reading-in feature class")
  fc <- st_read("Intersections.gdb", regional$name[i], quiet = TRUE) 
  
  ## calculate area (ha) length (km) or count
  if (regional$geomtype[[i]] %in% c("Multi Polygon", "3D Measured Multi Polygon")) {
    fc$metric <- as.numeric(set_units(st_area(fc), "hectares"))
  } else if (identical(regional$geomtype[[i]], "Multi Line String")) {
    fc$metric <- as.numeric(set_units(st_length(fc), "kilometers"))
  } 
  
  print("... summarizing metric")
  
  ## NB custom
  if (identical(regional$geomtype[[i]], "Point")) {
    n_unique <- c("ACCDC_SAR_2022", "ACCDC_Significant_Species_2022", 
                  "ACCDC_Black_Ash", "ACCDC_Butternut", "Bur_Oak")
    if (regional$name[i] %in% n_unique) {
      ### point count (unique observations)
      metric_to_join <- fc %>%
        st_drop_geometry() %>%
        group_by(PUID) %>%
        summarise(!!shp_name := n_distinct(ELCODE)) 
    } else {
      ### point count (all observations)
      metric_to_join <- fc %>%
        st_drop_geometry() %>%
        group_by(PUID) %>%
        summarise(!!shp_name := n()) 
    }
  } else {
    ### polygons and lines
    metric_to_join <- fc %>%
      st_drop_geometry() %>%
      group_by(PUID) %>%
      dplyr::summarise(!!shp_name := round(sum(metric), 1))
  }
  
  ## join metric back to the grid
  print("... joining metric to the grid")
  grid <- left_join(grid, metric_to_join)
}


# Write extracted grid to disk
write_sf(grid, "REGIONAL/Extractions/o1_Fleming_PRZ_10HA.shp")

# End timer
end <- Sys.time()
print(end - start)
