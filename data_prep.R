### Berry Conservation - Species Distribution Models
### Data cleaning

library(tidyverse)
library(sf)
library(maptools)
library(geojsonsf)
library(raster)

berry_df1 <- read.csv("./occurrence_data/berry_occurrence.csv")
berry_df2 <- read.csv("./occurrence_data/berry_occurrence_part2.csv")

raw_occurrence_df <- rbind(berry_df1, berry_df2)

## remove duplicate records
occurrence_df <- raw_occurrence_df %>%
  distinct(occurrenceID, .keep_all = TRUE) %>% # remove duplicates of the same obseration
  select(family, genus, species, infraspecificEpithet, taxonRank, # select relevant cols
         stateProvince, decimalLatitude, decimalLongitude, year, basisOfRecord) %>%
  group_by(species, decimalLatitude, decimalLongitude) %>% 
  distinct(.keep_all = TRUE) %>% # remove duplicates from the exact same lat/long
  filter(species != "") %>% # there are 4 records with blank names
  filter(!is.na(decimalLongitude)) # remove any occurrence without lat/long
  
## look at some test species to validate data
occurrence_rubus_canadensis <- occurrence_df %>%
  filter(species == "Rubus canadensis")

data(wrld_simpl)
plot(wrld_simpl, xlim=c(-100, -40), ylim=c(40, 55), axes=TRUE, col="light yellow")
# restore the box around the map
box()
# add the points
points(occurrence_rubus_canadensis$decimalLongitude, 
       occurrence_rubus_canadensis$decimalLatitude, 
       col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(occurrence_rubus_canadensis$decimalLongitude, 
       occurrence_rubus_canadensis$decimalLatitude, 
       col='red', cex=0.75)

## remove some strange and far out of bounds occurrences 
# join province name to all points
# Convert to geoJSON for spatial projection
spatial_df <- df_geojson(df = occurrence_df, lon = "decimalLongitude", lat = "decimalLatitude")
points <- geojson_sf(spatial_df)

# read province data
provinces <- st_read("./geo_data/canada_provinces.geojson")
str(provinces)

# spatial join between provinces and lat/long of points
shape_joined_1 <- st_join(points, provinces, join = st_nearest_feature, maxdist = 10000)

shape_joined_2 <- shape_joined_1 %>%
  select(-cartodb_id, -draworder, - visibility, -extrude, -tessellate, -nom,
         -icon, -altitudemode, -X_end, -begin, -timestamp, -show, -X_2012_membership) %>%
  mutate(species = str_replace(species, "×", "")) %>%
  rename("SPECIES" = "species",
         "PROVINCE" = "name")

# now read in file with clearly out of bounds provinces (as described in FNA 2021)
out_of_bounds <- read.csv("./Geo_Data/out_of_bound_ranges_provincial.csv")
sp_distr_province <- anti_join(shape_joined_2, out_of_bounds, by = c('SPECIES', 'PROVINCE'))

# trim bioclim rasters to Canada
canada <- st_read("./Geo_Data/canada.geojson", quiet = TRUE) # 1
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2
plot <- ggplot(canada) +
  geom_sf(fill = "white", color = "gray60", size = 0.1) +
  coord_sf(crs = crs_string)
plot

bioclim1 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_1.tif", quiet = TRUE) # 1
bioclim1_crop <- crop(bioclim1, canada)
plot(bioclim1_crop)
bioclim1_mask <- mask(bioclim1_crop, canada)
plot(bioclim1_mask)
points(occurrence_rubus_canadensis$decimalLongitude, 
       occurrence_rubus_canadensis$decimalLatitude, 
       col='black', pch=20, cex=0.75)

writeRaster(bioclim1_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_1.tif")

bioclim2 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_2.tif", quiet = TRUE) # 1
bioclim2_crop <- crop(bioclim2, canada)
bioclim2_mask <- mask(bioclim2_crop, canada)
writeRaster(bioclim2_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_2.tif")

bioclim3 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_3.tif", quiet = TRUE) # 1
bioclim3_crop <- crop(bioclim3, canada)
bioclim3_mask <- mask(bioclim3_crop, canada)
writeRaster(bioclim3_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_3.tif")

bioclim4 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_4.tif", quiet = TRUE) # 1
bioclim4_crop <- crop(bioclim4, canada)
bioclim4_mask <- mask(bioclim4_crop, canada)
writeRaster(bioclim4_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_4.tif")

bioclim5 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_5.tif", quiet = TRUE) # 1
bioclim5_crop <- crop(bioclim5, canada)
bioclim5_mask <- mask(bioclim5_crop, canada)
writeRaster(bioclim5_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_5.tif")

bioclim6 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_6.tif", quiet = TRUE) # 1
bioclim6_crop <- crop(bioclim6, canada)
bioclim6_mask <- mask(bioclim6_crop, canada)
writeRaster(bioclim6_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_6.tif")

bioclim7 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_7.tif", quiet = TRUE) # 1
bioclim7_crop <- crop(bioclim7, canada)
bioclim7_mask <- mask(bioclim7_crop, canada)
writeRaster(bioclim7_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_7.tif")

bioclim8 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_8.tif", quiet = TRUE) # 1
bioclim8_crop <- crop(bioclim8, canada)
bioclim8_mask <- mask(bioclim8_crop, canada)
writeRaster(bioclim8_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_8.tif")

bioclim9 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_9.tif", quiet = TRUE) # 1
bioclim9_crop <- crop(bioclim9, canada)
bioclim9_mask <- mask(bioclim9_crop, canada)
writeRaster(bioclim9_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_9.tif")

bioclim10 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_10.tif", quiet = TRUE) # 1
bioclim10_crop <- crop(bioclim10, canada)
bioclim10_mask <- mask(bioclim10_crop, canada)
writeRaster(bioclim10_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_10.tif")

bioclim11 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_11.tif", quiet = TRUE) # 1
bioclim11_crop <- crop(bioclim11, canada)
bioclim11_mask <- mask(bioclim11_crop, canada)
writeRaster(bioclim11_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_11.tif")

bioclim12 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_12.tif", quiet = TRUE) # 1
bioclim12_crop <- crop(bioclim12, canada)
bioclim12_mask <- mask(bioclim12_crop, canada)
writeRaster(bioclim12_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_12.tif")

bioclim13 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_13.tif", quiet = TRUE) # 1
bioclim13_crop <- crop(bioclim13, canada)
bioclim13_mask <- mask(bioclim13_crop, canada)
writeRaster(bioclim13_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_13.tif")

bioclim14 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_14.tif", quiet = TRUE) # 1
bioclim14_crop <- crop(bioclim14, canada)
bioclim14_mask <- mask(bioclim14_crop, canada)
writeRaster(bioclim14_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_14.tif")

bioclim15 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_15.tif", quiet = TRUE) # 1
bioclim15_crop <- crop(bioclim15, canada)
bioclim15_mask <- mask(bioclim15_crop, canada)
writeRaster(bioclim15_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_15.tif")

bioclim16 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_16.tif", quiet = TRUE) # 1
bioclim16_crop <- crop(bioclim16, canada)
bioclim16_mask <- mask(bioclim16_crop, canada)
writeRaster(bioclim16_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_16.tif")

bioclim17 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_17.tif", quiet = TRUE) # 1
bioclim17_crop <- crop(bioclim17, canada)
bioclim17_mask <- mask(bioclim17_crop, canada)
writeRaster(bioclim17_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_17.tif")

bioclim18 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_18.tif", quiet = TRUE) # 1
bioclim18_crop <- crop(bioclim18, canada)
bioclim18_mask <- mask(bioclim18_crop, canada)
writeRaster(bioclim18_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_18.tif")

bioclim19 <- raster("./wc2.1_5m_bio/wc2.1_5m_bio_19.tif", quiet = TRUE) # 1
bioclim19_crop <- crop(bioclim19, canada)
bioclim19_mask <- mask(bioclim19_crop, canada)
writeRaster(bioclim19_mask, 
            "./wc2.1_5m_bio_masked/wc2.1_5m_bio_19.tif")
