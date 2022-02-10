### Berry Conservation - Species Distributions

library(tidyverse)
library(sf)
library(maptools)
library(geojsonsf)

### Data cleaning

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

  
