### Berry Conservation - Species Distributions

library(tidyverse)
library(sp)
library(maptools)

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
