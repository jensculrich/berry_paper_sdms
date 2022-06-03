### Berry Conservation - Species Distribution Models
### In Situ Conservation Scores 
## Jens Ulrich May 2022

library(raster)
library(sf)
library(tidyverse)
library(cowplot)

# To generate in situ conservation scores for each species, I will calculate 
# the proportion of each species' range that is in protected areas 
# To do so, will need to take each species range (binary map) and identify
# the number of suitable habitat grid cells that overlap with protected area 
# grid cells (suitable habitat within protected areas) 
# and divide by the number of total grid cells (total suitable habitat).

# After looping across all n species, 
# this information will then be used to provide a conservation score 
# (prop of range in protected areas) for each species which will be 
# valuable in terms of identifying which taxa need most attention in Canada,
# and then will also be summarized by a mean and variation to provide
# a concrete precise summary of the low amount of range of berry CWR
# that is protected that we can visualize based on Figure 1.

# Later will loop across all species,
# Here visualize the range and protected areas for one single species

# Need to get raster extent from suitable area from one species so will walk through 
# a test while collecting that raster

## Run a test for what will go in the loop:
# Get a full list of all species to loop across: (this will go before the loop)
species_list <- read.csv(
  "./occurrence_data/occurrence_data_cleaned_thinned_no_crataegus.csv") %>%
  distinct(SPECIES)

# get species name from list of n = 144
sp_name <- species_list[5,1]
# and read in the raster associated with that species
suitable_area <- raster(paste0("./SSDM_no_crataegus/Species/", sp_name, "/Rasters/Binary.tif"))


# first, prep the protected area data
# read in all of the files
# read layers, later try to load kml into github folder (may be too big)
# and update file location accordingly
layer1 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers(
                                 "C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[2]))
layer2 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers(
                                 "C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[5]))
layer3 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[8]))
layer4 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[13]))
layer5 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[16]))
layer6 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[19]))
layer7 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[22]))
layer8 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[24]))
layer9 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[26]))
layer10 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[28]))
layer11 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[30]))
layer12 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[33]))
layer13 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[36]))
layer14 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[38]))
layer15 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[42]))
layer16 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[40]))

# now combine the multipolygon shapefiles (which all have matching attribute columns)
pa_layers <- rbind(layer1, layer2, layer3, layer4, layer5, layer6, layer7, layer8, 
                   layer9, layer10, layer11, layer12, layer13, layer14, 
                   layer15, layer16)

# free up session memory
rm(layer1, layer2, layer3, layer4, layer5, layer6, layer7, layer8, 
   layer9, layer10, layer11, layer12, layer13, layer14, 
   layer15, layer16)

# first rasterize protected areas and then subtracting them from suitable_area raster
layers_drop_z <- st_zm(pa_layers) # need to drop the z dimension in the PA

# rasterize layers using extent and pixel size of suitable area from a test species
# the extent and pixel size is the same for all n species in the dataset, so
# doesn't matter which species is used as the tester.
rasterized_pa <- rasterize(layers_drop_z, suitable_area) # rasterize the PA's at matching scale of suitable area

rasterized_pa_df <- as.data.frame(rasterized_pa, xy = TRUE) %>%
  filter(!is.na(layer_Name)) %>% 
  mutate(Binary = 1)


# calculate range size as the number of pixels suitable for species i:
suitable_area_df <- as.data.frame(suitable_area, xy = TRUE)
suitable_area_df_filtered <- drop_na(suitable_area_df) %>%
  filter(Binary == 1)
# Canadian range size = number of raster cells in Canada
range_size = nrow(suitable_area_df_filtered)

# to plot with ggplot, reformat the raster as a df 
suitable_area_df_filtered$Binary <- as.factor(as.integer(suitable_area_df_filtered$Binary))
levels(suitable_area_df_filtered$Binary) # should only be 1 (areas where plant occurs)

# plot the PA raster then the suitable area raster
p <- ggplot(rasterized_pa_df) +
  geom_tile(aes(x = x, y = y)) 

p <- p +
  theme_bw() + theme(axis.line=element_blank(),
                     panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.text.x=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     legend.position = c(0.2, 0.6),
                     legend.box.background = element_rect(colour = "black"))

p <- p +
  geom_tile(data = suitable_area_df_filtered, 
            aes(x = x, y = y, fill = as.factor(as.integer(Binary))
                ), alpha = 0.5) +
  scale_fill_manual(name = paste0("suitable habitat for ", sp_name),
                 labels = c("1" = "suitable"),
                 values = c("1" = "lightskyblue"),
                 na.value = "white")

p

# try an anti_join (remove rows in suitable_area_df_filtered, where there is 
# a match for BOTH x and y in rasterized_pa_df)
anti_join <- suitable_area_df_filtered %>%
  anti_join(rasterized_pa_df, 
            by=c("x", "y"))


# finally, divide number of unprotected pixels, by the entire
# range size to get the proportion of area protected.
# Subtract from 1 to get the proportion protected (in situ conservation score
# ranging from 0 (low) to 1 (complete/high))
prop_protected <-  1 - (nrow(anti_join) / range_size)

# and somehow return to a list that can be used as an output table 
# col1 = species name, col2 = range size, col3 = overlap area, col4 = proportion

prop_protected_area <- vector(length = nrow(species_list))


### Now loop across all species to get an output table with species name and
# in situ conservation score

for(i in 1:nrow(species_list)){ # for i in n species
  sp_name <- species_list[i,1] # take species name and access raster directory
  suitable_area <- raster(paste0("./SSDM_no_crataegus/Species/", sp_name, "/Rasters/Binary.tif"))
  
  # calculate range size as the number of pixels suitable for species i:
  suitable_area_df <- as.data.frame(suitable_area, xy = TRUE)
  suitable_area_df_filtered <- drop_na(suitable_area_df) %>%
    filter(Binary == 1) # only want the occupied pixels
  # Canadian range size = number of raster cells in Canada
  range_size = nrow(suitable_area_df_filtered) # range size = number of pixels
  
  # anti_join (remove rows in suitable_area_df_filtered, where there is 
  # a match for BOTH x and y in rasterized_pa_df)
  # this will remove pixels that are protected from the range
  # leaving only the unprotected pixels
  anti_join <- suitable_area_df_filtered %>%
    anti_join(rasterized_pa_df, 
              by=c("x", "y"))
  
  # finally, divide number of unprotected pixels, by the entire
  # range size to get the proportion of area protected.
  # Subtract from 1 to get the proportion protected (in situ conservation score
  # ranging from 0 (low) to 1 (complete/high))
  prop_protected_area[i] <-  1 - (nrow(anti_join) / range_size)
  
}

output_data <- cbind(species_list, prop_protected_area)
# write.csv(output_data, "./in_situ_conservation_scores.csv")

# calculate the mean and sd of protected suitable habitat across all n species
mean(prop_protected_area)
sd(prop_protected_area)
