### Berry Conservation - Species Distribution Models
### In Situ Conservation Scores 
## Jens Ulrich May 2022

library(raster)
library(sf)
library(tidyverse)
library(cowplot)

# To generate in situ conservation scores for each species, I will calculate 
# the proportion of each species' range that is in protected areas 
# or within a buffer zone of (say, 10 or 50 km of) protected areas.
# To do so, will need to take a species range (binary map) and identify
# the number of grid pixels that are within protected area buffers 
# and divide by the number of total grid pixels.

# This information will then be used to provide a conservation score 
# (prop of range in protected areas) for each species which will be 
# valuable in terms of identifying which taxa need most attention in Canada,
# and then will also be summarized by a mean and variation to provide
# a concrete precise summary of the low amount of range of berry CWR
# that is protected that we can visualize based on Figure 1.

# Test on one species, then loop over all n species
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# read national shapefile boundary
canada <- st_read("./geo_data/canada_provinces.geojson")
canada2 <- st_transform(canada, crs = crs_string)

# Read species distribution (binary) 
sp_name <- "Amelanchier alnifolia" # test species
sp_range <-  raster(paste0("./SSDM_no_crataegus/Species/", sp_name, "/Rasters/Binary.tif"))

# project the range and mask to canadian boundary
sdm_diversity_projected <- projectRaster(sp_range, crs = crs_string)
sdm_diversity_crop <- crop(sdm_diversity_projected, canada2)
sdm_diversity_mask <- mask(sdm_diversity_crop, canada2)

# Transform to df for plotting / visualization
sdm_diversity_df <- as.data.frame(sdm_diversity_mask, xy = TRUE)

# visualize binary distribution map
sdm_diversity_df$Binary <- as.factor(as.integer(sdm_diversity_df$Binary))
levels(sdm_diversity_df$Binary)

p <- ggplot(sdm_diversity_df) +
  geom_tile(aes(x = x, y = y,
                fill = Binary)) +
  scale_fill_manual(name = paste0("suitable habitat for ", sp_name),
                      labels = c("0" = "unsuitable",
                                 "1" = "suitable"),
                       values = c("0" = "aliceblue", 
                                  "1" = "lightskyblue"),
                       na.value = "white")

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
                     legend.position = c(0.8, 0.7))
p

## Run a test for what will go in the loop:
# Get a full list of all species to loop across: (this will go before the loop)
species_list <- read.csv(
  "./occurrence_data/occurrence_data_cleaned_thinned_no_crataegus.csv") %>%
  distinct(SPECIES)

# get protected area extent (here using BC province outline until I have PA's)
# for test (see how much of the range is covered by BC)
# BC_sf <- filter(canada[1,])
# BC_sf_2 <- st_transform(BC_sf, crs = crs_string)

layer2 <- st_transform(st_read("C:/Users/jensj/Desktop/Berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")$name[5]))
#                       ,
 #                      crs = crs_string)

# get species name from list of n = 144
sp_name <- species_list[5,1]
# and read in the raster associated with that species
suitable_area <- raster(paste0("./SSDM_no_crataegus/Species/", sp_name, "/Rasters/Binary.tif"))
# assign(paste0(sub(" ", "_", sp_name), "_range"), df)

# calculate range size as the number of pixels suitable for species i:
# suitable_area_projected <- projectRaster(suitable_area, crs = crs_string)
# suitable_area_projected <- projectRaster(suitable_area, crs = crs_string)
suitable_area_df <- as.data.frame(suitable_area, xy = TRUE)
suitable_area_df_filtered <- drop_na(suitable_area_df) %>%
  filter(Binary == 1)
# Canadian range size = number of raster cells in Canada
range_size = nrow(suitable_area_df_filtered)


suitable_area_df_filtered$Binary <- as.factor(as.integer(suitable_area_df_filtered$Binary))
levels(suitable_area_df_filtered$Binary) # should only be 1 (areas where plant occurs)

p <- ggplot(suitable_area_df_filtered) +
  geom_tile(aes(x = x, y = y,
                fill = Binary)) +
  scale_fill_manual(name = paste0("suitable habitat for ", sp_name),
                    labels = c(#"0" = "unsuitable",
                               "1" = "suitable"),
                    values = c(#"0" = "aliceblue", 
                               "1" = "lightskyblue"),
                    na.value = "white")

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
                     legend.position = c(0.8, 0.7))

# layer 2 (BC protected areas) should map [with imperfect intersection]
# over the suitable area for a species found in BC
p <- p +
  geom_sf(data = layer2, fill = "grey", alpha = 0.01, lwd = 0.001) # BC polygons

p

# calculate range in protected areas by extracting the number of suitable 
# habitat pixels that are intersecting with the extent of protected areas

# crop raster layer to where Binary = 1 before intersection!
# suitable_area_projected <- projectRaster(suitable_area, crs = crs_string)
suitable_area[ suitable_area[] < 1 ] <- NA

protected_area_intersection <- 
  raster::extract(
    as_Spatial(suitable_area), layer2, 
    # buffer=1000000000, 
    df = TRUE)
protected_area_intersection <- protected_area_intersection %>%
  filter(Binary == 1)
protected_area_intersection_length <- nrow(protected_area_intersection)


# finally, divide number of intersecting pixels by total range size pixels
# to get a score of the proportion of the range conserved in situ
prop <- protected_area_intersection_length / range_size

# and somehow return to a list that can be used as an output table 
# col1 = species name, col2 = range size, col3 = overlap area, col4 = proportion


for(i in 1:nrow(species_list)){ # 
  sp_name <- species_list[i,1]
  df <- raster(paste0("./SSDM_no_crataegus/Species/", sp_name, "/Rasters/Binary.tif"))
  assign(paste0(sub(" ", "_", sp_name), "_range"), df)
  
  
}




# Need to combine the protected area layers as one
layer1 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[2]),
                       crs = crs_string)
layer2 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[5]),
                       crs = crs_string)
layer3 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[8]),
                       crs = crs_string)
layer4 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[13]),
                       crs = crs_string)
layer5 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[16]),
                       crs = crs_string)
layer6 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[19]),
                       crs = crs_string)
layer7 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[22]),
                       crs = crs_string)
layer8 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[24]),
                       crs = crs_string)
layer9 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                               layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[26]),
                       crs = crs_string)
layer10 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[28]),
                        crs = crs_string)
layer11 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[30]),
                        crs = crs_string)
layer12 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[33]),
                        crs = crs_string)
layer13 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[36]),
                        crs = crs_string)
layer14 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[38]),
                        crs = crs_string)
layer15 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[42]),
                        crs = crs_string)
layer16 <- st_transform(st_read("C:/Users/jensj27/Desktop/berry_sdms/doc.kml", 
                                layer = st_layers("C:/Users/jensj27/Desktop/berry_sdms/doc.kml")$name[40]),
                        crs = crs_string)
