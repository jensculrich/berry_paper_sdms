### Species Distribution Models

library(raster)
library(sf)
# library(maptools)
# library(dismo)
# library(rJava)
library(tidyverse)
# data(wrld_simpl)
library(SSDM)

occurrence_df <- read.csv("./occurrence_data/occurrence_data_cleaned_thinned.csv")

bioclim1 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_1.tif", quiet = TRUE) # 1
bioclim2 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_2.tif", quiet = TRUE) # 1
bioclim3 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_3.tif", quiet = TRUE) # 1
bioclim4 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_4.tif", quiet = TRUE) # 1
bioclim5 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_5.tif", quiet = TRUE) # 1
bioclim6 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_6.tif", quiet = TRUE) # 1
bioclim7 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_7.tif", quiet = TRUE) # 1
bioclim8 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_8.tif", quiet = TRUE) # 1
bioclim9 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_9.tif", quiet = TRUE) # 1
bioclim10 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_10.tif", quiet = TRUE) # 1
bioclim11 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_11.tif", quiet = TRUE) # 1
bioclim12 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_12.tif", quiet = TRUE) # 1
bioclim13 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_13.tif", quiet = TRUE) # 1
bioclim14 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_14.tif", quiet = TRUE) # 1
bioclim15 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_15.tif", quiet = TRUE) # 1
bioclim16 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_16.tif", quiet = TRUE) # 1
bioclim17 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_17.tif", quiet = TRUE) # 1
bioclim18 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_18.tif", quiet = TRUE) # 1
bioclim19 <- raster("./wc2.1_5m_bio_masked/wc2.1_5m_bio_19.tif", quiet = TRUE) # 1
biome <- raster("./wc2.1_5m_bio_masked/biome.tif", quiet = TRUE)

# can test that everything looks right
occurrence_df_test <- occurrence_df %>%
  filter(SPECIES == "Malus fusca")

# plot(bioclim1)
# points(occurrence_df_test$longitude, 
#        occurrence_df_test$latitude, 
#        col='black', pch=20, cex=0.75)

# canada <- st_read("./Geo_Data/canada.geojson", quiet = TRUE) # 1

predictors <- stack(bioclim1, bioclim2, bioclim3, bioclim4, bioclim5,
                    bioclim6, bioclim7, bioclim8, bioclim9, bioclim10,
                    bioclim11, bioclim12, bioclim13, bioclim14, bioclim15,
                    bioclim16, bioclim17, bioclim18, bioclim19,
                    biome)

# names(predictors)
# plot(biome)
# plot(bioclim1)

# first layer of the RasterStack
#plot(predictors, 1)
# note the "add=TRUE" argument with plot
#data(wrld_simpl)
#plot(wrld_simpl, add=TRUE)
# with the points function, "add" is implicit
#points(occurrence_df_test$longitude, 
#       occurrence_df_test$latitude, 
#       col='black', pch=20, cex=0.75)

# ext <- extent(bioclim1)


### SDM for single species uding dismo package

# occurrence_df_test <- occurrence_df_test[,2:3]
# group <- kfold(occurrence_df_test, 5)

# pres_train <- occurrence_df_test[group != 1, ]
# pres_test <- occurrence_df_test[group == 1, ]

# backg <- randomPoints(predictors, n=1000, ext=ext, extf = 1.25)
# colnames(backg) = c('lon', 'lat')
# group <- kfold(backg, 5)
# backg_train <- backg[group != 1, ]
# backg_test <- backg[group == 1, ]

# evaluate importance of predictors for the training points
# xm <- maxent(predictors, pres_train, factor="biome")
# use  RasterStack with predictor variables to make a prediction to RasterLayer:
# pb <- predict(predictors, xm, ext=ext, progress='')

# evaluate the model in a similar way, by providing presence and 
# background (absence) points, the model, and a RasterStack:
# e <- dismo::evaluate(pres_test, backg_test, xm, predictors)

# Find a threshold
# tr <- dismo::threshold(e, 'spec_sens')
# tr

# plot raw values and presence/absence from threshold
# par(mfrow=c(1,2), mai = c(1, 0.5, 0.5, 1))
# plot(pb, main='Bioclim, raw values')
# plot(pb > tr, main='presence/absence')
# points(pres_train, pch='+')

###############################################
### SDM species stacking using 'SSDM' package #
###############################################

rm(bioclim1, bioclim2, bioclim3, bioclim4, bioclim5, bioclim6, 
   bioclim7, bioclim8, bioclim9, bioclim10, bioclim11, bioclim12,
   bioclim13, bioclim14, bioclim15, bioclim16,
   bioclim17, bioclim18, bioclim19, biome)

predictors <- predictors
occurrence_df_ssdm <- occurrence_df %>%
  dplyr::select(SPECIES, latitude, longitude) %>%
  rename("LONGITUDE" = "longitude",
         "LATITUDE" = "latitude") %>%
  dplyr::relocate(SPECIES, LONGITUDE, LATITUDE)

rm(occurrence_df)

# can test how many species with data (one or more lat/long)
# this shows 202 - there are 206 in the berry list
# species <- occurrence_df_ssdm %>% 
#   distinct(SPECIES, .keep_all = TRUE)

#occurrence_df_ssdm_test <- occurrence_df_ssdm %>%
#  filter(SPECIES == "Malus fusca" | SPECIES == "Rubus canadensis" |
#           SPECIES == "Amelanchier alnifolia" | SPECIES == "Fragaria vesca")

SSDM <- stack_modelling("MAXENT", occurrence_df_ssdm, 
                        predictors, rep = 1, ensemble.thresh = 0,
                        Xcol = 'LONGITUDE', Ycol = 'LATITUDE',
                        Spcol = 'SPECIES', method = "pSSDM", verbose = FALSE)

save.stack(SSDM, name = "Stack", path = getwd(), verbose = TRUE, GUI = FALSE)


### plot the diversity map

par(mfrow=c(1,1), mai = c(1, .5, 1, .5))
e <- extent(SSDM@diversity.map)
# plot(SSDM@diversity.map, 
  #   add = TRUE, ext = e)

test <-  raster("./Stack/Stack/Rasters/Diversity.tif")
test_df <- as.data.frame(test, xy = TRUE)

crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2
test2 <- projectRaster(test, crs = crs_string)
test2_df <- as.data.frame(test2, xy = TRUE)

p <- ggplot(test2_df) +
  geom_tile(aes(x = x, y = y,
                  fill = Diversity)) +
  scale_fill_gradientn(name = "Berry \nSpecies Richness",
                       colours = rev(terrain.colors(10)),
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
                     legend.position = c(0.85, 0.7))
p

## can map endemism too, but very few areas with any endemic species
test3 <-  raster("./Stack/Stack/Rasters/Endemism.tif")
# test_df <- as.data.frame(test3, xy = TRUE)

test4 <- projectRaster(test3, crs = crs_string)
test4_df <- as.data.frame(test4, xy = TRUE)

q <- ggplot(test4_df) +
  geom_tile(aes(x = x, y = y,
                fill = Endemism)) +
  scale_fill_gradientn(name = "Endemic \nBerry Species",
                       colours = rev(terrain.colors(10)),
                       na.value = "white")

q <- q +
  theme_bw() + theme(axis.line=element_blank(),
                     panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.text.x=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     legend.position = c(0.85, 0.7))
q
