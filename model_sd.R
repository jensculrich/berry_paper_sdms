### Species Distribution Models

library(raster)
library(sf)
library(maptools)
library(dismo)
library(rJava)
library(tidyverse)

occurrence_df <- read.csv("./occurrence_data/occurrence_data_cleaned.csv")

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

# can test that everything looks right
occurrence_df_test <- occurrence_df %>%
  filter(SPECIES == "Rubus canadensis")
# plot(bioclim1)
# points(occurrence_df_test$longitude, 
#        occurrence_df_test$latitude, 
#        col='black', pch=20, cex=0.75)

canada <- st_read("./Geo_Data/canada.geojson", quiet = TRUE) # 1
biome_sf <- st_read("./geo_data/canada_ecoregions_clipped.geojson", quiet = TRUE) # 1

str(biome_sf)
# Rasterize biome field and write to disk
biome <- raster(crs = "+proj=longlat +datum=WGS84", vals = 0, resolution = 0.08333333, 
                ext = extent(-141, -52.66667, 41.66667, 83.25))  %>%
    rasterize(biome_sf, .) 
biome2 <- raster(ext=extent(41.66667, 83.25, -141, -52.66667), res=c(0.01, 0.01))

xres(bioclim1)
xres(biome)
extent(bioclim1)
extent(biome)

predictors <- stack(bioclim1, bioclim2, bioclim3, bioclim4, bioclim5,
                    bioclim6, bioclim7, bioclim8, bioclim9, bioclim10,
                    bioclim11, bioclim12, bioclim13, bioclim14, bioclim15,
                    bioclim16, bioclim17, bioclim18, bioclim19,
                    biome)
  
names(predictors)
plot(biome)
plot(bioclim1)

# first layer of the RasterStack
#plot(predictors, 1)
# note the "add=TRUE" argument with plot
#data(wrld_simpl)
#plot(wrld_simpl, add=TRUE)
# with the points function, "add" is implicit
#points(occurrence_df_test$longitude, 
#       occurrence_df_test$latitude, 
#       col='black', pch=20, cex=0.75)

occurrence_df_test <- occurrence_df_test[,11:12]
group <- kfold(occurrence_df_test, 5)

# "layer" is the name of the biomes
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train, factor="layer")
  plot(xm)
  } else {
    cat('cannot run this example because maxent is not available')
    plot(1)
  }

if (file.exists(jar)) {
  e <- evaluate(pres_test, backg_test, xm, predictors)
  e
  px = predict(predictors, xm, ext=ext, progress='')
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  tr <- threshold(e, 'spec_sens')
  plot(px > tr, main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
 } else {
    plot(1)
}