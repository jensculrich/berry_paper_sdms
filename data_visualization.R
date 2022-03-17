### Berry Conservation - Species Distribution Models
### Species Distribution and Protected Areas Visualization
## Jens Ulrich March 2022

library(raster)
library(sf)
library(tidyverse)
library(cowplot)

### plot the diversity map
ssdm_diversity <-  raster("./Stack/Stack/Rasters/Diversity.tif")
canada <- st_read("./geo_data/canada_provinces.geojson")

# test_df <- as.data.frame(test, xy = TRUE)
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

ssdm_diversity_projected <- projectRaster(ssdm_diversity, crs = crs_string)
canada2 <- st_transform(canada, crs = crs_string)

ssdm_diversity_crop <- crop(ssdm_diversity_projected, canada2)
ssdm_diversity_mask <- mask(ssdm_diversity_crop, canada2)

ssdm_diversity_df <- as.data.frame(ssdm_diversity_mask, xy = TRUE)

# make a crop to zoom in on the SE berry rich region
ssdm_diversity_mask_se <- ssdm_diversity_mask
e <- as(extent(500000, 2400000, 5830000, 7000000), 'SpatialPolygons')
crs(e) <- crs(ssdm_diversity_mask_se)
ssdm_diversity_mask_se_cropped <- crop(ssdm_diversity_mask_se, e)
ssdm_diversity_df_se <- as.data.frame(ssdm_diversity_mask_se_cropped, xy = TRUE)

# make a crop to zoom in on the SW berry rich region
ssdm_diversity_mask_sw <- ssdm_diversity_mask
e2 <- as(extent(-2600000, -1600000, 6800000, 7970000), 'SpatialPolygons')
crs(e2) <- crs(ssdm_diversity_mask_sw)
ssdm_diversity_mask_sw_cropped <- crop(ssdm_diversity_mask_sw, e2)
ssdm_diversity_df_sw <- as.data.frame(ssdm_diversity_mask_sw_cropped, xy = TRUE)


# full map
p <- ggplot(ssdm_diversity_df) +
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
                     legend.position = c(0.95, 0.7))
p

# need to unzip the kmz file
# st_layers("C:/Users/jensj/Desktop/Berry_sdms/doc.kml")
# want layers: 2, 5, 8, 13, 16, 19, 22, 24, 26, 28, 30, 33, 36, 38, 42
# index <- c(2, 5, 8, 13, 16, 19, 22, 24, 26, 28, 30, 33, 36, 38, 40, 42)

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

# s <- p + 
#   geom_sf(data = layer4, fill = "grey", alpha = 0.1, lwd = 0.01)
# s

r <- p + 
  geom_sf(data = layer1, fill = "grey", alpha = 0.01, lwd = 0.001) + # AB polygons
  geom_sf(data = layer2, fill = "grey", alpha = 0.01, lwd = 0.001) + # BC polygons
  geom_sf(data = layer3, fill = "grey", alpha = 0.01, lwd = 0.001) + # bird sanctuaries
  geom_sf(data = layer4, fill = "grey", alpha = 0.01, lwd = 0.001) + # national parks
  geom_sf(data = layer5, fill = "grey", alpha = 0.01, lwd = 0.001) + # MB polygons
  geom_sf(data = layer6, fill = "grey", alpha = 0.01, lwd = 0.001) + # NB polygons
  geom_sf(data = layer7, fill = "grey", alpha = 0.01, lwd = 0.001) + # NL polygons
  geom_sf(data = layer8, fill = "grey", alpha = 0.01, lwd = 0.001) + # NT polygons
  geom_sf(data = layer9, fill = "grey", alpha = 0.01, lwd = 0.001) + # NS polyons
  geom_sf(data = layer10, fill = "grey", alpha = 0.01, lwd = 0.001) + # NU polygons
  geom_sf(data = layer11, fill = "grey", alpha = 0.01, lwd = 0.001) + # ON polygons
  geom_sf(data = layer12, fill = "grey", alpha = 0.01, lwd = 0.001) + # PEI polygons
  geom_sf(data = layer13, fill = "grey", alpha = 0.01, lwd = 0.001) + # QB polygons
  geom_sf(data = layer14, fill = "grey", alpha = 0.01, lwd = 0.001) + # SK polygons
  geom_sf(data = layer15, fill = "grey", alpha = 0.01, lwd = 0.001) + # really not sure what this is. around Ottawa
  geom_sf(data = layer16, fill = "grey", alpha = 0.01, lwd = 0.001) # YT polygons
r

# southeast inset
w <- r +xlim(500000, 2400000) +
  ylim(5830000, 7000000) 

w

  # southwest inset
z <- r + 
  xlim(-2600000, -1600000) +
  ylim(6800000, 7970000) 

z

bottom_row <- plot_grid(z, w, labels = c('B', 'C'), label_size = 12, rel_widths = c(.6, 1))
plot_grid(r, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1, rel_heights = c(2, 1.1))

