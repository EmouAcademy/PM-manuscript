library(ggplot2) 
library(maps) 
library(rnaturalearth)
library(getData)
library(tidyverse)
library(sf)
check_data_exist(scale = 110, category = "cultural", type = "countries")
get_data(
  scale = 110,
  type = c("countries", "map_units", "sovereignty", "tiny_countries")
)
world_map <- map_data("Mongolia") 
ggplot(world_map, aes(long, lat, group = group)) +  
  geom_polygon(fill = "gray90", color = "gray50") +  
  coord_map("mercator") +  
  ggtitle("World Map") +  
  theme_void()



# install.packages("devtools")
# devtools::install_github("ropensci/rnaturalearthhires")
### learn source: https://www.youtube.com/watch?v=NQZNpyEgVss&t=40s

library(rnaturalearth)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)

##### site locations
labs <- data.frame(
  long = c(104.42, 110.12, 111.90, 106.917222),
  lat = c(43.57, 44.87, 43.72, 47.920278),
  site = c("Dalanzadgad", "Sainshand", "Zamyn Uud", "Ulaanbaatar"),
  stringsAsFactors = FALSE
)  



library(geodata)
pop <- population(2020, 2.5, path=tempdir(), country = "Mongolia")
plot(pop)



geo_aimag <- data.frame(
  long = c(101.0316283,99.5146799,89.8549358,103.3965591,106.5525161,106.2901384,115.2177155,
           109.7783753,95.9325806,108.5173101,110.4201043,92.2959651,99.9170492,106.2611562,
           104.1133864,104.2998863,107.0629034,113.5077196,92.9558186,102.7237769, 96.4479958
  ),
  lat = c(47.8626459999702,45.2380937004438,48.5470082998046,48.9521437997042,47.4692538000605,
          49.3767290995996,47.6772781000133,44.4179637005133,45.3463139004315,46.4610827002652,
          47.8859809999647,46.8997977001816,50.2206195994032,45.5186722004105,43.2823630005413,
          49.0243743996863,49.3910135995962,46.2093417003088,49.6313886995382,45.8297039003681,
          48.2934283998669
  ),
  aimag_name = c("Arkhangai","Bayankhongor","Bayan-Ulgii","Bulgan","Tuv","Darkhan-Uul","Dornod",
                 "Dornogobi","Govi-Altai","Govisümber","Khentii","Khovd", "Khövsgöl","Dundgobi",
                 "Ömnögovi","Orkhon","Selenge","Sükhbaatar","Uvs","Uvurkhangai","Zavkhan"),
  stringsAsFactors = FALSE
)  

### DZ 43.57° N, 104.42° E
### SSnd 44.87° N, 110.12° E
### ZU  43.72° N, 111.90° E

##point_SITES_crs <- st_crs(spatial_data)
##point_SITES_crs$proj4string
### sites_location <-  sf_project("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
###                              sites_location) 


##### color palette #######################################################
# for land cover
intuitive_cols <- c(
  "darkgreen",
  "darkolivegreen4",
  "goldenrod2",
  "seagreen",
  "wheat",
  "slategrey",
  "white",
  "lightskyblue1"
)

power_cols <- c("#edf8fb",
  "#b3cde3",
  "#8c96c6",
  "#8856a7",
  "#810f7c")
# for wind speed and yellow dust
power_cols1 <- c("#edf8fb",
                "#b3cde3",
                "#24aae2",
                "#1171ba",
                "#096192")

power_cols2 <- c("#edf8fb",
                 "lightblue",
                 "#24aae2",
                 "#1399c6",
                 "#1171ba",
                "#096192"
                          )

library(viridisLite)
vir <- viridis(9)
mag <- magma(9)
red_gray <- brewer.pal(6, "BuPu")
              #### brewer.pal(9, "BuPu") # A sequential palette like
              #### brewer.pal(9, "BrBG") # A diverging palette like
              #### brewer.pal(9, "Set3") # A qualitative palette like
library(classInt)

# Create 5 "pretty" breaks with classIntervals()
classIntervals(values(prop_by_age[["age_18_24"]]), 
               n = 5, style = "pretty")

# Create 5 "quantile" breaks with classIntervals()
classIntervals(values(prop_by_age[["age_18_24"]]), 
               n = 5, style = "quantile")

#################################################################

map1 <- ne_countries(type = "countries", country = "Mongolia",
                     scale = "medium", returnclass = "sf")
map2 <- rnaturalearth::ne_states("Mongolia", returnclass = "sf")
p1 <- ggplot(map1) + geom_sf()
p2 <- ggplot(map2) + geom_sf()
p1 + p2
plot(map2)


##############
library(geodata)
library(tidyr)
library(raster)

## import shape file and plot;
mng_shp <- st_read("~/WORK/Research/Data/geo data/MNG_adm/MNG_adm0.shp")
plot(mng_shp)
## download monthly wind data of 1970-2000 with 1km2 resolution from WorldClima and plot;
d <- worldclim_country(country = "Mongolia", var = "wind",
                       path = tempdir())

terra::plot(mean(d), plg = list(title = "Wind (m/s)"))
plot(d)
summary(d)
# worldclim_global(var, res, path, version="2.1", ...)
# worldclim_country(country, var, path, version="2.1", ...)
# worldclim_tile(var, lon, lat, path, version="2.1", ...)


## check projection;
proj4string(d) ## doesn't work
st_crs(d)$proj4string # does work

## find seasonal wind speed
raster <- mean(c(d$MNG_wc2.1_30s_wind_3,d$MNG_wc2.1_30s_wind_4, d$MNG_wc2.1_30s_wind_5))
plot(raster)

##### crop then mask raster data
crop_raster <- crop(raster, mng_shp)
raster_mask <- mask(crop_raster, mng_shp)
plot(raster_mask)
## convert raster to data frame
rastdf_mask <- as.data.frame(raster_mask, xy=TRUE) %>% drop_na()

### plot our study sites in reference to season of wind
library(cowplot)
p1 <- ggplot() +
  geom_raster(aes(x=x, y=y, fill = mean), data = rastdf_mask) +
  scale_fill_gradientn(colors=power_cols2, 
                       name = "Wind speed (m/s)",
                       guide = guide_legend(
                         direction = "horizontal",
                         keyheight = unit(1.5, units = "mm"), 
                         keywidth = unit(6/length(labels), units = "mm"),
                         title.position = 'top',
                         title.hjust = 0,
                         label.hjust = 0.5,
                         nrow = 1,
                         byrow = TRUE,
                         reverse = FALSE,
                         label.position = "bottom"
                       )) +
  geom_sf(fill = "transparent", data = map2) +
  labs(x = "Longitude", y = "Latitude",
       title = "Locations of study sites",
       subtitle ='Averaged wind speed in the spring (March to May)',
       caption = 'Data source: WorldClim, 2022') +
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = c(2.5,2.5,2.5, 2.5), pch = c(1,24,22,8)) +
  geom_point(data = labs, aes(x = long, y = lat), color = "white", size = 1.5, pch = c(1,24,22, 8)) +
  geom_text(data = labs, aes(long, lat, label = site),
            vjust = c(-1, -1, -.3, -1), hjust = c(1,1,1.1,1)) +
  cowplot::theme_cowplot() +
  theme(legend.position = c(.73, .10),
        legend.title = element_text(size=9), #change legend title font size
        legend.text = element_text(size=7),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linetype = "dashed",
                                        size = .05),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE,
        panel.background = element_rect(fill = NA, colour = 'black', size = 1.5))
p1  
ggsave("~/WORK/Research/Data/03_figures/sites_of_pm10_at_gobi.png", width = 15, height = 10, units = "cm", dpi = 300)
######################### Elevation with POPULATION data
p2 <- ggplot() +
  geom_raster(aes(x=x, y=y, fill = file2ca3d796c65), data = rastdf_elev) +
  scale_fill_gradientn(colors=power_cols, 
                       name = "Elevation, m") +
  geom_sf(fill = "transparent", data = map2) +
  labs(x = "Longitude",
       y = "Latitude",
       title = " ",
       subtitle ='Elevation map with population (Aimag and City)',
       caption = 'Data source: 1212.mn NSO, 2022') +
  geom_point(data = Aimag_tuv_coord, 
             #　col = "transparent",
             alpha=1,
             aes(x=Lon, y= Lat, size = Population)
  ) +
#  scale_colour_gradientn(colours=rainbow(7), breaks=as.vector(bs), 
             #            name = "Population") +
  scale_size(range=c(0,4), breaks=as.vector(bs), 
                                            name = "Population") +
geom_point(data = labs, aes(x = long, y = lat), color = "black", size = c(2.5,2.5,2.5, 2.5), pch = c(1,24,22,8)) +
  geom_point(data = labs, aes(x = long, y = lat), color = "white", size = 1.5, pch = c(1,24,22, 8)) +
  geom_text(data = labs, aes(long, lat, label = site),
            vjust = c(-1, -1, -.3, -.5), hjust = c(1,1,1.1,1.1)) +
  cowplot::theme_cowplot() +
  theme(#legend.position = c(0.65, .70),
        legend.title = element_text(size=9), #change legend title font size
        legend.text = element_text(size=7),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linetype = "dashed",
                                        size = .05),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE,
        panel.background = element_rect(fill = NA, colour = 'black', size = 1.5)) 
  
p2  

ggsave("~/WORK/Research/Data/03_figures/sites_of_pm2.5_emission_population.png", width = 15, height = 10, units = "cm", dpi = 300)

bs <- c(1596335, 29645, 28097, 18585)



library(gridExtra)
grid.arrange(p1, p2, nrow = 1)
p3 <- cowplot::plot_grid(p1, p2,ncol = 2, 
                         rel_widths = c(.85, 1.0))
p3
ggsave("~/WORK/Research/Data/03_figures/sites_of_NIES.png", width = 30, height = 10, units = "cm", dpi = 300)


################ elevation data
library(rnaturalearth)
library(elevatr)
library(terra)
map <- ne_countries(type = "countries", country = "Mongolia",
                    scale = "medium", returnclass = "sf")
df_elev <- get_elev_raster(locations = map, z = 9, clip = "locations")
p4 <- terra::plot(rast(df_elev), plg = list(title = "Elevation (m)"))
## convert raster to data frame
rastdf_elev <- as.data.frame(df_elev, xy=TRUE) %>% drop_na()



library(ggmap)

grid.arrange(p1,p2+theme(legend.position='hidden'))
grid_arrange_shared_legend(p1, p2, ncol = 2, nrow = 1, position='top')
