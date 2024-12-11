library(ggplot2) 
library(maps) 
library(rnaturalearth)
library(getData)
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
  long = c(104.42, 110.12, 111.90),
  lat = c(43.57, 44.87, 43.72),
  site = c("Dalanzadgad", "Sainshand", "Zamyn Uud"),
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
library(viridisLite)
vir <- viridis(9)
mag <- magma(9)
red_gray <- brewer.pal(6, "BuPu")
              #### brewer.pal(9, "BuPu") # A sequential palette like
              #### brewer.pal(9, "BrBG") # A diverging palette like
              #### brewer.pal(9, "Set3") # A qualitative palette like
library(classInt)

# Create 5 "pretty" breaks with classIntervals()
intervals_population = c("1000", "2500", "5000")
intervals <- classIntervals(values(prop_by_age[["intervals_population"]]), 
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


plot(mn_hex$population)

population {geodata}
pop <- population(2020, 10, path=tempdir())

df_mn_hex <- as.data.frame(mn_hex, xy=TRUE) %>% drop_na()
pop
summary(mn_hex)

mn_hex <- project(mn_hex, crs(v_00))
template = rast(vect(mn_hex),res=10)
poly_raster <- rasterize(vect(mn_hex), template) # library(terra)
plot(poly_raster,col="red")

ggplot(mn_hex) +
  geom_sf(aes(fill = population),
  ) +
  geom_sf(
    data = mn_boundary,
    fill = NA,
    color = "black",
    linetype = "dashed",
    linewidth = 1
  )  +
scale_fill_stepsn(n.breaks = 12, colours = terrain.colors(12))

ggplot(mn_hex) +
  geom_sf(aes(fill = population),
 ) +
  geom_sf(
    data = mn_boundary,
    fill = NA,
    color = "black",
    linetype = "dashed",
    linewidth = 1
  )  +
  scale_fill_gradientn(colors=mag, 
                       name = "Population (m/s)",
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
                       ))



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
p3 <- ggplot() +
  geom_raster(aes(x=x, y=y, fill = mean), data = rastdf_mask) +
  scale_fill_gradientn(colors=power_cols, 
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
       caption = 'Source: WorldClim, 2022') +
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = c(2.5,2.5,2.5), pch = c(1,24,22)) +
  geom_point(data = labs, aes(x = long, y = lat), color = "white", size = 1.5, pch = c(1,24,22)) +
  geom_text(data = labs, aes(long, lat, label = site),
            vjust = c(-1, -1, -.3), hjust = c(1,1,1.1)) +
  cowplot::theme_cowplot() +
  theme(legend.position = c(.75, .10),
        legend.title = element_text(size=9), #change legend title font size
        legend.text = element_text(size=7),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linetype = "dashed",
                                        size = .05),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE,
        panel.background = element_rect(fill = NA, colour = 'black', size = 1.5))
p3  
ggsave("~/WORK/Research/Data/03_figures/sites_of_pm10_at_gobi.png", width = 15, height = 10, units = "cm", dpi = 300)



################ elevation data
library(rnaturalearth)
library(elevatr)
library(terra)
map <- ne_countries(type = "countries", country = "Mongolia",
                    scale = "medium", returnclass = "sf")
d <- get_elev_raster(locations = map, z = 9, clip = "locations")
p4 <- terra::plot(rast(d), plg = list(title = "Elevation (m)"))

library(gridExtra)
grid.arrange(p2, p3, nrow = 2)

library(ggmap)



