#install.packages("sf", dependencies=TRUE)
#install.packages("tmap", dependencies=TRUE)
#install.packages("mapview", dependencies=TRUE)
#install.packages("stars", dependencies=TRUE)
#install.packages("rayshader", dependencies=TRUE)
#install.packages("MetBrewer", dependencies=TRUE)
#install.packages("rayrender")
#install.packages("extrafont", dependencies=TRUE)
#install.packages("magick", dependencies=TRUE)

### data site: https://data.humdata.org/dataset/kontur-boundaries-mongolia
###　program source https://medium.com/@niloy.swe/how-to-create-a-3d-population-density-map-in-r-33dfaf7a71d7

options(rgl.useNULL = FALSE)
options(rgl.useNULL = TRUE)
library(rgl)

require(tidyverse)
require(sf)
require(tmap)
require(ggplot2)
require(mapview)
require(stars)
require(rayshader)
require(MetBrewer)
require(colorspace)
require(rayrender)
require(magick)
require(extrafont)


library(stars)
library(tidyverse)
library(raster)
library(osmdata)
library(scico)

mn_hex <-
  st_read("~/WORK/Research/Data/geo data/kontur_population_MN_20231101.gpkg") %>%
  st_transform(3106)
plot(mn_hex)

mn_admin <-
  st_read("~/WORK/Research/Data/geo data/kontur_boundaries_MN_20230628.gpkg") %>%
  st_transform(3106)

distinct_names <- mn_admin %>%
  distinct(name_en)
print(distinct_names)

library(ggnewscale)
alt <- getData('alt', country='Mongolia')
alt_stars = st_as_stars(alt) 

##### crop then mask raster data
crop_alt <- crop(alt, mng_shp)
raster_mask_alt <- mask(crop_alt, mng_shp)
plot(raster_mask_alt)
## convert raster to data frame
rastdf_mask_alt <- as.data.frame(raster_mask_alt, xy=TRUE) %>% drop_na()

pop <- getData('pop', country = 'Mongolia' )

ch_canvas = st_bbox(alt_stars) %>% opq()

# overpass query
ch_moto = ch_canvas %>%
  osmdata_sf() 

  add_osm_feature(key = "highway",
                  value="motorway") %>% 
  osmdata_sf() 



Aimag_tuv_coord <- read.csv("~/WORK/Research/Data/geo data/Aimag_tuv_coord.csv")
  

# make the polygons (using a smaller buffer)
df_buffer = st_buffer(df, 0.01)
df_p <- st_as_sf(df_buffer)
ggplot(df_p, aes(population)) +
  sp.points()
############################# Not bad
ggplot() +
  geom_stars(
    data = alt_stars,
    aes(x = x, y = y, fill = MNG_msk_alt),
    downsample = 2
  ) +
  scale_fill_continuous(low = "white", high = "black") +
  new_scale_fill() + ## geoms added after this will use a new scale definition
  geom_sf(
    data = df_buffer, 
    col = "transparent",
    aes(fill = population)
  ) +
  scale_colour_manual(values = cols)
###########################
ggplot() +
  geom_stars(
    data = alt_stars,
    aes(x = x, y = y, fill = MNG_msk_alt),
    downsample = 2
  ) +
  scale_fill_continuous(low = "white", high = "black") +
 # new_scale_fill() + ## geoms added after this will use a new scale definition
  geom_point(
    data = Aimag_tuv_coord, 
    #　col = "transparent",
    alpha=1,
    aes(x=Lon, y= Lat, size = Population, color=Population)
  ) +
  scale_colour_gradientn(colours=rainbow(7), breaks=as.vector(bs)) +
  scale_size(range=c(0,4), breaks=as.vector(bs),guide = F)

bs <- c(137969, 400000, 800000, 1200000, 1596335)

size <- log(Aimag_tuv_coord$Population)

 ggplot() +
  sp_point(aes(x=Lat, y=Lon, size = Population),
    data = Aimag_tuv_coord
  ) +
  scale_colour_manual(values = cols)

library(geodata)
library(terra)

f <- system.file(pop, package="terra")
v <- vect(f)
as.data.frame(v)

test <- pop[[1]] 
# Convert data frame to vector object
points <- vect(coords,
               geom=c("x", "y"),
               crs = "EPSG:4326")
# Extract values
values <- extract(test,
                  points)
raster::extract(pop, coords)
library(raster)

ggplot() +
  geom_raster(aes(x=x, y=y, size = population_density),
              data = pop_rastdf_mask
  ) +
  scale_colour_manual(values = cols)

pop_raster <- crop(pop, mng_shp)
pop_raster_mask <- mask(pop_raster, mng_shp)

pop_rastdf_mask <- as.data.frame(pop_raster_mask, xy=TRUE) %>% drop_na()

geom_raster(aes(x=x, y=y, fill = mean), data = rastdf_mask) 
ggplot(data = pop) +
  geom_sf(
    aes(fill = population)
  ) +
  scale_color_manual(values = cols)


df = ch_moto %>% pluck("osm_lines") %>% 
  dplyr::select("maxspeed") %>% 
  filter(!is.na(maxspeed),
         maxspeed != "none") %>% 
  mutate(maxspeed = as.numeric(maxspeed))

cols <- c("600000" = "red", "300000" = "blue", "100000" = "darkgreen", "50000" = "orange")
p + scale_colour_manual(values = cols)

plot(alt)


df <- mn_admin %>%  
  filter(admin_level == 4) 

geo_aimag <- c()


# Creating BD Boundary
mn_boundary <-
  mn_admin %>%
  # filter(name_en == 'Dhaka Division') %>% # Filtering Dhaka Only
  st_geometry %>%
  st_union %>%
  st_sf %>%
  st_make_valid()

# Only keep the part below if you want to plot the Dhaka Division only and plot only the dhaka_hex instead of bd_hex
# Otherwise comment it.

mn_boundary %>% 
  ggplot()+
  geom_sf()

dhaka_hex <- st_intersection(mn_hex, mn_boundary) %>% 
  st_transform(crs = 3106)


# check the boundary plot
ggplot(mn_hex) +
  geom_sf(aes(fill = population),
          color = "gray66",
          linewidth = 0) +
  geom_sf(
    data = mn_boundary,
    fill = NA,
    color = "black",
    linetype = "dashed",
    linewidth = 1
  )

###############
# setting the bd boundary as a bounding box
bbox <- st_bbox(mn_boundary)

# finding the aspect ratio
bottom_left <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) %>%
  st_sfc(crs = 3106)
bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) %>%
  st_sfc(crs = 3106)
top_left <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>%
  st_sfc(crs = 3106)
top_right <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>%
  st_sfc(crs = 3106)



width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

if(width > height) {
  w_ratio = 1
  h_ratio = height / width
  
} else {
  h_ratio = 1.1
  w_ratio = width / height
}



# convert to raster to convert to matrix
# size = 100
size = 1000 * 3.5

pop_raster <- st_rasterize(
  mn_hex,
  nx = floor(size * w_ratio) %>% as.numeric(),
  ny = floor(size * h_ratio) %>% as.numeric()
)

pop_matrix <- matrix(pop_raster$population,
                     nrow = floor(size * w_ratio),
                     ncol = floor(size * h_ratio))



subset_colors <- MetBrewer::met.brewer(name="Benedictus", direction = -1)

tx <- grDevices::colorRampPalette(subset_colors, bias = 4.5)(256)
swatchplot(tx)
swatchplot(subset_colors)


# Close any existing 3D plot before plotting another
rgl::close3d()

pop_matrix %>%
  height_shade(texture = tx) %>%
  plot_3d(heightmap = pop_matrix,
          zscale = 250 / 4.5,
          solid = F,
          shadowdepth = 0)

# Adjusting Camera Angle
render_camera(theta = 0,
              phi = 70,
              zoom = 0.55,
              fov = 100
)

# To interactively view the 3D plot
rgl::rglwidget()
library("plot3D")
