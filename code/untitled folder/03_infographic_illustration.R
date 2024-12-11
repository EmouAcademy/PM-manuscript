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

mn_hex <-
  st_read("~/WORK/Research/Data/geo data/kontur_population_MN_20231101.gpkg") %>%
  st_transform(3106)

mn_admin <-
  st_read("~/WORK/Research/Data/geo data/kontur_boundaries_MN_20230628.gpkg") %>%
  st_transform(3106)

distinct_names <- mn_admin %>%
  distinct(name_en)
print(distinct_names)


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


############# Step 9

# Close any existing 3D plot before plotting another
rgl::close3d()

pop_matrix %>%
  height_shade(texture = tx) %>%
  plot_3d(heightmap = pop_matrix,
          zscale = 250 / 4.5,
          solid = F,
          shadowdepth = 0) 
  image_write("~/WORK/Research/Data/03_figures/sitectus_40.png")

# Adjusting Camera Angle
render_camera(theta = 0,
              phi = 70,
              zoom = 0.55,
              fov = 100
)

# To interactively view the 3D plot
rgl::rglwidget()

############# Step 10

outfile <- glue::glue("~/WORK/Research/Data/03_figures/sitectus_4.png")

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    filename = outfile,
    interactive = F,
    lightdirection = 55, #Degree
    lightaltitude = c(30, 80),
    lightcolor = c("white", "white"),  # Set both lights to white
    lightintensity = c(600, 100),
    width = 1400,
    height = 1580,
    samples = 50,
    sample_method = "sobol"
    #samples = 2
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


############## Step 11


pop_raster <- image_read("~/WORK/Research/Data/03_figures/sitectus_4.png")

text_color <- darken(subset_colors[3], .4)
swatchplot(text_color)

library(showtext)
# Automatically enable font support
showtext_auto()

# Download and register the Philosopher font from Google Fonts
font_add_google("Philosopher", regular = "400", bold = "700")

pop_raster %>%
  image_annotate("Mongolia",
                 gravity = "northeast",
                 location = "+50+50",
                 color = text_color,
                 size = 120,
                 weight = 700,
                 # degrees = 0,
  ) %>%
  image_annotate("POPULATION DENSITY MAP",
                 gravity = "northeast",
                 location = "+50+175",
                 color = text_color,
                 size = 28.5,
                 weight = 500,
                 # degrees = 0,
  ) %>%
  image_annotate("Visualization by: Niloy Biswas with Rayshader\nData: Kontur Population 2023",
                 gravity = "southwest",
                 location = "+20+20",
                 color = alpha(text_color, .8),
                 font = "Philosopher",  # Corrected font name
                 size = 25,
                 # degrees = 0,
  ) %>%
  image_write("~/WORK/Research/Data/03_figures/Annotated_plot_bd_Benedictus_3.png", format = "png", quality = 100)

library("plot3D")
