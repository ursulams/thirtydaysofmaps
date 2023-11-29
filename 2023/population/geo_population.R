library(here)
library(tidyverse)
library(sf)
library(stars)
library(rayshader)
library(magick)

# 3D map of population density of the country of Georgia

# population and administrative info
geo <- st_read(here("2023/kontur_population_GE_20220630.gpkg"))  

# transform lat/lon-based bounding box to raster then to matrix for rayshader
# get E-W distance
bounding <- st_bbox(geo) # x-min, x-max, y-min, y-max to calc width/height ratio

bottom_e <- st_point(c(bounding[["xmax"]], bounding[["ymin"]])) %>% 
  st_sfc(crs = st_crs(geo))

bottom_w <- st_point(c(bounding[["xmin"]], bounding[["ymin"]])) %>% 
  st_sfc(crs = st_crs(geo))

top_w <- st_point(c(bounding[["xmin"]], bounding[["ymax"]])) %>% 
  st_sfc(crs = st_crs(geo))

ew_ratio <- 1
ns_ratio <- st_distance(top_w, bottom_w)/st_distance(bottom_e, bottom_w)

multi_size = 950
rast <- st_rasterize(geo, nx = floor(multi_size), ny = floor(multi_size*ns_ratio))

rayshade_matrix <- matrix(rast$population, nrow = floor(multi_size*ew_ratio), ncol = floor(multi_size*ns_ratio))

rayshade_matrix %>% 
  height_shade(texture = grDevices::colorRampPalette(c("#b4cfa4", "#c1e7d6", "#cdebde", "#d3f1f4"), bias = 4)(256)) %>% 
  plot_3d(heightmap = rayshade_matrix,
        zscale = 100,
        solid = FALSE,
        shadowcolor = "#2b2016",
        background = "#403121",
        windowsize = c(900*ew_ratio, 950*ns_ratio),) 

render_camera(theta = 5, phi = 50, zoom = 0.5)

# render png image
outfile <- "2023/images/geo_pop_density.png"

{
if (!file.exists(outfile)){
  png::writePNG(matrix(1), target = outfile)
}
# to lower the resolution and speed up rendering, reduce raster & matrix multiplier from 5000
render_highquality(outfile,
  interactive = FALSE,
  lightdirection = 245,
  lightaltitude = 30,
  lightintensity = 400,
  lightcolor = "white",
  samples = 10000) # higher number increases size, may induce hang in rendering
}

img <- image_read(outfile)

img_ <- image_annotate(img, "Population Density, Georgia",
                       gravity = "north",
                       location = "+5+10",
                       color = "#b7e8ed",
                       size = 24,
                       font = "Avenir")

img_ <- image_annotate(img_, "400 m H3 Hexagons",
                       gravity = "north",
                       location = "+5+40",
                       color = "#b7e8ed",
                       size = 18,
                       font = "Avenir")

img <- image_annotate(img_, "source: https://data.humdata.org",
                      gravity = "northeast",
                      location = "+5+450",
                      color = "#b7e8ed",
                      size = 16,
                      font = "Avenir")


image_write(img, here("2023/images/geo_pop_density.png"), format = "png")
