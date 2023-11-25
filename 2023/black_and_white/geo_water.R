library(here)
library(tidyverse)
library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(magick)

# georgian waterways data 
# download.file(url = "https://biogeo.ucdavis.edu/data/diva/wat/GEO_wat.zip",
#               destfile = here("2023/black_and_white/water.zip"))
# unzip(zipfile = here("2023/black_and_white/water.zip"), exdir = here("2023/black_and_white/"))

# rivers/streams & inland bodies
geo_waterline <- st_read(here("2023/black_and_white/GEO_water_lines_dcw.shp")) %>% 
  subset(select = "geometry")

geo_waterarea <- st_read(here("2023/black_and_white/GEO_water_areas_dcw.shp")) %>% 
  subset(select = "geometry")

geo_borders <- st_read(here("2023/kontur_boundaries_GE_20230628.gpkg"))

elev <- get_elev_raster(geo_borders, z = 9)
elev_mask <- mask(elev, geo_borders)
geo_mat <- raster_to_matrix(elev_mask)

# as spencer schien utilizes here: https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
# dynaimcally set window height and width based on object size
w <- nrow(geo_mat)
h <- ncol(geo_mat)

# scale the dimensions to use as multipliers
wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

# limit ratio so that the shorter side is at least .7 of longer side
if (min(c(wr, hr)) < .70) {
  if (wr < .70) {
    wr <- .70
  } else {
    hr <- .70
  }
}

geo_mat %>% 
height_shade(texture = colorRampPalette(c("#12101f", "#454252", "#8b8993"), bias = 2)(256)) %>%
  add_overlay(generate_line_overlay(geo_waterline,
    extent = geo_borders,
    heightmap = geo_mat,
    color = "white",
    linewidth = 5)) %>%
  add_overlay(generate_polygon_overlay(geo_waterarea,
    extent = geo_borders,
    heightmap = geo_mat,
    linecolor = "white",
    palette = "white")) %>% 
  plot_3d(heightmap =  geo_mat,
        zscale = 10,
        solid = FALSE,
        shadow = FALSE,
        background = "white",
        windowsize = c(900*wr, 900*hr))
render_camera(theta = 0, phi = 80, zoom = 0.7)

outfile <- here("2023/images/geo_water.png")

{
  if (!file.exists(outfile)){
    png::writePNG(matrix(1), target = outfile)
  }
  # to lower the resolution and speed up rendering, reduce raster & matrix multiplier from 5000
  render_highquality(outfile,
                     width = 900*wr,
                     height = 890*hr,
                     interactive = FALSE,
                     lightaltitude = 80,
                     lightintensity = 600,
                     samples = 5000) # higher number increases size, may induce hang in rendering
}

img <- image_read(outfile)

img_ <- image_annotate(img, "Bodies of Water",
                 gravity = "north",
                 location = "+5+10",
                 color = "white",
                 size = 24,
                 font = "Avenir")

img_ <- image_annotate(img_, "Georgia",
                      gravity = "north",
                      location = "+5+40",
                      color = "white",
                      size = 18,
                      font = "Avenir")

img <- image_annotate(img_, "source: https://biogeo.ucdavis.edu",
                 gravity = "northeast",
                 location = "+5+550",
                 color = "white",
                 size = 16,
                 font = "Avenir")

image_write(img, here("2023/images/geo_water.png"), format = "png")




