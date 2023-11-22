library(tidyverse)
library(here)
library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(magick)

# country border to build raster;
geo_borders <- st_read(here("2023/kontur_boundaries_GE_20230628.gpkg")) %>% 
  filter(osm_admin_level == "2")

elev <- get_elev_raster(geo_borders, z = 7)

# conform raster to country borders
elev_mask <- mask(elev, geo_borders)
rayshade_matrix <- raster_to_matrix(elev_mask)

# as spencer schien utilizes here: https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
# dynaimcally set window height and width based on object size
w <- nrow(rayshade_matrix)
h <- ncol(rayshade_matrix)

# scale the dimensions to use as multipliers
wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

# limit ratio so that the shorter side is at least .7 of longer side
if (min(c(wr, hr)) < .80) {
  if (wr < .80) {
    wr <- .80
  } else {
    hr <- .80
  }
}

rayshade_matrix %>% 
  height_shade(texture = colorRampPalette(c("#484058", "#696286", "#f2d9a4", "#dc5547"))(256)) %>%
  plot_3d(heightmap = rayshade_matrix,
          zscale = 100,
          solid = FALSE,
          windowsize = c(900*wr, 900*hr))

render_camera(theta = 5, 
              phi = 90, 
              zoom = 0.6)

outfile <- "2023/images/geo_raster.png"

png::writePNG(matrix(1), target = outfile)
  
render_highquality(outfile,
               width = 900*wr,
               height = 900*hr,
               interactive = FALSE,
               lightaltitude = 30,
               lightintensity = 700,
               samples = 10000) # higher number increases size, may induce hang in rendering


img <- image_read(outfile)

img_ <- image_annotate(img, "Highest Peaks",
                       gravity = "north",
                       location = "+5+40",
                       color = "#273049",
                       size = 28,
                       font = "Avenir")
img_ <- image_annotate(img_, "Greater Caucasus, Georgia",
                       gravity = "north",
                       location = "+5+80",
                       color = "#273049",
                       size = 22,
                       font = "Avenir")
img_ <- image_annotate(img_, "Shkhara (5193 m)",
                       gravity = "none",
                       location = "+397+361",
                       color = "#273049",
                       size = 16,
                       font = "Avenir")
img_ <- image_annotate(img_, "Dzhangi (5058 m)",
                       gravity = "none",
                       location = "+381+346",
                       color = "#273049",
                       size = 16,
                       font = "Avenir")
img_ <- image_annotate(img_, "Kazbegi (5033 m)",
                       gravity = "none",
                       location = "+491+394",
                       color = "#273049",
                       size = 16,
                       font = "Avenir")
img <- image_annotate(img_, "source: summitpost.org",
                      gravity = "northeast",
                      location = "+0+690",
                      color = "#273049",
                      size = 16,
                      font = "Avenir")

image_write(img, here("2023/images/geo_peaks.png"), format = "png")

