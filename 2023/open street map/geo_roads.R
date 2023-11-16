library(here)
library(tidyverse)
library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(magick)

# OSM map of roadways in the country of Georgia's capital city, Tbilisi
download.file(url = "https://export.hotosm.org/downloads/c602a7be-dbf7-45c1-abcc-ef197ea0f60d/hotosm_geo_roads_gpkg.zip",
              destfile = here("2023/open street map/roads.zip"))
unzip(zipfile = here("2023/open street map/roads.zip"), exdir = here("2023/open street map/"))

# tbilisi region border
tibs_border <- st_read(here("2023/kontur_boundaries_GE_20230628.gpkg")) %>% 
  subset(select = c("name_en", "geom")) %>% 
  filter(name_en == "Tbilisi")

# subset road network and spatially join to tbilisi region
tibs_roads <- st_read(here("2023/open street map/hotosm_geo_roads.gpkg")) %>% 
  subset(select = c("highway", "geom")) %>% 
  filter(highway %in% c("primary", "secondary", "tertiary"))  %>% 
  st_join(tibs_border, left = FALSE)

elev <- get_elev_raster(tibs_border, z = 12)
tibs_mat <- raster_to_matrix(elev)

# as spencer schien utilizes here: https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
# dynaimcally set window height and width based on object size
w <- nrow(tibs_mat)
h <- ncol(tibs_mat)

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

tibs_mat %>% 
height_shade(texture = colorRampPalette(c("#4e3e42", "#525959", "#707880", "#8a9299", "#b7f4f5"))(256)) %>%
  add_overlay(generate_line_overlay(tibs_roads, 
                                    extent = tibs_border,
                                    heightmap = tibs_mat, 
                                    color = "#fe1491", 
                                    linewidth = 5)) %>%
  add_overlay(generate_line_overlay(tibs_roads, 
                                    extent = tibs_border,
                                    heightmap = tibs_mat, 
                                    color = "white", 
                                    linewidth = 2)) %>%   
  add_overlay(generate_compass_overlay(heightmap = tibs_mat,
                                       x = 0.975,
                                       color1 = "#b7f4f5",
                                       color2 = "#fe1491",
                                       size = 0.025, 
                                       text_color = "#b7f4f5", 
                                       text_size = 3)) %>% 
  plot_3d(heightmap =  tibs_mat,
        zscale = 10,
        solid = FALSE,
        windowsize = c(800*wr, 800*hr)) 
render_camera(theta = 0, phi = 60, zoom = 0.4)

outfile <- here("2023/images/tibs_roads.png")

{
  if (!file.exists(outfile)){
    png::writePNG(matrix(1), target = outfile)
  }
  # to lower the resolution and speed up rendering, reduce raster & matrix multiplier from 5000
  render_highquality(outfile,
                     width = 800*wr,
                     height = 790*hr,
                     interactive = FALSE,
                     lightdirection = 245,
                     lightaltitude = 30,
                     lightintensity = c(400, 100),
                     lightcolor = c("#b7f4f5", "white"),
                     samples = 4000) # higher number increases size, may induce hang in rendering
}

img <- image_read(outfile)

img_ <- image_annotate(img, "Road Network",
                 gravity = "north",
                 location = "+5+10",
                 color = "white",
                 size = 24,
                 font = "Avenir")

img_ <- image_annotate(img_, "Tbilisi, Georgia",
                      gravity = "north",
                      location = "+5+40",
                      color = "white",
                      size = 18,
                      font = "Avenir")

img <- image_annotate(img_, "source: Humanitarian OpenStreetMap Team",
                 gravity = "northeast",
                 location = "+5+520",
                 color = "white",
                 size = 16,
                 font = "Avenir")

image_write(img, here("2023/images/tibs_roads.png"), format = "png")




