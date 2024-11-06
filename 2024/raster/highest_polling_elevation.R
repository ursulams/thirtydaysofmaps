library(tidyverse)
library(here)
library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(magick)

# state border to build raster
# uncomment below to download and unzip shapefile from UNM GIS repo
#shp_dir <- here("2024/raster/")
#url <- "https://gstore.unm.edu/apps/rgis/datasets/3b1de40a-b37e-4d35-abdd-fd40bbbbe39e/New_Mexico_Voting_Precinct_20240703.original.zip"
#zipfile <- download.file(url, destfile = paste0(shp_dir, gsub(".*/", "", url)))
#sapply(list.files(shp_dir, pattern = "zip", full.names = TRUE), unzip, exdir = shp_dir)

nm_border <- st_read(here("2024/raster/New_Mexico_Voting_Precinct_20240703/New_Mexico_Voting_Precinct_20240703_FGDC.shp"), quiet = TRUE) |>
  st_transform("+proj=longlat +datum=WGS84")

poll_locations <- st_read(here("2024/raster/polling_sites.geojson")) |> 
  st_join(nm_border, left = FALSE)

elev <- get_elev_raster(nm_border, z = 7)

# conform raster to state borders
elev_mask <- mask(elev, nm_border)
rayshade_matrix <- raster_to_matrix(elev_mask)

# dynaimcally set window height and width based on object size
w <- nrow(rayshade_matrix)
h <- ncol(rayshade_matrix)

# scale the dimensions to use as multipliers
wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

# limit ratio so that the shorter side is at least 8/10ths of longer side
if (min(c(wr, hr)) < .80) {
  if (wr < .80) {
    wr <- .80
  } else {
    hr <- .80
  }
}

rayshade_matrix |> 
  height_shade(texture = colorRampPalette(c("#023047", "#219ebc", "#8ecae6"))(256)) |> 
  add_overlay(generate_point_overlay(poll_locations, 
                                    extent = nm_border,
                                    heightmap = rayshade_matrix, 
                                    resolution_multiply = 0.5,
                                    color = "#fb8500", 
                                    size = 10)) |> 
  add_overlay(generate_point_overlay(poll_locations, 
                                     extent = nm_border,
                                     heightmap = rayshade_matrix, 
                                     resolution_multiply = 0.5,
                                     color = "#fdd7ac", 
                                     size = 6)) |> 
plot_3d(heightmap = rayshade_matrix,
          zscale = 10,
          solid = FALSE,
          shadow_darkness = 0.2,
          windowsize = c(900*wr, 900*hr))

render_camera(theta = 6, phi = 80, zoom = 0.6)

outfile <- "2024/images/nm_polls.png"

png::writePNG(matrix(1), target = outfile)

render_highquality(outfile,
                   width = 900*wr,
                   height = 900*hr,
                   lightcolor = "#8ecae6",
                   point_radius = 4,
                   interactive = FALSE,
                   lightaltitude = 65,
                   lightintensity = c(600, 300),
                   min_variance = 0,
                   sample_method = "sobol",
                   samples = 1000) # higher number increases size, may induce hang in rendering


img <- image_read(outfile)

img_ <- image_annotate(img, "Highest Polling Places in the United States",
                       gravity = "north",
                       location = "+5+40",
                       color = "#126782",
                       size = 28,
                       font = "Avenir")
img_ <- image_annotate(img_, "Cloudcroft, New Mexico (2644 m)",
                       gravity = "north",
                       location = "+5+80",
                       color = "#126782",
                       size = 22,
                       font = "Avenir")
img <- image_annotate(img_, "sources: New Mexico Resource Geographic Information System, Otero County, New Mexico",
                      gravity = "northeast",
                      location = "+0+850",
                      color = "#126782",
                      size = 12,
                      font = "Avenir")

image_write(img, here("2024/images/nm_polls.png"), format = "png", quality = 100)
