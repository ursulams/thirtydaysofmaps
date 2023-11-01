library(tidyverse)
library(sf)
library(here)


# read in airport lat/lon
points <- read.csv("https://ourairports.com/countries/GE/airports.csv", header = TRUE) %>% 
  st_as_sf(coords = c("longitude_deg","latitude_deg"), crs = "WGS84", remove = FALSE) %>% 
  select(iata_code, airport_name = name, type, elevation_ft, geometry) %>% 
  mutate(status= if_else(type == "closed", "closed", "not closed"))

# polygons of administrative boundaries with spatially joined airport points
# available here: https://data.humdata.org/dataset/kontur-boundaries-georgia
geo_poly <- st_read(here("2023/kontur_boundaries_GE_20230628.gpkg"))

g <- ggplot() +
  geom_sf(data = geo_poly, aes(fill = log(population))) +
  scale_fill_viridis_c("population (log)", option = "G") +
  geom_sf(data = points, aes(pch = status, color = status), size = 4, stroke = 0.8) +
  geom_sf_text(data = points, aes(label = iata_code), size = 4, color = "white") +
  scale_shape_manual(values = c(4, 1)) +
  scale_color_manual(values = c("#ff2000", "#90ee90")) +
  theme_void() +
  labs(title = "Airports in Georgia",
       caption = "sources:  ourairports.com \n data.humdata.org")

outfile <- here("2023/images/geo_airports.png")
ggsave(outfile, bg = "white")






