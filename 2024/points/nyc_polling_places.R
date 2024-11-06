library(tidyverse)
library(RSocrata)
library(sf)
library(here)

# read in airport lat/lon
points <- read.socrata("https://data.cityofnewyork.us/resource/mifw-tguq.json?site_status=A", 
            app_token = Sys.getenv("SOCRATA_KEY")) |> 
  filter(!is.na(longitude)) |> 
  st_as_sf(coords = c("longitude","latitude"), crs = "WGS84", remove = FALSE) |> 
  select(city, site_name, nta, longitude, latitude) |> 
  mutate(`site type` = if_else(grepl("PS|Elementary|School|HS|H.S.|IS |MS |Academy|College", site_name), 
    "is a school", "is not a school"))

# polygons of nyc borough boundaries with spatially joined polling place points
nyc_poly <- st_read(here("2024/points/nyc_boundaries.geojson"))

g <- ggplot() +
  geom_sf(data = nyc_poly, fill = "#d8dbe2", color = "#355070") +
  geom_sf(data = points, aes(color = `site type`, shape = `site type`), 
          size = 0.8) +
  scale_color_manual(values = c("#e07a5f", "#4b3f72")) +
  labs(title = "NYC Polling Sites: 2024 General Election",
       caption = "source:  data.cityofnewyork.us") +
  theme_void() +
  theme(plot.title = element_text(color = "#355070", size = 14, hjust = 0.25),
        legend.title = element_blank(),
        legend.margin = margin(5, 5, 5, 5, "pt"))


outfile <- here("2024/images/nyc_polls.png")
ggsave(outfile, bg = "#d8dbe2")






