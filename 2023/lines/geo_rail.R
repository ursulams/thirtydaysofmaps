library(here)
library(ggplot2)
library(sf)

# download rail lines + polygons
download.file("https://export.hotosm.org/downloads/c602a7be-dbf7-45c1-abcc-ef197ea0f60d/hotosm_geo_railways_gpkg.zip", 
              destfile = here("2023/lines/rail.zip"))
unzip(zipfile = here("2023/lines/rail.zip"), exdir = here("2023/lines/"))

geo_poly <- st_read(here("2023/kontur_boundaries_GE_20230628.gpkg"))

rail <- st_read(here("2023/lines/hotosm_geo_railways.gpkg"))

g <- ggplot() +
  geom_sf(data = geo_poly, aes(fill = osm_admin_level), color = "black") +
  scale_fill_manual("Open Street Map \nAdministrative Level",
                    values = c("#0b1f53", "#00435a", "#01537a", "#00685a", "#017c48","#f5b372", "#f5a221")) +
  geom_sf(data = rail, color = "#ff2000") +
  theme_void() +
  labs(title = "Rail lines in Georgia",
       caption = "source: data.humdata.org")
  
outfile <- here("2023/images/geo_rail_lines.png")
ggsave(outfile, bg = "white")  








