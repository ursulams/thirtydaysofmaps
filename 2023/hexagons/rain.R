library(tidyverse)
library(here)
library(h3jsr)
library(sf)
library(fuzzyjoin)

# read in geo and rain data
pcodes <- read.csv("https://data.humdata.org/dataset/cb963915-d7d1-4ffa-90dc-31277e24406f/resource/793e66fe-4cdb-4076-b037-fb8c053239e2/download/global_pcodes.csv") %>% 
  filter(Location == "GEO")

rain <- read.csv("https://data.humdata.org/dataset/a1e45bba-c52b-4c23-897a-2a2beb664591/resource/976e576d-d4c3-4e0f-b20a-ee048a749401/download/geo-rainfall-adm2-full.csv",
                 header = TRUE)[-1, ] %>% # first row is metadata
  left_join(pcodes %>% select(P.Code, Name), by = c("ADM2_PCODE" = "P.Code")) %>% 
  filter(grepl("08-01", date) & date >= as.Date("2013-08-01")) %>% 
  transmute(obs_date = as.Date(date),
            name_en = Name,
            r3h = as.numeric(r3h))

geo_hex <- st_read(here("2023/kontur_boundaries_GE_20230628.gpkg")) %>% 
  select(name_en, geom) %>% 
  transmute(name_en = sub("Municipality|Village|Raion|Administrative Unit|District", "", name_en),
            bounds = st_centroid(geom), 
            h3 = point_to_cell(bounds, res = 4), 
            hex = cell_to_polygon(input = h3, simple = FALSE)) %>% 
  filter(!is.na(name_en))

geo_rain <- geo_hex %>% 
  stringdist_join(rain, by = "name_en", mode = "left", method = "lv", max_dist = 9, ignore_case = TRUE) %>% 
  transmute(location = name_en.y,
            geometry = hex$geometry,
            summer = year(obs_date),
            `long term average 3 mo (mm)` = r3h)%>% 
  filter(!is.na(summer))

g <- ggplot() +
  geom_sf(data = geo_rain, aes(geometry = geometry, fill = `long term average 3 mo (mm)`)) +
  scale_fill_viridis_c("long term average 3 mo rolling agg (mm)", option = "G") +
  facet_wrap(~ summer) +
  theme_void() +
  labs(title = "Summertime rolling average rainfall totals in Georgia",
       caption = "sources: WFP Climate Hazards Group InfraRed Precipitation \n data.humdata.org") 
            
g
