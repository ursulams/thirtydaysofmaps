library(here)
library(tidyverse)
library(sf)
library(rayshader)

# 3D maps of illegal logging in the country of Georgia
# illegal logging data
download.file(url = "https://geostat.ge/media/45374/Environment.xls", here("2023/choropleth/logging.xls"))
logging <- readxl::read_xls(here("2023/choropleth/logging.xls"),
                            sheet = "Illegal Logging",
                            skip = 1,
                            na = "...", 
                            n_max = 12) %>%
  rename(name_en = `...1`) 

# regional polygons and name adjustments for joining
geo_log <- st_read(here("2023/kontur_boundaries_GE_20230628.gpkg"))  %>% 
  filter(osm_admin_level %in% c("3", "4")) %>% 
  select(name_en, geom) %>% 
  mutate(name_en = str_replace_all(name_en, c("Inner" = "Shida", 
                                              "Lower" = "Kvemo", 
                                              "Upper" = "Zemo",
                                              "Autonomous Republic of Adjara" = "Adjara AR"))) %>% 
  left_join(logging, by = "name_en") %>% 
  pivot_longer(cols = 2:7, names_to = "year", values_to = "illegal logging (cubic meters)")

# choropleth colors
palette <- colorRampPalette(c("#0b1e33", "#013c58", "#00537a", "#287c9c", "#a5ce8d",  "#ffba42"), bias = 4)(256)

# small multiples
g <- ggplot() +
  geom_sf(data = geo_log, aes(geometry = geom, fill = `illegal logging (cubic meters)`), linewidth = 0.1) +
  scale_fill_gradientn(colours = palette, na.value = "white") +
  facet_wrap(~ year) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 22, hjust = 0.5, vjust = 10),
        strip.text = element_text(size = 14), 
        plot.margin = margin(2, 2, 2, 2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  labs(title = "Illegal logging in Georgia (cubic meters)",
       caption = "sources: Ministry of Environment Protection and Agriculture of Georgia\n
        LEPL Forestry Agency of Adjara\n
        LEPL National Forestry Agency\n
        data.humdata.org")

# 3D plot
plot_gg(g, 
        offset_edges = TRUE,
        shadow_darkness = 0.3,
        width = 15, 
        height = 10, 
        scale = 100,
        windowsize = c(1400, 850),
        theta = -1, 
        phi = 60, 
        zoom = 0.4)

outfile <- "2023/images/geo_logging.png"

{
if (!file.exists(outfile)){
  png::writePNG(matrix(1), target = outfile)
}
  

render_highquality(outfile,
                   interactive = FALSE,
                   lightaltitude = 70,
                   samples = 5000) # higher number increases size, may induce hang in rendering
}
