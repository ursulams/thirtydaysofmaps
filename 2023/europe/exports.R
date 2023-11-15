library(tidyverse)
library(here)
library(sf)
library(rayshader)


# read in geodata; domestic export data ('000 USD); subset to europe
euro <- read_sf(here("2023/europe/europe.geojson")) %>% 
  select(UN, geometry)

# +1 in V1 to avoid division by zero
download.file(url = "https://geostat.ge/media/57690/Domestic-Export-Country_2014-2023.xlsx", here("2023/europe/exports.xlsx"))
geo_exports <- readxl::read_xlsx(here("2023/europe/exports.xlsx"),
  range = "2014-2022-years!A4:K193")[c(4:30, 33:35, 38:39, 42, 49, 63, 99, 102, 118, 120, 132, 134, 143, 154, 157:158, 168, 176, 180), ] %>% 
  pivot_longer(cols = 3:11, names_to = "year", values_to = "export_total_000") %>%
  mutate(export_total = export_total_000*1000) %>% 
  ungroup() %>% 
  left_join(euro, by = c("Code" = "UN"))

# plot small multiples
palette <- colorRampPalette(c("#e44622", "#c87763", "#a39593", "#6bbdb3", "#60adb4"), bias = 4)(256)

g <- ggplot() +
  geom_sf(data = geo_exports, aes(geometry = geometry, fill = export_total), linewidth = 0.1) +
  scale_fill_gradientn(colours = palette, na.value = "white") +
  facet_wrap(~ year) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 10),
        strip.text = element_text(size = 14), 
        plot.margin = margin(2, 2, 2, 2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  labs(title = "Georgian Domestic Export Totals to Europe (USD)",
       caption = "sources: LEPL Revenue Service of the Ministry of Finance of Georgia\n
        Georgian State Electrosystem, JSC\n
        Electricity System Commercial Operator, JSC\n
        Georgian Gas Transportation Company, LTD\n
        https://github.com/leakyMirror/map-of-europe/tree/master")
            
# 3D plot
plot_gg(g, 
        offset_edges = TRUE,
        shadow_darkness = 0.3,
        width = 15, 
        height = 10, 
        scale = 100,
        windowsize = c(1400, 850),
        theta = 0, 
        phi = 60, 
        zoom = 0.4)

outfile <- "2023/images/geo_exports.png"

{
  if (!file.exists(outfile)){
    png::writePNG(matrix(1), target = outfile)
  }
  
  
  render_highquality(outfile,
                     interactive = FALSE,
                     lightaltitude = 70,
                     samples = 5000) # higher number increases size, may induce hang in rendering
}

