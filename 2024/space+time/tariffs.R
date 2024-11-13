library(gtrendsR)
library(tidyverse)
library(here)
library(sf)
library(rayshader)

state_borders <- read_sf(here("2024/space+time/us_states.geojson")) |> 
  select(NAME, geometry)

states <- paste("US", datasets::state.abb, sep = "-")

# Sys.sleep() to avoid 429 response 
call_list <- lapply(states, function(i){
    tryCatch({call <- gtrends(keyword = "what is a tariff",
    geo = i,
    time =  "2024-10-31 2024-11-07",
    gprop = "web",
    low_search_volume = TRUE,
    onlyInterest = TRUE)$interest_over_time
    return(call)})
    Sys.sleep(20)
    }
  )

df <- do.call(rbind, call_list) |> 
  mutate(geo = sub("US-", "", geo)) |> 
  left_join(tigris::fips_codes |> distinct(state, NAME = state_name), by = c("geo" = "state")) |> 
  left_join(state_borders, by = "NAME")

# plot small multiples
palette <- colorRampPalette(c("#8cc5dc", "#4ba3c3", "#694463", "#843e5d", "#ba324f"), bias = 4)(256)         
  
g <- ggplot() +
  geom_sf(data = df, aes(geometry = geometry, fill = hits), linewidth = 0.1) +
  scale_fill_gradientn(colors = palette, na.value = "#cce6f4") +
  facet_wrap(~ date) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#eef7fb", color = "#eef7fb"),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 10),
        strip.text = element_text(size = 14), 
        plot.margin = margin(2, 2, 2, 2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  labs(title = "Popularity of the Google search term 'what is a tariff'",
       caption = "sources: Google Trends\n
        https://newsinitiative.withgoogle.com/resources/trainings/fundamentals/google-trends-understanding-the-data")

# 3D plot
plot_gg(g, 
        offset_edges = TRUE,
        shadow_darkness = 0.3,
        width = 15, 
        height = 10, 
        scale = 100,
        windowsize = c(1400, 850),
        theta = -0.25, 
        phi = 80, 
        zoom = 0.4)


outfile <- "2024/images/tariff_searches.png"

{
  if (!file.exists(outfile)){
    png::writePNG(matrix(1), target = outfile)
  }
  
  
  render_highquality(outfile,
                     interactive = FALSE,
                     lightaltitude = 70,
                     sample_method = "sobol",
                     samples = 500) # higher number increases size, may induce hang in rendering
}
