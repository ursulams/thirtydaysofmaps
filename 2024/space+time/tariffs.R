library(gtrendsR)
library(tidyverse)

states <- datasets::state.abb[1:3]

# Sys.sleep() avoids 429 response 
calls <- lapply(states, function(i){Sys.sleep(5); gtrends(keyword = "tariff",
               geo = i, 
               time =  "2024-10-31 2024-11-07",
               gprop = "web",
               low_search_volume = TRUE,
               cookie_url = "http://trends.google.com/Cookies/NID",
               onlyInterest = TRUE)$interest_over_time})
