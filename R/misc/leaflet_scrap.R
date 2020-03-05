library(here)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)

ed40 <- st_read(here("data", "merged_eds", "Philadelphia_1940.shp")) %>% 
  #mutate(year = 1940, decade = "1930-40") %>% 
  #st_transform(crs = 3857) %>% 
  mutate(
    pct_b_cat = factor(case_when(
      ed_pct_b >= 90 ~ ">90%",
      ed_pct_b < 90 & ed_pct_b >= 50 ~ "50% - 90%",
      ed_pct_b < 50 & ed_pct_b >= 25 ~ "25% - 50%",
      ed_pct_b < 25 & ed_pct_b >= 5 ~ "5% - 25%",
      ed_pct_b < 5 ~ "<5%"
    ), levels = c("<5%", "5% - 25%", "25% - 50%", "50% - 90%", ">90%"))
  )

cols <- brewer.pal(n = 5, name = "PuBuGn")
pal <- colorFactor(cols, sort(unique(ed40$pct_b_cat)))

leaflet(ed40) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~pal(pct_b_cat))
