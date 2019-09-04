library(here)
library(rio)
library(dplyr)
library(purrr)
library(readr)
library(sf)

source(here("R", "functions", "aggregate_microdata.R"))

# Load ED polygons
poly10 <- st_read(here("data", "shertzer_eds", "Philadelphia_1910.shp"))
poly20 <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp"))
poly30 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp"))
poly40 <- st_read(here("data", "shertzer_eds", "Philadelphia_1940.shp"))

polys <- list(poly10, poly20, poly30, poly40)
names(polys) <- c("1910", "1920", "1930", "1940")

# Aggregate microdata to ED level
ed_data <- c(10, 20, 30, 40) %>% 
  map(aggregate_microdata, ed) %>% 
  map(mutate, ed = as.character(ed)) %>% 
  map(select, ed, year, total_pop, n_black, n_frnbrn, pct_black, pct_frnbrn, mean_sei) %>% 
  map(rename, ed_total_pop = total_pop,
      ed_n_black = n_black, 
      ed_n_frnbrn = n_frnbrn, 
      ed_pct_black = pct_black, 
      ed_pct_frnbrn = pct_frnbrn, 
      ed_mean_sei = mean_sei) %>% 
  bind_rows()

# Create function to calculate Queen contiguity spatial lags
# generate lagged values for ED-level vars using queen contiguity (simple intersect)
calc_lags <- function(ed_num, year){
  # pull polygons for given year
  poly <- polys[[as.character(year)]]
  # get ED numbers of touching polygon main ED
  neighs <- suppressWarnings(st_intersection(filter(poly, ED == ed_num), poly)$ED.1)
  # drop focal ED from the lagged calculations, want to measure distinct area diff. from ED
  #neighs <- neighs[!(neighs == ed_num)]
  # filter only these EDs from data and return lagged values (add ed1 and year1 for merging with ed data)
  ed_data %>% 
    filter(year == year & ed %in% neighs) %>% 
    summarise(lag_pct_black = sum(ed_n_black) / sum(ed_total_pop) * 100,
              lag_pct_frnbrn = sum(ed_n_frnbrn) / sum(ed_total_pop) * 100,
              lag_mean_sei = weighted.mean(x = ed_mean_sei, w = ed_total_pop)) %>% 
    mutate(ed = ed_num, year = year)
}

# calculate and merge spatially lagged variables
lagged <- map2_dfr(ed_data$ed, ed_data$year, calc_lags)
ed_data <- left_join(ed_data, lagged)

# export ED-level data files for each year
filter(ed_data, year == 1910) %>% write_csv(here("data", "ed_data", "ED_data_1910.csv"))
filter(ed_data, year == 1920) %>% write_csv(here("data", "ed_data", "ED_data_1920.csv"))
filter(ed_data, year == 1930) %>% write_csv(here("data", "ed_data", "ED_data_1930.csv"))
filter(ed_data, year == 1940) %>% write_csv(here("data", "ed_data", "ED_data_1940.csv"))

