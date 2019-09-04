library(rio)
library(here)
library(sf)
library(purrr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(tigris)
library(magrittr)
library(cowplot)
library(ghibli)

#source(here("R", "functions", "aggregate_microdata.R"))

### Make a bivariate map for 1930 race data
# one variable is % black
# the other variable is mean SEI

# get water area from tigris
#options(tigris_use_cache = FALSE)
#water <- area_water(state = 42, county = 101)

# get ED data
#ed_data <- map(c(10, 20, 30, 40), aggregate_microdata, ed) %>% 
#  rename(ED = ed)
#ed_data[[1]]$year <- 1910
#ed_data[[2]]$year <- 1920
#ed_data[[3]]$year <- 1930
#ed_data[[4]]$year <- 1940

ed_data <- list(
  here("data", "ed_data", "ED_data_1910.csv"),
  here("data", "ed_data", "ED_data_1920.csv"),
  here("data", "ed_data", "ED_data_1930.csv"),
  here("data", "ed_data", "ED_data_1940.csv")
) %>% 
  map(import) %>% 
  map(mutate, ED = as.character(ed)) %>% 
  map(select, -ed)
ed_data[[1]]$year <- 1910
ed_data[[2]]$year <- 1920
ed_data[[3]]$year <- 1930
ed_data[[4]]$year <- 1940

ed_data <- bind_rows(ed_data)


# load ED polyogons
ed10 <- st_read(here("data", "shertzer_eds", "Philadelphia_1910.shp"))
ed20 <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp"))
ed30 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp"))
ed40 <- st_read(here("data", "shertzer_eds", "Philadelphia_1940.shp")) %>% st_transform(crs = st_crs(ed30))
ed10$year <- 1910
ed20$year <- 1920
ed30$year <- 1930
ed40$year <- 1940

ed_poly <- rbind(ed10,ed20,ed30,ed40)

# merge together
#ed30 <- merge(ed30_poly, ed30_data)
test <- inner_join(ed_data, ed_poly)
#ed30 <- test

# at some point, look at EDs that are missing microdata data

# test arguments
#sf_data <- ed30
#var1 <- "pct_black"
#var2 <- "pct_frnbrn"

  
# create 3 buckets for % black
cutoffs_var1 <- c(0, 5, 20, 100)

# create 3 buckets for % foreign born
cutoffs_var2 <- c(0, 15, 25, 100)

var1_lab <- "% black"
var2_lab <- "% immigrant"

### bivariate mapping function
#bivariate_map <- function(sf_data, var1, var2, cutoffs_var1, cutoffs_var2, var1_lab, var2_lab){
  
  # create quosures of variable names
  #var1 <- enquo(var1)
  #var2 <- enquo(var2)

  # create color scale that encodes two variables
  # red for gini and blue for mean income
  # the special notation with gather is due to readibility reasons
  bivariate_color_scale <- tibble(
    "3 - 3" = "#2a5a5b", # high inequality, high income
    "2 - 3" = "#567994",
    "1 - 3" = "#6c83b5", # low inequality, high income
    "3 - 2" = "#5a9178",
    "2 - 2" = "#90b2b3", # medium inequality, medium income
    "1 - 2" = "#b5c0da",
    "3 - 1" = "#73ae80", # high inequality, low income
    "2 - 1" = "#b8d6be",
    "1 - 1" = "#e8e8e8" # low inequality, low income
  ) %>%
    gather("group", "bg_fill")
  
  # attach groups and colors to ED data
  test %<>%
    # categorize vars by cutoffs
    mutate(var1_cat = cut(ed_pct_black, breaks = cutoffs_var1, include.lowest = TRUE),
           var2_cat = cut(ed_pct_frnbrn, breaks = cutoffs_var2, include.lowest = TRUE),
           # create 9 bi-variate groups
           group = paste(as.numeric(var1_cat), "-", as.numeric(var2_cat))) %>% 
    # merge hex color codes
    left_join(bivariate_color_scale, by = "group")
  
  # create map
  
  ggplot(test) +
    # color EDs according to their pct_black / mean_sei combination
    # use white boundaries
    geom_sf(aes(fill = bg_fill), lwd = 0, col = "white") +
    # as the sf object municipality_prod_geo has a column with name "fill" that
    # contains the literal color as hex code for each municipality, we can use
    # scale_fill_identity here
    scale_fill_identity() +
    facet_wrap(~year, nrow = 2) +
    # add titles
    labs(
      x = NULL,
      y = NULL#,
      #title = "Black and Immigrant Neighborhoods in Philadelphia",
      #subtitle = paste0("Enumeration Districts in 1930"),
      #caption = "Sources: Minnesota Population Center, Allison Shertzer"
    ) +
    # add the theme
    theme_minimal() +
    theme(axis.text = element_blank()) +
    ggsave(here("figures", "bivar_race_by_year.png"), height = 7, width = 5)
    
  
  # create legend
  # separate the groups
  bivariate_color_scale %<>%
    separate(group, into = c("var1", "var2"), sep = " - ") %>%
    mutate(var1 = as.integer(var1),
           var2 = as.integer(var2))
  
  ggplot() +
    geom_tile(
      data = bivariate_color_scale,
      mapping = aes(
        x = var1,
        y = var2,
        fill = bg_fill)
    ) +
    scale_fill_identity() +
    #labs(x = paste("Higher", var1_lab, sprintf("\u27f6")),
    #     y = paste("Higher", var2_lab, sprintf("\u27f6"))) +
    labs(x = paste(var1_lab, "-->"),
         y = paste(var2_lab, "-->")) +
    theme_map() +
    # make font small enough
    theme(
      axis.title = element_text(size = 6), 
      axis.title.y = element_text(angle = 90)
    ) +
    # quadratic tiles
    coord_fixed() +
    ggsave(here("figures", "bivar_race_legend.pdf"), height = 1, width = 1)
  
  #ggdraw() +
  #  draw_plot(map, 0, 0, 1, 1) +
  #  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)
#}


  # lets do a mean sei map real quick
  test %>% 
    mutate(sei_cat = factor(case_when(
      ed_mean_sei < 10 ~ "< 10",
      ed_mean_sei >= 10 & ed_mean_sei < 20 ~ "10 to 20",
      ed_mean_sei >= 20 ~ ">= 20"
    ), levels = c("< 10","10 to 20",">= 20"))) %>% 
  ggplot() +
    # color EDs according to their pct_black / mean_sei combination
    # use white boundaries
    geom_sf(aes(fill = sei_cat), lwd = 0, col = "white") +
    scale_fill_manual(values = ghibli_palette("MononokeMedium")[c(7,6,5)]) +
    facet_wrap(~year, nrow = 2) +
    # add titles
    labs(
      x = NULL,
      y = NULL,
      fill = "Mean SEI"
    ) +
    # add the theme
    theme_minimal() +
    theme(axis.text = element_blank()) +
    ggsave(here("figures", "ed_map_mean_sei.png"), height = 7, width = 7)
#bivar <- 
  
#bivariate_map(
#  sf_data = ed30,
#  var1 = pct_black,
#  var2 = pct_frnbrn,
#  cutoffs_var1 = c(0, 5, 20, 100),
#  cutoffs_var2 = c(0, 10, 25, 100),
#  var1_lab = "% Black",
#  var2_lab = "% Foreign born"
#)

#ggdraw() + draw_plot(bivar$map, 0, 0, 1, 1) + draw_plot(bivar$legend, 0.05, 0.075, 0.2, 0.2)


### Ideas

# Add 3x3 matrix counts to the legend

# re-position legend in the map's negative space

# decrease weight of lat/lon lines in theme_map

