library(here)
library(rio)
library(dplyr)
library(sf)

## load in 20 and 30 ED polygons
poly20 <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp"), stringsAsFactors = F)
poly30 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp"), stringsAsFactors = F)

## create named list for ED overlaps across years
## each element refers to ED in 1920
## elements contain vector of 1930 EDs that touches the 1920 ED

# initialize emply list
overlaps <- list()

# loop through each 1920 ED
for(a in 1:nrow(poly20)){
  # grab ED
  ed <- poly20[a,]
  # do intersection and only keep if area is greater than 5 square meters
  over <- st_intersection(ed, poly30)
  over$area <- as.numeric(st_area(over))
  over <- filter(over, area >= 25)
  # put 1930 EDs in overlap list
  overlaps[[a]] <- over$ED.1
}

# attach 1920 names to overlap list
names(overlaps) <- poly20$ED

## load 20-30 linked sample for locational attainment models
links <- here("data", "for_models", "phl_loc_attain.csv") %>% 
  import() %>% 
  filter(year1 == 1920)

## create "moved" variable for each record
# 0 if 1930 ED is in list of valid overlaps
# 1 if not
links$moved <- 0
for(a in 1:nrow(links)){
  row <- links[a,]
  if(row$ed2 %in% overlaps[[row$ed1]]){
    links$moved <- 1
  }
}



