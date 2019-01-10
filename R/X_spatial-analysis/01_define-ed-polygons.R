library(here)
library(sf)
library(mapview)
library(dplyr)

# load pblks
pblk_30 <- here("data", "in-house_eds", "Philadelphia_1930_Pblk.shp") %>% 
  st_read()

#### 1910 ####

shertz_10 <- here("data", "shertzer_eds", "Philadelphia_1910.shp") %>% 
  st_read() %>% 
  st_transform(st_crs(pblk_30))

pblk_ed10 <- pblk_30 %>% 
  st_centroid() %>% 
  st_join(shertz_10, join = st_within) %>% 
  as_tibble() %>% 
  select(pblk_id, ED) %>%
  mutate(ED = as.numeric(ED)) %>% 
  merge(pblk_30, .) 

st_write(pblk_ed10, here("data", "in-house_eds", "pblk_merge_10.shp"))

ed10 <- union_sf(pblk_ed10, key = "ED", planarCRS = 2272)


#### 1920 ####



#### 1930 ####

ed30 <- here("data", "in-house_eds", "Philadelphia_1930_pblk_manuallyverifiedEDswithoutinstitutions.shp") %>% 
  st_read() %>% 
  rename(ED = ED_Manual) %>% 
  group_by(ED) %>% 
  summarise(Flag = paste(unique(Flag), collapse = ", "),
            FlagReason = paste(unique(FlagReason), collapse = ", ")) %>% 
  mutate(Flag = str_remove(Flag, ", 0"),
         Flag = str_remove(Flag, "0, "),
         FlagReason = str_remove(FlagReason, "NA, "),
         FlagReason = str_remove(FlagReason, ", NA")) %>% 
  st_cast()
# do this with split() and st_union() and rbind()

st_write(ed30, here("data", "final", "inhouse_ed_30.shp"))

shertz_30 <- here("data", "shertzer_eds", "Philadelphia_1930.shp") %>% 
  st_read() %>% 
  st_transform(st_crs(pblk_30))


#### 1940 ####