library(dplyr)
library(rio)
library(here)

for(a in list.files(here("R", "functions"), full.names = T)){
  source(a)
}

#for(a in c(10, 20, 30)){
for(a in c(20, 30)){

t1 <- a
t2 <- t1 + 10

cw <- import(here("data", paste0("potential_matches_linked_", t1, "_", t2, ".csv"))) %>% 
  filter(auto_match == 1) %>% 
  select(uniqueid2, uniqueid1)

export(cw, here("data", "crosswalks", paste0("crosswalk_", t1, "_", t2, ".csv")))

linked <- load_linked_sample(t1, t2)

export(linked, here("data", "linked", paste0("linked_", t1, "_", t2, ".csv")))

}
