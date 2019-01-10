library(rvest)
library(stringr)
library(jsonlite)
library(dplyr)
library(here)
library(readr)

codes <- read_html("https://usa.ipums.org/usa-action/variables/occ1950#codes_section") %>% 
  as.character() %>% 
  str_split("\\n") %>% 
  .[[1]] %>% 
  .[18] %>% 
  str_remove(",$") %>% 
  str_remove("^ *categories: ") %>% 
  fromJSON() %>% 
  select(label, code) %>% 
  rename(sei = code) %>% 
  mutate(sei = as.numeric(sei)) %>% 
  filter(!is.na(sei))

write_csv(codes, here("data", "misc", "occ50_codes.csv"))
