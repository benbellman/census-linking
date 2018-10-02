library(tidyverse)
library(here)

# load pre-preprocessed data
#full_data <- read_csv(here("data", "training_all_vars_v1.csv"))
full_data <- read.csv(here("data", "training_all_vars_v1.csv"), stringsAsFactors = F) %>% 
  as_tibble()

