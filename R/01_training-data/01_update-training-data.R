## This code creates an update trianing data set 
## from the files generated by the matching app

## v1: initial version of household comprisons which follows blocking procedure

library(tidyverse)
library(here)

# load functions for calculating matching variables
#for(a in list.files(here("R", "functions"), full.names = T)){
#  source(a)
#}

source(here("R", "functions", "load_training_data.R"))
source(here("R", "functions", "build_comparisons.R"))
source(here("R", "functions", "add_matching_vars.R"))
source(here("R", "functions", "prepare_city.R"))
source(here("R", "functions", "unabbv_names.R"))
source(here("R", "functions", "compare_hh_rosters.R"))
source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "relate_pt.R"))

# load file containing household members of training cases
for_rosters <- read_csv(here("data", "for_rosters.csv")) %>% 
  mutate(namefrst = if_else(is.na(namefrst), "", namefrst),
         namelast = if_else(is.na(namelast), "", namelast))

# combine coded training files
full_data <- load_training_data(here("data", "Training"))

# write file for training models
write_csv(full_data, here("data", "training_all_vars_v3.csv"))
