library(dplyr)
library(stringr)
library(tibble)
library(RecordLinkage)
library(doParallel)
library(foreach)
library(rio)
library(here)
library(phonics)

source(here("R", "functions", "ferrie_matching.R"))
source(here("R", "functions", "build_comparisons.R"))
source(here("R", "functions", "add_matching_vars.R"))
source(here("R", "functions", "prepare_city.R"))
source(here("R", "functions", "unabbv_names.R"))
source(here("R", "functions", "compare_hh_rosters.R"))
source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "relate_pt.R"))

### This script applies Ferrie 1996 matching technique on comparison sample
# Note: must adapt slightly for backwards matching


# Load the comparison sample
to_compare <- import(here("data", "linking_comparisons", "comparison_sample.csv"))

# Load Philly 1920 for matching
m1 <- paste0("Phl", as.character(20), ".csv") %>% 
  here("data", "micro", .) %>% 
  prepare_city()
names(m1) <- paste0(names(m1), "1")

# Load Philly 1930 for household comparison
m2 <- paste0("Phl", as.character(30), ".csv") %>% 
  here("data", "micro", .) %>% 
  prepare_city()
names(m2) <- paste0(names(m2), "2")

#m2 <- import(here("data", "Phl30_slim2.csv"))

# add NYSIIS encoding for last names
to_compare$namelast_nysiis2 <- nysiis(to_compare$namelast2)
m1$namelast_nysiis1 <- nysiis(m1$namelast1)

# truncate the first name to 4 characters
to_compare$namefrst_trunc2 <- toupper(substr(to_compare$namefrst2, 1, 4))
m1$namefrst_trunc1 <- toupper(substr(m1$namefrst1, 1, 4))

# split for looping in parallel
to_compare <- to_compare %>%
  split(.$serial2)

# build the comparisons
cl <- makeCluster(32)
registerDoParallel(cl)

out <- foreach(x = to_compare, .combine = "rbind", .packages = c("dplyr")) %dopar% {
  build_comparisons_ferrie(x)
}

#stop cluster
stopCluster(cl)

# return rows without NA junk
out <- out %>% 
  filter(is.na(serial2) == F)

# export potential matches to save for later
export(out, here("data", "linking_comparisons", paste0("potential-matches-ferrie.csv")), row.names = F)


# re-split into focal record blocks to apply rules for elimination
to_eliminate <- split(out, out$serial2)


# apply elimination rules
cl <- makeCluster(32)
registerDoParallel(cl)

links <- foreach(x = to_eliminate, .combine = "rbind", .packages = c("dplyr", "phonics")) %dopar% {
  eliminate_links_ferrie(x)
}

#stop cluster
stopCluster(cl)

# return rows without NA junk
links <- links %>% 
  filter(is.na(serial2) == F)

cw <- links %>% 
  select(uniqueid2, uniqueid1)

export(cw, here("data", "linking_comparisons", "crosswalks", paste0("ferrie_crosswalk.csv")))





