library(dplyr)
library(stringr)
library(tibble)
library(RecordLinkage)
library(doParallel)
library(foreach)
library(rio)
library(here)

source(here("R", "functions", "build_comparisons.R"))
source(here("R", "functions", "add_matching_vars.R"))
source(here("R", "functions", "prepare_city.R"))
source(here("R", "functions", "unabbv_names.R"))
source(here("R", "functions", "compare_hh_rosters.R"))
source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "relate_pt.R"))


  
# set years of data to use
t1 <- 20
t2 <- 30

# load files for each decade
m1 <- paste0("Phl", as.character(t1), ".csv") %>% 
  here("data", "micro", .) %>% 
  prepare_city()

m2 <- paste0("Phl", as.character(t2), ".csv") %>% 
  here("data", "micro", .) %>% 
  prepare_city()

names(m1) <- paste0(names(m1), "1")
names(m2) <- paste0(names(m2), "2")

## create potential matches
#set.seed(123)

# select household heads from t2
#to_compare <- m2 %>% 
#  filter(relate2 == "Head/householder") %>% 
#  filter(sex2 == "Male") %>% 
#  # select a random 30,000 men sample
#  sample_n(30000)

# save comparison sample
#export(to_compare, here("data", "linking_comparisons", "comparison_sample.csv"))

to_compare <- import(here("data", "linking_comparisons", "comparison_sample.csv"))

# split into list for parallel processing
to_compare <- to_compare %>% 
  # split dataset into list of rows to apply blocking funct
  split(f = .$serial2) 






### Create potential matches for both procedures

#setup parallel backend to use many processors
#cores <- detectCores()
cl <- makeCluster(32)
registerDoParallel(cl)

out1 <- foreach(x = to_compare, .combine = "rbind", .packages = c("dplyr","tibble","RecordLinkage","stringr")) %dopar% {
  build_comparisons(x, no_hh = F)
}

#stop cluster
stopCluster(cl)

# return rows without NA junk
out1 <- out1 %>% 
  filter(is.na(serial2) == F)

export(out1, here("data", "linking_comparisons", paste0("potential-matches-mine.csv")), row.names = F)





#setup parallel backend to use many processors
cl <- makeCluster(32)
registerDoParallel(cl)

out2 <- foreach(x = to_compare, .combine = "rbind", .packages = c("dplyr","tibble","RecordLinkage","stringr")) %dopar% {
  build_comparisons(x, no_hh = T)
}

#stop cluster
stopCluster(cl)

# return rows without NA junk
out2 <- out2 %>% 
  filter(is.na(serial2) == F)

export(out2, here("data", "linking_comparisons", paste0("potential-matches-feigenbaum.csv")), row.names = F)




