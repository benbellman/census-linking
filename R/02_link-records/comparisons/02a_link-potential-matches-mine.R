library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(rio)
library(here)
library(caret)
library(foreach)
library(doParallel)

source(here("R", "functions", "build_comparisons.R"))
source(here("R", "functions", "add_matching_vars.R"))
source(here("R", "functions", "prepare_city.R"))
source(here("R", "functions", "unabbv_names.R"))
source(here("R", "functions", "compare_hh_rosters.R"))
source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "relate_pt.R"))
source(here("R", "functions", "get_match.R"))

#for(a in c(10, 20, 30)){
#for(a in c(20, 30)){

#t1 <- 30
#t2 <- t1 + 10

# load pre-preprocessed training data
full_data <- import(here("data", "training_all_vars_v3.csv")) %>% 
  as_tibble()

# save outcome as factor
full_data$match <- as.factor(full_data$match) %>% 
  recode_factor(`0` = "No",
                `1` = "Yes")

# grab names of features in the models
vars <- c("match", # variable to predict
          # name indicators
          #"exact", 
          "exact_all", 
          "f_start", "l_start", "f_end", "l_end",
          "jw_frst", "jw_last", 
          "fsoundex", "lsoundex",
          # indicators based on other potential matches
          "hits", "hits2",
          # indicators based on household comparisons
          "n_match", "pct_match", "hh_size_diff", "avg_jw_frst")

# retrain model on full training set, name as glm2
full_training <- full_data[vars]

set.seed(123)

# Train model using 25 bootstrap samples
fitControl <- trainControl(
  method = "boot",
  number = 25,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train model with all data
glm2 <- train(
  match ~ .,
  data = full_training,
  method = "glm",
  family = binomial,
  trControl = fitControl
)

# load potential matches
# read in the full 10-20 data set for predicting real matches
potential <- import(here("data", "linking_comparisons", paste0("potential-matches-mine.csv"))) %>% 
  as_tibble()


blocked <- potential %>%
  #filter(serial2 %in% unique(full1020$serial2)[1:100]) %>% 
  split(.$serial2) 

for(b1 in c(0.1, 0.2, 0.3)){
  for(b2 in c(1.25, 1.5, 1.75)){
    
    #setup parallel backend to use many processors
    #cores <- detectCores()
    cl <- makeCluster(32) 
    #cl <- makeCluster(3) 
    registerDoParallel(cl)
    
    results <- foreach(x = blocked, .combine = "rbind", .packages = c("dplyr","tibble","RecordLinkage","stringr","caret")) %dopar% {
      get_match(x, glm2, b1 = b1, b2 = b2)
    }
    
    #stop cluster
    stopCluster(cl)
    
    
    #results <- map(blocked, get_match, model = glm2, b1 = 0.2, b2 = 1.57) #%>% bind_rows()
    
    
    # still need to check that records from t1 are only linked with one record in t2
    check_double_match <- results %>% 
      group_by(serial1) %>% 
      summarise(total_matches = sum(auto_match)) %>% 
      filter(total_matches > 1)
    
    # undo any t1 double matches, can't be sure which one is the true match
    if(nrow(check_double_match) > 0){
      results[results$serial1 %in% check_double_match$serial1, "auto_match"] <- 0
    }
    
    # export file
    export(results, here("data", "linking_comparisons", paste0("potential-matches-mine-linked-", b1, "-", b2, ".csv")))
    
    # export crosswalk
    cw <- results %>% 
      filter(auto_match == 1) %>% 
      select(uniqueid2, uniqueid1)
    
    export(cw, here("data", "linking_comparisons", "crosswalks", paste0("mine_crosswalk-", b1, "-", b2, ".csv")))
    
    
  }
}




#}


