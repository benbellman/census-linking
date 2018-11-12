library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(rio)
library(here)
library(caret)

for(a in list.files(here("R", "functions"), full.names = T)){
  source(a)
}

#for(a in c(10, 20, 30)){
#for(a in c(20, 30)){
  
t1 <- 30
t2 <- t1 + 10

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
potential <- import(here("data", paste0("potential_matches_", t1, "_", t2, ".csv"))) %>% 
  as_tibble()

# automated math predictiong using trained probit coefficients ---
#results <- define_matches(b1 = 0.2, b2 = 1.57, data = full1020, model = glm2, cores = 32)

blocked <- potential %>%
  #filter(serial2 %in% unique(full1020$serial2)[1:100]) %>% 
  split(.$serial2) 

#setup parallel backend to use many processors
#cores <- detectCores()
cl <- makeCluster(32) 
#cl <- makeCluster(3) 
registerDoParallel(cl)

results <- foreach(x = blocked, .combine = "rbind", .packages = c("dplyr","tibble","RecordLinkage","stringr","caret")) %dopar% {
  get_match(x, glm2, b1 = 0.2, b2 = 1.57)
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
export(results, here("data", paste0("potential_matches_linked_", t1, "_", t2, ".csv")))

}


