library(dplyr)
library(caret)
library(doParallel)
library(foreach)
library(purrr)

# Define a function to apply to block of rows based on focal record
# When applying to final links, need to make sure that records in t2 are never matched to the same record in t1
# keep only the highest match score, explore if I can make a reasonable guess without rematching
get_match <- function(block, model, b1, b2){
  # instructions for more than one possible match
  if(nrow(block) > 1){
    # predict probabilities of match
    block$match_prob <- predict(model, block, type = "prob")$Yes
    
    # set the best and second-best match scores
    best_prob <- sort(block$match_prob, decreasing = T)[1]
    next_best_prob <- sort(block$match_prob, decreasing = T)[2]
    
    # define match
    block %>% 
      mutate(auto_match = if_else(match_prob == best_prob &
                               match_prob > b1 &
                               best_prob / next_best_prob > b2, 1, 0))
    # instructions if only one possible match
  } else {
    # predict probabilities of match
    block$match_prob <- predict(model, block, type = "prob")$Yes
    # define match
    block %>% 
      mutate(auto_match = if_else(match_prob > b1, 1, 0))
  }
}



get_match_grid_search <- function(b1, b2, block, model){
  # instructions for more than one possible match
  if(nrow(block) > 1){
    # predict probabilities of match
    block$match_prob <- predict(model, block, type = "prob")$Yes
    
    # set the best and second-best match scores
    best_prob <- sort(block$match_prob, decreasing = T)[1]
    next_best_prob <- sort(block$match_prob, decreasing = T)[2]
    
    # define match
    block %>% 
      mutate(auto_match = if_else(match_prob == best_prob &
                                    match_prob > b1 &
                                    best_prob / next_best_prob > b2, 1, 0))
    # instructions if only one possible match
  } else {
    # predict probabilities of match
    block$match_prob <- predict(model, block, type = "prob")$Yes
    # define match
    block %>% 
      mutate(auto_match = if_else(match_prob > b1, 1, 0))
  }
}


# function to define matches in full dataset
define_matches <- function(b1, b2, data, vars, model){
  data %>%
    split(.$serial2) %>% 
    map(select, vars) %>% 
    map(get_match, model, b1, b2) %>% 
    bind_rows()
}

# function to wrap around full dataset
#define_matches <- function(b1, b2, data, model, cores = 3){
#  blocked <- data %>%
#    split(.$serial2) %>% 
#    map(select, vars[-1])
  
  #setup parallel backend to use many processors
  #cores <- detectCores()
#  cl <- makeCluster(cores) 
#  registerDoParallel(cl)
  
#  out <- foreach(x = blocked, .combine = "rbind", .packages = c("dplyr","tibble","RecordLinkage","stringr", "caret")) %dopar% {
#    get_match(x, model, b1, b2)
#  }
  
  #stop cluster
#  stopCluster(cl)
  
  # return result
#  out
#}