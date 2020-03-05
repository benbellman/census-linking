library(dplyr)
library(mclust)
library(purrr)
library(circular)

# function to return cluster of input df
get_clusters <- function(df, G = NULL){
  # return a single value for cluster if only one unique row
  if(nrow(unique(df)) == 1){
    mutate(df, cluster = 1)
    # otherwise, run the clustering algorithm
  } else {
    df$cluster <- Mclust(df, G = G)$classification
    df
  }
} 

# function to calculate an angle in degrees from due North
move_angle <- function(X1, X2, Y1, Y2){
  #atan((test$X2 - test$X1) / (test$Y2 - test$Y1)) * 180 / pi
  circular(atan2(y = Y2 - Y1, x = X2 - X1))
}


# function for defining residential flows through clusting on start/end points and direction

get_flows <- function(links){
  # filter data and attach cluster IDs
  filter(links,
         is.na(X1) == F,
         is.na(X2) == F,
         is.na(Y1) == F,
         is.na(Y2) == F
  ) -> d
  
  # if there are no valid rows to cluster, return NA
  if(nrow(d) == 0){
    return(NA)
  }
  
  # calculate origin and destination x-y clusters for slice of data
  select(d, X1, X2, Y1, Y2) %>% 
    mutate(angle = move_angle(X1, X2, Y1, Y2)) %>% 
    get_clusters() -> ids
  
  # summarize coordinates as flows
  left_join(d, ids) %>% 
    group_by(cluster) %>% 
    summarise(
      n = n(),
      sei_mean = mean(sei1),
      sei_sd = stats::sd(sei1),
      sei_var = stats::var(sei1),
      pct_rent = mean((ownershp1/10)-1)*100,
      X1 = mean(X1),
      X1_sd = stats::sd(X1),
      X2 = mean(X2),
      X2_sd = stats::sd(X2),
      Y1 = mean(Y1),
      Y1_sd = stats::sd(Y1),
      Y2 = mean(Y2),
      Y2_sd = stats::sd(Y2)
    ) %>% 
    ungroup() %>% 
    mutate(
      sei_grp = case_when(sei_mean < 20 ~ "Under 20", sei_mean >= 20 & sei_mean < 40 ~ "20 to 40", sei_mean >= 40 ~ "40 and up"),
      sei_grp = factor(sei_grp, levels = c("Under 20", "20 to 40", "40 and up"))
    )
}
