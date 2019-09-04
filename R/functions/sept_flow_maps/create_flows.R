library(dplyr)
library(mclust)
library(purrr)


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


# custom function for selecting linked households and classifying moves
# extra arguments are passed to filter to limit households being clustered
get_flows <- function(links, ...){
  # collect category info from data
  y <- unique(links$year1)
  r <- unique(links$race_cat)
  c1 <- unique(links$custom1)
  c2 <- unique(links$custom2)
  
  # filter data and attach cluster IDs
  filter(links,
         is.na(x1) == F,
         is.na(x2) == F,
         is.na(y1) == F,
         is.na(y2) == F#,
         #...
  ) -> d
  
  # if there are no valid rows to cluster, return NA
  if(nrow(d) == 0){
    return(NA)
  }
  
  # calculate origin and destination x-y clusters for slice of data
  select(d, x1, y1) %>% 
    get_clusters() %>% 
    rename(cluster1 = cluster) -> ids1
  
  select(d, x2, y2) %>% 
    get_clusters() %>% 
    rename(cluster2 = cluster) -> ids2
  
  # collapse moves into data describing clusters, return result
  d %>% 
    left_join(ids1) %>% 
    left_join(ids2) %>% 
    select(serial1, cluster1, cluster2, x1, x2, y1, y2, sei1, ownershp1) %>% 
    unique() %>% 
    group_by(cluster1, cluster2) %>% 
    summarise(
      n = n(),
      mean_sei = mean(sei1),
      pct_rent = mean((ownershp1/10)-1)*100,
      x1 = mean(x1),
      x2 = mean(x2),
      y1 = mean(y1),
      y2 = mean(y2)
    ) %>% 
    ungroup() %>% 
    mutate(sei_grp = case_when(mean_sei < 20 ~ "Under 20", mean_sei >= 20 & mean_sei < 40 ~ "20 to 40", mean_sei >= 40 ~ "40 and up"),
           sei_grp = factor(sei_grp, levels = c("Under 20", "20 to 40", "40 and up")),
           year1 = y,
           race_cat = r,
           custom1 = c1,
           custom2 = c2) %>% 
    filter(n > 5)
}