# rows = object of formatted microdata for one city
# unit = variable containing spatial unit information, no quotes!
# group_var = name of variable containing group information, no quotes!
# group1 = quoted text for group from independently created variable
# group2 = quoted text for group from independently created variable
diss <- function(rows, unit, group_var, group1, group2){
  # set quosures
  quo_unit <- enquo(unit)
  quo_group_var <- enquo(group_var)
  
  # calculate group counts by specified unit ID
  counts <- as_tibble(rows) %>%
    mutate(unit = as.character(!!quo_unit), group_var = as.character(!!quo_group_var)) %>% 
    filter(group_var == group1 | group_var == group2) %>%
    group_by(unit, group_var) %>%
    summarise(count = n()) %>%
    #mutate(count = replace_na(count, 0)) %>% 
    melt() %>%
    #rename(unit = !!quo_unit, group_var = !!quo_group_var) %>% 
    dcast(unit ~ group_var + variable)
  
  # fix missing values
  counts[[2]] <- ifelse(is.na(counts[[2]]), 0, counts[[2]])
  counts[[3]] <- ifelse(is.na(counts[[3]]), 0, counts[[3]])
  
  #counts
  
  # caluclate dissimilarity
  round(sum(abs(counts[[paste0(group1, "_count")]]/sum(counts[[paste0(group1, "_count")]])-counts[[paste0(group2, "_count")]]/sum(counts[[paste0(group2, "_count")]])))*0.5, 3)
}


pstar <- function(rows, unit, group_var, group1, group2){
  # set quosures
  quo_unit <- enquo(unit)
  quo_group_var <- enquo(group_var)
  
  # calculate group counts by specified unit ID
  counts <- as_tibble(rows) %>%
    mutate(unit = as.character(!!quo_unit), group_var = as.character(!!quo_group_var)) %>% 
    group_by(unit) %>% 
    mutate(unit_total = n()) %>% 
    filter(group_var %in% c(group1, group2)) %>%
    group_by(unit, group_var) %>%
    summarise(count = n(), unit_total = max(unit_total)) %>%
    melt() %>%
    dcast(unit ~ group_var + variable)
  
  # drop any units where both groups are missing and fix missing values
  if(group1 != group2) {
    counts <- counts[(is.na(counts[[2]]) == F) | (is.na(counts[[4]]) == F),]
    counts[[2]] <- ifelse(is.na(counts[[2]]), 0, counts[[2]])
    counts[[3]] <- ifelse(is.na(counts[[3]]), counts[[5]], counts[[3]])
    counts[[4]] <- ifelse(is.na(counts[[4]]), 0, counts[[4]])
    counts[[5]] <- ifelse(is.na(counts[[5]]), counts[[3]], counts[[5]])
  }
  
  sum((counts[[paste0(group1, "_count")]] / sum(counts[[paste0(group1, "_count")]])) * (counts[[paste0(group2, "_count")]] / counts[[paste0(group1, "_unit_total")]])) %>%
    round(3)
}