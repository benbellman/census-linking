library(here)
library(RecordLinkage)

source(here("R", "functions", "unabbv_names.R"))

# input is the directory holding all the manually coded training files
load_training_data <- function(dir){
  
  # Combine all csv files
  training_set <- list.files(dir, full.names = T) %>% 
    map(read_csv) %>% 
    bind_rows() %>% 
    
    # Make some changes to data
    mutate(
      
      # convert columns to numeric data
      age1 = as.numeric(age1),
      age2 = as.numeric(age2),
      year1 = as.numeric(year1),
      year2 = as.numeric(year2),
      byear1 = as.numeric(byear1),
      byear2 = as.numeric(byear2),
      
      # replace missing name cells with empty strings
      namefrst1 = if_else(is.na(namefrst1), "", namefrst1),
      namefrst2 = if_else(is.na(namefrst2), "", namefrst2),
      
      # replace name abbrevaitions
      namefrst1 = unabbv_names(namefrst1),
      namefrst2 = unabbv_names(namefrst2),
      
      # re-calculate first name JW score
      jw_frst = jarowinkler(namefrst1, namefrst2),
  
      # drop spaces and apostrophes in last names
      namelast1 = str_remove_all(namelast1, " "),
      namelast1 = str_remove_all(namelast1, "'"),
      namelast2 = str_remove_all(namelast2, " "),
      namelast2 = str_remove_all(namelast2, "'"),
      
      # re-calculate last name JW score
      jw_last = jarowinkler(namelast1, namelast2)
      
      # Still need to retool the functions:
      # add_matching_vars()
      # compare_hh_rosters()
    ) %>% 
    split(.$serial2) %>% 
    map(add_matching_vars) %>% 
    bind_rows()
}