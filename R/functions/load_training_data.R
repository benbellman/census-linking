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
      
      # Next I need to retool the functions:
      # add_matching_vars()
      # compare_hh_rosters()
    )
}