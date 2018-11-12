library(dplyr)
library(tibble)
library(rio)

prepare_city <- function(filepath){
  city <- filepath %>% 
    import() %>% 
    as_tibble() 
  
  city$age <- recode(city$age, 
                     `90 (90+ in 1980 and 1990)` = "90",
                     `100 (100+ in 1960-1970)` = "100",
                     `112 (112+ in the 1980 internal data)` = "112",
                     `115 (115+ in the 1990 internal data)` = "115",
                     `Less than 1 year old` = "0")
  
  city %>% 
    select(serial, pernum, namefrst, namelast, bpl, age, sex, relate, year, marst) %>% 
    mutate(uniqueid = paste(serial, pernum, year, sep = "_"),
           year = as.numeric(year),
           age = as.numeric(age),
           byear = year - age,
           namefrst = str_to_upper(namefrst),
           namefrst = if_else(is.na(namefrst), "", namefrst),
           namefrst = unabbv_names(namefrst),
           mi = str_replace(str_extract(namefrst, " [A-Z] ?[A-Z]?$"), " ", ""),
           namefrst = str_replace(namefrst, " [A-Z] ?[A-Z]?$", ""),
           namelast = str_to_upper(namelast),
           namelast = if_else(is.na(namelast), "", namelast),
           namelast = unabbv_names(namelast),
           namelast = str_replace_all(namelast, " ", ""),
           namelast = str_replace_all(namelast, "'", ""))
}