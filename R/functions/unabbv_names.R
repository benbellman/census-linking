library(stringr)

# changes some male first names from abbreviations to the full name for comparison
# Middle initials shoudld already be placed in their own column
# text should be all upper case

unabbv_names <- function(names){
  names %>% 
    str_replace("^WM$", "WILLIAM") %>% 
    str_replace("^CHAS$", "CHARLES") %>% 
    str_replace("^JOS$", "JOSEPH") %>% 
    str_replace("^THOS$", "THOMAS") %>% 
    str_replace("^GEO$", "GEORGE")
}
