library(dplyr)
library(RecordLinkage)

# create a block of potential matches for a single focal record (household head in t2)
build_comparisons <- function(focal_record, no_hh = F){
  
  # define thresholds for record being considered
  byearmin <- focal_record$byear2 - 2
  byearmax <- focal_record$byear2 + 2
  sex <- focal_record$sex2
  bpl <- focal_record$bpl2
  namefrst <- focal_record$namefrst2
  namelast <- focal_record$namelast2
  
  # create initial block of possible matches
  block <- filter(m1, byear1 <= byearmax & byear1 >= byearmin & sex1 == sex & bpl1 == bpl) %>% 
    # calculate string similarities
    mutate(jw_frst = jarowinkler(namefrst1, namefrst),
           jw_last = jarowinkler(namelast1, namelast)) %>% 
    # limit block based on JW scores
    filter(jw_frst >= 0.75 & jw_last >= 0.8) %>%
    arrange(desc(jw_frst), desc(jw_last))
  
  if(nrow(block) > 0){
    if(no_hh == T){
      cbind(focal_record, block) %>% 
        # create matching variables for prediction
        add_matching_vars(predict = T, no_hh = T)
    } else {}
      cbind(focal_record, block) %>% 
        # create matching variables for prediction
        add_matching_vars(predict = T)
  } else {
    NA 
  }
}


