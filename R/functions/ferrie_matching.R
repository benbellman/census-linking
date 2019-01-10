library(dplyr)
library(phonics)

# functions for Ferrie 1996 matching procedure

build_comparisons_ferrie <- function(focal_record){
  nysiis2 <- focal_record$namelast_nysiis2
  trunc2 <- focal_record$namefrst_trunc2
  #bpl2 <- focal_record$bpl2
  
  block <- filter(m1, 
                  namelast_nysiis1 == nysiis2,
                  namefrst_trunc1 == trunc2)
  
  if(nrow(block) > 0 & nrow(block) < 11){
    cbind(focal_record, block)
  } else {
    NA
  }
}


eliminate_links_ferrie <- function(block){
  block$agediff <- block$age2 - block$age1
  block$agediff_to_10 <- abs(10 - block$agediff)
  block$n_hh_match <- hh_match_ferrie(block$uniqueid2, block$serial2, block$uniqueid1, block$serial1)
  
  keep <- block %>% 
    mutate(agediff = age2 - age1,
           agediff_to_10 = abs(10 - agediff),
           n_hh_match = hh_match_ferrie(uniqueid2, serial2, uniqueid1, serial1)) %>% 
    filter(between(agediff, 5, 15) &
             bpl1 == bpl2 &
             n_hh_match > 1)
  
  # if more than one option, keep most hh matches
  if (nrow(keep) > 1){
    keep <- filter(keep, n_hh_match == max(n_hh_match))
  }
  
  # if still more than one option, keep closest to 10 year age gap
  if (nrow(keep) > 1){
    keep <- filter(keep, agediff_to_10 == min(agediff_to_10))
  }
  
  # if still not only 1 option (or now no options), return nothing
  if (nrow(keep) == 1){
    keep
  } else {
    NA
  }
}



hh_match_ferrie <- function(id2, ser2, id1, ser1){
  roster1 <- filter(m1, serial1 == ser1 & 
                      uniqueid1 != id1) %>% 
    mutate(#nysiis1 = nysiis(namelast1),
           #trunc1 = toupper(substr(namefrst1, 1, 4)),
           ferrie_name1 = paste(namefrst_trunc1, namelast_nysiis1, sep = " "))
  roster2 <- filter(m2, serial2 == ser2 & 
                      uniqueid2 != id2 &
                      age2 > 10) %>% 
    mutate(nysiis2 = nysiis(namelast2),
           trunc2 = toupper(substr(namefrst2, 1, 4)),
           ferrie_name2 = paste(trunc2, nysiis2, sep = " "))
  
  if_else(nrow(roster2) == 0 & nrow(roster1) > 0, 0, as.numeric(length(intersect(roster1$ferrie_name1, roster2$ferrie_name2))))
}

