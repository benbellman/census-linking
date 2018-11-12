library(dplyr)
library(RecordLinkage)

### Function to compare household rosters of one record from each decade (one potential match)
# returns one row data frame of indicators to be combined with input row
compare_hh_rosters <- function(row, predict = T){
  
  # grab houshold rosters
  # must remove those younger than 11 from time 2, plus household head being linked
  if(predict == F){
    for_rosters$age <- recode(for_rosters$age, 
                              `90 (90+ in 1980 and 1990)` = "90",
                              `100 (100+ in 1960-1970)` = "100",
                              `112 (112+ in the 1980 internal data)` = "112",
                              `115 (115+ in the 1990 internal data)` = "115",
                              `Less than 1 year old` = "0") %>% as.numeric()
    
    roster_focal <- filter(for_rosters, serial == row$serial2 &
                             age > 10 & 
                             pernum != row$pernum2)
    names(roster_focal) <- paste0(names(roster_focal), "2")
    
    roster_comp <- filter(for_rosters, serial == row$serial1 &
                            pernum != row$pernum1)
    names(roster_comp) <- paste0(names(roster_comp), "1")
  } else {
    roster_focal <- m2 %>% 
      select(namefrst2, namelast2, age2, sex2, relate2, bpl2, year2, serial2, pernum2) %>% 
      filter(serial2 == row$serial2 &
               age2 > 8 & 
               pernum2 != row$pernum2)
    
    
    roster_comp <- m1 %>% 
      select(namefrst1, namelast1, age1, sex1, relate1, bpl1, year1, serial1, pernum1) %>% 
      filter(serial1 == row$serial1 &
               pernum1 != row$pernum1)
  }

  
  if(nrow(roster_focal) > 0 & nrow(roster_comp) > 0){
    # Do comparison
    combo <- all_row_combos(roster_focal, roster_comp) 
    combo <-  mutate(combo, 
                     jw_frst = jarowinkler(namefrst2, namefrst1),
                     jw_last = jarowinkler(namelast2, namelast1),
                     # use "point" system for each dimension records can match on
                     frst_pt = if_else(jw_frst > 0.8, 1, 0),
                     last_pt = if_else(jw_last > 0.8, 1, 0),
                     age_pt = if_else(age2 >= age1 + 8 & age2 <= age1 + 12, 1, 0),
                     bpl_pt = if_else(bpl2 == bpl1, 1, 0),
                     sex_pt = if_else(sex2 == sex1, 1, 0),
                     relate_pt = relate_pt(relate1, relate2, row$relate1, row$relate2),
                     # identify a match if 4 of 5 points
                     points = frst_pt + last_pt + age_pt + bpl_pt + sex_pt + relate_pt,
                     match = if_else(points >= 5, 1, 0))
    row$n_match <- sum(combo$match)
    row$pct_match <- row$n_match/nrow(roster_focal)*100
    row$hh_size_diff <- abs(nrow(roster_focal) - nrow(roster_comp))
    row$avg_jw_frst <- sum(combo$jw_frst) / nrow(combo)
    #row$avg_jw_last <- sum(combo$jw_last) / nrow(combo)
  } else {
    row$n_match <- 0
    row$pct_match <- 0
    row$hh_size_diff <- abs(nrow(roster_focal) - nrow(roster_comp))
    row$avg_jw_frst <- 0
    #row$avg_jw_last <- 0
  }
  row
}


