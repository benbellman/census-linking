### Function to compare household rosters of one record from each decade (one potential match)
# returns one row data frame of indicators to be combined with input row
compare_hh_rosters <- function(row){
  
  for_rosters$age <- recode(for_rosters$age, 
                      `90 (90+ in 1980 and 1990)` = "90",
                      `100 (100+ in 1960-1970)` = "100",
                      `112 (112+ in the 1980 internal data)` = "112",
                      `Less than 1 year old` = "0") %>% as.numeric()

  # grab houshold rosters
  # must remove those younger than 11 from time 2, plus household head being linked
  roster_focal <- filter(for_rosters, serial == row$serial2 &
                           age > 10 & 
                           pernum != row$pernum2)
  names(roster_focal) <- paste0(names(roster_focal), "2")
  
  roster_comp <- filter(for_rosters, serial == row$serial1 &
                          pernum != row$pernum1)
  names(roster_comp) <- paste0(names(roster_comp), "1")
  
  if(nrow(roster_focal) > 0 & nrow(roster_comp) > 0){
    # Do comparison
    combo <- all_row_combos(roster_focal, roster_comp) 
    combo <-  mutate(combo, 
                     jw_frst = jarowinkler(namefrst2, namefrst1),
                     jw_last = jarowinkler(namelast2, namelast1))
    
    # create weak and strong match columns
    # only use "relate" for matching if hh head is also head in time 1
    
    # You should relax these rule in general
    #if(row$relate1 == "Head/householder"){
    combo <- mutate(combo, fuzzy_match = if_else(jw_frst > 0.6 &
                                                     jw_last > 0.8 &
                                                     age2 <= age1 + 8 &
                                                     age2 <= age1 + 12 &
                                                     bpl2 == bpl1 &
                                                     #relate2 == relate1 & # relate var can be messy
                                                     sex2 == sex1, 1, 0),
                      exact_match = if_else(jw_frst == 1 &
                                              jw_last == 1 &
                                              age2 == age1 &
                                              bpl2 == bpl1 &
                                              #relate2 == relate1 &   # relate var can be messy
                                              sex2 == sex1, 1, 0))
    #} else {
    #  combo <- mutate(combo, fuzzy_match = if_else(jw_frst > 0.8 &
    #                                                 jw_last > 0.8 &
    #                                                 age2 >= age1 - 2 &
    #                                                 age2 <= age1 + 2 &
    #                                                 bpl2 == bpl1 &
    #                                                 sex2 == sex1, 1, 0),
    #                  exact_match = if_else(jw_frst == 1 &
    #                                          jw_last == 1 &
    #                                          age2 == age1 &
    #                                          bpl2 == bpl1 &
    #                                          sex2 == sex1, 1, 0))
    #}
    # compute indicators for potential match
    ### need to add indicators to scale probability based on number of choices
    ### This could also happen in the automated algorithm that repeats to maximize matches based on past decisions
    row$n_fuzzy <- sum(combo$fuzzy_match)
    row$pct_fuzzy <- row$n_fuzzy/nrow(roster_focal)*100
    row$n_exact <- sum(combo$exact_match)
    row$pct_exact <- row$n_exact/nrow(roster_focal)*100
    row$pct_match_int <- row$pct_fuzzy * row$pct_exact
    row$hh_size_diff <- abs(nrow(roster_focal) - nrow(roster_comp))
  } else {
    row$n_fuzzy <- 0
    row$pct_fuzzy <- 0
    row$n_exact <- 0
    row$pct_exact <- 0
    row$pct_match_int <- 0
    row$hh_size_diff <- abs(nrow(roster_focal) - nrow(roster_comp))
  }
  
  row
}



### Custom function to create df of all row combinations between 2 dfs
# based on cbind, but outputs a row for every row combo between dfs
# used in compare_hh_rosters()
all_row_combos <- function(df1, df2){
  bind_rows(purrr::map2(split(df1, row.names(df1)), list(rep(df2, nrow(df1))), cbind, row.names = NULL, stringsAsFactors = F))
}

