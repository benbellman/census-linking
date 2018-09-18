### Function to compare household rosters of one record from each decade (one potential match)
# returns one row data frame of indicators to be combined with input row
compare_hh_rosters <- function(row){
  
  # grab houshold rosters
  # must remove those younger than 11 from time 2, plus household head being linked
  roster_focal <- filter(phl40, serial40 == row$serial40 &
                           age40 > 10 & 
                           pernum40 != row$pernum40)
  
  roster_comp <- filter(phl30, serial30 == row$serial30 &
                          pernum30 != row$pernum30)
  
  if(nrow(roster_focal) > 0 & nrow(roster_comp) > 0){
    # Do comparison
    combo <- all_row_combos(roster_focal, roster_comp) 
    combo <-  mutate(combo, jw_frst = jarowinkler(namefrst40, namefrst30),
                     jw_last = jarowinkler(namelast40, namelast30))
    
    # create weak and strong match columns
    # only use "relate" for matching if hh head is also head in time 1
    
    # You should relax these rule in general
    if(row$relate30 == 101){
      combo <- mutate(combo, fuzzy_match = if_else(jw_frst > 0.6 &
                                                     jw_last > 0.8 &
                                                     byear40 >= byear30 - 2 &
                                                     byear40 <= byear30 + 2 &
                                                     bpl40 == bpl30 &
                                                     #relate40 == relate30 & # relate var can be messy
                                                     sex40 == sex30, 1, 0),
                      exact_match = if_else(jw_frst == 1 &
                                              jw_last == 1 &
                                              byear40 == byear30 &
                                              bpl40 == bpl30 &
                                              #relate40 == relate30 &   # relate var can be messy
                                              sex40 == sex30, 1, 0))
    } else {
      combo <- mutate(combo, fuzzy_match = if_else(jw_frst > 0.8 &
                                                     jw_last > 0.8 &
                                                     byear40 >= byear30 - 2 &
                                                     byear40 <= byear30 + 2 &
                                                     bpl40 == bpl30 &
                                                     sex40 == sex30, 1, 0),
                      exact_match = if_else(jw_frst == 1 &
                                              jw_last == 1 &
                                              byear40 == byear30 &
                                              bpl40 == bpl30 &
                                              sex40 == sex30, 1, 0))
    }
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

