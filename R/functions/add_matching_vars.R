add_matching_vars <- function(rows){
  
  ## Calculate all of Feigenbaum's (2016) indicators
  rows <- dplyr::mutate(rows,
                        hits = n(),
                        hits2 = hits^2,
                        # let's make sure the jaro-winkler scores are calculated
                        # the rest of the needed variables are calculated before two years are joined
                        jw_frst = jarowinkler(namefrst40, namefrst30),
                        jw_last = jarowinkler(namelast40, namelast30),
                        # difference in birth years
                        ydiff = abs(byear30 - byear40),
                        # exact matches across names (and birth year)
                        exact = if_else(namefrst30 == namefrst40 & namelast30 == namelast40, 1, 0),
                        exact_all = if_else(exact == 1 & ydiff == 0, 1, 0),
                        # do first and last letters of names match?
                        f_start = if_else(str_sub(namefrst30, 1, 1) == str_sub(namefrst40, 1, 1), 1, 0),
                        l_start = if_else(str_sub(namelast30, 1, 1) == str_sub(namelast40, 1, 1), 1, 0),
                        f_end = if_else(str_sub(namefrst30, -1, -1) == str_sub(namefrst40, -1,- 1), 1, 0),
                        l_end = if_else(str_sub(namelast30, -1, -1) == str_sub(namelast40, -1, -1), 1, 0),
                        # do middle initials match?
                        #mimatch_no_na = if_else(mi40 == mi30 &
                        #                          is.na(mi40) == F &
                        #                          is.na(mi30) == F, 1, 0),
                        #midiff_no_na = if_else(mi40 != mi30 &
                        #                         is.na(mi40) == F &
                        #                         is.na(mi30) == F, 1, 0),
                        # check for exact matches within potential matches for focal record
                        exact_mult = if_else(sum(exact) > 0, 1, 0),
                        exact_mult = if_else(sum(exact) > 0, 1, 0),
                        # create soundex codes
                        soundex_f30 = soundex(namefrst30),
                        soundex_f40 = soundex(namefrst40),
                        soundex_l30 = soundex(namelast30),
                        soundex_l40 = soundex(namelast40),
                        # check soundex matches
                        fsoundex = if_else(soundex_f30 == soundex_f40, 1, 0),
                        lsoundex = if_else(soundex_l30 == soundex_l40, 1, 0),
                        # other household head indicators
                        not_head_t1 = if_else(relate30 != 101, 1, 0),
                        mar_consistent = if_else(marst30 == marst40, 1, 0)) #same marriage status
  
  
  ## Create new variables for household indicators
  # see code for comparison function below
  bind_rows(purrr::map(split(rows, row.names(rows)), compare_hh_rosters))
}