add_matching_vars <- function(rows){
  
  ## Calculate all of Feigenbaum's (2016) indicators
  rows <- dplyr::mutate(rows,
                        hits = n(),
                        hits2 = hits^2,
                        # let's make sure the jaro-winkler scores are calculated
                        # the rest of the needed variables are calculated before two years are joined
                        jw_frst = jarowinkler(namefrst2, namefrst1),
                        jw_last = jarowinkler(namelast2, namelast1),
                        # difference in birth years
                        ydiff = abs(byear1 - byear2),
                        # exact matches across names (and birth year)
                        exact = if_else(namefrst1 == namefrst2 & namelast1 == namelast2, 1, 0),
                        exact_all = if_else(exact == 1 & ydiff == 0, 1, 0),
                        # do first and last letters of names match?
                        f_start = if_else(str_sub(namefrst1, 1, 1) == str_sub(namefrst2, 1, 1), 1, 0),
                        l_start = if_else(str_sub(namelast1, 1, 1) == str_sub(namelast2, 1, 1), 1, 0),
                        f_end = if_else(str_sub(namefrst1, -1, -1) == str_sub(namefrst2, -1,- 1), 1, 0),
                        l_end = if_else(str_sub(namelast1, -1, -1) == str_sub(namelast2, -1, -1), 1, 0),
                        # do middle initials match?
                        #mimatch_no_na = if_else(mi2 == mi1 &
                        #                          is.na(mi2) == F &
                        #                          is.na(mi1) == F, 1, 0),
                        #midiff_no_na = if_else(mi2 != mi1 &
                        #                         is.na(mi2) == F &
                        #                         is.na(mi1) == F, 1, 0),
                        # check for exact matches within potential matches for focal record
                        exact_mult = if_else(sum(exact) > 0, 1, 0),
                        exact_mult = if_else(sum(exact) > 0, 1, 0),
                        # create soundex codes
                        soundex_f1 = soundex(namefrst1),
                        soundex_f2 = soundex(namefrst2),
                        soundex_l1 = soundex(namelast1),
                        soundex_l2 = soundex(namelast2),
                        # check soundex matches
                        fsoundex = if_else(soundex_f1 == soundex_f2, 1, 0),
                        lsoundex = if_else(soundex_l1 == soundex_l2, 1, 0),
                        # other household head indicators
                        not_head_t1 = if_else(relate1 != "Head/householder", 1, 0),
                        mar_consistent = if_else(marst1 == marst2, 1, 0)) #same marriage status
  
  
  ## Create new variables for household indicators
  # see code for comparison function below
  bind_rows(purrr::map(split(rows, row.names(rows)), compare_hh_rosters))
}