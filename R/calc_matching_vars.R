library(tidyverse)
library(here)
library(RecordLinkage)

# function for adding matching variables
# input is a single potential match (data frame with one row)
# the data from both decades must be in memory to grab household members


#### NOTES

# It will be a challenge to standardize names based on years across decade files
# before anything is run, I'll probably need to transform names into something standard
# then transform it back afterwards for when I construct the final database for all years

# Important! Some vars need to be calculated in the larger data table, not in isolation
# "hits", "


# load the matched training data to work with
training_set <- read_csv("training_prelim_v1_1.csv")

# calculate "hits"
# this will be done with person-year ID in later version
training_set <- training_set %>% 
  group_by(namefrst40, namelast40) %>% 
  dplyr::mutate(hits = n(),
                hits2 = hits^2) %>% 
  ungroup()

# create middle initial and remove from first name
#training_set <- mutate(training_set,
#                       mi30 = str_replace(str_extract(namefrst30, " [A-Z] ?[A-Z]?$"), " ", ""),
#                       mi40 = str_replace(str_extract(namefrst40, " [A-Z] ?[A-Z]?$"), " ", ""))

#training_set$namefrst30 <- str_replace(training_set$namefrst30, " [A-Z] ?[A-Z]?$", "")
#training_set$namefrst40 <- str_replace(training_set$namefrst40, " [A-Z] ?[A-Z]?$", "")

# turn df into list of dfs based on target record (here 1940)
# this is based on name here but will be based on record ID later
# this will be looped over in parallel
training_set_ls <- split(training_set, training_set$uniqueid40)

# make a test row
rows <- training_set_ls[[1]]


# Function below
# required libraries: dplyr, stringr, RecordLinkage, 

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
                        mimatch_no_na = if_else(mi40 == mi30 &
                                                  is.na(mi40) == F &
                                                  is.na(mi30) == F, 1, 0),
                        midiff_no_na = if_else(mi40 != mi30 &
                                                 is.na(mi40) == F &
                                                 is.na(mi30) == F, 1, 0),
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
all_row_combos <- function(df1, df2){
  bind_rows(purrr::map2(split(df1, row.names(df1)), list(rep(df2, nrow(df1))), cbind, row.names = NULL, stringsAsFactors = F))
}



#test the roster compare
row <- slice(training_set_ls[[1]], 1)
row <- split(rows, row.names(rows))[[1]]
row <- slice(training_set, 46)

out <- compare_hh_rosters(row)
out[34:38]



# testing variables function on training data

results <- add_matching_vars(training_set_ls[[1]])
results <- bind_rows(purrr::map(training_set_ls, add_matching_vars))

write_csv(results, "training_prelim_v1_1_full.csv")


####### 
# code below is being temporarily copied from other scripts to be edited here


library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

results <- foreach(x = ls, .combine = "rbind", .packages = c("sp","rgeos","spatstat","raster")) %dopar% {
  get_seg_results(x[1], x[2], x[3])
}
#stop cluster
stopCluster(cl)




