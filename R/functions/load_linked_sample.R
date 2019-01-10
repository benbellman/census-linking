library(dplyr)
library(rio)
library(here)

load_microdata <- function(t, formatted = T){
  if(formatted == T){
    import(here("data", "micro", paste0("Phl", t, ".csv")), fill = T) %>% 
      mutate(age = recode(age, 
                          `90 (90+ in 1980 and 1990)` = "90",
                          `100 (100+ in 1960-1970)` = "100",
                          `112 (112+ in the 1980 internal data)` = "112",
                          `115 (115+ in the 1990 internal data)` = "115",
                          `Less than 1 year old` = "0"),
             race_grp = case_when(race == "Black/Negro" ~ "Black",
                                  race == "Mulatto" ~ "Black",
                                  race == "White" ~ "White",
                                  TRUE ~ "Other"),
             uniqueid = paste(serial, pernum, year, sep = "_"),
             year = as.numeric(year),
             age = as.numeric(age))
  } else {
    import(here("data", "micro", paste0("Phl", t, "_nofmt.csv")), fill = T) %>% 
      mutate(race_grp = case_when(race == "200" ~ "Black",
                                  race == "210" ~ "Black",
                                  race == "100" ~ "White",
                                  TRUE ~ "Other"),
             uniqueid = paste(serial, pernum, year, sep = "_"))
  }
}

load_linked_sample <- function(t1, t2){
  # join two years of microdata with two-column crosswalk of links (uniqueid in each year)
  inner_join(
    # crosswalk
    import(here("data", "crosswalks", paste0("crosswalk_", t1, "_", t2, ".csv"))),
    # time 1, drop hidden vars and add "1" tag
    load_microdata(t1, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))
  # load time 2 and join
  ) %>% inner_join(
    # drop hidden vars and add "2" tag
    load_microdata(t2, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2")))
  )
}
