library(tidyverse)
library(rio)
library(here)

for(a in list.files(here("R", "functions"), full.names = T)){
  source(a)
}

# testing the first version of auto links between 1910 and 1920

m1 <- import(here("data", "Phl10.csv")) %>% 
  mutate(age = recode(age, 
                      `90 (90+ in 1980 and 1990)` = "90",
                      `100 (100+ in 1960-1970)` = "100",
                      `112 (112+ in the 1980 internal data)` = "112",
                      `115 (115+ in the 1990 internal data)` = "115",
                      `Less than 1 year old` = "0"),
         uniqueid = paste(serial, pernum, year, sep = "_"),
         year = as.numeric(year),
         age = as.numeric(age))
names(m1) <- paste0(names(m1), "1")

m2 <- import(here("data", "Phl20.csv")) %>% 
  mutate(age = recode(age, 
                      `90 (90+ in 1980 and 1990)` = "90",
                      `100 (100+ in 1960-1970)` = "100",
                      `112 (112+ in the 1980 internal data)` = "112",
                      `115 (115+ in the 1990 internal data)` = "115",
                      `Less than 1 year old` = "0"),
         uniqueid = paste(serial, pernum, year, sep = "_"),
         year = as.numeric(year),
         age = as.numeric(age))
names(m2) <- paste0(names(m2), "2")

# Load cross-walk to merge records
cw <- import(here("data", "crosswalks", paste0("crosswalk_10_20.csv")))

linked <- cw %>% 
  inner_join(m1) %>% 
  inner_join(m2)

#part <- select(linked, namefrst1, namelast1, namefrst2, namelast2)

# compare race and age compositions of sample and full hh head population in 1920
m2_toplot <- m2 %>% 
  filter(relate2 == "Head/householder") %>% 
  filter(race2 %in% c("Black/Negro", "Mulatto", "White")) %>% 
  mutate(race2 = recode(race2,
                        "Black/Negro" = "Black",
                        "Mulatto" = "Black"),
         source = "Population") %>% 
  select(race2, sex2, age2, source)

linked_toplot <- linked %>% 
  filter(race2 %in% c("Black/Negro", "Mulatto", "White")) %>% 
  mutate(race2 = recode(race2,
                        "Black/Negro" = "Black",
                        "Mulatto" = "Black"),
         source = "Linked Sample") %>% 
  select(race2, sex2, age2, source)

toplot <- bind_rows(m2_toplot, linked_toplot)

# compare age distributions across race/sex intersections
ggplot(toplot) + 
  geom_density(aes(x = age2, fill = source), alpha = 0.5) +
  facet_grid(sex2 ~ race2)

# compare racial and sex representation
toplot %>% 
  group_by(source, race2, sex2) %>% 
  summarise(count = n()) %>% 
  group_by(source) %>% 
  mutate(n_source = sum(count),
         pct = count / n_source)
  
