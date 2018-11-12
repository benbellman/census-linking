library(dplyr)
library(stringr)
library(tibble)
library(RecordLinkage)
library(doParallel)
library(foreach)
library(rio)
library(here)

for(a in list.files(here("R", "functions"), full.names = T)){
  source(a)
}


#for(a in c(10, 20, 30)){
for(a in c(20, 30)){

# set years of data to use
t1 <- a
t2 <- a + 10

# load files for each decade
m1 <- paste0("Phl", as.character(t1), ".csv") %>% 
  here("data", .) %>% 
  prepare_city()

m2 <- paste0("Phl", as.character(t2), ".csv") %>% 
  here("data", .) %>% 
  prepare_city()

names(m1) <- paste0(names(m1), "1")
names(m2) <- paste0(names(m2), "2")

## create potential matches

# select household heads from t2
to_compare <- m2 %>% 
  filter(relate2 == "Head/householder") %>% 
  # split dataset into list of rows to apply blocking funct
  split(f = .$serial2) 

#setup parallel backend to use many processors
#cores <- detectCores()
cl <- makeCluster(32)
registerDoParallel(cl)

out <- foreach(x = to_compare, .combine = "rbind", .packages = c("dplyr","tibble","RecordLinkage","stringr")) %dopar% {
  build_comparisons(x)
}

#stop cluster
stopCluster(cl)

# return rows without NA junk
out <- out %>% 
  filter(is.na(serial2) == F)

export(out, here("data", paste0("potential_matches_", t1, "_", t2, ".csv")), row.names = F)


}



