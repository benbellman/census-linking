library(rio)
library(here)
library(dplyr)
library(ggplot2)

source(here("R", "functions", "load_linked_sample.R"))


#### commparing sample distributions

f_03_175 <- here("data", "linking_comparisons", "crosswalks", "feigenbaum_crosswalk-0.3-1.75.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2")))) %>% 
  mutate(method = "Feigenbaum")

m_03_175 <- here("data", "linking_comparisons", "crosswalks", "mine_crosswalk-0.3-1.75.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2")))) %>% 
  mutate(method = "Bellman")

ferrie <- here("data", "linking_comparisons", "crosswalks", "ferrie_crosswalk.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2")))) %>% 
  mutate(method = "Ferrie")

samp_ids <- here("data", "linking_comparisons", "comparison_sample.csv") %>% 
  import() %>% 
  .$uniqueid2
  
samp <- load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2"))) %>% 
  filter(uniqueid2 %in% samp_ids) %>% 
  mutate(method = "Original Sample")

# combine attributes for plotting
plot_vars <- c("method", "sei2", "age2", "race_grp2", "bpl2", "erscor502", "edscor502", "marst2")

to_plot <- list(f_03_175, m_03_175, ferrie, samp) %>% 
  map(select, plot_vars) %>% 
  bind_rows() %>% 
  mutate(new_bpl = case_when(bpl2 == 4200 ~ "Pennsylvania",
                             bpl2 != 4200 & bpl2 < 15000 ~ "Rest of USA",
                             TRUE ~ "Other Country"),
         new_marst = case_when(marst2 == 1 ~ "Married",
                               marst2 == 2 ~ "Married, spouse absent",
                               TRUE ~ "Not Married"))


# Plot SEI distributions
ggplot(data = to_plot) +
  geom_density(aes(x = sei2, col = method)) +
  labs(x = "SEI", y = "Density", col = "Method") +
  ggsave(filename = here("figures", "compare_sei_linkage.pdf"), width = 8, height = 4, units = "in")

# Plot age distributions
ggplot(data = to_plot) +
  geom_density(aes(x = age2, col = method)) +
  labs(x = "Age", y = "Density", col = "Method") +
  ggsave(filename = here("figures", "compare_age_linkage.pdf"), width = 8, height = 4, units = "in")




ggplot(data = to_plot) +
  geom_density(aes(x = erscor502, col = method))

ggplot(data = to_plot) +
  geom_density(aes(x = edscor502, col = method))


# plot racial breakdown
to_plot %>% 
  group_by(method, race_grp2) %>% 
  summarise(n = n()) %>% 
  group_by(method) %>% 
  mutate(method_n = sum(n),
         pct = n / method_n * 100) %>% 
  bind_rows(tibble(method = c("Ferrie"),
                   race_grp2 = c("Other"),
                   n = 0,
                   method_n = 1036,
                   pct = 0)) %>% 
  mutate(race_grp2 = factor(race_grp2, levels = c("White", "Black", "Other")),
         label = paste0(round(pct, 1), "%\nN = ", n)) %>% 
  ggplot(aes(x = method, y = pct, group = race_grp2)) +
    geom_col(aes(fill = race_grp2), position = "dodge") +
    geom_text(aes(label = label, y = c(32, 25, 40, 32, 25, 40, 32, 40, 32, 25, 40, 25)), position = position_dodge(0.9), size = 3) +
    labs(x = "Linkage Method", y = "% of Sample", fill = "Race Category") +
    ggsave(filename = here("figures", "compare_race_linkage.pdf"), width = 8, height = 4, units = "in")



# plot birthplace breakdown
to_plot %>% 
  group_by(method, new_bpl) %>% 
  summarise(n = n()) %>% 
  group_by(method) %>% 
  mutate(method_n = sum(n),
         pct = n / method_n * 100) %>% 
  mutate(new_bpl = factor(new_bpl, levels = c("Pennsylvania", "Rest of USA", "Other Country")),
         label = paste0(round(pct, 1), "%\nN = ", n)) %>% 
  ggplot(aes(x = method, y = pct, group = new_bpl)) +
    geom_col(aes(fill = new_bpl), position = "dodge") +
    geom_text(aes(label = label, y = rep(c(20, 35, 27), 4)), position = position_dodge(0.9), size = 3) +
    labs(x = "Linkage Method", y = "% of Sample", fill = "Birthplace") +
    ggsave(filename = here("figures", "compare_birthplace_linkage.pdf"), width = 8, height = 4, units = "in")



# plot marriage status
to_plot %>% 
  group_by(method, new_marst) %>% 
  summarise(n = n()) %>% 
  group_by(method) %>% 
  mutate(method_n = sum(n),
         pct = n / method_n * 100) %>% 
  mutate(new_marst = factor(new_marst, levels = c("Married", "Married, spouse absent", "Not Married")),
         label = paste0(round(pct, 1), "%\nN = ", n)) %>% 
  ggplot(aes(x = method, y = pct, group = new_marst)) +
    geom_col(aes(fill = new_marst), position = "dodge") +
    geom_text(aes(label = label, y = rep(c(50, 37, 25), 4)), position = position_dodge(0.9), size = 3) +
    labs(x = "Linkage Method", y = "% of Sample", fill = "Marriage Status") +
    ggsave(filename = here("figures", "compare_marriage_linkage.pdf"), width = 8, height = 4, units = "in")
  




# let's look at representation in my sample across hyperparam thresholds
m_01_125 <- here("data", "linking_comparisons", "crosswalks", "mine_crosswalk-0.1-1.25.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2"))))

m_02_150 <- here("data", "linking_comparisons", "crosswalks", "mine_crosswalk-0.2-1.5.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2"))))

m_03_175 <- here("data", "linking_comparisons", "crosswalks", "mine_crosswalk-0.3-1.75.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2"))))


ggplot() +
  geom_density(data = m_01_125, aes(x = sei2), col = "green") +
  geom_density(data = m_02_150, aes(x = sei2), col = "blue") +
  geom_density(data = m_03_175, aes(x = sei2), col = "red")


#### comparing confidence scores





#### add number of links to each param combo
# must run needed code in "explor_hyper_params"
params$n_mine <- 0
params$n_feig <- 0

for(a in 1:9){
  b1 <- params$b1[a]
  b2 <- params$b2[a]
  
  params$n_feig[a] <- nrow(import(here("data", "linking_comparisons", "crosswalks", paste0("feigenbaum_crosswalk-", b1, "-", b2, ".csv"))))
  params$n_mine[a] <- nrow(import(here("data", "linking_comparisons", "crosswalks", paste0("mine_crosswalk-", b1, "-", b2, ".csv"))))
}

params %>% 
  select(b1, b2, n_mine, tpr_mine, ppv_mine, n_feig, tpr_feig, ppv_feig) %>% 
  export("~/Documents/Computer Backup/Dissertation/Chapter - Linkage/ml_results_dec18.csv", row.names = F)


### let's take a look at median ages
to_plot %>% 
  group_by(method) %>% 
  summarise(med_age = median(age2))

