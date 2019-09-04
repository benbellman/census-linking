library(here)
library(rio)
library(dplyr)
library(ggplot2)

#### Vizualizing example predictions

# probabilities interpreted without contextual choice set 
# these are predictions for synthetic data of hypothestical cases



# collect predictions that do contain percent immigrant vars
preds1 <- list.files(here("data", "for_models", "dc-final-preds"), 
                    full.names = T,
                    pattern = "ex3-") %>% 
  map(import) 

# add year and group information
preds1[[1]]$Year <- "1910-20"
preds1[[2]]$Year <- "1920-30"
preds1[[3]]$Year <- "1930-40"
preds1[[4]]$Year <- "1910-20"
preds1[[5]]$Year <- "1920-30"
preds1[[6]]$Year <- "1930-40"
preds1[[7]]$Year <- "1910-20"
preds1[[8]]$Year <- "1920-30"
preds1[[9]]$Year <- "1930-40"

preds1[[1]]$race_cat <- "Black"
preds1[[2]]$race_cat <- "Black"
preds1[[3]]$race_cat <- "Black"
preds1[[4]]$race_cat <- "White Imm"
preds1[[5]]$race_cat <- "White Imm"
preds1[[6]]$race_cat <- "White Imm"
preds1[[7]]$race_cat <- "White NB"
preds1[[8]]$race_cat <- "White NB"
preds1[[9]]$race_cat <- "White NB"

preds1 <- bind_rows(preds1)


# collect predictions that don't contain percent immigrant vars
preds2 <- list.files(here("data", "for_models", "dc-final-preds"), 
                    full.names = T,
                    pattern = "ex2") %>% 
  map(import) 

# add year and group information
preds2[[1]]$Year <- "1910-20"
preds2[[2]]$Year <- "1920-30"
preds2[[3]]$Year <- "1930-40"
preds2[[4]]$Year <- "1910-20"
preds2[[5]]$Year <- "1920-30"
preds2[[6]]$Year <- "1930-40"
preds2[[7]]$Year <- "1910-20"
preds2[[8]]$Year <- "1920-30"
preds2[[9]]$Year <- "1930-40"

preds2[[1]]$race_cat <- "Black"
preds2[[2]]$race_cat <- "Black"
preds2[[3]]$race_cat <- "Black"
preds2[[4]]$race_cat <- "White Imm"
preds2[[5]]$race_cat <- "White Imm"
preds2[[6]]$race_cat <- "White Imm"
preds2[[7]]$race_cat <- "White NB"
preds2[[8]]$race_cat <- "White NB"
preds2[[9]]$race_cat <- "White NB"

preds2 <- bind_rows(preds2)


### Faceted plot for Hypothesis 1

# with immigrant vars
filter(preds1, dest_cat == "% black" & ed_pct_black1 %in% c(0, 10, 20, 30)) %>% 
ggplot() +
  geom_line(aes(x = dest_ed_pct_black, y = phat, lty = factor(ed_pct_black1))) +
  facet_grid(race_cat ~ Year) +
  labs(x = "% black of destination", 
       y = "Probability of Choice", 
       col = "Origin\n% black",
       title = "Hypothesis 1",
       subtitle = "With immigrant vars") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "for_meeting_8-21", "hypo1-imm.png"), height = 5, width = 7)

# without immigrant vars
filter(preds2, dest_cat == "% black" & ed_pct_black1 %in% c(0, 10, 20, 30)) %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_pct_black, y = phat, lty = factor(ed_pct_black1))) +
  facet_grid(race_cat ~ Year) +
  labs(x = "% black of destination", 
       y = "Probability of Choice", 
       col = "Origin\n% black",
       title = "Hypothesis 1",
       subtitle = "Without immigrant vars") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "for_meeting_8-21", "hypo1-noimm.png"), height = 5, width = 7)


### Faceted plot for Hypothesis 2

# with immigrant vars
filter(preds1, dest_cat == "Distance" & ed_pct_black1 %in% c(0, 10, 20, 30)) %>% 
  ggplot() +
  geom_line(aes(x = dist, y = phat, lty = factor(ed_pct_black1))) +
  facet_grid(race_cat ~ Year) +
  labs(x = "Distance to destination", 
       y = "Probability of Choice", 
       col = "Origin\n% black",
       title = "Hypothesis 2",
       subtitle = "With immigrant vars") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "for_meeting_8-21", "hypo2-imm.png"), height = 5, width = 7)

# without immigrant vars
filter(preds2, dest_cat == "Distance" & ed_pct_black1 %in% c(0, 10, 20, 30)) %>% 
  ggplot() +
  geom_line(aes(x = dist, y = phat, lty = factor(ed_pct_black1))) +
  facet_grid(race_cat ~ Year) +
  labs(x = "Distance to destination", 
       y = "Probability of Choice", 
       col = "Origin\n% black",
       title = "Hypothesis 2",
       subtitle = "Without immigrant vars") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "for_meeting_8-21", "hypo2-noimm.png"), height = 5, width = 7)


### Faceted plot for Hypothesis 3

# with immigrant vars
filter(preds1, dest_cat == "Mean SEI" & ed_pct_black1 %in% c(0, 10, 20, 30)) %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_mean_sei, y = phat, lty = factor(ed_pct_black1))) +
  facet_grid(race_cat ~ Year) +
  labs(x = "Mean SEI of destination", 
       y = "Probability of Choice", 
       col = "Origin\n% black",
       title = "Hypothesis 3",
       subtitle = "With immigrant vars") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "for_meeting_8-21", "hypo3-imm.png"), height = 5, width = 7)

# without immigrant vars
filter(preds2, dest_cat == "Mean SEI" & ed_pct_black1 %in% c(0, 10, 20, 30)) %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_mean_sei, y = phat, lty = factor(ed_pct_black1))) +
  facet_grid(race_cat ~ Year) +
  labs(x = "Mean SEI of destination", 
       y = "Probability of Choice", 
       col = "Origin\n% black",
       title = "Hypothesis 3",
       subtitle = "Without immigrant vars") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "for_meeting_8-21", "hypo3-noimm.png"), height = 5, width = 7)



