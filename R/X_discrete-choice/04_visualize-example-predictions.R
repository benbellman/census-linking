library(here)
library(rio)
library(dplyr)
library(ggplot2)

#### Vizualizing example predictions

# probabilities interpreted without contextual choice set 
# these are predictions for synthetic data of hypothestical cases


black_preds <- list.files(here("data", "for_models", "dc_example_preds"), 
                    pattern = "^ex-black",
                    full.names = T) %>% 
  map(import) 

black_preds[[1]]$Year <- "1910-20"
black_preds[[2]]$Year <- "1920-30"
black_preds[[3]]$Year <- "1930-40"

black_preds <- bind_rows(black_preds)


#### Black sample - % black of destination

black_preds %>%
  filter(dest_cat == "% black") %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_pct_black, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "ED % black",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "black-pblk-ex-pred.pdf"), height = 4, width = 6)


#### Black sample - % immigrant of destination

black_preds %>%
  filter(dest_cat == "% immigrant") %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_pct_frnbrn, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "ED % immigrant",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "black-pimm-ex-pred.pdf"), height = 4, width = 6)


#### Black sample - mean SEI of destination

black_preds %>%
  filter(dest_cat == "Mean SEI") %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_mean_sei, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "ED mean SEI",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "black-msei-ex-pred.pdf"), height = 4, width = 6)


#### Black sample - distance of destination

black_preds %>%
  filter(dest_cat == "Distance") %>% 
  ggplot() +
  geom_line(aes(x = dist, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "Distance to ED",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "black-dist-ex-pred.pdf"), height = 4, width = 6)




#############################
###### White immigrants #####
#############################

wimm_preds <- list.files(here("data", "for_models", "dc_example_preds"), 
                          pattern = "^ex-wimm",
                          full.names = T) %>% 
  map(import) 

wimm_preds[[1]]$Year <- "1910-20"
wimm_preds[[2]]$Year <- "1920-30"
wimm_preds[[3]]$Year <- "1930-40"

wimm_preds <- bind_rows(wimm_preds)


#### White immigrant - % black of destination

wimm_preds %>%
  filter(dest_cat == "% black") %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_pct_black, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "ED % black",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "wimm-pblk-ex-pred.pdf"), height = 4, width = 6)


#### White immigrant - % immigrant of destination

wimm_preds %>%
  filter(dest_cat == "% immigrant") %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_pct_frnbrn, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "ED % immigrant",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "wimm-pimm-ex-pred.pdf"), height = 4, width = 6)


#### White immigrant - mean SEI of destination

wimm_preds %>%
  filter(dest_cat == "Mean SEI") %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_mean_sei, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "ED mean SEI",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "wimm-msei-ex-pred.pdf"), height = 4, width = 6)


#### White immigrant sample - distance of destination

wimm_preds %>%
  filter(dest_cat == "Distance") %>% 
  ggplot() +
  geom_line(aes(x = dist, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "Distance to ED",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "wimm-dist-ex-pred.pdf"), height = 4, width = 6)




#############################
###### White Native-born #####
#############################

wnb_preds <- list.files(here("data", "for_models", "dc_example_preds"), 
                         pattern = "^ex-wnb",
                         full.names = T) %>% 
  map(import) 

wnb_preds[[1]]$Year <- "1910-20"
wnb_preds[[2]]$Year <- "1920-30"
wnb_preds[[3]]$Year <- "1930-40"

wnb_preds <- bind_rows(wnb_preds)


#### White immigrant - % black of destination

wnb_preds %>%
  filter(dest_cat == "% black") %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_pct_black, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "ED % black",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "wnb-pblk-ex-pred.pdf"), height = 4, width = 6)


#### White immigrant - % immigrant of destination

wnb_preds %>%
  filter(dest_cat == "% immigrant") %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_pct_frnbrn, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "ED % immigrant",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "wnb-pimm-ex-pred.pdf"), height = 4, width = 6)


#### White immigrant - mean SEI of destination

wnb_preds %>%
  filter(dest_cat == "Mean SEI") %>% 
  ggplot() +
  geom_line(aes(x = dest_ed_mean_sei, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "ED mean SEI",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "wnb-msei-ex-pred.pdf"), height = 4, width = 6)


#### White immigrant sample - distance of destination

wnb_preds %>%
  filter(dest_cat == "Distance") %>% 
  ggplot() +
  geom_line(aes(x = dist, y = phat2, col = Year, lty = sei_cat)) +
  labs(x = "Distance to ED",
       y = "Unconditional Probability of Choice",
       col = "",
       lty = "") +
  facet_grid(ed1_cat ~ .) +
  ggsave(here("figures", "dc_ex", "wnb-dist-ex-pred.pdf"), height = 4, width = 6)


