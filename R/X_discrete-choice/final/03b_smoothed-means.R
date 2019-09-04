library(here)
library(rio)
library(dplyr)
library(purrr)
library(ggplot2)
library(ghibli)

#### First visualizing conditional predictions of observed choice sets

# load and combine all observed black sample predictions

obvs <- list(
  import(here("data", "for_models", "dc-final-preds", "obsv3-black-20-preds.csv")),
  import(here("data", "for_models", "dc-final-preds", "obsv3-black-30-preds.csv")),
  import(here("data", "for_models", "dc-final-preds", "obsv3-black-40-preds.csv")),
  import(here("data", "for_models", "dc-final-preds", "obsv3-wimm-20-preds.csv")),
  import(here("data", "for_models", "dc-final-preds", "obsv3-wimm-30-preds.csv")),
  import(here("data", "for_models", "dc-final-preds", "obsv3-wimm-40-preds.csv")),
  import(here("data", "for_models", "dc-final-preds", "obsv3-wnb-20-preds.csv")),
  import(here("data", "for_models", "dc-final-preds", "obsv3-wnb-30-preds.csv")),
  import(here("data", "for_models", "dc-final-preds", "obsv3-wnb-40-preds.csv"))
) 

obvs[[1]]$race_cat <- "Black"
obvs[[2]]$race_cat <- "Black"
obvs[[3]]$race_cat <- "Black"
obvs[[4]]$race_cat <- "White Imm"
obvs[[5]]$race_cat <- "White Imm"
obvs[[6]]$race_cat <- "White Imm"
obvs[[7]]$race_cat <- "White NB"
obvs[[8]]$race_cat <- "White NB"
obvs[[9]]$race_cat <- "White NB"

obvs <- obvs %>% 
  bind_rows() %>% 
  mutate(
    # create category for origin percent black
    pblk_cat = case_when(
      ed_pct_black1 < 1 ~ "<1% black",
      ed_pct_black1 >= 1 & ed_pct_black1 < 15  ~ "1% to 15% black",
      ed_pct_black1 >= 15 & ed_pct_black1 < 40  ~ "15% to 40% black",
      ed_pct_black1 >= 40 ~ ">40% black"
    ),
    # Format the year variable
    Year = case_when(
      year1 == 1910 ~ "1910-20",
      year1 == 1920 ~ "1920-30",
      year1 == 1930 ~ "1930-40"
    ),
    # Format as factor
    pblk_cat = factor(pblk_cat, levels = c("<1% black", "1% to 15% black", "15% to 40% black", ">40% black"))
  )



# Hypothesis 1
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = pblk_cat), se = F) +
  facet_grid(race_cat ~ Year) +
  scale_color_brewer(palette = "Reds") +
  #scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Destination % black",
       y = "Probability of Choice",
       col = "") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "hyp1-3.pdf"), height = 5, width = 7)


# Hypothesis 2
ggplot(obvs) +
  geom_smooth(aes(x = dist, y = phat, col = pblk_cat), se = F) +
  xlim(0, 2500) +
  facet_grid(race_cat ~ Year) +
  scale_color_brewer(palette = "Reds") +
  #scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Distance to Destination",
       y = "Probability of Choice",
       col = "") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "hyp2-3.pdf"), height = 5, width = 7)


# Hypothesis 3
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_mean_sei, y = phat, col = pblk_cat), se = F) +
  xlim(0, 30) +
  facet_grid(race_cat ~ Year) +
  scale_color_brewer(palette = "Reds") +
  #scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Mean SEI of Destination",
       y = "Probability of Choice",
       col = "") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "hyp3-3.pdf"), height = 5, width = 7)






