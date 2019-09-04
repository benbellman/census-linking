library(here)
library(rio)
library(dplyr)
library(purrr)
library(ggplot2)
library(ghibli)

#### First visualizing conditional predictions of observed choice sets

# load and combine all observed black sample predictions

obvs <- list(
  import(here("data", "for_models", "dc_example_preds", "final-obsv-black-20-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "final-obsv-black-30-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "final-obsv-black-40-preds.csv"))
) %>% 
  ## add categories for each T1 variable
  map(mutate, 
      # categories for SEI based on within-race terciles
      sei_ter = case_when(
        hh_max_sei1 <= 8 ~ "SEI <8",
        hh_max_sei1 > 8 & hh_max_sei1 <= 18 ~ "SEI 8 to 18",
        hh_max_sei1 > 18 ~ "SEI >18"
      ),
      # % ED black
      pblk_cat = case_when(
        ed_pct_black1 < 1 ~ "<1% black",
        ed_pct_black1 >= 1 & ed_pct_black1 < 15  ~ "1% to 15% black",
        ed_pct_black1 >= 15 & ed_pct_black1 < 40  ~ "15% to 40% black",
        ed_pct_black1 >= 40 ~ ">40% black"
      ),
      # % ED immigrant
      pimm_cat = case_when(
        ed_pct_frnbrn1 < 15 ~ "<15% immigrant",
        ed_pct_frnbrn1 >= 15 & ed_pct_frnbrn1 < 55 ~ "15% to 25% immigrant",
        ed_pct_frnbrn1 >- 25 ~ ">25% immigrant"
      ),
      # mean ED SEI
      msei_cat = case_when(
        ed_mean_sei1 < 10 ~ "<10 Mean SEI",
        ed_mean_sei1 >= 10 & ed_mean_sei1 < 15 ~ "10 to 15 Mean SEI",
        ed_mean_sei1 >= 15 ~ ">15 Mean SEI"
      )) %>% 
  bind_rows() %>% 
  mutate(
    # Format the year variable
    Year = case_when(
      year1 == 1910 ~ "1910-20",
      year1 == 1920 ~ "1920-30",
      year1 == 1930 ~ "1930-40"
    ),
    # Format terciles as factors
    sei_ter = factor(sei_ter, levels = c("SEI <8", "SEI 8 to 18", "SEI >18")),
    pblk_cat = factor(pblk_cat, levels = c("<1% black", "1% to 15% black", "15% to 40% black", ">40% black")),
    pimm_cat = factor(pimm_cat, levels = c("<15% immigrant", "15% to 25% immigrant", ">25% immigrant")),
    msei_cat = factor(msei_cat, levels = c("<10 Mean SEI", "10 to 15 Mean SEI", ">15 Mean SEI")),
    # finally, force all negative predicted probabilties to equal zero
    phat = if_else(phat < 0, 0, phat)
  )

# plots of conditional predicted probabilities for observed black sample

# x-axis % black
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = sei_ter), se = F) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Destination % black",
       y = "Probability of Choice",
       col = "") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-black-xblack.pdf"), height = 5, width = 7)


# x-axis distance
ggplot(obvs) +
  geom_smooth(aes(x = dist, y = phat, col = sei_ter), se = F) +
  xlim(0, 2500) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Distance to Destination",
       y = "Probability of Choice",
       col = "") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-black-xdist.pdf"), height = 5, width = 7)









# load and combine all observed white immigrant sample predictions

obvs <- list(
  import(here("data", "for_models", "dc_example_preds", "final-obsv-wimm-20-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "final-obsv-wimm-30-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "final-obsv-wimm-40-preds.csv"))
) %>% 
  ## add categories for each T1 variable
  map(mutate, 
      # categories for SEI based on within-race terciles
      sei_ter = case_when(
        hh_max_sei1 <= 18 ~ "SEI <18",
        hh_max_sei1 > 18 & hh_max_sei1 <= 47 ~ "SEI 18 to 47",
        hh_max_sei1 > 47 ~ "SEI >47"
      ),
      # % ED black
      pblk_cat = case_when(
        ed_pct_black1 < 1 ~ "<1% black",
        ed_pct_black1 >= 1 & ed_pct_black1 < 15  ~ "1% to 15% black",
        ed_pct_black1 >= 15 & ed_pct_black1 < 40  ~ "15% to 40% black",
        ed_pct_black1 >= 40 ~ ">40% black"
      ),
      # % ED immigrant
      pimm_cat = case_when(
        ed_pct_frnbrn1 < 15 ~ "<15% immigrant",
        ed_pct_frnbrn1 >= 15 & ed_pct_frnbrn1 < 55 ~ "15% to 25% immigrant",
        ed_pct_frnbrn1 >- 25 ~ ">25% immigrant"
      ),
      # mean ED SEI
      msei_cat = case_when(
        ed_mean_sei1 < 10 ~ "<10 Mean SEI",
        ed_mean_sei1 >= 10 & ed_mean_sei1 < 15 ~ "10 to 15 Mean SEI",
        ed_mean_sei1 >= 15 ~ ">15 Mean SEI"
      )) %>% 
  bind_rows() %>% 
  mutate(
    # Format the year variable
    Year = case_when(
      year1 == 1910 ~ "1910-20",
      year1 == 1920 ~ "1920-30",
      year1 == 1930 ~ "1930-40"
    ),
    # Format terciles as factors
    sei_ter = factor(sei_ter, levels = c("SEI <18", "SEI 18 to 47", "SEI >47")),
    pblk_cat = factor(pblk_cat, levels = c("<1% black", "1% to 15% black", "15% to 40% black", ">40% black")),
    pimm_cat = factor(pimm_cat, levels = c("<15% immigrant", "15% to 25% immigrant", ">25% immigrant")),
    msei_cat = factor(msei_cat, levels = c("<10 Mean SEI", "10 to 15 Mean SEI", ">15 Mean SEI")),
    # finally, force all negative predicted probabilties to equal zero
    phat = if_else(phat < 0, 0, phat)
  )

## plots for white immigrants

# % black
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = sei_ter), se = F) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Destination % black",
       y = "Probability of Choice",
       col = "") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-wimm-xblack.pdf"), height = 5, width = 7)

# x-axis distance
ggplot(obvs) +
  geom_smooth(aes(x = dist, y = phat, col = sei_ter), se = F) +
  xlim(0, 2500) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Distance to Destination",
       y = "Probability of Choice",
       col = "") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-wimm-xdist.pdf"), height = 5, width = 7)


# x-axis mean SEI
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_mean_sei, y = phat, col = sei_ter), se = F) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Destination mean SEI",
       y = "Probability of Choice",
       col = "") +
  #ylim(0, 0.01) +
  xlim(0, 40) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-wimm-xsei.pdf"), height = 5, width = 7)


# x-axis % immigrant
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_frnbrn, y = phat, col = sei_ter), se = F) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Destination % immigrant",
       y = "Probability of Choice",
       col = "") +
  #ylim(0, 0.01) +
  xlim(0, 60) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-wimm-ximmigrant.pdf"), height = 5, width = 7)











# load and combine all observed white NB sample predictions

obvs <- list(
  import(here("data", "for_models", "dc_example_preds", "final-obsv-wnb-20-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "final-obsv-wnb-30-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "final-obsv-wnb-40-preds.csv"))
) %>% 
  ## add categories for each T1 variable
  map(mutate, 
      # categories for SEI based on within-race terciles
      sei_ter = case_when(
        hh_max_sei1 <= 33 ~ "SEI <33",
        hh_max_sei1 > 33 & hh_max_sei1 <= 49 ~ "SEI 33 to 49",
        hh_max_sei1 > 49 ~ "SEI >49"
      ),
      # % ED black
      pblk_cat = case_when(
        ed_pct_black1 < 1 ~ "<1% black",
        ed_pct_black1 >= 1 & ed_pct_black1 < 15  ~ "1% to 15% black",
        ed_pct_black1 >= 15 & ed_pct_black1 < 40  ~ "15% to 40% black",
        ed_pct_black1 >= 40 ~ ">40% black"
      ),
      # % ED immigrant
      pimm_cat = case_when(
        ed_pct_frnbrn1 < 15 ~ "<15% immigrant",
        ed_pct_frnbrn1 >= 15 & ed_pct_frnbrn1 < 55 ~ "15% to 25% immigrant",
        ed_pct_frnbrn1 >- 25 ~ ">25% immigrant"
      ),
      # mean ED SEI
      msei_cat = case_when(
        ed_mean_sei1 < 10 ~ "<10 Mean SEI",
        ed_mean_sei1 >= 10 & ed_mean_sei1 < 15 ~ "10 to 15 Mean SEI",
        ed_mean_sei1 >= 15 ~ ">15 Mean SEI"
      )) %>% 
  bind_rows() %>% 
  mutate(
    # Format the year variable
    Year = case_when(
      year1 == 1910 ~ "1910-20",
      year1 == 1920 ~ "1920-30",
      year1 == 1930 ~ "1930-40"
    ),
    # Format terciles as factors
    sei_ter = factor(sei_ter, levels = c("SEI <33", "SEI 33 to 49", "SEI >49")),
    pblk_cat = factor(pblk_cat, levels = c("<1% black", "1% to 15% black", "15% to 40% black", ">40% black")),
    pimm_cat = factor(pimm_cat, levels = c("<15% immigrant", "15% to 25% immigrant", ">25% immigrant")),
    msei_cat = factor(msei_cat, levels = c("<10 Mean SEI", "10 to 15 Mean SEI", ">15 Mean SEI"))
  )

# plots of conditional predicted probabilities for observed white NB sample

# % black
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = sei_ter), se = F) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Destination % black",
       y = "Probability of Choice",
       col = "") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-wnb-xblack.pdf"), height = 5, width = 7)

# x-axis distance
ggplot(obvs) +
  geom_smooth(aes(x = dist, y = phat, col = sei_ter), se = F) +
  xlim(0, 2500) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Distance to Destination",
       y = "Probability of Choice",
       col = "") +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-wnb-xdist.pdf"), height = 5, width = 7)


# x-axis mean SEI
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_mean_sei, y = phat, col = sei_ter), se = F) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Destination mean SEI",
       y = "Probability of Choice",
       col = "") +
  #ylim(0, 0.01) +
  xlim(0, 40) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-wnb-xsei.pdf"), height = 5, width = 7)

# x-axis % immigrant
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_frnbrn, y = phat, col = sei_ter), se = F) +
  facet_grid(pblk_cat ~ Year) +
  #scale_color_brewer(palette = "Reds") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  labs(x = "Destination % immigrant",
       y = "Probability of Choice",
       col = "") +
  #ylim(0, 0.007) +
  xlim(0, 60) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  ggsave(here("figures", "final-dc", "final-wnb-ximmigrant.pdf"), height = 5, width = 7)

