library(here)
library(rio)
library(dplyr)
library(purrr)
library(ggplot2)

#### First visualizing conditional predictions of observed choice sets

# load and combine all observed black sample predictions

obvs <- list(
  import(here("data", "for_models", "dc_example_preds", "obsv-black-20-preds-cats.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-black-30-preds-cats.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-black-40-preds-cats.csv"))
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
        ed_pct_black1_1to15 == 0 & ed_pct_black1_15up == 0 ~ "<1% black",
        ed_pct_black1_1to15 == 1 ~ "1% to 15% black",
        ed_pct_black1_15up == 1 ~ ">15% black"
      ),
      # % ED immigrant
      pimm_cat = case_when(
        ed_pct_frnbrn1_15to25 == 0 & ed_pct_frnbrn1_25up == 0 ~ "<15% immigrant",
        ed_pct_frnbrn1_15to25 == 1 ~ "15% to 25% immigrant",
        ed_pct_frnbrn1_25up == 1 ~ ">25% immigrant"
      ),
      # mean ED SEI
      msei_cat = case_when(
        ed_mean_sei1_10to15 == 0 & ed_mean_sei1_15up == 0 ~ "<10 Mean SEI",
        ed_mean_sei1_10to15 == 1 ~ "10 to 15 Mean SEI",
        ed_mean_sei1_15up == 1 ~ ">15 Mean SEI"
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
    pblk_cat = factor(pblk_cat, levels = c("<1% black", "1% to 15% black", ">15% black")),
    pimm_cat = factor(pimm_cat, levels = c("<15% immigrant", "15% to 25% immigrant", ">25% immigrant")),
    msei_cat = factor(msei_cat, levels = c("<10 Mean SEI", "10 to 15 Mean SEI", ">15 Mean SEI"))
  )

# four plots of conditional predicted probabilities for observed black sample

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pblk_cat) +
  labs(x = "% black of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "black-dest-pblk-smooth-cats.pdf"), height = 5, width = 7)
  
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_frnbrn, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pimm_cat) +
  labs(x = "% immigrant of ED",
         y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "black-dest-pimm-smooth-cats.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_mean_sei, y = phat, col = Year)) +
  facet_grid(sei_ter ~ msei_cat) +
  labs(x = "Mean SEI of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "black-dest-msei-smooth-cats.pdf"), height = 5, width = 7)







# load and combine all observed white immigrant sample predictions

obvs <- list(
  import(here("data", "for_models", "dc_example_preds", "obsv-wimm-20-preds-cats.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-wimm-30-preds-cats.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-wimm-40-preds-cats.csv"))
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
        ed_pct_black1_1to15 == 0 & ed_pct_black1_15up == 0 ~ "<1% black",
        ed_pct_black1_1to15 == 1 ~ "1% to 15% black",
        ed_pct_black1_15up == 1 ~ ">15% black"
      ),
      # % ED immigrant
      pimm_cat = case_when(
        ed_pct_frnbrn1_15to25 == 0 & ed_pct_frnbrn1_25up == 0 ~ "<15% immigrant",
        ed_pct_frnbrn1_15to25 == 1 ~ "15% to 25% immigrant",
        ed_pct_frnbrn1_25up == 1 ~ ">25% immigrant"
      ),
      # mean ED SEI
      msei_cat = case_when(
        ed_mean_sei1_10to15 == 0 & ed_mean_sei1_15up == 0 ~ "<10 Mean SEI",
        ed_mean_sei1_10to15 == 1 ~ "10 to 15 Mean SEI",
        ed_mean_sei1_15up == 1 ~ ">15 Mean SEI"
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
    pblk_cat = factor(pblk_cat, levels = c("<1% black", "1% to 15% black", ">15% black")),
    pimm_cat = factor(pimm_cat, levels = c("<15% immigrant", "15% to 25% immigrant", ">25% immigrant")),
    msei_cat = factor(msei_cat, levels = c("<10 Mean SEI", "10 to 15 Mean SEI", ">15 Mean SEI"))
  )

# four plots of conditional predicted probabilities for observed white immigrant sample

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pblk_cat) +
  labs(x = "% black of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wimm-dest-pblk-smooth-cats.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_frnbrn, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pimm_cat) +
  labs(x = "% immigrant of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wimm-dest-pimm-smooth-cats.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_mean_sei, y = phat, col = Year)) +
  facet_grid(sei_ter ~ msei_cat) +
  labs(x = "Mean SEI of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wimm-dest-msei-smooth-cats.pdf"), height = 5, width = 7)





# load and combine all observed white NB sample predictions

obvs <- list(
  import(here("data", "for_models", "dc_example_preds", "obsv-wnb-20-preds-cats.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-wnb-30-preds-cats.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-wnb-40-preds-cats.csv"))
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
        ed_pct_black1_1to15 == 0 & ed_pct_black1_15up == 0 ~ "<1% black",
        ed_pct_black1_1to15 == 1 ~ "1% to 15% black",
        ed_pct_black1_15up == 1 ~ ">15% black"
      ),
      # % ED immigrant
      pimm_cat = case_when(
        ed_pct_frnbrn1_15to25 == 0 & ed_pct_frnbrn1_25up == 0 ~ "<15% immigrant",
        ed_pct_frnbrn1_15to25 == 1 ~ "15% to 25% immigrant",
        ed_pct_frnbrn1_25up == 1 ~ ">25% immigrant"
      ),
      # mean ED SEI
      msei_cat = case_when(
        ed_mean_sei1_10to15 == 0 & ed_mean_sei1_15up == 0 ~ "<10 Mean SEI",
        ed_mean_sei1_10to15 == 1 ~ "10 to 15 Mean SEI",
        ed_mean_sei1_15up == 1 ~ ">15 Mean SEI"
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
    pblk_cat = factor(pblk_cat, levels = c("<1% black", "1% to 15% black", ">15% black")),
    pimm_cat = factor(pimm_cat, levels = c("<15% immigrant", "15% to 25% immigrant", ">25% immigrant")),
    msei_cat = factor(msei_cat, levels = c("<10 Mean SEI", "10 to 15 Mean SEI", ">15 Mean SEI"))
  )

# four plots of conditional predicted probabilities for observed white NB sample

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pblk_cat) +
  labs(x = "% black of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wnb-dest-pblk-smooth-cats.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_frnbrn, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pimm_cat) +
  labs(x = "% immigrant of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wnb-dest-pimm-smooth-cats.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_mean_sei, y = phat, col = Year)) +
  facet_grid(sei_ter ~ msei_cat) +
  labs(x = "Mean SEI of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wnb-dest-msei-smooth-cats.pdf"), height = 5, width = 7)


