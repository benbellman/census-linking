library(here)
library(rio)
library(dplyr)
library(purrr)
library(ggplot2)

#### First visualizing conditional predictions of observed choice sets

# load and combine all observed black sample predictions

obvs <- list(
  import(here("data", "for_models", "dc_example_preds", "obsv-black-20-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-black-30-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-black-40-preds.csv"))
) %>% 
  ## add within-year terciles for each T1 variable
  map(mutate, 
      # Household SEI
      sei_ter = case_when(
        hh_max_sei1 < quantile(hh_max_sei1, 0.333) ~ "Low HH SEI",
        hh_max_sei1 >= quantile(hh_max_sei1, 0.333) & hh_max_sei1 < quantile(hh_max_sei1, 0.667) ~ "Mid HH SEI",
        hh_max_sei1 >= quantile(hh_max_sei1, 0.667) ~ "High HH SEI"
      ),
      # % ED black
      pblk_ter = case_when(
        ed_pct_black1 < quantile(ed_pct_black1, 0.333) ~ "Low % black",
        ed_pct_black1 >= quantile(ed_pct_black1, 0.333) & ed_pct_black1 < quantile(ed_pct_black1, 0.667) ~ "Mid % black",
        ed_pct_black1 >= quantile(ed_pct_black1, 0.667) ~ "High % black"
      ),
      # % ED immigrant
      pimm_ter = case_when(
        ed_pct_frnbrn1 < quantile(ed_pct_frnbrn1, 0.333) ~ "Low % immigrant",
        ed_pct_frnbrn1 >= quantile(ed_pct_frnbrn1, 0.333) & ed_pct_frnbrn1 < quantile(ed_pct_frnbrn1, 0.667) ~ "Mid % immigrant",
        ed_pct_frnbrn1 >= quantile(ed_pct_frnbrn1, 0.667) ~ "High % immigrant"
      ),
      # mean ED SEI
      msei_ter = case_when(
        ed_mean_sei1 < quantile(ed_mean_sei1, 0.333) ~ "Low mean SEI",
        ed_mean_sei1 >= quantile(ed_mean_sei1, 0.333) & ed_mean_sei1 < quantile(ed_mean_sei1, 0.667) ~ "Mid mean SEI",
        ed_mean_sei1 >= quantile(ed_mean_sei1, 0.667) ~ "High mean SEI"
      ),
      # distance
      dist_ter = case_when(
        dist < quantile(dist, 0.333) ~ "Low distance",
        dist >= quantile(dist, 0.333) & dist < quantile(dist, 0.667) ~ "Mid distance",
        dist >= quantile(dist, 0.667) ~ "High distance"
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
    sei_ter = factor(sei_ter, levels = c("Low HH SEI", "Mid HH SEI", "High HH SEI")),
    pblk_ter = factor(pblk_ter, levels = c("Low % black", "Mid % black", "High % black")),
    pimm_ter = factor(pimm_ter, levels = c("Low % immigrant", "Mid % immigrant", "High % immigrant")),
    msei_ter = factor(msei_ter, levels = c("Low mean SEI", "Mid mean SEI", "High mean SEI")),
    dist_ter = factor(dist_ter, levels = c("Low distance", "Mid distance", "High distance"))
  )

# four plots of conditional predicted probabilities for observed black sample

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pblk_ter) +
  labs(x = "% black of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "black-dest-pblk-smooth.pdf"), height = 5, width = 7)
  
ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_frnbrn, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pimm_ter) +
  labs(x = "% immigrant of ED",
         y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "black-dest-pimm-smooth.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_mean_sei, y = phat, col = Year)) +
  facet_grid(sei_ter ~ msei_ter) +
  labs(x = "Mean SEI of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "black-dest-msei-smooth.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dist, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pblk_ter) +
  labs(x = "Distance to ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "black-dest-dist-smooth.pdf"), height = 5, width = 7)









# load and combine all observed white immigrant sample predictions

obvs <- list(
  import(here("data", "for_models", "dc_example_preds", "obsv-wimm-20-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-wimm-30-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-wimm-40-preds.csv"))
) %>% 
  ## add within-year terciles for each T1 variable
  map(mutate, 
      # Household SEI
      sei_ter = case_when(
        hh_max_sei1 < quantile(hh_max_sei1, 0.333) ~ "Low HH SEI",
        hh_max_sei1 >= quantile(hh_max_sei1, 0.333) & hh_max_sei1 < quantile(hh_max_sei1, 0.667) ~ "Mid HH SEI",
        hh_max_sei1 >= quantile(hh_max_sei1, 0.667) ~ "High HH SEI"
      ),
      # % ED black
      pblk_ter = case_when(
        ed_pct_black1 < quantile(ed_pct_black1, 0.333) ~ "Low % black",
        ed_pct_black1 >= quantile(ed_pct_black1, 0.333) & ed_pct_black1 < quantile(ed_pct_black1, 0.667) ~ "Mid % black",
        ed_pct_black1 >= quantile(ed_pct_black1, 0.667) ~ "High % black"
      ),
      # % ED immigrant
      pimm_ter = case_when(
        ed_pct_frnbrn1 < quantile(ed_pct_frnbrn1, 0.333) ~ "Low % immigrant",
        ed_pct_frnbrn1 >= quantile(ed_pct_frnbrn1, 0.333) & ed_pct_frnbrn1 < quantile(ed_pct_frnbrn1, 0.667) ~ "Mid % immigrant",
        ed_pct_frnbrn1 >= quantile(ed_pct_frnbrn1, 0.667) ~ "High % immigrant"
      ),
      # mean ED SEI
      msei_ter = case_when(
        ed_mean_sei1 < quantile(ed_mean_sei1, 0.333) ~ "Low mean SEI",
        ed_mean_sei1 >= quantile(ed_mean_sei1, 0.333) & ed_mean_sei1 < quantile(ed_mean_sei1, 0.667) ~ "Mid mean SEI",
        ed_mean_sei1 >= quantile(ed_mean_sei1, 0.667) ~ "High mean SEI"
      ),
      # distance
      dist_ter = case_when(
        dist < quantile(dist, 0.333) ~ "Low distance",
        dist >= quantile(dist, 0.333) & dist < quantile(dist, 0.667) ~ "Mid distance",
        dist >= quantile(dist, 0.667) ~ "High distance"
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
    sei_ter = factor(sei_ter, levels = c("Low HH SEI", "Mid HH SEI", "High HH SEI")),
    pblk_ter = factor(pblk_ter, levels = c("Low % black", "Mid % black", "High % black")),
    pimm_ter = factor(pimm_ter, levels = c("Low % immigrant", "Mid % immigrant", "High % immigrant")),
    msei_ter = factor(msei_ter, levels = c("Low mean SEI", "Mid mean SEI", "High mean SEI")),
    dist_ter = factor(dist_ter, levels = c("Low distance", "Mid distance", "High distance"))
  )

# four plots of conditional predicted probabilities for observed white immigrant sample

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pblk_ter) +
  labs(x = "% black of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wimm-dest-pblk-smooth.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_frnbrn, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pimm_ter) +
  labs(x = "% immigrant of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wimm-dest-pimm-smooth.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_mean_sei, y = phat, col = Year)) +
  facet_grid(sei_ter ~ msei_ter) +
  labs(x = "Mean SEI of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wimm-dest-msei-smooth.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dist, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pblk_ter) +
  labs(x = "Distance to ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wimm-dest-dist-smooth.pdf"), height = 5, width = 7)








# load and combine all observed white NB sample predictions

obvs <- list(
  import(here("data", "for_models", "dc_example_preds", "obsv-wnb-20-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-wnb-30-preds.csv")),
  import(here("data", "for_models", "dc_example_preds", "obsv-wnb-40-preds.csv"))
) %>% 
  ## add within-year terciles for each T1 variable
  map(mutate, 
      # Household SEI
      sei_ter = case_when(
        hh_max_sei1 < quantile(hh_max_sei1, 0.333) ~ "Low HH SEI",
        hh_max_sei1 >= quantile(hh_max_sei1, 0.333) & hh_max_sei1 < quantile(hh_max_sei1, 0.667) ~ "Mid HH SEI",
        hh_max_sei1 >= quantile(hh_max_sei1, 0.667) ~ "High HH SEI"
      ),
      # % ED black
      pblk_ter = case_when(
        ed_pct_black1 < quantile(ed_pct_black1, 0.333) ~ "Low % black",
        ed_pct_black1 >= quantile(ed_pct_black1, 0.333) & ed_pct_black1 < quantile(ed_pct_black1, 0.667) ~ "Mid % black",
        ed_pct_black1 >= quantile(ed_pct_black1, 0.667) ~ "High % black"
      ),
      # % ED immigrant
      pimm_ter = case_when(
        ed_pct_frnbrn1 < quantile(ed_pct_frnbrn1, 0.333) ~ "Low % immigrant",
        ed_pct_frnbrn1 >= quantile(ed_pct_frnbrn1, 0.333) & ed_pct_frnbrn1 < quantile(ed_pct_frnbrn1, 0.667) ~ "Mid % immigrant",
        ed_pct_frnbrn1 >= quantile(ed_pct_frnbrn1, 0.667) ~ "High % immigrant"
      ),
      # mean ED SEI
      msei_ter = case_when(
        ed_mean_sei1 < quantile(ed_mean_sei1, 0.333) ~ "Low mean SEI",
        ed_mean_sei1 >= quantile(ed_mean_sei1, 0.333) & ed_mean_sei1 < quantile(ed_mean_sei1, 0.667) ~ "Mid mean SEI",
        ed_mean_sei1 >= quantile(ed_mean_sei1, 0.667) ~ "High mean SEI"
      ),
      # distance
      dist_ter = case_when(
        dist < quantile(dist, 0.333) ~ "Low distance",
        dist >= quantile(dist, 0.333) & dist < quantile(dist, 0.667) ~ "Mid distance",
        dist >= quantile(dist, 0.667) ~ "High distance"
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
    sei_ter = factor(sei_ter, levels = c("Low HH SEI", "Mid HH SEI", "High HH SEI")),
    pblk_ter = factor(pblk_ter, levels = c("Low % black", "Mid % black", "High % black")),
    pimm_ter = factor(pimm_ter, levels = c("Low % immigrant", "Mid % immigrant", "High % immigrant")),
    msei_ter = factor(msei_ter, levels = c("Low mean SEI", "Mid mean SEI", "High mean SEI")),
    dist_ter = factor(dist_ter, levels = c("Low distance", "Mid distance", "High distance"))
  )

# four plots of conditional predicted probabilities for observed white NB sample

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_black, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pblk_ter) +
  labs(x = "% black of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wnb-dest-pblk-smooth.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_pct_frnbrn, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pimm_ter) +
  labs(x = "% immigrant of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wnb-dest-pimm-smooth.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dest_ed_mean_sei, y = phat, col = Year)) +
  facet_grid(sei_ter ~ msei_ter) +
  labs(x = "Mean SEI of ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wnb-dest-msei-smooth.pdf"), height = 5, width = 7)

ggplot(obvs) +
  geom_smooth(aes(x = dist, y = phat, col = Year)) +
  facet_grid(sei_ter ~ pblk_ter) +
  labs(x = "Distance to ED",
       y = "Conditional Probability of Choice") +
  ggsave(here("figures", "dc_obvs", "wnb-dest-dist-smooth.pdf"), height = 5, width = 7)



