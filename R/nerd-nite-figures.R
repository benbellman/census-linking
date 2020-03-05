ggplot(eds) +
  geom_sf(aes(fill = ed_custom),
          size = 0) +
  facet_wrap(. ~ year, nrow = 1) +
  scale_fill_manual(values = custom_pal) +
  labs(x = "", y = "", fill = "") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank()) +
  ggsave("~/Desktop/map-ed-categories.png", height = 4.5, width = 11)

eds %>% 
  mutate(
    pct_b_cat = factor(case_when(
      ed_pct_b >= 90 ~ ">90%",
      ed_pct_b < 90 & ed_pct_b >= 50 ~ "50% - 90%",
      ed_pct_b < 50 & ed_pct_b >= 25 ~ "25% - 50%",
      ed_pct_b < 25 & ed_pct_b >= 5 ~ "5% - 25%",
      ed_pct_b < 5 ~ "<5%"
    ), levels = c("<5%", "5% - 25%", "25% - 50%", "50% - 90%", ">90%"))
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = pct_b_cat), size = 0) +
  scale_fill_brewer(palette = "PuBuGn") +
  facet_wrap(. ~ year, nrow = 1) +
  labs(fill = "Percent Black") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(458000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave("~/Desktop/map-ed-pct-black.png", height = 4.5, width = 11)
