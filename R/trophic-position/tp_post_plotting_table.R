# ---- load packages ----

{
  library(bayestestR)
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(ggh4x)
  library(here)
  library(purrr)
  library(qs)
  library(readr)
  library(stringr)
  library(tidyr)
  library(viridis)
}


# ---- bring in data ----
post_sum <- qread(here("data-saved",
                       "trophic-position-top-three",
                       "bays_tp_post_sum.qs"))
tp <- qread(here("data-saved",
                 "trophic-position-top-three",
                 "bays_tp_post.qs"))

# ---- bring in zm ----

zm_summary <- qs::qread(here("data-saved",
                             "zebra-mussels",
                             "filtered_quant_raw_zebra_mussel_data.qs"))
# ---- select what we want from zm_summary ----

zm_summary <- zm_summary %>%
  select(common_name, ecoregion, cal_d13c, cal_d15n) %>%
  rename(
    d13c = cal_d13c,
    d15n = cal_d15n
  )
# ---- bring in data ----
df_wide <- read_rds(here("data-saved",
                         "top-three",
                         "top_three_cleaned_sia.rds")) %>%
  separate_wider_delim(common_name, delim = " ",
                       names = c('n1', 'n2'),
                       too_few = 'align_start', cols_remove = FALSE) %>%

  mutate(
    common_name_2 = paste(n1, "\n", n2, sep = ""),
    common_name_2 = if_else(common_name_2 %in% c("Alewife\nNA"),
                            true = str_remove(common_name_2, "\nNA"),
                            false = common_name_2),
    common_name_2 = factor(common_name_2,
                           levels =
                             c("Round\nGoby", "Alewife", "Lake\nTrout")
    ),
    ecoregion = factor(ecoregion, levels =
                         c("Anthropogenic",
                           "Inlet",
                           "Open-Coastal",
                           "Deep-Hole",
                           "Outlet",
                           "Embayment")),

    common_name = factor(common_name,
                         levels = c("Round Goby",
                                    "Alewife",
                                    "Lake Trout")),
    trophic_guild = case_when(
      trophic_guild %in% "Insectivore" ~ "Invertivore",
      .default = trophic_guild
    ),
    hab_trop = factor(paste(habitat_type, "\n", trophic_guild, sep = " "),
                      levels = c("Benthic \n Invertivore",
                                 "Benthopelagic \n Invertivore",
                                 "Benthopelagic \n Omnivore",
                                 "Pelagic \n Planktivore",
                                 "Pelagic \n Insectivore",
                                 "Benthopelagic \n Piscivore",
                                 "Pelagic \n Piscivore",
                                 "Benthic \n Omnivore")

    )
  ) %>%
  select(common_name, common_name_2, ecoregion, fish_guilds, hab_trop, lfree_d13c, lfree_d15n) %>%
  rename(
    d13c = lfree_d13c,
    d15n = lfree_d15n
  ) %>%
  bind_rows(., zm_summary) %>%
  mutate(
    common_name = if_else(common_name %in% c("D. polymorpha",
                                             "D. bugensis",
                                             "Dreissena bugensis",
                                             "Dreissena polymorpha",
                                             "Dreissena sp.",
                                             "Quagga mussel"),
                          false = common_name,
                          true = "Dreissenids")
  )

glimpse(df_wide)


tp %>%
  group_by(
    common_name, ecoregion
  ) %>%
  summarise(
    min = min(.x),
    max = max(.x),
  )


hab_trop_join <- df_wide %>%
  distinct(common_name, common_name_2, hab_trop)

# ---- plot ----

viridis_colors_eco <- viridis(6,
                              option = "D",
                              # alpha = 0.5
)
p1 <- ggplot() +

  stat_pointinterval(data = tp, aes(x = common_name,
                                    y = .x,
                                    # ymin = lower, ymax = upper,
                                    point_fill = ecoregion

  ),
  .width = c(0.5,
             0.9
  ),
  width = 1.2,
  point_size = 4,
  point_alpha = 0.5,
  interval_colour = "grey60",
  position = position_dodge(0.4),
  stroke = 1,
  shape = 21,
  ) +
  # geom_point(data = parm_tp, aes(x = common_name,
  #                                y = tp,
  #                                fill = ecoregion),
  #            shape = 21,
  #            position = position_dodge(0.4),
  #            size = 2) +
  scale_fill_manual(aesthetics = c("point_fill", "fill"),
                    values = viridis_colors_eco,
                    name = "Ecoregion") +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.81),
    plot.title = element_text(hjust = 0.5),
    legend.background = element_blank()
  ) +
  labs(
    y = "p(Trophic Position | X)",
    x = "Species"

  )

p1
ggsave(plot = p1, here("plots",
                       "top-three",
                       "trophic-position",
                       "trp-pkg",
                       "bays_trophic_position.png"),
       width = 12, height = 6)


# ---=------

glimpse(post_sum)

post_sum <- post_sum %>%
  mutate(
    ecoregion = factor(group, level =
                         c("Anthropogenic",
                           "Inlet",
                           "Open-Coastal",
                           "Deep-Hole",
                           "Outlet",
                           "Embayment"
                         )),
    common_name = factor(consumer,
                         level = c("Round Goby",
                                   "Alewife",
                                   "Lake Trout")),

  ) %>%
  left_join(hab_trop_join)

# ggplot(data = post_sum) +
#   geom_pointinterval(
#     aes(x = common_name,
#         y = median,
#         ymin = lower,
#         ymax = upper,
#         point_fill = ecoregion),
#     shape = 21,
#     point_size = 4,
#     position = position_dodge(0.4)
#   ) +
#   scale_fill_manual(aesthetics = "point_fill",
#                     values = viridis_colors_eco,
#                     name = "Ecoregion") +
#   theme_bw(
#     base_size = 15
#   ) +
#   theme(
#     panel.grid = element_blank(),
#     strip.background = element_blank(),
#     legend.position = "inside",
#     legend.position.inside = c(0.07, 0.81),
#     plot.title = element_text(hjust = 0.5),
#     legend.background = element_blank()
#   ) +
#   labs(
#     y = "p(Trophic Position | X)",
#     x = "Species"
#
#   )

# ---- calculate parametic tp ----

# zm_summary_1 <- zm_summary %>%
#   mutate(
#     baseline_common_name = if_else(common_name %in% c("D. polymorpha",
#                                                       "D. bugensis",
#                                                       "Dreissena bugensis",
#                                                       "Dreissena polymorpha",
#                                                       "Dreissena sp.",
#                                                       "Quagga mussel"),
#                                    false = common_name,
#                                    true = "Dreissenids"),
#
#   ) %>%
#   group_by(baseline_common_name, ecoregion) %>%
#   summarise(
#     n_sam = n(),
#     mean_d13c = mean(d13c),
#     mean_d15n = mean(d15n),
#   ) %>%
#   ungroup()
#
# df_wide_update <- df_wide %>%
#   left_join(zm_summary_1) %>%
#   group_by(common_name, ecoregion) %>%
#   mutate(
#     delta_n = 3.4,
#     lambda = 2,
#     # (2 + ((d15n - (tp2_b + tp2_p)) / 3.4)),
#     tp = lambda + (d15n - mean_d15n) / delta_n,
#   ) %>%
#   ungroup()
#
#
# df_wide_update %>%
#   group_by(common_name, ecoregion) %>%
#   summarise(
#     mean_tp = mean(tp),
#     sem = sd(tp) / sqrt(n())
#   ) %>%
#   ungroup() %>%
#   mutate(
#     common_name = factor(common_name,
#                          level = c("Round Goby",
#                                    "Alewife",
#                                    "Lake Trout",
#                                    "Dreissenids"))
#   ) %>%
#   ggplot() +
#   geom_errorbar(aes(y = mean_tp,
#                     x = common_name,
#                     group = ecoregion,
#                     ymin = mean_tp - sem,
#                     ymax = mean_tp + sem),
#                 width = 0.05,
#                 position = position_dodge(width = 0.5)) +
#   geom_point(aes(y = mean_tp,
#                  x = common_name,
#                  fill = ecoregion),
#              size = 3,
#              shape = 21,
#              position = position_dodge(width = 0.5)) +
#   scale_fill_viridis_d(name = "Ecoregion",
#                        alpha = 0.6) +
#   scale_y_continuous(breaks = seq(2, 5, 0.5)) +
#   theme_bw(
#     base_size = 15
#   ) +
#   theme(
#     panel.grid = element_blank(),
#     legend.position = "inside",
#     legend.position.inside = c(0.85, 0.9)
#   ) +
#   labs(
#     x = "Common Name",
#     y = "Mean Trophic Position"
#   )
tp <- tp %>%
  left_join(hab_trop_join)

tp_f <- tp %>%
  filter(case_when(
    common_name %in% "Round Goby" & ecoregion %in% "Inlet" ~ .x > 3.08 &
      .x < 3.95,
    .default = TRUE
  )
  )



tp_f_m <- tp %>%
  group_by(common_name, ecoregion, common_name_2, hab_trop) %>%
  summarise(
    median_tp = median(.x)
  ) %>%
  ungroup()
# ---- violin plot ----
p2 <- ggplot() +

  geom_violin(data = tp_f, aes(x = common_name_2,
                               y = .x,
                               fill = ecoregion),
              # trim = FALSE,
              # bounds = c(2.25, 5.5),
              position = position_dodge(0.7)
  ) +

  geom_point(data = tp_f_m, aes(x = common_name_2,
                                y = median_tp,
                                group = ecoregion),

             position = position_dodge(0.7),
             shape = 21,
             fill = "white",
             size = 2) +
  geom_vline(xintercept = seq(1.5, 3, 1), linetype = "dotted") +
  scale_y_continuous(breaks = seq(2, 5.5, 0.5)) +
  # scale_x_discrete(guide = guide_axis_nested(delim = "!", ),
  #                  name = "Fish Guild") +
  scale_fill_viridis_d(
    name = "Ecoregion",
    begin = 0.2, end = 0.98,
    option = "D", alpha = 0.65
    # alpha = 0.35
    ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.81),
    plot.title = element_text(hjust = 0.5),
    legend.background = element_blank()
  ) +
  labs(
    y = "p(Trophic Position | X)",
    x = "Species"

  )

p2

ggsave(plot = p2, here("plots",
                       "top-three",
                       "trophic-position",
                       "trp-pkg",
                       "bays_trophic_position_violin.png"),
       width = 12, height = 6)





post_sum
# ---- extract median and points ----
tp_b_sum <- tp %>%
  group_by(ecoregion, common_name) %>%
  summarise(
    median_post = round(median(.x), digits = 2)
  ) %>%
  ungroup()


ci_intervals <- tp %>%
  mutate(
    ec = paste(ecoregion, common_name, sep = ":")
  ) %>%
  split(.$ec) %>%

  map(~ ci(.x$.x, method = "ETI")) %>%
  bind_rows(.id = "name") %>%
  separate_wider_delim(name, names = c("ecoregion", "common_name"), delim = ":") %>%
  janitor::clean_names() %>%
  # left_join(c_names) %>%
  mutate(
    differences = round(ci_high - ci_low, 2),
    middle = round(differences / 2, 2),
    mid = round(ci_low + middle, 2)
  )

# ---- join bays median ----
ci_intervals_mids <- ci_intervals %>%
  left_join(tp_b_sum) %>%
  arrange(ecoregion, common_name)

# save to excel for reporting

openxlsx::write.xlsx(ci_intervals_mids, here("results",
                                             "top-three",
                                             "trophic-position",
                                             paste0("credible_intervals_bays_tp_",
                                                    Sys.Date(), ".xlsx")))




# ggplot() +
#
#   stat_summary(
#     geom = "point",
#     fun = mean,
#     size = 3,
#     position = position_dodge(width = 0.2),
#     data = df_wide_update,
#     aes(x = common_name, y = tp,
#         colour = ecoregion)
#
#   ) + stat_summary(
#     geom = "errorbar",
#     fun.data = mean_se,
#     width = 0.05,
#     position = position_dodge(width = 0.2),
#     data = df_wide_update,
#     aes(x = common_name, y = tp,
#         group = ecoregion)
#
#   )

# fix(credibilityIntervals)
#
# # If you want to use the median instead of the mode,
# # just add y1 and y2 as arguments
# credibilityIntervals(lake_o_model$df, x = "consumer", xlab ="Community",
#                      group_by = "group", legend = c(0.9, 0.8),
#                      position = position_dodge(width = 0.5),
#                      plotAlpha = FALSE)


