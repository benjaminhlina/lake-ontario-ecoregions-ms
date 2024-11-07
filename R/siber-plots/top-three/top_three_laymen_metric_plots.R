# ---- load packages -----
{
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(ggtext)
  library(here)
  library(purrr)
  library(qs)
  library(stringr)
  library(tidyr)
}

# ---- bring in data ----

laymen_metrics_b <- qs::qread(
  here("data-saved",
       "siber-top-three",
       "bays_community_metrics2024-06-19.qs")) %>%
  mutate(
    labels = factor(case_when(
      metric %in% "dX_range" ~ paste0("\U03B4","<sup>", 13, "</sup>",
                                      "C<br>Range"),
      metric %in% "dY_range" ~ paste0("\U03B4","<sup>", 15, "</sup>",
                                      "N<br>Range"),
      metric %in% "TA" ~ "Total Area",
      metric %in% "CD" ~ "Distance to<br>Centroid",
      metric %in% "NND" ~ "Nearest<br>Neighbor<br>Distance",
      metric %in% "SDNND" ~ "SD Nearest<br>Neighbor<br>Distance",
    ),
    levels = c("\U03B4<sup>13</sup>C<br>Range",
               "\U03B4<sup>15</sup>N<br>Range",
               "Total Area",
               "Distance to<br>Centroid",
               "Nearest<br>Neighbor<br>Distance",
               "SD Nearest<br>Neighbor<br>Distance")
    )
  )




laymen_metrics_b %>%
  distinct(metric, labels)

laymen_metrics <- qread(here("data-saved",
                             "siber-top-three",
                             "community_metrics_layman_2024-06-19.qs")) %>%
  mutate(
    labels = factor(case_when(
      names %in% "dX_range" ~ paste0("\U03B4","<sup>", 13, "</sup>",
                                     "C<br>Range"),
      names %in% "dY_range" ~ paste0("\U03B4","<sup>", 15, "</sup>",
                                     "N<br>Range"),
      names %in% "MNND" ~ "Nearest<br>Neighbor<br>Distance",
      names %in% "TA" ~ "Total Area",
      names %in% "CD" ~ "Distance to<br>Centroid",
      names %in% "SDNND" ~ "SD Nearest<br>Neighbor<br>Distance",
    ),
    levels = c("\U03B4<sup>13</sup>C<br>Range",
               "\U03B4<sup>15</sup>N<br>Range",
               "Total Area",
               "Distance to<br>Centroid",
               "Nearest<br>Neighbor<br>Distance",
               "SD Nearest<br>Neighbor<br>Distance")
    )
  )



laymen_metrics

ll <- laymen_metrics %>%
  distinct(names, labels)


laymen_metrics_b %>%
  group_by(metric, ecoregion) %>%
  summarise(
    min = min(estimates),
    max = max(estimates),
  ) %>%
  ungroup() %>%
  print(n = 36)

# ---- create plots
p <- ggplot() +
  geom_violin(data = laymen_metrics_b,
              aes(x = labels, y = estimates,
                  # fill = ecoregion
              )
  ) +

  geom_point(data = laymen_metrics, aes(x = labels, y = estimate),
             size = 3) +
  lemon::facet_rep_wrap( ~ ecoregion,
                         repeat.tick.labels = TRUE) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_markdown(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.16),
  ) +
  labs(
    x = "Ecoregion",
    y = expression("Standard Ellipse Area " ('permille' ^2))
  )


p

# ggsave(here("plots",
#             "siber-plots",
#             "top-three",
#             "laymen-metrics",
#             "top_three_laymen_metrics.png"),
#        plot = p,
#        width = 18, height = 12)


laymen_metrics_b %>%
  group_by(metric, labels, ecoregion) %>%
  summarise(
    med = median(estimates)
  ) %>%
  ungroup() %>%
  print(n = 40)

laymen_metrics %>%
  print(n = 40)

p1 <- ggplot() +
  stat_interval(data = laymen_metrics_b,
                aes(x = labels, y = estimates),
                # .width = c(0.5, 0.95),
                # alpha = 0.5,
                # stroke = 0.8,
                linewidth = 10) +
  geom_point(data = laymen_metrics, aes(x = labels, y = estimate),
             size = 4, shape = 21,
             colour = "black",
             fill = "white") +
  lemon::facet_rep_wrap( ~ ecoregion,
                         repeat.tick.labels = TRUE) +
  scale_colour_viridis_d(name = "Credible\nInterval (%)",
                         option = "B", end = 0.85, begin = 0.35) +
  # scale_colour_manual(name = "Credible\nInterval (%)",
  #                     values = c("#d73027",
  #                                "#fee090",
  #                                "#4575b4")) +
  theme_bw(
    base_size = 18
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_markdown(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # legend.position = "inside",
    # legend.position.inside = c(0.81, 0.17),
  ) +
  labs(
    x = "Community Metrics",
    y = "Value"
  )

p1

# p1
# ggsave(here("plots",
#             "siber-plots",
#             "top-three",
#             "laymen-metrics",
#             "top_three_credible_intervals_lm.png"),
#        plot = p1,
#        width = 16, height = 8)


p2 <- ggplot() +
  stat_interval(data = laymen_metrics_b,
                aes(x = labels, y = estimates,
                    group = ecoregion,
                    colour = ecoregion),
                .width = c(0.5),
                # alpha = 0.5,
                # stroke = 0.8,
                linewidth = 10,
                position = position_dodge(width = 0.9)
  ) +
  geom_point(data = laymen_metrics, aes(x = labels, y = estimate,
                                        group = ecoregion),
             size = 2, shape = 21,
             position = position_dodge(width = 0.9),
             colour = "black",
             fill = "white") +
  # lemon::facet_rep_wrap( ~ ecoregion,
  #                        repeat.tick.labels = TRUE) +
  scale_colour_viridis_d(name = "Credible\nInterval (%)",
                         option = "B", end = 0.85, begin = 0.35) +
  # scale_colour_manual(name = "Credible\nInterval (%)",
  #                     values = c("#d73027",
  #                                "#fee090",
  #                                "#4575b4")) +
  theme_bw(
    base_size = 18
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_markdown(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # legend.position = "inside",
    # legend.position.inside = c(0.81, 0.17),
  ) +
  labs(
    x = "Community Metrics",
    y = "Value"
  )
p2


p3 <- ggplot() +
  geom_violin(data = laymen_metrics_b,
              aes(x = labels, y = estimates,
                  fill = ecoregion
              ), linewidth = 0.15,
              scale = "width",
              width = 0.75,
              # position = position_dodge(width = 0.5)
  ) +
  geom_point(data = laymen_metrics, aes(x = labels, y = estimate,
                                        group = ecoregion),
             # size = 2,
             position = position_dodge(0.75)) +
  geom_vline(xintercept = seq(1.5, 6, 1), linetype = "dotted") +
  scale_fill_viridis_d(begin = 0.1,
                       # end = 0.9,
                       name = "Ecoregion", alpha = 0.35) +
  scale_colour_viridis_d(begin = 0.1,
                         # end = 0.9,
                         name = "Ecoregion") +
  theme_bw(
    # base_size = 22
  ) +
  theme(
    legend.key.size = unit(0.45, 'cm'),
    # legend.key.height = unit(0.5, 'cm'),
    # legend.key.width = unit(0.5, 'cm'),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_markdown(),
    legend.background = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "inside",
    legend.position.inside = c(0.93, 0.8),
  ) +
  labs(
    x = "Community Metrics",
    y = "p(‰|X)"
  )


p3







ggsave(here("plots",
            "siber-plots",
            "top-three",
            "laymen-metrics",
            "top_three_violin_lm.png"),
       plot = p3,
       width = 8, height = 4)



p4 <- ggplot() +
  geom_violin(data = laymen_metrics_b %>%
                filter(metric %in% c(
                  "CD",
                  "NND",
                  "SDNND")),
              aes(x = labels, y = estimates,
                  fill = ecoregion
              ), linewidth = 0.25,
  ) +
  geom_point(data = laymen_metrics %>%
               filter(names %in% c(
                 "CD",
                 "MNND",
                 "SDNND")), aes(x = labels, y = estimate,
                                group = ecoregion),
             size = 2.5,
             position = position_dodge(0.9)) +
  scale_fill_viridis_d(begin = 0.1,
                       # end = 0.9,
                       name = "Ecoregion", alpha = 0.35) +
  scale_colour_viridis_d(begin = 0.1,
                         # end = 0.9,
                         name = "Ecoregion") +
  theme_bw(
    # base_size = 22
  ) +
  theme(
    legend.key.size = unit(0.45, 'cm'),
    # legend.key.height = unit(0.5, 'cm'),
    # legend.key.width = unit(0.5, 'cm'),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_markdown(),
    legend.background = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "inside",
    legend.position.inside = c(0.93, 0.8),
  ) +
  labs(
    x = "Community Metrics",
    y = "‰"
  )


# p4
ggsave(here("plots",
            "siber-plots",
            "top-three",
            "laymen-metrics",
            "top_three_violin_cd_nnd_sdnnd.png"),
       plot = p4,
       width = 8, height = 4)
