# ---- load packages -----
{
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(ggtext)
  library(ggh4x)
  library(here)
  library(purrr)
  library(patchwork)
  library(readr)
  library(stringr)
  library(tidyr)
}

# ---- bring in data ----
df <-  read_rds(here("data-saved",
                     "top-three",
                     "top_three_cleaned_sia.rds")
)


glimpse(df)


ellipse_summary <- qs::qread(here(
  "data-saved",
  "estimated-ellipse",
  "top_three_ecoregion_ellipse_summary.qs"
))

# ---- biplot ----
biplot <-  ggplot() +
  # geom_errorbar(data = df_select_summary, aes(x = mean_d13c,
  #                                             y = mean_d15n,
  #                                             ymin = mean_d15n - se_d15n,
  #                                             ymax = mean_d15n + se_d15n,
  #                                             # xmin = mean_cal_d13c - sem_cal_d13c,
  #                                             # xmax = mean_cal_d13c + sem_cal_d13c
  # ),
  # width = 0.1) +
  # geom_errorbar(data = df_select_summary,
  #               aes(x = mean_d13c,
  #                   y = mean_d15n,
  #                   # ymin = mean_cal_d15n - sem_cal_d15n,
  #                   # ymax = mean_cal_d15n + sem_cal_d15n,
  #                   xmin = mean_d13c - se_d13c,
  #                   xmax = mean_d13c + se_d13c),
  #               width = 0.1) +
  geom_point(data = df,
             aes(x = cal_d13c,
                 y = cal_d15n,
                 fill = common_name),
             size = 3,
             shape = 21) +
  geom_polygon(data = ellipse_summary,
               mapping = aes(x = median_d13c, y = median_d15n,
                             group = common_name,
                             color = common_name
               ),
               fill = NA,
               linewidth = 1) +
  scale_fill_viridis_d(begin = 0.35,
                       option = "A",
                       end = 0.9,
                       name = "Species", alpha = 0.75) +
  scale_colour_viridis_d(begin = 0.35,
                         option = "A",
                         end = 0.9,
                         name = "Species") +
  scale_x_continuous(breaks = rev(seq(-34, -14, 4))) +
  scale_y_continuous(breaks = seq(7.5, 22.5, 2.5)) +

  lemon::facet_rep_wrap(~ ecoregion, repeat.tick.labels = TRUE) +

  guides(fill = guide_legend(
    override.aes = list(shape = 21,
                        stroke = 0.8,
                        size = 4),
  ),

  ) +
  theme_bw(
    base_size = 18
  ) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(),
    legend.key.spacing.y = unit(1.5, 'cm'),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.box.background = element_blank()
    # legend.position = "inside",
    # legend.position.inside = c(0.12, 0.2),
    # legend.key = element_rect(colour = "black")
    # legend.key = element_blank()
  ) +
  labs(
    x = expression(paste(delta ^ 13, "C (‰)")),
    y = expression(paste(delta ^ 15, "N (‰)"))
  )
biplot

ggsave(biplot, filename = here("plots",
                                 "top-three",
                                 "biplot",
                                 paste("biplot_raw_facet_ellipse_eco_", Sys.Date(),
                                       ".png", sep = "")
),
width = 16, height = 8)

