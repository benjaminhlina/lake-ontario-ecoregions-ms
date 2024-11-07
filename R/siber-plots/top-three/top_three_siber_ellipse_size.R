# ---- load packages -----
{
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(ggtext)
  library(ggh4x)
  library(here)
  library(purrr)
  library(qs)
  library(stringr)
  library(tidyr)
}

# ---- bring in data ----


seb_convert <- qread(here("data-saved",
                          "siber-top-three",
                          "bays_ellipse_siber_extract2024-06-19.qs"))



seb_convert

seac <- qread(here("data-saved",
                   "siber-top-three",
                   "centers_ellipse_siber_extract2024-06-19.qs"))

df <-  readr::read_rds(here("data-saved",
                            "top-three",
                            "top_three_cleaned_sia.rds")
)


# ---- SELECT WHAT WE WANT ----
df_select <- df %>%
  separate_wider_delim(common_name, delim = " ",
                       names = c('n1', 'n2'),
                       too_few = 'align_start', cols_remove = FALSE) %>%
  mutate(
    trophic_guild = case_when(
      trophic_guild %in% "Insectivore" ~ "Invertivore",
      .default = trophic_guild
    )
  ) %>%
  mutate(
    common_name_2 = paste(n1, "\n", n2, sep = ""),
    common_name_2 = if_else(common_name_2 %in% c("Alewife\nNA"),
                            true = str_remove(common_name_2, "\nNA"),
                            false = common_name_2),
    common_name_2 = factor(common_name_2,
                           levels =
                             c("Round\nGoby", "Alewife", "Lake\nTrout")
    ),
    fish_guilds = factor(fish_guilds,
                         level = c("Nearshore Benthic Invertivore",
                                   "Offshore Benthic Invertivore",

                                   "Nearshore Benthopelagic Invertivore",
                                   "Offshore Benthopelagic Insectivore",

                                   "Nearshore Pelagic Planktivore",
                                   "Offshore Pelagic Planktivore",

                                   "Nearshore Benthopelagic Omnivore",
                                   "Offshore Pelagic Insectivore",

                                   "Nearshore Benthopelagic Piscivore",
                                   "Offshore Pelagic Piscivore",

                                   "Nearshore Benthic Omnivore"


                         )
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
  )

df_select
hab_trop_join <- df_select %>%
  distinct(common_name, common_name_2, hab_trop)

seb_convert <- seb_convert %>%
  left_join(hab_trop_join )


seac <- seac %>%
  left_join(hab_trop_join)

# ---- create plots
p <- ggplot() +
  geom_violin(data = seb_convert,
              aes(x = ecoregion, y = sea,
                  fill = ecoregion)) +
  geom_point(data = seac, aes(x = ecoregion, y = estimates),
             size = 3) +
  lemon::facet_rep_wrap( ~ common_name, scales = "free_y",
                         repeat.tick.labels = TRUE) +
  # geom_vline(xintercept = seq(1.5, 3, 1), linetype = "dotted") +
  scale_fill_viridis_d(name = "Ecoregion", alpha = 0.45) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.16),
  ) +
  labs(
    x = "Ecoregion",
    y = expression("Standard Ellipse Area p(", 'permille' ^2, "|x)")
  )

p

ggsave(here("plots",
            "siber-plots",
            "top-three",
            "standard-ellipse-area",
            "top_three_ellipse_area_violin_fill.png"),
       plot = p,
       width = 18, height = 12)


p1 <- ggplot() +
  stat_interval(data = seb_convert,
                aes(x = ecoregion, y = sea),
                .width = c(0.5, 0.8, 0.95),
                # alpha = 0.5,
                # stroke = 0.8,
                linewidth = 10) +
  geom_point(data = seac, aes(x = ecoregion, y = estimates),
             size = 2.5, shape = 21,
             colour = "black",
             fill = "white") +
  # facet_wrap( ~ common_name, scales = "free_y") +
  lemon::facet_rep_wrap( ~ common_name, scales = "free_y",
                         # repeat.tick.labels = TRUE
  ) +
  scale_colour_brewer(name = "Credible Interval (%)") +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45,
                               hjust = 1,
                               size = 18
    ),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 2),
    legend.position = "inside",
    legend.position.inside = c(0.83, 0.13),
  ) +
  labs(
    x = "Ecoregion",
    y = expression("Standard Ellipse Area " ('permille' ^2))
  )
p1



# ggsave(here("plots",
#             "siber-plots",
#             "top-three",
#             "standard-ellipse-area",
#             "top_eleven_credible_intervals.png"),
#        plot = p1,
#        width = 18, height = 12)

# ---- violin_nofacet----
seac

p2 <- ggplot() +
  geom_violin(data = seb_convert,
              aes(x = common_name_2,
                  y = sea,
                  fill = ecoregion),
              scale = "width",
              width = 0.75,
              linewidth = 0.3) +
  geom_point(data = seac, aes(x = common_name_2,
                              y = estimates,
                              group = ecoregion),
             size = 2.5,
             position = position_dodge(width = 0.75)) +
  geom_vline(xintercept = seq(1.5, 3, 1), linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  # lemon::facet_rep_wrap( ~ common_name, scales = "free_y",
  #                        repeat.tick.labels = TRUE) +
  scale_fill_viridis_d(name = "Ecoregion", alpha = 0.35) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    # legend.key.size = unit(0.45, 'cm'),
    # legend.key.height = unit(0.5, 'cm'),
    # legend.key.width = unit(0.5, 'cm'),
    # legend.title = element_text(size = 11),
    # legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.background = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "inside",
    legend.position.inside = c(0.92, 0.76),
  ) +
  labs(
    x = "Species",
    y = expression(paste("Standard Ellipse Area p(", "‰" ^2, "|x)"))
  )

p2
ggsave(here("plots",
            "siber-plots",
            "top-three",
            "standard-ellipse-area",
            "top_three_ellipse_area_no_facet_violin.png"),
       plot = p2,
       width = 8.5 * 1.25, height = 4.25 * 1.25)


qs::qsave(p2, here::here("data-saved",
                         "niche-size-ecoregion-plots",
                         "niche_size_ecoregion_plot.qs"))
