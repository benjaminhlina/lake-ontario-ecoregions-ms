# ---- load packages -----
{
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(ggtext)
  library(ggh4x)
  library(ggsignif)
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

# ---- bring in among diel letters ----

d13c_letters <- read_csv(here("results",
                              "top-three",
                              "multicompare",
                              "d13c",
                              "rel_d13c_letters_same.csv"))


d13c_letters


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
                                   "Offshore Pelagic Invertivore",
                                   
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



glimpse(df_select)

# ----- arrange for plotting ----
df_isotope <- df_select %>%
  dplyr::select(
    fish_guilds, near_or_offshore,
    hab_trop,
    common_name, common_name_2, ecoregion,
    # cal_d13c, cal_d15n,
    lfree_d13c, lfree_d15n,
    rel_d13c_lfree, rel_d15n_lfree,
  ) %>%
  pivot_longer(cols = -c(fish_guilds, near_or_offshore,
                         hab_trop, common_name, common_name_2, ecoregion),
               names_to = "isotope") %>%
  mutate(
    neutron = if_else(isotope %in% c(
      # "cal_d13c",
      "lfree_d13c",
      "rel_d13c_lfree"
    ), true = 13,
    false = 15),
    element = if_else(isotope %in% c(
      # "cal_d13c",
      "lfree_d13c",
      "rel_d13c_lfree"), true = "C",
      false = "N"),
    ecoregion = factor(ecoregion, levels = c("Anthropogenic",
                                             "Inlet",
                                             "Open-Coastal",
                                             "Deep-Hole",
                                             "Outlet",
                                             "Embayment"))
  )


# ---- loop through using map and plot both relative and normal ----

p <- df_isotope %>%
  split(.$isotope) %>%
  map(~
        ggplot(data = .x, aes(x = common_name_2,
                              y = value,
                              fill = ecoregion)) +
        geom_boxplot(outlier.shape = NA,
                     width = 0.75,
                     # position = position_dodge(preserve = "single")
        ) +
        # geom_segment(aes(y = 5, yend = 5,
        #              x = c(1, 2, 3),
        #              xend = c(1.5, 2.5, 3.5)),
        #            linewidth = 1,
        #            colour = "black",
        #            ) +
        scale_fill_viridis_d(begin = 0.1,
                             # end = 0.9,
                             name = "Ecoregion", alpha = 0.5) +
        scale_colour_viridis_d(begin = 0.1,
                               # end = 0.8,
                               name = "Species") +
        guides(fill = guide_legend(override.aes = list(shape = 21,
                                                       colour = "black",
                                                       stroke = 0.8,
                                                       size = 3)),
               shape = guide_legend(override.aes = list(size = 3))
        ) +
        theme_bw(
          base_size = 15
        ) +
        theme(
          panel.grid = element_blank(),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_markdown(),
          # axis.text.x = element_text(angle = 90)
        ) +
        labs(
          x = "Species",
          y = paste("\U03B4",
                    "<sup>", unique(.$neutron), "</sup>",unique(.$element),
                    sep = ""
                    
          )
        )
  )


# ----- LOOK AT HOW MANY SPECIES AND ECO ----
ggstat <- ggplot_build(p$rel_d13c_lfree)$data

# sum_iso <- df_select %>%
#   group_by(common_name, ecoregion) %>%
#   summarise(
#     n = n(),
#     max_d13c = max(lfree_d13c),
#     min_d13c = min(lfree_d13c),
#   ) %>%
#   ungroup() %>%
#   print(n = 51) %>%
#   arrange(common_name, ecoregion)

# ---- fix letters ----

d13c_long <- d13c_letters %>%
  pivot_longer(cols = -common_name,
               names_to = "ecoregion",
               values_to = "letters") %>%
  separate_wider_delim(common_name, delim = " ",
                       names = c('n1', 'n2'),
                       too_few = 'align_start', cols_remove = FALSE) %>%
  mutate(
    common_name_2 = paste(n1, "\n", n2, sep = ""),
    common_name_2 = if_else(common_name_2 %in% c("Alewife\nNA",
                                                 "Walleye\nNA"),
                            true = str_remove(common_name_2, "\nNA"),
                            false = common_name_2),
    common_name_2 = factor(common_name_2,
                           levels =
                             c("Round\nGoby", "Deepwater\nSculpin",
                               "Slimy\nSculpin",
                               "Spottail\nShiner", "Rock\nBass",
                               "Yellow\nPerch", "Alewife", "Rainbow\nSmelt",
                               "Walleye", "Chinook\nsalmon", "Lake\nTrout")
    )
  ) %>%
  filter(!is.na(letters))


# ---- create object to join with sig_letess ----

hab_trop_join <- df_select %>%
  distinct(common_name_2, hab_trop)

# --- extract locations ----
ggstat <- ggplot_build(p$rel_d13c_lfree)$data

# # ---- letter prep ----
sig_let_d13c <-  tibble(
  common_name_2 = factor(d13c_long$common_name_2),
  
  ecoregion = factor(d13c_long$ecoregion, levels =
                       c("Anthropogenic",
                         "Inlet",
                         "Open-Coastal",
                         "Deep-Hole",
                         "Outlet",
                         "Embayment"),
                     order = TRUE),
  letter = d13c_long$letters,
  x = d13c_long$ecoregion,
  y = ggstat[[1]]$ymax,
  
) %>%
  arrange(common_name_2, ecoregion) %>%
  left_join(hab_trop_join, by = "common_name_2")


p



# ---- just boxplot ----


sum_max_c <- df_isotope %>%
  filter(isotope %in% "rel_d13c_lfree") %>%
  group_by(common_name, common_name_2, hab_trop) %>%
  summarise(
    max_value = max(value)
  ) %>%
  ungroup() %>%
  mutate(
    max_value = case_when(
      common_name == "Round Goby" ~ max_value + 1.0,
      # common_name == "Lake Trout" ~ max_value + 0.5,
      # common_name == "Alewife" ~ max_value - 1.5,
      .default = max_value
    )
  )

p5 <- (p$rel_d13c_lfree +
         geom_signif(y_position = sum_max_c$max_value,
                     xmin = c(0.64, 1.66, 2.66),
                     # seq(0.25, 3.25, 1.25),
                     xmax = c(1.34, 2.36, 3.36),
                     annotation = c("A", "B", "C"),
                     tip_length = 0.02, textsize = 6) +
         scale_y_continuous(breaks = seq(-5, 15, 2.5),
                            # limits = c(0, 12.5)
         ) +
         # scale_y_continuous(breaks = rev(seq(-32, -12, 2))) +
         geom_text(data = sig_let_d13c, 
                   aes(x = common_name_2,
                       y = y + 0.45,
                       label = letter,
                       group = ecoregion
                   ),
                   size = 5,
                   position = position_dodge(width = 0.75)) +
         coord_cartesian(ylim = c(-5, 15)) +
         theme(
           legend.position = "inside",
           # legend.position = "none",
           legend.position.inside = c(0.93, 0.81),
           legend.background = element_blank()
         ) +
         labs(
           x = "Species",
           y = paste("Relative ", "\U03B4",
                     "<sup>", 13, "</sup>", "C (‰)",
                     sep = "")
         ))

p5
ggsave(p5, filename = here("plots",
                           "top-three",
                           "boxplot",
                           paste("boxplot_relative_d13c_top_three_", Sys.Date(),
                                 ".png", sep = "")),
       width = 12, height = 6)


write_rds(p5, here("plot-objects",
                   "rel_d13c_top_three_boxplot_letters.rds"))
