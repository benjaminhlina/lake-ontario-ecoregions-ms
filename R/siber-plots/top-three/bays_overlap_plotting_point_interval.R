# ---- packages ----
{
  library(distributional)
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(ggh4x)
  library(here)
  library(qs)
  library(purrr)
  library(patchwork)
  library(viridis)


}

# ---- bring posterior dist of within_eco overlaps -----
bays_95_overlap <- qread(here("data-saved",
                              "siber-overlap-bays",
                              "correct_overlap_95_bays_within_eco_2024-06-19.qs"))
# ---- brting in species -----
bays_95_overlap_sp <- qread(here("data-saved",
                                 "siber-overlap-bays",
                                 "correct_overlap_95_bays_within_sp_2024-06-19.qs"))
# ---- create unique sets for within eco ---
compare_filter <- bays_95_overlap$compare %>%
  unique() %>%
  strsplit("_") %>%
  lapply(sort) %>%
  sapply(paste, collapse = "_") %>%
  unique()



# ---- filter within eco  ----

bays_95_overlap_f <- bays_95_overlap %>%
  filter(compare %in% compare_filter)

# ---- create unique sets for within sp among eco ---
compare_filter_sp <- bays_95_overlap_sp %>%
  distinct(compare) %>%
  .$compare %>%
  strsplit("_") %>%
  lapply(sort) %>%
  sapply(paste, collapse = "_") %>%
  unique()
# ---- filter within sp among eco
bays_95_overlap_sp_f <- bays_95_overlap_sp %>%
  filter(compare %in% compare_filter_sp)



# ---- plot within eco regions ----
# Define the viridis colors
viridis_colors_eco <- viridis(6,
                              option = "D",
                              # alpha = 0.5
)
p_eco_f <- bays_95_overlap_f %>%
  mutate(
    compare_id = paste(common_name_1," vs. \n", common_name_2, sep = "")
  ) %>%
  ggplot() +
  stat_pointinterval(aes(x = compare_id,
                         y = prop_overlap,
                         point_fill = eco_name_1,

  ),
  # .width = c(0.5, 0.95),
  # width = 1.2,
  point_size = 4,
  interval_colour = "grey60",
  position = position_dodge(0.4),
  # stroke = 1,
  shape = 21,
  ) +
  geom_hline(yintercept = 0.6, linetype = "dashed") +
  scale_fill_manual(aesthetics = "point_fill",
                    values = viridis_colors_eco,
                    name = "Ecoregion") +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.07, 0.88),
    plot.title = element_text(hjust = 0.5),
    legend.background = element_blank()
  ) +
  labs(
    y = "p(Percent Similarity | X)",
    x = "Species Comparison"

  )

# print
p_eco_f

# export
ggsave(here("plots",
            "siber-plots",
            "top-three",
            "niche-similiarties",
            "bays_niche_similirty_within_ecoregion_point_interval.png"),
       plot = p_eco_f,
       width = 16, height = 8.5)

# ---- plot within species eco ----
# Define the viridis colors
viridis_colors_sp <- viridis(3, begin = 0.35, end = 0.9,
                          option = "A",
                          alpha = 0.75
)

# plot
p_sp_f <- bays_95_overlap_sp_f %>%
  mutate(
    compare_id = paste(eco_name_1," vs. \n", eco_name_2, sep = "")
  ) %>%
  ggplot() +
  geom_vline(xintercept = seq(1.5, 15, 1), linetype = "dotted") +
  geom_hline(yintercept = 0.6, linetype = "dashed") +
  stat_pointinterval(aes(x = compare_id,
                         y = prop_overlap,
                         point_fill = common_name_1,

  ),
  # .width = c(0.5, 0.95),
  # width = 1.2,
  point_size = 4,
  interval_colour = "grey60",
  position = position_dodge(0.4),
  # stroke = 1,
  shape = 21,
  ) +
  scale_fill_manual(aesthetics = "point_fill",
                    values = viridis_colors_sp,
                    name = "Species") +

  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.06, 0.9075),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    # legend.background = element_blank()
  ) +
  labs(
    y = "p(Percent Similarity | X)",
    x = "Ecoregion Comparison"

  )

# print
p_sp_f

# save
ggsave(here("plots",
            "siber-plots",
            "top-three",
            "niche-similiarties",
            "bays_niche_similirty_among_ecoregion_point_interval.png"),
       plot = p_sp_f,
       width = 16, height = 8.5)


