# ---- packages ----
{
  library(bayestestR)
  library(distributional)
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(here)
  library(qs)
  library(purrr)
  library(tidyr)
  library(viridis)
}

# ---- brting in species -----
bays_95_overlap_sp <- qread(here("data-saved",
                                 "siber-overlap-bays",
                                 "correct_overlap_95_bays_within_sp_2024-06-19.qs"))


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
  filter(compare %in% compare_filter_sp) %>%
  mutate(
    compare_id = paste(eco_name_1," vs. \n", eco_name_2, sep = "")
  )

glimpse(bays_95_overlap_sp_f)

# ---- PLOT TO CONIFRM THE DATA WE HAVE ----

bays_95_overlap_sp_f %>%
  ggplot() +
  geom_vline(xintercept = seq(1.5, 15, 1), linetype = "dotted") +
  stat_pointinterval(aes(x = compare_id,
                         y = prop_overlap,
                         # point_fill = common_name_1,

  ),
  # .width = c(0.5, 0.95),
  # width = 1.2,
  point_size = 4,
  interval_colour = "grey60",
  position = position_dodge(0.4),
  # stroke = 1,
  shape = 21
  )

# ---- CREATE summary dataframe ----
glimpse(bays_95_overlap_sp_f)

mode_stat <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# laymen_bc_split %>%
#   map(~ ci(.x$post_est, method = "ETI")) %>%
#   bind_rows(.id = "name") %>%
#   separate_wider_delim(name, names = c("community", "metric"), delim = ":") %>%
#   janitor::clean_names() %>%
#   left_join(c_names) %>%
#   mutate(
#     differences = round(ci_high - ci_low, 2),
#     middle = round(differences / 2, 2),
#     mid = round(ci_low + middle, 2)
#   )



overlap_sum <- bays_95_overlap_sp_f %>%
  group_by(eco_name_1, eco_name_2, compare_id) %>%
  summarise(
    median_overlap = median(prop_overlap)  %>%
      round(digits = 2),
    mode_overlap = mode_stat(prop_overlap) %>%
      round(digits = 2),
    n = n(),
    mean_overlap = mean(prop_overlap) %>%
      round(digits = 2),
    sem_overlap = (sd(prop_overlap) / sqrt(n())) %>%
      round(digits = 3),

  ) %>%
  ungroup() %>%
  mutate(
    eco_name = paste(eco_name_1, eco_name_2, sep = ":")
  )


overlap_sum


ci_overlap_sum <- bays_95_overlap_sp_f %>%
  mutate(
    eco_name = paste(eco_name_1, eco_name_2, sep = ":")
  ) %>%
  split(.$eco_name) %>%
  map(~ ci(.x$prop_overlap, method = "ETI")) %>%
  bind_rows(.id = "eco_name") %>%
  separate_wider_delim(eco_name, names = c("eco_name_1", "eco_name_2"),
                       delim = ":", cols_remove = FALSE) %>%
  janitor::clean_names() %>%
  mutate(
    differences = round(ci_high - ci_low, 2),
    middle = round(differences / 2, 2),
    mid = round(ci_low + middle, 2)
  ) %>%
  left_join(overlap_sum) %>%
  mutate(
    eco_name_1 = factor(eco_name_1,
                        levels = c("Anthropogenic",
                                   "Inlet",
                                   "Open-Coastal",
                                   "Deep-Hole",
                                   "Outlet",
                                   "Embayment")),
    eco_name_2 = factor(eco_name_2,
                        levels = c("Anthropogenic",
                                   "Inlet",
                                   "Open-Coastal",
                                   "Deep-Hole",
                                   "Outlet",
                                   "Embayment"))
  )



ci_overlap_sum
overlap_sum


p <- ggplot(data = ci_overlap_sum, aes(x = eco_name_1,
                                  y = mid,
                                  fill = eco_name_2)) +
         geom_point(size = 4, shape = 21,
                    position = position_jitter(width = 0.1,
                                               seed = 2)) +
  scale_fill_viridis_d(begin = 0.1,
                       # end = 0.9,
                       name = "Ecoregion", alpha = 0.65) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.92, 0.82)
  ) +
  labs(
    x = "Ecoregion",
    y = "Median Niche Similarity"
  )

p
ggsave(filename = here("plots",
                       "top-three",
                       "niche-similarity",
                       "median_ecoregion_similarity.png"),
       width = 12, height = 6, plot = p)

viridis_colors <- viridis(5, begin = 0.35,
                          # end = 0.9,
                             # option = "",
                             alpha = 0.75
)


# ---- point interval ----
p1 <- ggplot(data = bays_95_overlap_sp_f, aes(x = eco_name_1,
                                  y = prop_overlap,
                                  point_fill = eco_name_2)) +
         stat_pointinterval(
                    # position = position_jitter(width = 0.1,
                    #                            seed = 2),
                    point_size = 4,
                    interval_colour = "grey60",
                    position = position_dodge(0.7),
                    # stroke = 1,
                    shape = 21,) +
  scale_fill_manual(aesthetics = "point_fill",
                    values = viridis_colors,
                    name = "Ecoregion") +
  geom_vline(xintercept = seq(1.5, 5, 1), linetype = "dotted") +
  geom_hline(yintercept = 0.6, linetype = "dashed") +

  # scale_fill_viridis_d(begin = 0.1,
  #                      # end = 0.9,
  #                      name = "Ecoregion", alpha = 0.65) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_blank(),
    legend.position.inside = c(0.925, 0.86)
  ) +
  labs(
    x = "Ecoregion",
    y = "Niche Similarity p(%|X)"
  )


p1

ggsave(filename = here("plots",
                       "top-three",
                       "niche-similarity",
                       "median_ecoregion_similarity_ci.png"),
       width = 12, height = 6, plot = p1)



qs::qsave(p1, here::here("data-saved",
                          "niche-similarity-ecoregion-plots",
                          "niche_similarity_ecoregion_plot.qs"))

ggplot(data = bays_95_overlap_sp_f, aes(x = eco_name_1,
                                        y = prop_overlap,
                                        fill = eco_name_2)) +
  geom_violin(
    # position = position_jitter(width = 0.1,
    #                            seed = 2),
    # point_size = 4,
    # interval_colour = "grey60",
    position = position_dodge(0.7),
    # stroke = 1,
    # shape = 21,
    ) +
  scale_fill_viridis_d(begin = 0.35,
                    name = "Ecoregion",
                    alpha = 0.5) +
  geom_vline(xintercept = seq(1.5, 5, 1), linetype = "dotted") +
  # scale_fill_viridis_d(begin = 0.1,
  #                      # end = 0.9,
  #                      name = "Ecoregion", alpha = 0.65) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_blank(),
    legend.position.inside = c(0.925, 0.86)
  ) +
  labs(
    x = "Ecoregion",
    y = "Niche Similarity p(%|X)"
  )




overlap_sum_sp <- bays_95_overlap_sp_f %>%
  group_by(common_name_1) %>%
  summarise(
    median_overlap = median(prop_overlap)  %>%
      round(digits = 2),
    mode_overlap = mode_stat(prop_overlap) %>%
      round(digits = 2),
    n = n(),
    mean_overlap = mean(prop_overlap) %>%
      round(digits = 2),
    sem_overlap = (sd(prop_overlap) / sqrt(n())) %>%
      round(digits = 3),

  ) %>%
  ungroup()


overlap_sum_sp

ci()


ci_overlap_sum_sp <- bays_95_overlap_sp_f %>%
  split(.$common_name_1) %>%
  map(~ ci(.x$prop_overlap, method = "ETI")) %>%
  bind_rows(.id = "common_name_1") %>%
  janitor::clean_names() %>%
  mutate(
    differences = round(ci_high - ci_low, 2),
    middle = round(differences / 2, 2),
    mid = round(ci_low + middle, 2)
  ) %>%
  left_join(overlap_sum_sp)

overlap_sum_sp
ci_overlap_sum_sp

bays_95_overlap_sp_f %>%
  ggplot(aes(x = eco_name_1,
             colour = eco_name_2,
             y = prop_overlap),
         shape = 21, stroke = 1.2) +
  geom_jitter()

