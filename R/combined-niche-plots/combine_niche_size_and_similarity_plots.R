# ---- laod packages ----

{
  library(ggplot2)
  library(ggh4x)
  library(here)
  library(patchwork)
  library(qs)
}

# ---- bring in similarity plot -----

p_sim <- qs::qread(here::here("data-saved",
                              "niche-similarity-ecoregion-plots",
                              "niche_similarity_ecoregion_plot.qs"))
p_sim

# ---- bring in nice size plot -----
p_siz <- qs::qread(here::here("data-saved",
                              "niche-size-ecoregion-plots",
                              "niche_size_ecoregion_plot.qs"))

p_siz


# ---- combine ----

p_comb <- p_siz / p_sim +
  plot_annotation(tag_levels = "a",
                  tag_suffix = ")")

p_comb


ggsave(plot = p_comb, filename = here::here("plots",
                          "combined-plots",
                          "sea_niche_similarity_combined.png"), width = 12,
       height = 12)
