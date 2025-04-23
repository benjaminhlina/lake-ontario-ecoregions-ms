{
  library(qs)
  library(ggplot2)
  library(dplyr)
  library(here)
}

# ----- posterior draws ----- 

post_draws <- qread(here("data-saved",
                         "trophic-position-top-three",
                         "brms-trps",
                         "tp_brms_trps_post_draws.qs")) %>% 
  ungroup()

post_draws

p <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dotdash",
             alpha = 0.5) +
  geom_vline(xintercept = seq(1.5, 3, 1), linetype = "dotted") +
  geom_violin(data = post_draws %>%
                filter(.variable == "ac"),
              aes(x = common_name,
                  y = .value,
                  fill = ecoregion),
              alpha = 0.65,
              position = position_dodge(0.7)) +
  stat_summary(data = post_draws %>%
                 filter(.variable == "ac"),
               geom = "point",
               fun = median,
               aes(x = common_name,
                   y = .value,
                   group = ecoregion),
               
               position = position_dodge(0.7),
               shape = 21,
               fill = "black",
               # fill = "white",
               size = 2) +
  scale_fill_brewer(
    name = "Ecoregion",type = "div", palette = 7
  ) +
  theme_bw(
    base_size = 15
  ) +
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(1, 3),
                  clip = "off"
  ) +
  annotate(geom = "segment", x = I(0.10),
           y = 0.10, xend = I(0.10),
           yend = 0.90,
           col = "black",
           linewidth = 0.8,
           arrow = arrow(length = unit(0.3, "cm"),
                         ends = "both")) +
  annotate(geom = "text", x = I(0.10), label = "Pelagic",
           y = 0.925,
           col = "black", size = 6,
  ) +
  annotate(geom = "text", x = I(0.10), label = "Benthic",
           y = 0.075,
           col = "black", size = 6,
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.85),
    plot.title = element_text(hjust = 0.5),
    legend.background = element_blank(),
    panel.border = element_rect(linewidth = 0.5),
    plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 1.5, "cm"),
  ) +
  labs(
    y =  expression(p(alpha[c] ~ "|" ~ X)),
    x = "Species"
    
  )


p + ggview::canvas(width = 11, height = 7)

ggsave(here("plots",
            "top-three",
            "trophic-position",
            "two_source_alpha_corrected_brms.png"), 
       width = 11, height = 7,
       plot = p)


# ----- trophic position ------ 
p1 <- ggplot() +
  geom_violin(data = post_draws %>%
                filter(.variable == "tp"),
              aes(x = common_name,
                  y = .value,
                  fill = ecoregion),
              alpha = 0.65,
              position = position_dodge(0.7)) +
  stat_summary(data = post_draws %>%
                 filter(.variable == "tp"),
               geom = "point",
               fun = median,
               aes(x = common_name,
                   y = .value,
                   group = ecoregion),
               
               position = position_dodge(0.7),
               shape = 21,
               fill = "black",
               size = 2) +
  scale_fill_brewer(
    name = "Ecoregion",type = "div", palette = 7
  ) +
  geom_vline(xintercept = seq(1.5, 3, 1), linetype = "dotted") +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.85),
    legend.background = element_blank(),
  ) +
  labs(
    y =  expression(p("Trophic Position" ~ "|" ~ X)),
    x = "Species"
    
  )

p1 + ggview::canvas(width = 11, height = 7)


ggsave(here("plots",
            "top-three",
            "trophic-position",
            "two_source_tp_brms.png"), width = 11, height = 7,
       plot = p1)
