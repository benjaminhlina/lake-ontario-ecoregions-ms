# ---- load packages -----
{
  library(dplyr)
  library(ggplot2)
  library(here)
  library(purrr)
  library(qs)
  library(readr)
  library(tidyr)
}

# ----- bring in benthic baseline ----

bb <- qs::qread(here('data-saved',
                     'benthic-baseline',
                     "cleaned_benthic_baseline.qs")) %>% 
  mutate(
    sample_types = "Benthic Baseline", 
    common_name =  "Dreissenids"
  )

glimpse(bb)
# ---- select what we need from benthic samples ---- 
bb_select <- bb %>% 
  select(common_name, 
         ecoregion, 
         cal_d13c, 
         cal_d15n, 
         sample_types) %>% 
  rename(
    d13c = cal_d13c,
    d15n = cal_d15n,
  )


glimpse(bb_select)

# ---- bring pelagic baseline ----
pb <- qs::qread(here("data-saved",
                     "zebra-mussels",
                     "filtered_quant_raw_zebra_mussel_data.qs")) %>% 
  mutate(
    sample_types = "Pelagic Baseline",
    common_name =  "Amphipoda"
  )
glimpse(pb)
unique(pb$common_name)
# ----- select what we need for pelagic samples ---- 

pb_select <- pb %>% 
  select(
    common_name, 
    ecoregion, 
    cal_d13c, 
    cal_d15n, 
    sample_types
  ) %>% 
  rename(
    d13c = cal_d13c,
    d15n = cal_d15n,
  )

glimpse(pb_select)

# ---- bring in data ----
df <- qread(here("data-saved",
                 "top-three",
                 "top_three_cleaned_sia.qs")) %>% 
  mutate(
    sample_types = "Consumers"
  ) 


glimpse(df)

df_select <- df %>% 
  select(
    common_name, ecoregion, tlen, flen, rwt,
    lfree_d13c, lfree_d15n, 
    sample_types
  ) %>% 
  rename(
    d13c = lfree_d13c,
    d15n = lfree_d15n,
  )

glimpse(df_select)
df_select


# ------ save full dataset with baselines ----- 

df_full <- df_select %>% 
  bind_rows(bb_select, pb_select)  %>% 
  mutate(
    common_name = factor(common_name,
                         levels = c("Round Goby",
                                    "Alewife", 
                                    "Lake Trout", 
                                    "Amphipoda", 
                                    "Dreissenids"))
  )
# save combined data  
df_full %>% 
  qsave(here(
    'data-saved',
    'top-three',
    'top_three_with_baseline_samples.qs'
  ))

# ---- create summary table ----- 

df_select_sum <- df_full %>% 
  group_by(
    common_name, ecoregion
  ) %>% 
  summarise(
    n = n(),
    mean_len = mean(tlen, na.rm = TRUE),
    sem_len = sd(tlen, na.rm = TRUE) / sqrt(n()),
    mean_wt = mean(rwt, na.rm = TRUE),
    sem_wt = sd(rwt, na.rm = TRUE) / sqrt(n()),
    mean_d13c = mean(d13c),
    sem_d13c = sd(d13c) / sqrt(n()),
    mean_d15n = mean(d15n),
    sem_d15n = sd(d15n) / sqrt(n()),
  ) %>% 
  ungroup() %>% 
  mutate(across(is.numeric,round, 2)) %>% 
  mutate(
    length = paste(mean_len, "±", sem_len, sep = " "),
    weight = paste(mean_wt, "±", sem_wt, sep = " "),
    d13c = paste(mean_d13c, "±", sem_d13c, sep = " "),
    d15n = paste(mean_d15n, "±", sem_d15n, sep = " "),
  ) %>% 
  select(
    common_name, ecoregion, n, length:d15n
  )

df_select_sum

df_select_sum %>% 
  openxlsx::write.xlsx(
    here('results',
         'top-three',
         'summary_stats_top_three_with_baseline.xlsx')
  )

# ----- plot raw samples ----- 
p <- ggplot(data = df_full, aes(x = d13c, 
                                y = d15n, 
                                fill = ecoregion)) + 
  geom_point(shape = 21, size = 3,  alpha = 0.5,) + 
  scale_fill_brewer(type = "div", 
                    palette = 5,
                    name = "Ecoregion") + 
  facet_wrap(~ common_name) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = 'inside',
    legend.position.inside = c(0.75, 0.35)
  ) + 
  labs(
    x = expression(paste(delta ^ 13, "C")),
    y = expression(paste(delta ^ 15, "N"))
  )


p + ggview::canvas(width = 11, 
                   height = 8.5)
p %>% 
  ggsave(filename = here('plots',
                         'top-three',
                         'biplot',
                         paste0('biplot_raw_faceted_species_', Sys.Date(), 
                                '.png')), 
         width = 11,
         height = 8.5)


p1 <- ggplot(data = df_full, aes(x = d13c, 
                                 y = d15n, 
                                 fill = common_name)) + 
  geom_point(shape = 21, size = 3,  alpha = 0.5,) + 
  scale_fill_brewer(type = "div", 
                    palette = 5,
                    name = "Species") + 
  facet_wrap(~ ecoregion) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = 'inside',
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position.inside = c(0.93, 0.1)
  ) + 
  labs(
    x = expression(paste(delta ^ 13, "C")),
    y = expression(paste(delta ^ 15, "N"))
  )


p1 + ggview::canvas(width = 11, 
                   height = 8.5)
p1 %>% 
  ggsave(filename = here('plots',
                         'top-three',
                         'biplot',
                         paste0('biplot_raw_faceted_ecoregion_', Sys.Date(), 
                                '.png')), 
         width = 11,
         height = 8.5)


# ------ resampling ----- 
# 
# Function to bootstrap the last two columns (d13c, d15n)
bootstrap_fun <- function(data, n = 50) {
  replicate(n, {
    boot_sample <- data[sample(nrow(data), replace = TRUE), ]
    colMeans(boot_sample[, c('tlen', 'rwt', "d13c", "d15n")], na.rm = TRUE)
  }, simplify = "data.frame") %>%
    t() %>%
    as.data.frame() %>%
    setNames(c('boot_tlen', 'boot_rwt', "boot_d13c", "boot_d15n"))
}


# Apply bootstrap within each group
df_select_bs <- df_full %>%
  group_by(common_name, ecoregion) %>%
  nest() %>%
  mutate(boot = map(data, bootstrap_fun, 50)) %>%
  select(-data) %>%
  unnest(boot) %>% 
  ungroup()

# ------ save bootstrapped samples ----- 
df_select_bs %>% 
  qsave(here(
    'data-saved',
    'top-three',
    'top_three_with_baseline_samples_bs.qs'
  ))

# ---- create summary ----- 
df_select_sum_bs <- df_select_bs %>% 
  group_by(
    common_name, ecoregion
  ) %>% 
  summarise(
    n = n(),
    mean_len = mean(boot_tlen, na.rm = TRUE),
    sem_len = sd(boot_tlen, na.rm = TRUE) / sqrt(n()),
    mean_wt = mean(boot_rwt, na.rm = TRUE),
    sem_wt = sd(boot_rwt, na.rm = TRUE) / sqrt(n()),
    mean_d13c = mean(boot_d13c),
    sem_d13c = sd(boot_d13c) / sqrt(n()),
    mean_d15n = mean(boot_d15n),
    sem_d15n = sd(boot_d15n) / sqrt(n()),
  ) %>% 
  ungroup() %>% 
  mutate(across(is.numeric,round, 2)) %>% 
  mutate(
    length = paste(mean_len, "±", sem_len, sep = " "),
    weight = paste(mean_wt, "±", sem_wt, sep = " "),
    d13c = paste(mean_d13c, "±", sem_d13c, sep = " "),
    d15n = paste(mean_d15n, "±", sem_d15n, sep = " "),
  ) %>% 
  select(
    common_name, ecoregion, n, length:d15n
  )

df_select_sum_bs

df_select_sum_bs %>% 
  openxlsx::write.xlsx(
    here('results',
         'top-three',
         'summary_stats_top_three_with_baseline_bs.xlsx')
  )

# ----- plot bootstraped samples ---- 
p2 <- ggplot(data = df_select_bs, aes(x = boot_d13c, 
                                y = boot_d15n, 
                                fill = ecoregion)) + 
  geom_point(shape = 21, size = 3,  alpha = 0.5,) + 
  scale_fill_brewer(type = "div", 
                    palette = 5,
                    name = "Ecoregion") + 
  facet_wrap(~ common_name) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = 'inside',
    legend.position.inside = c(0.75, 0.35)
  ) + 
  labs(
    x = expression(paste(delta ^ 13, "C")),
    y = expression(paste(delta ^ 15, "N"))
  )


p2 + ggview::canvas(width = 11, 
                   height = 8.5)
p2 %>% 
  ggsave(filename = here('plots',
                         'top-three',
                         'biplot',
                         paste0('biplot_bootstrap_faceted_species_', Sys.Date(), 
                                '.png')), 
         width = 11,
         height = 8.5)


p3 <- ggplot(data = df_select_bs, aes(x = boot_d13c, 
                                 y = boot_d15n, 
                                 fill = common_name)) + 
  geom_point(shape = 21, size = 3,  alpha = 0.5,) + 
  scale_fill_brewer(type = "div", 
                    palette = 5,
                    name = "Species") + 
  facet_wrap(~ ecoregion) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = 'inside',
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position.inside = c(0.93, 0.35)
  ) + 
  labs(
    x = expression(paste(delta ^ 13, "C")),
    y = expression(paste(delta ^ 15, "N"))
  )


p3 + ggview::canvas(width = 11, 
                    height = 8.5)
p3 %>% 
  ggsave(filename = here('plots',
                         'top-three',
                         'biplot',
                         paste0('biplot_bootstrap_faceted_ecoregion_', Sys.Date(), 
                                '.png')), 
         width = 11,
         height = 8.5)

