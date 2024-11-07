# ---- load packages -----
{
  library(bayestestR)
  library(brms)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(here)
  library(jagsUI)
  library(nichetools)
  library(purrr)
  library(postpack)
  library(readr)
  library(SIBER)
  library(sf)
  library(tidyr)
  library(tRophicPosition)
  library(viridis)
  source(here::here("functions",
                    "siber_fit_transform_bays.R"))
}



# ---- BRING IN zebra mussels ----

# ---- model outut ----


zm_summary <- qs::qread(here("data-saved",
                             "zebra-mussels",
                             "filtered_quant_raw_zebra_mussel_data.qs"))
# ---- bring in data ----
df <- read_rds(here("data-saved",
                    "top-three",
                    "top_three_cleaned_sia.rds")) %>%
  mutate(
    ecoregion = factor(ecoregion, levels =
                         c("Anthropogenic",
                           "Inlet",
                           "Open-Coastal",
                           "Deep-Hole",
                           "Outlet",
                           "Embayment")
    ))




# next we are going to select the columns we need for siber
# ---- select what we need for siber ----


# ---- join zebra mussels ----
glimpse(df)
glimpse(zm_summary)

zm_summary <- zm_summary %>%
  select(common_name, ecoregion, cal_d13c, cal_d15n) %>%
  rename(
    d13c = cal_d13c,
    d15n = cal_d15n
  )

zm_summary %>%
  distinct(common_name)

glimpse(df)

df_wide <- df %>%
  select(common_name, ecoregion, fish_guilds, lfree_d13c,
         lfree_d15n,rel_d13c_lfree, rel_d15n_lfree, habitat_type,
         rwt, tlen,
         trophic_guild,
          ) %>%
  rename(
    d13c = lfree_d13c,
    d15n = lfree_d15n
  ) %>%
  bind_rows(., zm_summary) %>%
  mutate(
    common_name = if_else(common_name %in% c("D. polymorpha",
                                             "D. bugensis",
                                             "Quagga mussel",
                                             "Dreissena sp.",
                                             "Dreissena bugensis"),
                          false = common_name,
                          true = "Dreissenids"),
    fish_g = paste(habitat_type, trophic_guild, sep = " "),
    community_name = paste(common_name, ecoregion, sep = "-"),
    fg = case_when(
      common_name == "Dreissenids" ~ "Benthic_BL",
      common_name == "Alewife" ~ "Alewife",
      common_name == "Round Goby" ~ "Round Goby",
      common_name == "Lake Trout" ~ "Lake Trout",
    )
  )

# ---- need to create columns that are the mean



# df_long <- df_wide %>%
#   pivot_longer(cols = -c(common_name, ecoregion,
#                          fish_guilds),
#                names_to = "isotope")


unique(df_wide$common_name)

df_wide <- df_wide %>%
  left_join(df_wide %>%
              filter(common_name == "Dreissenids") %>%
              group_by(ecoregion) %>%
              summarise(
                b1 = mean(d15n)
              ) %>%
              ungroup()
  ) %>%
  mutate(
    fish_g = if_else(fish_g %in% "NA NA", true = "Benthic Planktivore",
                     false = fish_g)
  )

df_wide_update <- df_wide %>%
  group_by(common_name, ecoregion) %>%
  mutate(
    delta_n = 3.4,
    lambda = 2,
    # (2 + ((d15n - (tp2_b + tp2_p)) / 3.4)),
    tp = lambda + (d15n - b1) / delta_n,
  ) %>%
  ungroup() %>%
  as.data.frame()





df_wide_update


ggplot() +
  stat_summary(
    geom = "point",
    fun = mean,
    size = 3,
    position = position_jitter(width = 0.2),
    data = df_wide_update,
    aes(x = common_name, y = tp,
        colour = ecoregion)

  )

df_wide_update
dput(unique(df_wide$common_name))

df_wide_summary <- df_wide_update %>%
  mutate(
    common_name = factor(common_name,
                         levels = c("Round Goby","Alewife",
                                    "Lake Trout", "Dreissenids"))
  ) %>%
  group_by(common_name, ecoregion, fish_g) %>%
  summarise(
    n = n(),
    mean_len = mean(tlen, na.rm = TRUE) %>%
      round(digits = 0),
    min_len = min(tlen,  na.rm = TRUE) %>%
      round(digits = 0),
    max_len = max(tlen,  na.rm = TRUE) %>%
      round(digits = 0),
    sem_len = (sd(tlen,  na.rm = TRUE) / sqrt(n())) %>%
      round(digits = 0),
    mean_wt = mean(rwt,  na.rm = TRUE) %>%
      round(digits = 0),
    min_wt = min(rwt,  na.rm = TRUE) %>%
      round(digits = 0),
    max_wt = max(rwt,  na.rm = TRUE) %>%
      round(digits = 0),
    sem_wt = (sd(rwt,  na.rm = TRUE) / sqrt(n())) %>%
      round(digits = 0),

    mean_rel_d13c = mean(rel_d13c_lfree) %>%
      round(digits = 2),
    rel_d13_sem = (sd(rel_d13c_lfree) / sqrt(n())) %>%
      round(digits = 2),
    mean_d13c = mean(d13c) %>%
      round(digits = 2),
    sem_d13c = (sd(d13c) / sqrt(n())) %>%
      round(digits = 2),
    mean_d15n = mean(d15n) %>%
      round(digits = 2),
    sem_d15n = (sd(d15n) / sqrt(n())) %>%
      round(digits = 2)
  ) %>%
  ungroup() %>%
  arrange(common_name, ecoregion) %>%
  mutate(
    weight = paste(mean_wt, " ± ", sem_wt, " (", min_wt, " - ",
                   max_wt, ")", sep = "")
  ) %>%
  print(n =  30)

# "invertivore"
df_wide_summary

openxlsx::write.xlsx(df_wide_summary,
                     here("results",
                          "top-three",
                          "summary",
                          "top_three_summary_table_fig_1.xlsx"))
