# ---- load packages -----
{
  library(bayesplot)
  library(brms)
  library(dplyr)
  library(ggplot2)
  library(here)
  library(purrr)
  library(qs)
  library(readr)
  library(tidyr)
  library(trps)
}

# ---- bring in data ---- 

df <- qread(
  here(
    'data-saved',
    'top-three',
    'top_three_with_baseline_samples.qs')
)

glimpse(df)

# ---- we need to make df be wide for trps with baselines ---- 
df_conumer <- df %>% 
  filter(sample_types == "Consumers") %>% 
  group_by(ecoregion, 
           common_name) %>% 
  mutate(
    id = row_number()
  ) %>% 
  ungroup()



# ---- df bb ----- 

df_bb <- df %>% 
  filter(sample_types == "Benthic Baseline") %>% 
  group_by(ecoregion) %>% 
  mutate(
    id = row_number()
  ) %>% 
  ungroup() %>% 
  rename(
    d13c_b1 = d13c,
    d15n_b1 = d15n
  ) %>% 
  select(id, ecoregion, d13c_b1, d15n_b1)
df_bb 
df_bb %>% 
  filter(ecoregion == "Deep-Hole")

# ---- df pb ----- 

df_pb <- df %>% 
  filter(sample_types == "Pelagic Baseline") %>% 
  group_by(ecoregion) %>% 
  mutate(
    id = row_number()
  ) %>% 
  ungroup() %>% 
  rename(
    d13c_b2 = d13c,
    d15n_b2 = d15n
  ) %>% 
  select(id, ecoregion, d13c_b2, d15n_b2)

df_pb

df_pb %>% 
  filter(ecoregion == "Deep-Hole")

# ---- combine to create wide dataframe ------ 

df_full <- df_conumer %>% 
  left_join(df_bb) %>% 
  left_join(df_pb) %>% 
  
  select(common_name, ecoregion, d13c, d15n, d13c_b1:d15n_b2) %>% 
  group_by(
    common_name, 
    ecoregion
  ) %>% 
  mutate(
    c1 = mean(d13c_b1, na.rm = TRUE), 
    c2 = mean(d13c_b2, na.rm = TRUE), 
    n1 = mean(d15n_b1, na.rm = TRUE), 
    n2 = mean(d15n_b2, na.rm = TRUE),
    l1 = 2, 
    l2 = 2.5, 
    name = paste(ecoregion, common_name, sep = "_")
  ) %>% 
  add_alpha() %>% 
  ungroup() %>% 
  arrange(
    ecoregion, common_name
  ) %>% 
  select(name, ecoregion, common_name, d13c, d15n, c1:l2, alpha:max_alpha)


df_full

df_full %>% 
  filter(name == "Deep-Hole_Lake Trout")

# ----- we are read to run trps ------ 

m <- df_full %>% 
  split(.$name) %>% 
  map(~ brm(
    formula = two_source_model_ar(lambda = 2),
    prior = two_source_priors_ar(),
    stanvar = two_source_priors_params_ar(), 
    family = gaussian(),
    warmup = 1000, 
    iter = 5000, 
    data = .x, 
    chains = 2,
    cores = 8, 
    seed = 4,
    control = list(adapt_delta = 0.95)
  ), 
  .progress = TRUE
  )
beepr::beep()
# ---- view trace plots ---- 

m %>% 
  iwalk(~ {
    plot(.x) 
    grid::grid.text(.y, x = 0.50, y = 0.98)
  })

# ----- view pp checks ----- 
m %>%  
  imap(~ {.x %>% 
      pp_check(resp = "alpha") +
      ggtitle(.y) +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  }
  )
m %>% 
  imap(~ {.x %>% 
      pp_check(resp = "d15n") +
      ggtitle(.y) +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  }
  )

# ----- save model ---- 
qs::qsave(m, here("data-saved",
                  "trophic-position-top-three",
                  "brms-trps",
                  "tp_model_bmrs_trps.qs"))

