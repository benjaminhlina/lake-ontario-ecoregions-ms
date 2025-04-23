{
  library(dplyr)
  library(ggdist)
  library(here)
  library(qs)
  library(tidybayes)
  library(tidyr)
}

# ----- bring in model ---- 
model <- qread(here("data-saved",
                    "trophic-position-top-three",
                    "brms-trps",
                    "tp_model_bmrs_trps.qs"))

str(model)

# ------ extract posteriors for tp and alpha ----- 

get_variables(model[[1]])
post_draws <- model %>%
  purrr::map(~ .x %>%
               gather_draws(b_alpha_ar_Intercept,
                            b_d15n_tp_Intercept,
                            sigma_alpha,
                            sigma_d15n)
  ) %>%
  bind_rows(.id = "name") %>%
  separate_wider_delim(name, names = c( "ecoregion", "common_name"),
                       delim = "_") %>%
  mutate(
    common_name = factor(common_name,
                         levels = c("Round Goby", "Alewife", "Lake Trout")),
    ecoregion = factor(ecoregion,
                       levels = c("Anthropogenic", "Inlet", "Open-Coastal",
                                  "Deep-Hole", "Outlet", "Embayment")),
    .variable = case_when(
      .variable %in% "b_alpha_ar_Intercept" ~ "ac",
      .variable %in% "b_d15n_tp_Intercept" ~ "tp",
      .default = .variable
    )
  ) %>%
  arrange(ecoregion, common_name)



post_draws


# ----- save posteriors 
qs::qsave(post_draws, here("data-saved",
                           "trophic-position-top-three",
                           "brms-trps",
                           "tp_brms_trps_post_draws.qs"))
