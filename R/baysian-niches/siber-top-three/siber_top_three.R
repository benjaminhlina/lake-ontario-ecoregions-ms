# ---- load packages -----
{
  library(broom.mixed)
  library(bayestestR)
  library(data.table)
  library(dplyr)
  library(DHARMa)
  library(emmeans)
  library(fitdistrplus)
  library(furrr)
  library(ggplot2)
  library(ggh4x)
  # library(gghdr)
  library(ggdist)
  library(ggtext)
  library(gratia)
  library(here)
  library(glmmTMB)
  library(purrr)
  library(patchwork)
  library(readr)
  library(stringr)
  library(SIBER)
  library(sf)
  library(tidyr)
  library(vegan)
}

# ---- bring in data ----
df <-  read_rds(here("data-saved",
                     "top-three",
                     "top_three_cleaned_sia.rds")
) %>%
  mutate(
    ecoregion = factor(ecoregion, levels =
                         c("Anthropogenic",
                           "Inlet",
                           "Open-Coastal",
                           "Deep-Hole",
                           "Outlet",
                           "Embayment")
    ))


df %>%
  group_by(common_name, ecoregion) %>%
  summarise(
    min_c = min(lfree_d13c),
    max_c = max(lfree_d13c)
  )
# mutate(
#   common_name = forcats::fct_relevel(common_name,
#                                      c("Rainbow Smelt", "Deepwater Sculpin",
#                                        "Slimy Sculpin", "Spottail Shiner",
#                                        "Rock Bass", "Yellow Perch", "Walleye",
#                                        "Round Goby",
#                                        "Alewife",
#
#                                        "Chinook salmon", "Lake Trout"))
# )

# ---- look at sample numbers ----
df %>%
  group_by(common_name, ecoregion) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  arrange(common_name, n) %>%
  print(n = 54)

glimpse(df)

# ----- select what we need for siber ----


df_select <- df %>%
  dplyr::select(cal_d13c, cal_d15n, common_name, ecoregion) %>%
  mutate(

  ) %>%
  as.data.frame() %>%
  rename(
    iso1 = cal_d13c,
    iso2 = cal_d15n,
    group = common_name,
    community = ecoregion
  )

# ---- create siber object ----
siber.example <- createSiberObject(df_select)

# ---- create group metrics ----

group_ml <- groupMetricsML(siber.example)



group_convert <- group_ml %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "metric") %>%
  pivot_longer(cols = -metric,
               names_to = "community_group",
               values_to = "estimates") %>%
  separate_wider_delim(community_group, delim = ".", names = c("community",
                                                               "group")) %>%
  arrange(community, group, metric)

group_convert
# ---- create community metrics ---

community_ml <- communityMetricsML(siber.example)

comunity_df <- community_ml %>%
  as.data.frame() %>%
  mutate(
    names = rownames(.)
  ) %>%
  pivot_longer(cols = -names,
               names_to = "community",
               values_to = "estimate")

comunity_df
unique(comunity_df$names)

qs::qsave(comunity_df, here("data-saved",
                            "siber-top-three",
                            "community_metrics_layman.qs"))

# ---- bayesian ellipse estimates -----
# ---- create priors -----
# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# ---- fit ellipse -----
# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the
# means. Fitting is via the JAGS method.

ellipses.posterior <- siberMVN(siber.example, parms, priors)
ellipses.posterior

glimpse(ellipses.posterior)

# ---- estimate ellipse area ----
sea_b <- siberEllipses(ellipses.posterior)

glimpse(sea_b)
# ---- extract ellipse area ----
seb_dat <- as.data.frame(sea_b)


names(seb_dat) <-  colnames(group_ml)

glimpse(seb_dat)

seb_convert <- seb_dat %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  pivot_longer(cols = -id,
               names_to = "community_group",
               values_to = "sea") %>%
  separate_wider_delim(community_group, delim = ".", names = c("community",
                                                               "group")) %>%
  arrange(community, group, id) %>%


seb_convert





qs::qsave(seb_convert, here("data-saved",
                            "siber-top-three",
                            "bays_ellipse_siber_extract.qs"))


seb_convert_grouped <- seb_convert %>%
  mutate(
    name = paste(group, community, sep = ":")
  ) %>%
  split(.$name)
  group_split(group, ecoregion)


seb_convert_grouped

ci_intervals_sea <- seb_convert_grouped %>%
  map(~ ci(.x$sea, method = "ETI")) %>%
  bind_rows(.id = "name") %>%
  separate_wider_delim(name, names = c("common_name", "ecoregion"), delim = ":") %>%
  janitor::clean_names() %>%
  mutate(
    differences = round(ci_high - ci_low, 2),
    middle = round(differences / 2, 2),
    mid = round(ci_low + middle, 2)
  )

ci_intervals_sea


# ---- filter out seac ----

seac <- group_convert %>%
  filter(metric == "SEAc")

qs::qsave(seac, here("data-saved",
                     "siber-top-three",
                     "centers_ellipse_siber_extract.qs"))


ci_intervals_sea <- ci_intervals_sea %>%
  left_join(seac, by = c("common_name" = "group",
                         "ecoregion" = "community"))

ci_intervals_sea

openxlsx::write.xlsx(ci_intervals_sea, here("results",
                                            "top-three",
                                            "siber-sea",
                                            "post_edi_sea_spp_ecoregion.xlsx"))
# ---- extract mu ---

mu_post <- extractPosteriorMeans(siber.example, ellipses.posterior)

mu_post

str(mu_post)
# ---- layman metrics ----

laymen_b <- bayesianLayman(mu.post = mu_post)
#
# sum(laymen_b[[1]][,"TA"] <
#       laymen_b[[2]][,"TA"]) /
#   nrow(laymen_b[[1]])

c_name <- df %>%
  distinct(ecoregion) %>%
  arrange(
    ecoregion
  ) %>%
  mutate(
    community = as.character(1:nrow(.))
  )


extract_layman <- function(data,
                           community_names) {
  df_laymen <- data %>%
    purrr::map(~ as_tibble(.x)) %>%
    bind_rows(.id = "community") %>%
    left_join(community_names, by = "community") %>%
    pivot_longer(
      cols = -c(ecoregion, community),
      names_to = "metric",
      values_to = "estimates"
    )
  return(df_laymen)
}

laymen_bc <- extract_layman(laymen_b, community_names = c_name)

laymen_bc_sum <- laymen_bc %>%
  group_by(ecoregion, metric) %>%
  summarise(
    post_meadian = median(estimates)
  ) %>%
  ungroup()



laymen_bc_split <- laymen_bc %>%
   mutate(
     names = paste(ecoregion, metric, sep = ":")
     ) %>%
  split(.$names)


ci_intervals <- laymen_bc_split %>%
  map(~ ci(.x$estimates, method = "ETI")) %>%
  bind_rows(.id = "name") %>%
  separate_wider_delim(name, names = c("ecoregion", "metric"), delim = ":") %>%
  janitor::clean_names() %>%
  mutate(
    differences = round(ci_high - ci_low, 2),
    middle = round(differences / 2, 2),
    mid = round(ci_low + middle, 2)
  )

ci_intervals_mids <- ci_intervals %>%
  left_join(laymen_bc_sum) %>%
  arrange(metric, ecoregion)




openxlsx::write.xlsx(ci_intervals_mids, here("results",
                                             "top-three",
                                             "siber-layman-bays",
                                             "credible_intervals_layman_metric.xlsx"))


#
qs::qsave(laymen_bc, here("data-saved",
                          "siber-top-three",
                          "bays_community_metrics.qs"))


# ---- ellipse overlap ----

#
# library(tibble)
# library(gtools)
#
# # Create the tibble
# ecoregions <- tibble(ecoregion = factor(c("Anthropogenic", "Inlet", "Open-Coastal", "Deep-Hole", "Outlet", "Embayment")))
#
# # Generate all unique combinations of ecoregions
# combinations <- combinations(n = nrow(ecoregions), r = 2, v = ecoregions$ecoregion)
#
# # Convert to a tibble
# combinations_tibble <- as_tibble(combinations)
# colnames(combinations_tibble) <- c("Ecoregion1", "Ecoregion2")
#
# print(combinations_tibble)

# The overlap of the maximum likelihood fitted standard ellipses are

# estimated using


cg_names <- df %>%
  distinct(ecoregion, common_name) %>%
  rename(
    community = ecoregion,
    group = common_name
  ) %>%
  arrange(community, group)

cg_names_within_eco <- cg_names %>%
  mutate(
    cg_1 = paste(community, group, sep = "."),
    cg_2 = paste(community, group, sep = "."),
  ) %>%
  dplyr::select(cg_1, cg_2) %>%
  expand(cg_1, cg_2) %>%
  filter(cg_1 != cg_2) %>% # keep when cg_1 and cg_2 are not the same
  separate_wider_delim(cg_1, delim = ".", names = c("c_1", "g_1"),
                       cols_remove = FALSE) %>%
  separate_wider_delim(cg_2, delim = ".", names = c("c_2", "g_2"),
                       cols_remove = FALSE) %>%
  filter(!(c_1 != c_2 )) %>%
  dplyr::select(-c("c_1", "c_2", "g_1", "g_2"))


cg_names_within_eco


# cg_names_within_eco %>%
#   mutate(
#     id = paste(cg_1, cg_2, sep = "_")
#   ) %>%
#   split(.$id) %>%
#   map(~ dplyr::select(.x, -id))


# Split for purrr
cg_eco_split <- cg_names_within_eco %>%
  mutate(
    names = paste(cg_1, cg_2, sep = "_")
  ) %>%
  split(.$names) %>%
  map(~ .x %>%
        dplyr::select(-names))





# grab names

head(cg_eco_split)

# ---- maximum likeilhood for within eco ----

ml_within_eco_overlap <- cg_eco_split %>%
  map(~ maxLikOverlap(.x$cg_1, .x$cg_2, siber.example,
                      p.interval = NULL, n = 100), .progress = TRUE)

# ---- extract maximum likilhood compare ----
ml_95_within_eco <- ml_within_eco_overlap %>%
  bind_rows(.id = "compare") %>%
  separate_wider_delim(compare, delim = "_", names = c("compare_1",
                                                       "compare_2")) %>%
  separate_wider_delim(compare_1, delim = ".", names = c("eco_name_1",
                                                         "common_name_1")) %>%
  separate_wider_delim(compare_2, delim = ".", names = c("eco_name_2",
                                                         "common_name_2")) %>%
  mutate(
    prop_overlap = overlap / ((area.2 + area.1) - overlap)
  )

ml_95_within_eco

qs::qsave(ml_95_within_eco, here("data-saved",
                      "siber-overlap-bays",
                      "overlap_ml_95_within_sp_compare_eco.qs"))
#
#
#

# ---- corresponding Bayesian estimates for the overlap ----
# between the 95% ellipses is given by:
bayes95.overlap <- cg_eco_split %>%
  map(~ bayesianOverlap(.x$cg_1, .x$cg_2, ellipses.posterior,
                        draws = 100, p.interval = 0.95,
                        n = 100),
      .progress = TRUE
  )

qs::qsave(bayes95.overlap, here("data-saved",
                            "siber-overlap-bays",
                            "overlap_95_within_sp_compare_eco.qs"))


# t <- (bayes95.overlap[[1]][,3] / (bayes95.overlap[[1]][,2] +
#                                     bayes95.overlap[[1]][,1] -
#                                     bayes95.overlap[[1]][,3])
# )
# t
#
bays_95_overlap <- bayes95.overlap %>%
  bind_rows(.id = "compare") %>%
  separate_wider_delim(compare, delim = "_", names = c("compare_1",
                                                       "compare_2"),
                       cols_remove = FALSE) %>%
  separate_wider_delim(compare_1, delim = ".", names = c("eco_name_1",
                                                         "common_name_1"),
                       cols_remove = FALSE) %>%
  separate_wider_delim(compare_2, delim = ".", names = c("eco_name_2",
                                                         "common_name_2"),
                       cols_remove = FALSE) %>%
  mutate(
    prop_overlap = overlap / ((area2 + area1) - overlap),
  ) %>%
  ungroup()


qs::qsave(bays_95_overlap, here("data-saved",
                                "siber-overlap-bays",
                                "correct_overlap_95_bays_within_eco.qs"))

ggplot(data = bays_95_overlap %>%
         filter(common_name_1 == "Round Goby" & common_name_2 == "Lake Trout"),
       aes(x = prop_overlap)) +
  geom_histogram() +
  facet_wrap(~ eco_name_1)

cg_names
# ---- look at species ecoregions ----
cg_names_within_sp <- cg_names %>%
  mutate(
    cg_1 = paste(community, group, sep = "."),
    cg_2 = paste(community, group, sep = "."),
  ) %>%
  dplyr::select(cg_1, cg_2) %>%
  expand(cg_1, cg_2) %>%
  filter(cg_1 != cg_2) %>% # keep when cg_1 and cg_2 are not the same
  separate_wider_delim(cg_1, delim = ".", names = c("c_1", "g_1"),
                       cols_remove = FALSE) %>%
  separate_wider_delim(cg_2, delim = ".", names = c("c_2", "g_2"),
                       cols_remove = FALSE) %>%
  filter(!(g_1 != g_2 )) %>%
  dplyr::select(-c("c_1", "c_2", "g_1", "g_2"))




glimpse(cg_names_within_sp)

# Split for purrr
cg_sp_split <- cg_names_within_sp %>%
  group_split(cg_1, cg_2)

# grab names
cg_sp_split <- cg_names_within_sp %>%
  mutate(
    name = paste(cg_1, cg_2, sep = "_")
  ) %>%
  split(.$name) %>%
  map(~ .x %>%
        dplyr::select(-name))

head(with_sp_names)
glimpse(cg_sp_split)



# ---- maximum likeilhood for within eco ----

ml_within_sp_overlap <- cg_sp_split %>%
  map(~ maxLikOverlap(.x$cg_1, .x$cg_2, siber.example,
                      p.interval = NULL, n = 100), .progress = TRUE)

# ---- extract maximum likilhood compare ----
ml_95_within_sp <- ml_within_sp_overlap %>%
  bind_rows(.id = "compare") %>%
  separate_wider_delim(compare, delim = "_", names = c("compare_1", "compare_2")) %>%
  separate_wider_delim(compare_1, delim = ".", names = c("eco_name_1",
                                                         "common_name_1")) %>%
  separate_wider_delim(compare_2, delim = ".", names = c("eco_name_2",
                                                         "common_name_2")) %>%
  mutate(
    prop_overlap = overlap / ((area.2 + area.1) - overlap)
  )

ml_95_within_eco

# qs::qsave(ml_95, here("data-saved",
#                       "siber-overlap-bays",
#                       "overlap_ml_95_within_sp_compare_eco.qs"))
#
#
#
glimpse(cg_sp_split)
# ---- corresponding Bayesian estimates for the overlap ----
# between the 95% ellipses is given by:
bayes95.overlap_sp <- cg_sp_split %>%
  map(~ bayesianOverlap(.x$cg_1, .x$cg_2, ellipses.posterior,
                        draws = 100, p.interval = 0.95,
                        n = 100),
      .progress = TRUE
  )

qs::qsave(bayes95.overlap, here("data-saved",
                            "siber-overlap-bays",
                            "overlap_95_within_sp_compare_eco.qs"))

#
# t <- (bayes95.overlap[[1]][,3] / (bayes95.overlap[[1]][,2] +
#                                     bayes95.overlap[[1]][,1] -
#                                     bayes95.overlap[[1]][,3])
# )
# t
#
bays_95_overlap_sp <- bayes95.overlap_sp %>%
  bind_rows(.id = "compare") %>%
  separate_wider_delim(compare, delim = "_", names = c("compare_1",
                                                       "compare_2"),
                       cols_remove = FALSE) %>%
  separate_wider_delim(compare_1, delim = ".", names = c("eco_name_1",
                                                         "common_name_1"),
                       cols_remove = FALSE) %>%
  separate_wider_delim(compare_2, delim = ".", names = c("eco_name_2",
                                                         "common_name_2"),
                       cols_remove = FALSE) %>%
  mutate(
    prop_overlap = overlap / ((area2 + area1) - overlap)
  )

qs::qsave(bays_95_overlap_sp, here("data-saved",
                                   "siber-overlap-bays",
                                   "correct_overlap_95_bays_within_sp.qs"))


