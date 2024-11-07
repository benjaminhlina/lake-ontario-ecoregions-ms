# ---- load packages -----
{
  library(bayestestR)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(here)
  library(nichetools)
  library(purrr)
  library(readr)
  library(SIBER)
  library(sf)
  library(tidyr)
  source(here::here("functions",
                    "siber_fit_transform_bays.R"))
}

# ---- bring in data ----
df <-  read_rds(here("data-saved",
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




sample_depth <- df %>%
  group_by(common_name) %>%
  summarise(
    n = n(),
    mean = mean(sidep, na.rm = TRUE),
    sem = sd(sidep, na.rm = TRUE) / sqrt(n()),
    min = min(sidep, na.rm = TRUE),
    max = max(sidep, na.rm = TRUE)
  ) %>%
  ungroup()

openxlsx::write.xlsx(sample_depth, here("results",
                                        "sample-depth",
                                        "sample_depth_min_max.xlsx"))
# ---- create namewith group and commnity dataframe ----
cg_names <- df %>%
  mutate(
    group = as.numeric(common_name) %>%
      as.character(),
    community = as.numeric(ecoregion) %>%
      as.character()
  ) %>%
  distinct(common_name, ecoregion, group,
           community) %>%
  arrange(common_name, ecoregion)

cg_names

# ---- just community names
c_names <- df %>%
  mutate(
    community = as.numeric(ecoregion) %>%
      as.character()
  ) %>%
  distinct(ecoregion,
           community) %>%
  arrange(ecoregion)

c_names

# next we are going to select the columns we need for siber
# ---- select what we need for siber ----
df_select <- df %>%
  dplyr::select(lfree_d13c, lfree_d15n, common_name, ecoregion) %>%
  mutate(
    common_name = as.integer(common_name),
    ecoregion = as.integer(ecoregion)
  ) %>%
  rename(
    iso1 = lfree_d13c,
    iso2 = lfree_d15n,
    group = common_name,
    community = ecoregion,
  ) %>%
  arrange(group, community) %>%
  as.data.frame()

# check if we are getting the correct values
df_select %>%
  distinct(group, community)

# ---- plot df selct to confrim correct stcuture ---
ggplot(data = df_select, aes(x = iso1, y = iso2,
                             colour = factor(group))) +
  geom_point() +
  facet_wrap(~ community)



# ---- create siber object ----
siber_example <- createSiberObject(df_select)
glimpse(siber_example)



# ---- Bayesian ellipse estimates -----
# ---- create priors -----
# options for running jags
parms_1 <- list()
parms_1$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms_1$n.burnin <- 1 * 10^3 # discard the first set of values
parms_1$n.thin <- 10     # thin the posterior by this many
parms_1$n.chains <- 2        # run this many chains

# define the priors
priors_1 <- list()
priors_1$R <- 1 * diag(2)
priors_1$k <- 2
priors_1$tau.mu <- 1.0E-3

# ---- fit ellipse -----
# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the
# means. Fitting is via the JAGS method.

# first we are going to grab zscores for each
test_1 <- siber_example$zscore.data %>%
  bind_rows(.id = "community_id") %>%
  arrange(community, group) %>%
  mutate(
    name = paste(community, group, sep = ".")
  )
# then we are going to convert name we are spliting into afcator
test_1 <- test_1 %>%
  mutate(
    name = factor(name, levels = dput(unique(name)))
  ) %>%
  arrange(name)

# then split this will be what we run baysain estimats off
test <- test_1 %>%
  split(.$name)


# we need make params list so its the same across all
parms_list <- replicate(length(test), parms_1, simplify = FALSE)
priors_list <- replicate(length(test), priors_1, simplify = FALSE)


# ---- Bayesian ellipses ----
mod <- pmap(list(test, parms_list, priors_list), fit_and_transform_group)



# ellipses_posterior <- siberMVN(siber_example, parms, priors)
# ellipses_posterior

glimpse(siber_example)
glimpse(mod)

# ---- extract mu and sigma ----
# tst <- mod %>%
#   map(~ as.data.frame(.x)) %>%
#   bind_rows(.id = "name") %>%
#   separate_wider_delim(name, cols_remove = FALSE,
#                        delim = ".", names = c("group",
#                                               "community"))
# glimpse(tst)

# ---- extract mu ----
df_mu <- extract_mu(mod, pkg = "SIBER", data_format = "wide") %>%
  separate_wider_delim(sample_name, cols_remove = FALSE,
                       delim = ".", names = c("community",
                                              "group")) %>%
  left_join(cg_names)

# ---- add cg_names to df_select for plotting ----
df_select_update <- df_select %>%
  mutate(
    group = as.character(group),
    community = as.character(community),
  ) %>%
  left_join(cg_names)

# ---- Bays mu with raw data to make sure bays est are correct ----
ggplot() +
  geom_point(data = df_mu, aes(x = d13c, y = d15n,
                               colour = common_name)) +
  geom_point(data = df_select_update, aes(x = iso1, y = iso2,
                                          colour = common_name)) +
  facet_grid(common_name ~ ecoregion) +
  scale_colour_viridis_d(option = "A", begin = 0.35, end = 0.85,
                         name = "Species", alpha = 0.75)
ggplot() +
  geom_point(data = df_mu, aes(x = d13c, y = d15n,
                               colour = common_name)) +
  geom_point(data = df_select_update, aes(x = iso1, y = iso2,
                                          colour = common_name)) +
  facet_wrap(~ ecoregion) +
  scale_colour_viridis_d(option = "A", begin = 0.35, end = 0.85,
                         name = "Species", alpha = 0.75)

# ---- extract mu long for ellipse ----

df_mu_long <- extract_mu(mod, pkg = "SIBER", data_format = "long") %>%
  separate_wider_delim(sample_name, cols_remove = FALSE,
                       delim = ".", names = c("community",
                                              "group")) %>%
  left_join(cg_names)


# ---- extract sigma ----

df_sigma <- extract_sigma(mod, pkg = "SIBER")

# ---- create ellipses -----
df_el <- niche_ellipse(dat_mu = df_mu_long,
                       dat_sigma = df_sigma, set_seed = 4, n = 20) %>%
  separate_wider_delim(sample_name, cols_remove = FALSE,
                       delim = ".", names = c("community",
                                              "group")) %>%
  left_join(cg_names)
# df_el_full <- niche_ellipse(dat_mu = df_mu_long,
#                        dat_sigma = df_sigma, random = FALSE) %>%
#   separate_wider_delim(sample_name, cols_remove = FALSE,
#                        delim = ".", names = c("community",
#                                               "group")) %>%
#   left_join(cg_names)


df_el

# df_el_full


# ---- make into sf object ----
df_el_sf <- st_as_sf(df_el, coords = c("d13c", "d15n"))

# convert to polygons
df_el_sf_poly <- df_el_sf %>%
  group_by(sample_name, sample_number, ecoregion, common_name) %>%
  summarise(do_union = FALSE) %>%
  st_cast('POLYGON')  %>%
  ungroup()


df_el_sf_poly <- df_el_sf_poly %>%
  mutate(
    area = st_area(.)
  )
# ---- make into sf object for full
# df_el_full_sf <- st_as_sf(df_el_full, coords = c("d13c", "d15n"))
#
# # convert to polygons
# df_el_full_poly <- df_el_full_sf %>%
#   group_by(sample_name, sample_number, ecoregion, common_name) %>%
#   summarise(do_union = FALSE) %>%
#   st_cast('POLYGON')  %>%
#   ungroup()
#
#
# df_sums <- df_select %>%
#   group_by(group, community) %>%
#   mutate(
#     group = as.character(group),
#     community = as.character(community),
#   ) %>%
#   summarise(
#     n = n()
#   ) %>%
#   ungroup() %>%
#   left_join(cg_names) %>%
#   select(-c("group", "community"))
#
# df_el_full_poly <- df_el_full_poly %>%
#   select(sample_name:area)
#
# df_el_full_poly_1 <- df_el_full_poly %>%
#   left_join(df_sums) %>%
#   mutate(
#     area = st_area(.)
#   )
#
# df_el_fulls <- df_el_full_poly_1 %>%
#   st_drop_geometry()
#
# ggplot(data = df_el_fulls, aes(y = area, x = common_name)) +
#   geom_violin(aes(fill = ecoregion), alpha = 0.5)

# plot ellispes with raw data
ggplot() +
  geom_sf(data = df_el_sf_poly, aes(colour = common_name),
          fill = NA) +
  geom_point(data = df_select_update, aes(x = iso1, y = iso2,
                                          fill = common_name),
             shape = 21) +
  facet_wrap(~ ecoregion) +
  scale_fill_viridis_d(option = "A", begin = 0.35, end = 0.85,
                       name = "Species", alpha = 0.75) +
  scale_colour_viridis_d(option = "A", begin = 0.35, end = 0.85,
                         name = "Species", alpha = 0.75)
# yayay they are correct# yayay theyfill_() are correct

# ---- create group metrics for each ecoregion ----
# using maximum liklihood


group_ml <- groupMetricsML(siber_example)


# convert into dataframe for ease of use
group_convert <- extract_group_metrics(data = group_ml,
                                       community_df = cg_names)

# group_convert %>%
#   distinct(community, group)


group_convert

# ---- create community metrics ---

community_ml <- communityMetricsML(siber_example)
commun
class(community_ml)
# convert into dataframe for ease of use


comunity_df <- community_ml %>%
  as.data.frame() %>%
  mutate(
    names = rownames(.)
  ) %>%
  pivot_longer(cols = -names,
               names_to = "community",
               values_to = "estimate") %>%
  left_join(c_names)

comunity_df

qs::qsave(comunity_df, here("data-saved",
                            "siber-top-three",
                            paste0(
                              "community_metrics_layman_",
                              Sys.Date(), ".qs")))

# ---- Bayesian layman metrics -----
# first extract mu using siber extrat
mu_post <- extractPosteriorMeans(siber_example, mod)
glimpse(mu_post)
# ---- Bayesian estimates of layman metrics ----

laymen_b <- bayesianLayman(mu.post = mu_post)
glimpse(laymen_b)

# code to cmpare TA in bay implement latter
# sum(laymen_b[[1]][,"TA"] <
#       laymen_b[[2]][,"TA"]) /
#   nrow(laymen_b[[1]])


# ---- extract laymen function ----
# # this will be in nichetools
# extract_layman <- function(data,
#                            community_names) {
#   df_laymen <- data %>%
#     purrr::map(~ as_tibble(.x)) %>%
#     bind_rows(.id = "community") %>%
#     left_join(community_names, by = "community") %>%
#     pivot_longer(
#       cols = -c(ecoregion, community),
#       names_to = "metric",
#       values_to = "estimates"
#     )
#   return(df_laymen)
# }

# extratract laymen metrics
laymen_bc <- extract_layman(laymen_b, community_df = c_names)

# save theme
qs::qsave(laymen_bc, here("data-saved",
                          "siber-top-three",
                          paste0("bays_community_metrics", Sys.Date(), ".qs")))

# ---- calculate the Equal Tailed interval (ETI) that will be reported ----
# with medians
# ---- create median summary of layman bays ----
laymen_bc_sum <- laymen_bc %>%
  group_by(ecoregion, metric) %>%
  summarise(
    post_meadian = median(post_est)
  ) %>%
  ungroup()

# split laymen bays for ci estitmea
laymen_bc_split <- laymen_bc %>%
  mutate(
    names = paste(community, metric, sep = ":")
  ) %>%
  split(.$names)

# ---- create eti estimates ----
ci_intervals <- laymen_bc_split %>%
  map(~ ci(.x$post_est, method = "ETI")) %>%
  bind_rows(.id = "name") %>%
  separate_wider_delim(name, names = c("community", "metric"), delim = ":") %>%
  janitor::clean_names() %>%
  left_join(c_names) %>%
  mutate(
    differences = round(ci_high - ci_low, 2),
    middle = round(differences / 2, 2),
    mid = round(ci_low + middle, 2)
  )

# ---- join bays median ----
ci_intervals_mids <- ci_intervals %>%
  left_join(laymen_bc_sum) %>%
  arrange(metric, ecoregion)

# save to excel for reporting

openxlsx::write.xlsx(ci_intervals_mids, here("results",
                                             "top-three",
                                             "siber-layman-bays",
                                             paste0("credible_intervals_layman_metric",
                                                    Sys.Date(), ".xlsx")))

# ---- estimate ellipse area niche area ----

sea_b <- siberEllipses(corrected.posteriors = mod)

glimpse(sea_b)
class(sea_b)

# plot with siber to get an idea what we have
siberDensityPlot(sea_b, xticklabels = colnames(group_ml),
                 xlab = c("Community | Group"),
                 ylab = expression("Standard Ellipse Area " ('permille'^2)),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses on each group"
)

# ---- extract ellipse area ----
df_select
# grp <- paste(df_select$community, df_select$group, sep = ".") |>
#   unique() |>
#   sort() |>
#   as.array()
# grp
# class(grp)
#
# extract_group_metrics(community_df = )
#
# extract_niche_size_s <- function(data,
#                                  name,
#                                  community_df) {
#
#   grp <- paste(name$community, name$group, sep = ".") |>
#     unique() |>
#     sort() |>
#     as.array()
#
#   seb_dat <- as.data.frame(data)
#   names(seb_dat) <- grp
#
#   seb_extract<- seb_dat |>
#     dplyr::mutate(
#       id = 1:nrow(data)
#     ) |>
#     tidyr::pivot_longer(cols = -id,
#                  names_to = "community_group",
#                  values_to = "sea") |>
#     separate_wider_delim(community_group, delim = ".", names = c("community",
#                                                                  "group")) |>
#     dplyr::arrange(community, group, id) |>
#     dplyr::left_join(community_df, by = c("community", "group"))
#
# }


seb_convert <- extract_niche_size(data = sea_b,
                                  pkg = "SIBER",
                                  # name = df_select,
                                  community_df = cg_names)

ggplot(data = seb_convert, aes(x = common_name,
                               y = sea,
                               fill = ecoregion)) +
  geom_violin()

# glimpse(seb_dat)

# ---- convert to data frame for ease of use ----
# seb_convert <- seb_dat %>%
#   mutate(
#     id = 1:nrow(.)
#   ) %>%
#   pivot_longer(cols = -id,
#                names_to = "community_group",
#                values_to = "sea") %>%
#   separate_wider_delim(community_group, delim = ".", names = c("community",
#                                                                "group")) %>%
#   arrange(community, group, id) %>%
#   left_join(cg_names)

seb_convert

# save dataframe
qs::qsave(seb_convert, here("data-saved",
                            "siber-top-three",
                            paste0("bays_ellipse_siber_extract",
                                   Sys.Date(), ".qs")))

# ---- extract equal tailed intervals for reporting and medians -----
seb_convert_grouped <- seb_convert %>%
  mutate(
    name = paste(group, community, sep = ":")
  ) %>%
  split(.$name)


seb_convert_grouped

ci_intervals_sea <- seb_convert_grouped %>%
  map(~ ci(.x$sea, method = "ETI")) %>%
  bind_rows(.id = "name") %>%
  separate_wider_delim(name, names = c("group", "community"), delim = ":") %>%
  janitor::clean_names() %>%
  mutate(
    differences = round(ci_high - ci_low, 2),
    middle = round(differences / 2, 2),
    mid = round(ci_low + middle, 2)
  ) %>%
  left_join(cg_names)

ci_intervals_sea

# save to excel for reporting

openxlsx::write.xlsx(ci_intervals_sea, here("results",
                                            "top-three",
                                            "siber-sea",
                                            paste0("post_edi_sea_spp_ecoregion",
                                                   Sys.Date(), ".xlsx")))
# ---- filter out seac from group convert ----

seac <- group_convert %>%
  filter(metric %in% "SEAc")

# save for later plotting
qs::qsave(seac, here("data-saved",
                     "siber-top-three",
                     paste0("centers_ellipse_siber_extract", Sys.Date(),
                            ".qs")))

# ---- ellipse overlap ----
# first we need to make all parirings for within ecoregions
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
  dplyr::select(-c("c_1", "c_2", "g_1", "g_2")) %>%
  mutate(
    cg_1 = factor(cg_1, levels = dput(unique(test_1$name))),
    cg_2 = factor(cg_2, levels = dput(unique(test_1$name)))
  )
cg_names_within_eco

# Split for purrr
cg_eco_split <- cg_names_within_eco %>%
  mutate(
    names = paste(cg_1, cg_2, sep = "_") %>%
      factor()
  ) %>%
  arrange(names) %>%
  split(.$names) %>%
  map(~ .x %>%
        dplyr::select(-names) %>%
        mutate(
          cg_1 = as.character(cg_1),
          cg_2 = as.character(cg_2),
        ))


# ---- maximum likelihood for within eco ----
# The overlap of the maximum likelihood fitted standard ellipses are
# estimated using

ml_within_eco_overlap <- cg_eco_split %>%
  map(~ maxLikOverlap(.x$cg_1, .x$cg_2, siber_example,
                      p.interval = NULL, n = 100), .progress = TRUE)
glimpse(ml_within_eco_overlap)

# ---- extract maximum likelihood compare ----
ml_95_within_eco <- ml_within_eco_overlap %>%
  bind_rows(.id = "compare") %>%
  separate_wider_delim(compare, delim = "_", names = c("compare_1",
                                                       "compare_2")) %>%
  separate_wider_delim(compare_1, delim = ".", names = c("community",
                                                         "group")) %>%
  left_join(cg_names) %>%
  rename(
    community_1 = community,
    group_1 = group,
    eco_name_1 = ecoregion,
    common_name_1 = common_name
  ) %>%
  separate_wider_delim(compare_2, delim = ".", names = c("community",
                                                         "group")) %>%
  left_join(cg_names) %>%
  rename(
    community_2 = community,
    group_2 = group,
    eco_name_2 = ecoregion,
    common_name_2 = common_name
  ) %>%
  mutate(
    prop_overlap = overlap / ((area.2 + area.1) - overlap)
  )

ml_95_within_eco

# save
qs::qsave(ml_95_within_eco, here("data-saved",
                                 "siber-overlap-bays",
                                 paste0("overlap_ml_95_within_sp_compare_eco", Sys.Date(),
                                        ".qs")))

# ---- corresponding Bayesian estimates for the overlap ----
# between the 95% ellipses is given by:
bayes95.overlap <- cg_eco_split %>%
  map(~ bayesianOverlap(.x$cg_1, .x$cg_2, mod,
                        draws = 100, p.interval = 0.95,
                        n = 100),
      .progress = TRUE
  )

# ---- extract bays overlap among species within an ecoregion ----
bays_95_overlap <- bayes95.overlap %>%
  bind_rows(.id = "compare") %>%
  separate_wider_delim(compare, delim = "_", names = c("compare_1",
                                                       "compare_2"),
                       cols_remove = FALSE) %>%
  separate_wider_delim(compare_1, delim = ".", names = c("community",
                                                         "group")) %>%
  left_join(cg_names) %>%
  rename(
    community_1 = community,
    group_1 = group,
    eco_name_1 = ecoregion,
    common_name_1 = common_name
  ) %>%
  separate_wider_delim(compare_2, delim = ".", names = c("community",
                                                         "group")) %>%
  left_join(cg_names) %>%
  rename(
    community_2 = community,
    group_2 = group,
    eco_name_2 = ecoregion,
    common_name_2 = common_name
  ) %>%
  mutate(
    prop_overlap = overlap / ((area2 + area1) - overlap),
  ) %>%
  ungroup()

glimpse(bays_95_overlap)

# ---- save bays within ecoregion overlap -----
qs::qsave(bays_95_overlap, here("data-saved",
                                "siber-overlap-bays",
                                paste0("correct_overlap_95_bays_within_eco_",
                                       Sys.Date(), ".qs")))

# ---- Bays overlap among ecoregions for same species ----
# ---- look at species differences among ecoregions ----
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
  filter(!(g_1 != g_2 )) %>% # this is important
  dplyr::select(-c("c_1", "c_2", "g_1", "g_2")) %>%
  mutate(
    cg_1 = factor(cg_1, levels = dput(unique(test_1$name))),
    cg_2 = factor(cg_2, levels = dput(unique(test_1$name)))
  )

glimpse(cg_names_within_sp)

# Split for purrr



cg_sp_split <- cg_names_within_sp %>%
  mutate(
    names = paste(cg_1, cg_2, sep = "_") %>%
      factor()
  ) %>%
  arrange(names) %>%
  split(.$names) %>%
  map(~ .x %>%
        dplyr::select(-names) %>%
        mutate(
          cg_1 = as.character(cg_1),
          cg_2 = as.character(cg_2),
        ))

cg_sp_split
# ---- maximum likelihood for within eco ----

ml_within_sp_overlap <- cg_sp_split %>%
  map(~ maxLikOverlap(.x$cg_1, .x$cg_2, siber_example,
                      p.interval = NULL, n = 100), .progress = TRUE)

# ---- extract maximum likelihood compare ----
ml_95_within_sp <- ml_within_sp_overlap %>%
  bind_rows(.id = "compare") %>%
  separate_wider_delim(compare, delim = "_", names = c("compare_1", "compare_2")) %>%
  separate_wider_delim(compare_1, delim = ".", names = c("community",
                                                         "group")) %>%
  left_join(cg_names) %>%
  rename(
    community_1 = community,
    group_1 = group,
    eco_name_1 = ecoregion,
    commmon_name_1 = common_name
  ) %>%
  separate_wider_delim(compare_2, delim = ".", names = c("community",
                                                         "group")) %>%
  left_join(cg_names) %>%
  rename(
    community_2 = community,
    group_2 = group,
    eco_name_2 = ecoregion,
    commmon_name_2 = common_name
  ) %>%
  mutate(
    prop_overlap = overlap / ((area.2 + area.1) - overlap)
  )

ml_95_within_eco

# save
qs::qsave(ml_95_within_eco, here("data-saved",
                                 "siber-overlap-bays",
                                 "overlap_ml_95_within_sp_compare_eco.qs"))



glimpse(cg_sp_split)
# ---- corresponding Bayesian estimates for the overlap ----
# between the 95% ellipses is given by:
bayes95.overlap_sp <- cg_sp_split %>%
  map(~ bayesianOverlap(.x$cg_1, .x$cg_2, mod,
                        draws = 100, p.interval = 0.95,
                        n = 100),
      .progress = TRUE
  )

# ---- extract species differences -----
bays_95_overlap_sp <- bayes95.overlap_sp %>%
  bind_rows(.id = "compare") %>%
  separate_wider_delim(compare, delim = "_", names = c("compare_1",
                                                       "compare_2"),
                       cols_remove = FALSE) %>%
  separate_wider_delim(compare_1, delim = ".", names = c("community",
                                                         "group")) %>%
  left_join(cg_names) %>%
  rename(
    community_1 = community,
    group_1 = group,
    eco_name_1 = ecoregion,
    common_name_1 = common_name
  ) %>%
  separate_wider_delim(compare_2, delim = ".", names = c("community",
                                                         "group")) %>%
  left_join(cg_names) %>%
  rename(
    community_2 = community,
    group_2 = group,
    eco_name_2 = ecoregion,
    common_name_2 = common_name
  ) %>%
  mutate(
    prop_overlap = overlap / ((area2 + area1) - overlap)
  )
glimpse(bays_95_overlap_sp)
# save
qs::qsave(bays_95_overlap_sp, here("data-saved",
                                   "siber-overlap-bays",
                                   paste0("correct_overlap_95_bays_within_sp_",
                                          Sys.Date(), ".qs")))


