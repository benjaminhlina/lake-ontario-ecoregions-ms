# ---- load packages -----
{
  library(bayestestR)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(here)
  library(nichetools)
  library(purrr)
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

# ---- filter out just alewife lkt and gobby ---





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



glimpse(df)
glimpse(zm_summary)

zm_summary <- zm_summary %>%
  select(common_name, ecoregion, cal_d13c, cal_d15n) %>%
  rename(
    d13c = cal_d13c,
    d15n = cal_d15n
  )
set.seed(123)



# inlet_fix <- data.frame(
#   common_name = "Dreissenids",
#   ecoregion = "Inlet",
#   fish_guilds = "Near-offshore Benthic Plakntivore",
#   d13c =
#   d15n = rnorm(n = 5, mean = 10.34, sd = 0.72)


df_wide <- df %>%
  select(common_name, ecoregion, fish_guilds, lfree_d13c, lfree_d15n) %>%
  rename(
    d13c = lfree_d13c,
    d15n = lfree_d15n
  ) %>%
  bind_rows(., zm_summary) %>%
  mutate(
    common_name = if_else(common_name %in% c("D. polymorpha",
                                             "D. bugensis",
                                             "Dreissena bugensis",
                                             "Dreissena polymorpha",
                                             "Dreissena sp.",
                                             "Quagga mussel"),
                          false = common_name,
                          true = "Dreissenids"),
    community_name = paste(common_name, ecoregion, sep = "-"),
    fg = case_when(
      common_name == "Dreissenids" ~ "Benthic_BL",
      common_name == "Alewife" ~ "Alewife",
      common_name == "Round Goby" ~ "Round Goby",
      common_name == "Lake Trout" ~ "Lake Trout",
    ),
    # d15n = case_when(
    #   common_name == "Dreissenids" &
    #     ecoregion == "Deep-Hole" ~ c(
    #   .default = d15n
    # )
  ) %>%
  as.data.frame()

glimpse(df_wide)
# df_wide %>%
#   distinct(common_name, fg)
#
#
# te <- df_wide %>%
#   filter(
#     common_name == "Dreissenids"
#   )
#
# m <- glm(d15n ~ ecoregion, data = te,
#          family = Gamma(link = "identity"))
#
# anova(m)
#
#
#
# res <- DHARMa::simulateResiduals(m)
# plot(res)
#
#
# fitdistrplus::descdist(te$d15n)


ggplot(data = te, aes(x = d15n)) +
  geom_histogram()

ggplot(data = df_wide, aes(x = d13c, y = d15n,
                           colour = common_name)) +
  geom_point() +
  facet_wrap(~ ecoregion)


df_wide %>%
  group_by(
    common_name, ecoregion
  ) %>%
  summarise(
    mean_d15n = mean(d15n)
  ) %>%
  ungroup() %>%
  print(n = 34)

# BilagayMEC <- read.csv(system.file("extdata", "Bilagay-MEC.csv",
#                                    package = "tRophicPosition"))
#
# glimpse(BilagayMEC)
#
# BilagayMEC %>%
#   distinct(Location)


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




df_wide_1 <- df_wide %>%
  tibble() %>%
  mutate(
    fish_guilds = if_else(fish_guilds %in% NA,
                          true = "Near-offshore Benthic Plakntivore",
                          false = fish_guilds)
  ) %>%
  select(-c(fg, community_name)) %>%
  as.data.frame()


df_wide_1 %>%
  distinct(fish_guilds)
# df_wide_1 <- df_wide %>%
#   filter(
#     fg == "Benthic_BL" |
#     common_name == "Alewife"
#   )

# extractIsotopeData()
?jagsOneBaseline

df_list <- extractIsotopeData(df_wide_1,
                              b1 = "Near-offshore Benthic Plakntivore",
                              baselineColumn = "fish_guilds",
                              consumers = c("Alewife", "Lake Trout",
                                            "Round Goby"),
                              consumersColumn = "common_name",
                              groupsColumn = "ecoregion",
                              d13C = "d13c", d15N = "d15n")




glimpse(df_list$`Open-Coastal-Lake Trout`)

# df_list$`Open-Coastal-Lake Trout`[names(df_list$`Open-Coastal-Lake Trout`) %in% c("dCb1", "dNb2", "dCb2", "deltaC", "dCc")] <- NULL

parm_tp <- df_list %>%
  map(~ parametricTP(siData = .x)
  ) %>%
  bind_rows(.id = "ecoregion_common_name") %>%
  mutate(
    id = 1
  ) %>%
  pivot_longer(cols = -id,
               names_to = "ecoregion_common_name",
               values_to = "tp") %>%
  separate_wider_delim(ecoregion_common_name, cols_remove = FALSE,
                       delim = "-", names = c("ecoregion", "common_name"),
                       too_many = "debug"
  ) %>%
  mutate(
    ecoregion = case_when(
      ecoregion %in% c("Open",
                       "Deep") ~ paste(ecoregion, common_name, sep = "-"),
      .default = ecoregion
    ),
    ecoregion = factor(ecoregion, level =
                         c("Anthropogenic",
                           "Inlet",
                           "Open-Coastal",
                           "Deep-Hole",
                           "Outlet",
                           "Embayment"
                         )),

    common_name = case_when(
      common_name %in% c("Coastal",
                         "Hole") ~ paste(ecoregion_common_name_remainder) %>%
        stringr::str_remove("-"),

      .default = common_name
    )
  )




parm_tp

# df_comp <- loadIsotopeData(df_wide_1,
#                            consumer = c("Alewife", "Lake Trout",
#                                         "Round Goby"),
#                            consumersColumn = "common_name",
#                             b1 = "Near-offshore Benthic Plakntivore",
#                             baselineColumn = "fish_guilds",
#                            groupsColumn = "ecoregion",
#                            d13C = "d13c", d15N = "d15n")

class(df_comp)
# plot(community, b1 = "Benthic baseline")
str(df_list)
glimpse(df_list)
names(df_list)
attributes(df_list$`Open-Coastal-Lake Trout`)$group
# glimpse(df_comp)
# t <- df_list %>%
#   map(~ plot(.x, b1 = "Benthic baseline"))
#
# t$`Open-Coastal-Lake Trout`


# ggsave(filename = here::here("plots",
#                   "top-three",
#                   "trophic-position",
#                   "trp-pkg",
#                   paste(attributes(.x$group), ".png", sep = "_")),
#        plot =
#          plot(.x, b1 = "Benthic baseline"),
#        height = 8.5, width = 11))
for (community in df_list) {
  print(summary(community))
  plot(community, b1 = "Benthic baseline")
}

# model.string <- jagsBayesianModel(model = "oneBaseline", TP = "dnorm(4, 0.1)")
#
# model <- TPmodel(data = df_list,
#                  model.string = model.string,
#                  n.adapt = 20000, n.chains = 2)
#
# test_model <- multiModelTP(df_list,
#                            model = "oneBaseline",
#              n.adapt = 10000, n.iter = 10000,
#              burnin = 10000, n.chains = 5)


lake_o_model <- multiSpeciesTP(df_list, model = "oneBaseline",
                               n.adapt = 10000, n.iter = 10000,
                               burnin = 10000, n.chains = 5,
                               # print = FALSE
)
class(df_list)
parm

posterior_TP
posterior.samples <- posteriorTP(lake_o_model, n.iter = 10000,
                                 variable.names = c("TP"))
glimpse(lake_o_model)
glimpse(lake_o_model$df)
glimpse(lake_o_model$TPs)
lake_o_model$df
lake_o_model$TPs

qs::qsave(lake_o_model, here("data-saved",
                                  "trophic-position-top-three",
                             "bays_model_output_tp.qs"))
