# ---- load packages -----
{
  library(easystats)
  library(broom.mixed)
  library(dplyr)
  library(DHARMa)
  library(emmeans)
  library(fitdistrplus)
  library(ggplot2)
  library(gratia)
  library(mgcv)
  library(mgcViz)
  library(here)
  library(glmmTMB)
  library(purrr)
  library(patchwork)
  library(readr)
  library(stringr)
  library(sf)
  library(tidyr)
  library(vegan)
}

# ---- bring in data ----
df_wide <- read_rds(here("data-saved",
                         "top-three",
                         "top_three_cleaned_sia.rds")) %>%
  mutate(
    ecoregion = factor(ecoregion, levels =
                         c("Anthropogenic",
                           "Inlet",
                           "Open-Coastal",
                           "Deep-Hole",
                           "Outlet",
                           "Embayment")),
    d13c_tr = rel_d13c_lfree ^ (1 / 3)
  ) %>%
  dplyr::select(common_name, ecoregion, near_or_offshore,
                habitat_type, trophic_guild, fish_guilds, year, lfree_d13c, lfree_d15n,
                rel_d13c_lfree, rel_d15n_lfree,
                d13c_tr)

glimpse(df_wide)

# ---- look at distribution of every isotope type measurement for adult/NA ----
df_wide %>%
  pivot_longer(cols = -c(common_name:fish_guilds),
               names_to = "isotope_type",
               values_to = "values") %>%
  ggplot(aes(x = values)) +
  geom_histogram() +
  facet_wrap(. ~ isotope_type, scale = "free")


# ----- LOOK AT HOW MANY SPECIES AND ECO ----

# df_summary <- df_wide %>%
#   group_by(common_name, ecoregion) %>%
#   summarise(
#     n = n(),
#     mean_c = mean(lfree_d13c),
#     sem_c = sd(lfree_d13c) / sqrt(n()),
#     mean_n = mean(lfree_d15n),
#     sem_n = sd(lfree_d15n) / sqrt(n()),
#     mean_c_rel = mean(rel_d13c_lfree),
#     sem_c_rel = sd(rel_d13c_lfree) / sqrt(n()),
#     mean_n_rel = mean(rel_d15n_lfree),
#     sem_n_rel = sd(rel_d15n_lfree) / sqrt(n()),
#   ) %>%
#   ungroup() %>%
#   print(n = 60)
#
#
# openxlsx::write.xlsx(df_summary, here("results",
#                                       "top-three",
#                                       "summary",
#                                       "summary_stats_top_three.xlsx"))
#
#

descdist(df_wide$rel_d13c_lfree)

# ---- the big 3 ----
# alewife, rainbow smelt, roundy goby, lake trout
glimpse(df_wide)

df_wide <- df_wide %>%
  mutate(
    year = factor(year)
  )



df_wide %>%
  group_by(ecoregion) %>%
  summarise(
    min = min(rel_d13c_lfree),
    max = max(rel_d13c_lfree),
  ) %>%
  ungroup() %>%
  mutate(
    differ = max - min
  )

# ---- model ----

m1 <- gam(rel_d13c_lfree ~ common_name * ecoregion +
            s(year, bs = "re"),
          data = df_wide,
          method = "fREML",
          family = scat(link = "identity",
                        # theta = c(10, log(2.5)),
                        # min.df = 5
                        )
)


appraise(m1)
anova.gam(m1)
draw(m1)
summary(m1)
res <- simulateResiduals(m1)

plot(res)
hist(res)

hist(resid(m1))

# m1 <- glmmTMB(rel_d13c_lfree ~ common_name * ecoregion +
#                 (1 | year),
#               # (1 | station),
#               data = df_wide,
#               # family = Gamma(link = "identity"),
#               # family = gaussian(link = "identity"),
#               family = t_family(link = "identity"),
#               REML = TRUE,
#               # start = 0,
#               start = list(psi = log(2.751098)),
#               map = list(psi = factor(NA)),
#
#
#               control = glmmTMBControl(optimizer = optim,
#                                      optArgs = list(method = "BFGS"))
#               # na.action = na.exclude
# )

# head(simulate(m1, nsim = 5))
# head(simulate(m1, nsim = 1))
# head(simulate(m1, nsim = 1))

# out <- check_predictions(m1)
# plot(out)


m2 <- update(m1, . ~ common_name +
               s(year, bs = "re"))
# m2 <- update(m1, . ~ common_name +
#                (1|year))
m3 <- update(m1, . ~ ecoregion +
               s(year, bs = "re"))

# put(paste0('m', seq(1, 21, 1)))
# ---- create model list
model_list <- list(m1, m2, m3)
# give the elements useful names
names(model_list) <- paste0("m", seq(1, 3, 1))
glance(m1)
# drop1(m, test = "Chisq")
# get the summaries using `lapply

summary_list <- lapply(model_list, function(x) tidy(x, parametric = TRUE))
glance_list <- lapply(model_list, glance)


glance_summary <- map_df(glance_list, ~as.data.frame(.x), .id = "id") %>%
  mutate(model = lapply(model_list, formula) %>%
           as.character(),
         family = lapply(model_list, function(x) family(x)$family),
         link = lapply(model_list, function(x) family(x)$link),
  ) %>%
  arrange(AIC) %>%
  mutate(
    delta_AIC = AIC - first(AIC),
    AIC_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
  ) %>%
  dplyr::select(family, link, model, id:AIC, delta_AIC, AIC_weight, BIC:df.residual)

# ---- view model selection ------
glance_summary


# ----- model fit ----
res <- simulateResiduals(m1)

plot(res)
hist(res)

hist(resid(m1))

# ---- effects -----
anova(m2)
# main_effects <- tidy(car::Anova(m1, test.statistic = "Chisq"))
# main_effects
# main_effects$statistic
m_overall <- anova.gam(m1, freq = FALSE)

# grab parametic overall effect
overall_parm <- m_overall$pTerms.table %>%
  as_tibble(rownames = "term") %>%
  janitor::clean_names()
overall_parm$chi_sq

ind_effects <- tidy(m1)
ind_effects

summary(m1)
summary(m1)
# out <- check_predictions(m1)
# plot(out)

# ---- compare -----
multi_comp <- emmeans(m1, ~ common_name * ecoregion,
                      adjust = "holm",
                      type = "response")
# contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects <- contrast(multi_comp, method = "pairwise",
                             adjust = "bonferroni")

contr <- tidy(contrast_effects) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
contr


contr_1 <- contr %>%
  separate(contrast, into = c("con_1", "con_2"), sep = " - ") %>%
  mutate(
    con_1 = str_remove(string = con_1, pattern = "\\("),
    con_1 = str_remove(string = con_1, pattern = "\\)"),
    con_2 = str_remove(string = con_2, pattern = "\\("),
    con_2 = str_remove(string = con_2, pattern = "\\)"),
  ) %>%
  separate_wider_delim(con_1,names = c("co_1", "name_1", "eco_1"), delim = " ",
                       # too_many = "merge"
                       too_few = "align_start") %>%
  separate_wider_delim(con_2,names = c("co_2", "name_2", "eco_2"), delim = " ",
                       # too_many = "merge"
                       too_few = "align_start") %>%
  mutate(
    eco_1 = if_else(is.na(eco_1), true = name_1,
                    false = eco_1),
    common_name_1 = if_else(name_1 %in% eco_1, false = paste(co_1, name_1, sep = " "),
                            true = co_1),
    eco_2 = if_else(is.na(eco_2), true = name_2,
                    false = eco_2),
    common_name_2 = if_else(name_2 %in% eco_2, false = paste(co_2, name_2,
                                                             sep = " "),
                            true = co_2)
  ) %>%
  dplyr::select(term, common_name_1, eco_1,
                common_name_2, eco_2, null_value:adj_p_value) %>%
  arrange(common_name_1, common_name_2, eco_1, eco_2)


glimpse(contr_1)
contr_1

sp_within <- contr_1 %>%
  filter(common_name_1 == common_name_2) %>%
  arrange(common_name_1, common_name_2, adj_p_value)


sp_within %>%
  group_by(common_name_1) %>%
  summarise(
    n_eco = n_distinct(eco_1)
  )

# eco_wiithin

# ---- save ----
openxlsx::write.xlsx(sp_within, here("results",
                                     "top-three",
                                     "multicompare",
                                     "d13c",
                                     "within_species_ecoregion_multicomp_update_aug_2024.xlsx"))
# ---- across species ----
multi_comp_sp <- emmeans(m2, ~ common_name,
                         adjust = "holm",
                         type = "response")
# contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects_so <- contrast(multi_comp_sp, method = "pairwise",
                                adjust = "holm")

contr_sp <- tidy(contrast_effects_so) %>%
  janitor::clean_names() %>%
  separate_wider_delim(contrast, delim = " - ", names = c("sp_1", "sp_2")) %>%
  arrange(adj_p_value, sp_1)
contr_sp$adj_p_value
openxlsx::write.xlsx(contr_sp, here("results",
                                    "top-three",
                                    "multicompare",
                                    "d13c",
                                    "species_ecoregion_multicomp_update_aug_2024.xlsx"))
