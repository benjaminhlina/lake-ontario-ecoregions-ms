# ---- packages ----
{
  library(bayestestR)
  library(distributional)
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(ggh4x)
  library(here)
  library(qs)
  library(purrr)
  library(patchwork)
  library(tidyr)
}


bays_95_overlap <- qread(here("data-saved",
                              "siber-overlap-bays",
                              "correct_overlap_95_bays_within_eco_2024-06-19.qs"))


# bays_95_overlap %>%
#   distinct(compare_1, common_name_1)
# compare_filter <- bays_95_overlap %>%
#   distinct(compare) %>%
#   .$compare %>%
#   strsplit("_") %>%
#   lapply(sort) %>%
#   sapply(paste, collapse = "_") %>%
#   unique()
#
# bays_95_overlap_f <- bays_95_overlap %>%
#   filter(compare %in% compare_filter)
# mutate(
#   common_name_1 = case_when(
#     common_name_2 %in% "Alewife"
#     & common_name_1 %in% "Round Goby" ~ "Alewife",
#     common_name_2 %in% "Alewife"
#     & common_name_1 %in% "Lake Trout" ~ "Alewife",
#     .default = common_name_1
#   )
# )
#
# bays_95_overlap_f %>%
#   distinct(common_name_1)

# library(data.table)
# bd <- setDT(bays_95_overlap)
# bd[, var1_alt := min(compare_1, compare_2), by = 1:nrow(bd)]
# bd[, var2_alt := max(compare_1, compare_2), by = 1:nrow(bd)]
#
# bd_2 = unique(bd[, .(compare_1 = var1_alt, compare_2 = var2_alt, prop_overlap)])
#
# bd_2

bays_95_overlap_sp <- qread(here("data-saved",
                                 "siber-overlap-bays",
                                 "correct_overlap_95_bays_within_sp_2024-06-19.qs"))

compare_filter_sp <- bays_95_overlap_sp %>%
  distinct(compare) %>%
  .$compare %>%
  strsplit("_") %>%
  lapply(sort) %>%
  sapply(paste, collapse = "_") %>%
  unique()

bays_95_overlap_sp_f <- bays_95_overlap_sp %>%
  filter(compare %in% compare_filter_sp)



compare_filter <- bays_95_overlap$compare %>%
  unique() %>%
  strsplit("_") %>%
  lapply(sort) %>%
  sapply(paste, collapse = "_") %>%
  unique()



# filter

bays_95_overlap_f <- bays_95_overlap %>%
  filter(compare %in% compare_filter)

ci_intervals <- bays_95_overlap_f %>%
  mutate(
    id = paste(common_name_1, eco_name_1, common_name_2, eco_name_2, sep = "_")
  ) %>%
  split(.$id) %>%
  map(~ ci(.x$prop_overlap, method = "ETI")) %>%
  bind_rows(.id = "name") %>%
  separate_wider_delim(name, names = c("common_name_1", "eco_name_1",
                                       "common_name_2", "eco_name_2"),
                       delim = "_") %>%
  janitor::clean_names() %>%
  mutate(
    differences = round(ci_high - ci_low, 2),
    middle = round(differences / 2, 2),
    mid = round(ci_low + middle, 2)
  )
ci_intervals


medi <- bays_95_overlap %>%
  mutate(
    id = paste(common_name_1, eco_name_1, common_name_2, eco_name_2, sep = "_")
  ) %>%
  group_by(id) %>%
  summarise(
    median_overlap = median(prop_overlap)
  ) %>%
  ungroup() %>%
  separate_wider_delim(id, names = c("common_name_1", "eco_name_1",
                                       "common_name_2", "eco_name_2"),
                       delim = "_")

ci_intervals <- ci_intervals %>%
  left_join(medi)







openxlsx::write.xlsx(ci_intervals, here("results",
                                             "top-three",
                                             "siber-overlap-bays",
                                             "credible_intervals_within_eco_overlap_bays.xlsx"))



ci_intervals_sp <- bays_95_overlap_sp_f %>%
  mutate(
    id = paste(common_name_1, eco_name_1, common_name_2, eco_name_2, sep = "_")
  ) %>%
  split(.$id) %>%
  map(~ ci(.x$prop_overlap, method = "ETI")) %>%
  bind_rows(.id = "name") %>%
  separate_wider_delim(name, names = c("common_name_1", "eco_name_1",
                                       "common_name_2", "eco_name_2"),
                       delim = "_") %>%
  janitor::clean_names() %>%
  mutate(
    differences = round(ci_high - ci_low, 2),
    middle = round(differences / 2, 2),
    mid = round(ci_low + middle, 2)
  )
ci_intervals_sp


medi_sp <- bays_95_overlap_sp_f %>%
  mutate(
    id = paste(common_name_1, eco_name_1, common_name_2, eco_name_2, sep = "_")
  ) %>%
  group_by(id) %>%
  summarise(
    median_overlap = median(prop_overlap)
  ) %>%
  ungroup() %>%
  separate_wider_delim(id, names = c("common_name_1", "eco_name_1",
                                     "common_name_2", "eco_name_2"),
                       delim = "_")

ci_intervals_sp <- ci_intervals_sp %>%
  left_join(medi_sp)



ci_intervals_sp_sum <- ci_intervals_sp %>%
  group_by(common_name_1,)



openxlsx::write.xlsx(ci_intervals_sp, here("results",
                                        "top-three",
                                        "siber-overlap-bays",
                                        "credible_intervals_within_sp_overlap_bays.xlsx"))

