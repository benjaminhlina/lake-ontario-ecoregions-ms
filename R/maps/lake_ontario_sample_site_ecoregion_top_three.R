# ---- load packages ----
{
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(here)
  library(sf)
  library(readr)
}
# ---- bring SIA data ----

# df <- read_rds(here(
#   "data-saved",
#   "combined-isotope-data",
#   "combined_fish_yrs_sf.rds"
# ))
#
#
# unique(df$fish_guilds)
#
#
# # ---- summary ----
# df <- df %>%
# ---- bring in data ----
df <- read_rds(here("data-saved",
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




# ---- bring in lake ontario ecoregions ----

lake_o_eco <- st_read(dsn = here("..",
                                 "lake-ontario-multitelemetry",
                                 "shapefiles",
                                 "lake-ontario-ecoregions",
                                 "."),
                      layer = "lake_ontario_ecoregions")


lake_o_complex <- st_read(dsn = here("..",
                                     "lolwf",
                                     "shapefiles",
                                     "lake-ontario",
                                     "Shapefiles_LO",
                                     "."),
                          layer = "Thewatermask") %>%
  st_transform(crs = 4326)

plot(lake_o_complex)

lake_o_complex <- st_read(dsn = here("shapefiles",
                                     "."),
                          layer = "lake_ontario_ecoregion_edit") %>%
  st_transform(crs = 4326) %>%
  mutate(
    ID = 1:nrow(.) %>%
      as.factor()
  )

ggplot() +
  geom_sf(data = lake_o_complex, aes(fill = factor(ID)))




lake_o_eco <- st_transform(lake_o_eco, crs = 4326)

dput(unique(lake_o_eco$NAME))
lake_o_eco <- lake_o_eco %>%
  mutate(
    NAME = factor(NAME,
                  levels = c("Anthropogenic","Inlet",
                             "Open-Coastal", "Deep-Hole",
                             "Outlet", "Embayment"
                  ))
  )


lake_o_eco %>%
  ggplot() +
  geom_sf(aes(fill = NAME))

ecoregion_id <- tibble(
  ID = as.factor(seq(1, 6, 1)),
  ecoregion = factor(c("Open-Coastal",
                       "Anthropogenic",
                       "Inlet",
                       "Deep-Hole",
                       "Outlet",
                       "Embayment"),
                     levels = c("Anthropogenic","Inlet",
                                "Open-Coastal", "Deep-Hole",
                                "Outlet", "Embayment"
                     ))
)

lake_o_complex <- lake_o_complex %>%
  left_join(ecoregion_id, by = "ID")


# ----- sample locations -----
glimpse(df)
df %>%
  distinct(station, station_common_name) %>%
  print(n = 200)

df <- df %>%
  # group_by(station, station_common_name) %>%
  mutate(
    station_common_name = case_when(
      is.na(station_common_name) ~ station,
      .default = station_common_name
    )
  )

df %>%
  distinct(station, station_common_name) %>%
  print(n = 200)
df %>%
  distinct(station_common_name) %>%
  print(n = 200)


glimpse(df)

df_loc <- df %>%
  # group_by(station_common_name) %>%
  # summarise(
  #   mean_lon = mean(lon),
  #   mean_lat = mean(lat)
  # ) %>%
  # ungroup() %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)


# df_loc <- st_intersection(df_loc, lake_o_eco)
# ---- cities ----
lake_o_cities <- read_csv(here("..",
                               "lake-ontario-multitelemetry",
                               "data-raw",
                               "cities-around-lake-ontario",
                               "cities_around_lake_ontario.csv")) %>%
  mutate(
    location = factor(location)
  )

lake_o_cities_sf <- st_as_sf(lake_o_cities, coords = c("lon",
                                                       "lat"),
                             crs = 4326)

levels(lake_o_cities$location)
nudge_label <- data.frame(
  location = levels(lake_o_cities$location),
  x = c(-0.15, -0.2, 0, 0, 0, 0),
  y = c(0.075, 0, -0.12, -0.075, -0.12, 0.11)
)

str(nudge_label)

summary(df)
# nudge_point <- data.frame(
#   location = levels(lake_o_cities$location),
#   x = c(0, 0, 0, 0, 0, 0),
#   y = c(0.05, 0, -0.05, 0, -0.05, 0)
# )

p <- ggplot() +
  # geom_sf(data = lake_o_eco, aes(fill = NAME), colour = "black") +
  geom_sf(data = lake_o_complex,
          aes(fill = ecoregion),
          # fill = "white",
          lwd = 0.15,
          colour = "black") +
  geom_sf(data = df_loc, size = 2) +
  geom_sf(data = lake_o_cities_sf,
          shape = 22, fill = "white",
          stroke = 0.4, colour = "black",
          size = 2
  ) +
  geom_sf_label(data = lake_o_cities_sf,
                aes(label = location),
                nudge_x = nudge_label$x,
                nudge_y = nudge_label$y
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(1.0, "cm"),
                         width = unit(1.0, "cm")) +
  scale_fill_viridis_d(name = "Ecoregion",
                       begin = 0.2, end = 0.98,
                       option = "D", alpha = 0.65) +
  # theme_void()
  theme_bw(
    base_size = 12,
    base_family = "Times"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.10, 0.80),
    legend.background = element_blank()
  ) +
  guides(fill = guide_legend(override.aes = list(linewidth = 0.5))) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

p
ggsave(here("plots",
            "maps",
            "lake_ontario_ecoregions_top_three_points.png"),
       width = 11, height = 5.5, plot = p)
ggsave(here("plots",
            "maps",
            "lake_ontario_ecoregionstop_three_points.pdf"),
       width = 11, height = 8.5, plot = p)

# ---- no samples sites ----
p1 <- ggplot() +
  geom_sf(data = lake_o_complex,
          aes(fill = ecoregion),
          # fill = "white",
          lwd = 0.15,
          colour = "black") +
  geom_sf(data = lake_o_cities_sf,
          shape = 22, fill = "white",
          stroke = 0.4, colour = "black",
          size = 2
  ) +
  geom_sf_label(data = lake_o_cities_sf,
                aes(label = location),
                nudge_x = nudge_label$x,
                nudge_y = nudge_label$y
  ) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(1.0, "cm"),
                         width = unit(1.0, "cm")) +
  scale_fill_viridis_d(name = "Ecoregion",
                       begin = 0.2, end = 0.98,
                       option = "D", alpha = 0.65) +
  # theme_void()
  theme_bw(
    base_size = 12,
    base_family = "Times"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.10, 0.80),
    legend.background = element_blank()
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

p1
ggsave(here("plots",
            "maps",
            "lake_ontario_ecoregions_nopoints.png"),
       width = 11, height = 5.5, plot = p1)
ggsave(here("plots",
            "maps",
            "lake_ontario_ecoregions_nopoints.pdf"),
       width = 11, height = 8.5, plot = p1)

