# ---- load packages ----
{
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(here)
  library(purrr)
  library(sf)
  library(terra)
  library(tidyterra)
}

# ---- bring in raster -----

bathy_raster <- rast(here("..",
                          "lolwf",
                          "raster-bathymetry",
                          "bathy_raster.tif"))


plot(bathy_raster)


# ---- bring ecoregion locations data ----

lake_o_eco <- st_read(dsn = here("..",
                                 "lake-ontario-multitelemetry",
                                 "shapefiles",
                                 "lake-ontario-ecoregions",
                                 "."),
                      layer = "lake_ontario_ecoregions")


lake_o_complex <- st_read(dsn = here("shapefiles",
                                     "."),
                          layer = "lake_ontario_ecoregion_edit") %>%
  st_transform(crs = 4326) %>%
  mutate(
    ID = 1:nrow(.) %>%
      as.factor()
  )

# ----
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

ggplot() +
  geom_sf(data = lake_o_complex)


# ------ spilt an by each region -----

bathy_ecoregion <- lake_o_complex %>%
  split(.$ecoregion) %>%
  map(~ crop(bathy_raster, .x)
  )



bathy_ecoregion %>%
  map(~ plot(.x))



bathy_ecoregion %>%
  map(~ global(.x, max, na.rm = TRUE)) %>%
  bind_rows(.id = "ecoregion")

mean_depth_eco <- tibble(
  bathy_ecoregion %>%
  map(~ global(.x, mean, na.rm = TRUE)) %>%
    bind_rows(.id = "ecoregion"),
  bathy_ecoregion %>%
    map(~ global(.x, nrow)) %>%
    bind_rows(),
  bathy_ecoregion %>%
    map(~ global(.x, sd, na.rm = TRUE)) %>%
    bind_rows(),
) %>%
  rename(
    n = global
  ) %>%
  mutate(
    mean = round(mean, 1),
    sem = round(sd / sqrt(n), 1),
    means = paste(mean, sem, sep = " ± ")
  ) %>%
  select(ecoregion, means) %>%
  pivot_wider(names_from = ecoregion,
              values_from = means)

mean_depth_eco
openxlsx::write.xlsx(mean_depth_eco,
                     here("mean_depth_ecoregion.xlsx"))
