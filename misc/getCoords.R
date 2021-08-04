# Description -------------------------------------------------------------
# This code will try to fetch coordinates from given adresses

# Load Modules ------------------------------------------------------------
source("partials/setup.R")
source("partials/header.R")
library(tidygeocoder)

# Google API --------------------------------------------------------------
geoCodeResult <- dfData %>%
  filter(year == "20/21") %>%
  mutate(
    search_address = glue("{address}, {zip}, {district}, {state}, Austria")
  ) %>%
  geocode(
    address = search_address,
    full_results = TRUE,
    method = "google",
    limit = 1
  ) %>%
  select(
    id,
    search_address:long,
    formatted_address:geometry.location_type
  ) %>%
  mutate(types = as.character(types)) %>%
  separate(
    id, c("id", "year"), "-"
  )


# OSM ---------------------------------------------------------------------

# geoCodeResult <- dfData %>% head(2) %>%
#   mutate(
#     country = "Austria"
#   ) %>%
#   geocode(
#     street = address,
#     postalcode = zip,
#     county = district,
#     state = state,
#     country = country,
#     full_results = TRUE,
#     method = "osm",
#     limit = 1
#     )

# Export ------------------------------------------------------------------
write_excel_csv(geoCodeResult, "./data/export_coords.csv")