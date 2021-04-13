# Description -------------------------------------------------------------
# This code will check the source for wrong coords 
# (e.g. place is not in state it should be or outside of the map) 

# Load Modules ------------------------------------------------------------
source("partials/setup.R")
source("partials/header.R")
source("partials/map.R")

# Code --------------------------------------------------------------------
# remove empty ones
districtCACHE <- dfData %>%
  filter(year == "19/20") %>%
  drop_na(longitude, latitude)
# Filter Year
# DISTRICT.CACHE %>% filter(year == "20/21")

# List of districts
districtCACHE <- districtCACHE %>%
  select(latitude:id) %>%
  filter(
    # Drop "In mehr as einem Bezirk" because we cannot know which one it belongs to
    district != "In mehr als einem Bezirk"
  )
# Create SF Object
districtCACHE <- st_as_sf(
  districtCACHE,
  coords = c("longitude", "latitude"),
  remove = F
)
# get same CRS from District Map
st_crs(districtCACHE) <- st_crs(mfDistricts)
# check if point intersects with polygon
# join districts on results
# filter if data district is not the same as on the map
# or if NA (not found)
districtCACHE <- districtCACHE %>%
  mutate(
    polygonId = st_intersects(districtCACHE, mfDistricts),
    polygonId = as.character(polygonId)
  ) %>%
  left_join(
    as_tibble(mfDistricts) %>% rownames_to_column("polygonId"),
    by = c("polygonId")
  ) %>%
  filter(district != PB | is.na(PB))

# save to csv
write_excel_csv2(
  as_tibble(districtCACHE),
  file = glue("{here()}/output/testCoords.csv")
)

# Plot the wrong coords on a map
ggplot() +
  geom_sf(data = mfStates, aes(group = BL), color = "black", size = 0.6, fill = "white") +
  geom_sf(data = mfDistricts, aes(group = PB), fill = NA, color = colorBlindBlack8[1], size = 0.2) +
  geom_point(data = districtCACHE, aes(longitude, latitude), size = 2, color = "red") +
  geom_text(
    data = districtCACHE,
    aes(longitude, latitude, label = id),
    hjust = -0.1, size = 3, color = colorBlindBlack8[4]) +
  coord_sf() +
  xlab("") + ylab("") +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )
