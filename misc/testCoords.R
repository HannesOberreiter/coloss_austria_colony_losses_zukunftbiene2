# Description -------------------------------------------------------------
# This code will check the source for wrong coords 
# (e.g. place is not in state it should be or outside of the map) 

# Load Modules ------------------------------------------------------------
source("partials/setup.R")
source("partials/header.R")
source("partials/map.R")

# Code --------------------------------------------------------------------
# remove empty ones
DISTRICT.CACHE <- RAW %>% 
  drop_na( longitude, latitude )
# Filter Year
# DISTRICT.CACHE %>% filter(year == "20/21")

# List of districts
DISTRICT.CACHE <- DISTRICT.CACHE %>% 
  select( latitude:id ) %>% 
  filter(
    # Drop "In mehr as einem Bezirk" because we cannot know which one it belongs to
    district != "In mehr als einem Bezirk"
  )
# Create SF Object
DISTRICT.CACHE <- st_as_sf(
  DISTRICT.CACHE, 
  coords = c("longitude", "latitude"),
  remove = F
)
# get same CRS from District Map
st_crs(DISTRICT.CACHE) <- st_crs(MF_DISTRICTS)
# check if point intersects with polygon
# join districts on results
# filter if data district is not the same as on the map
# or if NA (not found)
DISTRICT.CACHE <- DISTRICT.CACHE %>% 
  mutate(
    polygonId = st_intersects(DISTRICT.CACHE, MF_DISTRICTS),
    polygonId = as.character(polygonId)
  ) %>% 
  left_join(
    as_tibble(MF_DISTRICTS) %>% rownames_to_column("polygonId"),
    by = c("polygonId")
  ) %>% 
  filter(district != PB | is.na(PB))

# save to csv
write_excel_csv2(
  as_tibble(DISTRICT.CACHE),
  file = glue('{here()}/output/testCoords.csv')
)

# Plot the wrong coords on a map
ggplot() + 
  geom_sf(data = MF_STATES, aes(group = BL), color = "black", size = 0.6, fill = "white") +
  geom_sf(data = MF_DISTRICTS, aes(group = PB), fill=NA, color = colorBlindBlack8[1], size = 0.2) +
  geom_point( data = DISTRICT.CACHE, aes( longitude, latitude ), size = 2, color = "red") +
  geom_text(
    data = DISTRICT.CACHE,
    aes( longitude, latitude, label = id ),
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



