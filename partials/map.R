# Description -------------------------------------------------------------
# Map Loading and generation
# Important loading order to prevent errors!
# (https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true)
# library( rgeos )
# library( maptools )
# library( rgdal )
# Now only using sf, dont need the tools above
# library( sf )

# Import ------------------------------------------------------------------
mapDataPath <- glue("{here()}/data/maps.RData")
# Read MAPS
if (!exists("mfStates")) {
  if (file.exists(mapDataPath)) {
    load(mapDataPath)
  } else {
    mapAustria <- read_sf(glue("{here()}/map"))
    mfStates <- mapAustria %>%
      group_by(BL) %>%
      summarize(
        geometry = st_union(geometry)
      )
    mfStatesSimplify <- mfStates %>% st_simplify(dTolerance = 0.002)
    mfDistricts <- mapAustria %>%
      group_by(PB) %>%
      summarize(
        geometry = st_union(geometry)
      )
    mfDistrictsSimplify <- mfDistricts %>% st_simplify(dTolerance = 0.002)
    # save R object to prevent loading each time
    save(mapAustria, mfDistricts, mfStates, mfStatesSimplify, mfDistrictsSimplify, file = mapDataPath)
  }
}
rm(mapDataPath)
