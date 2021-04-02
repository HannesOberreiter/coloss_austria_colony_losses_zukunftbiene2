# Description -------------------------------------------------------------
# Map Loading and generation
# Important loading order to prevent errors! (https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true)
# library( rgeos )
# library( maptools )
# library( rgdal )
# Now only using sf, dont need the tools above
# library( sf )

# Import ------------------------------------------------------------------
mapDataPath <- glue("{here()}/data/maps.RData")
# Read MAPS
if(!exists("MF_STATES")){
  if(file.exists(mapDataPath)){
    load(mapDataPath)
  } else {
    MAP_AUSTRIA <- read_sf(glue("{here()}/map"))
    MF_STATES <- MAP_AUSTRIA %>% 
      group_by(BL) %>% 
      summarize(
        geometry = st_union(geometry)
      )
    MF_DISTRICTS <- MAP_AUSTRIA %>% 
      group_by(PB) %>% 
      summarize(
        geometry = st_union(geometry)
      )
    # save R object to prevent loading each time
    save(MAP_AUSTRIA, MF_DISTRICTS, MF_STATES, file = mapDataPath)
  }
}
rm(mapDataPath)

