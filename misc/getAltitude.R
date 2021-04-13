# Description -------------------------------------------------------------
# This code will generate a list of altitudes
# uses open source access off geonames.org
# altitude from satellite images GNsrtm3

# Libs --------------------------------------------------------------------
library(rjson)
library(geonames)
library(tidyverse)
library(here)

# Set API Key -------------------------------------------------------------
# https://www.geonames.org
# https://www.geonames.org/export/credits.html
options(geonamesUsername = Sys.getenv("geonamesKey"))

# Load Modules ------------------------------------------------------------
source("partials/setup.R")
source("partials/header.R")

# Code --------------------------------------------------------------------
# remove empty ones if there are any
dfCache <- dfData %>% drop_na(longitude, latitude)
# select start and end number of row
# please be careful with hard limit of API for altitude, max. 1_000 per hour and 10_000 per day
paste("Max rows in our dataframe: ", nrow(dfCache))

# Start and End Row we want to get elevation
rowStart <- 1
rowEnd   <- 10
#rowEnd    <- nrow(dfCache)

# Create a Named Vector with IDS
vIDs    <- dfCache$id[rowStart:rowEnd]
vValues <- setNames(rep(NA, length(vIDs)), vIDs)

# Loop with given limits
for (i in rowStart:rowEnd) {
  lID        <- dfCache$id[i]
  lElevation <- GNsrtm3(lat = dfCache[i, "latitude"], lng = dfCache[i, "longitude"])$srtm3
  print(paste("ID:", lID, " Elevation: ", lElevation))
  vValues[lID] <- lElevation
  Sys.sleep(4) # we prevent overuse (1k : 1 hour)
}
print("------RESULTS--------")
vValues
print("---------------------")
# save to csv
write_excel_csv2(
  as_tibble(vValues, rownames = "id"),
  file = glue("{here()}/output/altitude.csv")
  )
