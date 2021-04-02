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
D.CACHE <- RAW %>% drop_na(longitude, latitude)
# select start and end number of row
# please be careful with hard limit of API for altitude, max. 1_000 per hour and 10_000 per day
paste("Max rows in our dataframe: ", nrow(D.CACHE))

# Start and End Row we want to get elevation
ROW.START = 1
ROW.END = 10
#ROW.END   = nrow(D.CACHE)

# Create a Named Vector with IDS
V.IDS    <- D.CACHE$id[ROW.START:ROW.END]
V.VALUES <- setNames(rep(NA, length(V.IDS)), V.IDS)

# Loop with given limits
for (i in ROW.START:ROW.END) {
  L.ID        <- D.CACHE$id[i]
  L.ELEVATION <- GNsrtm3(lat = D.CACHE[i, "latitude"], lng = D.CACHE[i, "longitude"])$srtm3
  print(paste("ID:", L.ID, " Elevation: ", L.ELEVATION))
  V.VALUES[L.ID] <- L.ELEVATION
  Sys.sleep(4) # we prevent overuse (1k : 1 hour)
}
print("------RESULTS--------")
V.VALUES
print("---------------------")
# save to csv
write_excel_csv2(
  as_tibble(V.VALUES, rownames = "id"),
  file = glue('{here()}/output/altitude.csv')
  )




