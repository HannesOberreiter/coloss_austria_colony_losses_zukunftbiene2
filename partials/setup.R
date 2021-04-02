# Description -------------------------------------------------------------
# Loading of Libraries, settings and some standard constants (eg. colors)
print("Loading Setup")

# Libraries ---------------------------------------------------------------
libs <- c(
  "tidyverse", "here", "readxl", "glue", # Main Packages
  "sf" # Map
)

# sf library needs to be loaded directly and not from cache
# we do not use RENV for version control anymore as it did cause some problems
#library("sf", lib.loc = "/usr/local/lib/R/4.0/site-library/")

# Load Libraries with function, install binary if not installed (mac binaries are defined!)
# x = Libraries name as String
fLoadLibs <- function(x){
  if(!require(x, character.only = TRUE)) install.packages(x, type = "mac.binary");
  library(x, character.only = TRUE);
  return(T)
}

sapply(libs, fLoadLibs)
rm(libs, fLoadLibs)

# Misc Settings -------------------------------------------------------

# Increase base font size for our plots
theme_set(theme_classic(base_size=12))

# Constants ---------------------------------------------------------------
# Color Definition
# https://stackoverflow.com/questions/42458412/plotting-data-by-color-variable-with-ggplot2-in-r#comment72062654_42458412
colorBlindBlack8  <- c("#464343", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Generate List -----------------------------------------------------------
# This file contains our treatment list ~ names of treatment methods, 
# names will be used to connect costs, table column names etc.
# tsingle, ttotal represent the col names from import
# tname, tshort is for plots and paper
treatmentList = tibble(
  tsingle = c(
    "T_vcount_", 
    "T_drone_",
    "T_hyperthermia_",
    "T_biotechnical_",
    "T_formic_short_",
    "T_formic_long_",
    "T_lactic_",
    "T_oxalic_trickle_pure_",
    "T_oxalic_vapo_",
    "T_oxalic_trickle_mix_",
    #"T_oxalic_trickle_",
    "T_thymol_",
    "T_synthetic_",
    "T_other_"
  ),
  ttotal = NA,
  tname = c(
    "Varroa monitoring",
    "Drone brood removal",
    "Hyperthermia",
    "Another biotechnical method",
    "Formic acid - short term",
    "Formic acid - long term",
    "Lactic acid",
    "Oxal acid - pure",
    "Oxal acid - sublimation",
    "Oxal acid - mixture",
    "Thymol",
    "Synthetic methods",
    "Another methods"
  ),
  tshort = c(
    "V-check",
    "Drone",
    "Hyp.",
    "Biot.",
    "Fa-ST",
    "Fa-LT",
    "Lactic",
    "Ox-pure",
    "Ox-sub",
    "Ox-mix",
    "Thy",
    "chem. Meth.",
    "Another"
  )
) %>% 
  mutate(
    ttotal = paste0(tsingle, "total")
  )



print("Setup Loaded")



