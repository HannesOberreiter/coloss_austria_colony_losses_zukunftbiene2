# Description -------------------------------------------------------------
# Loading of Libraries, settings and some standard constants (eg. colors)
print("Loading Setup")

# Libraries ---------------------------------------------------------------
libs <- c(
  "tidyverse", "here", "readxl",
  "glue", "patchwork", "rlang",
  "boot", "ggsignif", "knitr",
  "kableExtra", "lintr", "skimr",
  "sf", "scales", "ggforce"
)

# sf library needs to be loaded directly and not from cache
# we do not use RENV for version control anymore as it did cause some problems
# library("sf", lib.loc = "/usr/local/lib/R/4.0/site-library/")

# Load Libraries with function, install binary if not installed (mac binaries are defined!)
# x = Libraries name as String
fLoadLibs <- function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, type = "mac.binary")
  library(x, character.only = TRUE)
  return(T)
}

sapply(libs, fLoadLibs)
rm(libs, fLoadLibs)

# Misc Settings -------------------------------------------------------

# IBasic Theme Settings
theme_set(
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_blank(),
    strip.background = element_rect(
      fill = "white",
      colour = "black", size = rel(2)
    ),
    panel.spacing = unit(1, "lines"),
    strip.text.x = element_text(size = 11),
    strip.placement = "outside",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 1)),
    axis.title.y = element_text(colour = "black", size = 11, angle = 90, vjust = 2),
    axis.text.y = element_text(angle = 0, size = 11),
    axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"),
    axis.line.y = element_line(linetype = "solid", size = 0.5),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_line(colour = "lightgrey")
  )
)

# Constants ---------------------------------------------------------------
# Color Definition
# https://stackoverflow.com/questions/42458412/plotting-data-by-color-variable-with-ggplot2-in-r#comment72062654_42458412
colorBlindBlack8 <- c(
  "#464343", "#E69F00", "#56B4E9", "#009E73",
  "#CC79A7", "#464343", "#F0E442", "#0072B2", "#D55E00"
)

# Months in Questionnaire Treatment
treatmentMonths <- list()
treatmentMonths[["names"]] <- c(
  "April", "Mai", "Juni", "Juli",
  "August", "September", "Oktober", "November",
  "Dezember", "Jänner", "Februar", "März"
)
treatmentMonths[["short"]] <- c(
  "April", "Mai", "Juni", "Juli",
  "Aug.", "Sept.", "Okt.", "Nov.",
  "Dez.", "Jan.", "Feb.", "März"
)
treatmentMonths[["color"]] <- c(
  rep(colorBlindBlack8[[7]], 2),
  rep(colorBlindBlack8[[8]], 5),
  rep(colorBlindBlack8[[1]], 3),
  rep(colorBlindBlack8[[6]], 2)
)
treatmentMonths[["groups"]] <- c(
  rep("Frühjahr", 2),
  rep("Sommer", 5),
  rep("Winter", 3),
  rep("Frühjahr (nächstes Jahr)", 2)
) %>%
  fct_relevel(
    c("Frühjahr", "Sommer", "Winter", "Frühjahr (nächstes Jahr)")
  )

# Generate List -----------------------------------------------------------
# This file contains our treatment list ~ names of treatment methods,
# names will be used to connect costs, table column names etc.
# tsingle, ttotal represent the col names from import
# tname, tshort is for plots and paper
treatmentList <- tibble(
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
    # "T_oxalic_trickle_",
    "T_thymol_",
    "T_synthetic_",
    "T_other_"
  ),
  ttotal = NA,
  tname = c(
    "Varroa Kontrolle",
    "Drohnenbrutentahme",
    "Hyperthermie",
    "Andere biot. Methode",
    "Ameisensäure - Kurzzeit",
    "Ameisensäure - Langzeit",
    "Milchsäure",
    "Oxalsäure - Träufeln o. Sprühen",
    "Oxalsäure - Verdampfen",
    "Oxalsäure - Mischung",
    "Thymol",
    "Chemische Methoden",
    "Andere Methode"
  ),
  tshort = c(
    "V-check",
    "Drohnenb.",
    "Hyp.",
    "Biot.",
    "AS-KZ",
    "AS-LZ",
    "Milchs.",
    "Ox-Träu.",
    "Ox-Sub.",
    "Ox-Mix",
    "Thymol",
    "chem. Meth.",
    "Andere"
  )
) %>%
  mutate(
    ttotal = paste0(tsingle, "total")
  )

print("Setup Loaded")