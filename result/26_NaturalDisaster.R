# Losses due natural cause
res26_LossNaturalDistaster <- list()

res26_LossNaturalDistaster$res <- dfData %>%
    group_by(year) %>%
    summarise(
        lost_b = sum(lost_b),
        lost_percent = round(lost_b * 100 / sum(hives_lost), 1) %>% fPrettyNum()
    )

res26_LossNaturalDistaster$tab <- res26_LossNaturalDistaster$res %>%
    mutate(
        year = paste0("20", year)
    ) %>%
    knitr::kable(
        "latex",
        caption = "Absolute Anzahl an Völkern die durch Elementarschäden (Flut, Vandalismus etc.) als Verlust gemeldet wurden sowie prozentuell zu der Gesamtsumme an verlorenen Völkern über den Winter.",
        label = "u:26naturaldisaster",
        booktabs = T,
        escape = F,
        linesep = "", # https://stackoverflow.com/questions/45409750/get-rid-of-addlinespace-in-kable
        col.names = c("Jahr", "Elementarschäden [n]", "[\\%]"),
        align = c("c", "r", "r")
    ) %>%
    kable_styling(latex_options = c("HOLD_position"))

# small bugfix as pack_rows introduces an linespaceing which breaks midrule
res26_LossNaturalDistaster$tab <- sub("midrule{}", "midrule", res26_LossNaturalDistaster$tab, fixed = T)
res26_LossNaturalDistaster$tab %>% save_kable(paste0("output/tables/26_NaturalDisaster.tex"))