res19_VarroaCheck <- list()
myFactor <- "varroa_checked"
fLevels <- c("Ja", "Nein", "Unsicher", "keine Angaben")
tempData <- dfData %>%
    # Question was not in Newspaper
    filter(submitted != "Zeitung") %>%
    mutate(
        !!myFactor := tidyr::replace_na(!!sym(myFactor), "keine Angaben"),
        !!myFactor := as.factor(!!sym(myFactor)) %>% fct_relevel(fLevels)
    )

res19_VarroaCheck$result <- fGlmNullModel(tempData, myFactor) %>%
    mutate(
        !!myFactor := as.factor(!!sym(myFactor)) %>% fct_relevel(fLevels)
    ) %>%
    arrange(year, !!sym(myFactor))

res19_VarroaCheck$chi <- fChistar(res19_VarroaCheck$result, myFactor, dropNoAnswer = TRUE)
res19_VarroaCheck$p <- fPlot(
    res19_VarroaCheck$result,
    res19_VarroaCheck$chi,
    myFactor,
    xTitle = "",
    dropNoAnswer = TRUE,
    allData = TRUE,
    raw = tempData %>% filter(!!sym(myFactor) %in% c("Ja", "Nein"))
)

caption <- "Höhe der Winterverluste in Prozent ($\\pm$95\\%CI) und Anzahl der Antworten ob eine Varroa Milben Kontrolle durchgeführt wurde oder nicht über die Umfragejahre 2017/18 - 2020/21"

fSaveTable("19_VarroaControl", res19_VarroaCheck$result, caption, myFactor, "u:19varroacontrol")
fSaveImages("19_VarroaControl", res19_VarroaCheck$p)

# Combination -----------------------------------------------------------------
tLevels <- c(
    "Nur\nFrühling", "Nur\nSommer", "Nur\nWinter",
    "Frühling\nSommer", "Sommer\nWinter", "Frühling\nWinter",
    "Frühling\nSommer\nWinter", "Keine\nKontrolle"
)

res19_VarroaCheck[["combination"]] <- tempData %>%
    filter(
        varroa_checked == "Nein" |
            (varroa_checked == "Ja" & (T_vcount_totalyn_spring + T_vcount_totalyn_summer + T_vcount_totalyn_winter) > 0)
    ) %>%
    mutate(
        T_vcount_comb = case_when(
            varroa_checked == "Nein" ~ "Keine\nKontrolle",
            T_vcount_totalyn_spring == 1 & (T_vcount_totalyn_summer + T_vcount_totalyn_winter) == 0 ~ "Nur\nFrühling",
            T_vcount_totalyn_summer == 1 & (T_vcount_totalyn_spring + T_vcount_totalyn_winter) == 0 ~ "Nur\nSommer",
            T_vcount_totalyn_winter == 1 & (T_vcount_totalyn_spring + T_vcount_totalyn_summer) == 0 ~ "Nur\nWinter",
            T_vcount_totalyn_spring == 0 & (T_vcount_totalyn_summer + T_vcount_totalyn_winter) == 2 ~ "Sommer\nWinter",
            T_vcount_totalyn_winter == 0 & (T_vcount_totalyn_spring + T_vcount_totalyn_summer) == 2 ~ "Frühling\nSommer",
            T_vcount_totalyn_summer == 0 & (T_vcount_totalyn_spring + T_vcount_totalyn_winter) == 2 ~ "Frühling\nWinter",
            (T_vcount_totalyn_spring + T_vcount_totalyn_summer + T_vcount_totalyn_winter) == 3 ~ "Frühling\nSommer\nWinter",
            TRUE ~ "FALSE"
        )
    )

logiLevels <- tLevels %in% unique(res19_VarroaCheck[["combination"]][["T_vcount_comb"]])

res19_VarroaCheck[["combination"]] <- res19_VarroaCheck[["combination"]] %>%
    mutate(
        T_vcount_comb = forcats::fct_relevel(T_vcount_comb, tLevels[logiLevels])
    )

tPlotFilter <- res19_VarroaCheck[["combination"]] %>%
    count(year, T_vcount_comb) %>%
    filter(n >= 20) %>%
    pull(T_vcount_comb) %>%
    unique()

# Filtering Frühling/Winter as it only has very small n < 10
res19_VarroaCheck$result_comb <- fGlmNullModel(
    res19_VarroaCheck[["combination"]] %>%
        filter(
            T_vcount_comb %in% tPlotFilter
        ),
    "T_vcount_comb"
)

logiLevels <- tLevels %in% unique(res19_VarroaCheck[["result_comb"]][["T_vcount_comb"]])

res19_VarroaCheck[["result_comb"]] <- res19_VarroaCheck[["result_comb"]] %>%
    mutate(
        T_vcount_comb = forcats::fct_relevel(T_vcount_comb, tLevels[logiLevels])
    ) %>%
    arrange(year, T_vcount_comb)

res19_VarroaCheck$chi_comb <- fChistar(res19_VarroaCheck$result_comb, "T_vcount_comb")
# manual setting because its simply doesnt work with so many sign. :/
res19_VarroaCheck$chi_comb$y <- c(22, 40, 30, 55, 65, 70)

res19_VarroaCheck$result_comb <- res19_VarroaCheck$result_comb %>%
    mutate(
        fill = if_else(T_vcount_comb == "Keine\nKontrolle", "gray60", "white")
    )

res19_VarroaCheck$p_comb <- fPlot(
    res19_VarroaCheck$result_comb,
    res19_VarroaCheck$chi_comb,
    "T_vcount_comb",
    xTitle = "",
    facet_scales = "free_x",
    facet_cols = 1,
    fillCross = TRUE,
    allData = TRUE,
    expandMax = 0.1,
    raw = res19_VarroaCheck[["combination"]] %>%
        filter(
            T_vcount_comb %in% tPlotFilter
        )
)

fSaveImages("19_VarroaControlComb", res19_VarroaCheck$p_comb, h = 10)

# Histogramm -----------------------------------------------------------------
tempDataHist <- tempData %>%
    filter(varroa_checked == "Ja" & T_vcount_total12 > 0)

res19_VarroaCheck[["hist"]] <- fHistTreatment(tempDataHist, c("T_vcount_01", "T_vcount_12"))

fSaveImages("19_VarroaControlDistr", res19_VarroaCheck[["hist"]][["plot"]], h = 10)

# Method Analysis -----------------------------------------------------------------
# Alcohol wash
# Sticky board (or other collection tray below hive)
# Sugar shake / roll
# Visual inspection of adult bees
# Visual inspection of drone brood
# Sent sample to lab
# Other
# Auswaschmethode	Bodeneinlage (Varroatasse, Stockwindel...)	Staubzuckermethode	Visuelle Inspektion erwachsener Bienen	Visuelle Inspektion Drohnenbrut	Laboranalyse eingesandter Proben	Sonstiges

res19_VarroaCheckMethod <- list()

res19_VarroaCheckMethod$deNames <- c(
    checked_washing = "Auswaschmethode",
    checked_floor = "Bodeneinlage",
    checked_sugar = "Staubzuckermethode",
    checked_visual = "Visuelle Inspektion erwachsener Bienen",
    checked_drone = "Visuelle Inspektion Drohnenbrut",
    checked_lab = "Laboranalyse eingesandter Proben",
    checked_other = "Sonstiges"
)

res19_VarroaCheckMethod$deNamesShort <- c(
    checked_washing = "Auswaschen",
    checked_floor = "Bodeneinlage",
    checked_sugar = "Staubzucker",
    checked_visual = "Visuell Bienen",
    checked_drone = "Drohnenbrut",
    checked_lab = "Labor",
    checked_other = "Sonstiges"
)

res19_VarroaCheckMethod$numberMethodsDf <- dfData %>%
    filter(year == "20/21") %>%
    select(id, hives_lost_e, hives_spring_e, hives_winter, starts_with("checked_")) %>%
    mutate(
        # create Binary
        across(-c("id", "hives_lost_e", "hives_spring_e", "hives_winter"), stringr::str_replace, pattern = "N/A", replacement = NA_character_),
        across(-c("id", "hives_lost_e", "hives_spring_e", "hives_winter"), stringr::str_replace, pattern = "[^(Nein)].*", replacement = "1"),
        across(-c("id", "hives_lost_e", "hives_spring_e", "hives_winter"), stringr::str_replace, pattern = "Nein", replacement = "0"),
        across(-c("id", "hives_lost_e", "hives_spring_e", "hives_winter"), as.integer)
    ) %>%
    mutate(
        s = rowSums(.[5:11], na.rm = TRUE)
    ) %>%
    filter(s != 0)

res19_VarroaCheckMethod$tableData <- res19_VarroaCheckMethod$numberMethodsDf %>%
    select(starts_with("checked_")) %>%
    pivot_longer(everything()) %>%
    count(name, value) %>%
    filter(value != 0) %>%
    mutate(
        Methode = str_replace_all(name, res19_VarroaCheckMethod$deNames),
        p = n / nrow(res19_VarroaCheckMethod$numberMethodsDf) * 100,
    ) %>%
    mutate(
        Prozent = p %>% round(., 1) %>% fPrettyNum()
    ) %>%
    arrange(desc(p))

tempLabel <- glue::glue("Anzahl der Antworten zu den vewendeten Methoden zur Bestimmung des Varroabefalls, im Umfragejahr 2020/21. Insgesamt gaben {nrow(res19_VarroaCheckMethod$numberMethodsDf)} TeilnehmerInnen eine Antwort zu dieser Frage ab. Mehrfachantworten sind möglich.")

res19_VarroaCheckMethod$table <- res19_VarroaCheckMethod$tableData %>%
    select(Methode, Meldungen = n, Prozent) %>%
    kable(
        "latex",
        caption = tempLabel,
        label = "u:19:VarroaControlMethods",
        booktabs = T,
        escape = F,
        align = c("l", rep("r", 2))
    ) %>%
    kable_styling(latex_options = "HOLD_position", font_size = 8)

res19_VarroaCheckMethod$table %>% save_kable(paste0("output/tables/19_VarroaControlMethods.tex"))


# Check Loss Rate for common Combinations
res19_VarroaCheckMethod$glmDf <- res19_VarroaCheckMethod$numberMethodsDf %>%
    mutate(
        unite(
            across(starts_with("checked_"), ~ ifelse(. == 1, cur_column(), NA)),
            na.rm = T,
            col = "checked_combined"
        )
    ) %>%
    mutate(
        checked_combined = stringr::str_replace_all(checked_combined, res19_VarroaCheckMethod$deNamesShort),
        checked_combined = stringr::str_replace_all(checked_combined, "_", " &\n")
        # checked_combined = stringr::str_remove_all(checked_combined, "checked_")
    )

# Only use the common ones (at least 10)
tmpCommon <- res19_VarroaCheckMethod$glmDf %>%
    count(checked_combined) %>%
    filter(n > 10) %>%
    pull(checked_combined) %>%
    unique()

# Temp no Control
tmp <- dfData %>%
    filter(year == "20/21", varroa_checked == "Nein") %>%
    mutate(
        checked_combined = "Keine\nKontrolle"
    ) %>%
    select(id, hives_lost_e, hives_spring_e, hives_winter, checked_combined)

res19_VarroaCheckMethod$glmDf <- bind_rows(
    res19_VarroaCheckMethod$glmDf %>% filter(checked_combined %in% tmpCommon),
    tmp
)

res19_VarroaCheckMethod$result <- fGlmNullModel(res19_VarroaCheckMethod$glmDf %>% mutate(year = "20/21"), "checked_combined")

res19_VarroaCheckMethod$p <- fPlot(
    res19_VarroaCheckMethod$result,
    tibble(),
    "checked_combined",
    xTitle = "Varroa Diagnose Methode",
)


fSaveImages("19_VarroaControlMethods", res19_VarroaCheckMethod$p, w = 8.5)