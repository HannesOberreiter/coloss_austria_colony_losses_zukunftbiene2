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