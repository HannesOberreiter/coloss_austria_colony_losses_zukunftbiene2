res20_Treatments <- list()
res20_Treatments$hist <- list()
res20_Treatments$combination <- list()
res20_Treatments$result_comb <- list()
res20_Treatments$chi_comb <- list()
res20_Treatments$p_comb <- list()
tempData <- dfData %>%
    filter(submitted != "Zeitung") %>%
    # Helper Col to Summarise the different synthetic methods
    mutate(
        T_synthetic_01 = rowSums(across(matches("T_synthetic.*01")), na.rm = T),
        T_synthetic_02 = rowSums(across(matches("T_synthetic.*02")), na.rm = T),
        T_synthetic_03 = rowSums(across(matches("T_synthetic.*03")), na.rm = T),
        T_synthetic_04 = rowSums(across(matches("T_synthetic.*04")), na.rm = T),
        T_synthetic_05 = rowSums(across(matches("T_synthetic.*05")), na.rm = T),
        T_synthetic_06 = rowSums(across(matches("T_synthetic.*06")), na.rm = T),
        T_synthetic_07 = rowSums(across(matches("T_synthetic.*07")), na.rm = T),
        T_synthetic_08 = rowSums(across(matches("T_synthetic.*08")), na.rm = T),
        T_synthetic_09 = rowSums(across(matches("T_synthetic.*09")), na.rm = T),
        T_synthetic_10 = rowSums(across(matches("T_synthetic.*10")), na.rm = T),
        T_synthetic_11 = rowSums(across(matches("T_synthetic.*11")), na.rm = T),
        T_synthetic_12 = rowSums(across(matches("T_synthetic.*_12")), na.rm = T)
    )

# Histogramm -----------------------------------------------------------------
for (treatment in treatmentList$tsingle[-1]) {
    message(glue::glue("Histogramm: {treatment}"))
    res20_Treatments$hist[[treatment]] <- fHistTreatment(
        tempData %>% filter(!!sym(glue::glue("{treatment}total12")) > 0),
        c(glue::glue("{treatment}01"), glue::glue("{treatment}12"))
    )

    fSaveImages(glue::glue("20_TreatmentDistr_{treatment}"), res20_Treatments$hist[[treatment]]$p, h = 10)
}

# Combination -----------------------------------------------------------------
for (treatment in treatmentList$tsingle[-1]) {
    # for (treatment in treatmentList$tsingle[10]) {
    message(glue::glue("Plot and Table: {treatment}"))
    tComb <- glue::glue("{treatment}comb")
    tSpring <- glue::glue("{treatment}totalyn_spring")
    tSummer <- glue::glue("{treatment}totalyn_summer")
    tWinter <- glue::glue("{treatment}totalyn_winter")
    tLevels <- c(
        "Nur\nFrühling", "Nur\nSommer", "Nur\nWinter",
        "Frühling\nSommer", "Sommer\nWinter", "Frühling\nWinter",
        "Frühling\nSommer\nWinter"
    )
    tNo <- treatmentList %>%
        filter(tsingle == treatment) %>%
        pull(tname)
    tNoShort <- treatmentList %>%
        filter(tsingle == treatment) %>%
        pull(tshort)
    tKein <- if_else(tNo == "Thymol", "Kein", "Keine")
    tLevels <- c(tLevels, glue::glue("{tKein}\n{tNoShort}"))

    res20_Treatments$combination[[treatment]] <- tempData %>%
        mutate(
            !!tComb := case_when(
                # Only one Season
                !!sym(tSpring) == 1 & (!!sym(tSummer) + !!sym(tWinter)) == 0 ~ tLevels[[1]],
                !!sym(tSummer) == 1 & (!!sym(tSpring) + !!sym(tWinter)) == 0 ~ tLevels[[2]],
                !!sym(tWinter) == 1 & (!!sym(tSummer) + !!sym(tSpring)) == 0 ~ tLevels[[3]],
                # Combinations of Seasons
                !!sym(tWinter) == 0 & (!!sym(tSummer) + !!sym(tSpring)) == 2 ~ tLevels[[4]],
                !!sym(tSpring) == 0 & (!!sym(tSummer) + !!sym(tWinter)) == 2 ~ tLevels[[5]],
                !!sym(tSummer) == 0 & (!!sym(tWinter) + !!sym(tSpring)) == 2 ~ tLevels[[6]],
                # All three Seasons
                (!!sym(tWinter) + !!sym(tSummer) + !!sym(tSpring)) == 3 ~ tLevels[[7]],
                # No Treatment
                TRUE ~ tLevels[[8]]
            )
        )
    logiLevels <- tLevels %in% unique(res20_Treatments$combination[[treatment]][[tComb]])
    res20_Treatments$combination[[treatment]] <- res20_Treatments$combination[[treatment]] %>%
        mutate(
            !!tComb := forcats::fct_relevel(!!sym(tComb), tLevels[logiLevels])
        )

    # GLM everything for table
    res20_Treatments$result_table[[treatment]] <- fGlmNullModel(
        res20_Treatments$combination[[treatment]],
        tComb
    ) %>%
        mutate(
            !!tComb := forcats::fct_relevel(!!sym(tComb), tLevels[logiLevels])
        )

    # GLM only for n >= 15, which we plot
    tPlotFilter <- res20_Treatments$combination[[treatment]] %>%
        count(year, !!sym(tComb)) %>%
        filter(n >= 15) %>%
        pull(!!sym(tComb)) %>%
        unique()

    res20_Treatments$result_comb[[treatment]] <- fGlmNullModel(
        res20_Treatments$combination[[treatment]] %>%
            filter(
                !!sym(tComb) %in% tPlotFilter
            ),
        tComb
    )
    logiLevels <- tLevels %in% unique(res20_Treatments$result_comb[[treatment]][[tComb]])
    res20_Treatments$result_comb[[treatment]] <- res20_Treatments$result_comb[[treatment]] %>%
        mutate(
            !!tComb := forcats::fct_relevel(!!sym(tComb), tLevels[logiLevels])
        ) %>%
        arrange(year, !!sym(tComb))

    tCols <- ifelse(nrow(res20_Treatments$result_comb[[treatment]]) <= 12, 2, 1)
    tExpand <- ifelse(nrow(res20_Treatments$result_comb[[treatment]]) <= 12, 0.07, 0.1)
    tFree <- ifelse(nrow(res20_Treatments$result_comb[[treatment]]) <= 12, "fixed", "free_x")
    tSize <- ifelse(nrow(res20_Treatments$result_comb[[treatment]]) <= 12, 6, 10)

    res20_Treatments$chi_comb[[treatment]] <- fChistar(res20_Treatments$result_comb[[treatment]], tComb)

    # Special Color if No Treatment
    res20_Treatments$result_comb[[treatment]] <- res20_Treatments$result_comb[[treatment]] %>%
        mutate(
            fill = if_else(!!sym(tComb) == tLevels[[8]], "gray60", "white")
        )

    res20_Treatments$p_comb[[treatment]] <- fPlot(
        res20_Treatments$result_comb[[treatment]],
        res20_Treatments$chi_comb[[treatment]],
        tComb,
        xTitle = "",
        facet_scales = tFree,
        facet_cols = tCols,
        fillCross = TRUE,
        expandMax = tExpand,
        allData = TRUE,
        raw = res20_Treatments$combination[[treatment]] %>%
            filter(
                !!sym(tComb) %in% tPlotFilter
            )
    )

    fSaveImages(glue::glue("20_Comb{treatment}"), res20_Treatments$p_comb[[treatment]], h = tSize)

    caption <- glue::glue("Höhe der Winterverluste in Prozent ($\\pm$95\\%CI) und Anzahl der Antworten ob und wann eine {tNo} durchgeführt wurde oder nicht über die Umfragejahre 2017/18 - 2020/21")
    fSaveTable(
        glue::glue("20_Comb{treatment}"),
        res20_Treatments$result_table[[treatment]] %>%
            mutate(
                !!tComb := str_replace_all(!!sym(tComb), "\n", " ")
            ),
        caption,
        tComb,
        glue::glue("u:20treatment{treatment}"),
        fontSize = 6
    )
}