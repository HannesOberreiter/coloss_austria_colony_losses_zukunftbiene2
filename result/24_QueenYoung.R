res24_QueenYoung <- list()
myFactor <- "young_queens_group"
factorOrder <- c("0-25%", "26-50%", "51-75%", "76-100%")
tempData <- dfData %>%
    drop_na(young_queens) %>%
    mutate(
        young_queens_rate = young_queens * 100 / hives_winter,
        young_queens_group = case_when(
            between(young_queens_rate, 0, 25) ~ factorOrder[[1]],
            between(young_queens_rate, 26, 50) ~ factorOrder[[2]],
            between(young_queens_rate, 51, 75) ~ factorOrder[[3]],
            TRUE ~ factorOrder[[4]]
        ),
        young_queens_group = forcats::fct_relevel(young_queens_group, factorOrder)
    )

# We get sometimes illogical entries with above 100% young_queens_rate
# tempData %>% select(id, hives_winter, young_queens, young_queens_rate)

res24_QueenYoung$freq <- tempData %>%
    add_count(year) %>%
    group_by(year, young_queens_group) %>%
    summarise(
        nBeek = n(),
        nBeek_print = format(nBeek, big.mark = ".", decimal.mark = ","),
        npBeek = fNumberFormat(nBeek * 100 / sum(n[[1]]))
    )
res24_QueenYoung$freq

res24_QueenYoung$freqHives <- tempData %>%
    group_by(year) %>%
    summarise(
        hives_winter = sum(hives_winter),
        young_queens = sum(young_queens),
        hives_winter_print = format(hives_winter, big.mark = ".", decimal.mark = ","),
        young_queens_print = format(young_queens, big.mark = ".", decimal.mark = ","),
        young_queens_rate = round(young_queens * 100 / hives_winter, 1),
        young_queens_rate_print = format(young_queens_rate, big.mark = ".", decimal.mark = ","),
    ) %>%
    select(year, ends_with("_print"))

# Young Queen Rate Table -----
res24_QueenYoung$freqHives %>%
    mutate(
        year = paste0("20", year)
    ) %>%
    knitr::kable(
        "latex",
        caption = "Anteil an jungen Königinnen von in Österreich. Zur Berechnung des Anteils an jungen Königinnen wurden nur TeilnehmerInnen herangezogen welche die Frage \\enquote{Wie viele Ihrer eingewinterten Völker hatten eine im Jahr 2019 begattete („junge``) Königin?} beantwortet haben",
        label = "24_QueenYoung_Rate",
        booktabs = T,
        escape = F,
        linesep = "", # https://stackoverflow.com/questions/45409750/get-rid-of-addlinespace-in-kable
        col.names = c("Jahr", "Völker [n]", "Junge Königinnen [n]", "Junge Königinnen [\\%]"),
        align = c("c", rep("r", 3))
    ) %>%
    kable_styling(latex_options = "HOLD_position", font_size = 10) %>%
    sub("midrule{}", "midrule", ., fixed = T) %>%
    save_kable("output/tables/24_QueenYoung_Rate.tex")

# Queen Young -- Queen Loss Rate -----
tempDataQueenLoss <- tempData %>%
    mutate(
        # change our loss to queen loss (lost_a)
        hives_lost_e = lost_a,
        hives_spring_e = hives_spring_queen
    )
res24_QueenYoung$resultQueenLoss <- fGlmNullModel(tempDataQueenLoss, myFactor)
res24_QueenYoung$chiQueenLoss <- fChistar(res24_QueenYoung$resultQueenLoss, myFactor) %>%
    mutate(
        # offset Playing as Queens Loss Rate is a lot lower
        # y = 4 + y - (y * 0.6)
    )
res24_QueenYoung$pQueenLoss <- fPlot(
    res24_QueenYoung$resultQueenLoss,
    res24_QueenYoung$chiQueenLoss,
    myFactor,
    yTitle = "Verlustrate nur mit Verluste durch Königinnen Probleme [%]",
    allData = TRUE,
    raw = tempDataQueenLoss
)
fSaveImages("24_QueenYoung_QueenLoss", res24_QueenYoung$pQueenLoss, w = 8.5)
caption <- "Höhe der Winterverluste (nur Verluste durch unlösbare Königinnenprobleme) in Prozent (und 95\\%~CI) und Anzahl der Antworten in Abhängigkeit vom Prozentsatz junger Königinnen pro Imkerei über die Umfragejahre 2017/18 - 2020/21."
fSaveTable(
    "24_QueenYoung_QueenLoss",
    res24_QueenYoung$resultQueenLoss %>%
        mutate(young_queens_group = stringr::str_replace(young_queens_group, "%", "\\\\%")),
    caption,
    myFactor,
    "u:24QueenYoungQueenLoss"
)

# Queen Problems -- Without Queen Loss Rate -----
tempDataNormalLoss <- tempData %>%
    mutate(
        # change our loss to "normal losses" (lost_c)
        hives_lost_e = lost_c,
        hives_spring_e = hives_spring_e + lost_a # add queen losses
    )
res24_QueenYoung$resultNormalLoss <- fGlmNullModel(tempDataNormalLoss, myFactor)
res24_QueenYoung$chiNormalLoss <- fChistar(res24_QueenYoung$resultNormalLoss, myFactor)

# Manually setting some sign bars to keep me sane
res24_QueenYoung$chiNormalLoss$y[3] <- 45
res24_QueenYoung$chiNormalLoss$y[7] <- 20

res24_QueenYoung$pNormalLoss <- fPlot(
    res24_QueenYoung$resultNormalLoss,
    res24_QueenYoung$chiNormalLoss,
    myFactor,
    yTitle = "Verlustrate ohne Verluste durch Königinnen Probleme [%]",
    allData = TRUE,
    raw = tempDataNormalLoss
)
fSaveImages("24_QueenYoung_NormalLoss", res24_QueenYoung$pNormalLoss, w = 8.5)
caption <- "Höhe der Winterverluste (exklusive den Verlusten durch unlösbare Königinnenprobleme.) in Prozent (und 95\\%~CI) und Anzahl der Antworten in Abhängigkeit vom Prozentsatz junger Königinnen pro Imkerei über die Umfragejahre 2017/18 - 2020/21."
fSaveTable(
    "24_QueenYoung_NormalLoss",
    res24_QueenYoung$resultNormalLoss %>%
        mutate(young_queens_group = stringr::str_replace(young_queens_group, "%", "\\\\%")),
    caption,
    myFactor,
    "u:24QueenYoungNormalLoss"
)