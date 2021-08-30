res23_QueenProblems <- list()
myFactor <- "queen_problems"
factorOrder <- c("Häufiger", "Normal", "Seltener", "Weiß nicht")

tempData <- dfData %>%
    drop_na(!!sym(myFactor)) %>%
    mutate(
        queen_problems = forcats::fct_relevel(!!sym(myFactor), factorOrder)
    )

res23_QueenProblems$freq <- tempData %>%
    add_count(year) %>%
    group_by(year, queen_problems) %>%
    summarise(
        nBeek = n(),
        nBeek_print = format(nBeek, big.mark = ".", decimal.mark = ","),
        npBeek = fNumberFormat(nBeek * 100 / sum(n[[1]]))
    )

# Queen Problems -- Queen Loss Rate -----
tempDataQueenLoss <- tempData %>%
    mutate(
        # change our loss to queen loss (lost_a)
        hives_lost_e = lost_a,
        hives_spring_e = hives_spring_queen
    )
res23_QueenProblems$resultQueenLoss <- fGlmNullModel(tempDataQueenLoss, myFactor)
res23_QueenProblems$chiQueenLoss <- fChistar(res23_QueenProblems$resultQueenLoss, myFactor) %>%
    mutate(
        # offset Playing as Queens Loss Rate is a lot lower
        # y = 4 + y - (y * 0.6)
    )
res23_QueenProblems$pQueenLoss <- fPlot(
    res23_QueenProblems$resultQueenLoss,
    res23_QueenProblems$chiQueenLoss,
    myFactor,
    yTitle = "Verlustrate nur mit Verluste durch Königinnen Probleme [%]",
    allData = TRUE,
    raw = tempDataQueenLoss
)
fSaveImages("23_QueenProblems_QueenLoss", res23_QueenProblems$pQueenLoss, w = 8.5)
caption <- "Höhe der Winterverluste (nur Verluste durch unlösbare Königinnenprobleme) in Prozent (und 95\\%~CI) und Anzahl der Antworten im Zusammenhang mit den beobachteten Königinnenproblemen über die Umfragejahre 2017/18 - 2020/21."
fSaveTable("23_QueenProblemsQueenLoss", res23_QueenProblems$resultQueenLoss, caption, myFactor, "u:23QueenProblemsQueenLoss")

# Queen Problems -- Without Queen Loss Rate -----
tempDataNormalLoss <- tempData %>%
    mutate(
        # change our loss to "normal losses" (lost_c)
        hives_lost_e = lost_c,
        hives_spring_e = hives_spring_e + lost_a # add queen losses
    )
res23_QueenProblems$resultNormalLoss <- fGlmNullModel(tempDataNormalLoss, myFactor)
res23_QueenProblems$chiNormalLoss <- fChistar(res23_QueenProblems$resultNormalLoss, myFactor)
res23_QueenProblems$pNormalLoss <- fPlot(
    res23_QueenProblems$resultNormalLoss,
    res23_QueenProblems$chiNormalLoss,
    myFactor,
    yTitle = "Verlustrate ohne Verluste durch Königinnen Probleme [%]",
    allData = TRUE,
    raw = tempDataNormalLoss
)
fSaveImages("23_QueenProblems_NormalLoss", res23_QueenProblems$pNormalLoss, w = 8.5)
res23_QueenProblems$pNormalLoss
caption <- "Höhe der Winterverluste (exklusive den Verlusten durch unlösbare Königinnenprobleme.) in Prozent (und 95\\%~CI) und Anzahl der Antworten im Zusammenhang mit den beobachteten Königinnenproblemen über die Umfragejahre 2017/18 - 2020/21."
fSaveTable("23_QueenProblemsNormalLoss", res23_QueenProblems$resultNormalLoss, caption, myFactor, "u:23QueenProblemsNormalLoss")