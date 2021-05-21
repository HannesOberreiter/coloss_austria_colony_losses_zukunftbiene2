res22_QueenState <- list()

myFactor <- "state"
res22_QueenState$names <- c(
    "Bgld." = "Burgenland",
    "Ktn."  = "Kärnten",
    "NÖ"    = "Niederösterreich",
    "OÖ"    = "Oberösterreich",
    "Sbg."  = "Salzburg",
    "Stmk." = "Steiermark",
    "T"     = "Tirol",
    "Vbg."  = "Vorarlberg",
    "W"     = "Wien"
)

tempData <- dfData %>%
    mutate(
        state = as.factor(state) %>% fct_relevel(res22_QueenState$names),
        # change our loss to queen loss (lost_a)
        hives_lost_e = lost_a,
        hives_spring_e = hives_spring_queen
    )

res22_QueenState$result <- fGlmNullModel(tempData, myFactor)
res22_QueenState$global <- fGlmNullModel(tempData, "global")
res22_QueenState$labels <- fLabeller(res22_QueenState$result)


# Queen Boxplot ----------------------------------------------------------
res22_QueenState$p <- res22_QueenState$result %>%
    ggplot(aes(x = state, y = middle, color = year)) +
    # Austria
    geom_hline(
        data = res22_QueenState$global,
        aes(yintercept = middle),
        size = 1,
        color = colorBlindBlack8[7]
    ) +
    geom_hline(
        data = res22_QueenState$global,
        aes(yintercept = upper),
        color = colorBlindBlack8[7],
        linetype = "dashed"
    ) +
    geom_hline(
        data = res22_QueenState$global,
        aes(yintercept = lower),
        color = colorBlindBlack8[7],
        linetype = "dashed"
    ) +
    # States
    geom_crossbar(
        aes(ymin = lower, ymax = upper),
        fill = "white",
        alpha = 0.8
    ) +
    geom_point(
        size = 3
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    scale_colour_manual(values = colorBlindBlack8[-1], aesthetics = "color", guide = FALSE) +
    ylab("Verlustrate (Unlösbare Königinnenprobleme) [%]") +
    xlab("") +
    geom_text(
        aes(
            y = 0.5,
            label = glue("n = {n}")
        ),
        vjust = 0,
        color = "black",
        size = 3
    ) +
    scale_y_continuous(
        limits = c(0, max(res22_QueenState$result$upper) + 1),
        expand = expansion(mult = c(0, 0.1))
    ) +
    facet_wrap(
        ~year,
        scales = "free_x",
        ncol = 1,
        labeller = res22_QueenState$labels
    ) +
    scale_x_discrete(labels = names(res22_QueenState$names))

fSaveImages("22_QueenState", res22_QueenState$p, h = 10)


# Queen Table ----------------------------------------------------------
caption <- "Höhe der Winterverluste durch unlösbare Königinnenprobleme in Prozent ($\\pm$95\\%CI) und Anzahl der Antworten für die einzelnen Bundesländer sowie Österreich gesamt über die Umfragejahre 2017/18 - 2020/21."

res22_QueenState$result_tab <- res22_QueenState$global %>%
    mutate(
        state = "Österreich"
    ) %>%
    bind_rows(res22_QueenState$result) %>%
    mutate(
        year = as.factor(year),
        state = as.factor(state) %>% fct_relevel(c("AUT" = "Österreich", res22_QueenState$names))
    ) %>%
    ungroup() %>%
    arrange(year, state)

fSaveTable("22_QueenStateRate", res22_QueenState$result_tab, caption, myFactor, "u:22QueenState")