res21_Combination <- list()

# Ugly Workaround
# it will change Ox trickling to Ox mix
# then change the first occurence of Ox mix to combined
# then remove possible double entries (eg. participants which did trickling pure and mix in the same month will be together)
tempData <- dfData %>%
    filter(submitted != "Zeitung") %>%
    mutate(
        c_short_od_comb = c_short_od %>%
            str_replace_all(glue::glue("{treatmentList$tshort[[8]]}"), glue::glue("{treatmentList$tshort[[10]]}")),
        c_short_od_comb = c_short_od_comb %>%
            str_replace(glue::glue("F-{treatmentList$tshort[[10]]}"), "F-Ox-Träu*"),
        c_short_od_comb = c_short_od_comb %>%
            str_replace(glue::glue("S-{treatmentList$tshort[[10]]}"), "S-Ox-Träu*"),
        c_short_od_comb = c_short_od_comb %>%
            str_replace(glue::glue("W-{treatmentList$tshort[[10]]}"), "W-Ox-Träu*"),
        c_short_od_comb = c_short_od_comb %>%
            str_replace(glue::glue("F-{treatmentList$tshort[[10]]}*[&]"), ""),
        c_short_od_comb = c_short_od_comb %>%
            str_replace(glue::glue("S-{treatmentList$tshort[[10]]}*[&]"), ""),
        c_short_od_comb = c_short_od_comb %>%
            str_replace(glue::glue(" &? W-{treatmentList$tshort[[10]]}*&?"), ""),
    ) %>%
    drop_na(c_short_od_comb)

# Global Loss of Filtered Data, we use for Plot
res21_Combination$global <- tempData %>%
    fGlmNullModel(., "global")

# Filter Combination which were used at least 15 times in one year
res21_Combination$uniques <- tempData %>%
    count(year, c_short_od_comb, sort = TRUE) %>%
    filter(n >= 15) %>%
    mutate(
        c_short_od_comb = forcats::fct_reorder(c_short_od_comb, n)
    ) %>%
    pull(c_short_od_comb) %>%
    unique()

res21_Combination$result <- tempData %>%
    filter(c_short_od_comb %in% res21_Combination$uniques) %>%
    fGlmNullModel(., "c_short_od_comb")

# Label Combinations based on first year
res21_Combination$resultLetters <- res21_Combination$result %>%
    filter(year == "17/18") %>%
    arrange(desc(n)) %>%
    mutate(
        c_short_od_comb = forcats::fct_reorder(c_short_od_comb, n),
        labelABC = LETTERS[1:n()],
        labelCount = str_count(c_short_od_comb, "&") + 1,
        labelCountColor = case_when(
            labelCount == 1 ~ "white",
            labelCount == 2 ~ "gray",
            TRUE ~ "black"
        ),
        labelABCcolor = case_when(
            labelCount == 1 ~ "black",
            labelCount == 2 ~ "black",
            TRUE ~ "white"
        )
    ) %>%
    select(c_short_od_comb, labelABC:labelABCcolor)

plotOrder <- as.character(res21_Combination$resultLetters$c_short_od_comb)

res21_Combination$result <- res21_Combination$resultLetters %>%
    right_join(res21_Combination$result)

res21_Combination$result <- res21_Combination$result %>%
    # filter(year %in% c("17/18", "18/19")) %>%
    mutate(
        c_short_od_comb = forcats::fct_relevel(c_short_od_comb, plotOrder),
        c_short_od_comb_label = str_replace_all(c_short_od_comb, " & ", "\n"),
    )

res21_Combination$labels <- fLabeller(res21_Combination$global)

res21_Combination$p <- res21_Combination$result %>%
    ggplot(aes(y = middle, x = c_short_od_comb)) +
    geom_hline(
        data = res21_Combination$global,
        aes(yintercept = middle),
        color = colorBlindBlack8[7],
        size = 0.5
    ) +
    geom_hline(
        data = res21_Combination$global,
        aes(yintercept = lower),
        linetype = "dashed",
        color = colorBlindBlack8[7],
        size = 0.5
    ) +
    geom_hline(
        data = res21_Combination$global,
        aes(yintercept = upper),
        linetype = "dashed",
        color = colorBlindBlack8[7],
        size = 0.5
    ) +
    geom_crossbar(
        aes(ymin = lower, ymax = upper),
        fill = "white",
        alpha = 0.8
    ) +
    geom_point(
        shape = 21,
        aes(fill = I(labelCountColor), color = I("black")),
        size = 5
    ) +
    geom_text(
        aes(label = labelABC, color = I(labelABCcolor))
    ) +
    geom_text(
        aes(y = 0, label = glue::glue("{n}")),
        nudge_y = 1,
        size = 2.5
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    scale_y_continuous(
        minor_breaks = seq(0, 100, 5),
        breaks = seq(0, 100, 10),
        expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_discrete(
        labels = rep(unique(res21_Combination$result$c_short_od_comb_label), times = 4),
    ) +
    coord_cartesian(
        ylim = c(0, 30),
        clip = "on"
    ) +
    xlab("") +
    ylab("Verlustrate [%]") +
    theme(
        axis.text.x = element_text(size = 4, vjust = 0.9),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "lightgrey")
    ) +
    facet_wrap(
        ~year,
        ncol = 1,
        labeller = res21_Combination$labels,
    )
fSaveImages("21_Combination", res21_Combination$p, h = 9.75, w = 8.25)

# Create a Helper Table
replString <- c(treatmentList$tname)
names(replString) <- treatmentList$tshort
# replSeason <- c("F-" = "Frühjahr ", "S-" = "Sommer ", "W-" = "Winter ")
replSeason <- c("F-" = "F  ", "S-" = "S ", "W-" = "W ")
res21_Combination$tab <- res21_Combination$result %>%
    filter(year == "17/18") %>%
    select(labelABC, c_short_od_comb) %>%
    mutate(
        short = glue::glue("({labelABC}) {c_short_od_comb}"),
        long = c_short_od_comb %>%
            str_replace_all(replString),
        long = long %>% str_replace_all(replSeason),
        long = long %>% str_replace_all("&", "\\\\&"),
        long = long %>% str_replace_all("Träufeln o. Sprühen", "Träufeln o. Mischung*"),
        short = short %>% str_replace_all("&", "\\\\&")
    ) %>%
    select(short, long) %>%
    knitr::kable(
        "latex",
        caption = "Erklärung zu den abgekürzten Kombinationen. W = Winter, S = Sommer, Einzelne Monate sind nach Saison zusammengefasst. Die Behandlungsmethoden Oxalsäure Träufeln o. Sprühen und Oxalsäure Mischung wurden zusammengefasst.",
        label = "u:21comb",
        booktabs = T,
        escape = F,
        linesep = "", # https://stackoverflow.com/questions/45409750/get-rid-of-addlinespace-in-kable
        col.names = c("Abkürzung", "Beschreibung"),
        align = c("l", "l")
    ) %>%
    kable_styling(latex_options = "HOLD_position", font_size = 10)

# small bugfix as pack_rows introduces an linespaceing which breaks midrule
res21_Combination$tab <- sub("midrule{}", "midrule", res21_Combination$tab, fixed = T)
res21_Combination$tab %>% save_kable(paste0("output/tables/21_Comb.tex"))