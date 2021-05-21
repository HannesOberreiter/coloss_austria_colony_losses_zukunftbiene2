fHistTreatment <- function(df, cols) {
    result <- df %>%
        group_by(year) %>%
        summarise(
            across(
                cols[[1]]:cols[[2]], ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        tidyr::pivot_longer(-year, names_to = "time", values_to = "n") %>%
        group_by(year) %>%
        mutate(
            freq = proportions(n) * 100,
            freq_print = fPrettyNum(freq)
        ) %>%
        ungroup() %>%
        mutate(
            group = rep(treatmentMonths[["groups"]], 4)
        )

    labels <- fLabeller(df %>% count(year))

    p <- result %>%
        ggplot(
            aes(x = time, y = n, fill = group)
        ) +
        geom_col() +
        scale_y_continuous(
            breaks = scales::pretty_breaks(),
            expand = expansion(mult = c(0, 0.1))
        ) +
        theme(
            axis.line.x = element_line(linetype = "solid", size = 0.5),
            legend.position = "top"
        ) +
        scale_colour_manual(
            values = unique(treatmentMonths[["color"]]),
            aesthetics = "fill",
            guide = guide_legend("Jahreszeit")
        ) +
        scale_x_discrete(
            labels = treatmentMonths[["short"]]
        ) +
        facet_wrap(
            ~year,
            scales = "free_x",
            ncol = 1,
            labeller = labels
        ) +
        xlab("") +
        ylab("Anzahl Meldungen [n]")

    return(
        list(
            "result" = result,
            "plot" = p
        )
    )
}