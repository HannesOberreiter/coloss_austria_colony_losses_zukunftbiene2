fPlot <- function(x, chi, f, xTitle = "", dropNoAnswer = FALSE, facet_scales = "fixed", facet_cols = 2, yMax = FALSE, fillCross = FALSE, yTitle = "Verlustrate [%]", yOffset = 2) {
  myvar <- rlang::sym(f)

  # Facet ~ Labels
  # year (n=participants)
  labels <- fLabeller(x)

  if (dropNoAnswer) {
    x <- x %>%
      filter(
        !(!!myvar %in% c("keine Angaben", "Unsicher", "keine\nAngaben"))
      )
    if (nrow(chi) > 0) {
      chi <- chi %>%
        filter(
          !(start %in% c("keine Angaben", "Unsicher", "keine\nAngaben")) &
            !(end %in% c("keine Angaben", "Unsicher", "keine\nAngaben"))
        )
    }
  }

  ## Special Color Filling
  filling <- "white"
  if (fillCross) {
    filling <- x %>% pull(fill)
  }

  p <- x %>%
    ggplot(aes(x = {{ myvar }}, y = middle, color = year)) +
    geom_crossbar(
      aes(ymin = lower, ymax = upper, fill = I(filling))
    ) +
    geom_point(
      size = 3
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    scale_colour_manual(values = colorBlindBlack8[-1], aesthetics = "color", guide = FALSE) +
    ylab(yTitle) +
    xlab(xTitle) +
    geom_text(
      aes(
        y = 0.8,
        label = glue("n = {n}")
      ),
      vjust = 0,
      color = "black",
      size = 3
    ) +
    scale_y_continuous(
      limits = c(0, max(x$upper, chi$y) + yOffset),
      expand = expansion(mult = c(0, 0.1))
    ) +
    facet_wrap(
      ~year,
      ncol = facet_cols,
      labeller = labels,
      scales = facet_scales
    )

  if (nrow(chi) > 0) {
    p <- p +
      geom_signif(
        data = chi,
        aes(xmin = start, xmax = end, annotations = label, y_position = y),
        textsize = 8, manual = TRUE, vjust = 0.5
      )
  }
  return(p)
}