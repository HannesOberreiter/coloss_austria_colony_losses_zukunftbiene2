fPlotOld <- function(x, chi, f, xTitle = "", dropNoAnswer = FALSE, facet_scales = "fixed", facet_cols = 2, yMax = TRUE, fillCross = FALSE, yTitle = "Verlustrate [%]", yOffset = 2, text = "", allData = FALSE, raw = "", expandMax = 0.07) {
  myvar <- rlang::sym(f)

  # Facet ~ Labels
  # year (n=participants)
  labels <- fLabeller(x)

  # y-Scale
  if (yMax) {
    yMax <- 100
  } else {
    yMax <- max(x$upper, chi$y) + yOffset
  }

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
    ggplot(aes(x = {{ myvar }}, y = middle, color = year))

  # Add Sina Background Plot with individual Loss Rate
  if (allData) {
    p <- p +
      ggforce::geom_sina(
        data = raw,
        mapping = aes(x = {{ myvar }}, y = lost_rate_e),
        alpha = 0.5,
        color = "lightgray",
        scale = "count",
        show.legend = FALSE
      )
  }

  p <- p +
    geom_crossbar(
      aes(ymin = lower, ymax = upper, fill = I(filling))
    ) +
    geom_point(
      size = 3
    ) +
    # geom_hline(yintercept = 0, color = "black", size = 1) +
    scale_colour_manual(values = colorBlindBlack8[-1], aesthetics = "color", guide = FALSE) +
    ylab(yTitle) +
    xlab(xTitle) +
    labs(title = text) +
    geom_text(
      aes(
        y = 100,
        label = glue("n = {n}")
      ),
      vjust = -0.5,
      color = "black",
      size = 3
    ) +
    scale_y_continuous(
      limits = c(0, yMax),
      expand = expansion(mult = c(0, expandMax))
    ) +
    facet_wrap(
      ~year,
      ncol = facet_cols,
      labeller = labels,
      scales = facet_scales
    ) +
    ggplot2::theme(
      axis.ticks.x = element_blank()
    ) +
    ggplot2::coord_cartesian(clip = "off")

  if (nrow(chi) > 0) {
    p <- p +
      geom_signif(
        data = chi,
        aes(xmin = start, xmax = end, annotations = label, y_position = y + 10),
        size = 1,
        textsize = 9,
        manual = TRUE,
        vjust = 0.5
      )
  }
  return(p)
}


fPlot <- function(x, chi, f, xTitle = "", dropNoAnswer = FALSE, facet_scales = "fixed", facet_cols = 2, yMax = TRUE, fillCross = FALSE, yTitle = "Verlustrate [%]", yOffset = 2, text = "", allData = FALSE, raw = "", expandMax = 0.07) {
  myvar <- rlang::sym(f)

  # Facet ~ Labels
  # year (n=participants)
  labels <- fLabeller(x)

  # y-Scale
  if (yMax) {
    yMax <- 100
  } else {
    yMax <- max(x$upper, chi$y) + yOffset
  }

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
    ggplot(aes(x = {{ myvar }}, y = middle, color = year))

  # Add Sina Background Plot with individual Loss Rate
  if (allData) {
    p <- p +
      ggforce::geom_sina(
        data = raw,
        mapping = aes(x = {{ myvar }}, y = lost_rate_e),
        alpha = 0.5,
        color = "lightgray",
        scale = "count",
        show.legend = FALSE
      )
  }

  p <- p +
    ggplot2::geom_errorbar(
      aes(ymin = lower, ymax = upper),
      size = 0.8
    ) +
    ggplot2::geom_point(
      size = 2
    ) +
    # geom_hline(yintercept = 0, color = "black", size = 1) +
    scale_colour_manual(values = colorBlindBlack8[-1], aesthetics = "color", guide = FALSE) +
    ylab(yTitle) +
    xlab(xTitle) +
    labs(title = text) +
    geom_text(
      aes(
        y = 100,
        label = glue("n = {n}")
      ),
      vjust = -0.1,
      color = "black",
      size = 3
    ) +
    scale_y_continuous(
      limits = c(0, yMax),
      expand = expansion(mult = c(0, expandMax))
    ) +
    facet_wrap(
      ~year,
      ncol = facet_cols,
      labeller = labels,
      scales = facet_scales
    ) +
    ggplot2::theme(
      axis.ticks.x = element_blank()
    ) +
    ggplot2::coord_cartesian(clip = "off")

  if (nrow(chi) > 0) {
    p <- p +
      geom_signif(
        data = chi,
        aes(xmin = start, xmax = end, annotations = label, y_position = y + 10),
        size = 1,
        textsize = 9,
        manual = TRUE,
        vjust = 0.5
      )
  }
  return(p)
}