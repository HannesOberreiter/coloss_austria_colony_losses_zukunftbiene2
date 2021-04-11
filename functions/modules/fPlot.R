fPlot <- function(x, chi, f){
  myvar <- sym(f)
  p <- x %>%
    ggplot(aes(x = {{myvar}}, y = middle)) +
    geom_crossbar(
      aes( ymin = lower, ymax = upper ), 
      fill = "white"
    ) +
    geom_point(
      size = 3
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    ylab("Verlustrate [%]") +
    xlab("") +
    geom_text(
      aes(
        y = 0.5,
        label = glue("n = {np}%")
      ),
      vjust = 0,
      color = "black",
      size = 3
    ) +
    scale_y_continuous(
      limits = c(0, max(x$upper)+5),
      expand = expansion(mult = c(0, 0.1))
    ) +
    facet_wrap(~ year, ncol = 2)
  if (nrow(chi) > 0) {
    p <- p + 
      geom_signif(
        data=chi, 
        aes(xmin=start, xmax=end, annotations=label, y_position=y), 
        textsize = 8, manual=TRUE, vjust = 0.5
        )
  }
  return(p)
}
