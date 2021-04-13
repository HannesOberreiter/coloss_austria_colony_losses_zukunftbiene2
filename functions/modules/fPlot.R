fPlot <- function(x, chi, f){
  myvar <- sym(f)
  
  # Facet ~ Labels 
  # year (n=participants)
  labels <- x %>% 
    group_by(year) %>% 
    summarise(
      n = sum(n),
      n = paste0(year[[1]], " (n=", n, ")")
    ) %>% 
    pull(n)
  names(labels) <- unique(x$year)
  labels <- as_labeller(labels)
  
  p <- x %>%
    ggplot(aes(x = {{myvar}}, y = middle, color = year)) +
    geom_crossbar(
      aes( ymin = lower, ymax = upper ), 
      fill = "white"
    ) +
    geom_point(
      size = 3
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    scale_colour_manual(values = colorBlindBlack8[-1], aesthetics = "color", guide = FALSE) + 
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
    facet_wrap(
      ~ year, 
      ncol = 2,
      labeller = labels
      )
  
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
