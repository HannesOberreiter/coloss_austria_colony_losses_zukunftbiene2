# Label generate helper for facet wrap of years
fLabeller <- function(df, x) {
  res <- df %>%
    group_by(year) %>%
    summarise(
      n = sum(n),
      n = format(n, big.mark = ".", decimal.mark = ","),
      n = paste0("20", year[[1]], " (n = ", n, ")")
    ) %>%
    pull(n)
  names(res) <- unique(df$year)
  return(ggplot2::as_labeller(res))
}
