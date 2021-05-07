# Function which generates a dataframe to draw significant results onto plot
# data = our initial dataframe (see return from F_GLM_FACTOR)
# f = factor of Interest
# myLabel here we can define what we want to see
# above the connected significant line
fChistar <- function(df = x,
                     myFactor = f,
                     myLabel = "*") {
  myVar <- sym(myFactor)
  # return empty if nothing is significant
  if (length(str_c(df$chistar, "")) == 0) {
    print("No signifikant results inside given DF")
    return(df)
  }
  resChi <- list()
  for (i in unique(df$year)) {
    dfYear <- df %>% filter(year == i)
    resYear <- list()
    for (j in 1:nrow(dfYear)) {
      curRow <- dfYear[j, ]
      if (curRow$chistar == "") next()
      endVector <- curRow %>%
        pull(chistar) %>%
        str_split(",", simplify = T) %>%
        as.vector()
      resYear[[j]] <- tibble(
        year = i,
        start = curRow %>% pull({{ myVar }}),
        end = endVector,
        y = curRow[["upper"]] + seq(2, 10, 2)[1:length(endVector)],
        label = myLabel
      )
    }
    resChi[[i]] <- dplyr::bind_rows(resYear)
  }
  return(dplyr::bind_rows(resChi))
}