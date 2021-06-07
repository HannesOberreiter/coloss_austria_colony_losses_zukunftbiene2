# Function which generates a dataframe to draw significant results onto plot
# data = our initial dataframe (see return from F_GLM_FACTOR)
# f = factor of Interest
# myLabel here we can define what we want to see
# above the connected significant line
fChistar <- function(df = x,
                     myFactor = f,
                     myLabel = "*",
                     dropNoAnswer = FALSE) {
  myVar <- sym(myFactor)
  # return empty if nothing is significant
  if (length(str_c(df$chistar, "")) == 0) {
    print("No signifikant results inside given DF")
    return(df)
  }
  resChi <- list()

  if (dropNoAnswer) {
    df <- df %>%
      filter(
        !(!!myVar %in% c("keine Angaben", "Unsicher", "keine\nAngaben"))
      ) %>%
      mutate(
        chi = chistar %>% str_remove("keine Angaben") %>% str_remove("Unsicher") %>% str_remove("keine\nAngaben")
      )
  }

  for (i in unique(df$year)) {
    dfYear <- df %>% filter(year == i)
    resYear <- list()
    yMax <- max(dfYear$upper)
    for (j in seq_len(nrow(dfYear))) {
      curRow <- dfYear[j, ]
      if (curRow$chistar == "") next()
      endVector <- curRow %>%
        pull(chistar) %>%
        str_split(",", simplify = T) %>%
        # safety as we could remove some rows if dropNoAnswer is True
        stringi:::stri_remove_empty() %>%
        as.vector()
      if (length(endVector) == 1) {
        yMax <- last(yMax) + 4
      } else {
        yMax <- last(yMax) + seq(8, length(endVector) * 8, 8)
      }
      resYear[[j]] <- tibble(
        year = i,
        start = curRow %>% pull({{ myVar }}),
        end = endVector,
        y = yMax,
        label = myLabel
      )
    }
    resChi[[i]] <- dplyr::bind_rows(resYear)
  }

  return(dplyr::bind_rows(resChi))
}