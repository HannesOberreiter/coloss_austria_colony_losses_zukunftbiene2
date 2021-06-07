res16_NewFrames <- list()
myFactor <- "op_new_frames"

dfData <- dfData %>%
  mutate(
    !!myFactor := tidyr::replace_na(!!sym(myFactor), "keine Angaben"),
    !!myFactor := as.factor(!!sym(myFactor)) %>%
      fct_relevel(c("0%", "1-30%", "31-50%", "mehr als 50%", "keine Angaben"))
  )
res16_NewFrames$result <- fGlmNullModel(dfData, myFactor) %>%
  mutate(
    !!myFactor := as.factor(!!sym(myFactor)) %>%
      fct_relevel(c("0%", "1-30%", "31-50%", "mehr als 50%", "keine Angaben"))
  )

res16_NewFrames$chi <- fChistar(res16_NewFrames$result, myFactor)

res16_NewFrames$p <- fPlot(
  res16_NewFrames$result,
  res16_NewFrames$chi,
  myFactor,
  xTitle = "Anteil der im Einwinterungsjahr erneuerten Brutwaben [%]",
  dropNoAnswer = TRUE,
  allData = TRUE,
  raw = dfData %>% filter(!!sym(myFactor) != "keine Angaben")
)

fSaveImages("16_NewFrames", res16_NewFrames$p, w = 8.5)

caption <- "Höhe der Winterverluste in Prozent ($\\pm$95\\%CI) und Anzahl der Antworten anhand dem relativen Anteil der im Einwinterungsjahr erneuerten Brutwaben über die Umfragejahre 2017/18 - 2020/21."

fSaveTable(
  "16_NewFrames",
  res16_NewFrames$result %>%
    mutate(
      !!myFactor := str_replace(!!sym(myFactor), "%", "\\\\%")
    ),
  caption,
  myFactor,
  "u:16newFrames"
)