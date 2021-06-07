res02_Anon <- list()
myFactor <- "anon"

dfData <- dfData %>%
  mutate(
    anon = ifelse(is.na(contact), "Anonym", "Nicht-Anonym"),
    anon = as.factor(anon)
  )

res02_Anon$result <- fGlmNullModel(dfData, myFactor)
res02_Anon$chi <- fChistar(res02_Anon$result, myFactor)

res02_Anon$p <- fPlot(
  res02_Anon$result,
  res02_Anon$chi,
  myFactor,
  allData = TRUE,
  raw = dfData %>% drop_na({{ myFactor }})
)

fSaveImages("02_Anonym", res02_Anon$p)

caption <- "Höhe der Winterverluste in Prozent ($\\pm$95\\%CI) und Anzahl der Antworten anonymer TeilnehmerInnen und nicht-anonymer TeilnehmerInnen über die Umfragejahre 2017/18 - 2020/21"

fSaveTable("02_Anonym", res02_Anon$result, caption, myFactor, "u:02anonym")