res03_Submitted <- list()
myFactor <- "submitted"

dfData <- dfData %>%
  mutate(
    submitted = as.factor(submitted)
  )

res03_Submitted$result <- fGlmNullModel(dfData, myFactor)
res03_Submitted$chi <- fChistar(res03_Submitted$result, myFactor)
res03_Submitted$p <- fPlot(res03_Submitted$result, res03_Submitted$chi, myFactor, allData = TRUE, raw = dfData)
fSaveImages("03_Submitted", res03_Submitted$p)

caption <- "Höhe der Winterverluste in Prozent ($\\pm$95\\%CI) und Anzahl der Antworten der unterschiedlichen Teilnehmearten (Online, Papier, Zeitung) über die Umfragejahre 2017/18 - 2020/21"


fSaveTable("03_Submitted", res03_Submitted$result, caption, myFactor, "u:03submitted")