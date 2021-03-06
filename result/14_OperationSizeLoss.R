res14_OperationSizeLoss <- list()
myFactor <- "operation_size_tri"

dfData <- dfData %>%
  mutate(
    operation_size_tri = case_when(
      between(hives_winter, 1, 20) ~ "1-20",
      between(hives_winter, 21, 50) ~ "21-50",
      hives_winter > 50 ~ "> 50"
    ),
    operation_size_tri = fct_relevel(operation_size_tri, "> 50", after = Inf)
  )

res14_OperationSizeLoss$result <- fGlmNullModel(dfData, myFactor) %>%
  mutate(
    operation_size_tri = fct_relevel(operation_size_tri, "> 50", after = Inf)
  )

res14_OperationSizeLoss$chi <- fChistar(res14_OperationSizeLoss$result, myFactor)
res14_OperationSizeLoss$chi$y[[6]] <- 40
res14_OperationSizeLoss$p <- fPlot(
  res14_OperationSizeLoss$result,
  res14_OperationSizeLoss$chi,
  myFactor,
  xTitle = "Völker / Imkerei",
  allData = TRUE,
  raw = dfData
)

fSaveImages("14_OperationSizeLoss", res14_OperationSizeLoss$p, w = 8.5)

caption <- "Höhe der Winterverluste in Prozent (und 95\\%~CI) und Anzahl der Antworten anhand der Betriebsgröße (eingewinerte Bienenvölker) über die Umfragejahre 2017/18 - 2020/21."

fSaveTable("14_OperationSizeLoss", res14_OperationSizeLoss$result, caption, myFactor, "u:14operationSizeLoss")