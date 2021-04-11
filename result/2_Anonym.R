res2_Anon <- list()

myFactor <- "anon" 

dfData <- dfData %>% 
  mutate(
    anon = ifelse(is.na(contact), "Anonym", "Nicht-Anonym"),
    anon = as.factor(anon)
  )

res2_Anon$result <- fGlm(dfData, myFactor)
res2_Anon$chi    <- fChistar(res2_Anon$result, myFactor)
res2_Anon$p      <- fPlot(res2_Anon$result, res2_Anon$chi, myFactor)

fSaveImages("2_Anonym", res2_Anon$p)

caption <- "Comparison of estimated expenses and the expenses given by participants in the survey. All values are in Euro."

fSaveTable("2_Anonym", res2_Anon$result, caption, myFactor)
