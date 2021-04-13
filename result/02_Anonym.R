res02_Anon <- list()

myFactor <- "anon" 

dfData <- dfData %>% 
  mutate(
    anon = ifelse(is.na(contact), "Anonym", "Nicht-Anonym"),
    anon = as.factor(anon)
  )

res02_Anon$result <- fGlm(dfData, myFactor)
res02_Anon$chi    <- fChistar(res02_Anon$result, myFactor)
res02_Anon$p      <- fPlot(res02_Anon$result, res02_Anon$chi, myFactor)

fSaveImages("02_Anonym", res02_Anon$p)

caption <- "Comparison of estimated expenses and the expenses given by participants in the survey. All values are in Euro."

fSaveTable("02_Anonym", res02_Anon$result, caption, myFactor, "u:02anonym")
