res2_Anon <- list()

myFactor <- "anon" 

RAW <- RAW %>% 
  mutate(
    anon = ifelse(is.na(contact), "Anonym", "Nicht-Anonym"),
    anon = as.factor(anon)
  )

res2_Anon$result <- fResult(RAW, myFactor)
res2_Anon$chi <- fChistar(res2_Anon$result, myFactor)
res2_Anon$p <- fPlot(res2_Anon$result, res2_Anon$chi, myFactor)

fSaveImages("2_Anonym", p)

c = "Comparison of estimated expenses and the expenses given by participants in the survey. All values are in Euro."

fSaveTable(res2_Anon$result, c, myFactor, "2_Anonym")
