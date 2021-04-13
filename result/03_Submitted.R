res03_Submitted <- list()

myFactor <- "submitted" 

dfData <- dfData %>% 
  mutate(
    submitted = as.factor(submitted)
  )

res03_Submitted$result <- fGlm(dfData, myFactor)
res03_Submitted$chi    <- fChistar(res03_Submitted$result, myFactor, "Internet", "Zeitung")
res03_Submitted$p      <- fPlot(res03_Submitted$result, res03_Submitted$chi, myFactor)
fSaveImages("03_Submitted", res03_Submitted$p)

caption <- "Comparison of estimated expenses and the expenses given by participants in the survey. All values are in Euro."

fSaveTable("03_Submitted", res03_Submitted$result, caption, myFactor, "u:03submitted")
