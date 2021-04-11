res3_Submitted <- list()

myFactor <- "submitted" 

dfData <- dfData %>% 
  mutate(
    submitted = as.factor(submitted)
  )

res3_Submitted$result <- fGlm(dfData, myFactor)
res3_Submitted$chi    <- fChistar(res3_Submitted$result, myFactor, "Internet", "Zeitung")
res3_Submitted$p      <- fPlot(res3_Submitted$result, res3_Submitted$chi, myFactor)

fSaveImages("3_Submitted", res3_Submitted$p)

caption <- "Comparison of estimated expenses and the expenses given by participants in the survey. All values are in Euro."

fSaveTable("3_Submitted", res3_Submitted$result, caption, myFactor)
