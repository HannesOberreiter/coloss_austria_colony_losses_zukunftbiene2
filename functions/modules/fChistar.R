# Function which generates a dataframe to draw significant results onto plot
# data = our initial dataframe (see return from F_GLM_FACTOR)
# f = factor of Interest
# startPoint and endPoint the two groups to connect
# myGroup if multiple plots are arranged with
# facet_grid we need here to define column which are used for grouping the grid
# myLabel here we can define what we want to see 
# above the connected significant line
fChistar <- function(
  data = x,
  myFactor = f, 
  startPoint = "Ja", 
  endPoint = "Nein", 
  myGroup = FALSE, 
  myLabel = "*"
  ){
  myVar <- sym(myFactor)
  # generate chistar brackets temporary dataframe
  dataTemp <- data %>% filter(chistar == TRUE)
  # return empty if nothing is significant
  if(nrow(dataTemp) == 0){
    print("No signifikant results inside given DF")
    return(dataTemp)
  }
  # easy logic if two flavors are given
  if(length(unique(dataTemp[[myVar]])) == 2){
    startPoint <- unique(dataTemp[[myVar]][1])
    endPoint   <- unique(dataTemp[[myVar]][2])
  }
  yearRes <- list()
  for(i in unique(dataTemp$year)){
    yearDf <- dataTemp %>% filter(year == i)
    startUpper <- yearDf %>% 
      filter({{myVar}} == startPoint) %>% 
      pull(upper)
    endUpper <- yearDf %>% 
      filter({{myVar}} == endPoint) %>% 
      pull(upper)
    yearRes[[i]] <- 
      tibble(
        year   = i,
        start  = yearDf %>%  filter({{myVar}} == startPoint) %>% pull({{myVar}}), 
        end    = yearDf %>%  filter({{myVar}} == endPoint) %>% pull({{myVar}}), 
        y      = ifelse(startUpper > endUpper, startUpper, endUpper) + 2, 
        label  = myLabel
      )
  }
  return(bind_rows(yearRes))
}
