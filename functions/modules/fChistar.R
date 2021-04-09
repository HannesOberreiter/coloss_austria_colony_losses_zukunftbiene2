# Function which generates a dataframe to draw significant results onto plot
# D.INPUT = our initial dataframe (see return from F_GLM_FACTOR)
# f = factor
# V.START and V.END the two groups to connect
# V.GROUP if multiple plots are arranged with facet_grid we need here to define column which are used for grouping the grid
# V.LABEL here we can define what we want to see above the connected significant line
fChistar <- function(D.INPUT, f, V.START = "Ja", V.END = "Nein", V.GROUP = FALSE, V.LABEL = "*"){
  myvar <- sym(f)
  # generate chistar brackets temporary dataframe
  D.TEMP <- D.INPUT %>% filter(chistar == TRUE)
  # return empty if nothing is significant
  if(nrow(D.TEMP) == 0){
    print("No signifikant results inside given DF")
    return(D.TEMP)
  }
  # easy logic if two flavors are given
  if(length(unique(D.TEMP[[myvar]])) == 2){
    V.START <- unique(D.TEMP[[myvar]][1])
    V.END   <- unique(D.TEMP[[myvar]][2])
  }
  yearRes <- list()
  for(i in unique(D.TEMP$year)){
    yearDf <- D.TEMP %>% filter(year == i)
    startUpper <- yearDf %>% 
      filter({{myvar}} == V.START) %>% 
      pull(upper)
    endUpper <- yearDf %>% 
      filter({{myvar}} == V.END) %>% 
      pull(upper)
    yearRes[[i]] <- 
      tibble(
        year   = i,
        start  = yearDf %>%  filter({{myvar}} == V.START) %>% pull({{myvar}}), 
        end    = yearDf %>%  filter({{myvar}} == V.END) %>% pull({{myvar}}), 
        y      = ifelse(startUpper > endUpper, startUpper, endUpper) + 2, 
        label  = V.LABEL
      )
  }
  return(bind_rows(yearRes))
}
