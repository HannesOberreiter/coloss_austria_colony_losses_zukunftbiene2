fPrettyNum <- function(x){
  prettyNum(
    sprintf("%.1f", x), 
    big.mark = ".", decimal.mark = ",", drop0trailing = F, 
    format = "d"
  )
}
