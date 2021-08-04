fPrettyNum <- function(x, afterComma = 1) {
  format(x, nsmall = afterComma, dec = ",", big.mark = ".", trim = T)
}