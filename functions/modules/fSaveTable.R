fSaveTable <- function(filename = filename, data = x, caption = caption, myFactor = f){
  myvar <- sym(myFactor)
  tab <- data %>% 
    mutate(
      np = fPrettyNum(np),
      ci = paste0("(", fPrettyNum(lower), " - ", fPrettyNum(upper), ")")
    ) %>% 
    select(
      {{myvar}}, n, np, middle, ci
    ) %>% 
    kable(
      "latex",
      caption = caption,
      booktabs = T,
      escape = F,
      col.names = c("", "Imkereien [\\textit{n}]", "[\\%]", "Verlustrate [\\%]", "95\\% CI [\\%]"),
      align = c("l", rep("r", 4))
    ) %>% 
    kable_styling(latex_options = "HOLD_position")

  begin <- 1
  for (i in unique(x$year)) {
    end <- begin + nrow(x %>% filter(year == i)) - 1
    tab <- tab %>% pack_rows(i, begin, end)
    begin <- end + 1
  }
  
  tab %>% save_kable(paste0("output/tables/", filename, ".tex"))
}




