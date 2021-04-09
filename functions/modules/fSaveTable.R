fSaveTable <- function(x, caption, f, filename){
  myvar <- sym(f)
  tab <- x %>% 
    mutate(
      np = fPrettyNum(np),
      ci = paste0("(", fPrettyNum(lower), " - ", fPrettyNum(upper), ")")
    ) %>% 
    select(
      {{myvar}}, n, np, middle, ci
    ) %>% 
    kable(
      "latex",
      caption = c,
      booktabs = T,
      escape = F,
      col.names = c("", "[\\textit{n}]", "[\\%]", "Verlustrate [\\%]", "95\\% CI [\\%]"),
      align = c("l", rep("r", 4))
    ) %>% 
    pack_rows("17/18", 1, 2) %>% 
    pack_rows("18/19", 3, 4) %>% 
    pack_rows("19/20", 5, 6)
  
  tab %>% save_kable(paste0("output/", filename, ".tex"))
}
