res06_Radius <- list()
res06_Radius$data <- dfData %>%
  mutate(
    apiary_nearby = if_else(apiaries == 1, "nur einen Bienenstand", apiary_nearby),
    apiary_nearby = replace_na(apiary_nearby, "keine Angaben") %>%
      as.factor() %>%
      fct_relevel("keine Angaben", after = Inf)
  ) %>%
  add_count(year) %>%
  group_by(year, apiary_nearby) %>%
  summarise(
    nNearby = n(),
    npNearby = fPrettyNum(round((nNearby * 100 / n[[1]]), 1), 1)
  )

res06_Radius$tab <- res06_Radius$data[, -1] %>%
  kable(
    "latex",
    caption = "Absolute und relative Anzahl der Antworten zur Frage: Sind alle Ihre Bienenvölker innerhalb von etwa \\SI{15}{\\kilo\\meter} des genannten Hauptbienenstand?",
    label = "u:06:Radius",
    booktabs = T,
    escape = F,
    col.names = c("Alle Völker innerhalb \\SI{15}{\\kilo\\meter} Radius", "Imkereien [\\textit{n}]", "[\\%]"),
    align = c("l", rep("r", 2))
  ) %>%
  kable_styling(latex_options = "HOLD_position", font_size = 8)

begin <- 1
for (i in unique(res06_Radius$data$year)) {
  end <- begin + nrow(res06_Radius$data %>% filter(year == i)) - 1
  res06_Radius$tab <- res06_Radius$tab %>% pack_rows(i, begin, end)
  begin <- end + 1
}

# small bugfix as pack_rows introduces an linespaceing which breaks midrule
res06_Radius$tab <- sub("midrule{}", "midrule", res06_Radius$tab, fixed = T)

res06_Radius$tab %>% save_kable(paste0("output/tables/06_Radius.tex"))