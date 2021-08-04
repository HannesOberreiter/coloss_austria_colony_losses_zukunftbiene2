res09_States <- list()
myFactor <- "state"
res09_States$names <- c(
  "Bgld." = "Burgenland",
  "Ktn."  = "Kärnten",
  "NÖ"    = "Niederösterreich",
  "OÖ"    = "Oberösterreich",
  "Sbg."  = "Salzburg",
  "Stmk." = "Steiermark",
  "T"     = "Tirol",
  "Vbg."  = "Vorarlberg",
  "W"     = "Wien"
)

dfData <- dfData %>%
  mutate(
    state = as.factor(state) %>% fct_relevel(res09_States$names)
  )

res09_States$result <- fGlmNullModel(dfData, myFactor)
res09_States$global <- fGlmNullModel(dfData, "global")
res09_States$labels <- fLabeller(res09_States$result)

# Table Count States ------------------------------------------------------------------

res09_States$resultCount <- dfData %>%
  group_by(year, state) %>%
  summarise(
    beekeeper = n(),
    hives_winter = sum(hives_winter),
    lost_queens = sum(lost_a),
    lost_element = sum(lost_b),
    lost_other = sum(lost_c),
    lost_all = sum(lost_queens, lost_other)
  ) %>%
  left_join(res09_States$result)

res09_States$globalCount <- dfData %>%
  group_by(year) %>%
  summarise(
    beekeeper = n(),
    hives_winter = sum(hives_winter),
    lost_queens = sum(lost_a),
    lost_element = sum(lost_b),
    lost_other = sum(lost_c),
    lost_all = sum(lost_queens, lost_other)
  ) %>%
  left_join(res09_States$global) %>%
  mutate(
    state = "Österreich"
  )

res09_States$all <- bind_rows(res09_States$resultCount, res09_States$globalCount) %>%
  mutate(
    year = as.factor(year),
    state = as.factor(state) %>% fct_relevel(c("AUT" = "Österreich", res09_States$names))
  ) %>%
  ungroup() %>%
  arrange(year, state)


tab <- res09_States$all %>%
  mutate(
    middle = fPrettyNum(middle),
    ci = paste0("(", fPrettyNum(lower), " - ", fPrettyNum(upper), ")"),
    hives_winter = fPrettyNum(hives_winter, 0),
    lost_queens = fPrettyNum(lost_queens, 0),
    lost_all = fPrettyNum(lost_all, 0),
    beekeeper = fPrettyNum(beekeeper, 0),
  ) %>%
  select(
    state, beekeeper, hives_winter, lost_queens, lost_other, lost_all, middle, ci
  ) %>%
  kable(
    "latex",
    caption = "",
    label = "u:09states",
    booktabs = T,
    escape = F,
    col.names = c(
      "",
      "Imkereien [\\textit{n}]",
      "Völker Eingewintert [\\textit{n}]",
      "Verluste (KöniginnenProbleme) [n]",
      "Tote \\\ Völker [n]",
      "Summe \\\ Verlust [n]",
      "Verlust [\\%]",
      "95\\% CI [\\%]"
    ),
    align = c("l", rep("r", 7))
  ) %>%
  kable_styling(latex_options = "HOLD_position", font_size = 8)

begin <- 1
for (i in unique(res09_States$all$year)) {
  end <- begin + nrow(res09_States$all %>% filter(year == i)) - 1
  tab <- tab %>% pack_rows(i, begin, end)
  begin <- end + 1
}

# small bugfix as pack_rows introduces an linespaceing which breaks midrule
tab <- sub("midrule{}", "midrule", tab, fixed = T)

tab %>% save_kable(paste0("output/tables/09_States.tex"))

rm(tab, begin, end, i)

# States Boxplot ----------------------------------------------------------
# We need the other way around to use te replace magic
tempNames <- names(res09_States$names)
names(tempNames) <- res09_States$names

res09_States$result <- res09_States$result %>%
  mutate(
    label = ifelse(
      chistar == "",
      "",
      paste0("*", chistar) %>%
        stringr::str_replace_all(tempNames) %>%
        # https://stackoverflow.com/questions/31761627/how-to-replace-nth-character-of-a-string-in-a-column-in-r
        stringr::str_replace("^((?:.*?,){2}.*?)((?:.*?,){2}.*?),", "\\1\n\\2\n")
    )
  )

res09_States$p <- res09_States$result %>%
  ggplot(aes(x = state, y = middle, color = year)) +
  # Austria
  geom_hline(
    data = res09_States$global,
    aes(yintercept = middle),
    size = 1,
    color = colorBlindBlack8[7]
  ) +
  geom_hline(
    data = res09_States$global,
    aes(yintercept = upper),
    color = colorBlindBlack8[7],
    linetype = "dashed"
  ) +
  geom_hline(
    data = res09_States$global,
    aes(yintercept = lower),
    color = colorBlindBlack8[7],
    linetype = "dashed"
  ) +
  # States
  geom_crossbar(
    aes(ymin = lower, ymax = upper),
    fill = "white",
    alpha = 0.8
  ) +
  geom_point(
    size = 3
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_colour_manual(values = colorBlindBlack8[-1], aesthetics = "color", guide = FALSE) +
  ylab("Verlustrate [%]") +
  xlab("") +
  geom_text(
    aes(
      y = 0.5,
      label = glue("n = {n}")
    ),
    vjust = 0,
    color = "black",
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, max(res09_States$result$upper) + 3),
    expand = expansion(mult = c(0, 0.1))
  ) +
  facet_wrap(
    ~year,
    scales = "free_x",
    ncol = 1,
    labeller = res09_States$labels
  ) +
  scale_x_discrete(labels = names(res09_States$names))

res09_States$p2 <- res09_States$p +
  coord_cartesian(clip = "off") +
  geom_text(
    aes(y = upper, label = label),
    size = 2.5,
    vjust = 0,
    nudge_y = 1
  )


fSaveImages("09_States", res09_States$p2, h = 10)
# States Map --------------------------------------------------------------

res09_States$result_map <- res09_States$result %>%
  left_join(mfStatesSimplify, by = c("state" = "BL"))

res09_States$pMap <- res09_States$result_map %>%
  ggplot() +
  geom_sf(
    data = mfStatesSimplify,
    aes(group = BL),
    color = "black",
    size = 0.6,
    fill = "white"
  ) +
  geom_sf(
    aes(group = state, fill = middle, geometry = geometry),
    color = "black",
    size = 0.6
  ) +
  coord_sf() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_viridis_c(
    option = "inferno",
    direction = -1,
    breaks = c(8, 10, 12, 14, 16, 18, 20)
  ) +
  labs(fill = "Verlustrate [%]") +
  facet_wrap(
    ~year,
    ncol = 2,
    labeller = res09_States$labels
  )

fSaveImages("09_StatesMap", res09_States$pMap)