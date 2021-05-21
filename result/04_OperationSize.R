res04_OperationSize <- list()

dfData <- dfData %>%
  mutate(
    operation_size = case_when(
      between(hives_winter, 1, 10) ~ "1-10",
      between(hives_winter, 11, 50) ~ "11-50",
      between(hives_winter, 51, 100) ~ "51-100",
      hives_winter > 100 ~ "> 100"
    ),
    operation_size = fct_relevel(operation_size, "> 100", after = Inf)
  )

res04_OperationSize$result <- dfData %>%
  add_count(year) %>%
  group_by(year) %>%
  mutate(
    sumHives = sum(hives_winter)
  ) %>%
  group_by(year, operation_size) %>%
  summarise(
    nBeek = n(),
    nBeek_print = format(nBeek, big.mark = ".", decimal.mark = ","),
    npBeek = fNumberFormat(nBeek * 100 / sum(n[[1]])),
    nHives = sum(hives_winter),
    npHives = nHives * 100 / sumHives[[1]],
    nHives_print = format(nHives, big.mark = ".", decimal.mark = ","),
    legend_plot = glue("{year[[1]]} (n = {n[[1]]})"),
    .groups = "drop"
  )

res04_OperationSize$skim <- dfData %>%
  select(year, hives_winter) %>%
  group_by(year) %>%
  skim()

res04_OperationSize$p1 <- res04_OperationSize$result %>%
  ggplot(
    aes(x = operation_size, y = npBeek, fill = legend_plot)
  ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = nBeek_print),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    hjust = -0.1,
    angle = 45,
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 60),
    breaks = seq(0, 100, 10),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme(
    axis.line.x = element_line(linetype = "solid", size = 0.5),
    legend.position = "top"
  ) +
  scale_colour_manual(
    values = colorBlindBlack8[-1],
    aesthetics = "fill",
    guide = guide_legend(
      title = "Jahr",
      label.position = "bottom",
      label.hjust = 0
    )
  ) +
  xlab("Völker / Imkerei") +
  ylab("Teilnehmende Imkereien [%]")

res04_OperationSize$p2 <- res04_OperationSize$result %>%
  ggplot(
    aes(x = operation_size, y = npHives, fill = year)
  ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = nHives_print),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    hjust = -0.1,
    angle = 45,
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 60),
    breaks = seq(0, 100, 10),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme(
    axis.line.x = element_line(linetype = "solid", size = 0.5),
  ) +
  scale_colour_manual(
    values = colorBlindBlack8[-1],
    aesthetics = "fill",
    guide = FALSE
  ) +
  xlab("Völker / Imkerei") +
  ylab("Bienenvölker [%]")

fSaveImages("04_OperationSize", res04_OperationSize$p1 / res04_OperationSize$p2 + patchwork::plot_annotation(tag_levels = "A"))
