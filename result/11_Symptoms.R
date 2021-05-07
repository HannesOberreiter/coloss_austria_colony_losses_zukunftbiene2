res11_Symptoms <- list()

res11_Symptoms$result <- dfData %>%
  filter(
    # Only care for symptoms if colonies are lost
    hives_lost_e != 0 &
      # Only care if symptoms were reported
      symp_total > 0
  ) %>%
  group_by(year) %>%
  summarise(
    symptomes = c("a)", "b)", "c)", "d)", "e)"),
    symptomes_values = c(sum(symp_a), sum(symp_b), sum(symp_c), sum(symp_d), sum(symp_e)),
    lost_hives = sum(hives_lost_e),
    n = n(),
    symptomes_sum = sum(symptomes_values),
    symptomes_values_print = format(symptomes_values, big.mark = ".", decimal.mark = ","),
    symptomes_percent = fNumberFormat(symptomes_values / lost_hives * 100),
    legend_plot = glue("{year[[1]]} (n = {n}),\nSumme = {symptomes_sum[[1]]}"),
    .groups = "drop"
  )

res11_Symptoms$p <- res11_Symptoms$result %>%
  ggplot(
    aes(x = symptomes, y = symptomes_percent, fill = legend_plot)
  ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = symptomes_values_print),
    position = position_dodge(width = 0.9),
    vjust = -0.1,
    hjust = 0.1,
    angle = 45,
    size = 3
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 45),
    breaks = seq(0, 100, 10),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme(
    axis.line.x = element_line(linetype = "solid", size = 0.5),
    legend.position = "top"
    # legend.spacing.y = unit(5.0, 'cm')
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
  xlab("Symptome") +
  ylab("Häufigkeit Symptome [%]")

fSaveImages("11_Symptoms", res11_Symptoms$p, h = 3.5)
