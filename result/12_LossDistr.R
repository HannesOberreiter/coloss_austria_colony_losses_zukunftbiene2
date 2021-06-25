# Work in Progress
res12_LossDistr <- list()

dfData <- dfData %>%
  mutate(
    loss_group = cut(
      lost_rate_e,
      c(0, 0.01, seq(10, 100, 10)),
      label = c("0%", ">0-10%", "11-20%", "21-30%", "31-40%", "41-50%", "51-60%", "61-70%", "71-80%", "81-90%", "91-100%"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

res12_LossDistr$result <- dfData %>%
  add_count(year) %>%
  group_by(year) %>%
  mutate(
    sumHives = sum(hives_winter),
    sumHivesLost = sum(hives_lost_e)
  ) %>%
  group_by(year, loss_group) %>%
  summarise(
    nBeek = format(n(), big.mark = ".", decimal.mark = ","),
    nBeek_continous = n(),
    npBeek = fNumberFormat(n() * 100 / sum(n[[1]])),
    nHives_continous = sum(hives_winter),
    npHives = round(nHives_continous * 100 / sumHives[[1]], 2),
    nHives = format(nHives_continous, big.mark = ".", decimal.mark = ","),
    nHivesLost_continous = sum(hives_lost_e),
    npHivesLost = fNumberFormat(round(nHivesLost_continous * 100 / sumHivesLost[[1]], 2)),
    nHivesLost = format(nHivesLost_continous, big.mark = ".", decimal.mark = ","),
    legend_plot = glue("{year[[1]]} (n = {n[[1]]})"),
    .groups = "drop"
  )

res12_LossDistr$labels <- fLabeller(res12_LossDistr$result, nBeek_continous)

res12_LossDistr$p <- res12_LossDistr$result %>%
  ggplot(
    aes(x = loss_group, y = nBeek_continous, fill = legend_plot)
  ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = paste0(npBeek, "%")),
    vjust = -0.8
  ) +
  scale_y_continuous(
    breaks = seq(0, 1000, 100),
    expand = expansion(mult = c(0, 0.2))
  ) +
  theme(
    axis.line.x = element_line(linetype = "solid", size = 0.5),
    legend.position = "top"
  ) +
  scale_colour_manual(
    values = colorBlindBlack8[-1],
    aesthetics = "fill",
    guide = FALSE
  ) +
  facet_wrap(
    ~year,
    scales = "free_x",
    ncol = 1,
    labeller = res12_LossDistr$labels
  ) +
  xlab("Verlust/Imkerei [%]") +
  ylab("Betroffene Imkereien [n]")

fSaveImages("12_LossDistr", res12_LossDistr$p, h = 10)

# Lost Hives Distribution Plot

res12_LossDistr$labels_hives <- fLabeller(res12_LossDistr$result, nHivesLost_continous, ntext = "Summe verlorene Bienenvölker")

res12_LossDistr$p_hives <- res12_LossDistr$result %>%
  ggplot(
    aes(x = loss_group, y = nHivesLost_continous, fill = legend_plot)
  ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = paste0(npHivesLost, "%")),
    vjust = -0.8
  ) +
  scale_y_continuous(
    breaks = seq(0, 10000, 200),
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme(
    axis.line.x = element_line(linetype = "solid", size = 0.5),
    legend.position = "top"
  ) +
  scale_colour_manual(
    values = colorBlindBlack8[-1],
    aesthetics = "fill",
    guide = FALSE
  ) +
  facet_wrap(
    ~year,
    scales = "free_x",
    ncol = 1,
    labeller = res12_LossDistr$labels_hives
  ) +
  xlab("Verlust/Imkerei [%]") +
  ylab("Verlorene Bienenvölker [n]")

fSaveImages("12_LossDistrHives", res12_LossDistr$p_hives, h = 10)