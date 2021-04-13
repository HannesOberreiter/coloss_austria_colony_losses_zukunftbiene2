res4_OperationSize <- list()

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

res4_OperationSize$result <- dfData %>% 
  add_count(year) %>%
  group_by(year) %>% 
  mutate(
    sumHives = sum(hives_winter)
  ) %>% 
  group_by(year, operation_size) %>% 
  summarise(
    nBeek  = format(n(), big.mark = ".", decimal.mark = ","),
    npBeek = fNumberFormat(n() * 100/ sum(n[[1]])),
    nHives = sum(hives_winter),
    npHives = nHives * 100 / sumHives[[1]],
    nHives = format(nHives, big.mark = ".", decimal.mark = ","),
    .groups = "drop"
  )

res4_OperationSize$skim <- dfData %>%
  select(year, hives_winter) %>% 
  group_by(year) %>% 
  skim()

res4_OperationSize$p1 <- res4_OperationSize$result %>% 
  ggplot(
    aes(x = operation_size, y = npBeek, fill = year)
    ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = nBeek),
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
    guide = guide_legend("Jahr")
    ) + 
  xlab("Völker / Imkerei") + 
  ylab("Teilnehmende Imkereien [%]") 



res4_OperationSize$p2 <- res4_OperationSize$result %>% 
  ggplot(
    aes(x = operation_size, y = npHives, fill = year)
    ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = nHives),
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

fSaveImages("04_OperationSize", res4_OperationSize$p1 / res4_OperationSize$p2)


