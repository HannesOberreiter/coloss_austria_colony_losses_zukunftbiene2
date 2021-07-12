# Numer of Participants longitudal ----------------------------------------
dfData %>%
  drop_na(contact) %>%
  group_by(year, contact) %>%
  summarise(
    contact = contact[[1]]
  ) %>%
  ungroup() %>%
  count(contact, name = "times") %>%
  group_by(times) %>%
  summarise(
    n = n()
  )

x <- dfData %>%
  drop_na(contact) %>%
  group_by(year, contact) %>%
  summarise(
    hives_winter = hives_winter[[1]],
    hives_lost_e = hives_lost_e[[1]],
    hives_spring_e = hives_spring_e[[1]],
    contact = contact[[1]],
    global = "1"
  ) %>%
  ungroup() %>%
  add_count(contact, sort = T) %>%
  filter(n == 4) %>%
  fGlmNullModel("global")
x
dfData %>%
  fGlmNullModel("global")


# Operation Size & Operational Faktors
dfData %>%
  filter(year == "19/20") %>%
  mutate(
    # size = ifelse(hives_winter < 50, "1-50", "50++"),
    lostrate = ifelse(lost_rate_e <= 10, "0-10%", "> 11%") %>% fct_relevel(c("0-10%", "> 11%"))
  ) %>%
  select(lostrate, starts_with("op_")) %>%
  add_count(lostrate) %>%
  pivot_longer(starts_with("op_"), names_to = "operational", values_to = "answer") %>%
  filter(operational != "op_new_frames" & answer == "Ja") %>%
  group_by(lostrate, operational) %>%
  summarise(
    c = n(),
    np = c * 100 / n[[1]]
  ) %>%
  ungroup() %>%
  group_by(operational) %>%
  mutate(
    d = round(base::diff(np), 1),
    d = paste0("Difference ", d, "%")
  ) %>%
  ggplot(aes(lostrate, np)) +
  geom_col() +
  geom_text(
    aes(label = c),
    nudge_y = 4
  ) +
  scale_y_continuous(limits = c(0, 75)) +
  geom_text(
    aes(x = 1.5, label = d, y = 70),
    check_overlap = T
  ) +
  facet_wrap(~operational)


dfData %>%
  filter(year == "19/20") %>%
  mutate(
    size = ifelse(hives_winter < 50, "1-50", "50++"),
    lostrate = ifelse(lost_rate_e <= 10, "0-10%", "> 11%") %>% fct_relevel(c("0-10%", "> 11%"))
  ) %>%
  add_count(size) %>%
  group_by(size, lostrate) %>%
  summarise(
    number = n(),
    percent = number * 100 / n[[1]]
  )



# Operation Size & Operational Faktors
dfData %>%
  filter(year == "19/20") %>%
  mutate(
    size = ifelse(hives_winter < 50, "1-50", "50++"),
    lostrate = ifelse(lost_rate_e <= 10, "0-10%", "> 11%") %>% fct_relevel(c("0-10%", "> 11%"))
  ) %>%
  select(size, lostrate, starts_with("op_")) %>%
  add_count(size, lostrate) %>%
  pivot_longer(starts_with("op_"), names_to = "operational", values_to = "answer") %>%
  filter(operational != "op_new_frames" & answer == "Ja") %>%
  group_by(size, lostrate, operational) %>%
  summarise(
    c = n(),
    np = c * 100 / n[[1]]
  ) %>%
  ungroup() %>%
  ggplot(aes(lostrate, np, fill = size, color = size)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = c, y = np + 5),
    position = position_dodge(width = 1),
  ) +
  scale_y_continuous(limits = c(0, 75)) +
  facet_wrap(~operational)



# Hives Apiaries ------

d <- dfData %>%
  mutate(
    ratio = hives_winter / apiaries
  ) %>%
  filter(ratio <= 1 & hives_winter > 2) %>%
  select(id, apiaries, hives_winter)

write.csv2(d, "ratio.csv")
d

summary(dfData$apiaries)
fit <- lm(hives_winter ~ apiaries, dfData)
s <- summary(fit)

dfData %>%
  ggplot(aes(x = apiaries, y = hives_winter, shape = year, size = hives_winter, color = year)) +
  geom_point() +
  stat_smooth(method = "lm", col = colorBlindBlack8[[2]], aes(group = 1)) +
  labs(
    title = paste(
      "Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
      "Intercept =", signif(fit$coef[[1]], 5),
      "Slope =", signif(fit$coef[[2]], 5),
      sep = " "
    )
  ) +
  theme_classic()


# Melezitose Weak Overwintered ------

dfData %>%
  drop_na(weak) %>%
  group_by(year, flow_melezitose) %>%
  summarise(
    p = sum(weak) * 100 / sum(hives_spring_e)
  )


# Groups like Economic Paper ----
dfData <- dfData %>%
  mutate(
    operation_size = case_when(
      between(hives_winter, 1, 10) ~ "1-10",
      between(hives_winter, 11, 20) ~ "11-20",
      between(hives_winter, 21, 50) ~ "21-50",
      between(hives_winter, 51, 100) ~ "51-100",
      between(hives_winter, 101, 150) ~ "101-150",
      hives_winter > 150 ~ "> 150"
    ),
    operation_size = fct_relevel(operation_size, "> 150", after = Inf)
  )

unique(dfData$year)

dfData %>%
  filter(year == "17/18") %>%
  group_by(operation_size) %>%
  summarise(
    n = n(),
    loss_rate = mean(hives_winter - hives_spring_e)
  )

# Fix Thymol Sign Bars ----
treatment <- "T_thymol_"
tComb <- "T_thymol_comb"
tPlotFilter <- c("Nur\nSommer", "Kein\nThymol", "Sommer\nWinter")
tSize <- 6
res20_Treatments$chi_comb$T_thymol_$y[[3]] <- 70

res20_Treatments$p_comb[[treatment]] <- fPlot(
  res20_Treatments$result_comb[[treatment]],
  res20_Treatments$chi_comb[[treatment]],
  tComb,
  xTitle = "",
  facet_scales = tFree,
  facet_cols = tCols,
  fillCross = TRUE,
  expandMax = tExpand,
  allData = TRUE,
  raw = res20_Treatments$combination[[treatment]] %>%
    filter(
      !!sym(tComb) %in% tPlotFilter
    )
)

fSaveImages(glue::glue("20_Comb{treatment}"), res20_Treatments$p_comb[[treatment]], h = tSize)


# Fix Ox Mix Sign Bars ----
treatment <- "T_oxalic_trickle_mix_"
tComb <- "T_oxalic_trickle_mix_comb"
tPlotFilter <- c("Nur\nSommer", "Nur\nWinter", "Sommer\nWinter", "Keine\nOx-Mix")
tSize <- 10
tCols <- 1
tExpand <- 0.1
tFree <- "free_x"
res20_Treatments$chi_comb$T_oxalic_trickle_mix_$y[[3]] <- 60

res20_Treatments$p_comb[[treatment]] <- fPlot(
  res20_Treatments$result_comb[[treatment]],
  res20_Treatments$chi_comb[[treatment]],
  tComb,
  xTitle = "",
  facet_scales = tFree,
  facet_cols = tCols,
  fillCross = TRUE,
  expandMax = tExpand,
  allData = TRUE,
  raw = res20_Treatments$combination[[treatment]] %>%
    filter(
      !!sym(tComb) %in% tPlotFilter
    )
)

fSaveImages(glue::glue("20_Comb{treatment}"), res20_Treatments$p_comb[[treatment]], h = tSize)


dfData %>%
  count(year) %>%
  pull(n) %>%
  mean()

# GLM Multivariable ----
y <- glm(as.formula(glue(
  "loss_rate_e~log(young_queens+1)*op_size*op_migratory_beekeeper"
)),
family = quasibinomial(link = "logit"),
data = tempData, na.action = na.omit,
weight = hives_lost_e + hives_spring_e
)
# specify predictor and target variables
test <- tibble(
  y = "loss_rate_e",
  x = c("log(young_queens+1)", "op_size", "op_migratory_beekeeper", "log(young_queens+1)*op_size", "log(young_queens+1)*op_migratory_beekeeper", "op_migratory_beekeeper+op_size", "op_migratory_beekeeper+op_migratory_beekeeper", "op_size + op_migratory_beekeeper", "op_size * op_migratory_beekeeper", "log(young_queens+1) + op_size + op_migratory_beekeeper", "log(young_queens+1) * op_size * op_migratory_beekeeper")
) %>%
  mutate(x = paste0(x, " + year"))
dt_model_info <- test %>% # create combinations
  mutate(
    model_id = row_number(), # create model id
    frml = paste0(y, "~", x)
  ) %>% # create model formula
  group_by(model_id, y, x) %>% # group by the above
  nest() %>% # nest data
  mutate(
    m = map(data, ~ glm(.$frml,
      family = quasibinomial(link = "logit"),
      data = tempData, na.action = na.omit, weight = hives_lost_e + hives_spring_e
    )), # create models
    m_table = map(m, ~ broom::tidy(.)), # tidy model output
    performance = map(m, ~ performance::model_performance(.)),
  )

# access model info
dt_model_info %>% unnest(performance)