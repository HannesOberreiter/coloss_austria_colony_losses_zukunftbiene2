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

# Other Stuff ------


x <- read.delim(pipe("pbpaste"), sep = " ")

y <- x %>%
  mutate(
    across(everything(), str_remove_all, pattern = "\\([^)]*"),
    across(everything(), str_remove_all, pattern = "\\)"),
    across(everything(), str_remove_all, pattern = "\\."),
  ) %>%
  add_row(Jahr = "2020", Imker = "31923", Bienenvölker = "426121") %>%
  mutate(
    across(everything(), as.integer)
  )

x %>%
  mutate(
    across(everything(), str_remove_all, pattern = "\\([^)]*"),
    across(everything(), str_remove_all, pattern = "\\)"),
    across(everything(), str_remove_all, pattern = "\\."),
  ) %>%
  add_row(Jahr = "2020", Imker = "31923", Bienenvölker = "426121") %>%
  mutate(
    across(everything(), as.integer)
  ) %>%
  arrange(Jahr) %>%
  y() %>%
  clipr::write_clip()


y <- y %>%
  mutate(Jahr = Jahr %>% as.character() %>% lubridate::as_date(Jahr, format = "%Y")) %>%
  glimpse()


p1 <- y %>% ggplot(aes(Jahr, Imker)) +
  ylab("ImkerInnen") +
  geom_step() +
  geom_area() +
  geom_point() +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("2 years"),
    labels = scales::label_date("'%y"),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::breaks_pretty(),
    labels = scales::label_number_auto()
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  xlab("Jahr")



p2 <- y %>% ggplot(aes(Jahr, Bienenvölker)) +
  geom_step() +
  geom_point() +
  ggplot2::scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::breaks_pretty(),
    labels = scales::label_number_auto()
  ) +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("2 years"),
    labels = scales::label_date("'%y"),
    expand = c(0, 0)
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  xlab("Jahr")
p2
patchwork <- (p1 + p2) + plot_annotation(
  title = "ImkerInnen und Bienenvölker in Österreich seit 1990 bis 2020",
  caption = "Daten von Biene Österreich",
)

fSaveImages("test", patchwork, w = 15)


iris %>%
  ggplot(aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(color = "black") +
  geom_line()



iris %>%
  mutate(
    NewCol = .data$Species,
    NewCol = .$NewCol
  )

iris %>%
  mutate(
    NewCol = .data$Species,
    NewCol = NULL
  )

xy <- expr(x + y)
xz <- expr(x + z)
yz <- expr(y + z)
abc <- exprs(a, b, c)

expr(((!!xy)) + !!yz - !!xy) # (3)
expr((!!xy) + !!yz - !!xy) # (3)


dfData %>%
  filter(year == "20/21") %>%
  summarise(
    hives_winter = sum(hives_winter),
    hives_lost = sum(hives_lost),
    hives_lost_e = sum(hives_lost_e),
    lost_a = sum(lost_a),
    lost_b = sum(lost_b),
    lost_c = sum(lost_c),
    lost_sum = sum(lost_a, lost_c),
    loss_rate = (lost_a + lost_c) / hives_winter * 100,
    lost_control = hives_lost_e / hives_winter * 100
  )
