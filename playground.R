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



# Groups like Economic Paper

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


# Fix Thymol Sign Bars
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


# Fix Ox Mix Sign Bars
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

# Varroa Checked Type

# naja, ich habe des ganze bei COLOSS sehr vorangetrieben, weil ich glaube, dass da echt noch was drinsteckt. Vor allem die Verlustraten dieser Gruppen. Bis jetzt haben wir einfach immer nur die Verlustrate "Hat Varroamonitoring gemacht" oder nicht drin gehabt, aber du siehst ja die Bandbreite der Methoden. Da gibt es unterschiedlich valide Methoden. CO2 habe ich auch nicht am Schirm gehabt, is aber in anderen Ländern verbreitet. Vielleicht auch so ein Hinweis auf Professionalität, wenn man mit validen Methoden arbeitet!

# Zu deiner Frage: Ja, ich glaube ich hätt es gern drin. Nicht in der Tabelle, aber als kurzen Abschnitt im Textkörper, weils ja nur ein Jahr umfasst

# Des sind übrigens die englischsprachigen Termini:
# Alcohol wash
# Sticky board (or other collection tray below hive)
# Sugar shake / roll
# Visual inspection of adult bees
# Visual inspection of drone brood
# Sent sample to lab
# Other

# Selbst wenn nix rauskommt bei den Verlustraten, könn ma noch immer diskutieren, dass in Ö Hauptsächlich Stockwindeldiagnose gemacht wird, wobei #es vielleicht bessere Methoden gibt, die noch weiter ausgebaut werden sollten

## Im Bericht werde ich mal rein deskriptiv bleiben ohne in Kontext mit Verlustrate.

# Robert Brodschneider  15:34 Uhr
# Dann fragt sich jeder, warum hams des net angeschaut?

# Hannes  15:34 Uhr
# tzzzz
# 15:37 Uhr
# Soll ich dann Kombinationen machen oder jedes für sich und im Text Anmerken, dass in der Gruppe “Drone” auch Personen von “Floor” drinnen sein könnten?


p <- dfData %>%
  filter(year == "20/21") %>%
  select(starts_with("checked_")) %>%
  select(1:6) %>%
  pivot_longer(everything()) %>%
  mutate(
    name = str_remove(name, "checked_")
  ) %>%
  filter(value != "N/A") %>%
  count(name, value) %>%
  ggplot(aes(x = name, y = n, fill = value)) +
  geom_col()

fSaveImages("varroa_checked_play", p)


dfData %>%
  count(checked_other) %>%
  drop_na()


tmpData <- dfData %>%
  filter(year == "20/21") %>%
  mutate(
    unite(
      across(starts_with("checked_"), ~ ifelse(. == "Ja", cur_column(), NA)),
      na.rm = T,
      col = "checked_combined"
    )
  ) %>%
  mutate(
    checked_combined = stringr::str_remove_all(checked_combined, "checked_"),
    checked_combined = ifelse(checked_combined == "", "Nein", checked_combined)
  )


resX_Checked <- list()
myFactor <- "checked_combined"

resX_Checked$result <- fGlmNullModel(tmpData, myFactor) %>%
  filter(n >= 10)
# resX_Checked$chi <- fChistar(resX_Checked$result, myFactor)

resX_Checked$p <- fPlot(
  resX_Checked$result,
  tibble(),
  f = myFactor,
  # allData = TRUE,
  # raw = tmpData %>% drop_na({{ myFactor }})
) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90)
  )

fSaveImages("playing", resX_Checked$p)

## Do a "Network Analysis" which cohorts belong to which

glimpse(dfData)


# Hobby vs Big Network

tmpH <- dfData %>%
  filter(hives_winter < 25) %>%
  select(id, op_migratory_beekeeper:op_foreign_wax, flow_brassica_napus:flow_melezitose) %>%
  mutate(
    across(-id, ~ ifelse(. == "Ja", 1, 0))
  )

tmpP <- dfData %>%
  filter(hives_winter >= 25) %>%
  select(id, op_migratory_beekeeper:op_foreign_wax, flow_brassica_napus:flow_melezitose) %>%
  mutate(
    across(-id, ~ ifelse(. == "Ja", 1, 0))
  )

Ht1 <- tmpH %>%
  mutate(id_count = n()) %>%
  pivot_longer(-c(id, id_count)) %>%
  group_by(name) %>%
  mutate(size = sum(value, na.rm = TRUE) / first(id_count)) %>%
  ungroup() %>%
  filter(value == 1)


Pt1 <- tmpP %>%
  mutate(id_count = n()) %>%
  pivot_longer(-c(id, id_count)) %>%
  group_by(name) %>%
  mutate(size = sum(value, na.rm = TRUE) / first(id_count)) %>%
  ungroup() %>%
  filter(value == 1)

# Create a second Dummy Set for joining
Ht2 <- Ht1 %>%
  rename(name2 = name) %>%
  select(id, name2)

Pt2 <- Pt1 %>%
  rename(name2 = name) %>%
  select(id, name2)

coords <- tibble(
  deg = seq(min(1), max(360), length.out = length(unique(Ht1$name)) + 1) %>% tail(-1),
  name = unique(Ht1$name)
) %>%
  # Convert to Coordinates
  mutate(
    theta = 2 * pi * deg / 360,
    x = 5 * sin(theta),
    y = 5 * cos(theta)
  )

Hp <- Ht1 %>%
  left_join(coords) %>%
  left_join(Ht2) %>%
  filter(name != name2) %>%
  rowwise() %>%
  mutate(
    connection = stringr::str_sort(c(name, name2)) %>% paste0(collapse = "-")
  ) %>%
  add_count(connection) %>%
  mutate(n = n / first(id_count)) %>%
  distinct(name, connection, .keep_all = TRUE) %>%
  arrange(desc(n))

Pp <- Pt1 %>%
  left_join(coords) %>%
  left_join(Pt2) %>%
  filter(name != name2) %>%
  rowwise() %>%
  mutate(
    connection = stringr::str_sort(c(name, name2)) %>% paste0(collapse = "-")
  ) %>%
  add_count(connection) %>%
  mutate(n = n / first(id_count)) %>%
  distinct(name, connection, .keep_all = TRUE) %>%
  arrange(desc(n))

glimpse(Hp)


p_hobby <- Hp %>% mutate(type = "< 25 Colonies")
p_big <- Pp %>% mutate(type = ">= 25 Colonies")

tempAbstract <- p_big %>%
  select(n, type, connection) %>%
  rename(n_big = n, type_big = type)
p_diff <- p_hobby %>%
  left_join(tempAbstract) %>%
  mutate(
    diff = n - n_big,
    a = abs(diff),
    diff_dis = ifelse(diff < 0, "negative", "positive")
  )
glimpse(p_diff)

p_diff %>%
  ggplot(
    aes(
      x = x,
      y = y,
      group = connection
    )
  ) +
  geom_line(aes(color = diff_dis, alpha = a), size = 2, show.legend = TRUE) +
  geom_point(aes(), color = "black", show.legend = FALSE) +
  geom_text(
    aes(label = name, y = ifelse(y < 0, y - 0.5, y + 0.5)),
    show.legend = FALSE,
    check_overlap = TRUE,
    nudge_y = 0.02
  ) +
  ggplot2::scale_colour_viridis_d() +
  ggplot2::coord_fixed(clip = "off") +
  theme_void()


dplyr::bind_rows(p_hobby, p_big) %>%
  ggplot(
    aes(
      x = x,
      y = y, group = connection, size = size, color = name
    )
  ) +
  geom_line(aes(size = n, alpha = n), color = "black", show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  geom_text(aes(label = name), show.legend = FALSE, check_overlap = TRUE, nudge_y = 0.02) +
  facet_wrap(~type) +
  theme_void()


#### Network Flow


tmp <- dfData %>%
  select(id, flow_brassica_napus:flow_melezitose) %>%
  mutate(
    across(-id, ~ ifelse(. == "Ja", 1, 0))
  )

t1 <- tmp %>%
  mutate(id_count = n()) %>%
  pivot_longer(-c(id, id_count)) %>%
  group_by(name) %>%
  mutate(size = sum(value, na.rm = TRUE) / first(id_count)) %>%
  ungroup() %>%
  filter(value == 1)

t2 <- t1 %>%
  rename(name2 = name) %>%
  select(id, name2)

coords <- tibble(
  deg = seq(min(1), max(360), length.out = length(unique(t1$name)) + 1) %>% tail(-1),
  name = unique(t1$name)
) %>%
  # Convert to Coordinates
  mutate(
    theta = 2 * pi * deg / 360,
    x = 5 * sin(theta),
    y = 5 * cos(theta)
  )

p <- t1 %>%
  left_join(coords) %>%
  left_join(t2) %>%
  filter(name != name2) %>%
  rowwise() %>%
  mutate(
    connection = stringr::str_sort(c(name, name2)) %>% paste0(collapse = "-")
  ) %>%
  add_count(connection) %>%
  mutate(n = n / first(id_count)) %>%
  distinct(name, connection, .keep_all = TRUE) %>%
  arrange(desc(n))

p %>%
  ggplot(
    aes(
      x = x,
      y = y, group = connection, size = size, color = name
    )
  ) +
  geom_line(data = p %>% distinct(connection, .keep_all = TRUE), aes(group = id, size = n, alpha = n), color = "black", show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  geom_text(
    aes(label = name, y = ifelse(y < 0, y - 0.5, y + 0.5)),
    show.legend = FALSE,
    check_overlap = TRUE,
    nudge_y = 0.02
  ) +
  ggplot2::coord_fixed(clip = "off") +
  theme_void()

p %>% distinct(connection, .keep_all = TRUE)

