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
