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


