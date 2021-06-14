res09_Districts <- list()

res09_Districts$result <- dfData %>%
  group_by(year, state, district) %>%
  summarise(
    loss = format(round((sum(hives_lost_e) / sum(hives_winter) * 100), 2), nsmall = 2),
    n = n(),
    hw = sum(hives_winter),
    loss_p = glue::glue("{loss}%") %>% stringr::str_replace("\\.", ","),
    nhw = glue::glue("({n}; {hw})")
  ) %>%
  filter(n > 4) %>%
  arrange(state, district)


res09_Districts$result %>%
  filter(year == "20/21") %>%
  filter(state == "Wien") %>%
  select(state, district, loss_p, nhw)