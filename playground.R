
# Numer of Participants longitudal ----------------------------------------


RAW %>% 
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


