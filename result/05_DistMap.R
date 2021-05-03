res05_DistMap <- list()

# Data Cleanup
res05_DistMap$data <- dfData %>%
  drop_na(longitude, latitude) %>% 
  filter(
    # Drop "In mehr as einem Bezirk" because we cannot know which one it belongs to
    district != "In mehr als einem Bezirk"
  )
# Caculate Proportions
res05_DistMap$data <- res05_DistMap$data %>%
  count(year, district) %>%
  group_by(year) %>%
  mutate(
    n = n,
    freq = proportions(n) * 100
  ) %>% 
  # join with map source
  left_join(mfDistrictsSimplify, by = c("district" = "PB"))

res05_DistMap$labels <- res05_DistMap$data %>% 
  group_by(year) %>% 
  summarise(
    n = sum(n),
    n = paste0(year[[1]], " (n=", n, ")")
  ) %>% 
  pull(n)

names(res05_DistMap$labels) <- unique(res05_DistMap$data$year)
res05_DistMap$labels <- as_labeller(res05_DistMap$labels)

res05_DistMap$p <- res05_DistMap$data %>% 
  ggplot() +
  geom_sf(
    data = mfStatesSimplify, 
    aes(group = BL), 
    color = "black", 
    size = 0.6, 
    fill = "white"
    ) +
  geom_sf(
    aes(group = district, fill = n, geometry = geometry),
    color = colorBlindBlack8[1],
    size = 0.2
    ) +
  coord_sf() +
  xlab("") + ylab("") +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_viridis_c(
    option = "inferno", 
    direction = -1
    ) +
  labs(fill = "TeilnehmerInnen [#]") +
  facet_wrap(
    ~ year,
    ncol = 2,
    labeller = res05_DistMap$labels
    )

fSaveImages("05_DistrMapN", res05_DistMap$p)
