res5_DistMap <- list()

# Data Cleanup
res5_DistMap$data <- dfData %>%
  drop_na(longitude, latitude) %>% 
  filter(
    # Drop "In mehr as einem Bezirk" because we cannot know which one it belongs to
    district != "In mehr als einem Bezirk"
  )
# Caculate Proportions
res5_DistMap$data <- res5_DistMap$data %>%
  count(year, district) %>%
  group_by(year) %>%
  mutate(
    n = n,
    freq = proportions(n) * 100
  ) %>% 
  # join with map source
  left_join(mfDistrictsSimplify, by = c("district" = "PB"))

res5_DistMap$labels <- res5_DistMap$data %>% 
  group_by(year) %>% 
  summarise(
    n = sum(n),
    n = paste0(year[[1]], " (n=", n, ")")
  ) %>% 
  pull(n)

names(res5_DistMap$labels) <- unique(res5_DistMap$data$year)
res5_DistMap$labels <- as_labeller(res5_DistMap$labels)

res5_DistMap$p <- res5_DistMap$data %>% 
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
    labeller = res5_DistMap$labels
    )

fSaveImages("05_DistrMapN", res5_DistMap$p)
