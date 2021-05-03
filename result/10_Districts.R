res10_Districts <- list()
myFactor <- "district"

dfData <- dfData %>% 
  mutate(
    district = as.factor(district)
  )


# Count -------------------------------------------------------------------
# We only need mean no CI and only use n >= 5
res10_Districts$result <- dfData %>%
  add_count(year, state, district) %>% 
  filter(n >= 5) %>%
  group_by(year, state, district) %>% 
  summarise(
    loss_rate = as.numeric(
      format(
        round((sum(hives_lost_e) / sum(hives_winter) * 100 ), 2), 
        nsmall = 2)
      ),
    n = n(),
    hives_winter = sum(hives_winter),
    number_print = glue("({n}; {hives_winter})"),
    .groups = "drop"
  )

# Labels ------------------------------------------------------------------
res10_Districts$labels <- res10_Districts$result %>%
  group_by(year) %>%
  summarise(
    n = sum(n),
    n = paste0(year[[1]], " (n=", n, ")")
  ) %>%
  pull(n)
names(res10_Districts$labels) <- unique(res10_Districts$result$year)
res10_Districts$labels <- as_labeller(res10_Districts$labels)

# Map ---------------------------------------------------------------------
res10_Districts$result_map <- res10_Districts$result %>%
  left_join(mfDistrictsSimplify, by = c("district" = "PB"))

res10_Districts$pMap <- res10_Districts$result_map %>%
  ggplot() +
  geom_sf(
    aes(group = district, fill = loss_rate, geometry = geometry),
    color = colorBlindBlack8[1],
    size = 0.2
  ) +
  geom_sf(
    data = mfStatesSimplify,
    aes(group = BL),
    color = "black",
    size = 0.6,
    fill = NA
  ) +
  coord_sf() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_viridis_c(
    option = "inferno",
    direction = -1,
    breaks = seq(0, 100, 10)
  ) +
  labs(fill = "Verlustrate [%]") +
  facet_wrap(
    ~year,
    ncol = 2,
    labeller = res10_Districts$labels
  )

fSaveImages("10_DistrictMap", res10_Districts$pMap)


