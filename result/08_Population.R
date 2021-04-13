res08_Population <- list()

res08_Population$data <- dfData %>%
  drop_na(hives_spring_before) %>% 
  group_by(year) %>% 
  summarise(
    spring        = sum(hives_spring_before),
    winter        = sum(hives_winter),
    summer_change = winter - spring,
    winter_loss   = winter - sum(hives_lost_e),
    summer_percentage      = fNumberFormat(winter * 100 / spring - 100),
    winter_loss_percentage = fNumberFormat(winter_loss * 100 / winter - 100),
    spring_spring          = fNumberFormat(winter_loss * 100 / spring - 100)
  )

##### Generate Plot from fixed values #####

# years and increase over summer and loss over upcoming winter in percent
# increase is over given year (spring - spring) and loss is for following spring next year.
# eg. 2013/2014 increase 24.4% from spring 2013 - winter 2013; loss 12.0% from winter 2013 - spring 2014
vYears <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
dConst <- tibble(
  vIncrease   = c(24.4, 24.6, 44.4, 44.9, res08_Population$data$summer_percentage) / 100,
  vLoss       = c(12.0, 28.6, 7.6, 22.5, res08_Population$data$winter_loss_percentage*-1) / 100
)
# Start Value for calculating the Populationsdynamics
vStart <- 100
for (i in seq_along(vYears)) {
  # first year start = startvalue
  if(i == 1){
    vCalc = vStart
    res08_Population$dPop <- tibble(
      year = vYears[i],
      season = 'Frühjahr',
      value = vStart
    )
  }
  if(i < length(vYears)){
    vCalc <- vCalc + vCalc * dConst$vIncrease[i]
    res08_Population$dPop <- res08_Population$dPop %>% add_row(
      year = vYears[i],
      season = 'Herbst',
      value = vCalc
    )
    vCalc <- vCalc - vCalc * dConst$vLoss[i]
    res08_Population$dPop <- res08_Population$dPop %>% add_row(
      year = vYears[i],
      season = 'Frühjahr',
      value = vCalc
    )
  }
}

# Cleanup
rm(i, vCalc)

res08_Population$dPop <- res08_Population$dPop %>% 
  rowid_to_column("idu") %>% 
  mutate(
    value       = round(value),
    labelbottom = ifelse(season == "Frühjahr", value, ""),
    labeltop    = ifelse(season == "Herbst",   value, ""),
  )

res08_Population$p <- ggplot(data = res08_Population$dPop) +
  aes(x = idu, y = value) +
  geom_line(aes(group = 1)) +
  geom_point(aes(colour = season), show.legend = FALSE, size = 5) + 
  #geom_text(aes(label = value), nudge_x = -0.3, hjust="right", show.legend = FALSE)+
  geom_text(aes(label = labeltop), nudge_y = +20, show.legend = FALSE)+
  geom_text(aes(label = labelbottom), nudge_y = -20, show.legend = FALSE)+
  geom_text(aes(y = 5, label=season, colour = season), show.legend = FALSE, size = 2.5) +
  ylab("Entwicklung Völkerzahl \n (basierend auf 100 Völkern am Beginn)") + 
  xlab("Zeitverlauf über Jahre") +
  ggtitle("") +
  theme_classic() + 
  #annotate("segment", x = 0, xend = Inf, y = 10, yend = 10) +
  theme(
    plot.title = element_text(), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 12, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" )
  ) +
  scale_x_continuous(
    limits = c( NA, max(dPop$idu)+0.5 ),
    breaks = seq(1.5,length(vYears)*2,2),
    labels = vYears
    #labels= D_DATA$season
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, max(dPop$value)+50, 50 ),
    limits = c( 0, max(dPop$value)+50 )
  )

rm(vStart, vYears)

fSaveImages("08_Population", res08_Population$p, h = 3.5)
