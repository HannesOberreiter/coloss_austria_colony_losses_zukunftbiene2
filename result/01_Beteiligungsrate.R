res01_Beteiligungsrate <- list()

res01_Beteiligungsrate$data <- tibble::tribble(
  ~year,    ~iPop,   ~iSurvey, ~iRate,     ~hPop,    ~hSurvey,  ~hRate,
  "2013/14", 25492,      1023,  4.0,      382638,       18794,     4.9,
  "2014/15", 25277,      1259,  5.0,      376121,       22882,     6.1,
  "2015/16", 26063,      1289,  4.9,      347128,       23418,     6.7,
  "2016/17", 26609,      1656,  6.2,      354080,       43852,    12.4,
  "2017/18", 27580,      1391,  5.0,      353267,       28373,     8.0,
  "2018/19", 28432,      1534,  5.4,      373412,       33651,     9.0,
  "2019/20", 30237,      1539,  5.1,      390607,       30724,     7.9,
  "2020/21", 31923,         0,    0,      426121,           0,       0
  ) %>% 
  mutate(
    iPopRate = iPop * 100 / first(iPop) - 100,
    hPopRate = hPop * 100 / first(hPop) - 100,
    year_short = str_replace(year, "20", "")
  )

res01_Beteiligungsrate$data$pcolor <- colorBlindBlack8[3]
res01_Beteiligungsrate$data$pcolor[5:8] <- colorBlindBlack8[6]

res01_Beteiligungsrate$p1 <- res01_Beteiligungsrate$data %>% 
  ggplot(aes(x = year_short)) +
  geom_line(aes(y = iPopRate, group = 2, color = pcolor)) +
  geom_point(aes(y = iPopRate, group = 2, color = pcolor), size = 2) +
  #geom_line(aes(y = iRate, group = 1, color = colorBlindBlack8[2]), ) +
  #geom_point(aes(y = iRate, group = 1, color = colorBlindBlack8[2]), size = 2) +
  scale_color_identity(
    #name = "",
    #labels = c(" Veränderung \n der Gesamtzahl \n seit 2013\n", " Anteil in \n der Umfrage"),
    #guide = "legend"
  ) +
  theme(
    axis.line.x = element_line(linetype = "solid", size = 0.5),
  ) + 
  ylab("Veränderung seit 2013 [%]") + xlab("") + 
  #labs(tag = "A") +
  scale_y_continuous(breaks = seq(-100, 100, 5)) +
  facet_grid( ~ "Imkereien")

res01_Beteiligungsrate$p2 <- res01_Beteiligungsrate$data %>% 
  ggplot(aes(x = year_short)) +
  geom_line(aes(y = hPopRate, group = 2, color = pcolor)) +
  geom_point(aes(y = hPopRate, group = 2, color = pcolor), size = 2) +
  #geom_line(aes(y = hRate, group = 1, color = colorBlindBlack8[2]) ) +
  #geom_point(aes(y = hRate, group = 1, color = colorBlindBlack8[2]),  size = 2) +
  scale_color_identity(
    ) +
  theme(
    axis.line.x = element_line(linetype = "solid", size = 0.5),
  ) + 
  ylab("Veränderung seit 2013 [%]") + xlab("") +
  #labs(tag = "B") +
  scale_y_continuous(breaks = seq(-100, 100, 5)) +
  facet_grid( ~ "Bienenvölker")


fSaveImages("01_Beteiligungsrate", res01_Beteiligungsrate$p1 / res01_Beteiligungsrate$p2, h = 4 * 1.5)

