res05_allYearLosses <- list()

# Hard-Coded as I don't have all previous years
res05_allYearLosses$data <- tibble::tribble(
      ~year, ~middle, ~lowerlim, ~upperlim,   ~n, ~ncolonies,
  "2007/08",    13.3,       9.9,      16.7,  374,      16217,
  "2008/09",     9.3,       6.9,      11.6,  575,      18141,
  "2009/10",    14.7,      12.7,      16.9,  311,       7676,
  "2010/11",    16.3,      14.9,      17.8,  565,      13179,
  "2011/12",    25.9,      24.6,      27.2, 1537,      32471,
  "2012/13",    17.3,      16.1,      18.7,  997,      19406,
  "2013/14",    12.8,      11.7,      14.0, 1023,      18794,
  "2014/15",    28.4,      27.0,      29.9, 1259,      22882,
  "2015/16",     8.1,       7.4,       8.8, 1289,      23418,
  "2016/17",    23.0,      22.1,      24.0, 1656,      43852,
  "2017/18",    11.8,      11.1,      12.5, 1391,      28373,
  "2018/19",    15.2,      14.4,      16.1, 1534,      33651,
  "2019/20",    12.6,      11.9,      13.3, 1539,      30724,
  "2020/21",       0,         0,         0,    0,          0,
  )

# Plot Data Helper
res05_allYearLosses$totalSummary <- c(
  "totalMean"       = mean(res05_allYearLosses$data$middle),
  "totalMedian"     = median(res05_allYearLosses$data$middle),
  "totalPooledMean" = sum(res05_allYearLosses$data$middle * res05_allYearLosses$data$n) / sum(res05_allYearLosses$data$n),
  "totalSD"         = sd(res05_allYearLosses$data$middle)
  )

#V.POOLED_LOWER <- sum(res05_allYearLosses$data$lowerlim * res05_allYearLosses$data$n) / sum(res05_allYearLosses$data$n)
#V.POOLED_UPPER <- sum(res05_allYearLosses$data$upperlim * res05_allYearLosses$data$n) / sum(res05_allYearLosses$data$n)

res05_allYearLosses$p <- res05_allYearLosses$data %>% 
  ggplot(
    aes(y = year, x = middle)
    ) +
  geom_vline(
    xintercept = res05_allYearLosses$totalSummary[["totalMean"]] - res05_allYearLosses$totalSummary[["totalSD"]], 
    linetype="dashed", color = "red", size=1
    ) +
  geom_vline(
    xintercept = res05_allYearLosses$totalSummary[["totalMean"]] + res05_allYearLosses$totalSummary[["totalSD"]], 
    linetype="dashed", color = "red", size=1
    ) +
  geom_vline(
    xintercept = res05_allYearLosses$totalSummary[["totalMean"]], color = "red", size=1
    ) +
  geom_vline(
    xintercept = res05_allYearLosses$totalSummary[["totalMedian"]], color = "blue", size=1
    ) +
  geom_crossbar(
    aes( xmin = lowerlim, xmax = upperlim ), 
    fill = "white"
    ) +
  geom_point(size = 3) +
  geom_text(
    aes(y = year, x = 2, label = paste("(TN = ", n, "; VÖ = ", ncolonies, ")", sep = "")), 
    angle = 0, color = "black", size = 4, hjust = 0
    ) +
  ylab("Winter / Jahr") + 
  xlab("Verlustrate [%]") +
  theme_classic() + 
  theme(
    plot.title = element_text(size=20), 
    axis.title.x = element_text(colour = "black", size = 15 ), 
    axis.title.x.top = element_blank(), 
    axis.title.y = element_text(colour = "black", size = 15 ), 
    axis.text.x = element_text(angle = 0, size = 13, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 13, face = "bold", margin = margin(0, -52, 0, 0)),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.x = element_line( colour = "grey" ),
    panel.grid.minor.x = element_line( colour = "grey" )
  ) +
  scale_x_continuous(
    sec.axis = dup_axis(),
    limits   = c(0, max(res05_allYearLosses$data$upperlim)*1.1),
    expand   = c( 0 , 0 ),
    breaks   = seq( 0, 100, 5 )
  )

fSaveImages("06_AllYear", res05_allYearLosses$p, w = 12)

