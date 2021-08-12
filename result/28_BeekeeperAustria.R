res28_BeekeeperAustria <- readxl::read_xlsx("data/tables.xlsx", sheet = "beekeeper", range = "A1:D100") %>% drop_na()


res28_BeekeeperAustria %>%
    tidyr::pivot_longer(cols = c("ImkerInnen", "Völker")) %>%
    ggplot2::ggplot(aes(x = Jahr, y = value)) +
    geom_line(aes(y = iPopRate, group = 2, color = pcolor)) +
    ggplot2::facet_wrap(~name, scales = "free_y", ncol = 1)