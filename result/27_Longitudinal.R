res27_Long <- list()

# Remove Participants with multiple answers (same contact different operation)
tmpMultiple <- dfData %>%
    drop_na(contact) %>%
    count(year, contact, sort = TRUE) %>%
    filter(n > 1) %>%
    pull(contact)


res27_Long$data <- dfData %>%
    # drop_na(contact) %>%
    # filter(!(contact %in% tmpMultiple)) %>%
    # Only use the ones which are at least in the same state
    # district name could change, maybe zip?
    group_by(contact, state) %>%
    mutate(
        count = n()
    ) %>%
    ungroup() %>%
    mutate(
        longitudinal = ifelse(count == 4 & !(contact %in% tmpMultiple), TRUE, FALSE)
    )

# Table Hives Winter
res27_Long$table_data <- res27_Long$data %>%
    filter(longitudinal) %>%
    group_by(year) %>%
    summarise(
        n = n(),
        mean_hives_winter = mean(hives_winter),
        median_hives_winter = median(hives_winter),
        sum_hives_winter = sum(hives_winter),
        sum_lost = sum(hives_lost_e)
    )

tempLabel <- glue::glue("Anzahl der eingewinterten Völker über die Jahre für TeilnehmerInnen die über alle vier Umfragejahre teilgenommen haben (n = {res27_Long$table_data$n[[1]]}).")

res27_Long$table <- res27_Long$table_data %>%
    select(Jahr = year, Mittelwert = mean_hives_winter, Median = median_hives_winter, Summe = sum_hives_winter) %>%
    mutate(
        Jahr = glue::glue("20{Jahr}"),
        Mittelwert = fPrettyNum(round(Mittelwert), 0),
        Summe = fPrettyNum(Summe, 0)
    ) %>%
    kable(
        "latex",
        caption = tempLabel,
        label = "u:27:LongColonies",
        booktabs = T,
        escape = F,
        align = c("c", rep("r", 3))
    ) %>%
    kable_styling(latex_options = "HOLD_position", font_size = 8)

res27_Long$table %>% save_kable(paste0("output/tables/27_LongColonies.tex"))

# Loss Rate

res27_Long$result <- fGlmNullModel(res27_Long$data %>% mutate(longitudinal = ifelse(longitudinal, "Langzeit-\nteilnehmerInnen", "Unbekannt")), "longitudinal")
res27_Long$chi <- fChistar(res27_Long$result, "longitudinal", dropNoAnswer = TRUE)
res27_Long$p <- fPlot(
    res27_Long$result,
    res27_Long$chi,
    "longitudinal",
    dropNoAnswer = TRUE,
    allData = TRUE,
    raw = res27_Long$data %>% mutate(longitudinal = ifelse(longitudinal, "Langzeit-\nteilnehmerInnen", "Unbekannt"))
)

fSaveImages("27_Longitudinal", res27_Long$p, w = 8.5)


caption <- "Höhe der Winterverluste in Prozent (und 95\\%~CI) und Anzahl der Antworten in Abhängigkeit ob LangzeitteilnehmerIn (alle vier Jahre) oder nicht über die Umfragejahre 2017/18 - 2020/21."

res27_Long$result %>%
    mutate(
        longitudinal = longitudinal %>% str_remove_all("-\n")
    ) %>%
    fSaveTable("27_Long", ., caption, "longitudinal", "u:27Long")