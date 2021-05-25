
# Frequency List of Treatment Methods relative and absolute number of participants
tmpRename <- treatmentList$tname
names(tmpRename) <- treatmentList$ttotal
tabData <- dfData %>%
    filter(submitted != "Zeitung" & varroa_treated == "Ja" & t_amount > 0) %>%
    select(year, ends_with("total12")) %>%
    select(-T_vcount_total12) %>%
    pivot_longer(-year, names_to = "treatment", values_to = "value") %>%
    mutate(
        value = ifelse(value > 0, "Ja", "Nein")
    ) %>%
    add_count(year, treatment) %>%
    count(year, treatment, n, value) %>%
    group_by(year, treatment, value) %>%
    summarise(
        np = nn * 100 / n,
        n = nn
    ) %>%
    mutate(
        sorting = np[1]
    ) %>%
    ungroup() %>%
    mutate(
        treatment = stringr::str_remove(treatment, "12") %>%
            stringr::str_replace_all(tmpRename)
    ) %>%
    arrange(treatment, year) %>%
    select(-sorting) %>%
    tidyr::pivot_wider(names_from = value, values_from = np:n) %>%
    select(treatment, year, n_Ja, np_Ja, n_Nein, np_Nein)

tab <- tabData %>%
    mutate(
        n_Ja = fPrettyNum(n_Ja, 0),
        n_Nein = fPrettyNum(n_Nein, 0),
        np_Ja = fPrettyNum(round(np_Ja, 1), 1),
        np_Nein = fPrettyNum(round(np_Nein, 1), 1)
    ) %>%
    select(-treatment) %>%
    knitr::kable(
        "latex",
        caption = "Anzahl (Prozent) der Imkereien, welche die genannte Methode zur Bekämpfung der Varroamilbe in zumindest einem Monat angewendet haben.",
        label = "u:behandlungsmethoden",
        booktabs = T,
        escape = F,
        linesep = "", # https://stackoverflow.com/questions/45409750/get-rid-of-addlinespace-in-kable
        col.names = c("Jahr", "Ja [n]", "Ja [\\%]", "Nein [n]", "Nein [\\%]"),
        align = c("l", rep("r", 4))
    ) %>%
    kable_styling(latex_options = "HOLD_position", font_size = 10)

begin <- 1
for (i in unique(tabData$treatment)) {
    end <- begin + nrow(tabData %>% filter(treatment == i)) - 1
    tab <- tab %>% pack_rows(i, begin, end)
    begin <- end + 1
}

# small bugfix as pack_rows introduces an linespaceing which breaks midrule
tab <- sub("midrule{}", "midrule", tab, fixed = T)

tab %>% save_kable(paste0("output/tables/20_TreatmentFreqList.tex"))