res13_Altitude <- list()
myFactor <- "altitude_group"

order <- c("0-200m", "201-400m", "401-600m", "601-800m", ">800m")
orderM <- order %>% stringr::str_remove("m")
dfData <- dfData %>%
    mutate(
        altitude_group = cut(
            altitude,
            c(seq(0, 800, 200), Inf),
            label = order,
            include.lowest = TRUE,
            right = TRUE
        )
    )

# For precision we only use the altitude data if all apiaries in 15km radius
# and also remove if district is in more than one
res13_Altitude$result <- dfData %>%
    filter(
        apiary_nearby == "Ja" & district != "In mehr als einem Bezirk"
    ) %>%
    drop_na(altitude_group) %>%
    fGlmNullModel(., myFactor) %>%
    mutate(
        altitude_group = str_remove(altitude_group, "m") %>%
            as_factor() %>%
            forcats::fct_relevel(orderM),
        chistar = str_replace_all(chistar, "m", "")
    )

res13_Altitude$chi <- fChistar(res13_Altitude$result, myFactor)

res13_Altitude$p <- fPlot(
    res13_Altitude$result,
    res13_Altitude$chi,
    myFactor,
    xTitle = "Seehöhe [m]",
    yMax = TRUE,
    allData = TRUE,
    raw = dfData %>% filter(
        apiary_nearby == "Ja" & district != "In mehr als einem Bezirk"
    ) %>%
        drop_na(altitude_group) %>%
        mutate(
            altitude_group = str_replace(altitude_group, "m", "") %>%
                as_factor() %>%
                forcats::fct_relevel(orderM)
        )
)

fSaveImages("13_Altitude", res13_Altitude$p, w = 8.5)

caption <- "Höhe der Winterverluste in Prozent (und 95\\%~CI) und Anzahl der Antworten in Abhängigkeit von der Seehöhe der Winterstandorte über die Umfragejahre 2017/18 - 2020/21. Nicht ausgewertet sind TeilnehmerInnen die „Nein“ oder „Unsicher“ oder „Ja“ bei Fragestellung „Alle Bienenvölker innerhalb eines 15 km Radius“ angaben, sowie ihre Bienenvölkerin „mehr als einem Bezirk“ aufgestellt haben."

fSaveTable("13_Altitude", res13_Altitude$result, caption, myFactor, "u:13altitude")