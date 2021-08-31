res17_Crop <- list()
res17_Crop$result <- list()
res17_Crop$chi <- list()
res17_Crop$p <- list()

# Names of Crop Questions (Yes, No, Uncertain, NA)
flows <- dfData %>%
    select(starts_with("flow_")) %>%
    names()

flowsDE <- c("Raps Tracht", "Sammelnde Bienen auf Mais", "Sonnenblume Tracht", "Tracht von spätblühenden Zwischenfrüchten", "Waldtracht", "Waldtracht mit Melezitose")
names(flowsDE) <- flows

for (flow in flows) {
    myFactor <- flow
    tempData <- dfData

    tempData <- tempData %>%
        mutate(
            !!flow := tidyr::replace_na(!!sym(flow), "keine Angaben"),
            !!flow := as.factor(!!sym(flow))
        )

    res17_Crop$result[[flow]] <- fGlmNullModel(tempData, myFactor) %>%
        mutate(
            !!flow := as.factor(!!sym(flow)) %>% fct_relevel(c("Ja", "Nein", "Unsicher", "keine Angaben"))
        ) %>%
        arrange(year, !!sym(flow))

    res17_Crop$chi[[flow]] <- fChistar(res17_Crop$result[[flow]], myFactor)
    res17_Crop$p[[flow]] <- fPlot(
        res17_Crop$result[[flow]],
        res17_Crop$chi[[flow]],
        myFactor,
        xTitle = "",
        dropNoAnswer = TRUE,
        text = flowsDE[[flow]],
        allData = TRUE,
        raw = tempData %>% filter(!!sym(flow) %in% c("Ja", "Nein"))
    )

    caption <- glue::glue("Höhe der Winterverluste in Prozent (und 95\\%~CI) und Anzahl 0000000 (eingewinerte Bienenvölker) über die Umfragejahre 2017/18 - 2020/21.")
    fSaveTable(glue::glue("17_Crop_{flow}"), res17_Crop$result[[flow]], caption, myFactor, glue::glue("u:17crop_{flow}"))

    fSaveImages(glue::glue("17_Crop_{flow}"), res17_Crop$p[[flow]], w = 8.5)
}