res15_OP <- list()
res15_OP$result <- list()
res15_OP$chi <- list()
res15_OP$p <- list()

# Names of Operational Questions (Yes, No, Uncertain, NA)
OPs <- c(
    "op_cert_org_beek", "op_migratory_beekeeper", "op_varroatolerant",
    "op_plastic_hives", "op_insulated_hives", "op_mash_bottom_board",
    "op_foreign_wax", "op_no_foundation", "op_small_broodcells",
    "merged"
)

opsDE <- c(
    "Zertifizierte Bio-Imkerei", "Wanderimkerei", "Zucht aus Varroa-Toleranz",
    "Kunststoff-Beuten", "Isolierte Beuten im Winter", "Offener Gitterboden im Winter",
    "Fremdwachs", "Naturwabenbau", "Kleine Brutzellen (5,1 mm oder weniger)",
    "Vereinigung von Völkern"
)
names(opsDE) <- OPs

for (OP in OPs) {
    myFactor <- OP
    tempData <- dfData

    if (OP == "merged") {
        # New Questions since 2019/20
        # Only available for Internet Submission
        tempData <- tempData %>% filter(year %in% c("19/20", "20/21") & submitted == "Internet")
    }

    tempData <- tempData %>%
        mutate(
            !!OP := tidyr::replace_na(!!sym(OP), "keine Angaben"),
            !!OP := as.factor(!!sym(OP))
        )

    res15_OP$result[[OP]] <- fGlmNullModel(tempData, myFactor) %>%
        mutate(
            !!OP := as.factor(!!sym(OP)) %>% fct_relevel(c("Ja", "Nein", "Unsicher", "keine Angaben"))
        ) %>%
        arrange(year, !!sym(OP))

    res15_OP$chi[[OP]] <- fChistar(res15_OP$result[[OP]], myFactor, dropNoAnswer = TRUE)
    res15_OP$p[[OP]] <- fPlot(res15_OP$result[[OP]], res15_OP$chi[[OP]], myFactor, xTitle = "", dropNoAnswer = TRUE, text = opsDE[[OP]])

    caption <- glue::glue("Höhe der Winterverluste in Prozent ($\\pm$95\\%CI) und Anzahl 0000000 (eingewinerte Bienenvölker) über die Umfragejahre 2017/18 - 2020/21.")
    fSaveTable(glue::glue("15_Operational_{OP}"), res15_OP$result[[OP]], caption, myFactor, glue::glue("u:15operational_{OP}"))

    fSaveImages(glue::glue("15_Operational_{OP}"), res15_OP$p[[OP]], w = 8.5, h = if_else(OP == "merged", 4, 6))
}