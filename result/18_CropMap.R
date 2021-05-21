res18_CropMap <- list()
res18_CropMap$result <- list()
res18_CropMap$p <- list()
res18_CropMap$labels <- list()

# Names of Crop Questions (Yes, No, Uncertain, NA)
flows <- dfData %>%
    select(starts_with("flow_")) %>%
    names()
flowNames <- c("Raps", "Mais", "Sonnenblume", "Spätblüher", "Waldtracht", "Waldtracht mit Melezitose")
names(flowNames) <- flows

for (flow in flows) {
    tempData <- dfData %>%
        filter(
            district != "In mehr als einem Bezirk" &
                op_migratory_beekeeper == "Nein" &
                !!sym(flow) == "Ja"
        )
    # Caculate Proportions
    res18_CropMap$result[[flow]] <- tempData %>%
        count(year, district) %>%
        group_by(year) %>%
        mutate(
            n = n,
            freq = proportions(n) * 100
        ) %>%
        # join with map source
        left_join(mfDistrictsSimplify, by = c("district" = "PB"))

    res18_CropMap$labels[[flow]] <- fLabeller(res18_CropMap$result[[flow]])

    res18_CropMap$p[[flow]] <- res18_CropMap$result[[flow]] %>%
        ggplot() +
        geom_sf(
            data = mfStatesSimplify,
            aes(group = BL),
            color = "black",
            size = 0.6,
            fill = "white"
        ) +
        geom_sf(
            aes(group = district, fill = freq, geometry = geometry),
            color = colorBlindBlack8[1],
            size = 0.2
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
            limits = c(0, 20),
            breaks = seq(0, 20, 4),
            direction = -1
        ) +
        labs(fill = glue::glue("{flowNames[[flow]]}  [%]")) +
        facet_wrap(
            ~year,
            ncol = 2,
            labeller = res18_CropMap$labels[[flow]]
        )
    saveName <- glue::glue("18_CropMap_{flow}")
    message(saveName)
    fSaveImages(saveName, res18_CropMap$p[[flow]])
}
