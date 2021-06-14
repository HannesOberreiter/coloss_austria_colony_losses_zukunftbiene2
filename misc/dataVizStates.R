source("partials/setup.R")
source("partials/header.R")
source("partials/map.R")
source("functions/functions.R")
# Adittional Libs
library(ggtext)
library(showtext)

# Data Mangling ----
yearData <- dfData %>% filter(year == "20/21")
resultAustria <- fGlmNullModel(yearData, "global") %>%
    mutate(
        state = "Österreich",
        color = "#D55E00",
        text = "bold"
    )
result <- fGlmNullModel(yearData, "state") %>%
    mutate(
        color = "#E69F00",
        text = "plain"
    ) %>%
    dplyr::bind_rows(resultAustria) %>%
    select(-chistar, -global) %>%
    mutate(
        state = state %>% forcats::fct_reorder(middle)
    ) %>%
    arrange(middle)

result_map <- result %>%
    filter(state != "Österreich") %>%
    left_join(mfStatesSimplify, by = c("state" = "BL"))

# Text ----
n <- nrow(yearData)
nHives <- sum(yearData$hives_winter)
nHivesLost <- sum(yearData$hives_lost_e)
middleNice <- fPrettyNum(resultAustria$middle)
lowerNice <- fPrettyNum(resultAustria$lower)
upperNice <- fPrettyNum(resultAustria$upper)
subtitle <- glue::glue("Absolute Verlustrate inklusive 95% Konfidenintervall für die Bundesländer und Österreich im Gesamten. Insgesamt nahmen <b>{n}</b> Imkerinnen und Imkern an der Umfrage Teil. Diese Imkereien<br> haben insgesamt <b>{nHives}</b> Bienenvölker eingewintert. Von diesen eingewinterten Bienenvölkern haben <b>{nHivesLost}</b>, das entspricht <b>{middleNice}</b> Prozent (Konfidenzintervall: {lowerNice}-{upperNice}%),<br> den Winter nicht überlebt oder hatten unlösbare Königinnenprobleme.")

# Set Base Theme ----
theme_set(
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(
            color = "grey10", size = 25, face = "bold",
            hjust = 0,
            margin = margin(t = 0)
        ),
        plot.subtitle = ggtext::element_markdown(
            color = "grey30", size = 15,
            lineheight = 1.35,
            hjust = 0,
            margin = margin(t = 10, b = 30)
        ),
        plot.title.position = "plot",
        strip.background = element_rect(
            fill = "white",
            colour = "black", size = rel(2)
        ),
        plot.caption = element_text(
            color = "grey30", size = 8,
            lineheight = 1.2, hjust = 1,
            margin = margin(t = 4, b = 0)
        ),
    )
)

font_add_google("Source Sans Pro", "sourcesans")
## Automatically use showtext to render text for future devices
showtext_auto()
## Tell showtext the resolution of the device,
## only needed for bitmap graphics. Default is 96
showtext_opts(dpi = 96)

# Plots ----
plot <- result %>%
    ggplot(aes(x = state, y = middle, color = I(color))) +
    ggplot2::geom_hline(yintercept = resultAustria$middle, color = "#D55E00", linetype = "dashed") +
    ggplot2::geom_pointrange(aes(ymin = lower, ymax = upper), size = 2) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
        limits = c(0, NA),
        expand = c(0, NA),
        breaks = seq(0, 25, 5),
        minor_breaks = seq(2.5, 25, 2.5)
    ) +
    xlab("") +
    ylab("Verlustrate [%]") +
    ggplot2::theme(
        axis.text.y = element_text(hjust = 1, face = result$text, size = 12.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"),
        axis.ticks.y = element_blank(),
        legend.key = element_blank(),
        axis.line.x = element_line(linetype = "solid", size = 0.5),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_line(colour = "lightgrey")
    )

map <- result_map %>%
    ggplot() +
    geom_sf(
        data = mfStatesSimplify,
        aes(group = BL),
        color = "black",
        size = 0.6,
        fill = "white"
    ) +
    geom_sf(
        aes(group = state, fill = middle, geometry = geometry),
        color = "black",
        size = 0.6
    ) +
    coord_sf() +
    xlab("") +
    ylab("") +
    scale_fill_viridis_c(
        option = "inferno",
        direction = -1,
        breaks = c(8, 10, 12, 14, 16, 18, 20)
    ) +
    ggplot2::labs(
        fill = "Verlustrate [%]"
    ) +
    labs() +
    ggplot2::theme(
        legend.position = "right",
        axis.ticks = element_blank()
    )


patchPlot <- plot + map
patchPlot <- patchPlot + plot_annotation(
    title = "Winterverluste Bienenvölker 2020/21",
    subtitle = subtitle,
    caption = "Institut der Biologie, Universität Graz, 2021 | Karte CC BY 4.0 Bundesamt für Eich- und Vermessungswesen"
)

fSaveImages("dataVizStates", patchPlot, 18, 7)