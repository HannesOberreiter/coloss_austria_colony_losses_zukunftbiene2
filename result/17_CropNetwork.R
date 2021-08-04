res17_CropNetwork <- list()

flows <- dfData %>%
    select(starts_with("flow_")) %>%
    names()
flowsDE <- c("Raps", "Mais", "Sonnenblume", "Spätblühende Zwischenfrüchte", "Waldtracht", "Melezitose")
names(flowsDE) <- flows

res17_CropNetwork$data <- dfData %>%
    select(id, year, starts_with("flow")) %>%
    mutate(
        # Create Binary for our Network Cols
        across(-c("id", "year"), ~ ifelse(. == "Ja", 1, 0))
    ) %>%
    add_count(year, name = "total_count") %>%
    # Dummy count for later
    pivot_longer(-c(id, year, total_count)) %>%
    group_by(name, year) %>%
    mutate(
        # represent relative amount of answers for col (relative to each operation size)
        answers = sum(value, na.rm = TRUE) / first(total_count) * 100
    ) %>%
    ungroup() %>%
    # Only "Yes" is relevant for our Network
    filter(value == 1) %>%
    mutate(
        name = str_replace_all(name, flowsDE)
    ) %>%
    glimpse()

# Temp names to selfjoin and build connections
tmp <- res17_CropNetwork$data %>%
    rename(name2 = name) %>%
    select(id, name2)

res17_CropNetwork$connection <- res17_CropNetwork$data %>%
    # Creating Coordinates from unique names, using the same for both groups
    left_join(fCircleCoordinates(unique(tmp$name2))) %>%
    # Self joining our temporary table
    left_join(tmp) %>%
    # Filtering redundant rows
    filter(name != name2) %>%
    rowwise() %>%
    mutate(
        connection = stringr::str_sort(c(name, name2)) %>% paste0(collapse = "-")
    ) %>%
    group_by(year, connection) %>%
    mutate(
        # Gives us the relative rate of the answers combination
        connection_percent = n() / first(total_count) * 100
    ) %>%
    arrange(year, desc(connection_percent)) %>%
    # don't need all the double entries, could also work with group_by
    distinct(name, name, connection, .keep_all = TRUE) %>%
    mutate(
        # nicer names for plot
        print_name = glue::glue("{name}\n{fPrettyNum(round(answers,1))}%"),
        print_name = forcats::fct_reorder(print_name, sort(print_name))
    ) %>%
    glimpse()

# Create a custom labeller for facets
res17_CropNetwork$labeller <- res17_CropNetwork$connection %>%
    group_by(year) %>%
    summarise(
        n = first(total_count),
        n = format(n, big.mark = ".", decimal.mark = ","),
        n = paste0("20", year[[1]], " (n = ", n, ")")
    ) %>%
    pull(n)
names(res17_CropNetwork$labeller) <- unique(res17_CropNetwork$connection$year)
res17_CropNetwork$labeller <- ggplot2::as_labeller(res17_CropNetwork$labeller)


res17_CropNetwork$p <- res17_CropNetwork$connection %>%
    ggplot(
        aes(x, y, label = print_name, group = connection, size = answers)
    ) +
    geom_line(
        aes(size = connection_percent, alpha = connection_percent),
        color = colorBlindBlack8[3]
    ) +
    geom_point(show.legend = FALSE) +
    geom_text(
        aes(
            # dodge around, double ifelse because switch wont work ?
            x = ifelse(x > 0, x + 0.6, ifelse(near(round(x), 0), x, x - 0.6)),
            y = ifelse(y < 0, y - 0.8, y + 0.8)
        ),
        size = 4,
        check_overlap = TRUE # still have for each point two entries so remove one with this
    ) +
    scale_size(
        "Häufigkeit der Kombination [%]",
        limits = c(0.1, 100),
    ) +
    xlab("") +
    ylab("") +
    ggplot2::coord_fixed(clip = "off") +
    facet_wrap(~year, labeller = res17_CropNetwork$labeller) +
    ggplot2::guides(alpha = "none") +
    ggplot2::theme(
        # cleanup we could also work from void up
        legend.position = "bottom",
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        # extend margin between facets and also on the right side because of missing text
        panel.spacing = unit(2.5, "lines"),
        # plot.margin = margin(r = 12, unit = "pt")
    )

fSaveImages("17_Crop_Network", res17_CropNetwork$p, w = 10, h = 10)
