res15_OP_Network <- list()

# Names of Operational Questions (Yes, No, Uncertain, NA)
# Shorter Version for NEtwork
OPs <- c(
    "op_cert_org_beek", "op_migratory_beekeeper", "op_varroatolerant",
    "op_plastic_hives", "op_insulated_hives", "op_mash_bottom_board",
    "op_foreign_wax", "op_no_foundation", "op_small_broodcells",
    "merged"
)
opsDE <- c(
    "Bio-Imkerei", "Wanderimkerei", "Varroatoleranz",
    "Kunststoff", "Isolierte\nBeuten", "Gitterboden",
    "Fremdwachs", "Naturwabenbau", "Kleine\nBrutzellen",
    "Vereinigung von Völkern"
)
names(opsDE) <- OPs

res15_OP_Network$data <- dfData %>%
    mutate(
        # Add New Split to separate
        op_size_split_25 = ifelse(hives_winter > 25, "Imkereien mit über 25 Völker", "Hobby-Imkereien (<= 25 Völker)")
    ) %>%
    select(id, op_size_split_25, op_migratory_beekeeper:op_foreign_wax) %>%
    mutate(
        # Create Binary for our Network Cols
        across(-c("id", "op_size_split_25"), ~ ifelse(. == "Ja", 1, 0))
    ) %>%
    add_count(op_size_split_25, name = "total_count") %>%
    # Dummy count for later
    pivot_longer(-c(id, total_count, op_size_split_25)) %>%
    group_by(name, op_size_split_25) %>%
    mutate(
        # represent relative amount of answers for col (relative to each operation size)
        answers = sum(value, na.rm = TRUE) / first(total_count) * 100
    ) %>%
    ungroup() %>%
    # Only "Yes" is relevant for our Network
    filter(value == 1) %>%
    mutate(
        name = str_replace_all(name, opsDE)
    ) %>%
    glimpse()

# Temp names to selfjoin and build connections
tmp <- res15_OP_Network$data %>%
    rename(name2 = name) %>%
    select(id, name2)

# Testing
round(fCircleCoordinates(unique(tmp$name2))$x)


res15_OP_Network$connection <- res15_OP_Network$data %>%
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
    group_by(op_size_split_25, connection) %>%
    mutate(
        connection_percent = n() / first(total_count) * 100
    ) %>%
    arrange(desc(connection_percent)) %>%
    distinct(name, connection, .keep_all = TRUE) %>%
    mutate(
        print_name = glue::glue("{name}\n{fPrettyNum(round(connection_percent,1))}%"),
        print_name = forcats::fct_reorder(print_name, sort(print_name))
    ) %>%
    glimpse()


res15_OP_Network$p <- res15_OP_Network$connection %>%
    ggplot(
        aes(x, y, label = print_name, group = connection, size = answers)
    ) +
    geom_line(
        aes(size = connection_percent, alpha = connection_percent),
        color = colorBlindBlack8[3]
        # alpha = 0.5,
        # size = 2
    ) +
    geom_point(show.legend = FALSE) +
    geom_text(
        aes(
            x = ifelse(x > 0, x + 0.6, ifelse(near(round(x), 0), x, x - 0.6)),
            y = ifelse(y < 0, y - 0.8, y + 0.8)
        ),
        size = 3,
        show.legend = FALSE,
        check_overlap = TRUE,
        nudge_y = 0.02
    ) +
    scale_color_viridis_c(
        "Kombination [%]",
        option = "inferno",
        direction = -1,
        # breaks = seq(0, 100, 10),
        # limits = c(1, 100)
    ) +
    scale_size(
        "Häufigkeit der Kombination [%]",
        limits = c(0.1, 100),
    ) +
    xlab("") +
    ylab("") +
    ggplot2::coord_fixed(clip = "off") +
    facet_wrap(~op_size_split_25) +
    ggplot2::guides(alpha = "none") +
    ggplot2::theme(
        legend.position = "bottom",
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        # extend margin between facets and also on the right side because of missing text
        panel.spacing = unit(2, "lines"),
        plot.margin = margin(r = 12, unit = "pt")
    )

fSaveImages("15_Operational_Network", res15_OP_Network$p, w = 9, h = 6)