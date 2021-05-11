# df = Dataset
# f = string of factor
# returns tibble
fGlmNullModel <- function(df, f) {
    message(glue::glue("fGlmNullModel on {f}"))
    myvar <- sym(f) # convert string to symbol to use it with rlang
    # create count table
    result <- df %>%
        add_count(year, name = "totalN") %>%
        group_by(year, {{ myvar }}) %>%
        summarise(
            n = n(),
            np = fNumberFormat(n() * 100 / totalN[[1]]),
            control_rate = as.numeric(format(round(
                (sum(hives_lost_e) / sum(hives_winter) * 100), 1
            ), nsmall = 2)),
            .groups = "drop"
        ) %>%
        mutate(
            # mutate it to character for joining, as factor does not work
            !!myvar := as.character({{ myvar }})
        )

    # If multiple years we generate here a the results for it
    # we don't use map here because its easier to read inside the loop
    yearRes <- list()
    for (i in unique(df$year)) {
        yearDf <- df %>%
            filter(year == i) %>%
            mutate(
                !!myvar := fct_drop({{ myvar }})
            )
        farRes <- list()
        for (j in unique(yearDf %>% pull({{ myvar }}))) {
            farDf <- yearDf %>%
                filter({{ myvar }} == j)
            resGLM <- glm(
                as.formula(glue(
                    "cbind(hives_lost_e, hives_spring_e)~1"
                )),
                family = quasibinomial(link = "logit"),
                data = farDf, na.action = na.omit
            )

            resMean <- resGLM$fitted.values[1]
            resDF <- resGLM$df.residual
            resSE <- summary(resGLM)$coefficients[, 2]
            resCI <- boot::inv.logit(coef(resGLM) + c(-1, 1) * qt(0.975, df = resDF) * resSE)

            resMean <- fNumberFormat(resMean * 100)
            resCI <- fNumberFormat(resCI * 100)

            farRes[[j]] <- tibble(
                year    = i,
                !!myvar := j,
                middle  = resMean,
                lower   = resCI[1],
                upper   = resCI[2]
            )
        }
        farResAll <- dplyr::bind_rows(farRes)
        # Generate Matrix of possible non CI Overlaps
        chiMatrix <- outer(farResAll$lower, farResAll$upper, ">")
        # https://stackoverflow.com/questions/9505849/r-how-to-get-row-and-column-names-of-the-true-elements-of-a-matrix
        # Transform Matrix into Vector of Strings
        rownames(chiMatrix) <- unique(farResAll %>% pull({{ myvar }}))
        colnames(chiMatrix) <- unique(farResAll %>% pull({{ myvar }}))
        chiMatrix <- t(t(apply(chiMatrix, 1, function(u) paste(names(which(u)), collapse = ","))))
        farResAll$chistar <- as.vector(chiMatrix)
        yearRes[[i]] <- farResAll
        # yearRes[[i]] <- yearRes[[i]] %>%
        #    mutate(
        #        chistar = any(outer(lower, upper, ">"))
        #    )
    }
    result <- result %>%
        left_join(bind_rows(yearRes), by = c("year", as.character(myvar)))
    return(result)
}