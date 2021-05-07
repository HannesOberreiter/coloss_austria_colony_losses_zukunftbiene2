# df = Dataset
# f = string of factor
# returns tibble
fGlm <- function(df, f) {
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

    if (f != "global") {
      resGLM <- glm(
        as.formula(glue(
          "cbind(hives_lost_e, hives_spring_e)~{myvar}"
        )),
        family = quasibinomial(link = "logit"),
        data = yearDf, na.action = na.omit
      )
    } else {
      resGLM <- glm(
        as.formula(glue(
          "cbind(hives_lost_e, hives_spring_e)~1"
        )),
        family = quasibinomial(link = "logit"),
        data = yearDf, na.action = na.omit
      )
    }


    # sumGLM   <- summary(resGLM)
    # We do a ChiSq Statistical Test if two factors are given
    resANOVA <- anova(resGLM, test = "Chisq")
    chistar <- FALSE
    if (nrow(resANOVA) > 1 & f != "global") {
      if (resANOVA[[5]][2] < 0.05) {
        chistar <- TRUE
      }
    }

    # Calculate odds via model for factors
    prePredict <- predict(
      resGLM,
      tibble(!!myvar := as.factor(levels(yearDf %>% pull({{ myvar }})))),
      type = "link",
      se.fit = T
    )
    CACHE.ODDS <- prePredict$fit
    CACHE.LOWERLIM <- prePredict$fit - qt(0.975, df = resGLM$df.residual) * prePredict$se.fit
    CACHE.UPPERLIM <- prePredict$fit + qt(0.975, df = resGLM$df.residual) * prePredict$se.fit

    # Add Prob. to our plot matrix
    CACHE.ODDS <- fNumberFormat(inv.logit(CACHE.ODDS) * 100)
    CACHE.LOWERLIM <- fNumberFormat(inv.logit(CACHE.LOWERLIM) * 100)
    CACHE.UPPERLIM <- fNumberFormat(inv.logit(CACHE.UPPERLIM) * 100)
    yearRes[[i]] <- tibble(
      year    = i,
      !!myvar := levels(yearDf %>% pull({{ myvar }})) %>% as.character(),
      middle  = CACHE.ODDS,
      lower   = CACHE.LOWERLIM,
      upper   = CACHE.UPPERLIM,
      chistar = chistar
    ) %>%
      mutate(
        # mutate it to character for joining, as factor does not work
        !!myvar := as.character({{ myvar }})
      )
  }
  result <- result %>%
    left_join(bind_rows(yearRes), by = c("year", as.character(myvar)))
  return(result)
}