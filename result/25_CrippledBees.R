res25_CrippledBees <- list()
myFactor <- "crippled_bees"

factorOrder <- c("Häufig", "Wenig", "Überhaupt nicht", "Weiß nicht", "keine Angaben")
factorOrderBreak <- factorOrder %>% str_replace_all(" ", "\n")
tempData <- dfData %>%
    filter(submitted != "Zeitung") %>%
    mutate(
        crippled_bees = crippled_bees %>% replace_na("keine Angaben") %>% str_replace_all(" ", "\n"),
        crippled_bees = crippled_bees %>% forcats::fct_relevel(factorOrderBreak)
    )

res25_CrippledBees$result <- fGlmNullModel(tempData, myFactor) %>%
    mutate(
        crippled_bees = crippled_bees %>% forcats::fct_relevel(factorOrderBreak)
    )
res25_CrippledBees$chi <- fChistar(res25_CrippledBees$result, myFactor, dropNoAnswer = TRUE)

# Hard Coded one sign. bar to keep my sanity :/
res25_CrippledBees$chi$y[2] <- 55
res25_CrippledBees$chi$y[3] <- 45
res25_CrippledBees$chi$y[6] <- 25
res25_CrippledBees$chi$y[11] <- 50

res25_CrippledBees$p <- fPlot(
    res25_CrippledBees$result,
    res25_CrippledBees$chi,
    myFactor,
    dropNoAnswer = TRUE,
    allData = TRUE,
    raw = tempData %>% filter(!!sym(myFactor) != "keine\nAngaben")
)

fSaveImages("25_CrippledBees", res25_CrippledBees$p, w = 8.5)
caption <- "Höhe der Winterverluste in Prozent ($\\pm$95\\%CI) und Anzahl der Antworten in Abhängigkeit von der Angabe wie häufig während der Sammelsaison Bienen mit verkrüppelten Flügeln in den Völkern bemerkt wurden über die Umfragejahre 2017/18 - 2020/21."

res25_CrippledBees$result %>%
    mutate(
        crippled_bees = crippled_bees %>% str_replace_all("\n", " ")
    ) %>%
    fSaveTable("25_CrippledBees", ., caption, myFactor, "u:25CrippledBees")