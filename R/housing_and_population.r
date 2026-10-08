# load required packages
library(tidyverse)
library(msmdata)

# load in the required datasets
downtown_census_comp = read_csv("./sdsc-data/outputs/census/downtown_census_comparison.csv")
cma_census_comp = read_csv("./sdsc-data/outputs/census/cma_census_comparison.csv")
downtown_vs_cma_comp = read_csv("./sdsc-data/outputs/census/downtown_vs_cma_comparison.csv")


## THEME 1 - WHICH DOWNTOWNS ARE GROWING AND BY HOW MUCH

# This answers the question of which downtowns are growing over the last 20 years and by how much

downtown_cma_pop_growth = downtown_vs_cma_comp %>%
    filter(variable_name == "Population" & value_2021_cma > 50000 & value_2001_dt > 11000) %>%
    # bucket 2001 CMA population for point size
    mutate(cma_size_2001 = cut(value_2001_cma,
                               breaks = c(-Inf, 100000, 1000000, Inf),
                               labels = c("Less than 100,000", "100,000 - 999,999",
                                          "Over 1,000,000"),
                               right = FALSE))

# Quadrant chart: downtown vs CMA population growth (2001-2021)
# Points above the dashed line = downtown grew faster than its CMA
pop_growth_quadrant = ggplot(downtown_cma_pop_growth,
                             aes(x = pct_change_cma, y = pct_change_dt)) +
    # quadrant dividers at 0% growth
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
    # slope-1 reference line: downtown growth = CMA growth
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(aes(size = cma_size_2001), colour = "#2a6f97", alpha = 0.7) +
    geom_text(aes(label = DTNAME), size = 2.8, vjust = -1, check_overlap = TRUE) +
    scale_size_manual(name = "CMA population (2001)",
                      values = c(2, 4, 8),
                      drop = FALSE, na.translate = FALSE) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
        title = "Downtown vs. CMA population growth, 2001-2021",
        x = "CMA population change (%)",
        y = "Downtown population change (%)"
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom",
          # square panel; each axis keeps its own range so points spread out
          aspect.ratio = 1)

# VS Code's httpgd plot viewer fails with systemfonts < 1.3.0, so draw on other devices:
# save to file with base R's png device
ggsave("./sdsc-data/outputs/charts/pop_growth_quadrant.png", pop_growth_quadrant,
       device = png, width = 9, height = 10, units = "in", dpi = 300, bg = "white")

# view in a native Windows plot window instead of the VS Code viewer
if (interactive()) {
    windows(width = 9, height = 10)
    print(pop_growth_quadrant)
}

# Bar chart: growth gap (downtown % change minus CMA % change, 2001-2021)
# Bars above 0 = downtown outpaced its CMA; below 0 = downtown lagged its CMA
growth_gap_data = downtown_cma_pop_growth %>%
    filter(!is.na(growth_gap)) %>%
    mutate(gap_direction = if_else(growth_gap >= 0,
                                   "Downtown grew faster than CMA",
                                   "Downtown grew slower than CMA"),
           DTNAME = fct_reorder(DTNAME, growth_gap))

growth_gap_bars = ggplot(growth_gap_data,
                         aes(x = DTNAME, y = growth_gap, fill = gap_direction)) +
    geom_col(width = 0.8) +
    # central axis at 0
    geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.5) +
    scale_fill_manual(name = NULL,
                      values = c("Downtown grew faster than CMA" = "#2e8b57",
                                 "Downtown grew slower than CMA" = "#c0392b")) +
    scale_y_continuous(labels = function(x) paste0(x, " pts")) +
    labs(
        title = "Downtown growth gap vs. CMA, 2001-2021",
        x = NULL,
        y = "Growth gap (downtown % change minus CMA % change)"
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
          legend.position = "top")

ggsave("./sdsc-data/outputs/charts/growth_gap_bars.png", growth_gap_bars,
       device = png, width = 12, height = 7, units = "in", dpi = 300, bg = "white")

if (interactive()) {
    windows(width = 12, height = 7)
    print(growth_gap_bars)
}


## THEME 2 - IMMIGRATION and MIGRATION

dt_immigration_migration = downtown_census_comp %>%
    filter(indicator == "Mobility")

