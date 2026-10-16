# load required packages
library(tidyverse)
library(msmdata)
library(scales)
library(ggrepel)
library(sf)

# load in the required datasets
# downtown_census_comp = read_csv("./sdsc-data/outputs/census/downtown_census_comparison.csv")
# cma_census_comp = read_csv("./sdsc-data/outputs/census/cma_census_comparison.csv")
downtown_vs_cma_comp = read_csv("./sdsc-data/outputs/census/downtown_vs_cma_comparison.csv")
downtown_boundaries = arrow::read_parquet("./sdsc-data/data/downtowns.parquet")

# join DT_TYPE
downtown_boundaries = downtown_boundaries %>%
    sf::st_drop_geometry() %>%
    select(DTNAME, DT_TYPE)

downtown_vs_cma_comp = downtown_vs_cma_comp %>%
    left_join(downtown_boundaries, by = "DTNAME") %>%
    mutate(DT_TYPE = case_when(
        DT_TYPE == 2 ~ "Secondary",
        TRUE ~ "Primary"
        )
    )

## Pre-step: classify downtowns in population buckets based on 2001 CMA population
cma_groups = downtown_vs_cma_comp %>%
    filter(variable_name == "Population") %>%
    transmute(
        DTNAME,
        cma_group = case_when(
            value_2001_cma < 50000 ~ "Less than 50,000",
            value_2001_cma < 100000 ~ "Between 50,000 - 100,000",
            value_2001_cma < 250000 ~ "Between 100,000 - 250,000",
            value_2001_cma < 500000 ~ "Between 250,000 - 500,000",
            value_2001_cma < 1000000 ~ "Between 500,000 - 1,000,000",
            value_2001_cma > 1000000 ~ "Above 1,000,000"
        )
    )

downtown_vs_cma_comp = downtown_vs_cma_comp %>%
    inner_join(cma_groups, by = "DTNAME")

downtown_vs_cma_comp = downtown_vs_cma_comp %>%
    filter(cma_group != "Less than 50,000")

# arrange the final columns
downtown_vs_cma_comp = downtown_vs_cma_comp %>%
    select(DTNAME, DT_TYPE, cma_group, everything())

# Group order and colours used in all charts
cma_group_levels = c(
    "Between 50,000 - 100,000",
    "Between 100,000 - 250,000",
    "Between 250,000 - 500,000",
    "Between 500,000 - 1,000,000",
    "Above 1,000,000"
)

cma_group_colours = setNames(
    c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4"),
    cma_group_levels
)


## THEME 1 - WHICH DOWNTOWNS ARE GROWING AND BY HOW MUCH

# Total Population Count of Downtowns
dt_population = downtown_vs_cma_comp %>%
    filter(indicator == "Population" & DT_TYPE == "Primary")

# Comparing Size
cma_size_dt_growth = dt_population %>%
    summarise(
        downtown_count = n(),
        change_dt = sum(change_dt),
        .by = cma_group
    )

cma_size_plot = cma_size_dt_growth %>%
    mutate(
        cma_group = factor(cma_group, levels = cma_group_levels)
    ) %>%
    ggplot(aes(x = cma_group, y = change_dt)) +
    geom_col(fill = "#2a78d6", width = 0.6) +
    # change_dt label at the end of the bar
    geom_text(
        aes(
            label = comma(change_dt, style_positive = "plus"),
            vjust = ifelse(change_dt >= 0, -0.5, 1.5)
        ),
        colour = "#0b0b0b",
        size = 3.8,
        fontface = "bold"
    ) +
    # downtown count inside the bar
    geom_text(
        aes(
            y = change_dt / 2,
            label = paste0("n = ", downtown_count)
        ),
        colour = "white",
        size = 3.5
    ) +
    geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.3) +
    scale_x_discrete(labels = label_wrap(14)) +
    scale_y_continuous(
        labels = comma,
        expand = expansion(mult = c(0.08, 0.12))
    ) +
    labs(
        title = "Downtown population change by 2001 CMA size",
        x = "CMA population group (2001)",
        y = "Change in downtown population",
        caption = "n = number of downtowns in each group"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
    )

ggsave(
    "./sdsc-data/outputs/charts/cma_size_dt_growth.png",
    plot = cma_size_plot,
    width = 7,
    height = 4.5,
    dpi = 300,
    bg = "white"
)

dt_population = downtown_vs_cma_comp %>%
    filter(indicator == "Population")

dt_cma_growth_plot = dt_population %>%
    filter(
        between(pct_change_dt, -150, 150),
        between(pct_change_cma, -150, 150)
    ) %>%
    mutate(
        cma_group = factor(cma_group, levels = cma_group_levels)
    ) %>%
    ggplot(aes(x = pct_change_cma, y = pct_change_dt, fill = cma_group)) +
    geom_hline(yintercept = 0, colour = "#c3c2b7", linewidth = 0.8) +
    geom_vline(xintercept = 0, colour = "#c3c2b7", linewidth = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#52514e", linewidth = 0.5) +
    geom_point(shape = 21, size = 3, colour = "white", stroke = 0.6, alpha = 0.9) +
    # labels only where there's room
    geom_text_repel(
        aes(label = DTNAME),
        size = 2.8,
        colour = "#52514e",
        max.overlaps = 4,          # lower = fewer labels in busy areas
        min.segment.length = 0.3,
        segment.colour = "#c3c2b7",
        segment.size = 0.3,
        box.padding = 0.3,
        seed = 42                  # same label placement every run
    ) +
    scale_fill_manual(values = cma_group_colours) +
    scale_x_continuous(labels = label_number(suffix = "%")) +
    scale_y_continuous(labels = label_number(suffix = "%")) +
    labs(
        title = "Downtown vs. CMA population growth, 2001–2021",
        subtitle = "Points above the dashed line: downtown grew faster than its CMA",
        x = "CMA population change (%)",
        y = "Downtown population change (%)",
        fill = "2001 CMA population"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top"
    )

ggsave(
    "./sdsc-data/outputs/charts/dt_vs_cma_growth_scatter.png",
    plot = dt_cma_growth_plot,
    width = 12,
    height = 8,
    dpi = 300,
    bg = "white"
)


## THEME 2 - PROFILE OF THE POPULATION

# Age differences between downtown and cma
dt_age = downtown_vs_cma_comp %>%
    filter(indicator == "Age" & variable_name != "Total Age")

dt_age_sum = dt_age %>%
    summarise(
        composition_gap_2021 = mean(composition_gap_2021), .by = variable_name
    )


gap_limit = max(abs(dt_age_sum$composition_gap_2021), na.rm = TRUE) * 1.15

dt_age_sum_plot = dt_age_sum %>%
    mutate(
        more_prevalent = if_else(composition_gap_2021 >= 0, "Downtown", "CMA"),
        variable_name = fct_rev(fct_inorder(variable_name))
    ) %>%
    ggplot(aes(x = composition_gap_2021, y = variable_name, fill = more_prevalent)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = "#52514e", linewidth = 0.4) +
    geom_text(
        aes(
            label = sprintf("%.1f", abs(composition_gap_2021)),
            hjust = if_else(composition_gap_2021 >= 0, -0.2, 1.2)
        ),
        colour = "#52514e",
        size = 3
    ) +
    scale_x_continuous(
        limits = c(-gap_limit, gap_limit),
        labels = function(x) paste0(abs(x), " pp"),
        expand = expansion(mult = 0)
    ) +
    scale_fill_manual(
        values = c("CMA" = "#eb6834", "Downtown" = "#2a78d6"),
        breaks = c("CMA", "Downtown"),
        labels = c("Higher share in CMA", "Higher share downtown")
    ) +
    labs(
        title = "Age profile: downtown vs. CMA, 2021",
        subtitle = "Average gap in share of population across downtowns, in percentage points",
        x = "← More prevalent in CMA          More prevalent downtown →",
        y = NULL,
        fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top"
    )

ggsave(
    "./sdsc-data/outputs/charts/dt_age_gap.png",
    plot = dt_age_sum_plot,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
)

# young adults
dt_age = downtown_vs_cma_comp %>%
    filter(variable_name %in% c("Age 20 to 24", "Age 25 to 29", "Age 30 to 34") & DT_TYPE == "Primary")

dt_young_adults = dt_age %>%
    summarise(
        across(
            c(percent_2001_dt, percent_2021_dt, percent_point_change_dt,
              percent_2001_cma, percent_2021_cma, percent_point_change_cma,
              composition_gap_2021, composition_gap_change),
            ~ sum(.x, na.rm = TRUE)
        ),
        .by = c(DTNAME, cma_group)
    )

dt_young_adults = dt_young_adults %>%
    summarise(
        across(
            c(percent_2001_dt, percent_2021_dt, percent_point_change_dt,
              percent_2001_cma, percent_2021_cma, percent_point_change_cma,
              composition_gap_2021, composition_gap_change),
            ~ mean(.x, na.rm = TRUE)
        ),
        .by = c(cma_group)
    )

dt_young_adults_plot = dt_young_adults %>%
    mutate(
        cma_group = factor(cma_group, levels = cma_group_levels)
    ) %>%
    ggplot(aes(x = cma_group, y = percent_2021_dt, fill = cma_group)) +
    geom_col(width = 0.6) +
    geom_text(
        aes(label = sprintf("%.1f%%", percent_2021_dt)),
        vjust = -0.5,
        colour = "#0b0b0b",
        size = 3.8,
        fontface = "bold"
    ) +
    scale_fill_manual(values = cma_group_colours) +
    scale_x_discrete(labels = label_wrap(14)) +
    scale_y_continuous(
        labels = label_number(suffix = "%"),
        expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
        title = "Young adults (20–34) as a share of downtown population, 2021 (Primary Downtowns)",
        subtitle = "Average across downtowns, by 2001 CMA population",
        x = "CMA population group (2001)",
        y = "Share of downtown population"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "none"
    )

ggsave(
    "./sdsc-data/outputs/charts/dt_young_adults_by_cma_group.png",
    plot = dt_young_adults_plot,
    width = 12,
    height = 6,
    dpi = 300,
    bg = "white"
)

# HOUSEHOLD SIZE
dt_hh_size = downtown_vs_cma_comp %>%
    filter(indicator == "Household Size" & variable_name != "Total Private Occupied Dwellings")


dt_hh_size_sum = dt_hh_size %>%
    summarise(
        composition_gap_2021 = mean(composition_gap_2021), .by = variable_name
    )

hh_gap_limit = max(abs(dt_hh_size_sum$composition_gap_2021), na.rm = TRUE) * 1.15

dt_hh_size_plot = dt_hh_size_sum %>%
    mutate(
        more_prevalent = if_else(composition_gap_2021 >= 0, "Downtown", "CMA"),
        variable_name = fct_rev(fct_inorder(variable_name))
    ) %>%
    ggplot(aes(x = composition_gap_2021, y = variable_name, fill = more_prevalent)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = "#52514e", linewidth = 0.4) +
    geom_text(
        aes(
            label = sprintf("%.1f", abs(composition_gap_2021)),
            hjust = if_else(composition_gap_2021 >= 0, -0.2, 1.2)
        ),
        colour = "#52514e",
        size = 3
    ) +
    scale_x_continuous(
        limits = c(-hh_gap_limit, hh_gap_limit),
        labels = function(x) paste0(abs(x), " pp"),
        expand = expansion(mult = 0)
    ) +
    scale_fill_manual(
        values = c("CMA" = "#eb6834", "Downtown" = "#2a78d6"),
        breaks = c("CMA", "Downtown"),
        labels = c("Higher share in CMA", "Higher share downtown")
    ) +
    labs(
        title = "Household size: downtown vs. CMA, 2021",
        subtitle = "Average gap in share of private households across downtowns, in percentage points",
        x = "← More prevalent in CMA          More prevalent downtown →",
        y = NULL,
        fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top"
    )

ggsave(
    "./sdsc-data/outputs/charts/dt_hh_size_gap.png",
    plot = dt_hh_size_plot,
    width = 8,
    height = 4,
    dpi = 300,
    bg = "white"
)


dt_sphh = dt_hh_size %>%
    filter(variable_name == "Household Size: 1 person" & DT_TYPE == "Primary")


dt_sphh = dt_sphh %>%
    summarise(
        across(
            c(percent_2001_dt, percent_2021_dt, percent_point_change_dt,
              percent_2001_cma, percent_2021_cma, percent_point_change_cma,
              composition_gap_2021, composition_gap_change),
            ~ mean(.x, na.rm = TRUE)
        ),
        .by = c(cma_group)
    )

dt_sphh_plot = dt_sphh %>%
    mutate(
        cma_group = factor(cma_group, levels = cma_group_levels)
    ) %>%
    ggplot(aes(x = cma_group, y = percent_2021_dt, fill = cma_group)) +
    geom_col(width = 0.6) +
    geom_text(
        aes(label = sprintf("%.1f%%", percent_2021_dt)),
        vjust = -0.5,
        colour = "#0b0b0b",
        size = 3.8,
        fontface = "bold"
    ) +
    scale_fill_manual(values = cma_group_colours) +
    scale_x_discrete(labels = label_wrap(14)) +
    scale_y_continuous(
        labels = label_number(suffix = "%"),
        expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
        title = "One-person households as a share of downtown households, 2021 (Primary Downtowns)",
        subtitle = "Average across downtowns, by 2001 CMA population",
        x = "CMA population group (2001)",
        y = "Share of downtown private households"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "none"
    )

ggsave(
    "./sdsc-data/outputs/charts/dt_sphh_by_cma_group.png",
    plot = dt_sphh_plot,
    width = 12,
    height = 6,
    dpi = 300,
    bg = "white"
)


# RESIDENT JOB OCCUPATION

dt_occupation = downtown_vs_cma_comp %>%
    filter(
        indicator == "Occupation", 
        !variable_name %in% c("Total Occupations", "Legislation and Senior Management")
    )

# Average gap between downtown and CMA occupation shares
dt_occupation_sum = dt_occupation %>%
    summarise(
        composition_gap_2021 = mean(composition_gap_2021, na.rm = TRUE),
        .by = variable_name
    )

occ_gap_limit = max(abs(dt_occupation_sum$composition_gap_2021), na.rm = TRUE) * 1.15

dt_occupation_plot = dt_occupation_sum %>%
    mutate(
        more_prevalent = if_else(composition_gap_2021 >= 0, "Downtown", "CMA"),
        variable_name = fct_reorder(variable_name, composition_gap_2021)
    ) %>%
    ggplot(aes(x = composition_gap_2021, y = variable_name, fill = more_prevalent)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = "#52514e", linewidth = 0.4) +
    geom_text(
        aes(
            label = sprintf("%.1f", abs(composition_gap_2021)),
            hjust = if_else(composition_gap_2021 >= 0, -0.2, 1.2)
        ),
        colour = "#52514e",
        size = 3
    ) +
    scale_x_continuous(
        limits = c(-occ_gap_limit, occ_gap_limit),
        labels = function(x) paste0(abs(x), " pp"),
        expand = expansion(mult = 0)
    ) +
    scale_y_discrete(labels = label_wrap(30)) +
    scale_fill_manual(
        values = c("CMA" = "#eb6834", "Downtown" = "#2a78d6"),
        breaks = c("CMA", "Downtown"),
        labels = c("Higher share in CMA", "Higher share downtown")
    ) +
    labs(
        title = "Occupation: downtown vs. CMA, 2021",
        subtitle = "Average gap in share of employed residents across downtowns, in percentage points",
        x = "← More prevalent in CMA          More prevalent downtown →",
        y = NULL,
        fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top"
    )

ggsave(
    "./sdsc-data/outputs/charts/dt_occupation_gap.png",
    plot = dt_occupation_plot,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
)


# EMPLOYMENT INCOME

employment_income = downtown_vs_cma_comp %>%
    filter(variable_name == "Average Employment Income FTFY")





