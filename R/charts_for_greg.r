library(tidyverse)
library(msmdata)
library(gt)

# load in the required datasets
downtown_vs_cma_comp = read_csv("./sdsc-data/outputs/census/downtown_vs_cma_comparison.csv")
cma = read_csv("./sdsc-data/outputs/census/cma_census_comparison.csv")

widen_breakdown <- function(df, ..., total_pattern = "^Total") {
  df |>
    filter(!str_detect(variable_name, total_pattern)) |>
    select(DTNAME, variable_name, ...) |>
    pivot_wider(
      names_from  = variable_name,
      values_from = !c(DTNAME, variable_name),
      names_glue  = "{variable_name}__{.value}",
      names_vary  = "slowest"   # keeps each category's columns together
    )
}

assign_province = function(df){
    df %>%
        mutate(
            
            prcode = str_sub(CMAPUID, 1, 2),
            Province = case_when(
                prcode == 10 ~ "Newfoundland and Labrador",
                prcode == 11 ~ "Prince Edward Island",
                prcode == 12 ~ "Nova Scotia",
                prcode == 13 ~ "New Brunswick",
                prcode == 24 ~ "Quebec",
                prcode == 35 ~ "Ontario",
                prcode == 46 ~ "Manitoba",
                prcode == 47 ~ "Saskatchewan",
                prcode == 48 ~ "Alberta",
                prcode == 59 ~ "British Columbia",
                prcode == 60 ~ "Yukon",
                prcode == 61 ~ "Northwest Territories",
                prcode == 62 ~ "Nunavut"
        )) %>%
        select(DTNAME, Province, everything(), -prcode)
}

## THEME 1 GROWTH POPULATION and DWELLINGS

# total
downtown_population_meta = downtown_vs_cma_comp %>%
    filter(indicator == "Population") %>%
    select(DTNAME, CMAPUID) %>%
    assign_province()

# downtown counts
downtown_population_count = downtown_vs_cma_comp %>%
    filter(indicator == "Population") %>%
    widen_breakdown(Downtown_2001_count = value_2001_dt, Downtown_2021_count = value_2021_dt, Downtown_Population_Change = change_dt)

cma_population_count = downtown_vs_cma_comp %>%
    filter(indicator == "Population") %>%
    widen_breakdown(CMA_2001_count = value_2001_cma, CMA_2021_count = value_2021_cma, CMA_Population_Change = change_cma)

population_growth_change = downtown_vs_cma_comp %>%
    filter(indicator == "Population") %>%
    widen_breakdown(Downtown_pct_change = pct_change_dt, CMA_pct_change = pct_change_cma, Growth_Gap = growth_gap)


tables = list(downtown_population_meta, downtown_population_count, cma_population_count, population_growth_change)

pop_joined = reduce(tables, left_join, by = "DTNAME")
pop_joined = pop_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(pop_joined, "./sdsc-data/outputs/initialsheets/population.csv")

rm(downtown_population_meta, downtown_population_count, cma_population_count, population_growth_change)



# Dwellings

downtown_dwelling_meta = downtown_vs_cma_comp %>%
    filter(indicator == "Dwellings") %>%
    select(DTNAME, CMAPUID) %>%
    assign_province()

# downtown counts
downtown_dwelling_count = downtown_vs_cma_comp %>%
    filter(indicator == "Dwellings") %>%
    widen_breakdown(Downtown_2001_count = value_2001_dt, Downtown_2021_count = value_2021_dt, Downtown_Dwelling_Change = change_dt)

cma_dwelling_count = downtown_vs_cma_comp %>%
    filter(indicator == "Dwellings") %>%
    widen_breakdown(CMA_2001_count = value_2001_cma, CMA_2021_count = value_2021_cma, CMA_Dwelling_Change = change_cma)

dwelling_growth_change = downtown_vs_cma_comp %>%
    filter(indicator == "Dwellings") %>%
    widen_breakdown(Downtown_pct_change = pct_change_dt, CMA_pct_change = pct_change_cma, Growth_Gap = growth_gap)


tables = list(downtown_dwelling_meta, downtown_dwelling_count, cma_dwelling_count, dwelling_growth_change)

dwell_joined = reduce(tables, left_join, by = "DTNAME")
dwell_joined = dwell_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(dwell_joined, "./sdsc-data/outputs/initialsheets/dwelling.csv")

rm(downtown_dwelling_meta, downtown_dwelling_count, cma_dwelling_count, dwelling_growth_change)



## THEME 2 - IMMIGRATION AND MIGRATION IN DOWNTOWN

# total
downtown_mobility_meta = downtown_vs_cma_comp %>%
    filter(indicator == "Mobility" & variable_name == "Total Mobility Status 5 years ago") %>%
    select(DTNAME, CMAPUID) %>%
    assign_province()


# downtown count
downtown_mobility_count = downtown_vs_cma_comp %>%
    filter(indicator == "Mobility") %>%
    widen_breakdown(Downtown_2001_count = value_2001_dt, Downtown_2021_count = value_2021_dt, Downtown_Gap_count = change_dt)


# downtown percentage
downtown_mobility_per = downtown_vs_cma_comp %>%
    filter(indicator == "Mobility") %>%
    widen_breakdown(Downtown_2001_per = percent_2001_dt, Downtown_2021_per = percent_2021_dt, Downtown_Gap_per = percent_point_change_dt)

# cma percentage
cma_mobility_per = downtown_vs_cma_comp %>%
    filter(indicator == "Mobility") %>%
    widen_breakdown(CMA_2001_per = percent_2001_cma, CMA_2021_per = percent_2021_cma, CMA_Gap_per = percent_point_change_cma)


# downtown vs cma
mobility_growth_change = downtown_vs_cma_comp %>%
    filter(indicator == "Mobility") %>%
    widen_breakdown(Downtown_pct_change = pct_change_dt, CMA_pct_change = pct_change_cma, Growth_Gap = growth_gap)


tables = list(downtown_mobility_meta, downtown_mobility_count, downtown_mobility_per, cma_mobility_per, mobility_growth_change)

mobility_joined = reduce(tables, left_join, by = "DTNAME")
mobility_joined = mobility_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(mobility_joined, "./sdsc-data/outputs/initialsheets/mobility_and_immigration.csv")

rm(downtown_mobility_meta, downtown_mobility_count, downtown_mobility_per, cma_mobility_per, mobility_growth_change)



## THEME 3 - HOUSING SUPPLY 

# total and meta data
downtown_housing_meta = downtown_vs_cma_comp %>%
    filter(variable_name == "Total private dwellings by structural type") %>%
    select(DTNAME, CMAPUID) %>%
    assign_province()

# apartments
downtown_apartments = downtown_vs_cma_comp %>%
    filter(variable_name == "Apartment in a building that has five or more storeys") %>%
    widen_breakdown(Downtown_2001_units = value_2001_dt, Downtown_2021_units = value_2021_dt, Downtown_Gap_count = change_dt)

cma_apartments = downtown_vs_cma_comp %>%
    filter(variable_name == "Apartment in a building that has five or more storeys") %>%
    widen_breakdown(CMA_2001_units = value_2001_cma, CMA_2021_units = value_2021_cma, CMA_pct_change = change_cma)

growth_rate_apt = downtown_vs_cma_comp %>%
    filter(variable_name == "Apartment in a building that has five or more storeys") %>%
    widen_breakdown(Downtown_2021_growth = pct_change_dt, CMA_2021_growth = pct_change_cma, Growth_Gap = growth_gap)

# renters
downtown_renter = downtown_vs_cma_comp %>%
    filter(variable_name == "Renter") %>%
        widen_breakdown(Downtown_2001_units = value_2001_dt, Downtown_2021_units = value_2021_dt, Downtown_Gap_count = change_dt)

cma_renter = downtown_vs_cma_comp %>%
    filter(variable_name == "Renter") %>%
    widen_breakdown(CMA_2001_units = value_2001_cma, CMA_2021_units = value_2021_cma, CMA_pct_change = change_cma)

growth_rate_renter = downtown_vs_cma_comp %>%
    filter(variable_name == "Renter") %>%
    widen_breakdown(Downtown_2021_growth = pct_change_dt, CMA_2021_growth = pct_change_cma, Growth_Gap = growth_gap)

# built in last 10 years
downtown_built_last_10 = downtown_vs_cma_comp %>%
    filter(variable_name == "Built in previous 10 years") %>%
        widen_breakdown(Downtown_2001_units = value_2001_dt, Downtown_2021_units = value_2021_dt, Downtown_Gap_count = change_dt)

cma_built_last_10 = downtown_vs_cma_comp %>%
    filter(variable_name == "Built in previous 10 years") %>%
    widen_breakdown(CMA_2001_units = value_2001_cma, CMA_2021_units = value_2021_cma, CMA_pct_change = change_cma)

growth_rate_built_last_10 = downtown_vs_cma_comp %>%
    filter(variable_name == "Built in previous 10 years") %>%
    widen_breakdown(Downtown_2021_growth = pct_change_dt, CMA_2021_growth = pct_change_cma, Growth_Gap = growth_gap)

tables = list(downtown_housing_meta, downtown_apartments, cma_apartments, growth_rate_apt, downtown_renter, cma_renter, growth_rate_renter, downtown_built_last_10, cma_built_last_10, growth_rate_built_last_10)

housing_supply_joined = reduce(tables, left_join, by = "DTNAME")
housing_supply_joined = housing_supply_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(housing_supply_joined, "./sdsc-data/outputs/initialsheets/housing_supply.csv")

rm(downtown_housing_meta, downtown_apartments, cma_apartments, growth_rate_apt, downtown_renter, cma_renter, growth_rate_renter, downtown_built_last_10, cma_built_last_10, growth_rate_built_last_10)


## THEME 4 - HOUSEHOLD SIZE, AGE AND DEMOGRAPHICS

downtown_demo_meta = downtown_vs_cma_comp %>%
    filter(variable_name == "")