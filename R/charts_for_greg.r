library(tidyverse)
library(msmdata)
library(arrow)
library(gt)

# load in the required datasets
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
    ))

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
    select(DTNAME, DT_TYPE, cma_group, CMAPUID) %>%
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
    select(DTNAME, DT_TYPE, cma_group, CMAPUID) %>%
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
    select(DTNAME, DT_TYPE, cma_group, CMAPUID) %>%
    assign_province()

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
    widen_breakdown(Composition_Gap_2021 = composition_gap_2021)


tables = list(downtown_mobility_meta, downtown_mobility_per, cma_mobility_per, mobility_growth_change)

mobility_joined = reduce(tables, left_join, by = "DTNAME")
mobility_joined = mobility_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(mobility_joined, "./sdsc-data/outputs/initialsheets/mobility_and_immigration.csv")

rm(downtown_mobility_meta, downtown_mobility_per, cma_mobility_per, mobility_growth_change)



## THEME 3 - HOUSING SUPPLY 

# total and meta data
downtown_housing_meta = downtown_vs_cma_comp %>%
    filter(variable_name == "Total private dwellings by structural type") %>%
    select(DTNAME, DT_TYPE, cma_group, CMAPUID) %>%
    assign_province()

# apartments
downtown_apartments = downtown_vs_cma_comp %>%
    filter(variable_name == "Apartment in a building that has five or more storeys") %>%
    widen_breakdown(Downtown_2001_units = value_2001_dt, Downtown_2021_units = value_2021_dt, Downtown_unit_change = change_dt)

cma_apartments = downtown_vs_cma_comp %>%
    filter(variable_name == "Apartment in a building that has five or more storeys") %>%
    widen_breakdown(CMA_2001_units = value_2001_cma, CMA_2021_units = value_2021_cma, CMA_unit_change = change_cma)

growth_rate_apt = downtown_vs_cma_comp %>%
    filter(variable_name == "Apartment in a building that has five or more storeys") %>%
    widen_breakdown(Downtown_pct_change = pct_change_dt, CMA_pct_change = pct_change_cma, Growth_Gap = growth_gap)

# renters
downtown_renter = downtown_vs_cma_comp %>%
    filter(variable_name == "Renter") %>%
        widen_breakdown(Downtown_2001_units = value_2001_dt, Downtown_2021_units = value_2021_dt, Downtown_unit_change = change_dt)

cma_renter = downtown_vs_cma_comp %>%
    filter(variable_name == "Renter") %>%
    widen_breakdown(CMA_2001_units = value_2001_cma, CMA_2021_units = value_2021_cma, CMA_unit_change = change_cma)

growth_rate_renter = downtown_vs_cma_comp %>%
    filter(variable_name == "Renter") %>%
    widen_breakdown(Downtown_pct_change = pct_change_dt, CMA_pct_change = pct_change_cma, Growth_Gap = growth_gap)

# built in last 10 years
downtown_built_last_10 = downtown_vs_cma_comp %>%
    filter(variable_name == "Built in previous 10 years") %>%
        widen_breakdown(Downtown_2001_units = value_2001_dt, Downtown_2021_units = value_2021_dt, Downtown_unit_change = change_dt)

cma_built_last_10 = downtown_vs_cma_comp %>%
    filter(variable_name == "Built in previous 10 years") %>%
    widen_breakdown(CMA_2001_units = value_2001_cma, CMA_2021_units = value_2021_cma, CMA_unit_change = change_cma)

growth_rate_built_last_10 = downtown_vs_cma_comp %>%
    filter(variable_name == "Built in previous 10 years") %>%
    widen_breakdown(Downtown_pct_change = pct_change_dt, CMA_pct_change = pct_change_cma, Growth_Gap = growth_gap)

tables = list(downtown_housing_meta, downtown_apartments, cma_apartments, growth_rate_apt, downtown_renter, cma_renter, growth_rate_renter, downtown_built_last_10, cma_built_last_10, growth_rate_built_last_10)

housing_supply_joined = reduce(tables, left_join, by = "DTNAME")
housing_supply_joined = housing_supply_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(housing_supply_joined, "./sdsc-data/outputs/initialsheets/housing_supply.csv")

rm(downtown_housing_meta, downtown_apartments, cma_apartments, growth_rate_apt, downtown_renter, cma_renter, growth_rate_renter, downtown_built_last_10, cma_built_last_10, growth_rate_built_last_10)


## THEME 4 - HOUSEHOLD SIZE, AGE AND DEMOGRAPHICS

downtown_demo_meta = downtown_vs_cma_comp %>%
    filter(variable_name == "Total Age") %>%
    select(DTNAME, DT_TYPE, cma_group, CMAPUID) %>%
    assign_province()

# Age Breakdown
downtown_age_per = downtown_vs_cma_comp %>%
    filter(indicator == "Age") %>%
    widen_breakdown(Downtown_2001_per = percent_2001_dt, Downtown_2021_per = percent_2021_dt, Downtown_Gap_per = percent_point_change_dt)

cma_age_per = downtown_vs_cma_comp %>%
    filter(indicator == "Age") %>%
    widen_breakdown(CMA_2001_per = percent_2001_cma, CMA_2021_per = percent_2021_cma, CMA_Gap_per = percent_point_change_cma)

age_growth_change = downtown_vs_cma_comp %>%
    filter(indicator == "Age") %>%
    widen_breakdown(Composition_Gap_2021 = composition_gap_2021)

tables = list(downtown_demo_meta, downtown_age_per, cma_age_per, age_growth_change)

age_joined = reduce(tables, left_join, by = "DTNAME")
age_joined = age_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(age_joined, "./sdsc-data/outputs/initialsheets/age_breakdown.csv")

rm(age_growth_change, downtown_age_per, cma_age_per)


# Household Size
downtown_demo_meta = downtown_vs_cma_comp %>%
    filter(variable_name == "Total Private Occupied Dwellings") %>%
    select(DTNAME, DT_TYPE, cma_group, CMAPUID) %>%
    assign_province()

# Household Size: 1 Person
downtown_hh1p_per = downtown_vs_cma_comp %>%
    filter(variable_name == "Household Size: 1 person") %>%
    widen_breakdown(Downtown_2001_per = percent_2001_dt, Downtown_2021_per = percent_2021_dt, Downtown_Gap_per = percent_point_change_dt)

cma_hh1p_per = downtown_vs_cma_comp %>%
    filter(variable_name == "Household Size: 1 person") %>%
    widen_breakdown(CMA_2001_per = percent_2001_cma, CMA_2021_per = percent_2021_cma, CMA_Gap_per = percent_point_change_cma)

hh1p_growth_change = downtown_vs_cma_comp %>%
    filter(variable_name == "Household Size: 1 person") %>%
    widen_breakdown(Composition_Gap_2021 = composition_gap_2021)

tables = list(downtown_demo_meta, downtown_hh1p_per, cma_hh1p_per, hh1p_growth_change)

hh1p_joined = reduce(tables, left_join, by = "DTNAME")
hh1p_joined = hh1p_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(hh1p_joined, "./sdsc-data/outputs/initialsheets/hh_size_one_person_breakdown.csv")

rm(downtown_demo_meta, downtown_hh1p_per, cma_hh1p_per, hh1p_growth_change)

# Employment Income
downtown_demo_meta = downtown_vs_cma_comp %>%
    filter(variable_name == "FTFY employment income recipients") %>%
    select(DTNAME, DT_TYPE, cma_group, CMAPUID) %>%
    assign_province()

employment_income = downtown_vs_cma_comp %>%
    filter(variable_name == "Average Employment Income FTFY") %>%
    widen_breakdown(Downtown_2021 = value_2021_dt, CMA_2021 = value_2021_cma)

tables = list(downtown_demo_meta, employment_income)

income_joined = reduce(tables, left_join, by = "DTNAME")
income_joined = income_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(income_joined, "./sdsc-data/outputs/initialsheets/average_employment_income.csv")

rm(downtown_demo_meta, employment_income)


# Visible Minorities
downtown_demo_meta = downtown_vs_cma_comp %>%
    filter(variable_name == "Total Visible Minority") %>%
    select(DTNAME, DT_TYPE, cma_group, CMAPUID) %>%
    assign_province()

downtown_vm_per = downtown_vs_cma_comp %>%
    filter(variable_name == "Visible Minorities") %>%
    widen_breakdown(Downtown_2001_per = percent_2001_dt, Downtown_2021_per = percent_2021_dt, Downtown_Gap_per = percent_point_change_dt)

cma_vm_per = downtown_vs_cma_comp %>%
    filter(variable_name == "Visible Minorities") %>%
    widen_breakdown(CMA_2001_per = percent_2001_cma, CMA_2021_per = percent_2021_cma, CMA_Gap_per = percent_point_change_cma)

vm_growth_change = downtown_vs_cma_comp %>%
    filter(variable_name == "Visible Minorities") %>%
    widen_breakdown(Composition_Gap_2021 = composition_gap_2021)

tables = list(downtown_demo_meta, downtown_vm_per, cma_vm_per, vm_growth_change)

vm_joined = reduce(tables, left_join, by = "DTNAME")
vm_joined = vm_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(vm_joined, "./sdsc-data/outputs/initialsheets/visible_minority_breakdown.csv")

rm(downtown_demo_meta, downtown_vm_per, cma_vm_per, vm_growth_change)


# First Generation Canadian
downtown_demo_meta = downtown_vs_cma_comp %>%
    filter(variable_name == "Total Generation status") %>%
    select(DTNAME, DT_TYPE, cma_group, CMAPUID) %>%
    assign_province()

downtown_fgc_per = downtown_vs_cma_comp %>%
    filter(variable_name == "First generation") %>%
    widen_breakdown(Downtown_2001_per = percent_2001_dt, Downtown_2021_per = percent_2021_dt, Downtown_Gap_per = percent_point_change_dt)

cma_fgc_per = downtown_vs_cma_comp %>%
    filter(variable_name == "First generation") %>%
    widen_breakdown(CMA_2001_per = percent_2001_cma, CMA_2021_per = percent_2021_cma, CMA_Gap_per = percent_point_change_cma)

fgc_growth_change = downtown_vs_cma_comp %>%
    filter(variable_name == "First generation") %>%
    widen_breakdown(Composition_Gap_2021 = composition_gap_2021)

tables = list(downtown_demo_meta, downtown_fgc_per, cma_fgc_per, fgc_growth_change)

fgc_joined = reduce(tables, left_join, by = "DTNAME")
fgc_joined = fgc_joined |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(fgc_joined, "./sdsc-data/outputs/initialsheets/first_generation_breakdown.csv")

rm(downtown_demo_meta, downtown_fgc_per, cma_fgc_per, fgc_growth_change)
