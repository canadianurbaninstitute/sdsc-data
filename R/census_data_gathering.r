library(tidyverse)
library(cancensus)
library(arrow)

# Read in the Renviron file
readRenviron("./sdsc-data/.Renviron")

# API key is read from .Renviron -- run once per machine:
#   set_cancensus_api_key("<your key>", install = TRUE)
#   set_cancensus_cache_path("<cache folder>", install = TRUE)
if (Sys.getenv("CM_API_KEY") == "") stop("CensusMapper API key not set, see comment above")

# persistent cache so re-running the script doesn't re-spend API quota
if (Sys.getenv("CM_CACHE_PATH") == "") {
    dir.create("./sdsc-data/data/cancensus_cache", showWarnings = FALSE)
    options(cancensus.cache_path = "./sdsc-data/data/cancensus_cache")
}


# Pull census data from CensusMapper for a set of regions at a given level.
#   geo_ids      : character vector of geographic IDs (e.g. CMAUIDs, PRUIDs, CTUIDs, DAUIDs)
#   level        : output geography: "C", "PR", "CMA", "CD", "CSD", "CT", "DA", "DB"
#   vars         : character vector of vectors (e.g. "v_CA21_4307"); codes are year-specific
#   year         : census year (2021, 2016, 2011, 2006, 2001, 1996)
#   spatial      : TRUE returns an sf object, FALSE returns a plain tibble
#   region_level : level of the geo_ids passed in (defaults to `level`)
# Returns a list: $data (columns named by vector code) and $labels (code -> full label)
get_census_data = function(geo_ids, level, vars, year = 2021, spatial = FALSE,
                           region_level = level) {

    dataset = if (year == 1996) "CA1996" else paste0("CA", substr(year, 3, 4))

    valid_levels = c("C", "PR", "CMA", "CD", "CSD", "CT", "DA", "DB")
    stopifnot(level %in% valid_levels, region_level %in% valid_levels)

    census_data = get_census(
        dataset    = dataset,
        regions    = setNames(list(as.character(geo_ids)), region_level),
        vectors    = vars,
        level      = level,
        geo_format = if (spatial) "sf" else NA,
        labels     = "short",
        use_cache  = TRUE
    )

    list(data = census_data, labels = label_vectors(census_data))
}


# Variable sheet: one row per census vector per variable. Rows sharing a
# variable_name are summed together, which is how 2001 and 2021 are lined up
# (e.g. 2001 male + female age groups -> one 2021 both-sexes age group)
census_vars = read_csv("./sdsc-data/data/census_vars_worksheet_v2.csv", col_types = cols(.default = "c"))

stopifnot(
    all(census_vars$agg %in% c("sum", "weighted_mean")),
    all(!is.na(census_vars$weight[census_vars$agg == "weighted_mean"])),
    all(na.omit(census_vars$weight) %in% census_vars$variable_name)
)

# columns get_census() returns for every region without being requested as vectors
base_cols = c("Population", "Dwellings", "Households")


# Pull every variable on the sheet for one census year in a single request and
# return it long: one row per downtown, DA, variable and census vector
pull_census_year = function(year, census_vars) {

    code_col = paste0("code_", year)

    year_vars = census_vars %>%
        filter(!is.na(.data[[code_col]])) %>%
        select(theme, indicator, variable_name, agg, weight, code = all_of(code_col))

    vectors = year_vars %>%
        filter(!code %in% base_cols) %>%
        pull(code) %>%
        unique()

    da_list = read_csv(sprintf("./sdsc-data/outputs/statscan_request/da_list_%s.csv", year),
                       col_types = cols(.default = "c"))

    census = get_census_data(unique(da_list$DAUID), "DA", vectors, year)

    labels = census$labels %>%
        select(code = Vector, label = Detail)

    census$data %>%
        select(DAUID = GeoUID, any_of(base_cols), all_of(vectors)) %>%
        pivot_longer(-DAUID, names_to = "code", values_to = "value") %>%
        inner_join(year_vars, by = "code", relationship = "many-to-many") %>%
        left_join(labels, by = "code") %>%
        mutate(label = coalesce(label, code)) %>%
        inner_join(da_list, by = "DAUID") %>%
        mutate(year = year, .before = 1)
}

# run each year on its own; once a year succeeds it's cached, so re-running
# that line later loads from disk instead of using API quota
census_2001 = pull_census_year(2001, census_vars)
census_2021 = pull_census_year(2021, census_vars)

census_long = bind_rows(census_2001, census_2021)


# DA-level values of the weighting variables (e.g. FTFY earners for average income)
da_weights = census_long %>%
    filter(variable_name %in% census_vars$weight) %>%
    distinct(year, DAUID, weight = variable_name, code, value) %>%
    group_by(year, DAUID, weight) %>%
    summarise(weight_value = sum(value, na.rm = TRUE), .groups = "drop")

# sum that stays NA when every DA is suppressed, rather than returning 0
sum_or_na = function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# weighted mean that drops DAs missing either the value or the weight
weighted_mean_or_na = function(x, w) {
    keep = !is.na(x) & !is.na(w)
    if (!any(keep)) NA_real_ else sum(x[keep] * w[keep]) / sum(w[keep])
}

# roll DAs up to downtowns; DA boundaries differ between years, downtowns don't
downtown_census = census_long %>%
    left_join(da_weights, by = c("year", "DAUID", "weight")) %>%
    group_by(year, DTNAME, theme, indicator, variable_name) %>%
    summarise(
        value = if (first(agg) == "sum") sum_or_na(value) else weighted_mean_or_na(value, weight_value),
        .groups = "drop"
    )

# percent of each variable within its indicator (0-100), using the indicator's
# "Total ..." row as the denominator (e.g. renters / total households by tenure).
# Percents are what compare fairly across years when a downtown's population has
# grown; indicators without a total row (Population, Dwellings, income) get NA
downtown_census = downtown_census %>%
    group_by(year, DTNAME, theme, indicator) %>%
    mutate(
        is_total = str_starts(variable_name, "Total"),
        percent = if (sum(is_total) == 1) value / value[is_total] * 100 else NA_real_,
        percent = if_else(is_total, NA_real_, percent)
    ) %>%
    ungroup() %>%
    select(-is_total)

# one row per downtown and variable with both years side by side, in sheet order
#   change / pct_change  : change in the count (or average, for income); pct_change is 0-100
#   percent_point_change : change in percent, in percentage points
downtown_comparison = downtown_census %>%
    pivot_wider(names_from = year, values_from = c(value, percent)) %>%
    mutate(
        change = value_2021 - value_2001,
        pct_change = change / value_2001 * 100,
        percent_point_change = percent_2021 - percent_2001,
        variable_name = factor(variable_name, levels = unique(census_vars$variable_name))
    ) %>%
    arrange(DTNAME, variable_name) %>%
    mutate(variable_name = as.character(variable_name))


dir.create("./sdsc-data/outputs/census", showWarnings = FALSE)
write_parquet(census_long, "./sdsc-data/outputs/census/census_da_long.parquet")
write_csv(downtown_comparison, "./sdsc-data/outputs/census/downtown_census_comparison.csv")
