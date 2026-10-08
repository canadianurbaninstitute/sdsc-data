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
# return it long, one row per region, variable and census vector:
#   geo_type = "DA"  : DAs in the downtowns (da_list_<year>.csv), keyed DAUID + DTNAME
#   geo_type = "CMA" : CMAs/CAs as defined in that year, keyed CMAPUID + CMANAME.
#                      Uses cma_list_<year>.csv if it exists, otherwise every CMA/CA
#                      in Canada (there is no 2001 list)
#   geo_type = "CSD" : CSDs as defined in that year, keyed CSDUID + CSDNAME, plus the
#                      CMAPUID of the CMA/CA each CSD falls in (NA outside CMAs/CAs).
#                      Uses csd_list_<year>.csv (CSDUID column) if it exists,
#                      otherwise every CSD in Canada
pull_census_year = function(year, census_vars, geo_type = c("DA", "CMA", "CSD")) {

    geo_type = match.arg(geo_type)
    code_col = paste0("code_", year)

    year_vars = census_vars %>%
        filter(!is.na(.data[[code_col]])) %>%
        select(theme, indicator, variable_name, agg, weight, code = all_of(code_col))

    vectors = year_vars %>%
        filter(!code %in% base_cols) %>%
        pull(code) %>%
        unique()

    if (geo_type == "DA") {
        da_list = read_csv(sprintf("./sdsc-data/outputs/statscan_request/da_list_%s.csv", year),
                       col_types = cols(.default = "c"))

        census = get_census_data(unique(da_list$DAUID), "DA", vectors, year)
        id_col = "DAUID"
    } else if (geo_type == "CSD") {
        csd_file = sprintf("./sdsc-data/outputs/statscan_request/csd_list_%s.csv", year)

        census = if (file.exists(csd_file)) {
            csd_list = read_csv(csd_file, col_types = cols(.default = "c"))
            get_census_data(unique(csd_list$CSDUID), "CSD", vectors, year)
        } else {
            get_census_data("01", "CSD", vectors, year, region_level = "C")
        }
        id_col = "CSDUID"
    } else {
        cma_file = sprintf("./sdsc-data/outputs/statscan_request/cma_list_%s.csv", year)

        census = if (file.exists(cma_file)) {
            cma_list = read_csv(cma_file, col_types = cols(.default = "c"))
            get_census_data(unique(cma_list$CMAPUID), "CMA", vectors, year)
        } else {
            get_census_data("01", "CMA", vectors, year, region_level = "C")
        }
        id_col = "CMAPUID"
    }

    labels = census$labels %>%
        select(code = Vector, label = Detail)

    # CSD-level pulls also return the parent CMA/CA code; carry it through as an id
    long = census$data %>%
        select(id = GeoUID, region_name = `Region Name`, any_of("CMA_UID"),
               any_of(base_cols), all_of(vectors)) %>%
        pivot_longer(-c(id, region_name, any_of("CMA_UID")), names_to = "code", values_to = "value") %>%
        inner_join(year_vars, by = "code", relationship = "many-to-many") %>%
        left_join(labels, by = "code") %>%
        mutate(label = coalesce(label, code), year = year, .before = 1)

    if (geo_type == "DA") {
        long %>%
            select(-region_name) %>%
            rename(DAUID = id) %>%
            inner_join(da_list, by = "DAUID")
    } else if (geo_type == "CSD") {
        # CSD names carry a CSD-type suffix that can be accented, e.g. "Lévis (V)", "Gaspé (MÉ)"
        long %>%
            mutate(CSDNAME = str_remove(as.character(region_name), " \\([^()]+\\)$")) %>%
            select(-region_name) %>%
            rename(CSDUID = id, any_of(c(CMAPUID = "CMA_UID")))
    } else {
        # CensusMapper names carry a type suffix, e.g. "Toronto (B)"
        long %>%
            mutate(CMANAME = str_remove(as.character(region_name), " \\([A-Z]+\\)$")) %>%
            select(-region_name) %>%
            rename(CMAPUID = id)
    }
}


# sum that stays NA when every region is suppressed, rather than returning 0
sum_or_na = function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# weighted mean that drops regions missing either the value or the weight
weighted_mean_or_na = function(x, w) {
    keep = !is.na(x) & !is.na(w)
    if (!any(keep)) NA_real_ else sum(x[keep] * w[keep]) / sum(w[keep])
}

# Roll census_long up from unit_col (the pulled region, e.g. DAUID) to group_col
# (e.g. DTNAME), add percents, and put 2001 and 2021 side by side.
# For CMAs unit and group are both CMAPUID, so each CMA "rolls up" to itself
summarise_census = function(census_long, unit_col, group_col) {

    # copy rather than rename: for CMAs unit_col and group_col are the same column
    census_long = census_long %>%
        mutate(unit = .data[[unit_col]], group = .data[[group_col]])

    # unit-level values of the weighting variables (e.g. FTFY earners for average income)
    unit_weights = census_long %>%
        filter(variable_name %in% census_vars$weight) %>%
        distinct(year, unit, weight = variable_name, code, value) %>%
        group_by(year, unit, weight) %>%
        summarise(weight_value = sum(value, na.rm = TRUE), .groups = "drop")

    summary = census_long %>%
        left_join(unit_weights, by = c("year", "unit", "weight")) %>%
        group_by(year, group, theme, indicator, variable_name) %>%
        summarise(
            value = if (first(agg) == "sum") sum_or_na(value) else weighted_mean_or_na(value, weight_value),
            .groups = "drop"
        )

    # percent of each variable within its indicator (0-100), using the indicator's
    # "Total ..." row as the denominator (e.g. renters / total households by tenure).
    # Percents are what compare fairly across years when a region's population has
    # grown; indicators without a total row (Population, Dwellings, income) get NA
    summary = summary %>%
        group_by(year, group, theme, indicator) %>%
        mutate(
            is_total = str_starts(variable_name, "Total"),
            percent = if (sum(is_total) == 1) value / value[is_total] * 100 else NA_real_,
            percent = if_else(is_total, NA_real_, percent)
        ) %>%
        ungroup() %>%
        select(-is_total)

    # one row per region and variable with both years side by side, in sheet order
    #   change / pct_change  : change in the count (or average, for income); pct_change is 0-100
    #   percent_point_change : change in percent, in percentage points
    summary %>%
        pivot_wider(names_from = year, values_from = c(value, percent)) %>%
        mutate(
            change = value_2021 - value_2001,
            pct_change = change / value_2001 * 100,
            percent_point_change = percent_2021 - percent_2001,
            variable_name = factor(variable_name, levels = unique(census_vars$variable_name))
        ) %>%
        arrange(group, variable_name) %>%
        mutate(variable_name = as.character(variable_name)) %>%
        rename("{group_col}" := group)
}


# run each year on its own; once a year succeeds it's cached, so re-running
# that line later loads from disk instead of using API quota

# --- Downtowns (DA level, rolled up; DA boundaries differ between years, downtowns don't) ---
census_da_2001 = pull_census_year(2001, census_vars, "DA")
census_da_2021 = pull_census_year(2021, census_vars, "DA")

census_da_long = bind_rows(census_da_2001, census_da_2021)
downtown_comparison = summarise_census(census_da_long, "DAUID", "DTNAME")

# --- CMAs/CAs as defined in each census year (boundaries are NOT held constant,
# so some change is territory added to a CMA, not growth within it). CMAs/CAs
# that only exist in one year get NA for the other ---
census_cma_2001 = pull_census_year(2001, census_vars, "CSD")

# join the csd cma crosswalk sheet
csd_cma_crosswalk = read_csv("./sdsc-data/data/csd_cma_crosswalk_2001.csv") %>%
    mutate(
        CMAPUID = as.character(CMAPUID),
        CSDUID = as.character(CSDUID)
    )

census_cma_2001 = census_cma_2001 %>%
    select(-CMAPUID) %>%
    left_join(csd_cma_crosswalk, by = "CSDUID") %>%
    select(year, CSDUID, CMAPUID, everything())

census_cma_2021 = pull_census_year(2021, census_vars, "CMA")

census_cma_long = bind_rows(census_cma_2001, census_cma_2021)

# one name per CMAPUID, preferring the 2021 name where a CMA was renamed
cma_names = census_cma_long %>%
    distinct(year, CMAPUID, CMANAME) %>%
    arrange(CMAPUID, desc(year)) %>%
    distinct(CMAPUID, .keep_all = TRUE) %>%
    select(CMAPUID, CMANAME)

cma_comparison = summarise_census(census_cma_long, "CMAPUID", "CMAPUID") %>%
    left_join(cma_names, by = "CMAPUID") %>%
    relocate(CMANAME, .after = CMAPUID)

# CMAPUIDs found in only one year: new/retired CAs, or code changes to check by hand
cma_one_year_only = census_cma_long %>%
    distinct(year, CMAPUID, CMANAME) %>%
    group_by(CMAPUID) %>%
    filter(n_distinct(year) == 1) %>%
    ungroup() %>%
    arrange(year, CMAPUID)
print(cma_one_year_only, n = Inf)


# --- Downtown vs CMA: one row per downtown and variable, downtown (_dt) and
# CMA (_cma) columns side by side, linked on CMAPUID (not names, which differ
# between sources). Built by statscan_request.r ---
dt_cma_crosswalk = read_csv("./sdsc-data/outputs/statscan_request/dt_cma_crosswalk.csv",
                            col_types = cols(.default = "c"))

var_agg = census_vars %>%
    distinct(variable_name, agg)

# Comparison measures (all in percent / percentage points):
#   growth_gap          : downtown pct_change minus CMA pct_change; > 0 means the
#                         downtown grew faster than its CMA
#   dt_share_<year>     : downtown as a percent of its CMA (counts only; NA for
#                         averages such as income, which can't be summed into shares)
#   dt_share_change     : change in that share, 2001 to 2021
#   composition_gap_<y> : downtown percent minus CMA percent within an indicator
#                         (e.g. renter share downtown vs. in the CMA)
#   composition_gap_change : whether the downtown became more or less distinct
# Caveat: CMAs are as defined each year, so CMA growth includes territory added
# to the CMA since 2001 while downtowns are fixed; growth_gap leans towards the CMA.
# cma_in_2001 = FALSE means the CMA/CA did not exist in 2001, so it has no baseline
dt_vs_cma = downtown_comparison %>%
    inner_join(dt_cma_crosswalk, by = "DTNAME") %>%
    left_join(cma_comparison, by = c("CMAPUID", "theme", "indicator", "variable_name"),
              suffix = c("_dt", "_cma")) %>%
    left_join(var_agg, by = "variable_name") %>%
    mutate(
        cma_in_2001 = !is.na(value_2001_cma),
        growth_gap = pct_change_dt - pct_change_cma,
        dt_share_2001 = if_else(agg == "sum", value_2001_dt / value_2001_cma * 100, NA_real_),
        dt_share_2021 = if_else(agg == "sum", value_2021_dt / value_2021_cma * 100, NA_real_),
        dt_share_change = dt_share_2021 - dt_share_2001,
        composition_gap_2001 = percent_2001_dt - percent_2001_cma,
        composition_gap_2021 = percent_2021_dt - percent_2021_cma,
        composition_gap_change = composition_gap_2021 - composition_gap_2001
    ) %>%
    select(-agg) %>%
    relocate(CMAPUID, CMANAME, cma_in_2001, .after = DTNAME)

# downtowns whose CMA didn't come back from the CMA pull (check the crosswalk
# and cma_list_2021.csv if this isn't empty)
dt_vs_cma %>%
    filter(is.na(value_2021_cma)) %>%
    distinct(DTNAME, CMAPUID) %>%
    print(n = Inf)


dir.create("./sdsc-data/outputs/census", showWarnings = FALSE)
write_parquet(census_da_long, "./sdsc-data/outputs/census/census_da_long.parquet")
write_csv(downtown_comparison, "./sdsc-data/outputs/census/downtown_census_comparison.csv")
write_parquet(census_cma_long, "./sdsc-data/outputs/census/census_cma_long.parquet")
write_csv(cma_comparison, "./sdsc-data/outputs/census/cma_census_comparison.csv")
write_csv(dt_vs_cma, "./sdsc-data/outputs/census/downtown_vs_cma_comparison.csv")
