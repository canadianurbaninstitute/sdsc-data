library(tidyverse)
library(rlanguageserver)
library(sf)
library(msmdata)
library(classInt)

dotenv::load_dot_env("C:/Users/atabascio/MMSDashboards/secrets/.env")

# load the downtown boundary and regional spatialfiles
downtowns = read_blob_data("Data/spatialfiles/downtowns.parquet", type = "geoparquet")
regions = read_blob_data("Data/spatialfiles/cma.parquet", type = "geoparquet")

# load the business data
business_data = read_blob_data("Data/eadata26/businesses/businesses_26.csv")
naics = read_blob_data("Data/NAICS_codes_complete.csv")

business_data = process_business_data(business_data, naics)

## Gather downtown and cma spatialfiles for the map
# vector of downtown study areas
downtowns_vec = c("Calgary", "Charlottetown", "Edmonton", "Gatineau", "Halifax", 
"Hamilton", "Lethbridge", "London", "Moncton", "Montréal", "North Bay", "Ottawa",
"Prince George", "Saskatoon", "St. John's", "Toronto", "Vancouver", "Whitehorse",
"Winnipeg", "Wood Buffalo")

# filter the downtowns sf based on the vector
downtowns_filtered = downtowns %>%
    filter(DTUID == "462660231" | DTNAME %in% downtowns_vec)

regions_filtered = regions %>%
    filter(CMANAME %in% downtowns_vec)

st_write(downtowns_filtered, "./sdsc-data/outputs/spatialfiles/downtown_boundaries.geojson") # nolint
st_write(regions_filtered, "./sdsc-data/outputs/spatialfiles/cma_boundaries.geojson")

## Test Area: Vancouver CMA -----

# filter to vancouver cma
van_cma = regions %>%
    filter(CMAUID == "933")

# create the hexgrid for the cma
cma_hex = st_make_grid(
    van_cma,
    cellsize = 500,
    square = FALSE,
    flat_topped = TRUE
) %>%
st_sf() %>%
st_intersection(van_cma)

# give each hexagon a stable id based on its centroid, so it survives re-runs/subsetting
hex_centroids = st_centroid(cma_hex) %>% st_coordinates()

cma_hex = cma_hex %>%
    mutate(hex_id = paste0("hex_", round(hex_centroids[, 1]), "_", round(hex_centroids[, 2])))

# join the business data to the hexgrid
bus_hex_int = st_join(cma_hex %>% select(hex_id), business_data %>% select(`Unique Identifier`, `Employee Size Code`), join = st_intersects, left = TRUE)

# summarise Employee Size Code per hex_id, keeping every hex_id and turning NA sums into 0
hex_employee_summary = bus_hex_int %>%
    st_drop_geometry() %>%
    mutate(`Employee Size Code` = as.numeric(`Employee Size Code`)) %>%
    group_by(hex_id) %>%
    summarise(total_employee_size = sum(`Employee Size Code`, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(total_employee_size = replace_na(total_employee_size, 0))

# join the summary back onto the hexgrid geometry
cma_hex_summary = cma_hex %>%
    left_join(hex_employee_summary, by = "hex_id")

st_write(cma_hex_summary, "./sdsc-data/outputs/spatialfiles/vancouver_emp_hex.geojson")

## Quick Visualization ----
# bin total_employee_size using natural breaks (Jenks)
hex_breaks = classIntervals(cma_hex_summary$total_employee_size, n = 5, style = "jenks")$brks

cma_hex_summary = cma_hex_summary %>%
    mutate(employee_size_bin = cut(total_employee_size, breaks = hex_breaks, include.lowest = TRUE))

# quick plot of the hexgrid, colored by summed Employee Size Code
ggplot(cma_hex_summary) +
    geom_sf(aes(fill = employee_size_bin), color = NA) +
    scale_fill_viridis_d(name = "Employee Size\n(sum)") +
    theme_void()

# Do the same with residential density
nar_data = read_blob_data("Data/spatialfiles/NAR_59.parquet", type = "geoparquet")
nar_data = nar_data %>%
    filter(BU_USE < 3)

# join the business data to the hexgrid
res_hex_int = st_join(cma_hex %>% select(hex_id), nar_data %>% select(LOC_GUID), join = st_intersects, left = TRUE)

# summarise Employee Size Code per hex_id, keeping every hex_id and turning NA sums into 0
hex_population_summary = res_hex_int %>%
    st_drop_geometry() %>%
    group_by(hex_id) %>%
    summarise(population_total = n()) %>%
    ungroup() %>%
    mutate(population_total = replace_na(population_total, 0))

# join the summary back onto the hexgrid geometry
cma_hex_summary = cma_hex %>%
    left_join(hex_population_summary, by = "hex_id")

st_write(cma_hex_summary, "./sdsc-data/outputs/spatialfiles/vancouver_pop_hex.geojson")

# bin population_total using natural breaks (Jenks)
hex_breaks = classIntervals(cma_hex_summary$population_total, n = 5, style = "jenks")$brks

cma_hex_summary = cma_hex_summary %>%
    mutate(employee_size_bin = cut(population_total, breaks = hex_breaks, include.lowest = TRUE))

# quick plot of the hexgrid, colored by summed Employee Size Code
ggplot(cma_hex_summary) +
    geom_sf(aes(fill = employee_size_bin), color = NA) +
    scale_fill_viridis_d(name = "Employee Size\n(sum)") +
    theme_void()