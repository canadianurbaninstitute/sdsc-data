library(tidyverse)
library(sf)
library(msmdata)

# LOAD THE NEEDED SPATIAL FILES (CMAs / DAs)
cmas = st_read("./sdsc-data/data/lcma000a21a_e") %>%
    st_transform(crs = 3347)
das = st_read("./data/gda_000b06a_e") %>%
    st_transform(crs = 3347)


dbs = st_read("./data/db_2001.geojson")


# Load the downtown file for CMAs
cma_dt_boundaries = st_read("./sdsc-data/data/cma_downtowns.geojson") %>%
    st_transform(crs = 3347)

# join das to the downtown boundaries
dt_boundaries_da_join = st_join(cma_dt_boundaries, dbs %>% select(DAUID), join = st_intersects, left = TRUE)

dt_boundaries_da_join = dt_boundaries_da_join %>%
    st_drop_geometry() %>%
    select(DTNAME, DAUID) %>%
    distinct(DAUID, .keep_all = TRUE)

dt_boundaries_cma_join = st_join(cma_dt_boundaries, cmas %>% select(CMAUID), join = st_intersects, left = TRUE)

dt_boundaries_cma_join = dt_boundaries_cma_join %>%
    st_drop_geometry() %>%
    select(CMANAME, CMAUID) %>%
    rename("CMAPUID" = CMAUID) %>%
    distinct(CMAPUID, .keep_all = TRUE)


# Load the downtown file for CAs
ca_dt_boundaries = st_read("./sdsc-data/outputs/spatialfiles/ca_downtown_boundaries.geojson")

# union each downtown boundary based on geography
ca_dt_boundaries = ca_dt_boundaries %>%
    summarise(
        geometry = st_union(geometry),
        .by = layer
    )

# join das to the downtown boundaries
ca_dt_boundaries_da_join = st_join(ca_dt_boundaries, dbs %>% select(DAUID), join = st_intersects, left = TRUE)

ca_dt_boundaries_da_join = ca_dt_boundaries_da_join %>%
    st_drop_geometry() %>%
    select(layer, DAUID) %>%
    rename("DTNAME" = layer) %>%
    distinct(DAUID, .keep_all = TRUE)

ca_dt_boundaries_cma_join = st_join(ca_dt_boundaries, cmas %>% select(CMAUID), join = st_intersects, left = TRUE)

ca_dt_boundaries_cma_join = ca_dt_boundaries_cma_join %>%
    st_drop_geometry() %>%
    select(layer, CMAUID) %>%
    rename(CMANAME = "layer",
        CMAPUID = "CMAUID"
    ) %>%
    distinct(CMAPUID, .keep_all = TRUE)


# bind export sheets
da_list = bind_rows(dt_boundaries_da_join, ca_dt_boundaries_da_join)
write_csv(da_list, "./outputs/statscan_request/da_list_2001.csv")

cma_list = bind_rows(dt_boundaries_cma_join, ca_dt_boundaries_cma_join)
write_csv(cma_list, "./outputs/statscan_request/cma_list_2006.csv")


# downtown -> 2021 CMA/CA crosswalk, used to compare each downtown with its CMA.
# One point inside each downtown so a downtown that touches a neighbouring CMA
# isn't matched to both; joined on the 5-digit CMAPUID used by the CMA lists
dt_cma_crosswalk = bind_rows(
        cma_dt_boundaries %>% select(DTNAME),
        ca_dt_boundaries %>% select(DTNAME = layer)
    ) %>%
    st_point_on_surface() %>%
    st_join(cmas %>% select(CMAPUID), join = st_within) %>%
    st_drop_geometry() %>%
    as_tibble()

# every downtown should land in exactly one CMA/CA
stopifnot(
    !anyNA(dt_cma_crosswalk$CMAPUID),
    !anyDuplicated(dt_cma_crosswalk$DTNAME)
)

write_csv(dt_cma_crosswalk, "./sdsc-data/outputs/statscan_request/dt_cma_crosswalk.csv")
