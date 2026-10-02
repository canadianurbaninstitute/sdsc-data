library(tidyverse)
library(sf)
library(msmdata)

# Load env variables
dotenv::load_dot_env("C:/Users/atabascio/MMSDashboards/secrets/.env")

## Internal functions ----

#' Get CMA inputs
#' 
#' loads and preparesbusiness data clipped to the cma area
#' 
#' @param business_data environics business data
#' @param cma_sf cma spatial file
#' @param exclude A vector of naics codes to exclude similar to what statscan did (hospitals, universities, etc.)
#' 
#' @return the business data clipped to the cma
#' @keywords internal
prepare_business_data = function(business_data, cma_sf, cmapuid, exclude = NULL){
    
    # filter the cma
    cma_sf = cma_sf %>% filter(CMAPUID == cmapuid)
    stopifnot(nrow(cma_sf) == 1)

    if(!is.null(exclude)){
        message("removing naics codes")

        business_data = business_data %>%
            filter(!(NAICS_4 %in% exclude))
    }

    message("Clipping business data to CMA/CA")
    business_cma_int = st_intersection(business_data, cma_sf)

    business_cma_int = business_cma_int %>%
        mutate(Employee.Size.Code = replace(Employee.Size.Code, is.na(Employee.Size.Code), 2.5))

    return(business_cma_int)
}


#' Compute a weighted median
#'
#' helper used by compute_bandwidth to find the weighted median distance
#' from the weighted mean center
#'
#' @param x numeric vector of values (e.g. distances)
#' @param w numeric vector of weights, same length as x
#'
#' @return the weighted median of x
#' @keywords internal
weighted_median = function(x, w){

    ord = order(x)
    x = x[ord]
    w = w[ord]

    cum_w = cumsum(w) / sum(w)

    x[which(cum_w >= 0.5)[1]]
}


#' Compute kernel density bandwidth
#'
#' Replicates the search radius/bandwidth calculation used by Esri's Kernel
#' Density tool (Silverman's Rule of Thumb), which is the same approach
#' Statistics Canada used to derive the 2016 downtown boundaries.
#'
#' @param points an sf POINT object (e.g. business locations) already
#'   projected to a local metric CRS
#' @param weight_field optional name of a numeric column in `points` to use
#'   as weights (e.g. employment count). If NULL, every point is weighted
#'   equally, matching Esri's behaviour with no population field set.
#'
#' @return a single numeric bandwidth value, in the linear units of the
#'   points' CRS
#' @keywords internal
compute_bandwidth = function(points, weight_field = NULL){

    coords = sf::st_coordinates(points)
    x = coords[, "X"]
    y = coords[, "Y"]

    if (!is.null(weight_field)) {
        w = points[[weight_field]]
        if (is.null(w)) {
            stop(paste0("weight_field '", weight_field, "' not found in points"), call. = FALSE)
        }
    } else {
        w = rep(1, length(x))
    }

    # Esri/StatCan definition of n: sum of weights when a population field is
    # supplied, otherwise the point count
    n = sum(w)

    # weighted mean center
    W = sum(w)
    x_mean = sum(w * x) / W
    y_mean = sum(w * y) / W

    # weighted standard distance
    sd_dist = sqrt(sum(w * (x - x_mean)^2) / W + sum(w * (y - y_mean)^2) / W)

    # weighted median distance from the weighted mean center
    dist_from_center = sqrt((x - x_mean)^2 + (y - y_mean)^2)
    dm = weighted_median(dist_from_center, w)

    # Silverman's Rule of Thumb, as implemented by Esri's Kernel Density tool
    bandwidth = 0.9 * min(sd_dist, sqrt(1 / log(2)) * dm) * n^(-0.2)

    return(bandwidth)
}


#' Compute job density surface
#'
#' Runs a weighted kernel density estimate over a set of points (e.g.
#' employment-weighted business locations), using the quartic kernel
#' Statistics Canada used to derive the 2016 downtown boundaries. Points
#' must already be projected to a local metric CRS (see compute_bandwidth).
#'
#' @param points an sf POINT object, projected to a local metric CRS
#' @param bandwidth numeric bandwidth (search radius), in the same linear
#'   units as the points' CRS - typically the output of compute_bandwidth()
#' @param weight_field optional name of a numeric column in `points` to use
#'   as weights (e.g. employment count). If NULL, every point is weighted
#'   equally
#' @param window optional sf polygon defining the study area (e.g. the CMA
#'   boundary). If NULL, defaults to the bounding box of `points` padded by
#'   one bandwidth on each side, so the kernel isn't truncated at the edge
#'   of the data
#' @param cell_size output raster cell size, in the same linear units as the
#'   points' CRS. Defaults to 10 (metres), matching the StatCan methodology;
#'   increase for large CMAs where a 10m surface is too slow to compute
#'
#' @return a terra SpatRaster of job density, at `cell_size` resolution
#' @keywords internal
compute_density_surface = function(points, bandwidth, weight_field = NULL, window = NULL, cell_size = 10){

    message("computing job density surface")

    if (is.null(window)) {
        bbox = sf::st_bbox(points)
        win = spatstat.geom::owin(
            xrange = c(bbox[["xmin"]] - bandwidth, bbox[["xmax"]] + bandwidth),
            yrange = c(bbox[["ymin"]] - bandwidth, bbox[["ymax"]] + bandwidth)
        )
    } else {
        win = spatstat.geom::as.owin(window)
    }

    coords = sf::st_coordinates(points)

    if (!is.null(weight_field)) {
        w = points[[weight_field]]
    } else {
        w = rep(1, nrow(coords))
    }

    pp = spatstat.geom::ppp(x = coords[, "X"], y = coords[, "Y"], window = win)

    # edge = FALSE matches Esri/StatCan's approach: Equation 1 is a plain
    # sum of quartic kernel contributions, with no isotropic edge correction
    density_im = spatstat.explore::density.ppp(
        pp,
        sigma = bandwidth,
        weights = w,
        kernel = "quartic",
        eps = cell_size,
        edge = FALSE
    )

    # spatstat's im$v has row 1 = smallest y (bottom); terra expects row 1 =
    # largest y (top), so the rows need reversing to line up geographically
    density_matrix = density_im$v[nrow(density_im$v):1, ]

    density_rast = terra::rast(
        density_matrix,
        extent = terra::ext(density_im$xrange[1], density_im$xrange[2], density_im$yrange[1], density_im$yrange[2]),
        crs = sf::st_crs(points)$wkt
    )

    return(density_rast)
}


#' Classify density surface into high-density core mask
#'
#' Replicates Statistics Canada's equal-interval classification step: divides
#' the KDE surface into 10 equal-interval classes and selects the top N classes
#' as the downtown core candidate zone, where N is determined by CMA employment
#' size stratum (StatCan 2021, Table 1).
#'
#' @param density_rast a terra SpatRaster of job density, typically the output
#'   of compute_density_surface()
#' @param n_jobs total employment count for the CMA, used to assign the CMA to
#'   a size stratum. Pass the sum of the employment weight field across all
#'   businesses in the CMA (e.g. sum of `Employee.Size.Code`)
#'
#' @return a terra SpatRaster binary mask: 1 where density falls in the top N
#'   classes, NA elsewhere
#' @keywords internal
classify_density_surface = function(density_rast, n_jobs){

    # Number of top classes to retain, by CMA employment stratum
    n_top = dplyr::case_when(
        n_jobs >= 500000 ~ 6L,
        n_jobs >= 100000 ~ 4L,
        TRUE             ~ 2L
    )

    message(sprintf("CMA stratum: %s jobs → retaining top %d of 10 classes", n_jobs, n_top))
    rast_min = terra::global(density_rast, "min", na.rm = TRUE)[[1]]
    rast_max = terra::global(density_rast, "max", na.rm = TRUE)[[1]]

    # Equal-interval width across 10 classes
    interval  = (rast_max - rast_min) / 10

    # Lower bound of the top n_top classes
    # Class 10 spans [max - interval, max], class (10 - n_top + 1) starts at:
    threshold = rast_min + (10 - n_top) * interval

    # Binary mask: 1 in top classes, NA elsewhere
    density_mask = terra::ifel(density_rast >= threshold, 1L, NA)

    return(density_mask)
}


#' Select downtown core polygons from a density mask
#'
#' Vectorizes the binary density mask into contiguous blobs, assigns each blob
#' to a CSD via centroid intersection, applies within-CSD selection (keep the
#' blob with the most jobs per CSD), then labels surviving blobs as "Primary"
#' (highest jobs across all CSDs) or "Secondary" (all others).
#'
#' @param density_mask a terra SpatRaster binary mask (1 / NA), typically the
#'   output of classify_density_surface()
#' @param business_data an sf POINT object of businesses clipped to the CMA,
#'   must contain an `Employee.Size.Code` column
#' @param csd_sf an sf polygon object of Census Sub-Division boundaries,
#'   must contain CSDUID and CSDNAME columns
#'
#' @return an sf POLYGON object with one row per surviving CSD downtown, with
#'   columns: blob_id, CSDUID, CSDNAME, total_jobs, DT_Type
#' @keywords internal
select_downtown_core = function(density_mask, business_data, csd_sf){

    message("vectorizing density mask into candidate blobs")
    # adjacent same-value cells so each disconnected cluster becomes one feature
    core_polys = terra::as.polygons(density_mask) |>
        sf::st_as_sf() |>
        sf::st_make_valid() |>
        sf::st_cast("POLYGON") |>
        dplyr::mutate(blob_id = dplyr::row_number())

    message(sprintf("%d candidate blob(s) identified", nrow(core_polys)))

    # assign each blob to a CSD using centroid
    csd_sf = sf::st_transform(csd_sf, sf::st_crs(core_polys))

    blob_csd = core_polys |>
        sf::st_centroid() |>
        sf::st_join(csd_sf[, c("CSDUID", "CSDNAME")], join = sf::st_within) |>
        sf::st_drop_geometry() |>
        dplyr::select(blob_id, CSDUID, CSDNAME)

    core_polys = dplyr::left_join(core_polys, blob_csd, by = "blob_id")

    # Step 3: sum jobs per blob from business point data
    business_data = sf::st_transform(business_data, sf::st_crs(core_polys))

    blob_jobs = sf::st_join(business_data, core_polys[, "blob_id"], join = sf::st_within) |>
        sf::st_drop_geometry() |>
        dplyr::group_by(blob_id) |>
        dplyr::summarise(total_jobs = sum(`Employee.Size.Code`, na.rm = TRUE), .groups = "drop")

    core_polys = core_polys |>
        dplyr::left_join(blob_jobs, by = "blob_id") |>
        dplyr::mutate(total_jobs = dplyr::coalesce(total_jobs, 0L))

    # Step 4: within each CSD keep only the blob with the most jobs (Rule 2)
    core_polys = core_polys |>
        dplyr::group_by(CSDUID) |>
        dplyr::slice_max(total_jobs, n = 2, with_ties = FALSE) |>
        dplyr::ungroup()

    message(sprintf("%d CSD downtown(s) retained after within-CSD selection", nrow(core_polys)))

    # Step 5: label across CSDs — highest jobs = Primary, all others = Secondary
    core_polys = core_polys |>
        dplyr::mutate(
            DT_Type = dplyr::if_else(total_jobs == max(total_jobs), "Primary", "Secondary")
        )

    return(core_polys)
}


#' Build downtown boundaries from core polygons and dissemination areas
#'
#' Applies a 1 km buffer to each core polygon, selects dissemination areas
#' whose centroid falls within the buffer, then unions those DAs per downtown
#' to produce the final boundary layer. Each row in the output represents one
#' downtown (Primary or Secondary) with a dissolved DA-based boundary.
#'
#' @param core_polys an sf POLYGON object, typically the output of
#'   select_downtown_core(), containing columns CSDUID, CSDNAME, DT_Type
#' @param da_sf an sf POLYGON object of Dissemination Area boundaries for the
#'   CMA, must contain a DAUID column
#' @param buffer_dist numeric buffer distance in metres (default 1000)
#'
#' @return an sf POLYGON object with one row per downtown, with columns:
#'   CSDUID, CSDNAME, DT_Type, n_das (number of DAs included)
#' @keywords internal
build_downtown_boundary = function(core_polys, da_sf, buffer_dist = 500){

    # make sure the DAs and core polygon have the same crs
    da_sf = sf::st_transform(da_sf, sf::st_crs(core_polys))

    # buffer each core polygon by buffer_dist
    core_buffered = sf::st_buffer(core_polys, dist = buffer_dist)

    # select DAs whose centroid falls within any buffered core
    da_centroids = sf::st_centroid(da_sf)

    da_matched = sf::st_join(
        da_centroids,
        core_buffered[, c("CSDUID", "CSDNAME", "DT_Type")],
        join = sf::st_within
    ) |>
        sf::st_drop_geometry()

    # join matched DA attributes back to full DA polygons
    da_included = da_sf |>
        dplyr::inner_join(da_matched[, c("DAUID", "CSDUID", "CSDNAME", "DT_Type")], by = "DAUID")

    # keep DAs as separate geometries per downtown (grouped by CSDUID to keep cities separate)
    downtown_boundaries = da_included |>
        dplyr::group_by(CSDUID, CSDNAME, DT_Type) |>
        dplyr::mutate(n_das = dplyr::n()) |>
        dplyr::ungroup() |>
        sf::st_as_sf()

    return(downtown_boundaries)
}

## Main function ----

#' Load shared inputs for downtown boundary creation
#'
#' Loads and prepares everything that is reused across CMAs: the national
#' business layer (joined to NAICS descriptions) and the national CMA, CSD,
#' and DA spatial layers
#'
#' @return a named list with elements: business_raw, cma_sf, csd_sf, da_sf
#' @keywords internal
load_downtown_inputs = function(){

    # check that environment variables are there to access Azure
    storage_account <- Sys.getenv("AZURE_STORAGE_ACCOUNT")
    storage_key     <- Sys.getenv("AZURE_STORAGE_KEY")
    container_name  <- Sys.getenv("AZURE_CONTAINER_NAME")

    if (storage_account == "" || storage_key == "") {
        stop("AZURE_STORAGE_ACCOUNT and AZURE_STORAGE_KEY must be set in .env file", call. = FALSE)
    }
    if (container_name == "") {
        stop("AZURE_CONTAINER_NAME must be set in .env file", call. = FALSE)
    }

    message("loading business data")
    # load in a prepare the business data
    business_raw = read_blob_data("Data/eadata26/businesses/businesses_26.csv")
    naics = read_blob_data("Data/NAICS_codes_complete.csv")
    business_processed = process_business_data(business_raw, naics)

    # load in spatialfiles needed for analysis
    message("loading spatial boundary files")
    cma_sf = read_blob_data("Data/spatialfiles/cma.parquet",  type = "geoparquet")
    csd_sf = read_blob_data("Data/spatialfiles/csd.parquet",  type = "geoparquet")
    da_sf  = read_blob_data("Data/spatialfiles/da.parquet",   type = "geoparquet")

    return(
        list(
            business_processed = business_processed,
            cma_sf       = cma_sf,
            csd_sf       = csd_sf,
            da_sf        = da_sf
        )
    )
}


#' Create downtown boundary for a CMA or CA
#'
#' Orchestrates the full Statistics Canada-based methodology: clips business
#' data to the CMA, runs a weighted KDE, classifies the density surface,
#' selects core polygons by CSD, applies a 1 km buffer, and unions the
#' included dissemination areas into a final downtown boundary.
#'
#' @param cmapuid character CMA/CA code to process
#' @param business_processed national business layer
#' @param cma_sf national CMA/CA spatial layer
#' @param csd_sf national CSD spatial layer
#' @param da_sf national DA spatial layer
#' @param exclude optional character vector of 4-digit NAICS codes to exclude
#' @param buffer_dist buffer distance in metres around the downtown core
#'
#' @return an sf POLYGON object with one row per downtown (Primary or
#'   Secondary), with columns: CSDUID, CSDNAME, DT_Type, n_das
create_downtown_boundary = function(cmapuid, business_processed, cma_sf, csd_sf, da_sf, exclude = NULL, buffer_dist = 1000){

    # clip the business data to the cma
    business_clipped = prepare_business_data(business_processed, cma_sf, cmapuid, exclude = exclude)

    # filter the overall cma/ca geography
    cma_filtered = cma_sf %>% filter(CMAPUID == cmapuid)

    # clip national DA layer to CMA extent
    da_cma = sf::st_filter(da_sf, cma_filtered)

    # --- Step 2: compute total employment (drives CMA stratum) ---
    n_jobs = sum(business_clipped[["Employee.Size.Code"]], na.rm = TRUE)

    # --- Step 3: bandwidth and KDE ---
    message("computing bandwidth and density surface")
    bandwidth = compute_bandwidth(business_clipped, weight_field = "Employee.Size.Code")
    density_rast = compute_density_surface(
        business_clipped,
        bandwidth,
        weight_field = "Employee.Size.Code",
        window = cma_filtered,
        cell_size = 100
    )

    # --- Step 4: classify density surface ---
    density_mask = classify_density_surface(density_rast, n_jobs)

    # --- Step 5: select downtown core polygons ---
    core_polys = select_downtown_core(density_mask, business_clipped, csd_sf)

    # --- Step 6: build final DA-based downtown boundary ---
    downtown = build_downtown_boundary(core_polys, da_cma, buffer_dist)

    # filter out DT Type NAs
    downtown = downtown %>% filter(!is.na(DT_Type))

    return(downtown)
}


# inputs
inputs = load_downtown_inputs()
business_processed = inputs$business_processed
# testing removing institutional naics codes
dt_naics_codes = c(44, 45, 51, 52, 53, 54, 55, 56, 71, 72, 81, 91, 92)
naics_to_remove = business_processed %>%
    st_drop_geometry() %>%
    filter(!(NAICS_2 %in% dt_naics_codes)) %>%
    distinct(NAICS_4) %>%
    pull(NAICS_4)

# get the list of CAs that need a downtown boundary
cmas = inputs$cma_sf
cmas = cmas %>%
    filter(CMATYPE != "B")

cma_puids = cmas %>%
    pull(CMAPUID, CMANAME)



# charlottetown_500_new = create_downtown_boundary(
#     cmapuid = "11105",
#     business_processed = inputs$business_processed,
#     cma_sf = inputs$cma_sf,
#     csd_sf = inputs$csd_sf,
#     da_sf = inputs$da_sf,
#     exclude = naics_to_remove,
#     buffer_dist = 500
# )

# st_write(charlottetown_500_new, "./outputs/spatialfiles/downtown_boundaries/charlottetown_500_new.geojson")



cma_puids = cma_puids[42]

# run downtown boundary creation for every CMA/CA and export each to its own file
for (cma_name in names(cma_puids)) {

    cmapuid = cma_puids[[cma_name]]

    message(sprintf("processing %s (%s)", cma_name, cmapuid))

    downtown = create_downtown_boundary(
        cmapuid = cmapuid,
        business_processed = inputs$business_processed,
        cma_sf = inputs$cma_sf,
        csd_sf = inputs$csd_sf,
        da_sf = inputs$da_sf,
        exclude = naics_to_remove,
        buffer_dist = 500
    )

    file_name = stringr::str_replace_all(cma_name, "[ -/]", "_")

    st_write(
        downtown,
        sprintf("./outputs/spatialfiles/downtown_boundaries/%s.geojson", file_name),
        delete_dsn = TRUE
    )
}
