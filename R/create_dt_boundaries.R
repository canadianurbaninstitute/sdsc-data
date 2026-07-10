library(tidyverse)
library(sf)

## use local msmdata until push ##
devtools::load_all("C:/Users/atabascio/MMSDashboards/msmdata")

# Load env variables
dotenv::load_dot_env("C:/Users/atabascio/MMSDashboards/secrets/.env")

## Internal functions


#' Get CMA inputs
#' 
#' loads and preparesbusiness data clipped to the cma area
#' 
#' @param cmauid CMA code for the downtown area
#' @param exclude A vector of naics codes to exclude similar to what statscan did (hospitals, universities, etc.)
#' 
#' @return the business data clipped to the cma
#' @keywords internal
prepare_business_data = function(cmauid, exclude = NULL){

     # check that environment variables are there to access Azure
    # Get Azure credentials from environment
    storage_account <- Sys.getenv("AZURE_STORAGE_ACCOUNT")
    storage_key <- Sys.getenv("AZURE_STORAGE_KEY")
    container_name <- Sys.getenv("AZURE_CONTAINER_NAME")

    # Validate credentials
    if (storage_account == "" || storage_key == "") {
        stop("AZURE_STORAGE_ACCOUNT and AZURE_STORAGE_KEY must be set in .env file", call. = FALSE)
    }
    if (container_name == "") {
        stop("AZURE_CONTAINER_NAME must be set in .env file", call. = FALSE)
    }

    message("loading cma and business data")
    # load in the cma and filter using the cmauid
    cma_sf = read_blob_data("Data/spatialfiles/cma.parquet", type = "geoparquet")
    cma_sf = cma_sf %>% filter(CMAUID == cmauid)
    stopifnot(nrow(cma_sf) == 1)

    # load and clip the ea business data
    business_data = read_blob_data("Data/eadata26/businesses/businesses_26.csv")
    naics = read_blob_data("Data/NAICS_codes_complete.csv")

    business_data = process_business_data(business_data, naics)

    if(!is.null(exclude)){
        message("removing naics codes")

        business_data = business_data %>%
            filter(!(NAICS_4 %in% exclude))
    }

    message("Clipping business data to CMA/CA")
    business_data = st_transform(business_data, st_crs(cma_sf))
    business_cma_int = st_intersection(business_data, cma_sf)

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
#'
#' @return a terra SpatRaster of job density, at `cell_size` resolution
#' @keywords internal
compute_density_surface = function(points, bandwidth, weight_field = NULL, window = NULL){

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
        eps = 10,
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






#' create downtown boundary
#' 
#' This internal function replicates the creation of the downtown boundaries
#' developed by statistics back in 2016 to derive a consistent definition of a core
#' downtown boundary using job data as the primary data point along with manual
#' refinement when needed
#' 
#' @param cmauid CMA code the downtown
#' 
#' @return a spatial file of the downtown boundary
#' @keywords Internal
create_downtown_boundary = function(cmauid){




}






# load statscan downtown boundaries
dt_boundaries = read_blob_data("Data/spatialfiles/downtowns.parquet", type = "geoparquet")

# load CMA file for cities
cma_boundaries = read_blob_data("Data/spatialfiles/cma.parquet", type = "geoparquet")
