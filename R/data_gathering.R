library(tidyverse)
library(sf)
library(msmdata)

dotenv::load_dot_env("C:/Users/atabascio/MMSDashboards/secrets/.env")

# add spacing to camelCase / number-adjacent / comma-separated column names
space_out_colnames = function(x) {
    x = gsub(",(?!\\s)", ", ", x, perl = TRUE)          # space after commas
    x = gsub("([a-z])([A-Z])", "\\1 \\2", x)             # lower -> Upper boundary
    x = gsub("([A-Z]+)([A-Z][a-z])", "\\1 \\2", x)       # acronym -> Word boundary
    x = gsub("([A-Za-z])([0-9])", "\\1 \\2", x)          # letter -> digit boundary
    x = gsub("([0-9])([A-Za-z])", "\\1 \\2", x)          # digit -> letter boundary
    x = gsub("\\bAnd\\b", "and", x)                      # lowercase the conjunction
    trimws(x)
}

# usage: colnames(df) = space_out_colnames(colnames(df))
# or: df = rename_with(df, space_out_colnames)

# load in the Downtown study areas
downtowns = read_blob_data("Data/spatialfiles/downtowns.parquet", type = "geoparquet") %>%
    mutate(across(where(is.character), ~{y = iconv(.x, from = "UTF-8", to = "windows-1252"); Encoding(y) = "UTF-8"; y}))

# vector of downtown study areas
downtowns_vec = c("Calgary", "Charlottetown", "Edmonton", "Gatineau", "Halifax", 
"Hamilton", "Lethbridge", "London", "Moncton", "Montréal", "North Bay", "Ottawa",
"Prince George", "Saskatoon", "St. John's", "Toronto", "Vancouver", "Whitehorse",
"Winnipeg", "Wood Buffalo")

# filter the downtowns sf based on the vector
downtowns_filtered = downtowns %>%
    filter(DTUID == "462660231" | DTNAME %in% downtowns_vec)

# load demostats data
message("loading: demostats")
dataset_file_path = paste0("Data/eadata26/demostats/demostats_26.csv")
demostats = read_blob_data(dataset_file_path, type = "csv")

meta_file_path = paste0("Data/eadata26/demostats/demostats_26_meta.csv")
demostats_meta = read_blob_data(meta_file_path, type = "csv")

# load in Business Data
businesses = read_blob_data("Data/eadata26/businesses/businesses_26.csv")
naics = read_blob_data("Data/NAICS_codes_complete.csv")

businesses = process_business_data(businesses, naics)

================================================================================

# vector of variables for foundational tab
vars_vec = c("ECYALSQKM", "ECYBASPOP", "ECYBASHHD", "ECYPTAAVG", "ECYHSZAVG",
"ECYHNI_020", "ECYHNI2040", "ECYHNI4060", "ECYHNI6080", "ECYHNIX100", "ECYHNIX125", 
"ECYHNIX150", "ECYHNIX200", "ECYHNIX300", "ECYHNI300P","ECYPNIAVG", "ECYVISVM", 
"ECYAIDABO", "ECYPIMIM", "ECYPIMP01", "ECYPIM0110", "ECYPIM1115", "ECYPIM1621", 
"ECYPIM22CY", "ECYPIMNPER", "ECYGEN1GEN", "ECYGEN2GEN", "ECYGEN3GEN")

overview_cma_cma = map2(
    downtowns_filtered$DTNAME,
    seq_len(nrow(downtowns_filtered)),
    ~process_ea_data(.x, downtowns_filtered[.y, ], demostats, demostats_meta, vars_vec,
                      paste0(.x, " CMA"), "CMACA", buffer = 0)
) %>%
  list_rbind()

# calculate population density
overview_cma = overview_cma %>%
  mutate(
    PopulationDensity = TotalPopulation / TotalLandArea
  ) %>%
  select(
    Area, TotalLandArea, TotalPopulation, PopulationDensity, everything()
  )

colnames(overview_cma) = space_out_colnames(colnames(overview_cma))



# economic vitality vector
vars_vec = c("ECYEDUNCDD", "ECYEDUHSCE", "ECYEDUCOLL", "ECYEDUUD",
             "ECYACTINLF", "ECYACTEMP", "ECYACTUEMP",
             "ECYOCCMGMT", "ECYOCCBFAD", "ECYOCCNSCI", "ECYOCCHLTH", "ECYOCCSSER",
             "ECYOCCARTS", "ECYOCCSERV", "ECYOCCTRAD", "ECYOCCPRIM", "ECYOCCSCND",
             "ECYINDAGRI", "ECYINDMINE", "ECYINDUTIL", "ECYINDCSTR", "ECYINDMANU",
             "ECYINDWHOL", "ECYINDRETL", "ECYINDTRAN", "ECYINDINFO", "ECYINDFINA",
             "ECYINDREAL", "ECYINDPROF", "ECYINDMGMT", "ECYINDADMN", "ECYINDEDUC",
             "ECYINDHLTH", "ECYINDARTS", "ECYINDACCO", "ECYINDOSER", "ECYINDPUBL"
)

economic_vitality_cma = map2(
    downtowns_filtered$DTNAME,
    seq_len(nrow(downtowns_filtered)),
    ~process_ea_data(.x, downtowns_filtered[.y, ], demostats, demostats_meta, vars_vec,
                      paste0(.x, " CMA"), "CMACA", buffer = 0)
) %>%
  list_rbind()

colnames(economic_vitality_cma) = space_out_colnames(colnames(economic_vitality_cma))


# Transportation vector
vars_vec = c("ECYTRADRIV", "ECYTRAPSGR", "ECYTRAPUBL", "ECYTRAWALK", "ECYTRABIKE")

transportation_cma = map2(
    downtowns_filtered$DTNAME,
    seq_len(nrow(downtowns_filtered)),
    ~process_ea_data(.x, downtowns_filtered[.y, ], demostats, demostats_meta, vars_vec,
                      paste0(.x, " CMA"), "CMACA", buffer = 0)
) %>%
  list_rbind()

colnames(transportation_cma) = space_out_colnames(colnames(transportation_cma))



# Housing vector
vars_vec = c("ECYHSZHHD", "ECYHSZ1PER", "ECYHSZ2PER", "ECYHSZ3PER", "ECYHSZ4PER", 
            "ECYHSZ5PER", "ECYTENOWN", "ECYTENRENT", "ECYPOCP60", "ECYPOC6180",
            "ECYPOC8190", "ECYPOC9100", "ECYPOC0105","ECYPOC0610", "ECYPOC1115",
            "ECYPOC1621", "ECYPOC22P","ECYSTYSING", "ECYSTYSEMI", "ECYSTYROW", "ECYSTYAPT", 
            "ECYSTYAP5P", "ECYSTYAPU5", "ECYSTYDUPL"
)

housing_cma = map2(
    downtowns_filtered$DTNAME,
    seq_len(nrow(downtowns_filtered)),
    ~process_ea_data(.x, downtowns_filtered[.y, ], demostats, demostats_meta, vars_vec,
                      paste0(.x, " CMA"), "CMACA", buffer = 0)
) %>%
  list_rbind()

colnames(housing_cma) = space_out_colnames(colnames(housing_cma))

# export all datasets
write_csv(overview_cma, "./sdsc-data/outputs/overview_data.csv")
write_csv(economic_vitality_cma, "./sdsc-data/outputs/economic_data.csv")
write_csv(transportation_cma, "./sdsc-data/outputs/transportation_data.csv")
write_csv(housing_cma, "./sdsc-data/outputs/housing_data.csv")


# Business data processing
