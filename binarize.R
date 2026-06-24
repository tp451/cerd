# ==============================================================================
# CERD — Data Binarization Script
# ==============================================================================
# Author:       Thorben Pelzer
# Purpose:      Read the collection of CSV / GeoJSON source files, perform the
#               same loading and combination steps the web interface relies on,
#               and serialise the resulting objects into a single, lightweight
#               binary bundle (cerd_data.rds).
#
#               app.R then loads this bundle instead of parsing every CSV and
#               re-running the joins on each start-up, which is considerably
#               faster and lighter.
#
# Usage:        Re-run this script (from the repository root) whenever any of
#               the underlying CSV / GeoJSON files change:
#                   Rscript binarize.R
#               or, inside an R session:
#                   source("binarize.R")
# ==============================================================================

library(tidyverse)   # read_csv, dplyr verbs
library(sf)          # read_sf for the spatial layers

# ------------------------------------------------------------------------------
# Data Loading (mirrors the original loading block in app.R)
# ------------------------------------------------------------------------------

world_1938 <- read_sf("CERD/world_1938.geojson")
china_1928 <- read_sf("china_tw_combined.geojson")
taiwan_1946 <- read_sf("taiwan_1946.geojson")

# Persons
CERD_persons <- read_csv("CERD/pelzer_cerd_180_persons_bio.csv", show_col_types = F) %>%
left_join(read_csv("CERD/pelzer_cerd_180_persons_societies.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
left_join(read_csv("CERD/pelzer_cerd_180_persons_ids.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
left_join(read_csv("CERD/pelzer_cerd_180_persons_names.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
unique()
CERD_locations <-read_sf("CERD/pelzer_cerd_180_locations.geojson") %>%
rename(longlat=geometry)
CERD_degrees <-read_csv("CERD/pelzer_cerd_180_degrees.csv", show_col_types = F)
CERD_colleges <-read_csv("CERD/pelzer_cerd_180_colleges.csv", show_col_types = F)
CERD_jobs <-read_csv("CERD/pelzer_cerd_180_jobs.csv", show_col_types = F)
CERD_employers <-read_csv("CERD/pelzer_cerd_180_employers.csv", show_col_types = F)
CERD_sources <-read_csv("CERD/pelzer_cerd_180_persons_sources.csv", show_col_types = F)

# CERD-Taiwan
CERD_TW_persons <- read_csv("CERD_Taiwan/CERD_TW_persons_bio.csv", show_col_types = F) %>%
left_join(read_csv("CERD_Taiwan/CERD_TW_persons_societies.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
left_join(read_csv("CERD_Taiwan/CERD_TW_persons_ids.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
left_join(read_csv("CERD_Taiwan/CERD_TW_persons_names.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
unique()
CERD_TW_locations <-read_sf("CERD_Taiwan/CERD_TW_locations.geojson") %>%
rename(longlat=geometry)
CERD_TW_degrees <-read_csv("CERD_Taiwan/CERD_TW_degrees.csv", show_col_types = F)
CERD_TW_jobs <-read_csv("CERD_Taiwan/CERD_TW_jobs.csv", show_col_types = F)
CERD_TW_employers <-read_csv("CERD_Taiwan/CERD_TW_employers.csv", show_col_types = F)

CERD_TW_sources <-read_csv("CERD_Taiwan/CERD_TW_persons_sources.csv", show_col_types = F)

CERD_persons <- CERD_persons %>%
rbind(CERD_TW_persons) %>%
unique()

CERD_sources <- CERD_sources %>%
rbind(CERD_TW_sources) %>%
unique()

CERD_locations <- CERD_locations %>%
rbind(CERD_TW_locations) %>%
unique()

CERD_degrees <- CERD_degrees %>%
rbind(CERD_TW_degrees) %>%
unique()

CERD_jobs <- CERD_jobs %>%
rbind(CERD_TW_jobs) %>%
unique()

CERD_employers <- CERD_employers %>%
rbind(CERD_TW_employers) %>%
unique()

# ------------------------------------------------------------------------------
# Serialisation
# ------------------------------------------------------------------------------
# Bundle the final objects (exactly those app.R expects as globals) into a
# single named list and write one compressed .rds file.

cerd_data <- list(
  world_1938     = world_1938,
  china_1928     = china_1928,
  taiwan_1946    = taiwan_1946,
  CERD_persons   = CERD_persons,
  CERD_locations = CERD_locations,
  CERD_degrees   = CERD_degrees,
  CERD_colleges  = CERD_colleges,
  CERD_jobs      = CERD_jobs,
  CERD_employers = CERD_employers,
  CERD_sources   = CERD_sources
)

out_file <- "cerd_data.rds"
saveRDS(cerd_data, out_file, compress = "xz")

message(sprintf("Wrote %s (%.1f MB) with %d objects.",
                out_file,
                file.size(out_file) / 1024^2,
                length(cerd_data)))
