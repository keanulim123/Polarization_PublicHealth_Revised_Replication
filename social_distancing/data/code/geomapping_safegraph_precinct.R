CRAN_packages <- c("dplyr", "sf", "stringr", "usmap", "magrittr", "readr")
lapply(CRAN_packages, require, character.only = TRUE)

source("code/helpers/load_shp.R") # Define function to load shapefiles
source("../lib/write_gslab_table.R")

main <- function() {
  planar_crs     <- st_crs(26915)              # Convert all projections to this planar crs system before comparing, as needed for sf::st_join. See https://github.com/r-spatial/sf/issues/493
  zip_folder_pre <- "external/precinct2016"    # Location of precinct shapefiles
  
  # Get list of precinct shapefiles and associated states
  pattern        <- "(^[a-z]{2})(_2016)\\.zip$"          # Pattern for precinct shapefile names, one per state
  zips           <- list.files(zip_folder_pre, pattern)  # Get list of precinct state files (42)
  states         <- str_sub(zips, 1, 2)                  # Get state abbreviations from zipfile name
  states_fips    <- usmap::fips(states)                  # Get state fips codes from abbreviations
  
  # Load POI to county matches
  poi_county <- load_poi_county(states_fips = states_fips, planar_crs = planar_crs)
  n_county   <- nrow(poi_county)
  
  # Loop through states, loading precinct shapefile, assigning POI, and saving matches
  append <- FALSE # Create a new file of matches with our first state
  for (i in seq_along(zips)) {
    shp_pre <- load_shp(zip_folder = zip_folder_pre,
                        zip_file = zips[i], state = states[i], st_fips = states_fips[i], 
                        planar_crs = planar_crs, repair_geom = TRUE) %>% 					# Loads state's shapefile as sf object, repairing geometries where needed
      					transmute(precinct_id = row_number())                           	# The shapefiles have very different ID names across files, so just create our own precinct id (unique only within state)
    
    poi_county %<>% match_poi_precinct(shp_pre = shp_pre, st_fips = states_fips[i], append = append)    # Matches POI to state shapefile, saves output, and removes state's POI from poi_county
    append <- TRUE # Add to our existing file of matches for all states after the first
  }
  # Save shares matched uniquely, and share dropped for multiple matches
  sh_precinct <- nrow(read_csv("output_local/placeid_precinct.csv")) / n_county
  duplicates  <- read_csv("output_local/placeid_precinct_duplicates.csv")
  stopifnot(all(duplicates$n == 2))
  sh_duplicate <- (nrow(duplicates) / 2) / n_county
  write_gslab_table(as.matrix(c(sh_precinct, sh_duplicate)), "output/share_precinct_matched.txt", "<tab:share_precinct_matched>")
}

load_poi_county <- function(states_fips, planar_crs) {
  # Load POI to County mapping produced by geomapping_safegraph_county.py, 
  # restricted to subset of states in states_fips
  poi_county <- read_csv("output_local/placeid_county.csv",
                         col_types = cols_only(safegraph_place_id = "c", state_fips = "c", 
                                             latitude = "d", longitude = "d")) %>%
    filter(state_fips %in% states_fips) %>%  # Only keep states for which we have precinct shapefiles
    st_as_sf(coords = c("longitude", "latitude"), agr = c("constant", "identity"), crs = 4326) %>% # Convert to sf object
    st_transform(crs = planar_crs) # Convert to planar coordinate system
  return(poi_county)
}

match_poi_precinct <- function(poi_county, shp_pre, st_fips, append) {
  # Match POI->precinct and save resulting matches
  poi_precinct <- poi_county %>% filter(state_fips == st_fips)     %>% # Subset POI to given state
    st_join(shp_pre, join = st_within, left = FALSE)               %>% # Find POI points within precinct shapefiles
    group_by(safegraph_place_id) %>% mutate(n = n()) %>% ungroup() %>% # Count number of matches for each POI
    st_drop_geometry()                                                 # Remove lat/long geometry column
  
  poi_precinct %>% filter(n == 1)                                    %>% # Drop POI matched to multiple precincts
    select(safegraph_place_id, state_fips, precinct_id)            %>% # Keep only certain variables
    write_csv("output_local/placeid_precinct.csv", append = append)      # Save matches
  
  poi_precinct %>% filter(n > 1)                                   %>% # Keep only POI matched to multiple precincts
    select(safegraph_place_id, state_fips, precinct_id, n)         %>% # Keep only certain variables
    write_csv("output_local/placeid_precinct_duplicates.csv", append = append) # Save duplicate matches
  
  # Remove completed state from dataset of remaining POIs
  poi_county %<>% filter(state_fips != st_fips) 
  return(poi_county)
}

main()