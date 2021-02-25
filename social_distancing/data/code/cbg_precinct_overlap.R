CRAN_packages <- c("dplyr", "sf", "stringr", "usmap", "magrittr", "readr")
lapply(CRAN_packages, require, character.only = TRUE)

source("code/helpers/load_shp.R") #Define function to load shapefiles

main <- function() {
  planar_crs     <- st_crs(26915)              #Convert all projections to this planar crs system before comparing, as needed for sf::st_join. See https://github.com/r-spatial/sf/issues/493
  zip_folder_pre <- "external/precinct2016"    #Location of precinct shapefiles
  
  #Get list of precinct shapefiles and associated states
  pattern        <- "(^[a-z]{2})(_2016)\\.zip$"          #Pattern for precinct shapefile names, one per state
  zips           <- list.files(zip_folder_pre, pattern)  #Get list of precinct state files (38)
  states         <- str_sub(zips, 1, 2)                  #Get state abbreviations from zipfile name
  states_fips    <- usmap::fips(states)                  #Get state fips codes from abbreviations
  
  zip_folder_cbg <- "external/cbg_shapefiles"  #Location of census block group shapefiles
  
  #Loop through states, loading precinct and CBG shapefiles, and calculating geographic overlap
  append <- FALSE #Create a new file of matches with our first state
  for (i in seq_along(zips)) {
    shp_pre <- load_shp(zip_folder=zip_folder_pre,
                        zip_file=zips[i], state=states[i], st_fips=states_fips[i], 
                        planar_crs=planar_crs, repair_geom=TRUE)     %>% #Loads state's precinct shapefile as sf object, repairing geometries where needed
      transmute(state_fips=states_fips[i], precinct_id=row_number()) %>% #The shapefiles have very different ID names across files, so just create our own precinct id (unique only within state)
      st_set_agr("constant") %>%                                         #Specifies that attributes are constant across sub-geographies
      st_set_precision(100000) %>% st_make_valid() %>% st_buffer(0.0)    #Further geography cleaning
      
    zip_cbg <- sprintf("tl_2019_%s_bg", states_fips[i])
    shp_cbg <- load_shp(zip_folder=zip_folder_cbg,
                       zip_file=zip_cbg, state=states[i], st_fips=states_fips[i], 
                       planar_crs=planar_crs, repair_geom=TRUE) %>%      #Loads state's CBG shapefile as sf object, repairing geometries where needed
      transmute(census_block_group=GEOID) %>%                            #Drops all previously existing columns
      st_set_agr("constant") %>%                                         #Specifies that attributes are constant across sub-geographies
      st_set_precision(100000) %>% st_make_valid() %>% st_buffer(0.0)    #Further geography cleaning
    
    save_overlap(shp_pre=shp_pre, shp_cbg=shp_cbg, append=append)        #Calculates geographic overlap between state's precincts and CBGs and saves output
    append <- TRUE #Add to our existing file of matches for all states after the first
  }
  return(NULL)
}

save_overlap <- function(shp_pre, shp_cbg, append) {
  #Match POI->precinct and save resulting matches
  intersection <- shp_pre %>% st_intersection(shp_cbg)               #Calculate geographic intersection between state's precincts and CBGs
  intersection$area_intersection <- as.vector(st_area(intersection)) #Get land area of intersection
  intersection %<>% st_drop_geometry() %>%                                                           #Drop geometry
    group_by(census_block_group) %>% mutate(area_cbg = sum(area_intersection)) %>% ungroup() %>%     #Get total area of intersections by CBG
    group_by(precinct_id)   %>% mutate(area_precinct = sum(area_intersection)) %>% ungroup() %>%     #Get total area of intersections by precinct
    transmute(state_fips=state_fips, precinct_id=precinct_id, census_block_group=census_block_group, #Keeping only ID columns and...
              sh_cbg=as.vector(area_intersection/area_cbg),                                          # share of CBG matches covered by intersection and...
              sh_precinct=as.vector(area_intersection/area_precinct))                                # share of precint matches covered by intersection
  intersection %>% write_csv("output_local/precinct_cbg_overlap.csv", append=append) #Save output to file
  return(NULL)
}

main()