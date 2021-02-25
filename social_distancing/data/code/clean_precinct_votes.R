CRAN_packages <- c("dplyr", "sf", "stringr", "usmap", "magrittr", "readr")
lapply(CRAN_packages, require, character.only = TRUE)

source("code/helpers/load_shp.R") #Define function to load shapefiles

main <- function() {
  planar_crs     <- st_crs(26915)              #Convert all projections to this planar crs system before comparing, as needed for sf::st_join. See https://github.com/r-spatial/sf/issues/493
  zip_folder_pre <- "external/precinct2016"    #Location of precinct shapefiles
  
  #Get list of precinct shapefiles and associated states
  pattern        <- "(^[a-z]{2})(_2016)\\.zip$"          #Pattern for precinct shapefile names, one per state
  zips           <- list.files(zip_folder_pre, pattern)  #Get list of precinct state files (42)
  states         <- str_sub(zips, 1, 2)                  #Get state abbreviations from zipfile name
  states_fips    <- usmap::fips(states)                  #Get state fips codes from abbreviations
  
  #Load precinct<->census_block_group overlap, used to allocate precinct votes to cbgs
  overlap        <- read_csv("output_local/precinct_cbg_overlap.csv") 
  
  #Loop through states, loading precinct shapefile, assigning POI, and saving precinct-level and CBG-level Republican vote shares
  append <- FALSE #Create a new file of matches with our first state
  for (i in seq_along(zips)) {
    df_pre <- load_shp(zip_folder=zip_folder_pre,
                        zip_file=zips[i], state=states[i], st_fips=states_fips[i], 
                        planar_crs=planar_crs, repair_geom=TRUE) %>% #Loads state's shapefile as sf object
      st_drop_geometry() %>%                                         #Drops geometry column
      select(starts_with("G16PRE")) %>%                              #Keeps only presidential election voting columns
      mutate(state_fips=states_fips[i], precinct_id=row_number())    #The shapefiles have very different ID names across files, so just create our own precinct id (unique only within state)
    
    #Clean and save vote shares at precinct-level
    df_pre %<>% clean_vote_share()
    df_pre %>% select(state_fips, precinct_id, r_share) %>%
      write_csv("output_local/precinct_voting.csv", append=append)
    
    #Allocate vote shares to CBG level and save
    overlap_st <- overlap %>% filter(state_fips==states_fips[i])
    df_pre %>% aggregate_cbg(overlap_st) %>% write_csv("output_local/cbg_voting.csv", append=append)
    
    #Remove completed state from overlap file
    overlap %<>% filter(state_fips!=states_fips[i])
    append <- TRUE #Add to our existing file of matches for all states after the first
  }
  return(NULL)
}

clean_vote_share <- function(df_pre) {  #Clean precinct-level votes to get republican vote share
  pres_vote_nms <- df_pre %>% select(starts_with("G16PRE")) %>% names()     #Get names of presidential vote columns
  pres_rep_vote_nm <- pres_vote_nms[startsWith(pres_vote_nms, "G16PRER")]   #Get name of Trump's vote share column
  stopifnot(length(pres_rep_vote_nm)==1)                                    #Check that we grabbed a single Trump vote share column
  df_pre %<>% mutate(total_vote = rowSums(as_tibble(.)[pres_vote_nms])) %>% #Get total presidential votes
    rename(trump_vote = all_of(pres_rep_vote_nm))                       %>% #Get votes for Trump
    mutate(r_share=trump_vote/total_vote)                               %>% #Get Republican vote share
    select(state_fips, precinct_id, r_share, trump_vote, total_vote)        #Keep desired variables
  return(df_pre)
}

aggregate_cbg <- function(df_pre, overlap) { #Allocate precinct-level votes to CBGs to get republican vote share
  df_cbg <- df_pre                                                                           %>% #Start with precinct-level votes
    merge(overlap, by=c("state_fips", "precinct_id"))                                        %>% #Merge on geographic overlap with CBGs
    mutate(trump_vote_wgtd = trump_vote*sh_precinct, total_vote_wgtd=total_vote*sh_precinct) %>% #Allocate precinct-level votes to intersections with CBGs based on geographic overlap
    group_by(census_block_group) %>%                                                             #Define CBGs as our new groups
    summarize(r_share = sum(trump_vote_wgtd)/sum(total_vote_wgtd)) %>% ungroup()                 #Calculate republican vote share by CBG
  return(df_cbg)
}

main()