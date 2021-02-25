#Helper function to load shapefiles, requires "sf", "stringr", "dplyr", and "magrittr" packages to have been loaded
load_shp <- function(zip_folder, zip_file, state, st_fips, planar_crs, repair_geom) {
  #Unzip state's zipped shapefiles
  stub <- zip_file %>% str_replace("\\.zip$", "")              #Remove .zip suffix
  exdir <- sprintf("temp/%s", stub)                            #External directory in which to save unzipped shapefiles
  unzip(sprintf("%s/%s", zip_folder, zip_file),                #Unzip shapefiles
        exdir=exdir, overwrite=TRUE, unzip=getOption("unzip"))
  
  #Load state's shapefiles as sf
  suff <- "" #Two states have non-standard suffices on the shapefiles within their precinct zipped folder, deal with these
  if (grepl("precinct", zip_folder) && state=="md") suff <- "_w_ushouse" 
  if (grepl("precinct", zip_folder) && state=="va") suff <- "_president"
  
  shp <- st_read(sprintf("%s/%s%s.shp", exdir, stub, suff), stringsAsFactors=FALSE) #Load shapefile as sf object
  
  #Convert to planar coordinate system
  if (st_crs(shp) != planar_crs) { shp %<>% st_transform(crs=planar_crs) } #Transform to planar
  
  #Possibly repair geometries
  if (repair_geom) {
    invalid_inds <- !st_is_valid(shp)
    if (any(invalid_inds)) {
      print(sprintf("Repairing %f portion of geometries", mean(invalid_inds)))
      shp[invalid_inds, ] <- st_make_valid(shp[invalid_inds, ])
    }
  }
  
  #Remove folder of state's unzipped shapefiles, no longer needed
  unlink(exdir, recursive=TRUE) 
  
  return(shp) #Return loaded shapefile
}