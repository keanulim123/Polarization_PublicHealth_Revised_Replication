###################
### ENVIRONMENT ###
###################
import git
import imp
import os
import yaml

# LOAD GSLAB MAKE
ROOT = git.Repo('.', search_parent_directories = True).working_tree_dir 
f, path, desc = imp.find_module('gslab_make', [os.path.join(ROOT, 'lib')]) 
gs = imp.load_module('gslab_make', f, path, desc)

# SET DEFAULT PATHS
PATHS = {
    'config'          : '../config.yaml',
    'config_user'     : '../config_user.yaml',
    'input_dir'       : 'input', 
    'external_dir'    : 'external',
    'output_dir'      : 'output/',
    'output_local_dir': ['output_local'],       # Optional; include any local directories with outputs
    'pdf_dir'         : 'output/',
    'makelog'         : 'log/make.log',         # Set to '' to avoid writing log
    'output_statslog' : 'log/output_stats.log', # Set to '' to avoid writing log
    'output_headslog' : 'log/output_heads.log', # Set to '' to avoid writing log
    'source_maplog'   : 'log/source_map.log',   # Set to '' to avoid writing log
    'source_statslog' : 'log/source_stats.log', # Set to '' to avoid writing log
    'source_headslog' : 'log/source_heads.log'  # Set to '' to avoid writing log
}

### SET PATH MAPPINGS
PATH_MAPPINGS = { 
    'root': ROOT
}

### LOAD CONFIG USER 
gs.update_executables(PATHS)
PATH_MAPPINGS = gs.update_mappings(PATHS, PATH_MAPPINGS)

############
### MAKE ###
############

### START MAKE
gs.remove_dir(['input', 'external'])
gs.clear_dir(['output', 'log', 'output_local'])
gs.clear_dir(['temp']) # Uncomment for Stata scripts
gs.start_makelog(PATHS)

### GET INPUT FILES 
inputs = gs.link_inputs(PATHS, ['input.txt'], PATH_MAPPINGS)
externals = gs.link_externals(PATHS, ['external.txt'], PATH_MAPPINGS)
gs.write_source_logs(PATHS, inputs + externals)
gs.get_modified_sources(PATHS, inputs + externals)

### RUN SCRIPTS

# Clean Survey
gs.run_stata(PATHS,     'code/survey_clean.do')

# Create Precinct-CBG Mapping
gs.run_r(PATHS, 		'code/cbg_precinct_overlap.R')

# Clean Other Data
gs.run_r(PATHS,         'code/clean_precinct_votes.R')
gs.run_r(PATHS,         'code/clean_policies.R')
gs.run_r(PATHS, 		'code/clean_demographics.R')
gs.run_stata(PATHS, 	'code/clean_dta_shapefiles.do')

# Clean SafeGraph w/o Time Dimension
gs.run_python(PATHS, 	'code/geomapping_safegraph_county.py')
gs.run_r(PATHS, 		'code/geomapping_safegraph_precinct.R')

# Clean SafeGraph and Weather w/ Time Dimension
gs.run_r(PATHS,         'code/clean_weather.R')
gs.run_python(PATHS, 	'code/clean_safegraph_poi.py')
gs.run_r(PATHS, 		'code/clean_safegraph_daily_distancing.R')

# Merge all pieces together
gs.run_r(PATHS, 		'code/merge_all.R')

### LOG OUTPUTS
gs.log_files_in_output(PATHS)

### CHECK FILE SIZES
gs.check_module_size(PATHS)

### END MAKE
gs.end_makelog(PATHS)
