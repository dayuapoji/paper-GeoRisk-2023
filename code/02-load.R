# LOAD DATA ####################################################################

# path to data folder
path <- '../../../../2-data/seattle/'

# TBM --------------------------------------------------------------------------

# switch to 1st edition since newer version of readr mess up non-ASCII 
readr::local_edition(1)

# data TBM
tbm_orig <- read_csv(paste0(path,'SR99.csv'))

# geologic segment
geo_layer <- read_csv('../../../../2-data/seattle/seattle-layer.csv')


