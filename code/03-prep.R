# PREPARE DATA #################################################################
 

# Prepare TBM Data -------------------------------------------------------------

tbm_list <- select_tbm_data(tbm_orig)

tbm_data <- tbm_list$data

# Soil Labeles -----------------------------------------------------------------

# initialize soil layer columns
tbm_data$CSG <- NA
tbm_data$CSF <- NA
tbm_data$CCS <- NA
tbm_data$TD  <- NA
tbm_data$TLD <- NA

# soil label
for (i in 1:nrow(tbm_data)) {
  for (j in 1:nrow(geo_layer)) {
    if (tbm_data$Chainage[i] > geo_layer$ChainageStart[j] &
        tbm_data$Chainage[i] <= geo_layer$ChainageEnd[j]) {
      tbm_data$CSG[i] <- geo_layer$CSG[j]
      tbm_data$CSF[i] <- geo_layer$CSF[j]
      tbm_data$CCS[i] <- geo_layer$CCS[j]
      tbm_data$TD[i]  <- geo_layer$TD[j]
      tbm_data$TLD[i] <- geo_layer$TLD[j]
    } 
  }
}

# dominant soil unit
for (i in 1:nrow(tbm_data)) {
  if(is.na(tbm_data[i, 17])) { 
    tbm_data$SoilUnit[i] = 'Unknown'
  } else {
    tbm_data$SoilUnit[i] = colnames(tbm_data[, c('CCS', 'CSF', 'CSG', 'TLD', 'TD')])[which.max(tbm_data[i, c('CCS', 'CSF', 'CSG', 'TLD', 'TD')])]
  }
}

# percentage of dominant soil unit
for (i in 1:nrow(tbm_data)) {
  if(is.na(tbm_data[i, 17])) { 
    tbm_data$SoilUnitPercent[i] = 0
  } else {
    tbm_data$SoilUnitPercent[i] = tbm_data[i, c('CCS', 'CSF', 'CSG', 'TLD', 'TD')][which.max(tbm_data[i, c('CCS', 'CSF', 'CSG', 'TLD', 'TD')])][[1]]
  }
}


