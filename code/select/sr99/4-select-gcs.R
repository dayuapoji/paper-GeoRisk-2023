# SELECT FEATURES - GROUND CONDITIONING ########################################

# Foam -------------------------------------------------------------------------

var_foam_pres <- data %>%
  select(contains('foam')) %>%
  select(contains('pres')) %>%
  select(!contains('liquid')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('FoamPres', 32)) %>%
  mutate(FoamPres = rowMeans(., na.rm = T))

var_foam_flow <- data %>%
  select(contains('foam')) %>%
  select(contains('flow')) %>%
  select(!contains('liquid')) %>%
  select(!contains('agent')) %>%
  select(!contains('total')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('FoamFlow', 32)) %>%
  mutate(FoamFlow = rowSums(., na.rm = T))

var_foam_vol <- data %>%
  select(contains('foam')) %>%
  select(contains('vol')) %>%
  select(!contains('water')) %>%
  select(!contains('liquid')) %>%
  select(!contains('agent')) %>%
  select(!contains('foam volume')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('FoamVol', 32)) %>%
  mutate(FoamVol = rowSums(., na.rm = T))

var_foam_fill_rate <- data %>%
  select(contains('foam')) %>%
  select(contains('fill rate')) %>%
  .[ , which(.[1,] == '%')] %>%
  set_df(.) %>%
  set_colnames(., 'FoamFillRate')

# Air --------------------------------------------------------------------------

var_air_pres <- data %>%
  select(contains('air')) %>%
  select(contains('pres')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('AirPres', 8)) %>%
  mutate(AirPres = rowMeans(., na.rm = T))

var_air_flow <- data %>%
  select(contains('air')) %>% 
  select(contains('flow')) %>%
  select(!contains('total')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('AirFlow', 32)) %>%
  mutate(AirFlow = rowSums(., na.rm = T))

var_air_vol <- data %>%
  select(contains('air')) %>%
  select(contains('vol')) %>%
  select(!contains('total')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('AirVol', 32)) %>%
  mutate(AirVol = rowSums(., na.rm = T))

# Foam Liquid (Solution) -------------------------------------------------------

var_foam_liquid_pres <- data %>%
  select(contains('foam liquid')) %>% 
  select(contains('pres')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('FoamLiquidPres', 32)) %>%
  mutate(FoamLiquidPres = rowMeans(., na.rm = T))

var_foam_liquid_flow <- data %>%
  select(contains('foam liquid')) %>% 
  select(contains('flow')) %>%
  select(!contains('water')) %>%
  select(!contains('bent')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('FoamLiquidFlow', 32)) %>%
  mutate(FoamLiquidFlow = rowSums(., na.rm = T))

var_foam_liquid_vol <- data %>%
  select(contains('foam liquid')) %>%
  select(contains('vol')) %>%
  select(!contains('water')) %>%
  select(!contains('bent')) %>%
  select(!contains('batch')) %>%
  select(!contains('total')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('FoamLiquidVol', 32)) %>%
  mutate(FoamLiquidVol = rowSums(., na.rm = T))

# Water ------------------------------------------------------------------------

var_water_vol <- data %>% 
  select(contains('foam water')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  .[ , 1:2] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('WaterVol', 2)) %>%
  mutate(WaterVol = rowSums(., na.rm = T))

# Foam Agent -------------------------------------------------------------------

var_foam_agent_flow <- data %>% 
  select(contains('foam agent')) %>% 
  select(contains('flow')) %>%
  select(!contains('volume')) %>%
  set_df(.) %>%
  set_colnames(., create_var_names('FoamAgentFlow', 6)) %>%
  mutate(FoamAgentFlow = rowSums(., na.rm = T))

var_foam_agent_vol <- data %>% 
  select(contains('foam agent')) %>% 
  select(contains('volume')) %>%
  select(!contains('flow')) %>%
  select(!contains('batch')) %>%
  .[ , c(13:14)] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('FoamAgentVol', 2)) %>%
  mutate(FoamAgentVol = rowSums(., na.rm = T))


# Polymer Agent  ---------------------------------------------------------------

var_polymer_agent_flow <- data %>%
  select(contains('polymer agent')) %>%
  select(!contains('volume')) %>%
  select(contains('flow')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('PolymerAgentFlow', 6)) %>%
  mutate(PolymerAgentFlow = rowSums(., na.rm = T))

var_polymer_agent_vol <- data %>%
  select(contains('polymer agent')) %>%
  select(contains('volume')) %>%
  select(!contains('flow')) %>%
  select(!contains('batch')) %>%
  .[ , c(13:18)] %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('PolymerAgentVol', 6)) %>%
  mutate(PolymerAgentVol = rowSums(., na.rm = T))
  

# Polymer ----------------------------------------------------------------------

var_polymer_pres <- data %>%
  select(contains('polymer')) %>%
  select(contains('pres')) %>%
  select(contains('port')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('PolymerPres', 15)) %>%
  mutate(PolymerPres = rowMeans(., na.rm = T))

var_polymer_flow <- data %>%
  select(contains('polymer')) %>% 
  select(contains('flow')) %>%
  select(contains('port')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('PolymerFlow', 15)) %>%
  mutate(PolymerFlow = rowSums(., na.rm = T))

var_polymer_vol <- data %>%
  select(contains('polymer')) %>% 
  select(contains('vol')) %>%
  select(contains('port')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('PolymerVol', 15)) %>%
  mutate(PolymerVol = rowSums(., na.rm = T))


# Slurry ---------------------------------------------------------------------

var_slurry_vol <- data %>%
  select(contains('slurry')) %>%
  select(!contains('bent')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., c(create_var_names('SlurryVol', 26), 'SlurryVol'))

var_slurry_flow <- data %>%
  select(contains('slurry')) %>%
  select(contains('flow')) %>%
  select(!contains('bent')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., c('SlurryFlow', create_var_names('SlurryFlow', 26)))

var_slurry_pres <- data %>%
  select(contains('slurry')) %>%
  select(contains('pres')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('SlurryPres', 26)) %>%
  mutate(SlurryPres = rowMeans(., na.rm = T))

# Additive ---------------------------------------------------------------------

var_additive_vol <- data %>%
  select(contains('additive')) %>%
  select(contains('vol')) %>%
  select(!contains('total')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('AdditiveVol', 15)) %>%
  mutate(AdditiveVol = rowSums(., na.rm = T))

var_additive_fill_rate <- data %>%
  select(contains('additive')) %>%
  select(contains('fill rate')) %>%
  .[ , which(.[1,] == '%')] %>%
  set_df(.) %>%
  set_colnames(., 'AdditiveFillRate')

# Bind -------------------------------------------------------------------------

group_gcs <- cbind(var_foam_pres, var_foam_flow, var_foam_vol, var_foam_fill_rate,
                   var_air_pres, var_air_flow, var_air_vol,
                   var_foam_liquid_pres, var_foam_liquid_flow, var_foam_liquid_vol,
                   var_water_vol,
                   var_foam_agent_flow, var_foam_agent_vol,
                   var_polymer_pres, var_polymer_flow, var_polymer_vol,
                   var_polymer_agent_flow, var_polymer_agent_vol,
                   var_slurry_pres, var_slurry_flow, var_slurry_vol,
                   var_additive_vol, var_additive_fill_rate)




                   
