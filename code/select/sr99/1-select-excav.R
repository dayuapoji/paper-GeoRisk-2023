# SELECT FEATURES - CUTTER #####################################################

# Cutter ----

var_cutter_rot <- data %>%
  select(contains('cutter')) %>%
  select(contains('speed')) %>%
  # select(!contains('copy')) %>% 
  .[ , which(.[1,] == 'min-1')] %>%
  set_df(.) %>%
  set_colnames(., 'CutterRotSpeed')

var_cutter_torque <- data %>%
  select(contains('cutter')) %>%
  select(contains('torque')) %>% 
  .[ , which(.[1,] == 'kN-m')] %>% 
  set_df(.) %>%
  set_colnames(., 'CutterTorque')
  
var_cutter_force <- data %>%
  select(contains('cutter')) %>%
  select(contains('force')) %>%
  select(contains('total')) %>%
  select(!contains('articulation')) %>%
  .[ , which(.[1,] == 'kN')] %>% 
  set_df(.) %>%
  set_colnames(., c('CutterForce'))

# Copy Cutter ----

var_copy_cutter_pres <- data %>%
  select(contains('cutter')) %>% 
  select(contains('copy')) %>%
  select(contains('pres')) %>%
  .[ , which(.[1,] == 'bar')] %>% 
  set_df(.) %>%
  set_colnames(., c('CopyCutterPres1', 'CopyCutterPres2')) %>%
  mutate(CopyCutterPres = rowSums(., na.rm = T))

var_copy_cutter_stroke <- data %>%
  select(contains('cutter')) %>% 
  select(contains('copy')) %>%
  select(contains('use')) %>%
  .[ , which(.[1,] == 'mm')] %>% 
  set_df(.) %>%
  set_colnames(., c('CopyCutterStroke1', 'CopyCutterStroke2')) %>%
  mutate(CopyCutterStroke = rowSums(., na.rm = T))

var_copy_cutter_pos <- data %>%
  select(contains('cutter')) %>% 
  select(contains('copy')) %>%
  select(contains('position')) %>%
  .[ , which(.[1,] == 'deg')] %>% 
  set_df(.) %>%
  set_colnames(., c('CopyCutterPos1', 'CopyCutterPos2')) %>%
  mutate(CopyCutterPos = rowSums(., na.rm = T))

# Bind -------------------------------------------------------------------------

group_excav <- cbind(var_cutter_rot, 
                     var_cutter_torque, 
                     var_cutter_force, 
                     var_copy_cutter_pres, 
                     var_copy_cutter_stroke, 
                     var_copy_cutter_pos)


