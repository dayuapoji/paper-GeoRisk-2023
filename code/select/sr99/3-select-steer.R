# SELECT FEATIRES - STEERING ###################################################

# Thrust Difference ---------------------------------------------------------         

var_diff_force <- data %>%
  select(contains('cutter')) %>%
  select(contains('force')) %>%
  select(contains('diff')) %>%
  select(!contains('articulation')) %>%
  .[ , which(.[1,] == 'kN')] %>% 
  set_df(.) %>%
  set_colnames(., c('CutterForceDifLR', 'CutterForceDifTB'))

var_diff_stroke <- data %>%
  select(contains('thrust')) %>%
  select(contains('dif')) %>%
  select(!contains('start') & !contains('finish')) %>%
  .[ , which(.[1,] == 'mm')] %>%
  set_df(.) %>%
  set_colnames(., c('ThrustStrokeDifLR', 'ThrustStrokeDifTB'))

# Articulation --------------------------------------------------------------         

var_artic_pres <- data %>%
  select(contains('thrust') | contains('jack')) %>%
  select(contains('pres')) %>%
  select(contains('artic')) %>%
  select(!contains('erector') & !contains('traction') & !contains('conveyor') & !contains('(rod)')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., c('ArticThrustPres', 
                    create_var_names('ArticThrustPres', 2)))

var_artic_force <- data %>%
  select(contains('thrust') | contains('jack')) %>%
  select(contains('force')) %>%
  select(contains('artic')) %>%
  .[ , which(.[1,] == 'kN')] %>%
  set_df(.) %>%
  set_colnames(., 'ArticThrustForce')

var_artic_stroke <- data %>%
  select(contains('thrust') | contains('jack') | contains('net stroke')) %>%
  select(contains('stroke')) %>%
  select(contains('artic')) %>%
  select(!contains('erector')) %>%
  select(!contains('robotec')) %>%
  .[ , which(.[1,] == 'mm')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('ArticThrustStroke', 4)) %>%
  mutate(ArticThrustStrokeAve = rowMeans(., na.rm = T))

var_artic_angle <- data %>% 
  select(contains('articulation')) %>%
  .[ , which(.[1,] == 'deg')] %>%
  set_df(.) %>%
  set_colnames(., c('ArticAngleLR', 'ArticAngleTB'))

# Shield Attitude --------------------------------------------------------------         

var_shield_attitude <- data %>%
  select(contains('pitch') | contains('roll') | contains('bear')) %>%
  select(contains('front') | contains('rear')) %>%
  select(!contains('[c]pitch') & !contains('[c]roll') & !contains('dev')) %>%
  .[ , which(.[1,] == 'deg')] %>% 
  set_df(.) %>%
  set_colnames(., c('PitchFront', 'RollFront', 'YawFront',
                    'PitchRear', 'RollRear', 'YawRear'))

# Deviations -------------------------------------------------------------------         

var_shield_deviation <- data %>%
  select(contains('deviation')) %>% 
  .[ , which(.[1,] == 'mm')] %>% 
  set_df(.) %>%
  set_colnames(., c('ArticDevLR', 'ArticDevTB', 
                    'DevHeadH', 'DevArtH', 'DevTailH',
                    'DevHeadV', 'DevArtV', 'DevTailV'))

# Bind -------------------------------------------------------------------------

group_steer <- cbind(var_diff_force, var_diff_stroke,
                     var_artic_pres, var_artic_force, 
                     var_artic_stroke, var_artic_angle,
                     var_shield_attitude, 
                     var_shield_deviation)

