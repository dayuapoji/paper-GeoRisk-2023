# SELECT FEATURES - THRUST #####################################################

var_thrust_speed <- data %>%
  select(contains('thrust') | contains('jack')) %>%
  select(contains('speed')) %>%
  # select(contains('average')) %>%
  .[ , which(.[1,] == 'mm/min')] %>%
  set_df(.) %>%
  set_colnames(., c('AdvanceRate', create_var_names('ThrustSpeed', 8)))

var_thrust_pres <- data %>%
  select(contains('thrust') | contains('jack')) %>%
  select(contains('pres')) %>%
  select(!contains('artic') & 
           !contains('erector') & 
           !contains('traction') & 
           !contains('conveyor') & 
           !contains('(rod)')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., c('ThrustPres', create_var_names('ThrustPres', 8)))

var_thrust_force <- data %>%
  select(contains('thrust') | contains('jack')) %>%
  select(contains('force')) %>%
  select(!contains('artic')) %>%
  .[ , which(.[1,] == 'kN')] %>%
  set_df(.) %>%
  set_colnames(., 'ThrustForce')

var_thrust_stroke <- data %>%
  select(contains('thrust') | contains('jack') | contains('net stroke')) %>%
  select(contains('stroke')) %>%
  select(!contains('erector')) %>%
  select(!contains('robotec')) %>%
  select(!contains('artic')) %>%
  select(!contains('dif')) %>%
  .[ , which(.[1,] == 'mm')] %>%
  set_df(.) %>%
  set_colnames(., c(create_var_names('ThrustStroke', 8), 
                    'ThrustStrokeNet'))

# Bind -------------------------------------------------------------------------

group_advance <- cbind(var_thrust_speed, 
                       var_thrust_pres,
                       var_thrust_force, 
                       var_thrust_stroke)
