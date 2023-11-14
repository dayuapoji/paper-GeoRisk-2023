# SELECT FEATURES - BACKFILL GROUTING ##########################################

# Grout --------------------------------------------------------------------------

var_grout_pres <- data %>%
  select(contains('grout')) %>%
  select(contains('pres')) %>%
  # .[ , which(.[1,] == 'MPa')] %>%
  set_df(.) %>%
  set_colnames(., 'GroutPres')

var_grout_flow <- data %>%
  select(contains('grout')) %>% 
  select(contains('flow')) %>%
  select(!contains('A+B')) %>%
  # .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., c(create_var_names('GroutFlowA', 2),
                    create_var_names('GroutFlowB', 2),
                    'GroutFlow'))

var_grout_vol <- data %>%
  select(contains('grout')) %>%
  select(contains('vol')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  .[, 1:22] %>%
  set_df(.) %>%
  set_colnames(., c(create_var_names('GroutVolA', 11),
                    create_var_names('GroutVolB', 11))) %>%
  mutate(GroutVol = rowSums(., na.rm = T))

var_grout_fill_rate <- data %>%
  select(contains('grout')) %>%
  select(contains('fill rate')) %>%
  .[ , which(.[1,] == '%')] %>%
  set_df(.) %>%
  set_colnames(., 'GroutFillRate')

# Bind -------------------------------------------------------------------------

group_grout <- cbind(var_grout_pres, 
                     var_grout_flow, 
                     var_grout_vol, 
                     var_grout_fill_rate)
