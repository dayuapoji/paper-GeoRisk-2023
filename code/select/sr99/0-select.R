# SELECT FEATURES ##############################################################

# Remove Unused Columns and Rows -----------------------------------------------

data <- data_orig %>% 
  
  # remove columns with 'NA' unit
  .[ , which(!is.na(.[1, ]))] %>% # 45 features
  # remove columns with categorical data
  .[, which(.[1,] != 'on/off')] %>% # 2436 features
  .[, which(.[1,] != 'off/on')] %>%
  select(!contains('status')) %>% # 1 features
  # remove unused columns, i.e. contains (Not Used) & [DEL]
  select(!contains('(Not Used)')) %>% # 8 features
  select(!contains('[DEL]')) %>% # 249 features
  # remove calculated values
  select(!contains('calc')) %>% # 5 features
  # remove columns contain information on cumulative
  select(!contains('cumulative')) %>% # 208 features
  select(!contains('accumulated')) %>%
  select(!contains('count')) %>% # 110 features
  select(!contains('counter')) %>%
  select(!contains('day')) %>%
  select(!contains('shift')) %>%
  # remove columns contain information on target
  select(!contains('setting')) %>%
  select(!contains('target')) %>%
  select(!contains('tgt')) %>%
  # remove columns contain information on capacity
  select(!contains('capacity')) %>%
  select(!contains('range')) %>%
  select(!contains('level')) %>%
  select(!contains('max')) %>%
  select(!contains('min')) %>%
  select(!contains('limit'))


# Position ---------------------------------------------------------------------

ring <- data %>%
  select(contains('ring no')) %>%
  set_df(.) %>%
  set_colnames(., 'Ring')

chainage <- data %>%
  select(contains('chainage')) %>% 
  .[ , which(.[1,] == 'ft')] %>% 
  set_df(.) %>%
  set_colnames(., c('ChainageHead',
                    'ChainageArt',
                    'ChainageTail'))


                  








