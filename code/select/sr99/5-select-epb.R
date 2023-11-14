# SELECT FEATURES - EPB ########################################################

var_chamber_pres <- data %>%
  select(contains('soil')) %>%
  select(contains('bulkhead')) %>%
  select(contains('No.')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('ChamberPres', 12)) %>%
  mutate(ChamberPres = rowMeans(., na.rm = T))
  
var_circum_pres <- data %>%
  select(contains('soil')) %>%
  select(contains('outer')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('CircumPres', 6)) %>%
  mutate(CircumPres = rowMeans(., na.rm = T))

var_backfill_pres <- data %>%
  select(contains('soil')) %>%
  select(contains('back-fill')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., create_var_names('BackFillPres', 6)) %>%
  mutate(BackFillPres = rowMeans(., na.rm = T))

# Bind -------------------------------------------------------------------------

group_epb <-cbind(var_chamber_pres, 
                  var_circum_pres,
                  var_backfill_pres)
