select_tbm_data <- function(tbm_orig) {
  
  
  data_orig <- tbm_orig
  # get variable features
  scripts <- list.files(path = 'select/sr99/', pattern = "[.]R$",
                        full.names=TRUE, recursive = F)
  # load
  for (i in (1:length(scripts))) {
    print(scripts[i])
    source(scripts[i], local = T)
  }
  
  # bind and clean data ----
  df <- cbind(ring, 
              chainage[1], # chainage at head
              group_excav,
              group_advance,
              group_steer,
              group_gcs,
              group_epb,
              group_muck,
              group_grout) %>%
    # rename column 
    rename(., Chainage = ChainageHead) %>%
    
      # transform data ----
    # standardize volume variables as m3/m
    standardize_vol(.) %>%
      
      # clean data ----
    # excavation phase only
    subset(., AdvanceRate > 0) %>%
    subset(., CutterTorque > 0) %>%
    # remove rows of duplicated chainage 
    remove_duplicate_chainage(., sort = 'without ring') %>%
    # remove rows with mising data
    drop_na(.) %>%
    # remove columns with constant values
    remove_constant_var(.)
  
  # select data ----
  df_excav <- df %>%
    subset(., select = c(colnames(group_excav %>% remove_constant_var()))) %>%
    remove_single_measurement(., NULL, 32) %>%
    select(!matches('force|pres'))

  df_advance <- df %>%
    subset(., select = c(colnames(group_advance %>% remove_constant_var()))) %>%
    remove_single_measurement(., NULL, 32) %>%
    select(!matches('pres'))

  df_steer <- df %>%
    subset(., select = c(colnames(group_steer %>% remove_constant_var()))) %>%
    remove_single_measurement(., NULL, 32) %>%
    select(!matches('rear|art|tail|articdev')) %>%
    select(!matches('forcedif'))

  df_gcs <- df %>%
    subset(., select = c(colnames(group_gcs %>%
                                    drop_na() %>%
                                    remove_constant_var()))) %>%
    remove_single_measurement(., NULL, 32) %>%
    select(!matches('fillrate|flow|air|water|liquid|agent|additive'))

  df_epb <- df %>%
    subset(., select = c(colnames(group_epb %>% remove_constant_var()))) %>%
    remove_single_measurement(., NULL, 32) %>%
    select(!matches('circum')) %>%
    select(!matches('backfill'))

  df_muck <- df %>%
    subset(., select = c(colnames(group_muck %>% remove_constant_var()))) %>%
    remove_single_measurement(., NULL, 32) %>%
    select(!matches('screwmuck|screwpolymer|torque')) %>%
    select(!matches('casing|gate'))

  df_grout <- df %>%
    subset(., select = c(colnames(group_grout %>% remove_constant_var()))) %>%
    remove_single_measurement(., NULL, 32) %>%
    select(!matches('fillrate|flow'))

  # merge df
  tbm_data <- df %>% subset(., select = c('Ring', 'Chainage',
                                        colnames(df_excav),
                                        colnames(df_advance),
                                        # colnames(df_steer),
                                        colnames(df_gcs))) #,
                                        # colnames(df_epb),
                                        # colnames(df_muck),
                                        # colnames(df_grout)))
  
  # create groups of features for plot colors
  group <- rbind(data.frame(Feature = colnames(df_excav),
                            Group = 'Excavation'),
                 data.frame(Feature = colnames(df_advance),
                            Group = 'Advancing'),
                 # data.frame(Feature = colnames(df_steer),
                 #            Group = 'Steering'),
                 data.frame(Feature = colnames(df_gcs),
                            Group = 'Ground Conditioning'))#,
                 # data.frame(Feature = colnames(df_epb),
                 #            Group = 'Earth Pressure Balancing'),
                 # data.frame(Feature = colnames(df_muck),
                 #            Group = 'Muck Extraction'),
                 # data.frame(Feature = colnames(df_grout),
                 #            Group = 'Tail Grouting'))
  
  # set factor for features
  group$Group <- factor(group$Group, levels = unique(group$Group))
  
  
  return(list(data = tbm_data, group = group))

}



