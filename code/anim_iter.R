# PLOT FIGURES FOR ANIMATION

# initialize iter params
past_i <- 0
past_pc <- NULL
k <- 1

# initialize df
lof <- NULL

# initialize list of figures
figs_pca <- NULL
figs_pca_label <- NULL
figs_data <- NULL
figs_lof <- NULL
figs_lof_i <- NULL

for(i in 13:nrow(tbm_data)) {
  
  # Get TBM data for i section ====
  tbm_data_i <- tbm_data[1:i, ] %>% 
    # remove non-predictor columns
    select(!contains('ring')) %>% 
    select(!contains('chainage')) %>%
    select(!contains('csg')) %>%
    select(!contains('csf')) %>%
    select(!contains('ccs')) %>%
    select(!contains('td')) %>%
    select(!contains('tld')) %>%
    select(!contains('soilunit')) %>%
    select(!contains('soilunitpercent')) %>%
    # scale unit
    scale(., center = TRUE, scale = TRUE) %>% as.data.frame() %>%
    # add back chainage
    mutate(., Chainage = tbm_data[1:i, 'Chainage'])
  
  # plot geo map and operation data
  fig_data <- plot_data(tbm_data_i)
  figs_data[[k]] <- fig_data
  
  # PCA ====
  pca_out <- prcomp(tbm_data_i %>% select(!contains('chainage')))
    # PC scores
  pc <- pca_out$x %>% as.data.frame() %>%
    mutate(., Ring = tbm_data$Ring[1:i]) %>%
    mutate(., Chainage = tbm_data$Chainage[1:i]) %>%
    mutate(., SoilUnit = tbm_data$SoilUnit[1:i]) %>%
    mutate(., SoilUnitPercent = tbm_data$SoilUnitPercent[1:i])

  # make sign consistent
  # if this is not the first iteration
  if (past_i != 0) {
    # do for all principal components
    for (j in 1:ncol(pca_out$x)) {
      # if dot product between this step and prev step pc is negative
      if ((t(past_pc[[j]]) %*% pc[1:past_i, j]) < 0) {
        # change the sign
        pc[j] <- pc[j] * -1
      }
    }
  }
  
  # plot data points embedding only
  fig_pca <- plot_pca(pc)
  figs_pca[[k]] <- fig_pca
  
  # plot according soil unit
  fig_pca_label <- plot_pca_label(pc)
  figs_pca_label[[k]] <- fig_pca_label
  
  # LOF ====
  lof_i <- data.frame(Chainage = tbm_data_i$Chainage,
                    LOF = lof(pc[1:i, 1:2], minPts = 10)) %>%
    tail(., 1)
  # LOF at current chainage
  lof <- rbind(lof, lof_i)
  
  # plot LOF in embedding space 
  fig_lof_i <- plot_lof_i(pc, lof_i)
  figs_lof_i[[k]] <- fig_lof_i
  
  # plot longitudinal LOF
  fig_lof <- plot_lof(lof)
  figs_lof[[k]] <- fig_lof

  # Iter updates ====
  
  # iter control
  print(paste('Iter', i, 'completed'))
  # update current iteration as past 
  past_i <- i
  past_pc <- pc %>% select(contains('pc'))
  # update for figure number
  k <- k+1
}