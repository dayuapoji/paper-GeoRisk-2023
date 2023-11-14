colors <- c('CCS' = "blue", 
            'CSF' = "aquamarine3",
            'CSG'= "sandybrown",  
            'TLD'= 'pink2',
            'TD'= "purple",
            'Unknown'='gray')

# ====

plot_data <- function(tbm_data_i) {
  
  fig_data <- ggplot() +
    geom_line(data = tbm_data_i %>% 
                melt(., id = 'Chainage'),
              mapping = aes(x = Chainage, y = value, color = variable), 
              alpha = 0.5) +
    geom_point(data = tbm_data_i[i, ] %>% 
                 melt(., id = 'Chainage'),
               mapping = aes(x = Chainage, y = value, color = variable),
               size = 1) +
    scale_x_continuous(limit = c(20000, 28800),
                       breaks = seq(20000, 28800, 500)) +
    ylim(-5, 5) +
    xlab('Chainage (ft)') +
    ylab('Scaled Data') +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = 'none')
  
  return(fig_data)
}

# ====

plot_pca <- function(pc) {
  
  fig_pca <- ggplot() +
    geom_point(data = pc,
               aes(x = PC1, y = PC2), #color = SoilUnit, size = SoilUnitPercent), 
               alpha = 0.5) +
    scale_size_continuous(range = c(1, 2.5)) +
    scale_color_manual(values = colors) +
    xlim(-5, 5) +
    ylim(-5, 5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = 'none')
  
  return(fig_pca)
}

# ====

plot_pca_label <- function(pc) {
  
  fig_pca_label <- ggplot() +
    geom_point(data = pc,
               aes(x = PC1, y = PC2, color = SoilUnit, size = SoilUnitPercent), 
               alpha = 0.5) +
    scale_size_continuous(range = c(1, 2.5)) +
    scale_color_manual(values = colors) +
    xlim(-5, 5) +
    ylim(-5, 5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = 'none')
  
  return(fig_pca_label)
}

# ====

plot_lof_i <- function(pc, lof_i){

  fig_lof_i <- ggplot() +
    
    geom_point(data = pc,
               aes(x = PC1, y = PC2), #color = SoilUnit, size = SoilUnitPercent), 
               alpha = 0.5) +
    
    geom_point(data = pc[i, ],
               aes(x = PC1, y = PC2,
                   stroke = 2,
                   # alpha = lof_i$LOF,
                   ),
               size = lof_i$LOF * 1.5^(lof_i$LOF),
               color = 'red', shape = 1) +
    
    # scale_size_continuous(range = c(1, 2.5)) +
    # scale_alpha_continuous(limits = c(1,4)) +
    # scale_size_continuous(range = c(1, 10), limits = c(1,4)) +
    scale_color_manual(values = colors) +
    xlim(-5, 5) +
    ylim(-5, 5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = 'none')
  
  return(fig_lof_i)
}

# ====

plot_lof <- function(lof) {

  fig_lof <- ggplot(data = lof) +
    geom_col(mapping = aes(x = Chainage, y = LOF, color = LOF),
             width = 1, alpha = 0.5) +
    
    scale_x_continuous(limit = c(20000, 28800),
                       breaks = seq(20000, 28800, 500)) +
    scale_color_gradientn(values = c(0, 1, 10),
                          colors = c('blue', 'red', 'red')) +
    ylim(0, 3) +
    # scale_y_continuous(breaks = c(1, 2, 3, 4, 5)) +
    xlab('Chainage (ft)') +
    ylab('LOF') +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = 'none')
  
 return(fig_lof) 
}


