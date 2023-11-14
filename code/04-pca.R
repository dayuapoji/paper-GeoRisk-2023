# PCA ##########################################################################

# Plot PCA Sequence ------------------------------------------------------------

fig_num <- c('(a)', '(b)', '(c)', 
             '(d)', '(e)', '(f)',
             '(g)', '(h)', '(i)')

colors <- c('CCS' = "blue", 
            'CSF' = "aquamarine3",
            'CSG'= "sandybrown",  
            'TLD'= 'pink2',
            'TD'= "purple",
            'Unknown'='gray')

# initialize
past_i <- 0
past_pc <- NULL
figs_pca <- NULL
k <- 1

for(i in c(13, 50, 100, 
           200, 400, 600,
           800, 1000, 1253)) {
  
  # PCA 
  pca_out <- prcomp(tbm_data[1:i, ] %>% 
                    
                      # remove non-predictor columns
                      select(!contains('ring')) %>% 
                      select(!contains('chainage')) %>%
                      select(!contains('csg')) %>%
                      select(!contains('csf')) %>%
                      select(!contains('ccs')) %>%
                      select(!contains('td')) %>%
                      select(!contains('tld')) %>%
                      select(!contains('soilunit')) %>%
                      select(!contains('soilunitpercent')),
                    
                    # standardized units
                    scale. = T, center = T)
  
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
  
  # update current iteration as past 
  past_i <- i
  for (j in 1:ncol(pca_out$x)) {
    past_pc[j] <- pc[j]
  }
  
  # plot according soil unit
  fig_pca <- ggplot() +
    geom_point(data = pc,
               aes(x = PC1, y = PC2, color = SoilUnit, size = SoilUnitPercent), 
               alpha = 0.5) +
    scale_size_continuous(range = c(1, 2.5)) +
    scale_color_manual(values = colors) +
    ggtitle(paste(fig_num[k], 'Chainage', round(tbm_data$Chainage[i], 0))) +
    xlim(-5, 5) +
    ylim(-5, 5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = 'none')
  
  figs_pca[[k]] <- fig_pca
  # save fig as pdf
  # pdf(file = paste0('../output/figs/pca-seq-', fig_num[k],'.pdf'),
  #     width = 7.5, height = 6)
  # print(fig_pca)
  # dev.off()
  
  # for figure number
  k <- k+1
  
}

# prepare legend
legend <- ggplot() +
  geom_point(data = pc,
             aes(x = PC1, y = PC2, color = SoilUnit, size = SoilUnitPercent)) +
  scale_size_continuous(range = c(1, 2.5)) +
  scale_color_manual(values = colors) +
  theme_bw() +
  theme(legend.position = 'bottom')

legend <- get_legend(legend)


# save fig as pdf
pdf(file = paste0('../output/figs/pca-seq.pdf'),
    width = 12, height = 13)
grid.arrange(
  plot_grid(figs_pca[[1]], figs_pca[[2]], figs_pca[[3]], ncol = 3),
  plot_grid(figs_pca[[4]], figs_pca[[5]], figs_pca[[6]], ncol = 3),
  plot_grid(figs_pca[[7]], figs_pca[[8]], figs_pca[[9]], ncol = 3),
  legend,
  ncol = 1, heights = c(1, 1, 1, 0.2)
)
dev.off()

# Plot PCA 3D ------------------------------------------------------------------

color_df <- data.frame(SoilUnit = c('CCS', 'CSF', 'CSG', 'TLD', 'TD', 'Unknown'),
                       SoilColor = c("blue", 
                                     "aquamarine3",
                                     "sandybrown",  
                                     'pink',
                                     "purple",
                                     'gray'))

pc <- merge(pc, color_df, by = 'SoilUnit')

fig <- plot_ly(data = pc, x = ~PC3, y = ~PC1, z = ~PC2, 
               type = 'scatter3d', 
               mode = "markers",
               
               marker = list(size = ~(SoilUnitPercent+20)/8,
                             color = ~SoilColor,
                             line = list(width=0)),
               
               color = ~SoilUnit,
               alpha = 0.5) %>%
  
  layout(scene = list(xaxis = list(range = c(-10, 5)),
                      yaxis = list(range = c(-5, 5)),
                      zaxis = list(range = c(-5, 5))))

# show fif
fig  

# Plot PCA Variance ------------------------------------------------------------

variance = pca_out$sdev^2 / sum(pca_out$sdev^2)

fig_scree <- ggplot()+
  
  geom_vline(xintercept = 2, linetype = 'dashed') +
  geom_vline(xintercept = 3, linetype = 'dashed') +
  geom_vline(xintercept = 8, linetype = 'dashed') +
  
  geom_segment(aes(x = 2, y = 0.22,
                   xend = 1.5, yend = 0.22),
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_segment(aes(x = 3, y = 0.18,
                   xend = 2.5, yend = 0.18),
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_segment(aes(x = 8, y = 0.1,
                   xend = 7.5, yend = 0.1),
               arrow = arrow(length = unit(0.02, "npc"))) +
  
  geom_col(mapping = aes(x = 1:ncol(pca_out$x), y = variance)) +
  scale_x_continuous(labels = as.character(1:ncol(pca_out$x)), 
                     breaks = c(1:ncol(pca_out$x))) +
  
  geom_label_repel(aes(x = 2, y = 0.22), 
                   hjust = 1,
                   label = paste('Total Var =', round(sum(variance[1:2]), 2))) +
  geom_label_repel(aes(x = 3, y = 0.18), 
                   hjust = 1,
                   label = paste('Total Var =', round(sum(variance[1:3]), 2))) +
  geom_label_repel(aes(x = 8, y = 0.1), 
                   hjust = 1,
                   label = paste('Total Var =', round(sum(variance[1:8]), 2))) +
  
  xlab('Principal Components') +
  ylab('Variance') +
  # scale_x_discrete(nbreaks = 15) +
  theme_bw() +
  theme(panel.grid = element_blank())

pdf(file = '../output/figs/fig-scree.pdf')#,
# width = 4, height = 4)
fig_scree
dev.off()

# Plot PCA Contribution --------------------------------------------------------

# biplot
fig_biplot <- fviz_pca_var(pca_out,
                           col.var = "contrib", # Color by contributions to the PC
                           # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = T) +
  scale_color_gradientn(name = 'Contributions', 
                        colours = c('blue', 'orange', 'red')) +
  xlab('PC1 (26.7%)') +
  ylab('PC2 (15.4%)') +
  guides(fill=guide_legend(title="Contributions")) +
  ggtitle('')



pdf(file = '../output/figs/fig-biplot.pdf')#,
# width = 4, height = 4)
fig_biplot
dev.off()  
