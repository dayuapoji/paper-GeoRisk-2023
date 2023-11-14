# LOF ##########################################################################

# LOF Along Tunnel Alginment ---------------------------------------------------

set.seed(1)

# initialize
results <- NULL

# compute LOF
for(i in 15:nrow(tbm_data)) {
  
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
  
  # LOF
  lof <- lof(pc[1:i, 1:2], minPts = 10)
  
  # LOF at current chainage
  lof_i <- data.frame(Chainage = tail(pc$Chainage, 1),
                      LOF = tail(lof, 1))
  
  # bind results
  results <- rbind(results, lof_i)
  print(paste(i, 'done'))
}

fig_lof <- ggplot() +
  annotation_raster(geomap,
                    xmin = 20000, xmax = 28800,
                    ymin = 1, ymax = 5) +
  annotate('rect', fill = "white", alpha = 0.6, 
           xmin = 20000, xmax = 28800,
           ymin = 1, ymax = 5) +
  # geom_segment(data = results,
  #              mapping =  aes(x = Chainage, xend = Chainage,
  #                             y = 0, yend = 1,
  #                             color = LOF)) +
  geom_line(data = results,
            mapping = aes(x = Chainage, y = LOF, color = LOF)) +
  # geom_point(data = results,
  #           mapping = aes(x = Chainage, y = LOF, color = LOF)) +
  
  scale_color_gradientn(colors = c('blue', 'orange', 'red', 'red'),
                        values = c(0, 1, 1.25, max(results$LOF))/max(results$LOF)) +
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = seq(20000, 28800, 500)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5)) +
  xlab('Chainage (ft)') +
  # scale_y_log10() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.key.size = unit(0.25, 'cm'))

fig_lof

# save fig in pdf
pdf(file = '../output/figs/lof.pdf', 
    width = 9, height = 9/3,
    onefile = FALSE)
fig_lof
# ggarrange(fig_geomap, fig_lof)
dev.off()

# LOF Visualization ------------------------------------------------------------

fig_num <- c('(x)', '(a)', '(b)', '(c)', '(d)', '(e)', '(f)')

chainage_vec <- NULL
for (i in 1:10) {
  chainage <- results[order(results$LOF, decreasing = T), ]$Chainage[i]
  chainage_vec <- append(chainage_vec, chainage)
}

# six figures example
# row_index_vec <- NULL
# for (i in chainage_vec) {
#   row_index <- as.numeric(rownames(tbm_data[tbm_data$Chainage == i, ]))
#   row_index_vec <- append(row_index_vec, row_index)
# }

# two figures comparison
row_index_vec <- NULL
for (i in c(results[order(results$LOF, decreasing = F), ]$Chainage[8],
            results[order(results$LOF, decreasing = T), ]$Chainage[4])) {
  row_index <- as.numeric(rownames(tbm_data[tbm_data$Chainage == i, ]))
  row_index_vec <- append(row_index_vec, row_index)
}

row_index_vec <- sort(row_index_vec)


# initialize
past_i <- 0
past_pc <- NULL
figs_lof <- NULL
k <- 1

# i <- 663
# for(i in row_index_vec[c(1, 2, 4, 6, 7, 8, 9)]) {
for(i in row_index_vec[c(1, 2)]) {
  
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
  
  # LOF
  pc$LOF <- lof(pc[, 1:2], minPts = 10)
  
  # current position
  pos_x <- tail(pc$PC1, 1)
  pos_y <- tail(pc$PC2, 1)
  
  # plot according soil unit
  fig_lof <- ggplot() +
    # plot PCA
    geom_point(data = pc,
               aes(x = PC1, y = PC2),
               size = 1,
               alpha = 0.5) +
    # plot LOF
    geom_point(data = pc,
               aes(x = PC1, y = PC2,
                   stroke = LOF,
                   alpha = LOF,
                   size = LOF),
               color = 'red', shape = 1) +
    
    # mark current chainage
    
    annotate("label", 
             x = pos_x, y = pos_y-1.2,
             color = 'blue',
             label = 'Current Chainage') +#,
    # box.padding = 2.5,
    # arrow = arrow(length = unit(0.015, "npc"))) +
    
    annotate("segment", 
             x = pos_x-0.5, xend = pos_x, 
             y = pos_y-0.8, yend = pos_y, size=1, color = 'blue',
             arrow = arrow(length = unit(0.1, 'inches'), type = "open")) +
    
    # scale_color_manual(values = colors) +
    scale_alpha_continuous(limits = c(1,4)) +
    scale_size_continuous(range = c(1, 10), limits = c(1,4)) +
    ggtitle(paste(fig_num[k], 'Chainage', round(tbm_data$Chainage[i],0))) +
    xlim(-5, 5) +
    ylim(-5, 5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = 'none')
  
  
  figs_lof[[(k)]] <- fig_lof
  
  # save fig as pdf
  # pdf(file = paste0('../output/figs/lof-seq-', fig_num[k]),
  #     width = 7.5, height = 6.5)
  # print(fig_lof)
  # dev.off()
  
  # for figure number
  k <- k+1
  
}


legend <- ggplot() +
  # plot PCA
  geom_point(data = pc,
             aes(x = PC1, y = PC2),
             size = 1,
             alpha = 0.5) +
  # plot LOF
  geom_point(data = pc,
             aes(x = PC1, y = PC2,
                 stroke = LOF,
                 alpha = LOF,
                 size = LOF),
             color = 'red', shape = 1) +
  
  # mark current chainage
  
  annotate("text", 
           x = pos_x, y = pos_y-1.2,
           label = 'Current Chainage') +#,
  # box.padding = 2.5,
  # arrow = arrow(length = unit(0.015, "npc"))) +
  
  annotate("segment", 
           x = pos_x-0.5, xend = pos_x, 
           y = pos_y-0.8, yend = pos_y, size=0.5, 
           arrow = arrow(length = unit(0.1, 'inches'), type = "open")) +
  
  # scale_color_manual(values = colors) +
  scale_alpha_continuous(limits = c(1,4)) +
  scale_size_continuous(range = c(1, 10), limits = c(1,4)) +
  ggtitle(paste(fig_num[k], 'Chainage', round(tbm_data$Chainage[i],0))) +
  xlim(-5, 5) +
  ylim(-5, 5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom')

legend <- get_legend(legend)

# save fig as pdf
pdf(file = paste0('../output/figs/lof-seq.pdf'),
    width = 8.5, height = 8.5/1.75)

grid.arrange(
  plot_grid(figs_lof[[1]], figs_lof[[2]], ncol = 2),
  legend,
  ncol = 1, heights = c(1, 0.1)
)

# grid.arrange(
#   plot_grid(figs_lof[[2]], figs_lof[[3]], figs_lof[[4]], ncol = 3),
#   plot_grid(figs_lof[[5]], figs_lof[[6]], figs_lof[[7]], ncol = 3),
#   legend,
#   ncol = 1, heights = c(1, 1, 0.2)
# )
dev.off()