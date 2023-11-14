# SAVE COMBINED FIGURES ===========================================
for (i in 1:(k-1)) {
  
  # combining PCA figs
  figs_tbm <- ggarrange(fig_geomap, figs_data[[i]], 
                         nrow = 2, heights = c(2, 1))
  figs_pca_combined <- plot_grid(figs_pca[[i]], figs_pca_label[[i]],
                                 ncol = 2)
  
  # save combined PCA figs as png
  ggsave(paste0("../output/anim/pca/fig_pca", i, ".png"),
         plot_grid(figs_tbm,
                   figs_pca_combined,
                   nrow = 2, rel_heights = c(1, 1.5)),
         width = 10, height = 8)
  
  # combining LOF figs
  figs_long_lof <- ggarrange(fig_geomap, figs_lof[[i]], 
                             nrow = 2, heights = c(2, 1))
  figs_lof_combined <- plot_grid(figs_pca_label[[i]], figs_lof_i[[i]],
                                 ncol = 2)
  
  # save combined LOF figs as png
  ggsave(paste0("../output/anim/lof/fig_lof", i, ".png"),
         plot_grid(figs_long_lof,
                   figs_lof_combined,
                   nrow = 2, rel_heights = c(1, 1.5)),
         width = 10, height = 8)
  
  # iter control
  print(paste('Plots', i, 'completed'))
}
