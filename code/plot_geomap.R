# Plot Geologic Map ------------------------------------------------------------

# load geologic map
geomap <- readPNG('../../../../2-data/seattle/geomap.png', 
                  native = TRUE,
                  info = TRUE)

# plot geologic map
fig_geomap <- ggplot() +
  annotation_raster(geomap,
                    xmin = 20000, xmax = 28800,
                    ymin = 0, ymax = 1) +
  
  ylim(0, 1) +
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = seq(20000, 28800, 500)) +
  xlab('Chainage (ft)') +
  # labels = geo_label$BH) +
  # ggtitle("(a)") +
  theme_classic() +#base_size = text_size) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid = element_blank())
