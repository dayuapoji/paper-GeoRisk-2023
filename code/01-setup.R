# SETUP ####

# Library ----------------------------------------------------------------------

library(rstudioapi) # setup
library(tidyverse)  # data processing
library(magrittr)
library(reshape2)
library(png)
library(factoextra) # unsupervised learning
library(dbscan)
library(cowplot)    # plotting
library(egg)
library(plotly)
# library(tidytext)
library(ggrepel)

# Set Working Directory --------------------------------------------------------

# set current directory as working directory
setwd(dirname(getActiveDocumentContext()$path))

# Functions --------------------------------------------------------------------

# list of functions
functions <- list.files(path = 'functions/', pattern = "[.]R$", 
                        full.names=TRUE, recursive = TRUE)

# load
for (i in (1:length(functions))) {
  print(functions[i])
  source(functions[i])
}