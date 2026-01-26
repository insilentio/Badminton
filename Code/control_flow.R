
#load libraries for the source files

library(waterfalls)
library(gridExtra)
library(scales)
library(cowplot)
library(grid)
library(gtable)
library(tidyverse)
library(readxl)
library(pdftools)
library(magick)
library(ggbreak)

# general stuff
mytheme <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(colour = "darkgrey"),
        plot.title = element_text(colour = "darkgrey", size = 11, hjust = 0.5),
        plot.subtitle = element_text(colour = "darkgrey", size = 9, hjust = 0.5),
        plot.margin = unit(rep(0.3, 4), "cm"),
        # plot.background = element_rect(colour = "darkgrey", fill=NA, linewidth =.5),
        panel.background = element_rect(fill = 'white', colour = 'white'))

# color definitions for actives, guests, passives
# (derived from presence file)
col_a <-  "#2F6EBA"
col_g <-  "#4EAD5B"
col_p <-  "#AF7440"
col_mw <- "#B02418"

# source all code files ---------------------------------------------------

#import data
source("Code/data_import.R")

#data prepaaration
source("Code/data_analysis.R")

#first page
source("Code/plots_page1.R")

# second page
source("Code/plots_page2.R")

# anniversary code
source("Code/jubilee.R")


# output ------------------------------------------------------------------

source("Code/outputs.R")
