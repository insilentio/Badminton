
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
# library(png)
library(magick)

# general stuff
mytheme <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(colour = "darkgrey"),
        plot.title = element_text(colour = "darkgrey", size = 11, hjust = 0.5),
        plot.subtitle = element_text(colour = "darkgrey", size = 9, hjust = 0.5),
        plot.margin = unit(rep(.3,4), "cm"),
        plot.background = element_rect(colour = "darkgrey", fill=NA, linewidth =.5) )

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

# save both pages into one pdf
  pdf("Output/Teilnehmerstatistik.pdf", width = 29.7/2.54, height = 21/2.54)
  lapply(list(gridplot1, gridplot2), grid.arrange)
  dev.off()
  