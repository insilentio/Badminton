library(waterfalls)
library(gridExtra)
library(scales)
library(cowplot)
library(grid)
library(gtable)

#import data
source("Code/data_import.R")

#data prepaaration
source("Code/data_analysis.R")

#first page
source("Code/plots_page1.R")

# second page
source("Code/plots_page2.R")


# save both pages into one pdf
pdf("Output/Teilnehmerstatistik.pdf", width = 29.7/2.54, height = 21/2.54)
lapply(list(gridplot1, gridplot2), grid.arrange)
dev.off()