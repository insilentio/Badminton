
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

# source all code files ---------------------------------------------------
# general stuff
source("Code/general.R")

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
