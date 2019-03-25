rm(list = ls())

library(plm)
library(foreign)
library(countrycode)
library(feather)
library(tidyverse)
library(lmtest)
library(ggpubr)
library(data.table)
library(factoextra)
library(dendextend)
library(cluster)
library(viridis)
library(xtable)
library(grid)
library(gridExtra)
library(haven)
library(labelled)
library(liloprojections)
# devtools::install_github("graebnerc/liloprojections", 
#                          force = F)

# 1. Derive the impulse response functions ----
exclude_countries <- c()
source("src/1_make-regressions.R")

# 2. Visualize the fixed effects
source("src/2_0_fe-clustering.R")

# 3. Visualize distinctive properties of country cluster ----
source("src/3_taxonomy.R")

# 4. Derive and visualize the measure for technological directedness ----
source("src/4_0_directedness-tech-change.R")

# 5. Summary statistics for the appendix ----
source("src/5_descriptives.R")
