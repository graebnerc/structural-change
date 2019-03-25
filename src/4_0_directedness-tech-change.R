# Make the regressions with weighted least squares, using the final export shares as weights
rm(list=ls())

source("src/4_0_directedness-functions.R")
source("src/new_color_palette.R")
source("src/ggplot_theme_update.R")

# Load data ----
raw_data_full <- fread("unzip -p data/directedness-data.csv.zip", 
                       colClasses=c("character", "integer", "character", 
                                    rep("numeric", 9)))

# Create the two plots for Germany and Greece (figure 5) ----
source("src/4_1_directedness_GER-vs-GRC.R")

# Create the overview for the country groups (figure A6) ----
source("src/4_2_directedness_country-groups.R")
