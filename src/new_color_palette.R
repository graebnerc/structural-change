# This file creates two functions to be used within ggplot
# They allow access a number of color schemes
# currently, the following color schemes are there:
#
  # `main`  = icae_public_cols("sand", "purple", "dark red"),
  # `cool`  = icae_public_cols("purple", "dark green", "dark blue"),
  # `hot`   = icae_public_cols("sand", "dark red"),
  # `mixed` = icae_public_cols("orange", "dark blue", "purple", "sand", "dark red"),
  # `grey`  = icae_public_cols("light grey", "dark grey")
#
# For line plots, the mixed palette is best
# 
# The functions defined for use in ggplot so far are:
  # scale_color_icae_public(palette = "main", discrete = TRUE, reverse = FALSE, ...)
  #  scale_fill_icae_public(palette = "main", discrete = TRUE, reverse = FALSE, ...)
#
# Dependencies: ggplot2  
#
# Based on:
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# library(ggplot2)

icae_public_colors <- c(
  `orange` = "#ff9900",
  `purple` = "#8600b3",
  `dark green` = "#006600",
  `sand` = "#d8c469",
  `dark blue` = "#002b80",
  `dark red` = "#800000")

icae_public_cols <- function(...) {  # Extracts hex codes
  cols <- c(...)
  
  if (is.null(cols))
    return (icae_public_colors)
  
  icae_public_colors[cols]
}

icae_public_cols()

icae_public_palettes <- list(# Links the colors above to palettes
  `main`  = icae_public_cols("dark green", "sand", "purple"),
  
  `cool`  = icae_public_cols("purple", "dark green", "dark blue"),
  
  `hot`   = icae_public_cols("sand", "dark red"),
  
  `mixed` = icae_public_cols("dark green", "orange", "dark blue", "purple", "sand", "dark red"),
  
  `grey`  = icae_public_cols("light grey", "dark grey")
)

icae_public_pal <- function(palette = "main", reverse = FALSE, ...) {
  # extrapolates the palletes
  pal <- icae_public_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

# Function to be used for coloring:
scale_color_icae_public <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- icae_public_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("icae_public_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# function to be used for filling
scale_fill_icae_public <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- icae_public_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("icae_public_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# Examples: 
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#   geom_point(size = 4) +
#   scale_color_icae_public("hot")
# 
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#   geom_point(size = 4, alpha = .6) +
#   scale_color_icae_public(discrete = FALSE, palette = "cool")


# A really wonderful next step would be to put all this wonderful code into a package for easy access by you and others. If you’re new to package development, I can’t think of a better resource to get you started than Hadley Wickham‘s book, R Packages.
