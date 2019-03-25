# function to update a ggplot
update_plot_theme <- function(initial_plot,
                              grid_used = "horizontal",
                              legend_position = "none",
                              icae_color_palette = "mixed",
                              discrete_palette = TRUE,
                              reverse_palette = FALSE,
                              keep_color = FALSE){
  final_plot <- initial_plot +
    theme_bw() + 
    theme(panel.border = element_blank(),
          axis.line = element_line(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.75),
          legend.position = legend_position, 
          legend.title = element_blank(), 
          legend.spacing.x = unit(5, "mm"),
          plot.margin = unit(c(3,3,3,3), "mm"))
  if (!keep_color){
    final_plot <- final_plot + 
      scale_color_icae_public(palette = icae_color_palette, discrete = discrete_palette, reverse = reverse_palette) +
      scale_fill_icae_public(palette = icae_color_palette, discrete = discrete_palette, reverse = reverse_palette)
  }
  
  if (grid_used=="vertical"){
    final_plot <- final_plot + 
      theme(panel.grid.major.x = element_line(colour = "grey"),
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank())
  } else if (grid_used=="vertical_full"){
    final_plot <- final_plot + 
      theme(panel.grid.major.x = element_line(colour = "grey"),
            panel.grid.minor.x = element_line(colour = "grey"),
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank())
  } else if (grid_used=="horizontal"){
    final_plot <- final_plot + 
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey"), 
            panel.grid.minor.y = element_blank())
  } else if (grid_used=="horizontal_full"){
    final_plot <- final_plot + 
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey"), 
            panel.grid.minor.y = element_line(colour = "grey"))
  } else if (grid_used=="both"){
    final_plot <- final_plot + 
      theme(panel.grid.major.x = element_line(colour = "grey"), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey"), 
            panel.grid.minor.y = element_blank())
  } else if (grid_used=="both_full"){
    final_plot <- final_plot + 
      theme(panel.grid.major.x = element_line(colour = "grey"), 
            panel.grid.minor.x = element_line(colour = "grey"), 
            panel.grid.major.y = element_line(colour = "grey"), 
            panel.grid.minor.y = element_line(colour = "grey"))
  } else if (grid_used=="none"){
    final_plot <- final_plot + 
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank())
  } else {
    warning("No (valid) grid specified. Choose horizontal, vertical, both or none")
  }
  return(final_plot)
}
