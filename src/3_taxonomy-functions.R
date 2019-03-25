
make_single_plot <- function(plot_data, plot_variable, plot_title = NA,
                             y_limit = NA, leg_pos = "bottom", y_label = "LABEL") {
  basic_plot <- ggplot(
    filter(
      plot_data,
      variable == plot_variable
    ),
    aes(x = "0", y = mean_value, colour = c_group, fill = c_group)
  ) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    geom_errorbar(aes(
      ymin = mean_value - sd_value,
      ymax = mean_value + sd_value, group = c_group
    ),
    position = position_dodge(0.9), width = .2, colour = "#00001a"
    ) +
    scale_color_icae_public(palette = "hot") + scale_fill_icae_public(palette = "hot") + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_text(aes(y = mean_value + sd_value * 1.1, label = "")) + # Simple hack to make spots at top of y axis
    theme_bw() +
    ylab(y_label) +
    theme(
      legend.position = leg_pos,
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 15),
      panel.border = element_blank(),
      axis.line = element_line()
    )
  if (!is.na(y_limit[1])) {
    basic_plot <- basic_plot + coord_cartesian(ylim = y_limit)
  }
  if (!is.na(plot_title)) {
    basic_plot <- basic_plot + ggtitle(paste(plot_title))
  } else {
    basic_plot <- basic_plot + ggtitle(paste(plot_variable))
  }
  return(basic_plot)
}
