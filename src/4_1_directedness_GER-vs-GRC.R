# Creates figure 5 of the main paper

countries_considered_new <- countrycode(c("Germany", "Greece"), "country.name", "iso3c")

EU_data_v1_groups <- raw_data_full[exporter %in% countries_considered_new & year <2015
                                   ][, c_group:=exporter] 

# aggregation here 

crisis_data <- make_plot_data(EU_data_v1_groups, period_considered = "crisis")
all_countries <- unique(EU_data_v1_groups$exporter)

# Make regressions -----
depend_var <- "diff_exp_val_total_log"
indep_vars <- c("av_pci_w")
weight_var <- c("late_exp_share_reg")
reg_formula <- set_up_reg_formula(depend_var, indep_vars)

reg_dat_crisis <- make_regression_data(depend_var, c(indep_vars, weight_var), crisis_data)

# Make the wls plots -----

plots_list_crisis_pos <- list()

for (country in all_countries){
  plots_list_crisis_pos[[country]] <- make_wls_plot(reg_dat_crisis, 
                                                    country_name = country, 
                                                    x_var = "av_pci_w",
                                                    y_var = "diff_exp_val_total_log", 
                                                    size_var = "late_exp_share_reg", 
                                                    change_type = "positive",
                                                    interval_val = "conf")
}

# Make the bubble plots ----

deu_plot_crisis <- plots_list_crisis_pos[["DEU"]]$plot +
  ggtitle("Expanding product groups in Germany") +
  ylab("Positive export change (log)") +
  xlab(expression(atop("Product complexity", paste("(a)")))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3,"line"),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
deu_plot_crisis <- update_plot_theme(initial_plot = deu_plot_crisis, 
                                     discrete_palette = F) + 
  theme(axis.title.x = element_text())
deu_plot_crisis

grc_plot_crisis <- plots_list_crisis_pos[["GRC"]]$plot +
  ggtitle("Expanding product groups in Greece") +
  ylab("Positive export change (log)") +
  xlab(expression(atop("Product complexity", paste("(a)")))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3,"line"),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
grc_plot_crisis <- update_plot_theme(initial_plot = grc_plot_crisis, 
                                     discrete_palette = F) + 
  theme(axis.title.x = element_text())
grc_plot_crisis

ggsave(deu_plot_crisis + xlab(expression(atop("Product complexity", 
                                              paste("(b)")))), 
       filename = "output/tech_directedness/fig_5b_tech-directedness-GER.pdf", 
       width = 5, 
       height = 5)
ggsave(grc_plot_crisis, 
       filename = "output/tech_directedness/fig_5a_tech-directedness-GRC.pdf", 
       width = 5, 
       height = 5)
