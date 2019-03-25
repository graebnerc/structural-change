# Creates figure 6 in the appendix

countries_interest <- list()
countries_interest[["France"]] <- countrycode("France", "country.name", "iso3c")
countries_interest[["Finance"]] <- countrycode(c("Cyprus", "Luxembourg", "Netherlands", "Malta", "Ireland"), "country.name", "iso3c")
countries_interest[["Core"]] <- countrycode(c("Austria", "Belgium", "Denmark", "Finland", "Germany","Sweden"), "country.name", "iso3c")
countries_interest[["Catchup"]] <- countrycode(c("Bulgaria", "Romania", "Czech Republic", "Hungary", "Poland", "Slovenia", "Slovakia", "Croatia"), "country.name", "iso3c")
countries_interest[["Baltikum"]] <- countrycode(c("Estonia", "Latvia", "Lithuania"), "country.name", "iso3c")
countries_interest[["Periphery"]] <- countrycode(c("Greece", "Italy", "Portugal", "Spain"), "country.name", "iso3c")
countries_considered_new <- unlist(countries_interest)

countries_considered_new_cnames <- countrycode(countries_considered_new, "iso3c", "country.name.de")

EU_data_v1_groups <- raw_data_full[, c_group:=ifelse(exporter %in% countries_interest[["France"]], "France", 
                                              ifelse(exporter %in% countries_interest[["Core"]], "Core", 
                                                     ifelse(exporter %in% countries_interest[["Catchup"]], "Catchup", 
                                                            ifelse(exporter %in% countries_interest[["Baltikum"]], "Baltic", 
                                                                   ifelse(exporter %in% countries_interest[["Periphery"]], "Periphery",
                                                                          ifelse(exporter %in% countries_interest[["Finance"]], "Financial hubs", NA))))))] # removed one )

# aggregation here 
EU_data_v2_groups <- EU_data_v1_groups[, .(exp_val=sum(exp_val, na.rm = T),
                                           pci=mean(pci, na.rm = T)), by=.(c_group, year, commoditycode)
                                       ][, world_exp_sect:=sum(exp_val), by=.(year, commoditycode) # total exports for this product
                                         ][, reg_exp_total:=sum(exp_val), by=.(year, c_group)# total exports in this group
                                           ][, exp_share_sect:=exp_val / world_exp_sect
                                             ][, exp_share_sect_reg:=exp_val / reg_exp_total
                                               ][, exporter:=c_group
                                                 ][, c_group:=NULL] 

# long_data <- make_plot_data(EU_data_v2_groups, period_considered = "long")
crisis_data <- make_plot_data(EU_data_v2_groups, period_considered = "crisis")
all_countries <- unique(EU_data_v2_groups$exporter)

# Make regressions -----
depend_var <- "diff_exp_val_total_log"
indep_vars <- c("av_pci_w")
weight_var <- c("late_exp_share_reg")
reg_formula <- set_up_reg_formula(depend_var, indep_vars)

# reg_dat_long <- make_regression_data(depend_var, c(indep_vars, weight_var), long_data)
reg_dat_crisis <- make_regression_data(depend_var, c(indep_vars, weight_var), crisis_data)


# Make the wls plots -----

plots_list_crisis_pos <- list()
plots_list_crisis_neg <- list()

for (country in all_countries){
  plots_list_crisis_pos[[country]] <- make_wls_plot(reg_dat_crisis, 
                                                    country_name = country, 
                                                    x_var = "av_pci_w",
                                                    y_var = "diff_exp_val_total_log", 
                                                    size_var = "late_exp_share_reg", 
                                                    change_type = "positive",
                                                    interval_val = "conf")
  
  plots_list_crisis_neg[[country]] <- make_wls_plot(reg_dat_crisis, 
                                                    country_name = country, 
                                                    x_var = "av_pci_w",
                                                    y_var = "diff_exp_val_total_log", 
                                                    size_var = "late_exp_share_reg", 
                                                    change_type = "negative",
                                                    interval_val = "conf")
}

# Make the bubble plots ----

core_plot_crisis <- plots_list_crisis_pos[["Core"]]$plot +   
  ggtitle("Expanding product groups in the core countries") +
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
core_plot_crisis <- update_plot_theme(initial_plot = core_plot_crisis, discrete_palette = F) + theme(axis.title.x = element_text())
core_plot_crisis


southern_europe_crisis <- plots_list_crisis_pos[["Periphery"]]$plot + 
  ggtitle("Expanding product groups in the periphery") +
  ylab("Positive export change (log)") + 
  xlab(expression(atop("Product complexity", paste("(c)")))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3,"line"),
        axis.title = element_text(size=14), 
        plot.title = element_text(size=16),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
southern_europe_crisis <- update_plot_theme(initial_plot = southern_europe_crisis, discrete_palette = F) + theme(axis.title.x = element_text())
southern_europe_crisis

france_plot_crisis <- plots_list_crisis_pos[["France"]]$plot + 
  ggtitle("Expanding product groups in France") +
  ylab("Positive export change (log)") + 
  xlab(expression(atop("Product complexity", paste("(b)")))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3,"line"),
        axis.title = element_text(size=14), 
        plot.title = element_text(size=16),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
france_plot_crisis <- update_plot_theme(initial_plot = france_plot_crisis, discrete_palette = F) + theme(axis.title.x = element_text())
france_plot_crisis

eastern_europe_crisis <- plots_list_crisis_pos[["Catchup"]]$plot + 
  ggtitle("Expanding product groups in the Catchup countries (without Baltics)") +
  ylab("Positive export change (log)") + 
  xlab(expression(atop("Product complexity", paste("(d)")))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3,"line"),
        axis.title = element_text(size=14), 
        plot.title = element_text(size=16),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
eastern_europe_crisis <- update_plot_theme(initial_plot = eastern_europe_crisis, discrete_palette = F) + theme(axis.title.x = element_text())
eastern_europe_crisis

baltic_europe_crisis <- plots_list_crisis_pos[["Baltic"]]$plot + 
  ggtitle("Expanding product groups in the Baltics") +
  ylab("Positive export change (log)") + 
  xlab(expression(atop("Product complexity", paste("(e)")))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3,"line"),
        axis.title = element_text(size=14), 
        plot.title = element_text(size=16),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
baltic_europe_crisis <- update_plot_theme(initial_plot = baltic_europe_crisis, discrete_palette = F) + theme(axis.title.x = element_text())
baltic_europe_crisis

finance_countries_crisis <- plots_list_crisis_pos[["Financial hubs"]]$plot + 
  ggtitle("Expanding product groups in the financial hubs") +
  ylab("Positive export change (log)") + 
  xlab(expression(atop("Product complexity", paste("(f)")))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3,"line"),
        axis.title = element_text(size=14), 
        plot.title = element_text(size=16),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
finance_countries_crisis <- update_plot_theme(initial_plot = finance_countries_crisis, 
                                              discrete_palette = F) + 
  theme(axis.title.x = element_text())
finance_countries_crisis

full_regional_plot <- ggpubr::ggarrange(core_plot_crisis, 
                                        france_plot_crisis, 
                                        southern_europe_crisis, 
                                        eastern_europe_crisis, 
                                        baltic_europe_crisis, 
                                        finance_countries_crisis, 
                                        ncol = 3, nrow = 2)
ggsave(full_regional_plot, 
       filename = "output/tech_directedness/fig_A7_TechDirectedness_groups.pdf", 
       width = 21, height = 16)
