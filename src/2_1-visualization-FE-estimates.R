fe_total_plot <- fe_total %>%
  dplyr::filter(var %in% c("unemp", "gdp_growth", "current_accout", 
                           "exp_to_gdp", "debt_public", "wage_share", 
                           "gdp_pcn", "va_finance"))
fe_total_plot <- as.data.table(fe_total_plot)
fe_total_plot[var == "unemp", var := "Unemployment"]
fe_total_plot[var == "gdp_growth", var := "GDP growth"]
fe_total_plot[var == "current_accout", var := "Current account balance"]
fe_total_plot[var == "exp_to_gdp", var := "Export to GDP"]
fe_total_plot[var == "debt_public", var := "Public debt"]
fe_total_plot[var == "wage_share", var := "Wage share"]
fe_total_plot[var == "gdp_pcn", var := "GDP per capita"]
fe_total_plot[var == "va_finance", var := "Share of finance (VA)"]
fe_total_plot[, k_val := substr(k_val, 3, 3)]
fe_total_plot[, var_val := var(fe_val, na.rm = T), by = .(var, k_val)]
fe_total_plot[var == "Public debt", var_val := var_val / 10]
fe_total_plot[var == "Share of finance (VA)", fe_val := fe_val / 100]

head(fe_total_plot)

var_vals_va_finance <- filter(fe_total_plot, var=="Share of finance (VA)" & country=="AUT")$var_val
fe_vals_va_finance <- filter(fe_total_plot, var=="Share of finance (VA)" & country=="AUT")$fe_val
# test_plot <- ggplot(fe_total_plot[country=="AUT"&var=="Share of finance (VA)"]) + 
# geom_line(aes(x = k_val, y = var_val, color = country, group = country),
#           color = "red", alpha = 0.95
# ) +
#   geom_line(aes(x = k_val, y = fe_val/1000, color = country, group = country),
#             color = "blue", alpha = 0.95
#   ) 
# test_plot
# qplot(1:length(vals_va_finance), vals_va_finance)

fe_k_plot <- ggplot(data = fe_total_plot) +
  geom_line(aes(x = k_val, y = fe_val, color = country, group = country),
            color = "grey", alpha = 0.25
  ) +
  geom_line(
    data = fe_total_plot[country == "AUT"],
    aes(x = k_val, y = var_val, group = country),
    color = "red", alpha = 0.95
  ) +
  xlab("k") +
  scale_x_discrete(expand = c(0, 0)) +
  ylab("FE estimate") +
  facet_wrap(~var, scales = "free") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "white", linetype = "blank"),
    strip.text = element_text(size=14),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size=12)
  )
fe_k_plot

if (length(exclude_countries)>0) {
  file_name <- paste0(
    "output/fe-estimates/fig_2_fe-estimates-emu_k_dependence_excl-",
    paste0(excl_countries, collapse = "-"), ".pdf"
  )
} else {
  file_name <- "output/fe-estimates/fig_2_fe-estimates_k_dependence.pdf"
}
ggsave(fe_k_plot,
       filename = file_name,
       height = fe_k_plot_height,
       width = fe_k_plot_width
)
