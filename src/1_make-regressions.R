# This file conduct all the relevant local projections in the paper

# Output: Files with FE estimates, i.e. "output/fe-estimates/fe_estimates.feather",
# Projection plots for figures 1, 7 and 8 in the main paper and figures 1-3 in the appendix.

rm(list = setdiff(ls(), c("exclude_countries")))

impulse_responses_aggregated_fig_width <- 8
impulse_responses_aggregated_fig_height <- 10
impulse_responses_disaggregated_fig_width <- 9
impulse_responses_disaggregated_fig_height <- 11

# Data preparation=============================================================
# Set countries that should be excluded for the estimation procedure 
# Use iso3c codes
if (length(exclude_countries)>0){
  excl_countries <- exclude_countries
} else{
  excl_countries <- c()
}

countries_of_interest <- c(
  "Czech Republic", "Estonia", "Latvia", "Lithuania", "Hungary", "Poland",
  "Slovenia", "Slovakia", "Belgium", "Denmark", "Germany", "Austria", "Finland",
  "Sweden", "Greece", "Spain", "France", "Italy", "Portugal", "Ireland",
  "Cyprus", "Luxembourg", "Malta", "Netherlands", "Bulgaria", "Romania"
)
country_groups <- list()
country_groups[["core"]] <- c("Belgium", "Denmark", "Germany", 
                              "Austria", "Finland", "Sweden")
country_groups[["periphery"]] <- c("Greece", "Spain", "France", 
                                   "Italy", "Portugal", "Cyprus")
country_groups[["finance"]] <- c("Luxembourg", "Malta", 
                                 "Netherlands", "Ireland")
country_groups[["catchup"]] <- c("Czech Republic", "Estonia", "Latvia", 
                                 "Lithuania", "Hungary", "Poland", 
                                 "Slovenia", "Slovakia", "Bulgaria", "Romania")

list_EShockentry_data <- fread("data/openness-shocks.csv") %>%
  dplyr::mutate(Country=countrycode(Country, "country.name", "iso3c"))

if (length(excl_countries) > 0) {
  countries_of_interest <- setdiff(countries_of_interest, 
                                   countrycode(excl_countries, 
                                               "iso3c", "country.name"))
}

var_list <- c(
  "Unemployment_rate", "Adjusted_wage_share",
  "GDP_growth", "GDP_pc_ppp_new", 
  "Current_account_balance_to_GDP", "Public_debt_to_GDP", 
  "finance_share_VA", "exp_to_gdp"
)

macro_data <- fread("data/macro_data.csv") %>%
  dplyr::mutate(GDP_pc_ppp_new=GDP_pc_ppp_new/1000) 

macro_data <- macro_data %>%
  left_join(., list_EShockentry_data, by=c("Country"="Country")) %>% 
  dplyr::mutate(EShockentry=ifelse(Year<Shock, 0, 1))
# Add for robustness check w.r.t. Greece as mentioned in appendix:
# EShockentry=ifelse(Country!="GRC", EShockentry, ifelse(Year<1999, 0, 1)))

# Specify estimations==========================================================
shock_var <- "EShockentry"

reg_equation <- list()
reg_equation[["Unemployment_rate"]] <- paste0(
  "~", shock_var, "+GDP_growth+Capital_accumulation+Unemployment_rate_DLAG+GDP_growth_DLAG+Capital_accumulation_DLAG+Unemployment_rate_LAG")
reg_equation[["GDP_growth"]] <- paste0(
  "~", shock_var, "+HBOOM+GDP_growth_DLAG+HBOOM_DLAG")
reg_equation[["Current_account_balance_to_GDP"]] <- paste0(
  "~", shock_var, "+Unemployment_rate+Current_account_balance_to_GDP_DLAG+Unemployment_rate_DLAG+Current_account_balance_to_GDP_LAG")
reg_equation[["exp_to_gdp"]] <- paste0(
  "~", shock_var, "+Unemployment_rate+exp_to_gdp_DLAG+Unemployment_rate_DLAG+exp_to_gdp_LAG")
reg_equation[["Public_debt_to_GDP"]] <- paste0(
  "~", shock_var, "+Capital_accumulation+Public_debt_to_GDP_DLAG+Capital_accumulation_DLAG+Public_debt_to_GDP_LAG")
reg_equation[["Adjusted_wage_share"]] <- paste0(
  "~", shock_var, "+Unemployment_rate+GDP_growth+Unemployment_rate_DLAG+GDP_growth_DLAG")
reg_equation[["GDP_pc_ppp_new"]] <- paste0(
  "~", shock_var, "+HBOOM+Unemployment_rate+GDP_growth_DLAG+HBOOM_DLAG+Unemployment_rate_DLAG")
reg_equation[["finance_share_VA"]]  <- paste0(
  "~", shock_var, "+Capital_accumulation+HBOOM+size_of_finance_DLAG+Capital_accumulation_DLAG+HBOOM_DLAG")

# Make the aggregate projections===============================================
list_of_fe_estimates <- list()
list_of_prejection_plots <- list()
list_of_projection_objects <- list()
list_of_shock_data <- list()

for (v in var_list) {
  print(v)
  if (v == "Gini_net") {
    y_label <- "change in Gini p."
  } else {
    y_label <- "change in perc. p."
  }
  regression_equation <- paste0(v, reg_equation[[v]])
  current_projections <- get_projections(data_obj = macro_data,
                                         regression_formula = regression_equation, 
                                         return_intermediate_data = T)
  print("...finished")
  list_of_fe_estimates[[v]] <- current_projections$fe_estimates
  list_of_prejection_plots[[v]] <- current_projections$impulse_plot
  list_of_projection_objects[[v]] <- current_projections$projections
  list_of_shock_data[[v]] <- current_projections$estimation_data
}

fe_estimates <- rbindlist(list_of_fe_estimates)
if (length(excl_countries) == 0) {
  write_feather(x = fe_estimates, path = "output/fe-estimates/fe_estimates.feather")
} else {
  write_feather(
    x = fe_estimates,
    path = paste0(
      "output/fe-estimates/fe_estimates_excluded-",
      paste0(excl_countries, collapse = "-"),
      ".feather"
    )
  )
}

# Make plots for aggregated projections----------------------------------------

full_plot <- ggpubr::ggarrange(
  list_of_prejection_plots[["Unemployment_rate"]] + 
    ggtitle("Unemployment rate") + ylab("change in perc. p."),
  list_of_prejection_plots[["Adjusted_wage_share"]] + 
    ggtitle("Wage share") + ylab("change in perc. p."),
  list_of_prejection_plots[["GDP_growth"]] + 
    ggtitle("GDP growth") + ylab("change in perc. p."),
  list_of_prejection_plots[["GDP_pc_ppp_new"]] + 
    ggtitle("GDP per capita") + ylab("change in 1000 $ (PPP)"),
  list_of_prejection_plots[["Current_account_balance_to_GDP"]] + 
    ggtitle("Current account balance to GDP") + ylab("change in perc. p."),
  list_of_prejection_plots[["Public_debt_to_GDP"]] + 
    ggtitle("Public debt to GDP") + ylab("change in perc. p."),
  list_of_prejection_plots[["finance_share_VA"]] + 
    ggtitle("Financial sector (% of total value added)") + ylab("change in perc. p."),
  list_of_prejection_plots[["exp_to_gdp"]] + 
    ggtitle("Exports to GDP") + ylab("change in perc. p."),
  ncol = 2, nrow = 4, legend = "none"
)
if (length(excl_countries) == 0) {
  projections_file_name <- "output/impulse-responses/fig_1_aggregated_estimations.pdf"
} else {
  projections_file_name <- paste0(
    "output/impulse-responses/fig_1_aggregated_estimations_excluded-",
    paste0(excl_countries, collapse = "-"), ".pdf"
  )
}
ggsave(
  plot = full_plot,
  filename = projections_file_name,
  height = impulse_responses_aggregated_fig_height,
  width = impulse_responses_aggregated_fig_width
)

print("Starting disaggregated projections!")


# Make the disagreggated projections===========================================

list_of_group_projection_plots <- list()
list_of_group_projection_objects <- list()

for (current_group in names(country_groups)) {
  print(current_group)
  list_of_group_projection_plots[[current_group]] <- list()
  list_of_group_projection_objects[[current_group]] <- list()
  for (v in var_list) {
    print(paste0(current_group, ": ", v))
    
    regression_equation <- paste0(v, reg_equation[[v]])
    
    reduced_data <- dplyr::filter(macro_data, Country %in% countrycode(
      country_groups[[current_group]], "country.name", "iso3c"))
    
    current_projections <- get_projections(data_obj = reduced_data,
                                           regression_formula = regression_equation, 
                                           return_intermediate_data = T)

    list_of_group_projection_plots[[current_group]][[v]] <- current_projections$impulse_plot
    list_of_group_projection_objects[[current_group]][[v]] <- current_projections$projections
  }
}

saveRDS(list_of_group_projection_objects, "output/fe-estimates/lilo-projections-disagg.rds")

# Make plots for disaggregated projections-------------------------------------

fig_7_agg_projections_1 <- ggpubr::ggarrange(
  list_of_group_projection_plots[["core"]][["Unemployment_rate"]] +
    ggtitle("Unemployment rate:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["Unemployment_rate"]] +
    ggtitle("Unemployment rate:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["Unemployment_rate"]] +
    ggtitle("Unemployment rate:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["Unemployment_rate"]] +
    ggtitle("Unemployment rate:\nCatch-up countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["core"]][["Adjusted_wage_share"]] +
    ggtitle("Wage share:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["Adjusted_wage_share"]] +
    ggtitle("Wage share:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["Adjusted_wage_share"]] +
    ggtitle("Wage share:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["Adjusted_wage_share"]] +
    ggtitle("Wage share:\nCatch-up countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["core"]][["GDP_growth"]] +
    ggtitle("GDP growth:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["GDP_growth"]] +
    ggtitle("GDP growth:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["GDP_growth"]] +
    ggtitle("GDP growth:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["GDP_growth"]] +
    ggtitle("GDP growth:\nCatch-up countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["core"]][["GDP_pc_ppp_new"]] +
    ggtitle("GDP per capita:\nCore countries") + ylab("change in 1000 $ (PPP)"),
  list_of_group_projection_plots[["periphery"]][["GDP_pc_ppp_new"]] +
    ggtitle("GDP per capita:\nPeriphery countries") + ylab("change in 1000 $ (PPP)"),
  list_of_group_projection_plots[["finance"]][["GDP_pc_ppp_new"]] +
    ggtitle("GDP per capita:\nFinancial hubs") + ylab("change in 1000 $ (PPP)"),
  list_of_group_projection_plots[["catchup"]][["GDP_pc_ppp_new"]] +
    ggtitle("GDP per capita:\nCatch-up countries") + ylab("change in 1000 $ (PPP)"),
  ncol = 4, nrow = 4, legend = "none"
)

if (length(excl_countries) == 0) {
  projections_file_name <- "output/impulse-responses/fig_7_disaggregated_estimations_1.pdf"
} else {
  projections_file_name <- paste0(
    "output/impulse-responses/fig_1_aggregated_estimations_excluded-",
    paste0(excl_countries, collapse = "-"), ".pdf"
  )
}
ggsave(
  plot = fig_7_agg_projections_1,
  filename = projections_file_name,
  height = impulse_responses_aggregated_fig_height+1,
  width = impulse_responses_aggregated_fig_width+1
)

fig_8_agg_projections_2 <- ggarrange(
  list_of_group_projection_plots[["core"]][["Current_account_balance_to_GDP"]] +
    ggtitle("CA balance to GDP:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["Current_account_balance_to_GDP"]] +
    ggtitle("CA balance to GDP:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["Current_account_balance_to_GDP"]] +
    ggtitle("CA balance to GDP:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["Current_account_balance_to_GDP"]] +
    ggtitle("CA balance to GDP:\nCatch-up countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["core"]][["Public_debt_to_GDP"]] +
    ggtitle("Public debt to GDP:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["Public_debt_to_GDP"]] +
    ggtitle("Public debt to GDP:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["Public_debt_to_GDP"]] +
    ggtitle("Public debt to GDP:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["Public_debt_to_GDP"]] +
    ggtitle("Public debt to GDP:\nCatch-up countries"),
  list_of_group_projection_plots[["core"]][["finance_share_VA"]] +
    ggtitle("Share finance:\nCore countries") + 
    scale_y_continuous(name = "change in ppm", labels = function(x)x*10000),
  list_of_group_projection_plots[["periphery"]][["finance_share_VA"]] +
    ggtitle("Share finance:\nPeriphery countries") +
    scale_y_continuous(name = "change in ppm", labels = function(x)x*10000),
  list_of_group_projection_plots[["finance"]][["finance_share_VA"]] +
    ggtitle("Share finance:\nFinancial hubs") + 
    scale_y_continuous(name = "change in ppm", labels = function(x)x*10000),
  list_of_group_projection_plots[["catchup"]][["finance_share_VA"]] +
    ggtitle("Share finance:\nCatch-up countries") +
    scale_y_continuous(name = "change in ppm", labels = function(x)x*10000),
  list_of_group_projection_plots[["core"]][["exp_to_gdp"]] +
    ggtitle("Exports to GDP:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["exp_to_gdp"]] +
    ggtitle("Exports to GDP:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["exp_to_gdp"]] +
    ggtitle("Exports to GDP:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["exp_to_gdp"]] +
    ggtitle("Exports to GDP:\nCatch-up countries") + ylab("change in perc. p."),
  ncol = 4, nrow = 4
)

if (length(excl_countries) == 0) {
  projections_file_name <- "output/impulse-responses/fig_8_disaggregated_estimations_2.pdf"
} else {
  projections_file_name <- paste0(
    "output/impulse-responses/fig_8_disaggregated_estimations_2_excl-",
    paste0(excl_countries, collapse = "-"), ".pdf"
  )
}
ggsave(
  plot = fig_8_agg_projections_2,
  filename = projections_file_name,
  height = impulse_responses_aggregated_fig_height+1,
  width = impulse_responses_aggregated_fig_width+1
)

# Create FE estimates with finance cluster excluded (for the appendix)=========

list_of_fe_estimates_no_finance <- list()
list_of_projection_plots_no_finance <- list()
list_of_projection_objects_no_finance <- list()
list_of_shock_data_no_finance <- list()
exclude <- countrycode(c("Cyprus", "Luxembourg", "Malta"), 
                       "country.name", "iso3c")
macro_data_no_finance <- macro_data %>%
  dplyr::filter(!Country %in% exclude)

for (v in var_list) {
  print(v)
  if (v == "Gini_net") {
    y_label <- "change in Gini p."
  } else {
    y_label <- "change in perc. p."
  }
  regression_equation <- paste0(v, reg_equation[[v]])
  current_projections <- get_projections(data_obj = macro_data_no_finance,
                                         regression_formula = regression_equation, 
                                         return_intermediate_data = T)
  print("...finished")
  list_of_fe_estimates_no_finance[[v]] <- current_projections$fe_estimates
  list_of_projection_plots_no_finance[[v]] <- current_projections$impulse_plot
  list_of_projection_objects_no_finance[[v]] <- current_projections$projections
  list_of_shock_data_no_finance[[v]] <- current_projections$estimation_data
}

fe_estimates_no_finance <- rbindlist(list_of_fe_estimates_no_finance)
if (length(excl_countries) == 0) {
  write_feather(x = fe_estimates_no_finance, 
                path = "output/fe-estimates/fe_estimates_no_finance.feather")
} else {
  write_feather(
    x = fe_estimates_no_finance,
    path = paste0(
      "output/fe-estimates/fe_estimates_no_finance_excluded-",
      paste0(excl_countries, collapse = "-"),
      ".feather"
    )
  )
}

# Projections with the KOF index (for the appendix)============================
shock_var <- "kof_econ"

reg_equation <- list()
reg_equation[["Unemployment_rate"]] <- paste0(
  "~", shock_var, "+GDP_growth+Capital_accumulation+Unemployment_rate_DLAG+GDP_growth_DLAG+Capital_accumulation_DLAG+Unemployment_rate_LAG")
reg_equation[["GDP_growth"]] <- paste0(
  "~", shock_var, "+HBOOM+GDP_growth_DLAG+HBOOM_DLAG")
reg_equation[["Current_account_balance_to_GDP"]] <- paste0(
  "~", shock_var, "+Unemployment_rate+Current_account_balance_to_GDP_DLAG+Unemployment_rate_DLAG+Current_account_balance_to_GDP_LAG")
reg_equation[["exp_to_gdp"]] <- paste0(
  "~", shock_var, "+Unemployment_rate+exp_to_gdp_DLAG+Unemployment_rate_DLAG+exp_to_gdp_LAG")
reg_equation[["Public_debt_to_GDP"]] <- paste0(
  "~", shock_var, "+Capital_accumulation+Public_debt_to_GDP_DLAG+Capital_accumulation_DLAG+Public_debt_to_GDP_LAG")
reg_equation[["Adjusted_wage_share"]] <- paste0(
  "~", shock_var, "+Unemployment_rate+GDP_growth+Unemployment_rate_DLAG+GDP_growth_DLAG")
reg_equation[["GDP_pc_ppp_new"]] <- paste0(
  "~", shock_var, "+HBOOM+Unemployment_rate+GDP_growth_DLAG+HBOOM_DLAG+Unemployment_rate_DLAG")
reg_equation[["finance_share_VA"]]  <- paste0(
  "~", shock_var, "+Capital_accumulation+HBOOM+size_of_finance_DLAG+Capital_accumulation_DLAG+HBOOM_DLAG")

# Aggregated projections with the KOF index (for the appendix)=================
list_of_fe_estimates <- list()
list_of_prejection_plots <- list()
list_of_projection_objects <- list()
list_of_shock_data <- list()

for (v in var_list) {
  print(v)
  if (v == "Gini_net") {
    y_label <- "change in Gini p."
  } else {
    y_label <- "change in perc. p."
  }
  regression_equation <- paste0(v, reg_equation[[v]])
  current_projections <- get_projections(data_obj = macro_data,
                                         regression_formula = regression_equation, 
                                         return_intermediate_data = T)
  print("...finished")
  list_of_fe_estimates[[v]] <- current_projections$fe_estimates
  list_of_prejection_plots[[v]] <- current_projections$impulse_plot
  list_of_projection_objects[[v]] <- current_projections$projections
  list_of_shock_data[[v]] <- current_projections$estimation_data
}

fe_estimates <- rbindlist(list_of_fe_estimates)
if (length(excl_countries) == 0) {
  write_feather(x = fe_estimates, path = "output/fe-estimates/fe_estimates_kof.feather")
} else {
  write_feather(
    x = fe_estimates,
    path = paste0(
      "output/fe-estimates/fe_estimates_kof_excluded-",
      paste0(excl_countries, collapse = "-"),
      ".feather"
    )
  )
}

# Make plots for aggregated projections----------------------------------------

full_plot <- ggpubr::ggarrange(
  list_of_prejection_plots[["Unemployment_rate"]] + 
    ggtitle("Unemployment rate") + ylab("change in perc. p."),
  list_of_prejection_plots[["Adjusted_wage_share"]] + 
    ggtitle("Wage share") + ylab("change in perc. p."),
  list_of_prejection_plots[["GDP_growth"]] + 
    ggtitle("GDP growth") + ylab("change in perc. p."),
  list_of_prejection_plots[["GDP_pc_ppp_new"]] + 
    ggtitle("GDP per capita") + ylab("change in 1000 $ (PPP)"),
  list_of_prejection_plots[["Current_account_balance_to_GDP"]] + 
    ggtitle("Current account balance to GDP") + ylab("change in perc. p."),
  list_of_prejection_plots[["Public_debt_to_GDP"]] + 
    ggtitle("Public debt to GDP") + ylab("change in perc. p."),
  list_of_prejection_plots[["finance_share_VA"]] + 
    ggtitle("Financial sector (% of total value added)") + ylab("change in perc. p."),
  list_of_prejection_plots[["exp_to_gdp"]] + 
    ggtitle("Exports to GDP") + ylab("change in perc. p."),
  ncol = 2, nrow = 4, legend = "none"
)
if (length(excl_countries) == 0) {
  projections_file_name <- "output/impulse-responses/fig_A2_aggregated_estimations_kof.pdf"
} else {
  projections_file_name <- paste0(
    "output/impulse-responses/fig_A2_aggregated_estimations_kof_excluded-",
    paste0(excl_countries, collapse = "-"), ".pdf"
  )
}
ggsave(
  plot = full_plot,
  filename = projections_file_name,
  height = impulse_responses_aggregated_fig_height,
  width = impulse_responses_aggregated_fig_width
)


# Disaggregated projections with the KOF index (for the appendix)==============

list_of_group_projection_plots <- list()
list_of_group_projection_objects <- list()

for (current_group in names(country_groups)) {
  print(current_group)
  list_of_group_projection_plots[[current_group]] <- list()
  list_of_group_projection_objects[[current_group]] <- list()
  for (v in var_list) {
    print(paste0(current_group, ": ", v))
    
    regression_equation <- paste0(v, reg_equation[[v]])
    
    reduced_data <- dplyr::filter(macro_data, Country %in% countrycode(
      country_groups[[current_group]], "country.name", "iso3c"))
    
    current_projections <- get_projections(data_obj = reduced_data,
                                           regression_formula = regression_equation, 
                                           return_intermediate_data = T)
    
    list_of_group_projection_plots[[current_group]][[v]] <- current_projections$impulse_plot
    list_of_group_projection_objects[[current_group]][[v]] <- current_projections$projections
  }
}

# Make plots for disaggregated projections-------------------------------------

fig_A2_agg_projections_1 <- ggpubr::ggarrange(
  list_of_group_projection_plots[["core"]][["Unemployment_rate"]] +
    ggtitle("Unemployment rate:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["Unemployment_rate"]] +
    ggtitle("Unemployment rate:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["Unemployment_rate"]] +
    ggtitle("Unemployment rate:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["Unemployment_rate"]] +
    ggtitle("Unemployment rate:\nCatch-up countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["core"]][["Adjusted_wage_share"]] +
    ggtitle("Wage share:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["Adjusted_wage_share"]] +
    ggtitle("Wage share:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["Adjusted_wage_share"]] +
    ggtitle("Wage share:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["Adjusted_wage_share"]] +
    ggtitle("Wage share:\nCatch-up countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["core"]][["GDP_growth"]] +
    ggtitle("GDP growth:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["GDP_growth"]] +
    ggtitle("GDP growth:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["GDP_growth"]] +
    ggtitle("GDP growth:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["GDP_growth"]] +
    ggtitle("GDP growth:\nCatch-up countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["core"]][["GDP_pc_ppp_new"]] +
    ggtitle("GDP per capita:\nCore countries") + ylab("change in 1000 $ (PPP)"),
  list_of_group_projection_plots[["periphery"]][["GDP_pc_ppp_new"]] +
    ggtitle("GDP per capita:\nPeriphery countries") + ylab("change in 1000 $ (PPP)"),
  list_of_group_projection_plots[["finance"]][["GDP_pc_ppp_new"]] +
    ggtitle("GDP per capita:\nFinancial hubs") + ylab("change in 1000 $ (PPP)"),
  list_of_group_projection_plots[["catchup"]][["GDP_pc_ppp_new"]] +
    ggtitle("GDP per capita:\nCatch-up countries") + ylab("change in 1000 $ (PPP)"),
  ncol = 4, nrow = 4, legend = "none"
)

if (length(excl_countries) == 0) {
  projections_file_name <- "output/impulse-responses/fig_A3_grouped_estimations_kof_1.pdf"
} else {
  projections_file_name <- paste0(
    "output/impulse-responses/fig_A3_grouped_estimations_kof_1_excluded-",
    paste0(excl_countries, collapse = "-"), ".pdf"
  )
}
ggsave(
  plot = fig_A2_agg_projections_1,
  filename = projections_file_name,
  height = impulse_responses_aggregated_fig_height+1,
  width = impulse_responses_aggregated_fig_width+1
)

fig_A3_agg_projections_2 <- ggarrange(
  list_of_group_projection_plots[["core"]][["Current_account_balance_to_GDP"]] +
    ggtitle("CA balance to GDP:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["Current_account_balance_to_GDP"]] +
    ggtitle("CA balance to GDP:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["Current_account_balance_to_GDP"]] +
    ggtitle("CA balance to GDP:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["Current_account_balance_to_GDP"]] +
    ggtitle("CA balance to GDP:\nCatch-up countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["core"]][["Public_debt_to_GDP"]] +
    ggtitle("Public debt to GDP:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["Public_debt_to_GDP"]] +
    ggtitle("Public debt to GDP:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["Public_debt_to_GDP"]] +
    ggtitle("Public debt to GDP:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["Public_debt_to_GDP"]] +
    ggtitle("Public debt to GDP:\nCatch-up countries"),
  list_of_group_projection_plots[["core"]][["finance_share_VA"]] +
    ggtitle("Share finance:\nCore countries") + 
    scale_y_continuous(name = "change in ppm", labels = function(x)x*10000),
  list_of_group_projection_plots[["periphery"]][["finance_share_VA"]] +
    ggtitle("Share finance:\nPeriphery countries") +
    scale_y_continuous(name = "change in ppm", labels = function(x)x*10000),
  list_of_group_projection_plots[["finance"]][["finance_share_VA"]] +
    ggtitle("Share finance:\nFinancial hubs") + 
    scale_y_continuous(name = "change in ppm", labels = function(x)x*10000),
  list_of_group_projection_plots[["catchup"]][["finance_share_VA"]] +
    ggtitle("Share finance:\nCatch-up countries") +
    scale_y_continuous(name = "change in ppm", labels = function(x)x*10000),
  list_of_group_projection_plots[["core"]][["exp_to_gdp"]] +
    ggtitle("Exports to GDP:\nCore countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["periphery"]][["exp_to_gdp"]] +
    ggtitle("Exports to GDP:\nPeriphery countries") + ylab("change in perc. p."),
  list_of_group_projection_plots[["finance"]][["exp_to_gdp"]] +
    ggtitle("Exports to GDP:\nFinancial hubs") + ylab("change in perc. p."),
  list_of_group_projection_plots[["catchup"]][["exp_to_gdp"]] +
    ggtitle("Exports to GDP:\nCatch-up countries") + ylab("change in perc. p."),
  ncol = 4, nrow = 4
)

if (length(excl_countries) == 0) {
  projections_file_name <- "output/impulse-responses/fig_A4_grouped_estimations_kof_2.pdf"
} else {
  projections_file_name <- paste0(
    "output/impulse-responses/fig_A4_grouped_estimations_kof_2_excl-",
    paste0(excl_countries, collapse = "-"), ".pdf"
  )
}
ggsave(
  plot = fig_A3_agg_projections_2,
  filename = projections_file_name,
  height = impulse_responses_aggregated_fig_height+1,
  width = impulse_responses_aggregated_fig_width+1
)

# Conduct projections with EU entry as shock variable (for the appendix)=======
macro_data_eu_entry <- fread("data/macro_data.csv") %>%
  dplyr::mutate(GDP_pc_ppp_new=GDP_pc_ppp_new/1000) %>%
  left_join(., list_EShockentry_data, by=c("Country"="Country")) %>% 
  dplyr::mutate(EShockentry=ifelse(Year<EUEntry, 0, 1))

shock_var <- "EShockentry"

reg_equation <- list()
reg_equation[["Unemployment_rate"]] <- paste0(
  "~", shock_var, "+GDP_growth+Capital_accumulation+Unemployment_rate_DLAG+GDP_growth_DLAG+Capital_accumulation_DLAG+Unemployment_rate_LAG")
reg_equation[["GDP_growth"]] <- paste0(
  "~", shock_var, "+HBOOM+GDP_growth_DLAG+HBOOM_DLAG")
reg_equation[["Current_account_balance_to_GDP"]] <- paste0(
  "~", shock_var, "+Unemployment_rate+Current_account_balance_to_GDP_DLAG+Unemployment_rate_DLAG+Current_account_balance_to_GDP_LAG")
reg_equation[["exp_to_gdp"]] <- paste0(
  "~", shock_var, "+Unemployment_rate+exp_to_gdp_DLAG+Unemployment_rate_DLAG+exp_to_gdp_LAG")
reg_equation[["Public_debt_to_GDP"]] <- paste0(
  "~", shock_var, "+Capital_accumulation+Public_debt_to_GDP_DLAG+Capital_accumulation_DLAG+Public_debt_to_GDP_LAG")
reg_equation[["Adjusted_wage_share"]] <- paste0(
  "~", shock_var, "+Unemployment_rate+GDP_growth+Unemployment_rate_DLAG+GDP_growth_DLAG")
reg_equation[["GDP_pc_ppp_new"]] <- paste0(
  "~", shock_var, "+HBOOM+Unemployment_rate+GDP_growth_DLAG+HBOOM_DLAG+Unemployment_rate_DLAG")
reg_equation[["finance_share_VA"]]  <- paste0(
  "~", shock_var, "+Capital_accumulation+HBOOM+size_of_finance_DLAG+Capital_accumulation_DLAG+HBOOM_DLAG")

# Make the aggregate projections-----------------------------------------------
list_of_fe_estimates_eu_entry <- list()
list_of_prejection_plots_eu_entry <- list()
list_of_projection_objects_eu_entry <- list()
list_of_shock_data_eu_entry <- list()

for (v in var_list) {
  print(v)
  regression_equation <- paste0(v, reg_equation[[v]])
  current_projections <- get_projections(data_obj = macro_data_eu_entry,
                                         regression_formula = regression_equation, 
                                         return_intermediate_data = T)
  print("...finished")
  list_of_fe_estimates_eu_entry[[v]] <- current_projections$fe_estimates
  list_of_prejection_plots_eu_entry[[v]] <- current_projections$impulse_plot
  list_of_projection_objects_eu_entry[[v]] <- current_projections$projections
  list_of_shock_data_eu_entry[[v]] <- current_projections$estimation_data
}

fe_estimates_eu_entry <- rbindlist(list_of_fe_estimates_eu_entry)
if (length(excl_countries) == 0) {
  write_feather(x = fe_estimates_eu_entry, path = "output/fe-estimates/fe_estimates_eu_entry.feather")
} else {
  write_feather(
    x = fe_estimates_eu_entry,
    path = paste0(
      "output/fe-estimates/fe_estimates_excluded-",
      paste0(excl_countries, collapse = "-"),
      "eu_entry.feather"
    )
  )
}
