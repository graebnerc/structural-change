# This file creates the descriptive statistics for the appendix
rm(list = ls())

# Preparation ----
macro_data_raw <- fread("data/macro_data.csv")

countries_interest <- list()
countries_interest[["Core"]] <- countrycode(c("Austria", "Belgium", "Denmark", "Finland", "Germany", "Sweden"), "country.name", "iso3c")
countries_interest[["Catchup"]] <- countrycode(c("Bulgaria", "Romania", "Czech Republic", "Estonia", "Latvia", "Lithuania", "Hungary", "Poland", "Slovenia", "Slovakia", "Croatia"), "country.name", "iso3c")
countries_interest[["Finance"]] <- countrycode(c("Luxembourg", "Netherlands", "Malta", "Ireland"), "country.name", "iso3c")
countries_interest[["Periphery"]] <- countrycode(c("Cyprus", "France", "Greece", "Italy", "Portugal", "Spain"), "country.name", "iso3c")
countries_of_interest <- unlist(countries_interest)

vars_of_interest <- c("Country", "Year",
                      "complexity_HH", "population", "GDP_pc_PPP", 
                      "ind_output_meur", "Unemployment_rate",
                      "Current_account_balance_to_GDP", "exp_to_gdp",
                      "Public_debt_to_GDP", "Adjusted_wage_share", "employm_indus", 
                      "fdi_in_gdp", "Private_sector_debt_to_GDP",
                      "Tax_Wealth", "finance_share_GO",
                      "Capital_accumulation", "GDP_growth", "Gini_net", "size_of_finance",
                      "FDI_assets_stock", "FDI_liabilities_stock")

macro_data <- macro_data_raw %>%
  filter(
    Country %in% countrycode(countries_of_interest, "iso3c", "un"),
    Year >= 2000 & Year < 2016
  ) %>%
  select(one_of(vars_of_interest)) %>%
  mutate(
    c_group = ifelse(
      Country %in% countrycode(countries_interest[["Core"]], "iso3c", "un"), "core", ifelse(
        Country %in% countrycode(countries_interest[["Catchup"]], "iso3c", "un"), "catchup", ifelse(
          Country %in% countrycode(countries_interest[["Finance"]], "iso3c", "un"), "finance", ifelse(
            Country %in% countrycode(countries_interest[["Periphery"]], "iso3c", "un"), "periphery", NA
          )
        )
      )
    )
  ) %>%
  mutate(foreign_ownership = ((FDI_assets_stock) / (FDI_liabilities_stock)) - 1) %>% # foreign_assests_to_liabilities
  select(-one_of("FDI_assets_stock", "FDI_liabilities_stock"))

descriptives <- macro_data %>%
  select(-one_of("Country", "Year", "c_group")) %>%
  gather(variable, value) %>%
  mutate(value2=as.double(value)) %>%
  group_by(variable) %>%
  summarise(`Mean value`=mean(value, na.rm = T),
            `Standard deviation`=sd(value, na.rm = T),
            Observations=sum(!is.na(value))) %>%
  rename(Variable=variable)

desc_obj <- print.xtable(xtable(descriptives, digits = 4), 
                         file = "output/tab_A2_descriptive-stats.tex", 
                         floating = F,
                         booktabs = T, 
                         include.rownames = F)
