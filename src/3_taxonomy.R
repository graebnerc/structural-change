# This file re-creates figure 4 of the paper, which provides descriptive evidence for the four
# theoretical country groups in Europe. Figure 7 in the appendix is also created.
# TODO: substitute complexity_HH with complexity_HH
rm(list = ls())

source("src/3_taxonomy-functions.R")
source("src/new_color_palette.R")
source("src/ggplot_theme_update.R")
golden_ratio <- 1.6180339887498948482
pct <- function(old, new) {
  return((new - old) / old)
}

# Preparation==================================================================
macro_data_raw <- fread("data/macro_data.csv")

countries_interest <- list()
countries_interest[["Core"]] <- countrycode(c("Austria", "Belgium", "Denmark", 
                                              "Finland", "Germany", "Sweden"), 
                                            "country.name", "iso3c")
countries_interest[["Catchup"]] <- countrycode(c("Bulgaria", "Romania", 
                                                 "Czech Republic", "Estonia", 
                                                 "Latvia", "Lithuania", 
                                                 "Hungary", "Poland", "Slovenia", 
                                                 "Slovakia", "Croatia"), 
                                               "country.name", "iso3c")
countries_interest[["Finance"]] <- countrycode(c("Luxembourg", "Netherlands", 
                                                 "Malta", "Ireland"), 
                                               "country.name", "iso3c")
countries_interest[["Periphery"]] <- countrycode(c("Cyprus", "France", "Greece", 
                                                   "Italy", "Portugal", "Spain"), 
                                                 "country.name", "iso3c")
countries_of_interest <- unlist(countries_interest)

vars_of_interest <- c("Country", "Year",
                      "complexity_HH", "population", "GDP_pc_PPP", 
                      "ind_output_meur", "Unemployment_rate",
                      "Current_account_balance_to_GDP", "exp_to_gdp",
                      "Public_debt_to_GDP", "Adjusted_wage_share", 
                      "employm_indus", "fdi_in_gdp", "Private_sector_debt_to_GDP",
                      "Tax_Wealth", "finance_share_GO",
                      "Capital_accumulation", "GDP_growth", "Gini_net", 
                      "size_of_finance", "FDI_assets_stock", "FDI_liabilities_stock")


macro_data <- macro_data_raw %>%
  dplyr::filter(
    Country %in% countries_of_interest,
    Year >= 2000 & Year < 2016
  ) %>%
  select(one_of(vars_of_interest)) %>%
  mutate(
    c_group = ifelse(
      Country %in% countries_interest[["Core"]], "core", ifelse(
        Country %in% countries_interest[["Catchup"]], "catchup", ifelse(
          Country %in% countries_interest[["Finance"]], "finance", ifelse(
            Country %in% countries_interest[["Periphery"]], "periphery", NA
          )
        )
      )
    )
  ) %>%
  mutate(foreign_ownership = ((FDI_assets_stock) / (FDI_liabilities_stock)) - 1) # foreign_assests_to_liabilities



# The core=====================================================================
macro_data_agg_core <- macro_data %>%
  mutate(c_group = ifelse(c_group == "core", "core", "other")) %>%
  group_by(c_group, Year) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  ungroup()

macro_data_agg_core_weighted <- macro_data %>%
  mutate(c_group = ifelse(c_group == "core", "core", "other")) %>%
  group_by(c_group, Year) %>%
  summarise(
    complexity_HH = weighted.mean(complexity_HH, population, na.rm=TRUE),
    GDP_pc_PPP = weighted.mean(GDP_pc_PPP, population, na.rm=TRUE),
    ind_output_meur = weighted.mean(ind_output_meur, population, na.rm=TRUE),
    Unemployment_rate = weighted.mean(Unemployment_rate, population, na.rm=TRUE)
  ) %>%
  ungroup()

# Core - unweighted - data-----------------------------------------------------
macro_data_agg_core_mean <- macro_data_agg_core %>%
  group_by(c_group) %>%
  summarise(
    GDP = mean(GDP_pc_PPP, na.rm=TRUE),
    `Industrial product.` = mean(ind_output_meur, na.rm=TRUE),
    Complexity = mean(complexity_HH, na.rm=TRUE),
    Unemployment = mean(Unemployment_rate, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_core_mean)

macro_data_agg_core_sd <- macro_data_agg_core %>%
  group_by(c_group) %>%
  summarise(
    GDP = sd(GDP_pc_PPP, na.rm=TRUE),
    `Industrial product.` = sd(ind_output_meur, na.rm=TRUE),
    Complexity = sd(complexity_HH, na.rm=TRUE),
    Unemployment = sd(Unemployment_rate, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_core_sd)

macro_data_core_peri_sum <- left_join(macro_data_agg_core_mean, 
                                      macro_data_agg_core_sd, 
                                      by = c("c_group", "variable"))

# Core - unweighted - plots----------------------------------------------------

complexity <- make_single_plot(macro_data_core_peri_sum,
  "Complexity",
  # y_limit = c(40, 65),
  leg_pos = "none",
  y_label = "Economic complexity index"
)
complexity 

ind_prod <- make_single_plot(macro_data_core_peri_sum,
  "Industrial product.",
  leg_pos = "none",
  y_label = "Trillion EUR (current prices)"
) + ggtitle("Indus. output") + 
  scale_y_continuous(labels = scales::number_format(scale=0.000001, 
                                                    accuracy = 0.5),
                     expand = c(0, 0))
ind_prod

unemp <- make_single_plot(
  macro_data_core_peri_sum,
  "Unemployment",
  leg_pos = "none",
  y_label = "Percentage of active population"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
unemp 

gdp_pc <- make_single_plot(macro_data_core_peri_sum,
                           "GDP",
                           "GDP per capita",
                           leg_pos = "none",
                           y_label = "GDP per capita (thousand PPP, constant prices)"
) + scale_y_continuous(labels = scales::number_format(scale=0.001),
                       expand = c(0, 0))
gdp_pc

pdf("output/taxonomy/fig_4a_taxonomy-core.pdf", width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(ggarrange(complexity, gdp_pc, ind_prod, unemp,
  ncol = 4,
  common.legend = TRUE,
  legend = "bottom"
),
vp = vplayout(1, 1)
)
dev.off()

# Core weighted - data---------------------------------------------------------

macro_data_agg_core_mean_w <- macro_data_agg_core_weighted %>%
  group_by(c_group) %>%
  summarise(
    GDP = mean(GDP_pc_PPP, na.rm=TRUE),
    `Industrial product.` = mean(ind_output_meur, na.rm=TRUE),
    Complexity = mean(complexity_HH, na.rm=TRUE),
    # FDI_stock=mean(FDI_assets_stock, na.rm=TRUE),
    Unemployment = mean(Unemployment_rate, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_core_mean_w)

macro_data_agg_core_sd_w <- macro_data_agg_core_weighted %>%
  group_by(c_group) %>%
  summarise(
    GDP = sd(GDP_pc_PPP, na.rm=TRUE),
    `Industrial product.` = sd(ind_output_meur, na.rm=TRUE),
    Complexity = sd(complexity_HH, na.rm=TRUE),
    Unemployment = sd(Unemployment_rate, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_core_sd_w)

macro_data_core_peri_sum_w <- left_join(macro_data_agg_core_mean_w, 
                                        macro_data_agg_core_sd_w, 
                                        by=c("c_group", "variable"))

# Core weighted - plot---------------------------------------------------------

complexity_w <- make_single_plot(macro_data_core_peri_sum_w,
  "Complexity",
  # y_limit = c(40, 65),
  leg_pos = "none",
  y_label = "Economic complexity index"
)
complexity_w

ind_prod_w <- make_single_plot(macro_data_core_peri_sum_w,
                               "Industrial product.",
                               leg_pos = "none",
                               y_label = "Trillion EUR (current prices)"
) + ggtitle("Indus. output") + 
  scale_y_continuous(labels = scales::number_format(scale=0.000001, 
                                                    accuracy = 0.5),
                     expand = c(0, 0))
ind_prod_w

unemp_w <- make_single_plot(macro_data_core_peri_sum_w,
  "Unemployment",
  leg_pos = "none",
  y_label = "Percentage of active population"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
unemp_w

gdp_pc_w <- make_single_plot(macro_data_core_peri_sum_w,
                             "GDP",
                             "GDP per capita",
                             leg_pos = "none",
                             y_label = "GDP per capita (thousand PPP, constant prices)"
) + scale_y_continuous(labels = scales::number_format(scale=0.001),
                       expand = c(0, 0))
gdp_pc_w


pdf("output/taxonomy/fig_A8a_taxonomy-core-weighted.pdf", width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(ggarrange(complexity_w, gdp_pc_w, ind_prod_w, unemp_w,
  ncol = 4,
  common.legend = TRUE,
  legend = "bottom"
),
vp = vplayout(1, 1)
)
dev.off()




# The periphery================================================================

macro_data_agg_peri <- macro_data %>%
  mutate(c_group = ifelse(c_group == "periphery", "periphery", "other")) %>%
  group_by(c_group, Year) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  ungroup()

macro_data_agg_peri_weighted <- macro_data %>%
  mutate(c_group = ifelse(c_group == "periphery", "periphery", "other")) %>%
  group_by(c_group, Year) %>%
  summarise(
    Current_account_balance_to_GDP = weighted.mean(Current_account_balance_to_GDP, 
                                                   population, na.rm=TRUE),
    exp_to_gdp = weighted.mean(exp_to_gdp, population, na.rm=TRUE),
    Unemployment_rate = weighted.mean(Unemployment_rate, population, na.rm=TRUE),
    Public_debt_to_GDP = weighted.mean(Public_debt_to_GDP, population, na.rm=TRUE)
  ) %>%
  ungroup()

# Periphery unweighted - data--------------------------------------------------

macro_data_agg_peri_mean <- macro_data_agg_peri %>%
  group_by(c_group) %>%
  summarise(
    `Exports/GDP` = mean(exp_to_gdp, na.rm=TRUE),
    `Current account` = mean(Current_account_balance_to_GDP, na.rm=TRUE),
    Unemployment = mean(Unemployment_rate, na.rm=TRUE),
    `Public debt` = mean(Public_debt_to_GDP, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_peri_mean)

macro_data_agg_peri_sd <- macro_data_agg_peri %>%
  group_by(c_group) %>%
  summarise(
    `Exports/GDP` = sd(exp_to_gdp, na.rm=TRUE),
    `Current account` = sd(Current_account_balance_to_GDP, na.rm=TRUE),
    Unemployment = sd(Unemployment_rate, na.rm=TRUE),
    `Public debt` = sd(Public_debt_to_GDP, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_peri_sd)

macro_data_agg_peri_sum <- left_join(macro_data_agg_peri_mean, 
                                     macro_data_agg_peri_sd, 
                                     by=c("c_group", "variable"))
macro_data_agg_peri_sum <- macro_data_agg_peri_sum %>%
  mutate(c_group = ifelse(c_group == "periphery", "aperiphery", c_group))


# Periphery - unweighted - plots-----------------------------------------------

curr_acc <- make_single_plot(macro_data_agg_peri_sum,
  "Current account",
  leg_pos = "none",
  y_label = "Percentage of GDP",
  y_limit = c(-10, 2)
)  + 
  scale_y_continuous(
    breaks = seq(-10, 2, by=1.0),
    labels = scales::percent_format(scale = 1, accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_icae_public(palette = "hot", labels=c("periphery", "other")) +
  scale_color_icae_public(palette = "hot", labels=c("periphery", "other"))
curr_acc

exp_gdp <- make_single_plot(macro_data_agg_peri_sum,
  "Exports/GDP",
  "Exports to GDP",
  leg_pos = "none",
  y_label = "Percentage of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_icae_public(palette = "hot", labels=c("periphery", "other")) +
  scale_color_icae_public(palette = "hot", labels=c("periphery", "other"))
exp_gdp

pub_debt <- make_single_plot(macro_data_agg_peri_sum,
  "Public debt",
  leg_pos = "none",
  y_label = "Percentage of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_icae_public(palette = "hot", labels=c("periphery", "other")) +
  scale_color_icae_public(palette = "hot", labels=c("periphery", "other"))
pub_debt

unemp <- make_single_plot(macro_data_agg_peri_sum,
  "Unemployment",
  leg_pos = "none",
  y_label = "Percentage of active population"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_icae_public(palette = "hot", labels=c("periphery", "other")) +
  scale_color_icae_public(palette = "hot", labels=c("periphery", "other"))
unemp

pdf("output/taxonomy/fig_4b_taxonomy-periphery.pdf", width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(ggarrange(curr_acc, exp_gdp, pub_debt, unemp,
  ncol = 4,
  common.legend = TRUE,
  legend = "bottom"
),
vp = vplayout(1, 1)
)
dev.off()


# Periphery weighted - data----------------------------------------------------

macro_data_agg_peri_mean_w <- macro_data_agg_peri_weighted %>%
  group_by(c_group) %>%
  summarise(
    `Exports/GDP` = mean(exp_to_gdp, na.rm=TRUE),
    `Current account` = mean(Current_account_balance_to_GDP, na.rm=TRUE),
    Unemployment = mean(Unemployment_rate, na.rm=TRUE),
    `Public debt` = mean(Public_debt_to_GDP, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_peri_mean_w)

macro_data_agg_peri_sd_w <- macro_data_agg_peri_weighted %>%
  group_by(c_group) %>%
  summarise(
    `Exports/GDP` = sd(exp_to_gdp, na.rm=TRUE),
    `Current account` = sd(Current_account_balance_to_GDP, na.rm=TRUE),
    Unemployment = sd(Unemployment_rate, na.rm=TRUE),
    `Public debt` = sd(Public_debt_to_GDP, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_peri_sd_w)

macro_data_agg_peri_sum_w <- left_join(macro_data_agg_peri_mean_w, 
                                       macro_data_agg_peri_sd_w, 
                                       by=c("c_group", "variable"))
macro_data_agg_peri_sum_w <- macro_data_agg_peri_sum_w %>%
  mutate(c_group = ifelse(c_group == "periphery", "aperiphery", c_group))

# Periphery weighted - plots---------------------------------------------------

curr_acc_w <- make_single_plot(macro_data_agg_peri_sum_w,
  "Current account",
  leg_pos = "none",
  y_label = "Percentage of GDP",
  y_limit = c(-10, 3)
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_icae_public(palette = "hot", labels=c("periphery", "other")) +
  scale_color_icae_public(palette = "hot", labels=c("periphery", "other"))
curr_acc_w

exp_gdp_w <- make_single_plot(macro_data_agg_peri_sum_w,
  "Exports/GDP",
  "Exports to GDP",
  leg_pos = "none",
  y_label = "Percentage of GDP"
)  + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_icae_public(palette = "hot", labels=c("periphery", "other")) +
  scale_color_icae_public(palette = "hot", labels=c("periphery", "other"))
exp_gdp_w

pub_debt_w <- make_single_plot(macro_data_agg_peri_sum_w,
  "Public debt",
  leg_pos = "none",
  y_label = "Percentage of GDP"
)  + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_icae_public(palette = "hot", labels=c("periphery", "other")) +
  scale_color_icae_public(palette = "hot", labels=c("periphery", "other"))
pub_debt_w

unemp_w <- make_single_plot(macro_data_agg_peri_sum_w,
  "Unemployment",
  leg_pos = "none",
  y_label = "Percentage of active population"
)  + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  ) +
  scale_fill_icae_public(palette = "hot", labels=c("periphery", "other")) +
  scale_color_icae_public(palette = "hot", labels=c("periphery", "other"))
unemp_w

pdf("output/taxonomy/fig_A8b_taxonomy-periphery-weighted.pdf", width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(ggarrange(curr_acc_w, exp_gdp_w, pub_debt_w, unemp_w,
  ncol = 4,
  common.legend = TRUE,
  legend = "bottom"
),
vp = vplayout(1, 1)
)
dev.off()


# Catch-up=====================================================================
macro_data_agg_catchup <- macro_data %>%
  mutate(c_group = ifelse(c_group == "catchup", "catchup", "other")) %>%
  group_by(c_group, Year) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  ungroup()

macro_data_agg_catchup_weighted <- macro_data %>%
  mutate(c_group = ifelse(c_group == "catchup", "catchup", "other")) %>%
  group_by(c_group, Year) %>%
  summarise(
    Adjusted_wage_share = weighted.mean(Adjusted_wage_share, population, na.rm=TRUE),
    GDP_pc_PPP = weighted.mean(GDP_pc_PPP, population, na.rm=TRUE),
    employm_indus = weighted.mean(employm_indus, population, na.rm=TRUE),
    foreign_ownership = weighted.mean(foreign_ownership, population, na.rm=TRUE)
  ) %>%
  ungroup()

# Catchup - unweighted - data--------------------------------------------------

macro_data_agg_catchup_mean <- macro_data_agg_catchup %>%
  group_by(c_group) %>%
  summarise(
    `Wage share` = mean(Adjusted_wage_share, na.rm=TRUE),
    `GDP pc (PPP)` = mean(GDP_pc_PPP, na.rm=TRUE),
    `Foreign ownership` = mean(foreign_ownership, na.rm=TRUE),
    `Industial empl.` = mean(employm_indus, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_catchup_mean)

macro_data_agg_catchup_sd <- macro_data_agg_catchup %>%
  group_by(c_group) %>%
  summarise(
    `Wage share` = sd(Adjusted_wage_share, na.rm=TRUE),
    `GDP pc (PPP)` = sd(GDP_pc_PPP, na.rm=TRUE),
    `Foreign ownership` = sd(foreign_ownership, na.rm=TRUE),
    `Industial empl.` = sd(employm_indus, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_catchup_sd)

macro_data_agg_catchup_sum <- left_join(macro_data_agg_catchup_mean, 
                                        macro_data_agg_catchup_sd, 
                                        by=c("c_group", "variable"))

# Catchup - unweighted - plots-------------------------------------------------

wage_share <- make_single_plot(macro_data_agg_catchup_sum,
  "Wage share",
  y_limit = c(40, 65),
  leg_pos = "none",
  y_label = "Adj. wage share in % of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
wage_share

ind_emp <- make_single_plot(macro_data_agg_catchup_sum,
  "Industial empl.",
  leg_pos = "none",
  y_label = "Percentage of total employment"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
ind_emp

for_own <- make_single_plot(macro_data_agg_catchup_sum,
  "Foreign ownership",
  "Foreign own.",
  y_limit = c(-1.0, 0.1),
  leg_pos = "none",
  y_label = "Foreign assets to foreign liabilities"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 100),
    expand = c(0, 0)
  )
for_own

gdp_pc <- make_single_plot(macro_data_agg_catchup_sum,
  "GDP pc (PPP)", 
  "GDP per capita",
  leg_pos = "none",
  y_label = "GDP per capita (thousand PPP, constant prices)"
) + scale_y_continuous(labels = scales::number_format(scale=0.001),
                       expand = c(0, 0))
gdp_pc

pdf("output/taxonomy/fig_4d_taxonomy-catchup.pdf", width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(ggarrange(for_own, gdp_pc, ind_emp, wage_share,
  ncol = 4,
  common.legend = TRUE,
  legend = "bottom"
),
vp = vplayout(1, 1)
)
dev.off()


# Catchup weighted - data------------------------------------------------------

macro_data_agg_catchup_mean_w <- macro_data_agg_catchup_weighted %>%
  group_by(c_group) %>%
  summarise(
    `Wage share` = mean(Adjusted_wage_share, na.rm=TRUE),
    `GDP pc (PPP)` = mean(GDP_pc_PPP, na.rm=TRUE),
    `Foreign ownership` = mean(foreign_ownership, na.rm=TRUE),
    `Industial empl.` = mean(employm_indus, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_catchup_mean_w)

macro_data_agg_catchup_sd_w <- macro_data_agg_catchup %>%
  group_by(c_group) %>%
  summarise(
    `Wage share` = sd(Adjusted_wage_share, na.rm=TRUE),
    `GDP pc (PPP)` = sd(GDP_pc_PPP, na.rm=TRUE),
    `Foreign ownership` = sd(foreign_ownership, na.rm=TRUE),
    `Industial empl.` = sd(employm_indus, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_catchup_sd_w)

macro_data_agg_catchup_sum_w <- left_join(macro_data_agg_catchup_mean_w, 
                                          macro_data_agg_catchup_sd_w, 
                                          by=c("c_group", "variable"))


# Catchup weighted - plots-----------------------------------------------------

wage_share_w <- make_single_plot(macro_data_agg_catchup_sum_w,
  "Wage share",
  y_limit = c(40, 65),
  leg_pos = "none",
  y_label = "Adj. wage share in % of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
wage_share_w

ind_emp_w <- make_single_plot(macro_data_agg_catchup_sum_w,
  "Industial empl.",
  leg_pos = "none",
  y_label = "Percentage of total employment"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
ind_emp_w

for_own_w <- make_single_plot(macro_data_agg_catchup_sum_w,
                              "Foreign ownership",
                              "Foreign own.",
                              y_limit = c(-1.0, 0.35),
                              leg_pos = "none",
                              y_label = "Foreign assets to foreign liabilities"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 100),
    breaks = seq(-1, 0.4, by=0.25),
    expand = c(0, 0)
  )
for_own_w

gdp_pc_w <- make_single_plot(macro_data_agg_catchup_sum_w,
                             "GDP pc (PPP)", 
                             "GDP per capita",
                             leg_pos = "none",
                             y_label = "GDP per capita (thousand PPP, constant prices)"
) + scale_y_continuous(labels = scales::number_format(scale=0.001),
                       expand = c(0, 0))
gdp_pc_w

pdf("output/taxonomy/fig_A8d_taxonomy-catchup-weighted.pdf", width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(ggarrange(for_own_w, gdp_pc_w, ind_emp_w, wage_share_w,
  ncol = 4,
  common.legend = TRUE,
  legend = "bottom"
),
vp = vplayout(1, 1)
)
dev.off()

# Finance======================================================================

macro_data_agg_finance <- macro_data %>%
  mutate(c_group = ifelse(c_group == "finance", "finance", "other")) %>%
  group_by(c_group, Year) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  ungroup()

macro_data_agg_finance_weighted <- macro_data %>%
  mutate(c_group = ifelse(c_group == "finance", "finance", "other")) %>%
  group_by(c_group, Year) %>%
  summarise(
    fdi_in_gdp = weighted.mean(fdi_in_gdp, population, na.rm=TRUE),
    Private_sector_debt_to_GDP = weighted.mean(Private_sector_debt_to_GDP, 
                                               population, na.rm=TRUE),
    Tax_Wealth = weighted.mean(Tax_Wealth, population, na.rm=TRUE),
    finance_share_GO = weighted.mean(finance_share_GO, population, na.rm=TRUE)
  ) %>%
  ungroup()

# Finance unweighted - data----------------------------------------------------

macro_data_agg_finance_mean <- macro_data_agg_finance %>%
  group_by(c_group) %>%
  summarise(
    `Private debt` = mean(Private_sector_debt_to_GDP, na.rm=TRUE),
    FDI = mean(fdi_in_gdp, na.rm=TRUE),
    `Share finance (GO)` = mean(finance_share_GO, na.rm=TRUE),
    `Wealth tax` = mean(Tax_Wealth, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_finance_mean)

macro_data_agg_finance_sd <- macro_data_agg_finance  %>%
  group_by(c_group) %>%
  summarise(
    `Private debt` = sd(Private_sector_debt_to_GDP, na.rm=TRUE),
    FDI = sd(fdi_in_gdp, na.rm=TRUE),
    `Share finance (GO)` = sd(finance_share_GO, na.rm=TRUE),
    `Wealth tax` = sd(Tax_Wealth, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_finance_sd)

macro_data_agg_finance_sum <- left_join(macro_data_agg_finance_mean, 
                                        macro_data_agg_finance_sd, 
                                        by=c("c_group", "variable"))


# Finance unweighted - plots---------------------------------------------------

fdi <- make_single_plot(macro_data_agg_finance_sum,
  "FDI",
  leg_pos = "none",
  y_label = "Percentage of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
fdi

priv_debt <- make_single_plot(macro_data_agg_finance_sum,
  "Private debt",
  leg_pos = "none",
  y_label = "Private sector debt to GDP"
)  + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
priv_debt

finance_go <- make_single_plot(macro_data_agg_finance_sum,
  "Share finance (GO)",
  leg_pos = "none",
  y_label = "Share in gross output of all sectors"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 100),
    expand = c(0, 0)
  ) + ggtitle("Share finance")
finance_go

wealth_tax <- make_single_plot(macro_data_agg_finance_sum,
  "Wealth tax",
  leg_pos = "none",
  y_label = "Tax revenue as % of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1, scale = 1),
    expand = c(0, 0)
  )
wealth_tax

pdf("output/taxonomy/fig_4c_taxonomy-finance.pdf", width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(ggarrange(fdi, finance_go, priv_debt, wealth_tax,
  ncol = 4,
  common.legend = TRUE,
  legend = "bottom"
),
vp = vplayout(1, 1)
)
dev.off()

# Finance weighted - data------------------------------------------------------

macro_data_agg_finance_mean_w <- macro_data_agg_finance_weighted %>%
  group_by(c_group) %>%
  summarise(
    `Private debt` = mean(Private_sector_debt_to_GDP, na.rm=TRUE),
    FDI = mean(fdi_in_gdp, na.rm=TRUE),
    `Share finance (GO)` = mean(finance_share_GO, na.rm=TRUE),
    `Wealth tax` = mean(Tax_Wealth, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, mean_value, -c_group)
head(macro_data_agg_finance_mean_w)

macro_data_agg_finance_sd_w <- macro_data_agg_finance_weighted %>%
  group_by(c_group) %>%
  summarise(
    `Private debt` = sd(Private_sector_debt_to_GDP, na.rm=TRUE),
    FDI = sd(fdi_in_gdp, na.rm=TRUE),
    `Share finance (GO)` = sd(finance_share_GO, na.rm=TRUE),
    `Wealth tax` = sd(Tax_Wealth, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  gather(variable, sd_value, -c_group)
head(macro_data_agg_finance_sd_w)

macro_data_agg_finance_sum_w <- left_join(macro_data_agg_finance_mean_w, 
                                          macro_data_agg_finance_sd_w, 
                                          by=c("c_group", "variable"))

# Finance weighted - plots-----------------------------------------------------

fdi_w <- make_single_plot(macro_data_agg_finance_sum_w,
  "FDI",
  leg_pos = "none",
  y_label = "Percentage of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
fdi_w

priv_debt_w <- make_single_plot(macro_data_agg_finance_sum_w,
  "Private debt",
  leg_pos = "none",
  y_label = "Percentage of total employment"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0)
  )
priv_debt_w

finance_go_w <- make_single_plot(macro_data_agg_finance_sum_w,
  "Share finance (GO)",
  leg_pos = "none",
  y_label = "Share in gross output of all sectors"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 100),
    expand = c(0, 0)
  )
finance_go_w

wealth_tax_w <- make_single_plot(macro_data_agg_finance_sum_w,
  "Wealth tax",
  leg_pos = "none",
  y_label = "Tax revenue as % of GDP"
) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.01, scale = 1),
    expand = c(0, 0)
  )
wealth_tax_w

pdf("output/taxonomy/fig_A8c_taxonomy-finance-weighted.pdf", width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(ggarrange(fdi_w, finance_go_w, priv_debt_w, wealth_tax_w,
  ncol = 4,
  common.legend = TRUE,
  legend = "bottom"
),
vp = vplayout(1, 1)
)
dev.off()
