rm(list = setdiff(ls(), c("exclude_countries", "excl_countries")))

source("src/2_0_fe-clustering-functions.R")

if (!exists("exclude_countries")){
  exclude_countries <- c()
}

fe_k_plot_height <- 8
fe_k_plot_width <- 12

if (length(exclude_countries)>0) {
  data_new <- read_feather(paste0("output/fe-estimates/fe_estimates_excluded", 
                                  paste0("-", exclude_countries, collapse = ""), 
                                  ".feather"))
} else {
  data_new <- read_feather("output/fe-estimates/fe_estimates.feather")
}

# 1. Data preparation==========================================================

# dictionary with variable names
var_name_dict <- list()
var_name_dict[["Capital_accumulation"]] <- "capital_acc"
var_name_dict[["Unemployment_rate"]] <- "unemp"
var_name_dict[["GDP_growth"]] <- "gdp_growth"
var_name_dict[["Current_account_balance_to_GDP"]] <- "current_accout"
var_name_dict[["Public_debt_to_GDP"]] <- "debt_public"
var_name_dict[["Gini_net"]] <- "gini_net"
var_name_dict[["size_of_finance"]] <- "go_finance"
var_name_dict[["exp_to_gdp"]] <- "exp_to_gdp"
var_name_dict[["Adjusted_wage_share"]] <- "wage_share"
var_name_dict[["GDP_pc_ppp_new"]] <- "gdp_pcn"
var_name_dict[["finance_share_VA"]] <- "va_finance"
var_name_dict[["GDP_pc_PPP"]] <- "gdp_pc"
var_name_dict[["indus_output_new"]] <- "ind_output"
var_name_dict[["Total_debt_to_GDP"]] <- "debt_total"
var_name_dict <- data.frame(feather_name = names(unlist(var_name_dict)), 
                            cluster_name = unlist(var_name_dict), row.names = NULL)
var_name_dict

fe_total <- data_new %>%
  rename(country=csu) %>%
  mutate(var = countrycode::countrycode(var, "feather_name", "cluster_name", 
                                        custom_dict = var_name_dict)) %>%
  gather(k_val, fe_val, -country, -var) %>%
  mutate(k_val = gsub("\\.", "_", k_val))

length(unique(fe_total$var))
fe_total <- as.data.table(fe_total)


# 2. Plot dynamics of FE estimates=============================================
source("src/2_1-visualization-FE-estimates.R")

# 3. Comparing clustering methods==============================================
source("src/2_2_compare-methods.R")

# 4. Clustering of EMU estimates===============================================

# 4.1. Variations of classical variable selection =============================
var_selects <- list()
var_selects[["classic1"]] <- c("wage_share", "unemp", "gdp_growth", "gdp_pcn", 
                               "current_accout", "debt_public", "va_finance", 
                               "exp_to_gdp")
var_selects[["noCA_noGDPpc"]] <- c("wage_share", "unemp", "gdp_growth", 
                                   "debt_public", "va_finance", "exp_to_gdp")
var_selects[["noCA"]] <- c("wage_share", "unemp", "gdp_pcn","gdp_growth", 
                           "debt_public", "va_finance", "exp_to_gdp")

subset_clusters <- list()
subset_clusters[["all"]] <- list()
subset_clusters[["last4"]] <- list()
subset_clusters[["last2"]] <- list()
subset_clusters[["last"]] <- list()

nb_groups <- 5
for (i in 1:length(names(var_selects))){
  print(paste0("Clustering: ", i, "/", length(names(var_selects))))
  vars_considered <- var_selects[[names(var_selects)[i]]]
  
  dendo_stuff_all <- make_dendo(fe_total,
                                k_considered = "all",
                                kof_case = FALSE,
                                n_groups = nb_groups,
                                restrict_variables=vars_considered
  )
  
  
  dendo_stuff_last4 <- make_dendo(fe_total,
                                  k_considered = 4:8,
                                  kof_case = FALSE,
                                  n_groups = nb_groups,
                                  restrict_variables=vars_considered
  )
  
  dendo_stuff_last2 <- make_dendo(fe_total,
                                  k_considered = 6:8,
                                  kof_case = FALSE,
                                  n_groups = nb_groups,
                                  restrict_variables=vars_considered
  )
  
  dendo_stuff_last <- make_dendo(fe_total,
                                 k_considered = "last",
                                 kof_case = FALSE,
                                 n_groups = nb_groups,
                                 restrict_variables=vars_considered
  )
  
  subset_clusters[["all"]][[names(var_selects)[i]]] <- dendo_stuff_all$plot_obj + 
    ggtitle(paste0("all: ", dendo_stuff_all$plot_obj$labels$title))
  subset_clusters[["last4"]][[names(var_selects)[i]]] <- dendo_stuff_last4$plot_obj + 
    ggtitle(paste0("last4: ", dendo_stuff_last4$plot_obj$labels$title))
  subset_clusters[["last2"]][[names(var_selects)[i]]] <- dendo_stuff_last2$plot_obj + 
    ggtitle(paste0("last2: ", dendo_stuff_last2$plot_obj$labels$title))
  subset_clusters[["last"]][[names(var_selects)[i]]] <- dendo_stuff_last$plot_obj + 
    ggtitle(paste0("last: ", dendo_stuff_last$plot_obj$labels$title))
}

classic_plot <- ggpubr::ggarrange(plotlist = subset_clusters[["all"]], 
                                  ncol=3, nrow=1)
ggsave("output/fe-estimates/selection-search/clustering_options_classics.pdf",
       plot = classic_plot, height = 10, width = 26, limitsize = F)

# 4.2. Creating figure 3: clustering of all k==================================
variable_selection_clustering <-  c("wage_share", "unemp", 
                                    "gdp_growth", "gdp_pcn", 
                                    "current_accout", "debt_public", 
                                    "va_finance", "exp_to_gdp")
nb_groups <- 5

fig_3_main <- make_dendo(fe_total,
                         k_considered = "all",
                         kof_case = FALSE,
                         n_groups = nb_groups,
                         restrict_variables=variable_selection_clustering
)

plot_file_name <- "output/fe-estimates/fig_3_clustering.pdf"
w <- 9 * 1.618
h <- 7
pdf(plot_file_name, width = w, height = h) # golden ratio would be 1.618
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, 
                                           widths = c(2*(w/3), w/3), 
                                           default.units = "in")))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(fig_3_main$plot_obj + ggtitle("Hierarchical clustering of FE estimates"), 
      vp = vplayout(1, 1))
print(fig_3_main$factor_plot + ggtitle("Factor map for FE estimates"), 
      vp = vplayout(1, 2))
dev.off()


# 4.3.Robustness checks for figure 3: subset of k==============================

last4k_clustering <- make_dendo(fe_total,
                                 k_considered = 5:8,
                                 kof_case = FALSE,
                                 n_groups = 6,
                                 restrict_variables=variable_selection_clustering
)

plot_file_name <- "output/fe-estimates/fig_A5_clustering_last4k.pdf"
w <- 9 * 1.618
h <- 7
pdf(plot_file_name, width = w, height = h) # golden ratio would be 1.618
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, 
                                           widths = c(2*(w/3), w/3), 
                                           default.units = "in")))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(last4k_clustering$plot_obj + 
        ggtitle("Hierarchical clustering of FE estimates (only second half of k considered)"), 
      vp = vplayout(1, 1))
print(last4k_clustering$factor_plot + ggtitle("Factor map for FE estimates"), 
      vp = vplayout(1, 2))
dev.off()


# 4.4. Robustness checks for figure 3: country subset==========================

fe_no_finance <- read_feather("output/fe-estimates/fe_estimates_no_finance.feather") %>%
  rename(country=csu) %>%
  mutate(var = countrycode::countrycode(var, "feather_name", "cluster_name", 
                                        custom_dict = var_name_dict)) %>%
  gather(k_val, fe_val, -country, -var) %>%
  mutate(k_val = gsub("\\.", "_", k_val))

fe_no_finance <- as.data.table(fe_no_finance)

nb_groups <- 4
fig_3_no_finance <- make_dendo(fe_no_finance,
                               k_considered = "all",
                               kof_case = FALSE,
                               n_groups = nb_groups,
                               restrict_variables=variable_selection_clustering
)

plot_file_name <- "output/fe-estimates/fig_A6_fig3_no_finance.pdf"
w <- 9 * 1.618
h <- 7
pdf(plot_file_name, width = w, height = h) # golden ratio would be 1.618
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, 
                                           widths = c(2*(w/3), w/3), 
                                           default.units = "in")))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(fig_3_no_finance$plot_obj + 
        ggtitle("Hierarchical clustering of FE estimates (with Cyprus, Luxembourg and Malta being excluded)"), 
      vp = vplayout(1, 1))
print(fig_3_no_finance$factor_plot + ggtitle("Factor map for FE estimates"), 
      vp = vplayout(1, 2))
dev.off()

# 4.5. Robustness check for different shock var (using EU entry for all)=======

fe_eu_entry <- read_feather("output/fe-estimates/fe_estimates_eu_entry.feather") %>%
  rename(country=csu) %>%
  mutate(var = countrycode::countrycode(var, "feather_name", "cluster_name", 
                                        custom_dict = var_name_dict)) %>%
  gather(k_val, fe_val, -country, -var) %>%
  mutate(k_val = gsub("\\.", "_", k_val))

fe_eu_entry <- as.data.table(fe_eu_entry)

nb_groups <- 5
fig_3_no_finance <- make_dendo(fe_eu_entry,
                               k_considered = "all",
                               kof_case = FALSE,
                               n_groups = nb_groups,
                               restrict_variables=variable_selection_clustering
)

plot_file_name <- "output/fe-estimates/fig_A1_fig3_eu_entry_shock.pdf"
w <- 9 * 1.618
h <- 7
pdf(plot_file_name, width = w, height = h) # golden ratio would be 1.618
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, 
                                           widths = c(2*(w/3), w/3), 
                                           default.units = "in")))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(fig_3_no_finance$plot_obj + 
        ggtitle("Hierarchical clustering of FE estimates (shock dummy set according to EU entry of all countries)"), 
      vp = vplayout(1, 1))
print(fig_3_no_finance$factor_plot + ggtitle("Factor map for FE estimates"), 
      vp = vplayout(1, 2))
dev.off()

