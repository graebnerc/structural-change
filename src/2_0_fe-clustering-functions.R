# TODO: remove k_cons and replace it by more general k_considered
make_dendo <- function(init_data,
                       # first_k=FALSE,
                       # k_cons = "all", # either 'all', 'first' or 'last' 
                       k_considered,
                       kof_case = FALSE,
                       n_groups = 4,
                       restrict_variables=F) {
  # make_dendo: takes total data of fixed effect estimates and clusters countries;
  # returns both output name and dendo plot in a list
  # stopifnot(k_cons %in% c("all", "first", "last"))
  init_data <- dplyr::mutate(init_data, 
                             country = countrycode(country, "iso3c", "country.name"))
  
  # Select the adequate variables =============================================
  if (!TRUE %in% (restrict_variables==FALSE)){
    init_data <- init_data %>%
      dplyr::filter(var %in% restrict_variables)
  }
  
  # Select the adequate projection horizon to consider ========================
  if (is.character(k_considered)){
    if (!k_considered %in% c("first", "last", "all")){
      stop("Admissible keywords for k values are: 'first', 'last', and 'all'. 
           Otherwise, only integers allowed to specify which k to consider.")
    } else{
      if (k_considered=="first"){
        k_used <- "k_1"
      } else if (k_considered=="last"){
        k_used <- paste0("k_", length(unique(init_data$k_val)))
      } else {
        stopifnot(k_considered=="all")
        k_used <- paste0("k_", 1:length(unique(init_data$k_val)))
      } 
    }
  } else if (is.integer(k_considered)){
    k_used <- paste0("k_", k_considered)
  } else if (is.numeric(k_considered)){
    warning("The k horizon was given as double, not integer; 
            transformed into integer to proceed!")
    k_used <- paste0("k_", as.integer(k_considered))
  } else {
    stop("The k horizon to be considered must be provided as vector of 
         integers or a keyword mentioned in function help!")
  }

    work_data_v1 <- init_data %>%
    dplyr::filter(k_val %in% k_used) %>%
    tidyr::unite("var_k", c("var", "k_val"), sep = "_") %>%
    tidyr::spread(var_k, fe_val)#  %>%     dplyr::select(-k_val)
  work_data_v1 <- as.data.frame(work_data_v1)
  rownames(work_data_v1) <- work_data_v1$country
  
  #   work_data_v1 <- init_data %>%
  #     unite("var_k", c("var", "k_val"), sep = "_") %>%
  #     spread(var_k, fe_val)
  #   work_data_v1 <- as.data.frame(work_data_v1)
  #   rownames(work_data_v1) <- work_data_v1$country
  #   if (kof_case == TRUE) {
  #     output_name <- "kof_all_k_cluster.pdf"
  #     dendo_title <- "Hierachical clustering of kof estimates (all k)"
  #     factor_title <- "Factor map for kof estimates (all k)"
  #   } else {
  #     output_name <- "emu_all_k_cluster.pdf"
  #     dendo_title <- "Hierachical clustering of EMU estimates (all k)"
  #     factor_title <- "Factor map for EMU estimates (all k)"
  #   }
  # }
  
  # Prepare output nomenclature depending on EMU or KOF case ==================
  if (!kof_case %in% c(TRUE, FALSE)){
    stop(paste0("kof_case must be either TRUE or FALSE, but is: ", kof_case))
  }
  if (kof_case==T){
    case_var <- "kof"
  } else {
    case_var <- "emu"
  } 
    
  output_name <- paste0(case_var, "_k", min(k_considered), "-", 
                        max(k_considered), "_cluster.pdf"
                        )
  title_addendum <- paste0("(", case_var, " with k: ", min(k_considered), "-", 
                           max(k_considered),")"
                           )
  dendo_title <- paste0("Hierachical clustering of FE estimates ", 
                        title_addendum
                        )
  factor_title <- paste0("Factor map for FE estimates ", 
                         title_addendum
                         )
  
  if (!TRUE %in% (restrict_variables==FALSE)){
    output_name <- paste0("only_", paste0(restrict_variables, collapse = "-"), 
                          "_" , output_name)
    dendo_title <- paste0(restrict_variables, collapse = "+")
  } 


  
  # if (k_cons == "first") {
  #   work_data_v1 <- init_data %>%
  #     filter(k_val == "k_1") %>%
  #     spread(var, fe_val) %>%
  #     select(-k_val)
  #   rownames(work_data_v1) <- work_data_v1$country
  #   if (kof_case == TRUE) {
  #     output_name <- "kof_first_k_cluster.pdf"
  #     dendo_title <- "Hierachical clustering of kof estimates (first k)"
  #     factor_title <- "Factor map for kof estimates (first k)"
  #   } else {
  #     output_name <- "emu_first_k_cluster.pdf"
  #     dendo_title <- "Hierachical clustering of EMU estimates (first k)"
  #     factor_title <- "Factor map for EMU estimates (first k)"
  #   }
  # } else if (k_cons == "last") {
  #   work_data_v1 <- init_data %>%
  #     filter(k_val == "k_8") %>%
  #     spread(var, fe_val) %>%
  #     select(-k_val)
  #   rownames(work_data_v1) <- work_data_v1$country
  #   if (kof_case == TRUE) {
  #     output_name <- "kof_last_k_cluster.pdf"
  #     dendo_title <- "Hierachical clustering of kof estimates (last k)"
  #     factor_title <- "Factor map for kof estimates (last k)"
  #   } else {
  #     output_name <- "emu_last_k_cluster.pdf"
  #     dendo_title <- "Hierachical clustering of EMU estimates (last k)"
  #     factor_title <- "Factor map for EMU estimates (last k)"
  #   }
  # } else {
  #   work_data_v1 <- init_data %>%
  #     unite("var_k", c("var", "k_val"), sep = "_") %>%
  #     spread(var_k, fe_val)
  #   work_data_v1 <- as.data.frame(work_data_v1)
  #   rownames(work_data_v1) <- work_data_v1$country
  #   if (kof_case == TRUE) {
  #     output_name <- "kof_all_k_cluster.pdf"
  #     dendo_title <- "Hierachical clustering of kof estimates (all k)"
  #     factor_title <- "Factor map for kof estimates (all k)"
  #   } else {
  #     output_name <- "emu_all_k_cluster.pdf"
  #     dendo_title <- "Hierachical clustering of EMU estimates (all k)"
  #     factor_title <- "Factor map for EMU estimates (all k)"
  #   }
  # }

  
# Conduct the clustering and create the plot object =========================
  work_data_v1 <- select(work_data_v1, -country)
  clustering_object <- work_data_v1 %>%
    get_diff_matrix(raw_dat = FALSE) %>% # Scale the data
    agnes(method = "ward") # Compute hierachical clustering
  dendo_plot <- fviz_dend(clustering_object,
    main = dendo_title,
    xlab = "Countries", ylab = "",
    k = n_groups, # Cut in groups
    cex = 0.75, # label size
    rect = TRUE, # Add rectangle around groups
    rect_fill = TRUE,
    color_labels_by_k = TRUE, # color labels by groups
    k_colors = RColorBrewer::brewer.pal(n_groups, "Dark2"),
    rect_border = RColorBrewer::brewer.pal(n_groups, "Dark2"),
    horiz = TRUE
  )
  sub_grp <- cutree(as.hclust(clustering_object), k = n_groups)

# Create the factor map =======================================================  
  factor_map <- fviz_cluster(list(
    data = get_diff_matrix(work_data_v1, raw_dat = T),
    cluster = sub_grp
  ),
  repel = TRUE, # Avoid label overlapping
  show.clust.cent = TRUE, # Show cluster centers
  palette = "jco", # Color palette see ?ggpubr::ggpar
  ggtheme = theme_minimal(),
  main = factor_title
  )

# Create the final return object ==============================================
  list_to_return <- list(
    plot_obj = dendo_plot,
    plot_name = output_name,
    factor_plot = factor_map,
    clust_obj = clustering_object
  ) # ,two_d=dendo_2d_plot
  return(list_to_return)
}

get_diff_matrix <- function(init_data, raw_dat = FALSE, m = "euclidean") {
  # Takes data, omits missing values, scales data and returns distance matrix
  work_data_1 <- na.omit(init_data)
  work_data_2 <- scale(work_data_1)
  if (raw_dat == TRUE) {
    return(work_data_2)
  } else {
    diss_ma <- dist(work_data_2, method = m)
    return(diss_ma)
  }
}

compare_clustering_types <- function(raw_dat,
                                     k_num = "all") {
  # Compares the different aggregative/divisive coefficients
  # of the standard clustering algorithms
  if (k_num == "all") { # all k are considered
    int_dat <- raw_dat %>%
      unite("var_k", c("var", "k_val"), sep = "_") %>%
      spread(var_k, fe_val)
  } else if (k_num == "first") { # only first k is considered
    int_dat <- fe_total %>%
      filter(k_val == "k_1") %>%
      spread(var, fe_val) %>%
      select(-k_val)
  } else if (k_num == "last") { # only last k is considered
    int_dat <- fe_total %>%
      filter(k_val == "k_8") %>%
      spread(var, fe_val) %>%
      select(-k_val)
  } else {
    stop("Wrong value for 'k_num' given. Allowed: 'first', 'last', or 'all'!")
  }
  rownames(int_dat) <- int_dat$country
  int_dat <- select(int_dat, -country)
  int_dat <- get_diff_matrix(int_dat, raw_dat = TRUE)
  diss_matrix <- get_diff_matrix(int_dat, raw_dat = FALSE)

  hc_agnes_complete_linkage <- agnes(diss_matrix, method = "complete") # Hierarchical clustering using Complete Linkage
  hc_agnes_average_linkage <- agnes(diss_matrix, method = "average") # Hierarchical clustering using Average Linkage
  hc_agnes_single_linkage <- agnes(diss_matrix, method = "single") # Hierarchical clustering using single Linkage
  hc_agnes_ward <- agnes(diss_matrix, method = "ward") # Hierarchical clustering using Ward's method
  divisive_cluster <- diana(int_dat) # divisive hierarchical clustering
  cluster_type <- c("agnes_complete", "agnes_average", "agnes_single", "agnes_ward", "diana_divisive")
  fit_coefs <- c(hc_agnes_complete_linkage$ac, hc_agnes_average_linkage$ac, hc_agnes_single_linkage$ac, hc_agnes_ward$ac, divisive_cluster$dc)
  info_frame <- data.frame(type_clustering = cluster_type, dif_coef = fit_coefs) %>%
    arrange(desc(dif_coef)) %>%
    rename(Algorithm=type_clustering,
           `Clust. coef.`=dif_coef)
  return(info_frame)
}
