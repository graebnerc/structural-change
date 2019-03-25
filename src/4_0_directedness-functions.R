add_periods <- function(init_data, period="long", gap=FALSE){
  if (gap==FALSE){
    if (period=="long"){
      y_threshold_low <- 1979
      y_threshold_middle <- 2000
    } else if (period=="crisis"){
      y_threshold_low <- 1999
      y_threshold_middle <- 2008
    } else {
      stop("Wrong period given: long or crisis")
    }
    data_work <- data.table(init_data)
    data_work <- data_work[year > y_threshold_low]
    data_work <- data_work %>%
      mutate(period=ifelse(year<y_threshold_middle, "early", "late"))
  }
  else {
    if (period=="long"){
      y_threshold_low <- 1994# 1979
      y_threshold_middle <- 2000# 2000 - gap
      y_threshold_up <- 2010 #2000 + gap
    } else if (period=="crisis"){
      y_threshold_low <- 1999
      y_threshold_middle <- 2008 - gap
      y_threshold_up <- 2000 + gap
    } else {
      stop("Wrong period given: long or crisis")
    }
    data_work <- data.table(init_data)
    data_work <- data_work[year > y_threshold_low]
    data_work <- data_work %>%
      mutate(period=ifelse(year<y_threshold_middle, "early", 
                           ifelse(year>y_threshold_up, "late", "inter")))
  }
  
  return(data_work)
}

make_plot_data <- function(raw_dat, period_considered="long", gap_used=FALSE) {
  stopifnot(period_considered %in% c("long", "crisis"))
  
  work_data_p <- add_periods(raw_dat, period = period_considered, gap = gap_used) 
  work_data <- work_data_p %>%
    filter(period %in% c("early", "late")) %>%
    group_by(exporter, period, commoditycode) %>%
    summarise(av_pci=mean(pci, na.rm=TRUE),
              av_pci_w=weighted.mean(pci, exp_val, na.rm=TRUE),
              exp_val_total=sum(exp_val, na.rm=TRUE),
              exp_val_mean=mean(exp_val, na.rm=TRUE),
              exp_share_wt_mean=mean(exp_share_sect, na.rm=TRUE),
              exp_share_reg_mean=mean(exp_share_sect_reg, na.rm=TRUE)) %>%
    ungroup()
  # print(1)
  late_exp_share_reg <- work_data_p %>%
    filter(year>2012) %>%
    group_by(exporter, commoditycode) %>%
    summarise(late_exp_share_reg=mean(exp_share_sect_reg, na.rm=TRUE)) %>%
    ungroup()
  # print(2)
  fulltime_data <- work_data_p %>%
    group_by(exporter, commoditycode) %>%
    summarise(av_pci=mean(pci, na.rm=TRUE),
              av_pci_w=weighted.mean(pci, exp_val, na.rm=TRUE),
              av_exp_share_reg=mean(exp_share_sect_reg, na.rm=TRUE)) %>%
    ungroup() %>% 
    left_join(late_exp_share_reg, by=c("exporter", "commoditycode"))
  # print(3)
  
  diff_data_diffs <- work_data %>%
    select(exporter, period, commoditycode, exp_val_total, exp_share_wt_mean, exp_share_reg_mean) %>%
    gather(measure, value, -exporter, -period, -commoditycode) %>%
    spread(period, value) %>%
    mutate(diff_late_early=late-early) %>%
    select(-early, -late) %>%
    spread(measure, diff_late_early) %>%
    rename(diff_exp_share_reg=exp_share_reg_mean,
           diff_exp_share_wt=exp_share_wt_mean,
           diff_exp_val_total=exp_val_total)
  head(diff_data_diffs)
  
  # print(4)
  diff_data_ratios <- work_data %>%
    select(exporter, period, commoditycode, exp_val_total, exp_share_wt_mean, exp_share_reg_mean) %>%
    gather(measure, value, -exporter, -period, -commoditycode) %>%
    spread(period, value) %>%
    mutate(ratio_late_early=late/early) %>%
    select(-early, -late) %>%
    spread(measure, ratio_late_early) %>%
    rename(ratio_exp_share_reg=exp_share_reg_mean, # late/early
           ratio_exp_share_wt=exp_share_wt_mean,
           ratio_exp_val_total=exp_val_total)
  head(diff_data_ratios)
  
  # print(5)
  
  # diff_data heisst jetzt diff_data_diffs
  data_final <- left_join(fulltime_data, diff_data_diffs, by=c("exporter", "commoditycode")) %>%
    left_join(diff_data_ratios, by=c("exporter", "commoditycode"))
  
  var_label(data_final) <- list(exporter = "The exporting region according to Jakob's classification.", 
                                commoditycode = "SITC V2 commoditycode", 
                                av_pci = "Average complexity of the product over the full time considered", 
                                av_pci_w = "Weighted average complexity of the product over the full time considered. Weights are yearly total exports, so it differs among countries.", 
                                av_exp_share_reg = "Average export share of the product in it's regions' total exports.", 
                                late_exp_share_reg = "Average export share of the product in it's regions' total exports for the last 3 years of the sample.", 
                                diff_exp_share_reg = "Difference between average export share in region's total exports in early and late period.", 
                                diff_exp_share_wt = "Difference between average export share in world total exports in early and late period.", 
                                diff_exp_val_total = "Difference between the sums of export value in early and late period.",
                                ratio_exp_share_reg = "Ratio of average export share in region's total exports in early and late period (late/early).",
                                ratio_exp_share_wt = "Ratio of average export share in world total exports in early and late period (late/early).",
                                ratio_exp_val_total = "Ratio of the sums of export value in early and late period (late/early).")
  return(data_final)
}


make_wls_plot <- function(regression_data, # nimmt long or crisis data
                          country_name,
                          x_var, 
                          y_var, 
                          size_var, # aktuell auch weight var
                          change_type="positive",
                          interval_val="conf") {
  if (change_type=="positive"){
    reg_dat_used <- filter(regression_data[["pos"]], exporter == country_name)
  } else if (change_type=="negative"){
    reg_dat_used <- filter(regression_data[["neg"]], exporter == country_name)
  } else {
    stop("Wrong change_type given:positive or negative.")
  }
  reg_formula <- set_up_reg_formula(y_var, x_var)
  
  reg_model <- lm(reg_formula, 
                  data = reg_dat_used,
                  weights = late_exp_share_reg)
  
  predicted_df <- data.frame(predict(reg_model, 
                                     reg_dat_used, 
                                     interval = "conf"), 
                             complexity=reg_dat_used$av_pci_w) 
  
  exp_plot_re_coef <- ggplot(reg_dat_used,   
                             aes_string(x=x_var,
                                        y=y_var,
                                        color=size_var)) +
    geom_point(aes_string(size=size_var), alpha=0.5) +
    geom_line(color='#00394d', data = predicted_df, aes(x=complexity, y=fit), linetype="solid") +
    geom_line(color='#0086b3', data = predicted_df, aes(x=complexity, y=upr), linetype="dashed") +
    geom_line(color='#0086b3', data = predicted_df, aes(x=complexity, y=lwr), linetype="dashed") +
    ggtitle(paste0(country_name, ": WLS with recent export shares as weights (", change_type, " change)")) +
    scale_size(guide=FALSE) + 
    scale_color_viridis() + 
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_text(size=7),
          legend.key = element_rect(size = 10),
          legend.key.size = unit(2, "mm"),
          legend.key.width = unit(8, "mm"),
          plot.title = element_text(size=8),
          axis.title = element_text(size=6),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))
  
  scale_color_viridis()
  
  return(list(plot=exp_plot_re_coef, reg_model=reg_model))
}

set_up_reg_formula <- function(dependent_variable, independent_variables){
  regression_formula <- paste0(dependent_variable, " ~ ")
  for(i in 1:length(independent_variables)) {regression_formula <-paste0(regression_formula, independent_variables[i], " + ")}
  regression_formula <- as.formula(gsub('.{3}$', '', regression_formula))
  return(regression_formula)
}


make_regression_data <- function(dep_var, # The dependent variable, in its log formulation
                                 idep_vars, # Vector with the independent variables
                                 raw_dat){ # The raw data set
  # Returns a list with two data frames: one with the positive changes, one with the negative changes
  # In both cases, logs are taken from the changes, and the abs values of the negative changes
  if (!grepl("_log", dep_var)){
    # Assume that the dependent variable is given as later used in formula
    stop("Please provide dependent variable in log form (i.e. with '_log' at the end!")
  }
  
  dep_var_unlog <- gsub('.{4}$', '', dep_var) # Get the original name of the dep var (i.e. removing '_log')
  
  inter_dat <- raw_dat %>% # long_reg_data_v1
    select(one_of("exporter", "commoditycode", # Identifier
                  dep_var_unlog, # Dependent var
                  idep_vars)) # Independent vars
  
  return_list <- list()
  # Return two data frames: one with positive changes and one with negative changes
  
  return_list[["pos"]] <- inter_dat %>%
    filter(UQ(as.name(dep_var_unlog))>0) %>%
    mutate(UQ(as.name(dep_var)) := log(UQ(as.name(dep_var_unlog))))
  
  return_list[["neg"]] <- inter_dat %>%
    filter(UQ(as.name(dep_var_unlog))<0) %>%
    mutate(UQ(as.name(dep_var_unlog)) := abs(UQ(as.name(dep_var_unlog))) ) %>%
    mutate(UQ(as.name(dep_var)) := log(UQ(as.name(dep_var_unlog))))
  
  return(return_list)
}