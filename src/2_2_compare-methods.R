# For all k:
if (length(exclude_countries)>0) {
  file_name <- paste0(
    "output/fe-estimates/tab_A5a_cluster-coefs_emu_allk-",
    paste0(excl_countries, collapse = "-"),
    ".tex"
  )
} else {
  file_name <- "output/fe-estimates/tab_A5a_cluster-coefs_emu_allk.tex"
}
write(print(xtable(compare_clustering_types(fe_total, k = "all")),
            booktabs = TRUE, floating = FALSE, comment = FALSE
),
file = file_name
)

# For first k only:
if (length(exclude_countries)>0) {
  file_name <- paste0(
    "output/fe-estimates/tab_A5b_cluster-coefs_emu_firstk-",
    paste0(excl_countries, collapse = "-"),
    ".tex"
  )
} else {
  file_name <- "output/fe-estimates/tab_A5b_cluster-coefs_emu_firstk.tex"
}
write(print(xtable(compare_clustering_types(fe_total, k = "first")),
            booktabs = TRUE, floating = FALSE, comment = FALSE
),
file = file_name
)
