rm(list = ls())
library("devtools")
library("knitr")

get_session_info <- devtools::session_info()

writeLines(text = {
  paste(sep = "\n", collapse = "",
        paste0(rep("-", 80), collapse = ""),
        paste(paste0(rep("-", 32), collapse = ""),
              "R environment",
              paste0(rep("-", 33), collapse = "")),
        paste0(rep("-", 80), collapse = ""),
        paste(knitr::kable(data.frame(setting = as.character(get_session_info$platform)[2:10])), collapse = "\n"),
        paste0(rep("-", 80), collapse = ""),
        paste(paste0(rep("-", 35), collapse = ""),
              "packages",
              paste0(rep("-", 35), collapse = "")),
        paste0(rep("-", 80), collapse = ""),
        paste(get_session_info$packages, collapse = "\n")
  )
}, con = "output/session_info.txt")
