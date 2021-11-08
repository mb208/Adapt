library(tidyverse)

format_data_dl <- function(data, pid, timeid, outcome, covariates) {
  reshape2::dcast(
    reshape2::melt(data,
      id.vars = c(pid, timeid),
      measure.vars = c(outcome, covariates)),
    stringr::str_interp("${pid} ~ variable + ${timeid}")
    )
}

get_dl_outcome <- function(data, outcome) {
  regx <- paste(outcome, "_\\d+$", sep = "")
  data[ ,grep(regx, colnames(data))]
}

get_dl_vars <- function(data, vars) {
  
  if (length(vars) > 1) {
    regx <- paste0("(",
                   paste(vars, collapse = "|"),
                   ")",
                   "_\\d+$",
                   collapse = "")
    
  } else {
    regx <- paste(vars, "_\\d+$", sep = "")
    
  }
  
  return(data[ ,grep(regx, colnames(data))])
}
