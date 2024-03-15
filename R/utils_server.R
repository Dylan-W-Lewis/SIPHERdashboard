#' Get category levels (in correct order) from variable name
#'
#' @param var variable name
#'
#' @noRd
#'
#' @examples
#' get_cats("cares for sick/disabled/elderly in household")
get_cats <- function(var) {
  if(!var %in% codebook$obs) {
    warning(paste0("variable '", var, "' not found in codebook"))
    cats <- NULL
  }
  else {
    cats <- dplyr::pull(codebook$cat[codebook$obs==var][[1]])
  }
  return(cats)
}

var_names <- function(vars){
  quoted <- paste0("'", vars, "'")
  commas <- paste0(quoted[1:length(quoted)-1], collapse = ", ")
  full <- paste0(commas, ", and ", quoted[length(quoted)])
  return(full)
}
