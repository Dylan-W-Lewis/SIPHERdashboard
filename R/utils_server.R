#' get_cats
#'
#' @description get category levels (in correct order) from variable name
#'
#' @param var variable name
#'
#' @noRd
#'
#' @examples
#' get_cats("racel_dv")
get_cats <- function(var) {
  if(!var %in% reference$obs) {
    warning(paste0("variable '", var, "' not found"))
    cats <- NULL
  }
  else {
    cats <- dplyr::pull(reference$cats[reference$obs==var][[1]])
  }
  return(cats)
}

#' var_names
#'
#' @description combine variable names into a nice sentence
#'
#' @param vars vector of variable names
#'
#' @noRd
var_names <- function(vars){
  quoted <- paste0("'", vars, "'")
  commas <- paste0(quoted[1:length(quoted)-1], collapse = ", ")
  full <- paste0(commas, ", and ", quoted[length(quoted)])
  return(full)
}

#' translate_codes
#'
#' @description get human friendly names from variable/category codes
#'
#' @param codes character vector of codes
#'
#' @noRd
translate_codes <- function(codes){
  purrr::walk(codes, ~if(! .x %in% codebook$code){warning(paste(.x, "not found in codebook"))})
  out <- codebook$name[match(codes, codebook$code)]
  return(out)
}
