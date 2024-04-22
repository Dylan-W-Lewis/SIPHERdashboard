#' get_cats
#'
#' @description get category levels (in correct order) from variable name
#'
#' @param var variable name
#' @param ref_only logical. Should all categories be returned or only the reference category?
#'
#' @noRd
#'
#' @examples
#' get_cats("racel_dv")
get_cats <- function(var, ref_only=F) {
  if(length(var)>1) {
    stop("get_cats is not vectorised - try map(x, get_cats)")
  }
  if(!var %in% reference$obs) {
    stop(paste0("variable '", var, "' not found"))
  }
  else {
    if(ref_only==F){
      cats <- dplyr::pull(reference$cats[reference$obs==var][[1]])
    }
    if(ref_only==T){
      cats <- reference$cat_reference[reference$obs==var]
    }
    cats[is.na(cats)] <- var
    return(cats)
  }
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


#' make_var_labels
#'
#' @description Make axis labels combining variable name and (if categorical) reference category
#'
#' @param vars character vector of codes
#'
#' @noRd
make_var_labels <- function(vars){
  varNames <- purrr::map_chr(vars,
                      function(.x){
                        if(reference$categorical[reference$obs==.x]){
                          stringr::str_c(codebook$name[codebook$code==.x],
                                         codebook$name[codebook$code==get_cats(.x, ref_only = T)],
                                         sep= ": ")
                        } else {
                          codebook$name[codebook$code==.x]
                        }
                      })
  return(varNames)
}
