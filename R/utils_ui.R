#' sipher_palettes
#'
#' @description get colours from SIPHER palettes
#'
#' @param n number of colours to return
#' @param name name of palette to use
#' @param type continuous or discrete
#'
#' @return colours as hex values
#'
#' @noRd
sipher_palettes <- function(n, palette_name = "full",  type = c("discrete", "continuous")) {

  all_palettes <- list(
    trio = c(
      "#005398", # Sky Blue
      "#52473B", # Brown
      "#005C8A" # Cobalt
    ),
    cb = c(
      "#E69F00", # Orange
      "#56B4E9", # Light Blue
      "#009E73", # Green
      "#F0E442" # Yellow
    ),
    diverging = c(
      "#A53C0B", # Dark red
      "#FFFFFF", # White
      "#005C8A" # Cobalt
    ),
    full = c(
      "#005398", # Sky Blue
      "#E69F00", # Orange
      "#56B4E9", # Light Blue
      "#009E73", # Green
      "#F0E442", # Yellow
      "#52473B", # Brown
      "#005C8A", # Cobalt
      "#A53C0B", # Dark red
      "#C19664" # Light brown
    )
  )


  palette = all_palettes[[palette_name]]

  if (missing(n)) {
    n = length(palette)
  }

  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = palette_name, class = "palette")
}


#' scale_colour_sipher, scale_fill_sipher
#'
#' @description pass colour palette to ggplot2
#'
#' @return modified scale_colour_manual function for use in ggplot.
#'
#' @noRd
scale_colour_sipher <- function(palette_name = "full", type= c("discrete", "continuous")) {
  if(type == "discrete") {
    ggplot2::scale_colour_manual(values = sipher_palettes(palette_name = palette_name,
                                                        type = "discrete"))
  } else {
    ggplot2::scale_colour_gradientn(colours = sipher_palettes(palette_name = palette_name,
                                                            type = "continuous"))
  }
}

scale_fill_sipher <- function(palette_name = "full", type= c("discrete", "continuous")) {
if(type == "discrete") {
    ggplot2::scale_fill_manual(values = sipher_palettes(palette_name = palette_name,
                                                          type = "discrete"))
  } else {
   ggplot2::scale_fill_gradientn(colours = sipher_palettes(palette_name = palette_name,
                                                         type = "continuous"))
 }
}
