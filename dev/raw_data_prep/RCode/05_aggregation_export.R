### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2024-04-26
# About: this file prepares lists for automated run of function. It then runs 
#        the function and saves outputs.

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] run overarching area-level loop ### 

for (y in 1: length(definitions$area_levels)) {
  # Print Progress
  print(paste("area level:", definitions$area_levels[y],
              ", area",y, "out of", length(definitions$area_levels)))
  
# initialise list
area_list <- vector(mode = "list", length = dim(indicator_scale)[1])

# run function over loop
for (x in 1:length(area_list)) {
  # Print Progress
  print(paste("indicator:", indicator_scale[x,1],
              ", indicator", x, "out of", dim(indicator_scale)[1]))
  # Feed in Function
  area_list[[x]] <- .AggIndicator(data_in = dt_sp,
                               area_level = definitions$area_levels[y],
                               var_name = indicator_scale$indicator[x],
                               categorical = indicator_scale$categorical[x])
  gc(full = TRUE)  # deep clean RAM
}

# rbindlist
area_list <- data.table::rbindlist(area_list, fill = FALSE, idcol = NULL)

# first variable is called area 
data.table::setnames(area_list, old = names(area_list)[1], new = "area")

# ---------------#

# export
data.table::fwrite(area_list, paste0("ROutput/",
                                     definitions$area_levels[y],
                                     "_export_raw.csv"))
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
