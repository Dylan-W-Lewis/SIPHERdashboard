##### Meta #####

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2024-06-04
# About: this file contains the core aggregation function 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] Aggregation Function: Documentation ###

# about: this two-branch function performs the aggregation from the linked SP 
#        data set. The function subsets to valid observations only. Based on
#        variable type (metric vs. categorical) it then runs one out of two 
#        branches. Within each branch, the function performs aggregations by 
#        age bands (as specified by constraints), sex (male/female) and provides 
#        overall group totals for every area level of interest. We run the 
#        function in a loop and feed in arguments via a separate data.table object.
#        This allows for all results to be generated and exported automated.
#
# input "data_in":    linked synthetic population containing variables of interest
# input "area_level": name of the area-level variable we want to aggregate over
# input "var_name":   name of the variables we want to aggregate over
# input "categorical": TRUE for categorical values, FALSE for metric
#
# output "data_out":  data.frame/table containing metric variable over sex, age, 
#                     and all respective totals

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [2] Aggregation Function: Categorical Variables ###

.AggIndicator <- function(data_in, area_level, var_name, categorical) {

## [BRANCH A]: Categorical ## 
  
 if (categorical == TRUE) {
  
    # level 3: area, by sex, by age 
    data_out1 <- data_in[!is.na(get(var_name)),
                .(get(area_level), sex, age, get(var_name))]
    data_out1 <- data.table::dcast(data_out1, V1 + sex + age ~ V4,
                                   value.var = "V4")
    data_out1[, row_sum := rowSums(.SD), .SDcols = 4:(dim(data_out1)[2])]
    for (i in 4:(dim(data_out1)[2]-1)){
      data_out1[,i] <- (data_out1[,paste0(colnames(data_out1)[i]), with = FALSE] / 
                          data_out1[, ncol(data_out1), with = FALSE])  * 100
    }
    
    # level 2: area, by sex, all age
    data_out2 <- data_in[!is.na(get(var_name)),
                .(get(area_level), sex, age = "all_ages", get(var_name))]
    data_out2 <- data.table::dcast(data_out2, V1 + sex + age ~ V4,
                                   value.var = "V4")
    data_out2[, row_sum := rowSums(.SD), .SDcols = 4:(dim(data_out2)[2])]
    for (i in 4:(dim(data_out2)[2]-1)){
      data_out2[,i] <- (data_out2[,paste0(colnames(data_out2)[i]), with = FALSE] / 
                          data_out2[, ncol(data_out2), with = FALSE])  * 100
    }
    
    # level 1: area, all sex, by age
    data_out3 <- data_in[!is.na(get(var_name)), 
                .(get(area_level), sex = "both", age, get(var_name))]
    data_out3 <- data.table::dcast(data_out3, V1 + sex + age ~ V4,
                                   value.var = "V4")
    data_out3[, row_sum := rowSums(.SD), .SDcols = 4:(dim(data_out3)[2])]
    for (i in 4:(dim(data_out3)[2]-1)){
      data_out3[,i] <- (data_out3[,paste0(colnames(data_out3)[i]), with = FALSE] / 
                          data_out3[, ncol(data_out3), with = FALSE])  * 100
    }
    
    # level 0: area, all sex, all age
    data_out4 <- data_in[!is.na(get(var_name)), 
                .(get(area_level), sex = "both", age = "all_ages",
                  get(var_name))]
    data_out4 <- data.table::dcast(data_out4, V1 + sex + age ~ V4,
                                   value.var = "V4")
    data_out4[, row_sum := rowSums(.SD), .SDcols = 4:(dim(data_out4)[2])]
    for (i in 4:(dim(data_out4)[2]-1)){
      data_out4[,i] <- (data_out4[,paste0(colnames(data_out4)[i]), with = FALSE] / 
                          data_out4[, ncol(data_out4), with = FALSE])  * 100
    }
    
    # bind and reduce
    data_out_all <- rbind(data_out1, data_out2, data_out3, data_out4)
    data_out_all <- data_out_all[,1:(dim(data_out_all)[2]-1)]
    
    # melt
    data_out_all <- melt(data_out_all,
                measure.vars = colnames(data_out_all)[4:(dim(data_out_all)[2])],
                variable.name = "cat", value.name = "value")
    
    # create observation variable
    data_out_all[, obs := var_name]
    
    # rename
    data.table::setnames(data_out_all, old = "V1", new = area_level)
    
    # column order
    data.table::setcolorder(data_out_all, c(colnames(data_out_all)[1:3],
                                            colnames(data_out_all)[6],
                                            colnames(data_out_all)[4:5]))

}
  
  # ------------------- #

## [BRANCH B]: Metric ## 

 else {
  
  # open output
  data_out_all <- rbind(
    # level 3: area, by sex, by age 
    data_in[get(var_name) >= 0, 
            .(value = mean(get(var_name))),
            by = c(area_level, "sex", "age")],
    # level 2: area, by sex, all age
    data_in[get(var_name) >= 0, 
            .(age = "all_ages", value = mean(get(var_name))),
            by = c(area_level, "sex")],
    # level 1: area, all sex, by age
    data_in[get(var_name) >= 0, 
            .(sex = "both", value = mean(get(var_name))),
            by = c(area_level, "age")],
    # level 0: area, all sex, all ages
    data_in[get(var_name) >= 0, 
            .(sex = "both", age = "all_ages", value = mean(get(var_name))),
            by = c(area_level)]
  )
  
  # create obs 
  data_out_all[, obs := var_name]
  
  # create cat 
  data_out_all[, cat := var_name]
  
  # column order
  data.table::setcolorder(data_out_all, c(colnames(data_out_all)[1:3],
                                          colnames(data_out_all)[5:6],
                                          colnames(data_out_all)[4]))
  
 }

  # ------------------- #
  
  ## EXPLICIT RETURN: one object, irrespective which branch
  return(data_out_all) 

}

# ------------------- #

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
