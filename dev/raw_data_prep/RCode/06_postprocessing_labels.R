### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2024-04-26
# About: this file loads the aggregated data and assign labels. This is split 
# from the aggregation to allow for speedy change of labels without re-running 
# the entire process 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] Polarity and Reference Category Table ### 

## [1-A: Polarity]
# work of indicator scales # 
dt_ref_polarity <- copy(indicator_scale)
dt_ref_polarity[, polarity := "NA"]
# more means better ... # 
dt_ref_polarity[indicator %in% c("sf12pcs_dv", "sf12mcs_dv",
                                 "fimnlabnet_dv",  "fihhmnnet1_dv"),
                polarity := "more_better"]
# more means worse ... "
dt_ref_polarity[indicator %in% c("scghq1_dv", "houscost1_dv"),
                polarity := "more_worse"]

## [1-B: Reference Category] --> only for categorical variables / based on majority
dt_ref_polarity[, cat_reference := "NA"]
dt_ref_polarity[indicator %in% c("hiqual_dv"), cat_reference := "level_4_and_above"]
dt_ref_polarity[indicator %in% c("racel_dv"), cat_reference := "white"]
dt_ref_polarity[indicator %in% c("marstat"), cat_reference := "married"]
dt_ref_polarity[indicator %in% c("jbstat"), cat_reference := "in_paid_employment"]
dt_ref_polarity[indicator %in% c("scsf1"), cat_reference := "excellent_very_good"]
dt_ref_polarity[indicator %in% c("sclonely"), cat_reference := "hardly_never"]
dt_ref_polarity[indicator %in% c("scsf2a"), cat_reference := "no_never"]
dt_ref_polarity[indicator %in% c("sclfsat1"), cat_reference := "satisfied"]
dt_ref_polarity[indicator %in% c("sclfsat2"), cat_reference := "satisfied"]
dt_ref_polarity[indicator %in% c("sclfsato"), cat_reference := "satisfied"]
dt_ref_polarity[indicator %in% c("jbnssec5_dv"), cat_reference := "management_professional"]
dt_ref_polarity[indicator %in% c("benbase4"), cat_reference := "uc_not_mentioned"]
dt_ref_polarity[indicator %in% c("wkvege"), cat_reference := "every_day"]
dt_ref_polarity[indicator %in% c("auditc3"), cat_reference := "monthly_less"]
dt_ref_polarity[indicator %in% c("ecigs1"), cat_reference := "never"]
dt_ref_polarity[indicator %in% c("aidhh"), cat_reference := "yes"]
dt_ref_polarity[indicator %in% c("ccare"), cat_reference := "yes"]
dt_ref_polarity[indicator %in% c("tenure_dv"), cat_reference := "owned_outright"]
dt_ref_polarity[indicator %in% c("hhtype_dv"), cat_reference := "married_with_children"]
dt_ref_polarity[indicator %in% c("hheat"), cat_reference := "yes"]
dt_ref_polarity[indicator %in% c("heatch"), cat_reference := "yes"]
dt_ref_polarity[indicator %in% c("ctband_dv"), cat_reference := "c"]

# correct naming
data.table::setnames(dt_ref_polarity, old = "indicator", new = "obs")

# export
data.table::fwrite(dt_ref_polarity, paste0("ROutput/polarity_cat_reference.csv"))

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [2] Lookup ### 

## function for unique combinations of variable and measurement 
.VarLabLookup <- function(data_input, colnumber) {
  data_output <- unique(data_input[, ..colnumber])
  data_output[, obs := colnames(data_output)[1]]
  data.table::setnames(data_output,
                       old = colnames(data_output)[1],
                       new = "cat")
  data.table::setcolorder(data_output, c("obs", "cat"))
  return(data_output)
}

## run over list 
lookup_list <- vector(mode = "list", length = 30)
for (z in 1:30) {
  lookup_list[[z]] <- .VarLabLookup(dt_sp, z + 9)
  print(paste("variable z level:", z))
}

# subset for categorical variables only 
lookup_list <- data.table::rbindlist(lookup_list, fill = FALSE, idcol = NULL)
lookup_list <- lookup_list[obs %in% 
                indicator_scale$indicator[indicator_scale$categorical==TRUE] 
                | obs %in% c("sex","age")]

# add cat_label
lookup_list[, cat_label := gsub(cat, pattern = "[_]", replacement = " ")] 
lookup_list[cat_label %in% c("missing inapplicable"),
            cat_label := "missing or inapplicable"]
lookup_list[obs == "wkvege" & cat_label == "1 3 days", cat_label := "1 to 3 days"]
lookup_list[obs == "wkvege" & cat_label == "4 6 days", cat_label := "4 to 6 days"]
lookup_list[obs == "auditc3" & cat_label == "2 4 month", cat_label := "2 to 4 per month"]
lookup_list[obs == "auditc3" & cat_label == "3 4 week", cat_label := "3 to 4 per week"]
lookup_list[obs == "auditc3" & cat_label == "4 more week", cat_label := "more than 4 per week"]

# add obs_label

.ObsLabelLookup <- function(data) {
  
  # introduce labels for observation
  data[, obs_label := "MISSING"]
  
  # constraints
  data[obs == "hiqual_dv", obs_label := "highest qualification"]
  data[obs == "racel_dv", obs_label := "ethnicity"]
  data[obs == "marstat", obs_label := "marital status"]
  data[obs == "jbstat", obs_label := "employment status"]
  data[obs == "hhtype_dv", obs_label := "household composition / type"]
  data[obs == "scsf1", obs_label := "general health"]
  data[obs == "tenure_dv", obs_label := "household tenure"]
  
  # addittional household-level variables 
  data[obs == "hheat", obs_label := "keep accommodation warm enough"]
  data[obs == "heatch", obs_label := "household has central heating"]
  data[obs == "ctband_dv", obs_label := "council tax band"]
  data[obs == "fihhmnnet1_dv", obs_label := "total household net income - no deductions (mean)"] # metric
  
  # additional individual-level variables 
  data[obs == "sclonely", obs_label := "feeling lonely"]
  data[obs == "scsf2a", obs_label := "health limits moderate activities"]
  data[obs == "sclfsato", obs_label := "satisfaction with life overall"]
  data[obs == "sclfsat1", obs_label := "satisfaction with health"]
  data[obs == "sclfsat2", obs_label := "satisfaction with income"]
  data[obs == "jbnssec5_dv", obs_label := "current job: five class NS-SEC"]
  data[obs == "benbase4", obs_label := "universal credit"]
  data[obs == "wkvege", obs_label := "days each week eat vegetables"]
  data[obs == "auditc3", obs_label := "alcohol frequency past 12 months"]
  data[obs == "ecigs1", obs_label := "uses electronic cigarettes"]
  data[obs == "aidhh", obs_label := "cares for sick/disabled/elderly in household"]
  data[obs == "ccare", obs_label := "uses childcare"]

  data[obs %in% c("sex", "age"), obs_label := obs]
  
  # explicit
  return(data)
}

lookup_list <- .ObsLabelLookup(lookup_list)

# add metric 
lookup_list <- rbind(lookup_list,
      data.table::data.table(obs = "houscost1_dv", cat = "NA", cat_label = "NA",
        obs_label = "monthly housing cost including mortgage principal payments (mean)"),
      data.table::data.table(obs = "sf12pcs_dv", cat = "NA", cat_label = "NA",
        obs_label = "SF-12 Physical Component Summary (mean)"), 
      data.table::data.table(obs = "sf12mcs_dv", cat = "NA", cat_label = "NA",
        obs_label = "SF-12 Mental Component Summary (mean)"), 
      data.table::data.table(obs = "fimnlabnet_dv", cat = "NA", cat_label = "NA",
        obs_label = "basic pay hourly rate (mean)"), 
      data.table::data.table(obs = "scghq1_dv", cat = "NA", cat_label = "NA",
        obs_label = "Subjective wellbeing (GHQ): Likert (mean)"))

# col order
data.table::setcolorder(lookup_list, neworder = c("obs", "obs_label",
                                                  "cat", "cat_label"))

# export
data.table::fwrite(lookup_list, paste0("ROutput/lookup_list.csv"))

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

