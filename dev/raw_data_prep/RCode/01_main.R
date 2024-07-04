##### Meta #####

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2024-06-04
# About: this is the main control capturing all definitions and prerequisites 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Preparation ###

# clean workspace
rm(list = ls())  # remove all objects from work space 
gc(full = TRUE)  # deep clean garbage
dir()

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Libraries ###

# List of Required Packages 
RequiredPackages <- c("data.table") # Dialect 

# ensure all packages are installed and loaded 
.EnsurePackages <- function(packages_vector) {
  new_package <- packages_vector[!(packages_vector %in% 
                                     installed.packages()[, "Package"])]
  if (length(new_package) != 0) {
    install.packages(new_package) }
  sapply(packages_vector, suppressPackageStartupMessages(require),
         character.only = TRUE)
}
.EnsurePackages(RequiredPackages)

### Initialise File Paths ##
here <- here::here # in order to ensure correct file paths 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Definitions ###

# initialize ...
definitions <- list()

# stages 
definitions$aggregation     <- TRUE
definitions$post_processing <- TRUE

# options: display format of numbers and timeout for downloads
definitions$scipen  <- 1e4
definitions$timeout <- 1e4

# US Wave 
definitions$us_wave     <- "k"   

# define area-level variables for aggregation
definitions$area_levels <- c("gb", "ctr", "lad", "ward")

# US keep variables: Individuals
definitions$ind_keep <- c("pidp", "hidp",
  "sex", "age_dv", "hiqual_dv", "racel_dv", "marstat", "jbstat", "scsf1",
  "sf12pcs_dv", "sf12mcs_dv", "sclonely", "scghq1_dv", "scsf2a",
  "sclfsat1", "sclfsat2", "sclfsato", "jbnssec5_dv", "fimnlabnet_dv",
  "benbase4", "wkvege", "auditc3", "ecigs1", "aidhh", "ccare")

# US keep variables: Households
definitions$hh_keep <- c("hidp",
                         "tenure_dv", "hhtype_dv", "fihhmnnet1_dv",                 
                         "houscost1_dv", "hheat", "heatch", "ctband_dv")
# Rounding in Report: 
definitions$round <- 4

# apply options
options(scipen  = definitions$scipen, timeout = definitions$timeout)


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Source Code ###

# Benchmark Run Time: START #
benchmark_time <- list() 
benchmark_time$start <- Sys.time()

# always source
source("RCode/02_load_link_data.R")
source("RCode/03_recode_data.R")

# Source Code Files Conditionally
if (definitions$aggregation  == TRUE) {
    gc(full = TRUE)  # deep clean RAM
    source("RCode/04_aggregation_function.R")
    source("RCode/05_aggregation_export.R")
}

if (definitions$post_processing  == TRUE) {
    gc(full = TRUE)  # deep clean RAM
    source("RCode/06_postprocessing_labels.R")
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Benchmark Time ###

benchmark_time$end <- Sys.time()
print("Duration of Program:")
print(round(benchmark_time$end - benchmark_time$start), 
      definitions$rounding_results)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #