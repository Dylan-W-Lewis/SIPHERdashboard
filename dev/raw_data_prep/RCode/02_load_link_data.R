##### Meta #####

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2024-06-04
# About: loads SP, loads survey and retains variables of interest, links SP 
#        wit survey data for individuals & households + look up files

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Load SP Backbone: With Health ###

# load sp # 
dt_sp <- data.table::fread("RData/sp_final/sp_ind_wavek_census2011_est2020_8cons.csv")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Load US raw data for individuals ###

# load us ind data # 
dt_us_ind <- data.table::fread("RData/us_raw/k_indresp.tab")

# rename # 
data.table::setnames(dt_us_ind, old = names(dt_us_ind),
        new = gsub(paste0("^",definitions$us_wave,"_"),"",names(dt_us_ind)))

# subset for variables to keep
ind_keep <- definitions$ind_keep
dt_us_ind <- dt_us_ind[, ..ind_keep]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Load US raw data for households ###

# load us hh data # 
dt_us_hh <- data.table::fread("RData/us_raw/k_hhresp.tab")

# rename # 
data.table::setnames(dt_us_hh, old = names(dt_us_hh),
        new = gsub(paste0("^",definitions$us_wave,"_"),"",names(dt_us_hh)))

# subset for variables to keep
hh_keep <- definitions$hh_keep
dt_us_hh <- dt_us_hh[, ..hh_keep]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Merge Data: US survey files into one and then with SP ###

# merge ind and hh survey data
dt_us_both <- merge(dt_us_ind, dt_us_hh, by = c("hidp"), all.x = TRUE)

# Create SP with all variables
dt_sp <- merge(dt_sp, dt_us_both, by = c("pidp"), all.x = TRUE)

# SP's Zone ID is the lsoa2011
data.table::setnames(dt_sp, old = "ZoneID", new = "lsoa11")

# Area level #1: all of GB
dt_sp[, gb := "GB"]

# Area level #2: Countries
dt_sp[, ctr := substr(lsoa11, 1, 1)]
dt_sp[ctr == "S", ctr := "Scotland"]
dt_sp[ctr == "E", ctr := "England"]
dt_sp[ctr == "W", ctr := "Wales"]


# that's the sp ready for granular area-level linkages ...


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Granular Area Level Linkages ###

### [1] LSOA 2011 to 2021: Bridge ###

# Best-Fit Bridge for England and Wales ONLY FOR Scotland: 2011 = 2021 FOR NOW
# source: https://geoportal.statistics.gov.uk/datasets/b14d449ba10a48508bd05cd4a9775e2b_0/explore
lu_lsoa_bridge <- data.table::fread(paste0("RData/geo_lookup/LSOA_(2011)_to_LSOA_",
                              "(2021)_to_Local_Authority_District_(2022)",
                              "_Lookup_for_England_and_Wales_(Version_2).csv"))
lu_lsoa_bridge <- lu_lsoa_bridge[, .(lsoa11 = LSOA11CD, lsoa21 = LSOA21CD)]
dt_sp <- merge(dt_sp, lu_lsoa_bridge, by = "lsoa11", all.x= TRUE)

# for Scotland, we cannot bridge yet so lsoa11 -> lsoa 21
dt_sp[ctr == "Scotland", lsoa21 := lsoa11]

# ------------- # 

### [2] LSOA 2021 to msoa 2021 and lads ###

# LSOA 2021 to higher level geographies (November 2023): msoa 2021 and lad 23
# source: https://geoportal.statistics.gov.uk/datasets/18b6cc7d58bb413f814237b6dfa1f5c9/about
lu_geo_higher <-  data.table::fread(paste0("RData/geo_lookup/PCD_OA21_LSOA21_",
                                           "MSOA21_LAD_NOV23_UK_LU.csv"))
lu_geo_higher[, ctr := substr(lsoa21cd, 1, 1)]
lu_geo_higher_ew <- unique(lu_geo_higher[ctr %in% c("E","W"),
                                      .(lsoa21 = lsoa21cd,
                                        msoa21 = msoa21cd,
                                        lad = ladcd)])
lu_geo_higher <- unique(lu_geo_higher[ctr %in% c("E","S","W"),
                .(lsoa21 = lsoa21cd, msoa21 = msoa21cd, lad = ladcd)])
dt_sp <- merge(dt_sp, lu_geo_higher, by = "lsoa21", all.x= TRUE)

# ------------- # 

### [3] LSOA 2011 to wards ###

# LSOA 2011 to Ward 2022
lu_geo_ward <- data.table::fread("RData/geo_lookup/lsoa11_wd22_map.csv")
data.table::setnames(lu_geo_ward, old = "LSOA11", new = "lsoa11")
data.table::setnames(lu_geo_ward, old = "WD22", new = "ward")
dt_sp <- merge(dt_sp, lu_geo_ward, by = "lsoa11", all.x = TRUE)

# order columns 
data.table::setcolorder(dt_sp, neworder = c("lsoa11", "lsoa21", "msoa21",
                                            "ward", "lad", "ctr", "gb"))

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Pre-Processing Checks: Area ###

# every individuals comes with an area code for all levels 
dim(dt_sp[is.na(gb) | is.na(ctr) | is.na(lad) | is.na(ward) |
     is.na(msoa21) | is.na(lsoa21) | is.na(lsoa11), ])[1] == 0

# gb -> only one gb, correct
length(unique(dt_sp$gb)) == 1

# ctrs -> three gb nations, correct
length(unique(dt_sp$ctr)) == 3

# lads in 2021 -> 363 local authorities, correct
length(unique(dt_sp$lad)) == length(unique(lu_geo_higher$lad))

# wards in 2022 -> 7,973 wards, correct
length(unique(dt_sp$ward)) == length(unique(lu_geo_ward$ward))

### A bit more challenging ... for lowest census level geographies ....

# Load LSOA 2011 reference
# source: https://geoportal.statistics.gov.uk/datasets/postcode-to-output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-may-2021-lookup-in-the-uk-1/about
lu_lsoa_2011 <- data.table::fread(paste0("RData/geo_lookup/OA_to_Local_",
                                         "Authority_District_May_2021.csv"))
data.table::setnames(lu_lsoa_2011, old = "lsoa11cd", new = "lsoa11")
lu_lsoa_2011[, ctr := substr(lsoa11,1,1)]
lu_lsoa_2011 <- unique(lu_lsoa_2011[ctr %in% c("S", "E", "W"), .(lsoa11, ctr)])
lu_lsoa_2011_ew <- lu_lsoa_2011[ctr %in% c("E", "W"), ]
length(unique(dt_sp$lsoa11)) - length(unique(lu_lsoa_2011$lsoa11)) 
diff_lsoa11 <- setdiff(unique(lu_lsoa_2011$lsoa11), unique(dt_sp$lsoa11)) # LSOA in 2011 -> 41,729 vs. 41,726 
# --> 3 LSOAs aroung Glasgow do not appear in SP - we know this, all correct

# MSOAs in 2021 -> 8,540 vs. 8,543: "E02007056" "E02006995" "E02007028" --> 3 MSOAs did not get bridged after best fit LSOA 
length(unique(dt_sp$msoa21)) - length(unique(lu_geo_higher$msoa21))
diff_msoa21 <- setdiff(unique(lu_geo_higher$msoa21), unique(dt_sp$msoa21))

# LSOA in 2021: 
length(unique(lu_geo_higher$lsoa21)) # there should be 42,648
length(unique(dt_sp$lsoa21))         # our SP only has 41,601 (-1,047)

# problem is the bridging, for example: -1,044 LSOA's do not have a best fit bridge
length(unique(lu_lsoa_bridge$lsoa21)) - length(unique(lu_geo_higher_ew$lsoa21))

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Prepare list of variables ready for Aggregation ###

# initialise data set 
indicator_scale <- data.table::data.table(
  indicator   = c(definitions$ind_keep, definitions$hh_keep),
  categorical = NA)

# define scale level for functional form queries ...

# [1] constraints  
indicator_scale[indicator == "hiqual_dv",  categorical := TRUE]
indicator_scale[indicator == "racel_dv",   categorical := TRUE]
indicator_scale[indicator == "marstat",    categorical := TRUE]
indicator_scale[indicator == "jbstat",     categorical := TRUE]
indicator_scale[indicator == "scsf1",      categorical := TRUE]
indicator_scale[indicator == "hhtype_dv",  categorical := TRUE]
indicator_scale[indicator == "tenure_dv",  categorical := TRUE]

# [2] additional Individual level
indicator_scale[indicator == "sf12pcs_dv",   categorical := FALSE]
indicator_scale[indicator == "sf12mcs_dv",   categorical := FALSE]
indicator_scale[indicator == "fimnlabnet_dv",categorical := FALSE]
indicator_scale[indicator == "scghq1_dv",    categorical := FALSE]
indicator_scale[indicator == "sclonely",     categorical := TRUE]
indicator_scale[indicator == "scsf2a",       categorical := TRUE]
indicator_scale[indicator == "sclfsato",     categorical := TRUE]
indicator_scale[indicator == "sclfsat1",     categorical := TRUE]
indicator_scale[indicator == "sclfsat2",     categorical := TRUE]
indicator_scale[indicator == "jbnssec5_dv",  categorical := TRUE]
indicator_scale[indicator == "benbase4",     categorical := TRUE]
indicator_scale[indicator == "wkvege",       categorical := TRUE]
indicator_scale[indicator == "auditc3",      categorical := TRUE]
indicator_scale[indicator == "ecigs1",       categorical := TRUE]
indicator_scale[indicator == "aidhh",        categorical := TRUE]
indicator_scale[indicator == "ccare",        categorical := TRUE]

# [3] additional HH level
indicator_scale[indicator == "fihhmnnet1_dv", categorical := FALSE]
indicator_scale[indicator == "houscost1_dv",  categorical := FALSE]
indicator_scale[indicator == "hheat",         categorical := TRUE]
indicator_scale[indicator == "heatch",        categorical := TRUE]
indicator_scale[indicator == "ctband_dv",     categorical := TRUE]

# only keep defined indicators, for now: constraints 
indicator_scale <- indicator_scale[!is.na(categorical),]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Tidy Workspace & Deep Clean RAM ###
rm(dt_us_both, dt_us_ind, dt_us_hh, ind_keep, hh_keep,
   lu_geo_higher, lu_geo_higher_ew,
   lu_geo_ward,
   lu_lsoa_2011, lu_lsoa_2011_ew,
   lu_lsoa_bridge)
gc(full = TRUE)   

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


