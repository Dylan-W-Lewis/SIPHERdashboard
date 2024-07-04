##### Meta #####

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2024-06-04
# About: this file re codes the linked SP data set. We have to ensure that: 
#        missing metric variables are < 0 (e.g., -8, -9 etc.)
#        missing/other/unknown categorical observations are in residual category

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] Recode Constraints ###

# note: constraint categories are similar to those defined during the alignment 
# process and when creating the synthetic population data set.

# Sex #
dt_sp[, sex := as.character(sex)]
dt_sp[, sex := fifelse(sex == "1", "male", "female")]

# Age #
dt_sp[, age_dv := cut(age_dv,
                   breaks = c(15, 24, 34, 49, 64, 74, Inf),
                   labels   = c("16_to_24", "25_to_34", "35_to_49",
                                "50_to_64", "65_to_74", "75_up"))]
data.table::setnames(dt_sp, old = "age_dv", new = "age")

# Highest Qualification #
dt_sp[, hiqual_dv := as.character(hiqual_dv)]
dt_sp[hiqual_dv %in% c("1","2"), hiqual_dv := "level_4_and_above"]
dt_sp[hiqual_dv == "3", hiqual_dv := "level_3"]
dt_sp[hiqual_dv %in% c("4","5"), hiqual_dv := "level_1_to_2"]
dt_sp[hiqual_dv == "9", hiqual_dv := "none"]

# Ethnicity #
dt_sp[, racel_dv := as.character(racel_dv)]
dt_sp[racel_dv %in% c("1", "2", "3", "4"),   racel_dv := "white"] 
dt_sp[racel_dv %in% c("5", "6", "7", "8"),   racel_dv := "mixed"] 
dt_sp[racel_dv %in% c("9", "10", "11", "12", "13"),  racel_dv := "asian"] 
dt_sp[racel_dv %in% c("14", "15", "16"),  racel_dv := "black"]  
dt_sp[racel_dv %in% c("17", "97"), racel_dv := "other"]  

# Marital Status #
dt_sp[, marstat := as.character(marstat)]
dt_sp[marstat ==  "1", marstat := "single"] 
dt_sp[marstat ==  "2", marstat := "married"] 
dt_sp[marstat ==  "3", marstat:= "civil_partner"] 
dt_sp[marstat ==  "4", marstat := "separated"]  
dt_sp[marstat ==  "5", marstat := "divorced"]  
dt_sp[marstat ==  "6", marstat := "widowed_surviving"]  
dt_sp[marstat ==  "7", marstat := "separated"]  
dt_sp[marstat ==  "8", marstat := "divorced"]  
dt_sp[marstat ==  "9", marstat := "widowed_surviving"]  

# Employment Status 
dt_sp[, jbstat := as.character(jbstat)]
dt_sp[jbstat ==  "1", jbstat := "self_employed"] 
dt_sp[jbstat ==  "2", jbstat := "in_paid_employment"] 
dt_sp[jbstat ==  "3", jbstat := "unemployed"] 
dt_sp[jbstat ==  "4", jbstat := "retired"] 
dt_sp[jbstat ==  "5", jbstat := "other"] 
dt_sp[jbstat ==  "6", jbstat := "looking_after_home"] 
dt_sp[jbstat ==  "7", jbstat := "student"] 
dt_sp[jbstat ==  "8", jbstat := "long_term_sick_disabled"] 
dt_sp[jbstat %in% c("9","10", "11", "12", "13", "97"), jbstat := "other"] 

# HH Composition / Type
dt_sp[, hhtype_dv := as.character(hhtype_dv)]
dt_sp[hhtype_dv %in% c("1", "2", "3"), hhtype_dv := "one_person_no_child"] 
dt_sp[hhtype_dv %in% c("4", "5"), hhtype_dv := "one_person_with_children"] 
dt_sp[hhtype_dv %in% c("6", "8"), hhtype_dv := "married_no_child"] 
dt_sp[hhtype_dv %in% c("10", "11", "12"), hhtype_dv := "married_with_children"] 
dt_sp[hhtype_dv %in% c("16", "17"), hhtype_dv := "cohabiting_no_child"] 
dt_sp[hhtype_dv %in% c("18"), hhtype_dv := "cohabiting_with_children"] 
dt_sp[hhtype_dv %in% c("19"), hhtype_dv := "other_family_no_child"] 
dt_sp[hhtype_dv %in% c("20", "21"), hhtype_dv := "other_not_family_with_children"] 
dt_sp[hhtype_dv %in% c("22"), hhtype_dv := "other_not_family_no_child"] 
dt_sp[hhtype_dv %in% c("23"), hhtype_dv := "other_not_family_with_children"] 

# General Health 
dt_sp[, scsf1 := as.character(scsf1)]
dt_sp[scsf1 %in% c("1", "2"), scsf1 := "excellent_very_good"] 
dt_sp[scsf1 ==  "3", scsf1 := "good"] 
dt_sp[scsf1 ==  "4", scsf1 := "fair"] 
dt_sp[scsf1 ==  "5", scsf1 := "poor"] 

# HH Tenure
dt_sp[, tenure_dv := as.character(tenure_dv)]
dt_sp[tenure_dv == "1", tenure_dv := "owned_outright"] 
dt_sp[tenure_dv == "2", tenure_dv := "owned_mortage"] 
dt_sp[tenure_dv %in% c("3", "4"), tenure_dv := "social_rented"] 
dt_sp[tenure_dv %in% c("5", "6", "7", "8"), tenure_dv := "private_rented"] 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [2] Recode Additional HH-Level Variables ###  
# not required / metric: "fihhmnnet1_dv", "houscost1_dv"
# required: "hheat", "heatch", "ctband_dv"

# hheat: keep accommodation warm enough
dt_sp[, hheat := as.character(hheat)]
dt_sp[hheat %in% c("-2", "-1", "3"), hheat := "missing_inapplicable"] 
dt_sp[hheat == "1", hheat := "yes"] 
dt_sp[hheat == "2", hheat := "no"] 

# heatch: HH has central heating
dt_sp[, heatch := as.character(heatch)]
dt_sp[heatch %in% c("-2", "-1"), heatch := "missing_inapplicable"] 
dt_sp[heatch == "1", heatch := "yes"] 
dt_sp[heatch == "2", heatch := "no"] 

# ctband_dv
dt_sp[, ctband_dv := as.character(ctband_dv)]
dt_sp[ctband_dv %in% c("-9", "-8"), ctband_dv := "missing_inapplicable"] 
dt_sp[ctband_dv %in% c("1","2"), ctband_dv := "a_or_b"] 
dt_sp[ctband_dv %in% c("3","4"), ctband_dv := "c_or_d"] 
dt_sp[ctband_dv %in% c("5","6"), ctband_dv := "e_or_f"] 
dt_sp[ctband_dv %in% c("7", "8", "9", "10"),  ctband_dv := "g_plus"] 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [3] Recode Additional Individual-Level Variables ###
# not required / metric: "sf12pcs_dv", "sf12mcs_dv", "fimnlabnet_dv", scghq1_dv
# required: "sclonely", "scsf2a", "sclfsato", "sclfsat1", sclfsat2, "jbnssec5_dv", benbase4, wkvege, auditc3, ecigs1, aidhh, ccare
            
# sclonely: feeling lonely 
dt_sp[, sclonely := as.character(sclonely)]
dt_sp[sclonely %in% c("-9", "-2", "-1"), sclonely := "missing_inapplicable"]
dt_sp[sclonely == "1", sclonely := "hardly_never"]
dt_sp[sclonely == "2", sclonely := "sometimes"]
dt_sp[sclonely == "3", sclonely := "often"]

# scsf2a: health limits physical activities
dt_sp[, scsf2a := as.character(scsf2a)]
dt_sp[scsf2a %in% c("-9", "-2", "-1"), scsf2a := "missing_inapplicable"]
dt_sp[scsf2a == "1", scsf2a := "yes_lot"]
dt_sp[scsf2a == "2", scsf2a := "yes_little"]
dt_sp[scsf2a == "3", scsf2a := "no_never"]

# sclfsato: satisfaction with life overvall
dt_sp[, sclfsato := as.character(sclfsato)]
dt_sp[sclfsato %in% c("-9", "-2", "-1"), sclfsato := "missing_inapplicable"]
dt_sp[sclfsato %in% c("1", "2", "3"), sclfsato := "dissatisfied"]
dt_sp[sclfsato == "4", sclfsato := "neither_nor"]
dt_sp[sclfsato %in% c("5", "6", "7"), sclfsato := "satisfied"]

# sclfsat1: satisfaction with health
dt_sp[, sclfsat1 := as.character(sclfsat1)]
dt_sp[sclfsat1 %in% c("-9", "-2", "-1"), sclfsat1 := "missing_inapplicable"]
dt_sp[sclfsat1 %in% c("1", "2", "3"), sclfsat1 := "dissatisfied"]
dt_sp[sclfsat1 == "4", sclfsat1 := "neither_nor"]
dt_sp[sclfsat1 %in% c("5", "6", "7"), sclfsat1 := "satisfied"]

# sclfsat2: satisfaction with income
dt_sp[, sclfsat2 := as.character(sclfsat2)]
dt_sp[sclfsat2 %in% c("-9", "-2", "-1"), sclfsat2 := "missing_inapplicable"]
dt_sp[sclfsat2 %in% c("1", "2", "3"), sclfsat2 := "dissatisfied"]
dt_sp[sclfsat2 == "4", sclfsat2 := "neither_nor"]
dt_sp[sclfsat2 %in% c("5", "6", "7"), sclfsat2 := "satisfied"]

# jbnssec5_dv: NsSec
dt_sp[, jbnssec5_dv := as.character(jbnssec5_dv)]
dt_sp[jbnssec5_dv %in% c("-9", "-8"), jbnssec5_dv := "missing_inapplicable"]
dt_sp[jbnssec5_dv == "1", jbnssec5_dv := "management_professional"]
dt_sp[jbnssec5_dv == "2", jbnssec5_dv := "intermediate"]
dt_sp[jbnssec5_dv == "3", jbnssec5_dv := "small_employers_own_account"]
dt_sp[jbnssec5_dv == "4", jbnssec5_dv := "lower_supervisory_technical"]
dt_sp[jbnssec5_dv == "5", jbnssec5_dv := "semi_routine_routine"]

# benbase4: universal credit
dt_sp[, benbase4 := as.character(benbase4)]
dt_sp[benbase4 %in% c("0", "-1", "-2"), benbase4 := "inapplicable_or_not_mentioned"]
dt_sp[benbase4 == "1", benbase4 := "in_receipt_of_uc"]

# wkvege: days each week eat vegetables
dt_sp[, wkvege := as.character(wkvege)]
dt_sp[wkvege %in% c("-1", "-2"), wkvege := "missing_inapplicable"]
dt_sp[wkvege == "1", wkvege := "never"]
dt_sp[wkvege == "2", wkvege := "1_3_days"]
dt_sp[wkvege == "3", wkvege := "4_6_days"]
dt_sp[wkvege == "4", wkvege := "every_day"]

# auditc3: alcohol frequency
dt_sp[, auditc3 := as.character(auditc3)]
dt_sp[auditc3 %in% c("1", "-1", "-2", "-8", "-9"),
      auditc3 := "inapplicable_or_never"]
dt_sp[auditc3 == "2", auditc3 := "monthly_less"]
dt_sp[auditc3 == "3", auditc3 := "2_4_month"]
dt_sp[auditc3 == "4", auditc3 := "3_4_week"]
dt_sp[auditc3 == "5", auditc3 := "4_more_week"]

# ecigs1: electronic cigarettes
dt_sp[, ecigs1 := as.character(ecigs1)]
dt_sp[ecigs1 %in% c("-1", "-2"), ecigs1 := "missing_inapplicable"]
dt_sp[ecigs1 == "1", ecigs1 := "never"]
dt_sp[ecigs1 %in% c("2", "3", "4", "5"), ecigs1 := "past_irregular"]
dt_sp[ecigs1 == "6", ecigs1 := "weekly"]

# aidhh: caring redt_sponsibilities 
dt_sp[, aidhh := as.character(aidhh)]
dt_sp[aidhh %in% c("-1", "-2", "-8", "-9"), aidhh := "missing_inapplicable"]
dt_sp[aidhh == "1", aidhh := "yes"]
dt_sp[aidhh == "2", aidhh := "no"]

# ccare: uses childcare  
dt_sp[, ccare := as.character(ccare)]
dt_sp[ccare %in% c("-1", "-2", "-8", "-9"), ccare := "missing_inapplicable"]
dt_sp[ccare == "1", ccare := "yes"]
dt_sp[ccare == "2", ccare := "no"]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Deep Clean RAM ###

gc(full=TRUE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
