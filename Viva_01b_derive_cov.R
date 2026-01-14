################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 09-Oct-2024
# This script is to derive covariates and other relevant variables of interest in Project Viva.

################################################################################

#------------------------------------------------------------------------------#
#                                 Housekeeping                                 #----
#------------------------------------------------------------------------------#

# Clear environment
rm(list = ls())

# Collect information about the current R session
sessionInfo()

# Load packages
pacman::p_load(
  tidyverse,
  haven,
  descr,
  expss,
  gtsummary,
  kableExtra,
  flextable,
  readr,
  magrittr,
  openxlsx,
  corrplot,
  ggcorrplot
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/Viva/dat_exp.rds")
dat <- zap_labels(dat)

head(dat)
dim(dat)  # 1872  534

#------------------------------------------------------------------------------#
#                               Early Pregnancy                                #----
#------------------------------------------------------------------------------#

# Maternal age
dat$age_Mat_con <- dat$age_mom_enroll_d

dat$age_Mat_con <-
  as.numeric(dat$age_Mat_con)

var_lab(dat$age_Mat_con) = "Maternal age (years)"

str(dat$age_Mat_con)
summary(dat$age_Mat_con)

# Ethnicity

## Detailed ethnic groups
dat$ethnic_Mat_cat <-
  dat$race2_mom_epi_epia_d

dat$ethnic_Mat_cat <-
  car::recode(
    dat$ethnic_Mat_cat,
    "'white'=0;'black'=1;'hispa'=2;'asian'=3;'more than 1 race'=4;'amind'=4;'other'=4",
    as.factor = F,
    as.numeric = T
  )
dat$ethnic_Mat_cat <-
  as.numeric(dat$ethnic_Mat_cat)

val_lab(dat$ethnic_Mat_cat) = c(
  "White" = 0,
  "Black" = 1,
  "Hispanic" = 2,
  "Asian" = 3,
  "Mixed or Other" = 4
)

dat$ethnic_Mat_cat <-
  as.factor(dat$ethnic_Mat_cat)

var_lab(dat$ethnic_Mat_cat) = "Maternal ethnicity (detailed)"

str(dat$ethnic_Mat_cat)
table(dat$ethnic_Mat_cat, useNA = "always")

## Binary - White vs. Other
dat$ethnic_Mat_bin <-
  dat$race2_mom_epi_epia_d

dat$ethnic_Mat_bin <-
  car::recode(
    dat$ethnic_Mat_bin,
    "'white'=0;'black'=1;'hispa'=1;'asian'=1;'more than 1 race'=1;'amind'=1;'other'=1",
    as.factor = F,
    as.numeric = T
  )
dat$ethnic_Mat_bin <-
  as.numeric(dat$ethnic_Mat_bin)

val_lab(dat$ethnic_Mat_bin) = c("White" = 0, "Other" = 1)

dat$ethnic_Mat_bin <- as.factor(dat$ethnic_Mat_bin)

var_lab(dat$ethnic_Mat_bin) = "Maternal ethnicity (binary)"

str(dat$ethnic_Mat_bin)
table(dat$ethnic_Mat_bin, useNA = "always")

# Education level
table(dat$education_mom_epi_epia_d, useNA = "always")
table(dat$coll_grad, useNA = "always")

## Binary
dat$edu_Mat_bin <- dat$coll_grad

val_lab(dat$edu_Mat_bin) = c("Lower than college degree" = 0,
                             "College degree or more" = 1)

dat$edu_Mat_bin <- as.factor(dat$edu_Mat_bin)

var_lab(dat$edu_Mat_bin) = "Maternal education attainment (binary)"

str(dat$edu_Mat_bin)
table(dat$edu_Mat_bin, useNA = "always")

## Ordinal - 3 categories - Low: Lower than college degree; Medium: College degree; High: Graduate degree
dat$edu_Mat_3cat <- as.character(dat$education_mom_epi_epia_d)

dat$edu_Mat_3cat[dat$edu_Mat_3cat %in% c("1", "2", "3")] <- "Low (lower than college degree)"
dat$edu_Mat_3cat[dat$edu_Mat_3cat == "4"] <- "Medium (college degree)"
dat$edu_Mat_3cat[dat$edu_Mat_3cat == "5"] <- "High (graduate degree)"

dat$edu_Mat_3cat <-
  factor(
    dat$edu_Mat_3cat,
    levels = c(
      "Low (lower than college degree)",
      "Medium (college degree)",
      "High (graduate degree)"
    ),
    ordered = T
  )  # Set as ordinal variable

var_lab(dat$edu_Mat_3cat) = "Maternal education attainment (3 categories)"

str(dat$edu_Mat_3cat)
table(dat$edu_Mat_3cat, useNA = "always")

# Household income
table(dat$income_hh_epq_epqa_d, useNA = "always")
table(dat$gt70k, useNA = "always")

## Binary
dat$income_Fam_bin <- dat$gt70k

val_lab(dat$income_Fam_bin) = c("≤$70,000/year" = 0, ">$70,000/year" = 1)

dat$income_Fam_bin <- as.factor(dat$income_Fam_bin)

var_lab(dat$income_Fam_bin) = "Household income (binary)"

str(dat$income_Fam_bin)
table(dat$income_Fam_bin, useNA = "always")

## 3 categories
dat$income_Fam_3cat <- as.character(dat$income_hh_epq_epqa_d)

dat$income_Fam_3cat[dat$income_Fam_3cat %in% c("1", "2", "3", "4")] <- "Low (≤$40,000/year)"
dat$income_Fam_3cat[dat$income_Fam_3cat == "5"] <- "Medium ($40,001 - $70,000/year)"
dat$income_Fam_3cat[dat$income_Fam_3cat == "6"] <- "High (>$70,000/year)"

dat$income_Fam_3cat <-
  factor(
    dat$income_Fam_3cat,
    levels = c(
      "Low (≤$40,000/year)",
      "Medium ($40,001 - $70,000/year)",
      "High (>$70,000/year)"
    ),
    ordered = T
  )  # Set as ordinal variable

var_lab(dat$income_Fam_3cat) = "Household income (3 categories)"

str(dat$income_Fam_3cat)
table(dat$income_Fam_3cat, useNA = "always")

# Marital status
dat$marital_Mat_bin <- dat$married_cohab

val_lab(dat$marital_Mat_bin) = c("Not married" = 0, "Married or cohabiting" = 1)

dat$marital_Mat_bin <- as.factor(dat$marital_Mat_bin)

var_lab(dat$marital_Mat_bin) = "Maternal marital status (binary)"

str(dat$marital_Mat_bin)
table(dat$marital_Mat_bin, useNA = "always")

# Parity
dat$parity_d

dat$parity_Mat_bin <- dat$nullip

dat$parity_Mat_bin <-
  car::recode(dat$parity_Mat_bin,
              "1=0;0=1",
              as.factor = F,
              as.numeric = T)

val_lab(dat$parity_Mat_bin) = c("0" = 0, "≥1" = 1)

dat$parity_Mat_bin <- as.factor(dat$parity_Mat_bin)

var_lab(dat$parity_Mat_bin) = "Maternal parity (binary)"

str(dat$parity_Mat_bin)
table(dat$parity_Mat_bin, useNA = "always")

# Pre-pregnancy BMI
dat$BMI_Mat_PRE.p_con <- dat$bmi_mom_prepreg_d

dat$BMI_Mat_PRE.p_con <-
  as.numeric(dat$BMI_Mat_PRE.p_con)

var_lab(dat$BMI_Mat_PRE.p_con) = "Maternal pre-pregnancy BMI (kg/m^2)"

str(dat$BMI_Mat_PRE.p_con)
summary(dat$BMI_Mat_PRE.p_con)

# Smoking

## Binary - Yes vs. No
dat$smoking_Mat_EAR.p_bin <-
  dat$smokpreg_final_d

dat$smoking_Mat_EAR.p_bin <-
  car::recode(
    dat$smoking_Mat_EAR.p_bin,
    "'xnever'=0;'former'=0;'smoke preg'=1",
    as.factor = F,
    as.numeric = T
  )

dat$smoking_Mat_EAR.p_bin <-
  as.numeric(dat$smoking_Mat_EAR.p_bin)

val_lab(dat$smoking_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$smoking_Mat_EAR.p_bin <-
  as.factor(dat$smoking_Mat_EAR.p_bin)

var_lab(dat$smoking_Mat_EAR.p_bin) = "Maternal smoking in early pregnancy (binary)"

str(dat$smoking_Mat_EAR.p_bin)
table(dat$smoking_Mat_EAR.p_bin, useNA = "always")

################################################################################
## 3 categories - Never vs. Former vs. Current
dat$smoking_Mat_EAR.p_3cat <-
  dat$smokpreg_final_d

dat$smoking_Mat_EAR.p_3cat <-
  car::recode(
    dat$smoking_Mat_EAR.p_3cat,
    "'xnever'=0;'former'=1;'smoke preg'=2",
    as.factor = F,
    as.numeric = T
  )

dat$smoking_Mat_EAR.p_3cat <-
  as.numeric(dat$smoking_Mat_EAR.p_3cat)

val_lab(dat$smoking_Mat_EAR.p_3cat) = c("Never" = 0,
                                        "Former" = 1,
                                        "Current" = 2)

dat$smoking_Mat_EAR.p_3cat <-
  as.factor(dat$smoking_Mat_EAR.p_3cat)

var_lab(dat$smoking_Mat_EAR.p_3cat) = "Maternal smoking in early pregnancy (3 categories)"

str(dat$smoking_Mat_EAR.p_3cat)
table(dat$smoking_Mat_EAR.p_3cat, useNA = "always")
################################################################################

# Alcohol drinking (>=1 portion (glass)/week defined as "Yes")
dat$alcohol_Mat_EAR.p_bin <- NA
dat$alcohol_Mat_EAR.p_bin[dat$alc_f1d *
                            7 < 1] <- 0
dat$alcohol_Mat_EAR.p_bin[dat$alc_f1d *
                            7 >= 1] <- 1

val_lab(dat$alcohol_Mat_EAR.p_bin) = c("No (<1 glass/week)" = 0,
                                       "Yes (≥1 glass/week)" = 1)

dat$alcohol_Mat_EAR.p_bin <- as.factor(dat$alcohol_Mat_EAR.p_bin)

var_lab(dat$alcohol_Mat_EAR.p_bin) = "Maternal alcohol drinking in early pregnancy (binary)"

str(dat$alcohol_Mat_EAR.p_bin)
table(dat$alcohol_Mat_EAR.p_bin, useNA = "always")

dat$alcohol_Mat_EAR.p_con <-
  dat$alc_f1d

dat$alcohol_Mat_EAR.p_con <-
  as.numeric(dat$alcohol_Mat_EAR.p_con)

var_lab(dat$alcohol_Mat_EAR.p_con) = "Maternal alcohol consumption in early pregnancy (servings/day)"

str(dat$alcohol_Mat_EAR.p_con)
summary(dat$alcohol_Mat_EAR.p_con)

# Physical activity - Not available in early pregnancy; use pre-pregnancy instead
dat$phys.act_Mat_PRE.p_bin <- NA
dat$phys.act_Mat_PRE.p_bin[dat$totact_pre_d < 2.5] <-
  0
dat$phys.act_Mat_PRE.p_bin[dat$totact_pre_d >= 2.5] <-
  1

val_lab(dat$phys.act_Mat_PRE.p_bin) = c(
  "Sedentary (<2.5 hours/week)" = 0,
  "Not sedentary (≥2.5 hours/week)" = 1
)

dat$phys.act_Mat_PRE.p_bin <-
  as.factor(dat$phys.act_Mat_PRE.p_bin)

var_lab(dat$phys.act_Mat_PRE.p_bin) = "Maternal pre-pregnancy physical activity (binary)"

str(dat$phys.act_Mat_PRE.p_bin)
table(dat$phys.act_Mat_PRE.p_bin, useNA = "always")

dat$phys.act_Mat_PRE.p_con <-
  dat$totact_pre_d

dat$phys.act_Mat_PRE.p_con <-
  as.numeric(dat$phys.act_Mat_PRE.p_con)

var_lab(dat$phys.act_Mat_PRE.p_con) = "Maternal pre-pregnancy physical activity (hours/week)"

str(dat$phys.act_Mat_PRE.p_con)
summary(dat$phys.act_Mat_PRE.p_con)

# Dietary supplement use
dat$vitA.supp_Mat_EAR.p_bin <-
  dat$vita_epi
val_lab(dat$vitA.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$vitA.supp_Mat_EAR.p_bin <- as.factor(dat$vitA.supp_Mat_EAR.p_bin)
var_lab(dat$vitA.supp_Mat_EAR.p_bin) = "Maternal vitamin A supplement use in early pregnancy"
str(dat$vitA.supp_Mat_EAR.p_bin)
table(dat$vitA.supp_Mat_EAR.p_bin, useNA = "always")

dat$vitC.supp_Mat_EAR.p_bin <-
  dat$vitc_epi
val_lab(dat$vitC.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$vitC.supp_Mat_EAR.p_bin <- as.factor(dat$vitC.supp_Mat_EAR.p_bin)
var_lab(dat$vitC.supp_Mat_EAR.p_bin) = "Maternal vitamin C supplement use in early pregnancy"
str(dat$vitC.supp_Mat_EAR.p_bin)
table(dat$vitC.supp_Mat_EAR.p_bin, useNA = "always")

dat$vitE.supp_Mat_EAR.p_bin <-
  dat$vite_epi
val_lab(dat$vitE.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$vitE.supp_Mat_EAR.p_bin <- as.factor(dat$vitE.supp_Mat_EAR.p_bin)
var_lab(dat$vitE.supp_Mat_EAR.p_bin) = "Maternal vitamin E supplement use in early pregnancy"
str(dat$vitE.supp_Mat_EAR.p_bin)
table(dat$vitE.supp_Mat_EAR.p_bin, useNA = "always")

dat$vitB6.supp_Mat_EAR.p_bin <-
  dat$vitb6_epi
val_lab(dat$vitB6.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$vitB6.supp_Mat_EAR.p_bin <- as.factor(dat$vitB6.supp_Mat_EAR.p_bin)
var_lab(dat$vitB6.supp_Mat_EAR.p_bin) = "Maternal vitamin B6 supplement use in early pregnancy"
str(dat$vitB6.supp_Mat_EAR.p_bin)
table(dat$vitB6.supp_Mat_EAR.p_bin, useNA = "always")

dat$folate.supp_Mat_EAR.p_bin <-
  dat$folate_epi
val_lab(dat$folate.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$folate.supp_Mat_EAR.p_bin <- as.factor(dat$folate.supp_Mat_EAR.p_bin)
var_lab(dat$folate.supp_Mat_EAR.p_bin) = "Maternal folate supplement use in early pregnancy"
str(dat$folate.supp_Mat_EAR.p_bin)
table(dat$folate.supp_Mat_EAR.p_bin, useNA = "always")

dat$calcium.supp_Mat_EAR.p_bin <-
  dat$calcium_epi
val_lab(dat$calcium.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$calcium.supp_Mat_EAR.p_bin <- as.factor(dat$calcium.supp_Mat_EAR.p_bin)
var_lab(dat$calcium.supp_Mat_EAR.p_bin) = "Maternal calcium supplement use in early pregnancy"
str(dat$calcium.supp_Mat_EAR.p_bin)
table(dat$calcium.supp_Mat_EAR.p_bin, useNA = "always")

dat$iron.supp_Mat_EAR.p_bin <-
  dat$iron_epi
val_lab(dat$iron.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$iron.supp_Mat_EAR.p_bin <- as.factor(dat$iron.supp_Mat_EAR.p_bin)
var_lab(dat$iron.supp_Mat_EAR.p_bin) = "Maternal iron supplement use in early pregnancy"
str(dat$iron.supp_Mat_EAR.p_bin)
table(dat$iron.supp_Mat_EAR.p_bin, useNA = "always")

dat$zinc.supp_Mat_EAR.p_bin <-
  dat$zinc_epi
val_lab(dat$zinc.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$zinc.supp_Mat_EAR.p_bin <- as.factor(dat$zinc.supp_Mat_EAR.p_bin)
var_lab(dat$zinc.supp_Mat_EAR.p_bin) = "Maternal zinc supplement use in early pregnancy"
str(dat$zinc.supp_Mat_EAR.p_bin)
table(dat$zinc.supp_Mat_EAR.p_bin, useNA = "always")

################################################################################
dat <- dat %>%
  mutate(
    multivit.supp_Mat_EAR.p_bin = case_when(
      vitnotpr_epi == 0 & vitpresc_epi == 0 ~ 0,
      vitnotpr_epi == 1 | vitpresc_epi == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$multivit.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$multivit.supp_Mat_EAR.p_bin <- as.factor(dat$multivit.supp_Mat_EAR.p_bin)
var_lab(dat$multivit.supp_Mat_EAR.p_bin) = "Maternal prenatal multivitamin supplement use in early pregnancy"

str(dat$multivit.supp_Mat_EAR.p_bin)
table(dat$multivit.supp_Mat_EAR.p_bin, useNA = "always")
table(dat$vitnotpr_epi, useNA = "always")
table(dat$vitpresc_epi, useNA = "always")
################################################################################

dat <- dat %>%
  mutate(
    any.supp_Mat_EAR.p_bin = case_when(
      multivit.supp_Mat_EAR.p_bin == "No" &
        vitA.supp_Mat_EAR.p_bin == "No" &
        vitC.supp_Mat_EAR.p_bin == "No" &
        vitE.supp_Mat_EAR.p_bin == "No" &
        vitB6.supp_Mat_EAR.p_bin == "No" &
        folate.supp_Mat_EAR.p_bin == "No" &
        calcium.supp_Mat_EAR.p_bin == "No" &
        iron.supp_Mat_EAR.p_bin == "No" &
        zinc.supp_Mat_EAR.p_bin == "No" ~ 0,
      multivit.supp_Mat_EAR.p_bin == "Yes" |
        vitA.supp_Mat_EAR.p_bin == "Yes" |
        vitC.supp_Mat_EAR.p_bin == "Yes" |
        vitE.supp_Mat_EAR.p_bin == "Yes" |
        vitB6.supp_Mat_EAR.p_bin == "Yes" |
        folate.supp_Mat_EAR.p_bin == "Yes" |
        calcium.supp_Mat_EAR.p_bin == "Yes" |
        iron.supp_Mat_EAR.p_bin == "Yes" |
        zinc.supp_Mat_EAR.p_bin == "Yes" ~ 1,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$any.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)
dat$any.supp_Mat_EAR.p_bin <- as.factor(dat$any.supp_Mat_EAR.p_bin)
var_lab(dat$any.supp_Mat_EAR.p_bin) = "Maternal any dietary supplement use in early pregnancy"

str(dat$any.supp_Mat_EAR.p_bin)
table(dat$any.supp_Mat_EAR.p_bin, useNA = "always")

################################################################################
## Iron-containing supplements (iron + prenatal multivitamins) - !!! Used for iron supplementation-stratification analysis (both supplements contain iron) !!!
dat <- dat %>%
  mutate(
    iron.multivit.supp_Mat_EAR.p_bin = case_when(
      iron.supp_Mat_EAR.p_bin == "No" &
        multivit.supp_Mat_EAR.p_bin == "No" ~ 0,
      iron.supp_Mat_EAR.p_bin == "Yes" |
        multivit.supp_Mat_EAR.p_bin == "Yes" ~ 1,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$iron.multivit.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$iron.multivit.supp_Mat_EAR.p_bin <-
  as.factor(dat$iron.multivit.supp_Mat_EAR.p_bin)

var_lab(dat$iron.multivit.supp_Mat_EAR.p_bin) = "Maternal iron + prenatal multivitamins supplement use in early pregnancy (binary)"

str(dat$iron.multivit.supp_Mat_EAR.p_bin)
table(dat$iron.multivit.supp_Mat_EAR.p_bin, useNA = "always")
################################################################################
## Any supplements (excluding iron + prenatal multivitamins) - !!! Used for iron supplementation-stratification analysis (as a covariate) !!!
dat <- dat %>%
  mutate(
    NON.iron.multivit.supp_Mat_EAR.p_bin = case_when(
      vitA.supp_Mat_EAR.p_bin == "No" &
        vitC.supp_Mat_EAR.p_bin == "No" &
        vitE.supp_Mat_EAR.p_bin == "No" &
        vitB6.supp_Mat_EAR.p_bin == "No" &
        folate.supp_Mat_EAR.p_bin == "No" &
        calcium.supp_Mat_EAR.p_bin == "No" &
        zinc.supp_Mat_EAR.p_bin == "No" ~ 0,
      vitA.supp_Mat_EAR.p_bin == "Yes" |
        vitC.supp_Mat_EAR.p_bin == "Yes" |
        vitE.supp_Mat_EAR.p_bin == "Yes" |
        vitB6.supp_Mat_EAR.p_bin == "Yes" |
        folate.supp_Mat_EAR.p_bin == "Yes" |
        calcium.supp_Mat_EAR.p_bin == "Yes" |
        zinc.supp_Mat_EAR.p_bin == "Yes" ~ 1,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$NON.iron.multivit.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.iron.multivit.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.iron.multivit.supp_Mat_EAR.p_bin)

var_lab(dat$NON.iron.multivit.supp_Mat_EAR.p_bin) = "Maternal any non-iron/non-prenatal multivitamins supplement use in early pregnancy (binary)"

str(dat$NON.iron.multivit.supp_Mat_EAR.p_bin)
table(dat$NON.iron.multivit.supp_Mat_EAR.p_bin, useNA = "always")
################################################################################

# Self-defined vegetarianism

## Binary
dat$self.VegDiet_Mat_EAR.p_bin <-
  dat$vegdiet_epi

val_lab(dat$self.VegDiet_Mat_EAR.p_bin) = c("Non-vegetarian" = 0, "Vegetarian" = 1)

dat$self.VegDiet_Mat_EAR.p_bin <-
  as.factor(dat$self.VegDiet_Mat_EAR.p_bin)

var_lab(dat$self.VegDiet_Mat_EAR.p_bin) = "Maternal self-defined vegetarianism in early pregnancy (binary)"

str(dat$self.VegDiet_Mat_EAR.p_bin)
table(dat$self.VegDiet_Mat_EAR.p_bin, useNA = "always")

## 4 categories - !!! NOT USED, too many NAs !!!
dat$redmeat_epi
dat$fish_epi
dat$poultry_epi
dat$eggs_epi
dat$dairy_epi
sum(is.na(dat$redmeat_epi))  # Missing: 1748

# Total energy intake
dat$energy_Mat_EAR.p_con <-
  dat$calor_f1

dat$energy_Mat_EAR.p_con <-
  as.numeric(dat$energy_Mat_EAR.p_con)

var_lab(dat$energy_Mat_EAR.p_con) = "Maternal total energy intake (kcal/day) in early pregnancy"

str(dat$energy_Mat_EAR.p_con)
summary(dat$energy_Mat_EAR.p_con)

################################################################################
# Nausea or vomiting
dat$nausea_epi
dat$vomit_epi

dat$nausea.vomit_Mat_EAR.p_bin <- NA
dat$nausea.vomit_Mat_EAR.p_bin[dat$nausea_epi == 2] <-
  0
dat$nausea.vomit_Mat_EAR.p_bin[dat$nausea_epi == 1 |
                                 dat$vomit_epi > 1] <-
  1

val_lab(dat$nausea.vomit_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$nausea.vomit_Mat_EAR.p_bin <- as.factor(dat$nausea.vomit_Mat_EAR.p_bin)

var_lab(dat$nausea.vomit_Mat_EAR.p_bin) = "Maternal nausea or vomiting in early pregnancy"

str(dat$nausea.vomit_Mat_EAR.p_bin)
table(dat$nausea.vomit_Mat_EAR.p_bin, useNA = "always")
################################################################################

#------------------------------------------------------------------------------#
#                                Mid-Pregnancy                                 #----
#------------------------------------------------------------------------------#

# Smoking
dat$smoking_Mat_MID.p_bin <-
  dat$smk3mos_mpq

val_lab(dat$smoking_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)

dat$smoking_Mat_MID.p_bin <-
  as.factor(dat$smoking_Mat_MID.p_bin)

var_lab(dat$smoking_Mat_MID.p_bin) = "Maternal smoking in mid-pregnancy"

str(dat$smoking_Mat_MID.p_bin)
table(dat$smoking_Mat_MID.p_bin, useNA = "always")

# Alcohol drinking (≥1 portion (glass)/week defined as "Yes")
dat$alcohol_Mat_MID.p_bin <- NA
dat$alcohol_Mat_MID.p_bin[dat$alc_f2d *
                            7 < 1] <- 0
dat$alcohol_Mat_MID.p_bin[dat$alc_f2d *
                            7 >= 1] <- 1

val_lab(dat$alcohol_Mat_MID.p_bin) = c("No (<1 glass/week)" = 0,
                                       "Yes (≥1 glass/week)" = 1)

dat$alcohol_Mat_MID.p_bin <- as.factor(dat$alcohol_Mat_MID.p_bin)

var_lab(dat$alcohol_Mat_MID.p_bin) = "Maternal alcohol drinking in mid-pregnancy (binary)"

str(dat$alcohol_Mat_MID.p_bin)
table(dat$alcohol_Mat_MID.p_bin, useNA = "always")

dat$alcohol_Mat_MID.p_con <-
  dat$alc_f2d

dat$alcohol_Mat_MID.p_con <-
  as.numeric(dat$alcohol_Mat_MID.p_con)

var_lab(dat$alcohol_Mat_MID.p_con) = "Maternal alcohol consumption in mid-pregnancy (servings/day)"

str(dat$alcohol_Mat_MID.p_con)
summary(dat$alcohol_Mat_MID.p_con)

# Physical activity
dat$phys.act_Mat_MID.p_bin <- NA
dat$phys.act_Mat_MID.p_bin[dat$totact_preg_d < 2.5] <-
  0
dat$phys.act_Mat_MID.p_bin[dat$totact_preg_d >= 2.5] <-
  1

val_lab(dat$phys.act_Mat_MID.p_bin) = c(
  "Sedentary (<2.5 hours/week)" = 0,
  "Not sedentary (≥2.5 hours/week)" = 1
)

dat$phys.act_Mat_MID.p_bin <-
  as.factor(dat$phys.act_Mat_MID.p_bin)

var_lab(dat$phys.act_Mat_MID.p_bin) = "Maternal physical activity in mid-pregnancy (binary)"

str(dat$phys.act_Mat_MID.p_bin)
table(dat$phys.act_Mat_MID.p_bin, useNA = "always")

dat$phys.act_Mat_MID.p_con <-
  dat$totact_preg_d

dat$phys.act_Mat_MID.p_con <-
  as.numeric(dat$phys.act_Mat_MID.p_con)

var_lab(dat$phys.act_Mat_MID.p_con) = "Maternal physical activity in mid-pregnancy (hours/week)"

str(dat$phys.act_Mat_MID.p_con)
summary(dat$phys.act_Mat_MID.p_con)

# Dietary supplement use
dat$vitA.supp_Mat_MID.p_bin <-
  dat$vitAfreq_f2
dat$vitA.supp_Mat_MID.p_bin[dat$vitA.supp_Mat_MID.p_bin >
                              0] <- 1
val_lab(dat$vitA.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$vitA.supp_Mat_MID.p_bin <- as.factor(dat$vitA.supp_Mat_MID.p_bin)
var_lab(dat$vitA.supp_Mat_MID.p_bin) = "Maternal vitamin A supplement use in mid-pregnancy"
str(dat$vitA.supp_Mat_MID.p_bin)
table(dat$vitA.supp_Mat_MID.p_bin, useNA = "always")

dat$vitC.supp_Mat_MID.p_bin <-
  dat$vitCfreq_f2
dat$vitC.supp_Mat_MID.p_bin[dat$vitC.supp_Mat_MID.p_bin >
                              0] <- 1
val_lab(dat$vitC.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$vitC.supp_Mat_MID.p_bin <- as.factor(dat$vitC.supp_Mat_MID.p_bin)
var_lab(dat$vitC.supp_Mat_MID.p_bin) = "Maternal vitamin C supplement use in mid-pregnancy"
str(dat$vitC.supp_Mat_MID.p_bin)
table(dat$vitC.supp_Mat_MID.p_bin, useNA = "always")

dat$vitE.supp_Mat_MID.p_bin <-
  dat$vitEfreq_f2
dat$vitE.supp_Mat_MID.p_bin[dat$vitE.supp_Mat_MID.p_bin >
                              0] <- 1
val_lab(dat$vitE.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$vitE.supp_Mat_MID.p_bin <- as.factor(dat$vitE.supp_Mat_MID.p_bin)
var_lab(dat$vitE.supp_Mat_MID.p_bin) = "Maternal vitamin E supplement use in mid-pregnancy"
str(dat$vitE.supp_Mat_MID.p_bin)
table(dat$vitE.supp_Mat_MID.p_bin, useNA = "always")

dat$vitB6.supp_Mat_MID.p_bin <-
  dat$vitB6freq_f2
dat$vitB6.supp_Mat_MID.p_bin[dat$vitB6.supp_Mat_MID.p_bin >
                               0] <- 1
val_lab(dat$vitB6.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$vitB6.supp_Mat_MID.p_bin <- as.factor(dat$vitB6.supp_Mat_MID.p_bin)
var_lab(dat$vitB6.supp_Mat_MID.p_bin) = "Maternal vitamin B6 supplement use in mid-pregnancy"
str(dat$vitB6.supp_Mat_MID.p_bin)
table(dat$vitB6.supp_Mat_MID.p_bin, useNA = "always")

dat$folate.supp_Mat_MID.p_bin <-
  dat$folatefreq_f2
dat$folate.supp_Mat_MID.p_bin[dat$folate.supp_Mat_MID.p_bin >
                                0] <- 1
val_lab(dat$folate.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$folate.supp_Mat_MID.p_bin <- as.factor(dat$folate.supp_Mat_MID.p_bin)
var_lab(dat$folate.supp_Mat_MID.p_bin) = "Maternal folate supplement use in mid-pregnancy"
str(dat$folate.supp_Mat_MID.p_bin)
table(dat$folate.supp_Mat_MID.p_bin, useNA = "always")

dat$calcium.supp_Mat_MID.p_bin <-
  dat$calciumfreq_f2
dat$calcium.supp_Mat_MID.p_bin[dat$calcium.supp_Mat_MID.p_bin >
                                 0] <- 1
val_lab(dat$calcium.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$calcium.supp_Mat_MID.p_bin <- as.factor(dat$calcium.supp_Mat_MID.p_bin)
var_lab(dat$calcium.supp_Mat_MID.p_bin) = "Maternal calcium supplement use in mid-pregnancy"
str(dat$calcium.supp_Mat_MID.p_bin)
table(dat$calcium.supp_Mat_MID.p_bin, useNA = "always")

dat$iron.supp_Mat_MID.p_bin <-
  dat$ironfreq_f2
dat$iron.supp_Mat_MID.p_bin[dat$iron.supp_Mat_MID.p_bin >
                              0] <- 1
val_lab(dat$iron.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$iron.supp_Mat_MID.p_bin <- as.factor(dat$iron.supp_Mat_MID.p_bin)
var_lab(dat$iron.supp_Mat_MID.p_bin) = "Maternal iron supplement use in mid-pregnancy"
str(dat$iron.supp_Mat_MID.p_bin)
table(dat$iron.supp_Mat_MID.p_bin, useNA = "always")

dat$zinc.supp_Mat_MID.p_bin <-
  dat$zincfreq_f2
dat$zinc.supp_Mat_MID.p_bin[dat$zinc.supp_Mat_MID.p_bin >
                              0] <- 1
val_lab(dat$zinc.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$zinc.supp_Mat_MID.p_bin <- as.factor(dat$zinc.supp_Mat_MID.p_bin)
var_lab(dat$zinc.supp_Mat_MID.p_bin) = "Maternal zinc supplement use in mid-pregnancy"
str(dat$zinc.supp_Mat_MID.p_bin)
table(dat$zinc.supp_Mat_MID.p_bin, useNA = "always")

################################################################################
dat <- dat %>%
  mutate(
    multivit.supp_Mat_MID.p_bin = case_when(
      prenatalfreq_f2 == 0 & othermultfreq_f2 == 0 ~ 0,
      prenatalfreq_f2 > 0 | othermultfreq_f2 > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$multivit.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$multivit.supp_Mat_MID.p_bin <- as.factor(dat$multivit.supp_Mat_MID.p_bin)
var_lab(dat$multivit.supp_Mat_MID.p_bin) = "Maternal prenatal multivitamin supplement use in mid-pregnancy"

str(dat$multivit.supp_Mat_MID.p_bin)
table(dat$multivit.supp_Mat_MID.p_bin, useNA = "always")
table(dat$prenatalfreq_f2, useNA = "always")
table(dat$othermultfreq_f2, useNA = "always")
################################################################################

dat <- dat %>%
  mutate(
    any.supp_Mat_MID.p_bin = case_when(
      multivit.supp_Mat_MID.p_bin == "No" &
        vitA.supp_Mat_MID.p_bin == "No" &
        vitC.supp_Mat_MID.p_bin == "No" &
        vitE.supp_Mat_MID.p_bin == "No" &
        vitB6.supp_Mat_MID.p_bin == "No" &
        folate.supp_Mat_MID.p_bin == "No" &
        calcium.supp_Mat_MID.p_bin == "No" &
        iron.supp_Mat_MID.p_bin == "No" &
        zinc.supp_Mat_MID.p_bin == "No" ~ 0,
      multivit.supp_Mat_MID.p_bin == "Yes" |
        vitA.supp_Mat_MID.p_bin == "Yes" |
        vitC.supp_Mat_MID.p_bin == "Yes" |
        vitE.supp_Mat_MID.p_bin == "Yes" |
        vitB6.supp_Mat_MID.p_bin == "Yes" |
        folate.supp_Mat_MID.p_bin == "Yes" |
        calcium.supp_Mat_MID.p_bin == "Yes" |
        iron.supp_Mat_MID.p_bin == "Yes" |
        zinc.supp_Mat_MID.p_bin == "Yes" ~ 1,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$any.supp_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)
dat$any.supp_Mat_MID.p_bin <- as.factor(dat$any.supp_Mat_MID.p_bin)
var_lab(dat$any.supp_Mat_MID.p_bin) = "Maternal any dietary supplement use in mid-pregnancy"

str(dat$any.supp_Mat_MID.p_bin)
table(dat$any.supp_Mat_MID.p_bin, useNA = "always")

################################################################################

# Self-defined vegetarianism

## Binary
dat$self.VegDiet_Mat_MID.p_bin <-
  dat$VEGDIET_MPI

val_lab(dat$self.VegDiet_Mat_MID.p_bin) = c("Non-vegetarian" = 0, "Vegetarian" = 1)

dat$self.VegDiet_Mat_MID.p_bin <-
  as.factor(dat$self.VegDiet_Mat_MID.p_bin)

var_lab(dat$self.VegDiet_Mat_MID.p_bin) = "Maternal self-defined vegetarianism in mid-pregnancy (binary)"

str(dat$self.VegDiet_Mat_MID.p_bin)
table(dat$self.VegDiet_Mat_MID.p_bin, useNA = "always")

## 4 categories - !!! NOT USED, too many NAs !!!
dat$REDMEAT_MPI
dat$FISH_MPI
dat$POULTRY_MPI
dat$EGGS_MPI
dat$DAIRY_MPI
sum(is.na(dat$REDMEAT_MPI))  # Missing: 1777

# Total energy intake
dat$energy_Mat_MID.p_con <-
  dat$calor_f2

dat$energy_Mat_MID.p_con <-
  as.numeric(dat$energy_Mat_MID.p_con)

var_lab(dat$energy_Mat_MID.p_con) = "Maternal total energy intake (kcal/day) in mid-pregnancy"

str(dat$energy_Mat_MID.p_con)
summary(dat$energy_Mat_MID.p_con)

################################################################################
# Nausea or vomiting
dat$NAUSEA_MPI
dat$VOMIT_MPI

dat$nausea.vomit_Mat_MID.p_bin <- NA
dat$nausea.vomit_Mat_MID.p_bin[dat$NAUSEA_MPI == 2] <-
  0
dat$nausea.vomit_Mat_MID.p_bin[dat$NAUSEA_MPI == 1 |
                                 dat$VOMIT_MPI > 1] <-
  1

val_lab(dat$nausea.vomit_Mat_MID.p_bin) = c("No" = 0, "Yes" = 1)

dat$nausea.vomit_Mat_MID.p_bin <- as.factor(dat$nausea.vomit_Mat_MID.p_bin)

var_lab(dat$nausea.vomit_Mat_MID.p_bin) = "Maternal nausea or vomiting in mid-pregnancy"

str(dat$nausea.vomit_Mat_MID.p_bin)
table(dat$nausea.vomit_Mat_MID.p_bin, useNA = "always")
################################################################################

#------------------------------------------------------------------------------#
#                         From Early to Mid-Pregnancy                          #----
#------------------------------------------------------------------------------#

# Total energy intake during pregnancy
dat$energy_Mat_DUR.p_con <-
  rowMeans(dat[, c("energy_Mat_EAR.p_con", "energy_Mat_MID.p_con")], na.rm = T)

dat$energy_Mat_DUR.p_con <-
  as.numeric(dat$energy_Mat_DUR.p_con)

var_lab(dat$energy_Mat_DUR.p_con) = "Maternal total energy intake (kcal/day) during pregnancy"

str(dat$energy_Mat_DUR.p_con)
summary(dat$energy_Mat_DUR.p_con)

################################################################################
# !!! Diet-based + self-defined vegetarianism !!!
dat <- dat %>% mutate(
  VegDiet_bin_FFQ_self = case_when(
    VegDiet_bin == "Non-vegetarian" &
      (
        self.VegDiet_Mat_EAR.p_bin == "Non-vegetarian" |
          self.VegDiet_Mat_MID.p_bin == "Non-vegetarian"
      ) ~ 0,
    VegDiet_bin == "Vegetarian" &
      (
        self.VegDiet_Mat_EAR.p_bin == "Vegetarian" |
          self.VegDiet_Mat_MID.p_bin == "Vegetarian"
      ) ~ 1,
    TRUE ~ NA_real_
  )
)

val_lab(dat$VegDiet_bin_FFQ_self) = c("Non-vegetarian" = 0, "Vegetarian" = 1)

dat$VegDiet_bin_FFQ_self <-
  as.factor(dat$VegDiet_bin_FFQ_self)
var_lab(dat$VegDiet_bin_FFQ_self) <- "Diet-based + self-defined vegetarianism (binary) during pregnancy"

table(dat$VegDiet_bin_FFQ_self, useNA = "always")
################################################################################

#------------------------------------------------------------------------------#
#                                Post-Pregnancy                                #----
#------------------------------------------------------------------------------#

# Offspring sex - Collected after delivery
dat$sex_Chi_bin <- dat$female_d

val_lab(dat$sex_Chi_bin) = c("Male" = 0, "Female" = 1)

dat$sex_Chi_bin <- as.factor(dat$sex_Chi_bin)

var_lab(dat$sex_Chi_bin) = "Offspring sex"

str(dat$sex_Chi_bin)
table(dat$sex_Chi_bin, useNA = "always")

#------------------------------------------------------------------------------#
#                                 Check & Save                                 #----
#------------------------------------------------------------------------------#

head(dat)
dim(dat)  # 1872  586

saveRDS(dat, "data/Viva/dat_exp_cov.rds")

################################################################################
