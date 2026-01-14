################################################################################
#      Maternal Vegetarian/Plant-based diet & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 12-Jun-2025
# This script is to perform paternal negative control analysis on breastfeeding duration for vegetarian diets in ALSPAC.

# Update on 12-Jun-2025: Added partners' confounders in mutually adjusted models

################################################################################

# Clear environment
rm(list = ls())

# Collect information about the current R session
sessionInfo()

# Load packages
pacman::p_load(
  tidyverse,
  openxlsx,
  haven,
  expss,
  gtsummary,
  kableExtra,
  flextable,
  readr,
  magrittr,
  ggplot2,
  hrbrthemes,
  ordinal,
  VGAM
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/ALSPAC/dat_exp_cov_out_pat.rds")
head(dat)
dim(dat)  # 11693  XXX

################################################################################

# Outcome grouping

## Load outcome lists and labels
MRPREG_outcome_labels <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

primary_bin <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Primary_bin")
primary_bin  # 13 primary (binary) outcomes

secondary_bin <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_bin")
secondary_bin  # 9 secondary binary outcomes

secondary_con <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_con")
secondary_con  # 4 secondary continuous outcomes

secondary_cat <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_cat")
secondary_cat  # 1 (secondary) ordinal/categorical outcome (bf_dur_4c as negative outcome)

## Identify available outcomes in the cohort
ALSPAC_primary_bin <-
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat))]
ALSPAC_primary_bin  # 13 primary (binary) outcomes available in ALSPAC

ALSPAC_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat))]
ALSPAC_secondary_bin  # 8 secondary binary outcomes available in ALSPAC

ALSPAC_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat))]
ALSPAC_secondary_con  # 4 secondary continuous outcomes available in ALSPAC

ALSPAC_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat))]
ALSPAC_secondary_cat  # 1 (primary) ordinal/categorical outcome available in ALSPAC

## Group outcome variables
ALSPAC_out_bin <- c(ALSPAC_primary_bin, ALSPAC_secondary_bin)
ALSPAC_out_con <- ALSPAC_secondary_con
ALSPAC_out_cat <- ALSPAC_secondary_cat

################################################################################
# Modelling
## Model 1 (maternal model): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex
## Model 2 (maternal model adjusted for paternal exposure): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex + VietDiet_3cat_Pat
## Model 3 (paternal model): VegDiet_bin_Pat + age, ethnicity, education, IMD, number of children, BMI, smoking, alcohol, offspring sex
## Model 4 (paternal model adjusted for maternal exposure): VegDiet_bin_Pat + age, ethnicity, education, IMD, number of children, BMI, smoking, alcohol, offspring sex + VietDiet_3cat
################################################################################

################################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!! Subset to those with paternal dietary data for PNC analysis !!!
dat <- subset(dat, is.na(VegDiet_bin_Pat) == F)
head(dat)
dim(dat)  # 11716 -> 8575
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
################################################################################

# Create a function to extract regression results with SE (for ORDINAL logistic regression) - !!! Specifically for binary exposures
extract_ord.beta.SE_bin <- function(mymodel) {
  exp <-
    as.data.frame(c("Pesco-/full vegetarian"))
  colnames(exp) <- "Exposure"
  
  b <- summary(mymodel)$coef[4, 1]
  se <- summary(mymodel)$coef[4, 2]
  pval <- summary(mymodel)$coef[4, 4]
  
  x <- cbind(b, se, pval, exp)
  
  return(x)
}

# Create a function to extract ORDINAL logistic regression results - !!! Specifically for binary exposures
extract_ord.log.res_bin <- function(mymodel) {
  ## Non-vegetarian (ref)
  Exposure_1 <- "Non-vegetarian (ref)"
  N_1 <- sum(mymodel$model[, 2] == "Non-vegetarian")
  OR_1 <- format(round(1.00, digits = 2), nsmall = 2)
  CI_1 <- "-"
  pval_1 <- "-"
  x_1 <- cbind(Exposure_1, N_1, OR_1, CI_1, pval_1)
  ##############################################################################
  ## Pesco-vegetarian
  Exposure_2 <- "Pesco-/full vegetarian"
  N_2 <- sum(mymodel$model[, 2] == "Vegetarian")
  OR_2 <-
    format(round(exp(summary(mymodel)$coef[4, 1]), digits = 2), nsmall = 2)
  CI_2 <-
    paste0(format(round(
      exp(summary(mymodel)$coef[4, 1] - 1.96 * summary(mymodel)$coef[4, 2]), digits = 2
    ), nsmall = 2), ", ", format(round(
      exp(summary(mymodel)$coef[4, 1] + 1.96 * summary(mymodel)$coef[4, 2]), digits = 2
    ), nsmall = 2))  # confint() too time consuming for logistic models - calculating 95% CI manually
  pval_2 <-
    style_pvalue(summary(mymodel)$coefficients[4, 4], digits = 3)
  x_2 <- cbind(Exposure_2, N_2, OR_2, CI_2, pval_2)
  ##############################################################################
  
  x <- rbind(x_1, x_2)
  colnames(x) <- c("Exposure", "N", "OR", "CI", "pval")
  
  return(x)
}

################################################################################
################################################################################

# Vegetarian diet and breastfeeding duration (3 categories) - Ordinal logistic regression

## Model 1 (maternal model): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, supplement use, offspring sex
mod_1 <-
  clm(
    bf_dur_4c ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
    data = dat
  )
x_1 <- extract_ord.log.res_bin(mod_1)
y_1 <- extract_ord.beta.SE_bin(mod_1)

## Model 2 (maternal model adjusted for paternal exposure): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, supplement use, offspring sex + VietDiet_3cat_Pat
mod_2 <-
  clm(
    bf_dur_4c ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + VegDiet_bin_Pat + age_Pat_con + ethnic_Pat_bin + edu_Pat_3cat + parity_Pat_bin + BMI_Pat_EAR.p_con + smoking_Pat_EAR.p_bin + alcohol_Pat_EAR.p_bin,
    data = dat
  )
x_2 <- extract_ord.log.res_bin(mod_2)
y_2 <- extract_ord.beta.SE_bin(mod_2)

## Model 3 (paternal model): VegDiet_bin_Pat + age, ethnicity, education, IMD, number of children, BMI, smoking, alcohol, supplement use, offspring sex
mod_3 <-
  clm(
    bf_dur_4c ~ VegDiet_bin_Pat + age_Pat_con + ethnic_Pat_bin + edu_Pat_3cat  + IMD_Fam_cat + parity_Pat_bin + BMI_Pat_EAR.p_con + smoking_Pat_EAR.p_bin + alcohol_Pat_EAR.p_bin + sex_Chi_bin,
    data = dat
  )
x_3 <- extract_ord.log.res_bin(mod_3)
y_3 <- extract_ord.beta.SE_bin(mod_3)

## Model 4 (paternal model adjusted for maternal exposure): VegDiet_bin_Pat + age, ethnicity, education, IMD, number of children, BMI, smoking, alcohol, supplement use, offspring sex + VietDiet_3cat
mod_4 <-
  clm(
    bf_dur_4c ~ VegDiet_bin_Pat + age_Pat_con + ethnic_Pat_bin + edu_Pat_3cat  + IMD_Fam_cat + parity_Pat_bin + BMI_Pat_EAR.p_con + smoking_Pat_EAR.p_bin + alcohol_Pat_EAR.p_bin + sex_Chi_bin + VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin,
    data = dat
  )
x_4 <- extract_ord.log.res_bin(mod_4)
y_4 <- extract_ord.beta.SE_bin(mod_4)

################################################################################
################################################################################

## Combine regression results
outcome <- var_lab(dat$bf_dur_4c)

outcome_x <- as.data.frame(rep(outcome, 2))
colnames(outcome_x) <- "Outcome"
x <-
  cbind(outcome_x, x_1, x_2, x_3, x_4)  # Columns: Outcome name | Model 1 | Model 2 | Model 3 | Model 4; rows: Non-vegetarian | Pesco-/full vegetarian

outcome_col <- as.data.frame(rep(outcome, 4))
colnames(outcome_col) <- "Outcome"
model_col <-
  as.data.frame(c(
    "Maternal Model 1",
    "Maternal Model 2",
    "Paternal Model 1",
    "Paternal Model 2"
  ))
colnames(model_col) <- "Model"
y <- cbind(outcome_col, model_col, rbind(y_1, y_2, y_3, y_4))

################################################################################
################################################################################

## View and save results
obs.tbl_VegDiet_ord <- as.data.frame(x)
obs.tbl_VegDiet_ord[, c(7, 12, 17)] <-
  " "  # For convenience when making tables
obs.tbl_VegDiet_ord
dim(obs.tbl_VegDiet_ord)  # 1 outcome for 2 categories in 4 models
write.xlsx(
  obs.tbl_VegDiet_ord,
  "results/ALSPAC/PNC_BF_obs.tbl_VegDiet_ord.xlsx",
  overwrite = T
)
obs.tbl_VegDiet_ord <-
  read.xlsx("results/ALSPAC/PNC_BF_obs.tbl_VegDiet_ord.xlsx")

obs.res_VegDiet_ord <- as.data.frame(y)
obs.res_VegDiet_ord$N_exp <- NA
obs.res_VegDiet_ord$N_ref <- NA
for (i in 1:nrow(obs.res_VegDiet_ord)) {
  if (obs.res_VegDiet_ord$Model[i] == "Maternal Model 1") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 3]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_ord$Model[i] == "Maternal Model 2") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 8]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 8]
  } else if (obs.res_VegDiet_ord$Model[i] == "Paternal Model 1") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 13]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 13]
  } else if (obs.res_VegDiet_ord$Model[i] == "Paternal Model 2") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 18]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 18]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 1 outcome * 1 category * 4 models = 4 obs
write.xlsx(
  obs.res_VegDiet_ord,
  "results/ALSPAC/PNC_BF_obs.res_VegDiet_ord.xlsx",
  overwrite = T
)
obs.res_VegDiet_ord <-
  read.xlsx("results/ALSPAC/PNC_BF_obs.res_VegDiet_ord.xlsx")
