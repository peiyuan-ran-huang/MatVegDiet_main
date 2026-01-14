################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 12-Jun-2025
# This script is to perform paternal negative control analysis on breastfeeding duration (with imputed data) for vegetarian diets in ALSPAC.

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
  VGAM,
  mice
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load imputed data
load("data/ALSPAC/dat_exp_cov_out_pat_IMP_PNC.RData")

## Create a list of imputed datasets
dat <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat[[i]] <- complete_dat
}

## Check the first imputed dataset
dat_1 <- dat[[1]]
head(dat_1)
dim(dat_1)  # 8557  XXX
for (varname in colnames(dat_1)) {
  print(varname)
  print(sum(is.na(dat_1[[varname]])))
}

################################################################################
# Modelling
## Model 1 (maternal model): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex
## Model 2 (maternal model adjusted for paternal exposure): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex + VietDiet_3cat_Pat
## Model 3 (paternal model): VegDiet_bin_Pat + age, ethnicity, education, IMD, number of children, BMI, smoking, alcohol, offspring sex
## Model 4 (paternal model adjusted for maternal exposure): VegDiet_bin_Pat + age, ethnicity, education, IMD, number of children, BMI, smoking, alcohol, offspring sex + VietDiet_3cat
################################################################################

# Create a function to extract regression results with SE (for ORDINAL logistic regression) - !!! Specifically for binary exposures
IMP_extract_ord.beta.SE_bin <- function(mymodel) {
  exp <-
    as.data.frame(c("Pesco-/full vegetarian"))
  colnames(exp) <- "Exposure"
  
  b <- summary(mymodel)[4, 2]
  se <- summary(mymodel)[4, 3]
  pval <- summary(mymodel)[4, 6]
  
  x <- cbind(b, se, pval, exp)
  
  return(x)
}

# Create a function to extract ORDINAL logistic regression results - !!! Specifically for binary exposures
IMP_extract_ord.log.res_bin <- function(mymodel) {
  ## Non-vegetarian (ref)
  Exposure_1 <- "Non-vegetarian (ref)"
  OR_1 <- format(round(1.00, digits = 2), nsmall = 2)
  CI_1 <- "-"
  pval_1 <- "-"
  x_1 <- cbind(Exposure_1, OR_1, CI_1, pval_1)
  ##############################################################################
  ## Pesco-vegetarian
  Exposure_2 <- "Pesco-/full vegetarian"
  OR_2 <-
    format(round(exp(summary(mymodel)[4, 2]), digits = 2), nsmall = 2)
  CI_2 <- paste0(format(round(exp(
    summary(mymodel, conf.int = T)[4, 7]
  ), digits = 2), nsmall = 2), ", ", format(round(exp(
    summary(mymodel, conf.int = T)[4, 8]
  ), digits = 2), nsmall = 2))
  pval_2 <-
    style_pvalue(summary(mymodel)[4, 6], digits = 3)
  x_2 <- cbind(Exposure_2, OR_2, CI_2, pval_2)
  ##############################################################################
  
  x <- rbind(x_1, x_2)
  colnames(x) <- c("Exposure", "OR", "CI", "pval")
  
  return(x)
}

################################################################################
################################################################################

# Vegetarian diet and breastfeeding duration (3 categories) - Ordinal logistic regression

## 4 models for vegetarian diet
### Model 1 (maternal model): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex
mod_1 <- list()
for (i in 1:dat_imp$m) {
  mod_1[[i]] <-
    clm(dat[[i]][["bf_dur_4c"]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["IMD_Fam_cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
}
mod_1_pooled <- pool(mod_1)
x_1 <- IMP_extract_ord.log.res_bin(mod_1_pooled)
y_1 <- IMP_extract_ord.beta.SE_bin(mod_1_pooled)

### Model 2 (maternal model adjusted for paternal exposure): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex + VietDiet_3cat_Pat
mod_2 <- list()
for (i in 1:dat_imp$m) {
  mod_2[[i]] <-
    clm(
      dat[[i]][["bf_dur_4c"]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["IMD_Fam_cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["VegDiet_bin_Pat"]] + dat[[i]][["age_Pat_con"]] + dat[[i]][["ethnic_Pat_bin"]] + dat[[i]][["edu_Pat_3cat"]] + dat[[i]][["parity_Pat_bin"]] + dat[[i]][["BMI_Pat_EAR.p_con"]] + dat[[i]][["smoking_Pat_EAR.p_bin"]] + dat[[i]][["alcohol_Pat_EAR.p_bin"]]
    )
}
mod_2_pooled <- pool(mod_2)
x_2 <- IMP_extract_ord.log.res_bin(mod_2_pooled)
y_2 <- IMP_extract_ord.beta.SE_bin(mod_2_pooled)

### Model 3 (paternal model): VegDiet_bin_Pat + age, ethnicity, education, IMD, number of children, BMI, smoking, alcohol, offspring sex
mod_3 <- list()
for (i in 1:dat_imp$m) {
  mod_3[[i]] <-
    clm(dat[[i]][["bf_dur_4c"]] ~ dat[[i]][["VegDiet_bin_Pat"]] + dat[[i]][["age_Pat_con"]] + dat[[i]][["ethnic_Pat_bin"]] + dat[[i]][["edu_Pat_3cat"]]  + dat[[i]][["IMD_Fam_cat"]] + dat[[i]][["parity_Pat_bin"]] + dat[[i]][["BMI_Pat_EAR.p_con"]] + dat[[i]][["smoking_Pat_EAR.p_bin"]] + dat[[i]][["alcohol_Pat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
}
mod_3_pooled <- pool(mod_3)
x_3 <- IMP_extract_ord.log.res_bin(mod_3_pooled)
y_3 <- IMP_extract_ord.beta.SE_bin(mod_3_pooled)

### Model 4 (paternal model adjusted for maternal exposure): VegDiet_bin_Pat + age, ethnicity, education, IMD, number of children, BMI, smoking, alcohol, offspring sex + VietDiet_3cat
mod_4 <- list()
for (i in 1:dat_imp$m) {
  mod_4[[i]] <-
    clm(
      dat[[i]][["bf_dur_4c"]] ~ dat[[i]][["VegDiet_bin_Pat"]] + dat[[i]][["age_Pat_con"]] + dat[[i]][["ethnic_Pat_bin"]] + dat[[i]][["edu_Pat_3cat"]]  + dat[[i]][["IMD_Fam_cat"]] + dat[[i]][["parity_Pat_bin"]] + dat[[i]][["BMI_Pat_EAR.p_con"]] + dat[[i]][["smoking_Pat_EAR.p_bin"]] + dat[[i]][["alcohol_Pat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]]
    )
}
mod_4_pooled <- pool(mod_4)
x_4 <- IMP_extract_ord.log.res_bin(mod_4_pooled)
y_4 <- IMP_extract_ord.beta.SE_bin(mod_4_pooled)

################################################################################
################################################################################

## Combine regression results
outcome <- var_lab(dat_1$bf_dur_4c)

outcome_x <- as.data.frame(rep(outcome, 2))
colnames(outcome_x) <- "Outcome"
N_x_Mat <- as.data.frame(c(nrow(dat_1[dat_1$VegDiet_bin == 0 &
                                        !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$VegDiet_bin == 1 &
                                                                                       !is.na(dat_1[["bf_dur_4c"]]), ])))
colnames(N_x_Mat) <- "N_Mat"

N_x_Pat <- as.data.frame(c(nrow(dat_1[dat_1$VegDiet_bin_Pat == 0 &
                                        !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$VegDiet_bin_Pat == 1 &
                                                                                       !is.na(dat_1[["bf_dur_4c"]]), ])))
colnames(N_x_Pat) <- "N_Pat"
x <- cbind(outcome_x, N_x_Mat, x_1, x_2, N_x_Pat, x_3, x_4)  # Columns: Outcome name | N | Model 1 | Model 2 | N | Model 3 | Model 4; rows: Non-vegetarian | Pesco-/full vegetarian

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
obs.tbl_VegDiet_ord[, c(7, 12, 16)] <-
  " "  # For convenience when making tables
obs.tbl_VegDiet_ord <-
  obs.tbl_VegDiet_ord[, c(1, 3, 2, 4:10, 12, 11, 13:ncol(obs.tbl_VegDiet_ord))]  # For convenience when making tables
obs.tbl_VegDiet_ord
dim(obs.tbl_VegDiet_ord)  # 1 outcome for 2 categories in 3 models
write.xlsx(
  obs.tbl_VegDiet_ord,
  "results/ALSPAC/IMP_PNC_BF_obs.tbl_VegDiet_ord.xlsx",
  overwrite = T
)
obs.tbl_VegDiet_ord <-
  read.xlsx("results/ALSPAC/IMP_PNC_BF_obs.tbl_VegDiet_ord.xlsx")

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
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 3]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_ord$Model[i] == "Paternal Model 1") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 12]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 12]
  } else if (obs.res_VegDiet_ord$Model[i] == "Paternal Model 2") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 12]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 12]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 1 outcome * 1 category * 4 models = 4 obs
write.xlsx(
  obs.res_VegDiet_ord,
  "results/ALSPAC/IMP_PNC_BF_obs.res_VegDiet_ord.xlsx",
  overwrite = T
)
obs.res_VegDiet_ord <-
  read.xlsx("results/ALSPAC/IMP_PNC_BF_obs.res_VegDiet_ord.xlsx")
