################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 01-Aug-2025
# This script is to perform negative control outcome analysis (with imputed data) on breastfeeding for quintiles of plant-based diet indices (PDIs) in ALSPAC.

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
  mice
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load imputed data
load("data/ALSPAC/dat_exp_cov_out_pat_IMP_PDIs.RData")

## Create a list of imputed datasets
dat <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat[[i]] <- complete_dat
}

## Check the first imputed dataset
dat_1 <- dat[[1]]
head(dat_1)
dim(dat_1)  # 11589  XXX
for (varname in colnames(dat_1)) {
  print(varname)
  print(sum(is.na(dat_1[[varname]])))
}

################################################################################
# Modelling
## Model 1: exposure only - i.e., the unadjusted model
## Model 2: + age, ethnicity, education, IMD, parity, pre-pregnancy BMI, smoking, alcohol drinking, offspring sex - i.e., additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
## Model 3: + dietary supplementation and total energy intake - i.e., additionally adjusting for nutrition-related factors
################################################################################

# Create a function to extract regression results with SE (for ORDINAL logistic regression) - !!! Specifically for 5-categories exposures !!!
IMP_extract_ord.beta.SE_con <- function(mymodel) {
  b <- summary(mymodel)[c(4, 5, 6, 7), 2]
  se <- summary(mymodel)[c(4, 5, 6, 7), 3]
  pval <- summary(mymodel)[c(4, 5, 6, 7), 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

################################################################################
################################################################################

# PDIs and breastfeeding duration (3 categories) - Ordinal logistic regression

## PDI

### Model 1 - Univariate (unadjusted) model
mod_1 <- list()
for (i in 1:dat_imp$m) {
  mod_1[[i]] <-
    clm(bf_dur_4c ~ PDI_5Q, data = dat[[i]])
}
mod_1_pooled <- pool(mod_1)
y_1 <- IMP_extract_ord.beta.SE_con(mod_1_pooled)

### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_2 <- list()
for (i in 1:dat_imp$m) {
  mod_2[[i]] <-
    clm(
      bf_dur_4c ~ PDI_5Q + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
      data = dat[[i]]
    )
}
mod_2_pooled <- pool(mod_2)
y_2 <- IMP_extract_ord.beta.SE_con(mod_2_pooled)

### Model 3 - Additionally adjusting for nutrition-related factors (including hPDI as the indicator for the healthfulness of diet)
mod_3 <- list()
for (i in 1:dat_imp$m) {
  mod_3[[i]] <-
    clm(
      bf_dur_4c ~ PDI_5Q + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin,
      data = dat[[i]]
    )
}
mod_3_pooled <- pool(mod_3)
y_3 <- IMP_extract_ord.beta.SE_con(mod_3_pooled)

## hPDI

### Model 1 - Univariate (unadjusted) model
mod_4 <- list()
for (i in 1:dat_imp$m) {
  mod_4[[i]] <-
    clm(bf_dur_4c ~ hPDI_5Q, data = dat[[i]])
}
mod_4_pooled <- pool(mod_4)
y_4 <- IMP_extract_ord.beta.SE_con(mod_4_pooled)

### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_5 <- list()
for (i in 1:dat_imp$m) {
  mod_5[[i]] <-
    clm(
      bf_dur_4c ~ hPDI_5Q + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
      data = dat[[i]]
    )
}
mod_5_pooled <- pool(mod_5)
y_5 <- IMP_extract_ord.beta.SE_con(mod_5_pooled)

### Model 3 - Additionally adjusting for nutrition-related factors (including hPDI as the indicator for the healthfulness of diet)
mod_6 <- list()
for (i in 1:dat_imp$m) {
  mod_6[[i]] <-
    clm(
      bf_dur_4c ~ hPDI_5Q + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin,
      data = dat[[i]]
    )
}
mod_6_pooled <- pool(mod_6)
y_6 <- IMP_extract_ord.beta.SE_con(mod_6_pooled)

## uPDI

### Model 1 - Univariate (unadjusted) model
mod_7 <- list()
for (i in 1:dat_imp$m) {
  mod_7[[i]] <-
    clm(bf_dur_4c ~ uPDI_5Q, data = dat[[i]])
}
mod_7_pooled <- pool(mod_7)
y_7 <- IMP_extract_ord.beta.SE_con(mod_7_pooled)

### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_8 <- list()
for (i in 1:dat_imp$m) {
  mod_8[[i]] <-
    clm(
      bf_dur_4c ~ uPDI_5Q + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
      data = dat[[i]]
    )
}
mod_8_pooled <- pool(mod_8)
y_8 <- IMP_extract_ord.beta.SE_con(mod_8_pooled)

### Model 3 - Additionally adjusting for nutrition-related factors
mod_9 <- list()
for (i in 1:dat_imp$m) {
  mod_9[[i]] <-
    clm(
      bf_dur_4c ~ uPDI_5Q + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin,
      data = dat[[i]]
    )
}
mod_9_pooled <- pool(mod_9)
y_9 <- IMP_extract_ord.beta.SE_con(mod_9_pooled)

################################################################################
################################################################################

## Combine regression results
outcome <- var_lab(dat_1$bf_dur_4c)

outcome_col <- as.data.frame(rep(outcome, 12))
colnames(outcome_col) <- "Outcome"
quintile_col <- as.data.frame(rep(c("Q2", "Q3", "Q4", "Q5"), 3))
colnames(quintile_col) <- "Quintile"
model_col <- as.data.frame(c(rep("Model 1", 4), rep("Model 2", 4), rep("Model 3", 4)))
colnames(model_col) <- "Model"

N_exp_PDI <- as.data.frame(rep(c(
  nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[2] &
               !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[3] &
                                                              !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[4] &
                                                                                                             !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[5] &
                                                                                                                                                            !is.na(dat_1[["bf_dur_4c"]]), ])
), 3))
colnames(N_exp_PDI) <- "N_exp"
N_ref_PDI <- as.data.frame(rep(nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[1] &
                                            !is.na(dat_1[["bf_dur_4c"]]), ]), 12))
colnames(N_ref_PDI) <- "N_ref"

N_exp_hPDI <- as.data.frame(rep(c(
  nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[2] &
               !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[3] &
                                                              !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[4] &
                                                                                                             !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[5] &
                                                                                                                                                            !is.na(dat_1[["bf_dur_4c"]]), ])
), 3))
colnames(N_exp_hPDI) <- "N_exp"
N_ref_hPDI <- as.data.frame(rep(nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[1] &
                                             !is.na(dat_1[["bf_dur_4c"]]), ]), 12))
colnames(N_ref_hPDI) <- "N_ref"

N_exp_uPDI <- as.data.frame(rep(c(
  nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[2] &
               !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[3] &
                                                              !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[4] &
                                                                                                             !is.na(dat_1[["bf_dur_4c"]]), ]), nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[5] &
                                                                                                                                                            !is.na(dat_1[["bf_dur_4c"]]), ])
), 3))
colnames(N_exp_uPDI) <- "N_exp"
N_ref_uPDI <- as.data.frame(rep(nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[1] &
                                             !is.na(dat_1[["bf_dur_4c"]]), ]), 12))
colnames(N_ref_uPDI) <- "N_ref"

y_PDI <- cbind(outcome_col,
               quintile_col,
               model_col,
               N_exp_PDI,
               N_ref_PDI,
               rbind(y_1, y_2, y_3))
y_hPDI <- cbind(outcome_col,
                quintile_col,
                model_col,
                N_exp_hPDI,
                N_ref_hPDI,
                rbind(y_4, y_5, y_6))
y_uPDI <- cbind(outcome_col,
                quintile_col,
                model_col,
                N_exp_uPDI,
                N_ref_uPDI,
                rbind(y_7, y_8, y_9))

################################################################################
################################################################################

## View and save results
obs.res_PDI_ord <- as.data.frame(y_PDI)
obs.res_PDI_ord
dim(obs.res_PDI_ord)  # 1 outcome * 3 models * 4 comparisons = 12 obs
write.xlsx(
  obs.res_PDI_ord,
  "results/ALSPAC/IMP_BF_obs.res_PDI.quintile_ord.xlsx",
  overwrite = T
)
obs.res_PDI_ord <-
  read.xlsx("results/ALSPAC/IMP_BF_obs.res_PDI.quintile_ord.xlsx")

obs.res_hPDI_ord <- as.data.frame(y_hPDI)
obs.res_hPDI_ord
dim(obs.res_hPDI_ord)  # 1 outcome * 3 models * 4 comparisons = 12 obs
write.xlsx(
  obs.res_hPDI_ord,
  "results/ALSPAC/IMP_BF_obs.res_hPDI.quintile_ord.xlsx",
  overwrite = T
)
obs.res_hPDI_ord <-
  read.xlsx("results/ALSPAC/IMP_BF_obs.res_hPDI.quintile_ord.xlsx")

obs.res_uPDI_ord <- as.data.frame(y_uPDI)
obs.res_uPDI_ord
dim(obs.res_uPDI_ord)  # 1 outcome * 3 models * 4 comparisons = 12 obs
write.xlsx(
  obs.res_uPDI_ord,
  "results/ALSPAC/IMP_BF_obs.res_uPDI.quintile_ord.xlsx",
  overwrite = T
)
obs.res_uPDI_ord <-
  read.xlsx("results/ALSPAC/IMP_BF_obs.res_uPDI.quintile_ord.xlsx")

################################################################################
