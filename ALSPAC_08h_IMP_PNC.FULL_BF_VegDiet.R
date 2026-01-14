################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 20-Jan-2025
# This script is to perform paternal negative control analysis on breastfeeding duration (with imputed data) for vegetarian diets in ALSPAC.
## Maternal Model 1 in FULL samples (for interpretation)

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
load("data/ALSPAC/dat_exp_cov_out_pat_IMP_main.RData")

## Create a list of imputed datasets
dat <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat[[i]] <- complete_dat
}

## Check the first imputed dataset
dat_1 <- dat[[1]]
head(dat_1)
dim(dat_1)  # 11693  XXX
for (varname in colnames(dat_1)) {
  print(varname)
  print(sum(is.na(dat_1[[varname]])))
}

################################################################################
# Modelling
## Model 1 (maternal model): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex
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

################################################################################
################################################################################

# Vegetarian diet and breastfeeding duration (3 categories) - Ordinal logistic regression

## Model 1 (maternal model): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex
mod_1 <- list()
for (i in 1:dat_imp$m) {
  mod_1[[i]] <-
    clm(dat[[i]][["bf_dur_4c"]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["IMD_Fam_cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
}
mod_1_pooled <- pool(mod_1)
y_1 <- IMP_extract_ord.beta.SE_bin(mod_1_pooled)

################################################################################
################################################################################

## Combine regression results
outcome <- var_lab(dat_1$bf_dur_4c)
outcome_col <- as.data.frame(c(outcome))
colnames(outcome_col) <- "Outcome"
model_col <-
  as.data.frame(c("Maternal Model 1"))
colnames(model_col) <- "Model"
y <- cbind(outcome_col, model_col, rbind(y_1))

################################################################################
################################################################################

## View and save results
obs.res_VegDiet_ord <- as.data.frame(y)
obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 1 outcome * 1 category * 1 model = 1 obs
write.xlsx(
  obs.res_VegDiet_ord,
  "results/ALSPAC/IMP_PNC.FULL_BF_obs.res_VegDiet_ord.xlsx",
  overwrite = T
)
