################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 20-Jan-2025
# This script is to perform paternal negative control analysis (with imputed data) for vegetarian diets in ALSPAC.
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
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat_1))]
ALSPAC_primary_bin  # 13 primary (binary) outcomes available in ALSPAC

ALSPAC_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat_1))]
ALSPAC_secondary_bin  # 8 secondary binary outcomes available in ALSPAC

ALSPAC_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat_1))]
ALSPAC_secondary_con  # 4 secondary continuous outcomes available in ALSPAC

ALSPAC_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat_1))]
ALSPAC_secondary_cat  # 1 (primary) ordinal/categorical outcome available in ALSPAC

## Group outcome variables
ALSPAC_out_bin <- c(ALSPAC_primary_bin, ALSPAC_secondary_bin)
ALSPAC_out_con <- ALSPAC_secondary_con
ALSPAC_out_cat <- ALSPAC_secondary_cat

################################################################################
# Modelling
## Model 1 (maternal model): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex
################################################################################

# Create a function to extract beta, SE, and p-value (for both linear and logistic regression) - !!! Specifically for binary exposures !!!
IMP_extract_beta.SE_bin <- function(mymodel) {
  exp <-
    as.data.frame(c("Pesco-/full vegetarian"))
  colnames(exp) <- "Exposure"
  
  b <- summary(mymodel)[2, 2]
  se <- summary(mymodel)[2, 3]
  pval <- summary(mymodel)[2, 6]
  
  x <- cbind(b, se, pval, exp)
  
  return(x)
}

################################################################################

# Vegetarian diet and binary outcomes - Logistic regression

## Create empty output tables
obs.res_VegDiet_bin <-
  c() # For forestplots and meta-analysis

################################################################################

## Loop for each binary outcome
for (myoutcome in ALSPAC_out_bin) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### Model 1 (maternal model): VegDiet_bin + age, ethnicity, education, IMD, parity, BMI, smoking, alcohol, offspring sex
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["IMD_Fam_cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE_bin(mod_1_pooled)
  
  outcome_col <- as.data.frame(c(outcome))
  colnames(outcome_col) <- "Outcome"
  model_col <-
    as.data.frame(c("Maternal Model 1"))
  colnames(model_col) <- "Model"
  y <- cbind(outcome_col, model_col, rbind(y_1))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_bin <- rbind(obs.res_VegDiet_bin, y)
}

obs.res_VegDiet_bin

################################################################################

## View and save results
obs.res_VegDiet_bin <- as.data.frame(obs.res_VegDiet_bin)
obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 21 outcomes * 1 category * 1 model = 21 obs
write.xlsx(
  obs.res_VegDiet_bin,
  "results/ALSPAC/IMP_PNC.FULL_obs.res_VegDiet_bin.xlsx",
  overwrite = T
)

################################################################################

# Vegetarian diet and continuous outcomes - Linear regression

## Create empty output tables
obs.res_VegDiet_con <-
  c() # For forestplots and meta-analysis

################################################################################

## Loop for each continuous outcome
for (myoutcome in ALSPAC_out_con) {
  #### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### Model 1 (maternal model): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["IMD_Fam_cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE_bin(mod_1_pooled)
  
  ### Regression results for one outcome
  outcome_col <- as.data.frame(c(outcome))
  colnames(outcome_col) <- "Outcome"
  model_col <-
    as.data.frame(c("Maternal Model 1"))
  colnames(model_col) <- "Model"
  y <- cbind(outcome_col, model_col, rbind(y_1))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_con <- rbind(obs.res_VegDiet_con, y)
}

obs.res_VegDiet_con

################################################################################

## View and save results
obs.res_VegDiet_con <- as.data.frame(obs.res_VegDiet_con)
obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 4 outcomes * 1 category * 1 model = 4 obs.
write.xlsx(
  obs.res_VegDiet_con,
  "results/ALSPAC/IMP_PNC.FULL_obs.res_VegDiet_con.xlsx",
  overwrite = T
)
