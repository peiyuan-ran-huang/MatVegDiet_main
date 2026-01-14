################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 20-Jan-2025
# This script is to perform paternal negative control analysis on breastfeeding duration (with imputed data) for vegetarian diets in MoBa.
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
setwd("N:/durable/projects/Ran_MoBa_var")

################################################################################

# Load imputed data
load("dat_exp_cov_out_pat_IMP_main.RData")

## Create a list of imputed datasets
dat <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat[[i]] <- complete_dat
}

## Check the first imputed dataset
dat_1 <- dat[[1]]
head(dat_1)
dim(dat_1)  # 73868  XXX
for (varname in colnames(dat_1)) {
  print(varname)
  print(sum(is.na(dat_1[[varname]])))
}
################################################################################

# Outcome grouping

## Load outcome lists and labels
MRPREG_outcome_labels <-
  read.xlsx("MRPREG_outcome_labels.xlsx", sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

primary_bin <-
  read.xlsx("MRPREG_outcome_labels.xlsx", sheet = "Primary_bin")
primary_bin  # 13 primary (binary) outcomes

secondary_bin <-
  read.xlsx("MRPREG_outcome_labels.xlsx", sheet = "Secondary_bin")
secondary_bin  # 9 secondary binary outcomes

secondary_con <-
  read.xlsx("MRPREG_outcome_labels.xlsx", sheet = "Secondary_con")
secondary_con  # 4 secondary continuous outcomes

secondary_cat <-
  read.xlsx("MRPREG_outcome_labels.xlsx", sheet = "Secondary_cat")
secondary_cat  # 1 (secondary) ordinal/categorical outcome (bf_dur_4c as negative control outcome)

## Identify available outcomes in the cohort
MoBa_primary_bin <-
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat_1))]
MoBa_primary_bin  # 13 primary (binary) outcomes available in MoBa

MoBa_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat_1))]
MoBa_secondary_bin  # 9 secondary binary outcomes available in MoBa

MoBa_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat_1))]
MoBa_secondary_con  # 4 secondary continuous outcomes available in MoBa

MoBa_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat_1))]
MoBa_secondary_cat  # 1 (primary) ordinal/categorical outcome available in MoBa

## Group outcome variables
MoBa_out_bin <- c(MoBa_primary_bin, MoBa_secondary_bin)
MoBa_out_con <- MoBa_secondary_con
MoBa_out_cat <- MoBa_secondary_cat

################################################################################
# Modelling
## Model 1 (maternal model): VegDiet_bin + age, education, income, parity, BMI, smoking, alcohol, supplement use, offspring sex
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

## Model 1 (maternal model): VegDiet_bin + age, education, income, parity, BMI, smoking, alcohol, supplement use, offspring sex
mod_1 <- list()
for (i in 1:dat_imp$m) {
  mod_1[[i]] <-
    clm(dat[[i]][["bf_dur_4c"]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
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
  "results/IMP_PNC.FULL_BF_obs.res_VegDiet_ord.xlsx",
  overwrite = T
)
