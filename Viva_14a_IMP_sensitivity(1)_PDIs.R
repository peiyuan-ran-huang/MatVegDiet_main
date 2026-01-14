################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 03-Dec-2024
# This script is to perform sensitivity analysis (with imputed data) for plant-based diet indices (PDIs) in Project Viva.
## Sensitivity Analysis 1 - Dietary exposure in early pregnancy

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
load("data/Viva/dat_exp_cov_out_IMP_PDIs.RData")

################################################################################
# Sensitivity Analysis 1 - Dietary exposure in early pregnancy
################################################################################

# Prepare data

## Create a list of imputed datasets
dat <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat[[i]] <- complete_dat
  dat[[i]] <- subset(dat[[i]], !is.na(dat[[i]]$PDI_1_z))
}

## Check the first imputed dataset
dat_1 <- dat[[1]]
head(dat_1)
dim(dat_1)  # 1872 -> 1750 (with available data on early pregnancy PDIs)
for (varname in colnames(dat_1)) {
  print(varname)
  print(sum(is.na(dat_1[[varname]])))
}

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
Viva_primary_bin <-
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat_1))]
Viva_primary_bin  # 9 primary (binary) outcomes available in Project Viva

Viva_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat_1))]
Viva_secondary_bin  # 8 secondary binary outcomes available in Project Viva

Viva_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat_1))]
Viva_secondary_con  # 2 secondary continuous outcomes available in Project Viva

Viva_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat_1))]
Viva_secondary_cat  # 1 (primary) ordinal/categorical outcome available in Project Viva

## Group outcome variables
Viva_out_bin <- c(Viva_primary_bin, Viva_secondary_bin)
Viva_out_con <- Viva_secondary_con
Viva_out_cat <- Viva_secondary_cat

################################################################################
# Modelling
## Model 1: exposure only - i.e., the unadjusted model
## Model 2: + age, ethnicity, education, income, parity, pre-pregnancy BMI, smoking, alcohol drinking, offspring sex - i.e., additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
## Model 3: + dietary supplementation and total energy intake - i.e., additionally adjusting for nutrition-related factors
################################################################################

# Create a function to extract regression results with SE (for both linear and logistic regression) - !!! Specifically for continuous exposures !!!
IMP_extract_beta.SE_con <- function(mymodel) {
  b <- summary(mymodel)[2, 2]
  se <- summary(mymodel)[2, 3]
  pval <- summary(mymodel)[2, 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

################################################################################

# PDIs and binary outcomes - Logistic regression

## Create a function to extract logistic regression results - !!! Specifically for continuous exposures !!!
IMP_extract_log.res_con <- function(mymodel) {
  OR <-
    format(round(exp(summary(mymodel)[2, 2]), digits = 2), nsmall = 2)  # Extract OR (keep 2 decimal places)
  
  CI <-
    paste0(format(round(
      exp(summary(mymodel)[2, 2] - 1.96 * summary(mymodel)[2, 3]), digits = 2
    ), nsmall = 2), ", ", format(round(
      exp(summary(mymodel)[2, 2] + 1.96 * summary(mymodel)[2, 3]), digits = 2
    ), nsmall = 2))  # Extract 95% CI (keep 2 decimal places);   # confint() too time-consuming for logistic models - calculating 95% CI manually instead
  
  pval <-
    style_pvalue(summary(mymodel)[2, 6], digits = 3)  # Extract p-value (keep 3 decimal places)
  
  x <- cbind(OR, CI, pval)
  
  return(x)
}

## Create empty output tables
obs.tbl_PDI.hPDI.uPDI_bin <-
  c()  # For tabulated results (PDI, hPDI, uPDI in the same table)

obs.res_PDI_bin <-
  c() # For forestplots and meta-analysis for PDI
obs.res_hPDI_bin <-
  c() # For forestplots and meta-analysis for hPDI
obs.res_uPDI_bin <-
  c() # For forestplots and meta-analysis for uPDI

################################################################################

## Loop for each binary outcome
for (myoutcome in Viva_out_bin) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### PDI
  #### Model 1 - Univariate (unadjusted) model
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_1_z"]], family = binomial)
  }
  mod_1_pooled <- pool(mod_1)
  x_1 <- IMP_extract_log.res_con(mod_1_pooled)
  y_1 <- IMP_extract_beta.SE_con(mod_1_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_2_pooled <- pool(mod_2)
  x_2 <- IMP_extract_log.res_con(mod_2_pooled)
  y_2 <- IMP_extract_beta.SE_con(mod_2_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      glm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_EAR.p_con"]],
        family = binomial
      )
  }
  mod_3_pooled <- pool(mod_3)
  x_3 <- IMP_extract_log.res_con(mod_3_pooled)
  y_3 <- IMP_extract_beta.SE_con(mod_3_pooled)
  
  ### hPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_4 <- list()
  for (i in 1:dat_imp$m) {
    mod_4[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_1_z"]], family = binomial)
  }
  mod_4_pooled <- pool(mod_4)
  x_4 <- IMP_extract_log.res_con(mod_4_pooled)
  y_4 <- IMP_extract_beta.SE_con(mod_4_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_5 <- list()
  for (i in 1:dat_imp$m) {
    mod_5[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_5_pooled <- pool(mod_5)
  x_5 <- IMP_extract_log.res_con(mod_5_pooled)
  y_5 <- IMP_extract_beta.SE_con(mod_5_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_6 <- list()
  for (i in 1:dat_imp$m) {
    mod_6[[i]] <-
      glm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_EAR.p_con"]],
        family = binomial
      )
  }
  mod_6_pooled <- pool(mod_6)
  x_6 <- IMP_extract_log.res_con(mod_6_pooled)
  y_6 <- IMP_extract_beta.SE_con(mod_6_pooled)
  
  ### uPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_7 <- list()
  for (i in 1:dat_imp$m) {
    mod_7[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_1_z"]], family = binomial)
  }
  mod_7_pooled <- pool(mod_7)
  x_7 <- IMP_extract_log.res_con(mod_7_pooled)
  y_7 <- IMP_extract_beta.SE_con(mod_7_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_8 <- list()
  for (i in 1:dat_imp$m) {
    mod_8[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_8_pooled <- pool(mod_8)
  x_8 <- IMP_extract_log.res_con(mod_8_pooled)
  y_8 <- IMP_extract_beta.SE_con(mod_8_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_9 <- list()
  for (i in 1:dat_imp$m) {
    mod_9[[i]] <-
      glm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_EAR.p_con"]],
        family = binomial
      )
  }
  mod_9_pooled <- pool(mod_9)
  x_9 <- IMP_extract_log.res_con(mod_9_pooled)
  y_9 <- IMP_extract_beta.SE_con(mod_9_pooled)
  
  ### Regression results for one outcome - Column 1: outcome name * 3; Column 2: Model 1 * 3; Column 3: Model 2 * 3; Column 4: Model 3 * 3
  x <-
    cbind(
      Outcome = as.data.frame(rep(outcome, 3)),
      Exposure = as.data.frame(c("PDI", "hPDI", "uPDI")),
      N = as.data.frame(c(
        paste0(nrow(dat_1[!is.na(dat_1[["PDI_1_z"]]) &
                            !is.na(dat_1[[myoutcome]]) &
                            dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[!is.na(dat_1[["PDI_1_z"]]) &
                                                                             !is.na(dat_1[[myoutcome]]), ])),
        paste0(nrow(dat_1[!is.na(dat_1[["hPDI_1_z"]]) &
                            !is.na(dat_1[[myoutcome]]) &
                            dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[!is.na(dat_1[["hPDI_1_z"]]) &
                                                                             !is.na(dat_1[[myoutcome]]), ])),
        paste0(nrow(dat_1[!is.na(dat_1[["uPDI_1_z"]]) &
                            !is.na(dat_1[[myoutcome]]) &
                            dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[!is.na(dat_1[["uPDI_1_z"]]) &
                                                                             !is.na(dat_1[[myoutcome]]), ]))
      )),
      Model_1 = rbind(x_1, x_4, x_7),
      space = " ",
      Model_2 = rbind(x_2, x_5, x_8),
      space = " ",
      Model_3 = rbind(x_3, x_6, x_9)
    )  # PDI: 1 2 3; hPDI: 4 5 6; uPDI: 7 8 9
  colnames(x)[1] <- "Outcome"
  colnames(x)[2] <- "Exposure"
  colnames(x)[3] <- "N"
  
  outcome_col <- as.data.frame(rep(outcome, 3))
  colnames(outcome_col) <- "Outcome"
  model_col <- as.data.frame(c("Model 1", "Model 2", "Model 3"))
  colnames(model_col) <- "Model"
  y_PDI <- cbind(outcome_col, model_col, rbind(y_1, y_2, y_3))
  y_hPDI <- cbind(outcome_col, model_col, rbind(y_4, y_5, y_6))
  y_uPDI <- cbind(outcome_col, model_col, rbind(y_7, y_8, y_9))
  
  ### Rows continuously added to the output table
  obs.tbl_PDI.hPDI.uPDI_bin <- rbind(obs.tbl_PDI.hPDI.uPDI_bin, x)
  
  obs.res_PDI_bin <- rbind(obs.res_PDI_bin, y_PDI)
  obs.res_hPDI_bin <- rbind(obs.res_hPDI_bin, y_hPDI)
  obs.res_uPDI_bin <- rbind(obs.res_uPDI_bin, y_uPDI)
}

################################################################################

## View and save results
obs.tbl_PDI.hPDI.uPDI_bin <-
  as.data.frame(obs.tbl_PDI.hPDI.uPDI_bin)
obs.tbl_PDI.hPDI.uPDI_bin
dim(obs.tbl_PDI.hPDI.uPDI_bin)  # 17 outcomes * 3 exposures = 51 obs
write.xlsx(
  obs.tbl_PDI.hPDI.uPDI_bin,
  "results/Viva/IMP_SENS.EAR.p_obs.tbl_PDI.hPDI.uPDI_bin.xlsx",
  overwrite = T
)
obs.tbl_PDI.hPDI.uPDI_bin <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.tbl_PDI.hPDI.uPDI_bin.xlsx")

obs.res_PDI_bin <- as.data.frame(obs.res_PDI_bin)
obs.res_PDI_bin$N <- NA
for (i in 1:nrow(obs.res_PDI_bin)) {
  if (obs.res_PDI_bin$Model[i] == "Model 1") {
    obs.res_PDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_PDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "PDI", 3]
  } else if (obs.res_PDI_bin$Model[i] == "Model 2") {
    obs.res_PDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_PDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "PDI", 3]
  } else if (obs.res_PDI_bin$Model[i] == "Model 3") {
    obs.res_PDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_PDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "PDI", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_PDI_bin
dim(obs.res_PDI_bin)  # 17 outcomes * 3 models = 51 obs
write.xlsx(obs.res_PDI_bin,
           "results/Viva/IMP_SENS.EAR.p_obs.res_PDI_bin.xlsx",
           overwrite = T)
obs.res_PDI_bin <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.res_PDI_bin.xlsx")

obs.res_hPDI_bin <- as.data.frame(obs.res_hPDI_bin)
obs.res_hPDI_bin$N <- NA
for (i in 1:nrow(obs.res_hPDI_bin)) {
  if (obs.res_hPDI_bin$Model[i] == "Model 1") {
    obs.res_hPDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_hPDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "hPDI", 3]
  } else if (obs.res_hPDI_bin$Model[i] == "Model 2") {
    obs.res_hPDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_hPDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "hPDI", 3]
  } else if (obs.res_hPDI_bin$Model[i] == "Model 3") {
    obs.res_hPDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_hPDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "hPDI", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_hPDI_bin
dim(obs.res_hPDI_bin)  # 17 outcomes * 3 models = 51 obs
write.xlsx(
  obs.res_hPDI_bin,
  "results/Viva/IMP_SENS.EAR.p_obs.res_hPDI_bin.xlsx",
  overwrite = T
)
obs.res_hPDI_bin <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.res_hPDI_bin.xlsx")

obs.res_uPDI_bin <- as.data.frame(obs.res_uPDI_bin)
obs.res_uPDI_bin$N <- NA
for (i in 1:nrow(obs.res_uPDI_bin)) {
  if (obs.res_uPDI_bin$Model[i] == "Model 1") {
    obs.res_uPDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_uPDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "uPDI", 3]
  } else if (obs.res_uPDI_bin$Model[i] == "Model 2") {
    obs.res_uPDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_uPDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "uPDI", 3]
  } else if (obs.res_uPDI_bin$Model[i] == "Model 3") {
    obs.res_uPDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_uPDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "uPDI", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_uPDI_bin
dim(obs.res_uPDI_bin)  # 17 outcomes * 3 models = 51 obs
write.xlsx(
  obs.res_uPDI_bin,
  "results/Viva/IMP_SENS.EAR.p_obs.res_uPDI_bin.xlsx",
  overwrite = T
)
obs.res_uPDI_bin <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.res_uPDI_bin.xlsx")

## Merge results for all 3 PDIs for forest plots
obs.res_PDI_bin$Exposure <- "PDI"
obs.res_hPDI_bin$Exposure <- "hPDI"
obs.res_uPDI_bin$Exposure <- "uPDI"

obs.res_PDI.hPDI.uPDI_bin <-
  rbind(obs.res_PDI_bin, obs.res_hPDI_bin, obs.res_uPDI_bin)

## Forest plots - For Model 3 only
obs.res_PDI.hPDI.uPDI_bin <-
  subset(obs.res_PDI.hPDI.uPDI_bin, Model == "Model 3")

obs.res_PDI.hPDI.uPDI_bin$Group <- NA
obs.res_PDI.hPDI.uPDI_bin$Group[obs.res_PDI.hPDI.uPDI_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_PDI.hPDI.uPDI_bin$Group[obs.res_PDI.hPDI.uPDI_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_PDI.hPDI.uPDI_bin$Group[obs.res_PDI.hPDI.uPDI_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_PDI.hPDI.uPDI_bin$Group <-
  factor(
    obs.res_PDI.hPDI.uPDI_bin$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_PDI.hPDI.uPDI_bin$Outcome <-
  factor(obs.res_PDI.hPDI.uPDI_bin$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_PDI.hPDI.uPDI_bin$Outcome])  # Make sure the outcomes appear in the right order

obs.res_PDI.hPDI.uPDI_bin$b <-
  as.numeric(obs.res_PDI.hPDI.uPDI_bin$b)
obs.res_PDI.hPDI.uPDI_bin$se <-
  as.numeric(obs.res_PDI.hPDI.uPDI_bin$se)
obs.res_PDI.hPDI.uPDI_bin$pval <-
  as.numeric(obs.res_PDI.hPDI.uPDI_bin$pval)

obs.res_PDI.hPDI.uPDI_bin$Exposure <-
  factor(obs.res_PDI.hPDI.uPDI_bin$Exposure,
         levels = c("uPDI", "hPDI", "PDI"))

head(obs.res_PDI.hPDI.uPDI_bin)
dim(obs.res_PDI.hPDI.uPDI_bin)  # 17 outcomes * 3 exposures * 1 model = 51 obs

### Nightingale forest plots
obs.forest_PDI.hPDI.uPDI_bin <- ggforestplot::forestplot(
  df = obs.res_PDI.hPDI.uPDI_bin,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Exposure,
  xlab = "OR and 95% CI (per 1 SD increase)",
  title = "Binary outcomes in Project Viva",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkorange", "darkgreen", "yellowgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_PDI.hPDI.uPDI_bin

ggsave(
  obs.forest_PDI.hPDI.uPDI_bin,
  file = "results/Viva/IMP_SENS.EAR.p_obs.forest_PDI.hPDI.uPDI_bin.png",
  height = 10,
  width = 7
)

################################################################################

# PDIs and continuous outcomes - Linear regression

## Create a function to extract linear regression results - !!! Specifically for continuous exposures !!!
IMP_extract_lin.res_con <- function(mymodel) {
  Beta <-
    format(round(summary(mymodel)[2, 2], digits = 2), nsmall = 2)  # Extract Beta (keep 2 decimal places)
  
  CI <-
    paste0(format(round(
      summary(mymodel)[2, 2] - 1.96 * summary(mymodel)[2, 3], digits = 2
    ), nsmall = 2), ", ", format(round(
      summary(mymodel)[2, 2] + 1.96 * summary(mymodel)[2, 3], digits = 2
    ), nsmall = 2))  # Extract 95% CI (keep 2 decimal places);   # confint() too time-consuming for logistic models - calculating 95% CI manually instead
  
  pval <-
    style_pvalue(summary(mymodel)[2, 6], digits = 3)  # Extract p-value (keep 3 decimal places)
  
  x <- cbind(Beta, CI, pval)
  
  return(x)
}

## Create empty output tables
obs.tbl_PDI.hPDI.uPDI_con <-
  c()  # For tabulated results (PDI, hPDI, uPDI in the same table)

obs.res_PDI_con <-
  c() # For forestplots and meta-analysis for PDI
obs.res_hPDI_con <-
  c() # For forestplots and meta-analysis for hPDI
obs.res_uPDI_con <-
  c() # For forestplots and meta-analysis for uPDI

################################################################################

## Loop for each continuous outcome
for (myoutcome in Viva_out_con) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### PDI
  #### Model 1 - Univariate (unadjusted) model
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_1_z"]])
  }
  mod_1_pooled <- pool(mod_1)
  x_1 <- IMP_extract_lin.res_con(mod_1_pooled)
  y_1 <- IMP_extract_beta.SE_con(mod_1_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_2_pooled <- pool(mod_2)
  x_2 <- IMP_extract_lin.res_con(mod_2_pooled)
  y_2 <- IMP_extract_beta.SE_con(mod_2_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      lm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_EAR.p_con"]]
      )
  }
  mod_3_pooled <- pool(mod_3)
  x_3 <- IMP_extract_lin.res_con(mod_3_pooled)
  y_3 <- IMP_extract_beta.SE_con(mod_3_pooled)
  
  ### hPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_4 <- list()
  for (i in 1:dat_imp$m) {
    mod_4[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_1_z"]])
  }
  mod_4_pooled <- pool(mod_4)
  x_4 <- IMP_extract_lin.res_con(mod_4_pooled)
  y_4 <- IMP_extract_beta.SE_con(mod_4_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_5 <- list()
  for (i in 1:dat_imp$m) {
    mod_5[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_5_pooled <- pool(mod_5)
  x_5 <- IMP_extract_lin.res_con(mod_5_pooled)
  y_5 <- IMP_extract_beta.SE_con(mod_5_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_6 <- list()
  for (i in 1:dat_imp$m) {
    mod_6[[i]] <-
      lm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_EAR.p_con"]]
      )
  }
  mod_6_pooled <- pool(mod_6)
  x_6 <- IMP_extract_lin.res_con(mod_6_pooled)
  y_6 <- IMP_extract_beta.SE_con(mod_6_pooled)
  
  ### uPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_7 <- list()
  for (i in 1:dat_imp$m) {
    mod_7[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_1_z"]])
  }
  mod_7_pooled <- pool(mod_7)
  x_7 <- IMP_extract_lin.res_con(mod_7_pooled)
  y_7 <- IMP_extract_beta.SE_con(mod_7_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_8 <- list()
  for (i in 1:dat_imp$m) {
    mod_8[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_8_pooled <- pool(mod_8)
  x_8 <- IMP_extract_lin.res_con(mod_8_pooled)
  y_8 <- IMP_extract_beta.SE_con(mod_8_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_9 <- list()
  for (i in 1:dat_imp$m) {
    mod_9[[i]] <-
      lm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_1_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_EAR.p_con"]]
      )
  }
  mod_9_pooled <- pool(mod_9)
  x_9 <- IMP_extract_lin.res_con(mod_9_pooled)
  y_9 <- IMP_extract_beta.SE_con(mod_9_pooled)
  
  ### Regression results for one outcome - Column 1: outcome name * 3; Column 2: Model 1 * 3; Column 3: Model 2 * 3; Column 4: Model 3 * 3
  x <-
    cbind(
      Outcome = as.data.frame(rep(outcome, 3)),
      Exposure = as.data.frame(c("PDI", "hPDI", "uPDI")),
      N = as.data.frame(c(
        nrow(dat_1[!is.na(dat_1[["PDI_1_z"]]) &
                     !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[!is.na(dat_1[["hPDI_1_z"]]) &
                                                                  !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[!is.na(dat_1[["uPDI_1_z"]]) &
                                                                                                               !is.na(dat_1[[myoutcome]]), ])
      )),
      Model_1 = rbind(x_1, x_4, x_7),
      space = " ",
      Model_2 = rbind(x_2, x_5, x_8),
      space = " ",
      Model_3 = rbind(x_3, x_6, x_9)
    )  # PDI: 1 2 3; hPDI: 4 5 6; uPDI: 7 8 9
  colnames(x)[1] <- "Outcome"
  colnames(x)[2] <- "Exposure"
  colnames(x)[3] <- "N"
  
  outcome_col <- as.data.frame(rep(outcome, 3))
  colnames(outcome_col) <- "Outcome"
  model_col <- as.data.frame(c("Model 1", "Model 2", "Model 3"))
  colnames(model_col) <- "Model"
  y_PDI <- cbind(outcome_col, model_col, rbind(y_1, y_2, y_3))
  y_hPDI <- cbind(outcome_col, model_col, rbind(y_4, y_5, y_6))
  y_uPDI <- cbind(outcome_col, model_col, rbind(y_7, y_8, y_9))
  
  ### Rows continuously added to the output table
  obs.tbl_PDI.hPDI.uPDI_con <- rbind(obs.tbl_PDI.hPDI.uPDI_con, x)
  
  obs.res_PDI_con <- rbind(obs.res_PDI_con, y_PDI)
  obs.res_hPDI_con <- rbind(obs.res_hPDI_con, y_hPDI)
  obs.res_uPDI_con <- rbind(obs.res_uPDI_con, y_uPDI)
}

################################################################################

## View and save results
obs.tbl_PDI.hPDI.uPDI_con <-
  as.data.frame(obs.tbl_PDI.hPDI.uPDI_con)
obs.tbl_PDI.hPDI.uPDI_con
dim(obs.tbl_PDI.hPDI.uPDI_con)  # 2 outcomes with 3 exposures in 3 models
write.xlsx(
  obs.tbl_PDI.hPDI.uPDI_con,
  "results/Viva/IMP_SENS.EAR.p_obs.tbl_PDI.hPDI.uPDI_con.xlsx",
  overwrite = T
)
obs.tbl_PDI.hPDI.uPDI_con <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.tbl_PDI.hPDI.uPDI_con.xlsx")

obs.res_PDI_con <- as.data.frame(obs.res_PDI_con)
obs.res_PDI_con$N <- NA
for (i in 1:nrow(obs.res_PDI_con)) {
  if (obs.res_PDI_con$Model[i] == "Model 1") {
    obs.res_PDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_PDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "PDI", 3]
  } else if (obs.res_PDI_con$Model[i] == "Model 2") {
    obs.res_PDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_PDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "PDI", 3]
  } else if (obs.res_PDI_con$Model[i] == "Model 3") {
    obs.res_PDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_PDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "PDI", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_PDI_con
dim(obs.res_PDI_con)  # 2 outcomes * 3 models = 6 obs
write.xlsx(obs.res_PDI_con,
           "results/Viva/IMP_SENS.EAR.p_obs.res_PDI_con.xlsx",
           overwrite = T)
obs.res_PDI_con <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.res_PDI_con.xlsx")

obs.res_hPDI_con <- as.data.frame(obs.res_hPDI_con)
obs.res_hPDI_con$N <- NA
for (i in 1:nrow(obs.res_hPDI_con)) {
  if (obs.res_hPDI_con$Model[i] == "Model 1") {
    obs.res_hPDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_hPDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "hPDI", 3]
  } else if (obs.res_hPDI_con$Model[i] == "Model 2") {
    obs.res_hPDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_hPDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "hPDI", 3]
  } else if (obs.res_hPDI_con$Model[i] == "Model 3") {
    obs.res_hPDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_hPDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "hPDI", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_hPDI_con
dim(obs.res_hPDI_con)  # 2 outcomes * 3 models = 6 obs
write.xlsx(
  obs.res_hPDI_con,
  "results/Viva/IMP_SENS.EAR.p_obs.res_hPDI_con.xlsx",
  overwrite = T
)
obs.res_hPDI_con <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.res_hPDI_con.xlsx")

obs.res_uPDI_con <- as.data.frame(obs.res_uPDI_con)
obs.res_uPDI_con$N <- NA
for (i in 1:nrow(obs.res_uPDI_con)) {
  if (obs.res_uPDI_con$Model[i] == "Model 1") {
    obs.res_uPDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_uPDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "uPDI", 3]
  } else if (obs.res_uPDI_con$Model[i] == "Model 2") {
    obs.res_uPDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_uPDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "uPDI", 3]
  } else if (obs.res_uPDI_con$Model[i] == "Model 3") {
    obs.res_uPDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_uPDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "uPDI", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_uPDI_con
dim(obs.res_uPDI_con)  # 2 outcomes * 3 models = 6 obs
write.xlsx(
  obs.res_uPDI_con,
  "results/Viva/IMP_SENS.EAR.p_obs.res_uPDI_con.xlsx",
  overwrite = T
)
obs.res_uPDI_con <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.res_uPDI_con.xlsx")

## Merge results for all 3 PDIs for forest plots
obs.res_PDI_con$Exposure <- "PDI"
obs.res_hPDI_con$Exposure <- "hPDI"
obs.res_uPDI_con$Exposure <- "uPDI"

obs.res_PDI.hPDI.uPDI_con <-
  rbind(obs.res_PDI_con, obs.res_hPDI_con, obs.res_uPDI_con)

## Forest plots - For Model 3 only
obs.res_PDI.hPDI.uPDI_con <-
  subset(obs.res_PDI.hPDI.uPDI_con, Model == "Model 3")

obs.res_PDI.hPDI.uPDI_con$Group <- NA
obs.res_PDI.hPDI.uPDI_con$Group[obs.res_PDI.hPDI.uPDI_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_PDI.hPDI.uPDI_con$Group[obs.res_PDI.hPDI.uPDI_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_PDI.hPDI.uPDI_con$Group[obs.res_PDI.hPDI.uPDI_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_PDI.hPDI.uPDI_con$Group <-
  factor(
    obs.res_PDI.hPDI.uPDI_con$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_PDI.hPDI.uPDI_con$Outcome <-
  factor(obs.res_PDI.hPDI.uPDI_con$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_PDI.hPDI.uPDI_con$Outcome])  # Make sure the outcomes appear in the right order

obs.res_PDI.hPDI.uPDI_con$b <-
  as.numeric(obs.res_PDI.hPDI.uPDI_con$b)
obs.res_PDI.hPDI.uPDI_con$se <-
  as.numeric(obs.res_PDI.hPDI.uPDI_con$se)
obs.res_PDI.hPDI.uPDI_con$pval <-
  as.numeric(obs.res_PDI.hPDI.uPDI_con$pval)

obs.res_PDI.hPDI.uPDI_con$Exposure <-
  factor(obs.res_PDI.hPDI.uPDI_con$Exposure,
         levels = c("uPDI", "hPDI", "PDI"))

head(obs.res_PDI.hPDI.uPDI_con)
dim(obs.res_PDI.hPDI.uPDI_con)  # 2 outcomes * 3 models = 6 obs

### Nightingale forest plots
obs.forest_PDI.hPDI.uPDI_con <- ggforestplot::forestplot(
  df = obs.res_PDI.hPDI.uPDI_con,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Exposure,
  xlab = "Beta and 95% CI (per 1 SD increase)",
  title = "Continuous outcomes in Project Viva",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkorange", "darkgreen", "yellowgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_PDI.hPDI.uPDI_con

ggsave(
  obs.forest_PDI.hPDI.uPDI_con,
  file = "results/Viva/IMP_SENS.EAR.p_obs.forest_PDI.hPDI.uPDI_con.png",
  height = 3,
  width = 7
)

################################################################################

# PDIs and ordinal outcomes - Ordered logistic regression

## Create a function to extract regression results with SE (for ORDINAL logistic regression) - !!! Specifically for continuous exposures !!!
IMP_extract_ord.beta.SE_con <- function(mymodel) {
  b <- summary(mymodel)[4, 2]
  se <- summary(mymodel)[4, 3]
  pval <- summary(mymodel)[4, 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

## Create a function to extract ORDINAL logistic regression results - !!! Specifically for continuous exposures !!!
IMP_extract_ord.log.res_con <- function(mymodel) {
  OR <-
    format(round(exp(summary(mymodel)[4, 2]), digits = 2), nsmall = 2)  # Extract OR (keep 2 decimal places)
  
  CI <-
    paste0(format(round(
      exp(summary(mymodel)[4, 2] - 1.96 * summary(mymodel)[4, 3]), digits = 2
    ), nsmall = 2), ", ", format(round(
      exp(summary(mymodel)[4, 2] + 1.96 * summary(mymodel)[4, 3]), digits = 2
    ), nsmall = 2))  # Extract 95% CI (keep 2 decimal places);   # confint() too time-consuming for logistic models - calculating 95% CI manually instead
  
  pval <-
    style_pvalue(summary(mymodel)[4, 6], digits = 3)  # Extract p-value (keep 3 decimal places)
  
  x <- cbind(OR, CI, pval)
  
  return(x)
}

################################################################################
################################################################################

## PDI

### Model 1 - Univariate (unadjusted) model
mod_1 <- list()
for (i in 1:dat_imp$m) {
  mod_1[[i]] <-
    clm(bf_dur_4c ~ PDI_1_z, data = dat[[i]])
}
mod_1_pooled <- pool(mod_1)
x_1 <- IMP_extract_ord.log.res_con(mod_1_pooled)
y_1 <- IMP_extract_ord.beta.SE_con(mod_1_pooled)

### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_2 <- list()
for (i in 1:dat_imp$m) {
  mod_2[[i]] <-
    clm(
      bf_dur_4c ~ PDI_1_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
      data = dat[[i]]
    )
}
mod_2_pooled <- pool(mod_2)
x_2 <- IMP_extract_ord.log.res_con(mod_2_pooled)
y_2 <- IMP_extract_ord.beta.SE_con(mod_2_pooled)

### Model 3 - Additionally adjusting for nutrition-related factors (including hPDI as the indicator for the healthfulness of diet)
mod_3 <- list()
for (i in 1:dat_imp$m) {
  mod_3[[i]] <-
    clm(
      bf_dur_4c ~ PDI_1_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin,
      data = dat[[i]]
    )
}
mod_3_pooled <- pool(mod_3)
x_3 <- IMP_extract_ord.log.res_con(mod_3_pooled)
y_3 <- IMP_extract_ord.beta.SE_con(mod_3_pooled)

## hPDI

### Model 1 - Univariate (unadjusted) model
mod_4 <- list()
for (i in 1:dat_imp$m) {
  mod_4[[i]] <-
    clm(bf_dur_4c ~ hPDI_1_z, data = dat[[i]])
}
mod_4_pooled <- pool(mod_4)
x_4 <- IMP_extract_ord.log.res_con(mod_4_pooled)
y_4 <- IMP_extract_ord.beta.SE_con(mod_4_pooled)

### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_5 <- list()
for (i in 1:dat_imp$m) {
  mod_5[[i]] <-
    clm(
      bf_dur_4c ~ hPDI_1_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
      data = dat[[i]]
    )
}
mod_5_pooled <- pool(mod_5)
x_5 <- IMP_extract_ord.log.res_con(mod_5_pooled)
y_5 <- IMP_extract_ord.beta.SE_con(mod_5_pooled)

### Model 3 - Additionally adjusting for nutrition-related factors (including hPDI as the indicator for the healthfulness of diet)
mod_6 <- list()
for (i in 1:dat_imp$m) {
  mod_6[[i]] <-
    clm(
      bf_dur_4c ~ hPDI_1_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin,
      data = dat[[i]]
    )
}
mod_6_pooled <- pool(mod_6)
x_6 <- IMP_extract_ord.log.res_con(mod_6_pooled)
y_6 <- IMP_extract_ord.beta.SE_con(mod_6_pooled)

## uPDI

### Model 1 - Univariate (unadjusted) model
mod_7 <- list()
for (i in 1:dat_imp$m) {
  mod_7[[i]] <-
    clm(bf_dur_4c ~ uPDI_1_z, data = dat[[i]])
}
mod_7_pooled <- pool(mod_7)
x_7 <- IMP_extract_ord.log.res_con(mod_7_pooled)
y_7 <- IMP_extract_ord.beta.SE_con(mod_7_pooled)

### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_8 <- list()
for (i in 1:dat_imp$m) {
  mod_8[[i]] <-
    clm(
      bf_dur_4c ~ uPDI_1_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
      data = dat[[i]]
    )
}
mod_8_pooled <- pool(mod_8)
x_8 <- IMP_extract_ord.log.res_con(mod_8_pooled)
y_8 <- IMP_extract_ord.beta.SE_con(mod_8_pooled)

### Model 3 - Additionally adjusting for nutrition-related factors
mod_9 <- list()
for (i in 1:dat_imp$m) {
  mod_9[[i]] <-
    clm(
      bf_dur_4c ~ uPDI_1_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin,
      data = dat[[i]]
    )
}
mod_9_pooled <- pool(mod_9)
x_9 <- IMP_extract_ord.log.res_con(mod_9_pooled)
y_9 <- IMP_extract_ord.beta.SE_con(mod_9_pooled)

################################################################################
################################################################################

## Combine regression results
outcome <- var_lab(dat_1$bf_dur_4c)

x <-
  cbind(
    Outcome = as.data.frame(rep(outcome, 3)),
    Exposure = as.data.frame(c("PDI", "hPDI", "uPDI")),
    N = as.data.frame(c(nrow(dat_1[!is.na(dat_1$PDI_1_z) &
                                     !is.na(dat_1$bf_dur_4c), ]), nrow(dat_1[!is.na(dat_1$hPDI_1_z) &
                                                                               !is.na(dat_1$bf_dur_4c), ]), nrow(dat_1[!is.na(dat_1$uPDI_1_z) &
                                                                                                                         !is.na(dat_1$bf_dur_4c), ]))),
    Model_1 = rbind(x_1, x_4, x_7),
    space = " ",
    Model_2 = rbind(x_2, x_5, x_8),
    space = " ",
    Model_3 = rbind(x_3, x_6, x_9)
  )  # PDI: 1 2 3; hPDI: 4 5 6; uPDI: 7 8 9
colnames(x)[1] <- "Outcome"
colnames(x)[2] <- "Exposure"
colnames(x)[3] <- "N"

outcome_col <- as.data.frame(rep(outcome, 3))
colnames(outcome_col) <- "Outcome"
model_col <- as.data.frame(c("Model 1", "Model 2", "Model 3"))
colnames(model_col) <- "Model"
y_PDI <- cbind(outcome_col, model_col, rbind(y_1, y_2, y_3))
y_hPDI <- cbind(outcome_col, model_col, rbind(y_4, y_5, y_6))
y_uPDI <- cbind(outcome_col, model_col, rbind(y_7, y_8, y_9))

################################################################################
################################################################################

## View and save results
obs.tbl_PDI.hPDI.uPDI_ord <- as.data.frame(x)
obs.tbl_PDI.hPDI.uPDI_ord
dim(obs.tbl_PDI.hPDI.uPDI_ord)  # 1 outcome * 3 exposures = 3 obs
write.xlsx(
  obs.tbl_PDI.hPDI.uPDI_ord,
  "results/Viva/IMP_SENS.EAR.p_obs.tbl_PDI.hPDI.uPDI_ord.xlsx",
  overwrite = T
)
obs.tbl_PDI.hPDI.uPDI_ord <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.tbl_PDI.hPDI.uPDI_ord.xlsx")

obs.res_PDI_ord <- as.data.frame(y_PDI)
obs.res_PDI_ord$N <- NA
for (i in 1:nrow(obs.res_PDI_ord)) {
  if (obs.res_PDI_ord$Model[i] == "Model 1") {
    obs.res_PDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_PDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "PDI", 3]
  } else if (obs.res_PDI_ord$Model[i] == "Model 2") {
    obs.res_PDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_PDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "PDI", 3]
  } else if (obs.res_PDI_ord$Model[i] == "Model 3") {
    obs.res_PDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_PDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "PDI", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_PDI_ord
dim(obs.res_PDI_ord)  # 1 outcome * 3 models = 3 obs
write.xlsx(obs.res_PDI_ord,
           "results/Viva/IMP_SENS.EAR.p_obs.res_PDI_ord.xlsx",
           overwrite = T)
obs.res_PDI_ord <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.res_PDI_ord.xlsx")

obs.res_hPDI_ord <- as.data.frame(y_hPDI)
obs.res_hPDI_ord$N <- NA
for (i in 1:nrow(obs.res_hPDI_ord)) {
  if (obs.res_hPDI_ord$Model[i] == "Model 1") {
    obs.res_hPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_hPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "hPDI", 3]
  } else if (obs.res_hPDI_ord$Model[i] == "Model 2") {
    obs.res_hPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_hPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "hPDI", 3]
  } else if (obs.res_hPDI_ord$Model[i] == "Model 3") {
    obs.res_hPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_hPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "hPDI", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_hPDI_ord
dim(obs.res_hPDI_ord)  # 1 outcome * 3 models = 3 obs
write.xlsx(
  obs.res_hPDI_ord,
  "results/Viva/IMP_SENS.EAR.p_obs.res_hPDI_ord.xlsx",
  overwrite = T
)
obs.res_hPDI_ord <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.res_hPDI_ord.xlsx")

obs.res_uPDI_ord <- as.data.frame(y_uPDI)
obs.res_uPDI_ord$N <- NA
for (i in 1:nrow(obs.res_uPDI_ord)) {
  if (obs.res_uPDI_ord$Model[i] == "Model 1") {
    obs.res_uPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_uPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "uPDI", 3]
  } else if (obs.res_uPDI_ord$Model[i] == "Model 2") {
    obs.res_uPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_uPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "uPDI", 3]
  } else if (obs.res_uPDI_ord$Model[i] == "Model 3") {
    obs.res_uPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_uPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "uPDI", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_uPDI_ord
dim(obs.res_uPDI_ord)  # 1 outcome * 3 models = 3 obs
write.xlsx(
  obs.res_uPDI_ord,
  "results/Viva/IMP_SENS.EAR.p_obs.res_uPDI_ord.xlsx",
  overwrite = T
)
obs.res_uPDI_ord <-
  read.xlsx("results/Viva/IMP_SENS.EAR.p_obs.res_uPDI_ord.xlsx")

## Merge results for all 3 PDIs for forest plots
obs.res_PDI_ord$Exposure <- "PDI"
obs.res_hPDI_ord$Exposure <- "hPDI"
obs.res_uPDI_ord$Exposure <- "uPDI"

obs.res_PDI.hPDI.uPDI_ord <-
  rbind(obs.res_PDI_ord, obs.res_hPDI_ord, obs.res_uPDI_ord)

## Forest plots - For Model 3 only
obs.res_PDI.hPDI.uPDI_ord <-
  subset(obs.res_PDI.hPDI.uPDI_ord, Model == "Model 3")

obs.res_PDI.hPDI.uPDI_ord$Group <- NA
obs.res_PDI.hPDI.uPDI_ord$Group[obs.res_PDI.hPDI.uPDI_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_PDI.hPDI.uPDI_ord$Group[obs.res_PDI.hPDI.uPDI_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_PDI.hPDI.uPDI_ord$Group[obs.res_PDI.hPDI.uPDI_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_PDI.hPDI.uPDI_ord$Group <-
  factor(
    obs.res_PDI.hPDI.uPDI_ord$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_PDI.hPDI.uPDI_ord$Outcome <-
  factor(obs.res_PDI.hPDI.uPDI_ord$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_PDI.hPDI.uPDI_ord$Outcome])  # Make sure the outcomes appear in the right order

obs.res_PDI.hPDI.uPDI_ord$b <-
  as.numeric(obs.res_PDI.hPDI.uPDI_ord$b)
obs.res_PDI.hPDI.uPDI_ord$se <-
  as.numeric(obs.res_PDI.hPDI.uPDI_ord$se)
obs.res_PDI.hPDI.uPDI_ord$pval <-
  as.numeric(obs.res_PDI.hPDI.uPDI_ord$pval)

obs.res_PDI.hPDI.uPDI_ord$Exposure <-
  factor(obs.res_PDI.hPDI.uPDI_ord$Exposure,
         levels = c("uPDI", "hPDI", "PDI"))

head(obs.res_PDI.hPDI.uPDI_ord)
dim(obs.res_PDI.hPDI.uPDI_ord)  # 1 outcome * 3 exposures * 1 model = 3 obs

### Nightingale forest plots
obs.forest_PDI.hPDI.uPDI_ord <- ggforestplot::forestplot(
  df = obs.res_PDI.hPDI.uPDI_ord,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Exposure,
  xlab = "OR and 95% CI (per 1 SD increase)",
  title = "Ordinal outcomes in Project Viva",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkorange", "darkgreen", "yellowgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_PDI.hPDI.uPDI_ord

ggsave(
  obs.forest_PDI.hPDI.uPDI_ord,
  file = "results/Viva/IMP_SENS.EAR.p_obs.forest_PDI.hPDI.uPDI_ord.png",
  height = 2,
  width = 7
)

################################################################################
