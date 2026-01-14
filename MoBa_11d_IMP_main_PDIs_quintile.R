################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 01-Aug-2025
# This script is to perform main association analysis (with imputed data) for quintiles of plant-based diet indices (PDIs) in MoBa.

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
load("dat_exp_cov_out_pat_IMP_PDIs.RData")

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
MoBa_secondary_bin <-
  MoBa_secondary_bin[MoBa_secondary_bin != "anaemia_preg_subsamp"]  # !!! Exclude maternal anaemia occurring in pregnancy for this analysis !!!
MoBa_secondary_bin  # 8 secondary binary outcomes available in MoBa

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
## Model 1: exposure only - i.e., the unadjusted model
## Model 2: + age, ethnicity, education, income, parity, pre-pregnancy BMI, smoking, alcohol drinking, offspring sex - i.e., additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
## Model 3: + dietary supplementation and total energy intake - i.e., additionally adjusting for nutrition-related factors
################################################################################

# Create a function to extract regression results with SE (for both linear and logistic regression) - !!! Specifically for 5-categories exposures !!!
IMP_extract_beta.SE_cat <- function(mymodel) {
  b <- summary(mymodel)[c(2, 3, 4, 5), 2]
  se <- summary(mymodel)[c(2, 3, 4, 5), 3]
  pval <- summary(mymodel)[c(2, 3, 4, 5), 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

################################################################################

# PDIs and binary outcomes - Logistic regression

## Create empty output tables
obs.res_PDI_bin <-
  c() # For forestplots and meta-analysis for PDI
obs.res_hPDI_bin <-
  c() # For forestplots and meta-analysis for hPDI
obs.res_uPDI_bin <-
  c() # For forestplots and meta-analysis for uPDI

################################################################################

## Loop for each binary outcome
for (myoutcome in MoBa_out_bin) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### PDI
  #### Model 1 - Univariate (unadjusted) model
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_z"]], family = binomial)
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE_cat(mod_1_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_2_pooled <- pool(mod_2)
  y_2 <- IMP_extract_beta.SE_cat(mod_2_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      glm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_DUR.p_con"]],
        family = binomial
      )
  }
  mod_3_pooled <- pool(mod_3)
  y_3 <- IMP_extract_beta.SE_cat(mod_3_pooled)
  
  ### hPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_4 <- list()
  for (i in 1:dat_imp$m) {
    mod_4[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_z"]], family = binomial)
  }
  mod_4_pooled <- pool(mod_4)
  y_4 <- IMP_extract_beta.SE_cat(mod_4_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_5 <- list()
  for (i in 1:dat_imp$m) {
    mod_5[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_5_pooled <- pool(mod_5)
  y_5 <- IMP_extract_beta.SE_cat(mod_5_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_6 <- list()
  for (i in 1:dat_imp$m) {
    mod_6[[i]] <-
      glm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_DUR.p_con"]],
        family = binomial
      )
  }
  mod_6_pooled <- pool(mod_6)
  y_6 <- IMP_extract_beta.SE_cat(mod_6_pooled)
  
  ### uPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_7 <- list()
  for (i in 1:dat_imp$m) {
    mod_7[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_z"]], family = binomial)
  }
  mod_7_pooled <- pool(mod_7)
  y_7 <- IMP_extract_beta.SE_cat(mod_7_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_8 <- list()
  for (i in 1:dat_imp$m) {
    mod_8[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_8_pooled <- pool(mod_8)
  y_8 <- IMP_extract_beta.SE_cat(mod_8_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_9 <- list()
  for (i in 1:dat_imp$m) {
    mod_9[[i]] <-
      glm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_DUR.p_con"]],
        family = binomial
      )
  }
  mod_9_pooled <- pool(mod_9)
  y_9 <- IMP_extract_beta.SE_cat(mod_9_pooled)
  
  outcome_col <- as.data.frame(rep(outcome, 12))
  colnames(outcome_col) <- "Outcome"
  quintile_col <- as.data.frame(rep(c("Q2", "Q3", "Q4", "Q5"), 3))
  colnames(quintile_col) <- "Quintile"
  model_col <-
    as.data.frame(c(rep("Model 1", 4), rep("Model 2", 4), rep("Model 3", 4)))
  colnames(model_col) <- "Model"
  
  N_exp_PDI <- as.data.frame(rep(c(
    paste0(nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[2] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[2] &
                                                                         !is.na(dat_1[[myoutcome]]), ])),
    paste0(nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[3] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[3] &
                                                                         !is.na(dat_1[[myoutcome]]), ])),
    paste0(nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[4] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[4] &
                                                                         !is.na(dat_1[[myoutcome]]), ])),
    paste0(nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[5] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[5] &
                                                                         !is.na(dat_1[[myoutcome]]), ]))
  ), 3))
  colnames(N_exp_PDI) <- "N_exp"
  N_ref_PDI <-
    as.data.frame(rep(paste0(nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[1] &
                                          !is.na(dat_1[[myoutcome]]) &
                                          dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[1] &
                                                                                           !is.na(dat_1[[myoutcome]]), ])), 12))
  colnames(N_ref_PDI) <- "N_ref"
  
  N_exp_hPDI <- as.data.frame(rep(c(
    paste0(nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[2] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[2] &
                                                                         !is.na(dat_1[[myoutcome]]), ])),
    paste0(nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[3] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[3] &
                                                                         !is.na(dat_1[[myoutcome]]), ])),
    paste0(nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[4] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[4] &
                                                                         !is.na(dat_1[[myoutcome]]), ])),
    paste0(nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[5] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[5] &
                                                                         !is.na(dat_1[[myoutcome]]), ]))
  ), 3))
  colnames(N_exp_hPDI) <- "N_exp"
  N_ref_hPDI <-
    as.data.frame(rep(paste0(nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[1] &
                                          !is.na(dat_1[[myoutcome]]) &
                                          dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[1] &
                                                                                           !is.na(dat_1[[myoutcome]]), ])), 12))
  colnames(N_ref_hPDI) <- "N_ref"
  
  N_exp_uPDI <- as.data.frame(rep(c(
    paste0(nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[2] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[2] &
                                                                         !is.na(dat_1[[myoutcome]]), ])),
    paste0(nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[3] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[3] &
                                                                         !is.na(dat_1[[myoutcome]]), ])),
    paste0(nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[4] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[4] &
                                                                         !is.na(dat_1[[myoutcome]]), ])),
    paste0(nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[5] &
                        !is.na(dat_1[[myoutcome]]) &
                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[5] &
                                                                         !is.na(dat_1[[myoutcome]]), ]))
  ), 3))
  colnames(N_exp_uPDI) <- "N_exp"
  N_ref_uPDI <-
    as.data.frame(rep(paste0(nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[1] &
                                          !is.na(dat_1[[myoutcome]]) &
                                          dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[1] &
                                                                                           !is.na(dat_1[[myoutcome]]), ])), 12))
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
  
  ### Rows continuously added to the output table
  obs.res_PDI_bin <- rbind(obs.res_PDI_bin, y_PDI)
  obs.res_hPDI_bin <- rbind(obs.res_hPDI_bin, y_hPDI)
  obs.res_uPDI_bin <- rbind(obs.res_uPDI_bin, y_uPDI)
}

################################################################################

## View and save results
obs.res_PDI_bin <- as.data.frame(obs.res_PDI_bin)
obs.res_PDI_bin$Exposure <- "PDI"
obs.res_PDI_bin
dim(obs.res_PDI_bin)  # 21 outcomes * 3 models * 4 comparisons = 252 obs
write.xlsx(obs.res_PDI_bin,
           "results/IMP_MAIN_obs.res_PDI.quintile_bin.xlsx",
           overwrite = T)

obs.res_hPDI_bin <- as.data.frame(obs.res_hPDI_bin)
obs.res_hPDI_bin$Exposure <- "hPDI"
obs.res_hPDI_bin
dim(obs.res_hPDI_bin)  # 21 outcomes * 3 models * 4 comparisons = 252 obs
write.xlsx(obs.res_hPDI_bin,
           "results/IMP_MAIN_obs.res_hPDI.quintile_bin.xlsx",
           overwrite = T)

obs.res_uPDI_bin <- as.data.frame(obs.res_uPDI_bin)
obs.res_uPDI_bin$Exposure <- "uPDI"
obs.res_uPDI_bin
dim(obs.res_uPDI_bin)  # 21 outcomes * 3 models * 4 comparisons = 252 obs
write.xlsx(obs.res_uPDI_bin,
           "results/IMP_MAIN_obs.res_uPDI.quintile_bin.xlsx",
           overwrite = T)

################################################################################

# PDIs and continuous outcomes - Linear regression

## Create empty output tables
obs.res_PDI_con <-
  c() # For forestplots and meta-analysis for PDI
obs.res_hPDI_con <-
  c() # For forestplots and meta-analysis for hPDI
obs.res_uPDI_con <-
  c() # For forestplots and meta-analysis for uPDI

################################################################################

## Loop for each continuous outcome
for (myoutcome in MoBa_out_con) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### PDI
  #### Model 1 - Univariate (unadjusted) model
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_z"]])
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE_cat(mod_1_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_2_pooled <- pool(mod_2)
  y_2 <- IMP_extract_beta.SE_cat(mod_2_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      lm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["PDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_DUR.p_con"]]
      )
  }
  mod_3_pooled <- pool(mod_3)
  y_3 <- IMP_extract_beta.SE_cat(mod_3_pooled)
  
  ### hPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_4 <- list()
  for (i in 1:dat_imp$m) {
    mod_4[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_z"]])
  }
  mod_4_pooled <- pool(mod_4)
  y_4 <- IMP_extract_beta.SE_cat(mod_4_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_5 <- list()
  for (i in 1:dat_imp$m) {
    mod_5[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_5_pooled <- pool(mod_5)
  y_5 <- IMP_extract_beta.SE_cat(mod_5_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_6 <- list()
  for (i in 1:dat_imp$m) {
    mod_6[[i]] <-
      lm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["hPDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_DUR.p_con"]]
      )
  }
  mod_6_pooled <- pool(mod_6)
  y_6 <- IMP_extract_beta.SE_cat(mod_6_pooled)
  
  ### uPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_7 <- list()
  for (i in 1:dat_imp$m) {
    mod_7[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_z"]])
  }
  mod_7_pooled <- pool(mod_7)
  y_7 <- IMP_extract_beta.SE_cat(mod_7_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_8 <- list()
  for (i in 1:dat_imp$m) {
    mod_8[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_8_pooled <- pool(mod_8)
  y_8 <- IMP_extract_beta.SE_cat(mod_8_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_9 <- list()
  for (i in 1:dat_imp$m) {
    mod_9[[i]] <-
      lm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["uPDI_z"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_DUR.p_con"]]
      )
  }
  mod_9_pooled <- pool(mod_9)
  y_9 <- IMP_extract_beta.SE_cat(mod_9_pooled)
  
  outcome_col <- as.data.frame(rep(outcome, 12))
  colnames(outcome_col) <- "Outcome"
  quintile_col <- as.data.frame(rep(c("Q2", "Q3", "Q4", "Q5"), 3))
  colnames(quintile_col) <- "Quintile"
  model_col <-
    as.data.frame(c(rep("Model 1", 4), rep("Model 2", 4), rep("Model 3", 4)))
  colnames(model_col) <- "Model"
  
  N_exp_PDI <- as.data.frame(rep(c(
    nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[2] &
                 !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[3] &
                                                              !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[4] &
                                                                                                           !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[5] &
                                                                                                                                                        !is.na(dat_1[[myoutcome]]), ])
  ), 3))
  colnames(N_exp_PDI) <- "N_exp"
  N_ref_PDI <-
    as.data.frame(rep(nrow(dat_1[dat_1$PDI_5Q == levels(dat_1$PDI_5Q)[1] &
                                   !is.na(dat_1[[myoutcome]]), ]), 12))
  colnames(N_ref_PDI) <- "N_ref"
  
  N_exp_hPDI <- as.data.frame(rep(c(
    nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[2] &
                 !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[3] &
                                                              !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[4] &
                                                                                                           !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[5] &
                                                                                                                                                        !is.na(dat_1[[myoutcome]]), ])
  ), 3))
  colnames(N_exp_hPDI) <- "N_exp"
  N_ref_hPDI <-
    as.data.frame(rep(nrow(dat_1[dat_1$hPDI_5Q == levels(dat_1$hPDI_5Q)[1] &
                                   !is.na(dat_1[[myoutcome]]), ]), 12))
  colnames(N_ref_hPDI) <- "N_ref"
  
  N_exp_uPDI <- as.data.frame(rep(c(
    nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[2] &
                 !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[3] &
                                                              !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[4] &
                                                                                                           !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[5] &
                                                                                                                                                        !is.na(dat_1[[myoutcome]]), ])
  ), 3))
  colnames(N_exp_uPDI) <- "N_exp"
  N_ref_uPDI <-
    as.data.frame(rep(nrow(dat_1[dat_1$uPDI_5Q == levels(dat_1$uPDI_5Q)[1] &
                                   !is.na(dat_1[[myoutcome]]), ]), 12))
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
  
  ### Rows continuously added to the output table
  obs.res_PDI_con <- rbind(obs.res_PDI_con, y_PDI)
  obs.res_hPDI_con <- rbind(obs.res_hPDI_con, y_hPDI)
  obs.res_uPDI_con <- rbind(obs.res_uPDI_con, y_uPDI)
}

################################################################################

## View and save results
obs.res_PDI_con <- as.data.frame(obs.res_PDI_con)
obs.res_PDI_con$Exposure <- "PDI"
obs.res_PDI_con
dim(obs.res_PDI_con)  # 4 outcomes * 3 models * 4 comparisons = 48 obs
write.xlsx(obs.res_PDI_con,
           "results/IMP_MAIN_obs.res_PDI.quintile_con.xlsx",
           overwrite = T)

obs.res_hPDI_con <- as.data.frame(obs.res_hPDI_con)
obs.res_hPDI_con$Exposure <- "hPDI"
obs.res_hPDI_con
dim(obs.res_hPDI_con)  # 4 outcomes * 3 models * 4 comparisons = 48 obs
write.xlsx(obs.res_hPDI_con,
           "results/IMP_MAIN_obs.res_hPDI.quintile_con.xlsx",
           overwrite = T)

obs.res_uPDI_con <- as.data.frame(obs.res_uPDI_con)
obs.res_uPDI_con$Exposure <- "uPDI"
obs.res_uPDI_con
dim(obs.res_uPDI_con)  # 4 outcomes * 3 models * 4 comparisons = 48 obs
write.xlsx(obs.res_uPDI_con,
           "results/IMP_MAIN_obs.res_uPDI.quintile_con.xlsx",
           overwrite = T)

################################################################################
