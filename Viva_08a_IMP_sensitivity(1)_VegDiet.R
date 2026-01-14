################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 02-Dec-2024
# This script is to perform sensitivity analysis (with imputed data) for vegetarian diets in Project Viva.
## Sensitivity Analysis 1 - FFQ-based + self-defined vegetarianism

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
load("data/Viva/dat_exp_cov_out_IMP.RData")

## Create a list of imputed datasets
dat <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat[[i]] <- complete_dat
}

## Check the first imputed dataset
dat_1 <- dat[[1]]
head(dat_1)
dim(dat_1)  # 1872  XXX
for (varname in colnames(dat_1)) {
  print(varname)
  print(sum(is.na(dat_1[[varname]])))
}

################################################################################
# Sensitivity Analysis 1 - Diet-based + self-defined vegetarianism
################################################################################

# Subset to those with non-missing diet-based + self-defined vegetarianism variable
for (i in 1:dat_imp$m) {
  dat[[i]] <- subset(dat[[i]], !is.na(dat[[i]]$VegDiet_bin_FFQ_self))
}

dat_1 <- dat[[1]]  # Generate the first imputed dataset again
head(dat_1)
dim(dat_1)  # 1837  XXX
for (varname in colnames(dat_1)) {
  print(varname)
  print(sum(is.na(dat_1[[varname]])))
}
table(dat_1$VegDiet_bin_FFQ_self, useNA = "always")
table(dat_1$VegDiet_bin, useNA = "always")

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
## Model 3: + dietary supplementation, total energy intake, hPDIm - i.e., additionally adjusting for nutrition-related factors
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

## Create a function to extract logistic regression results (N, OR, 95% CI, p-value) - !!! Specifically for binary exposures !!!
IMP_extract_log.res_bin <- function(mymodel) {
  ### Non-vegetarian (ref)
  Exposure_1 <- "Non-vegetarian (ref)"
  OR_1 <- format(round(1.00, digits = 2), nsmall = 2)
  CI_1 <- "-"
  pval_1 <- "-"
  x_1 <- cbind(Exposure_1, OR_1, CI_1, pval_1)
  ##############################################################################
  ### Pesco-/full vegetarian
  Exposure_2 <- "Pesco-/full vegetarian"
  OR_2 <-
    format(round(exp(summary(mymodel)[2, 2]), digits = 2), nsmall = 2)
  CI_2 <- paste0(format(round(exp(
    summary(mymodel, conf.int = T)[2, 7]
  ), digits = 2), nsmall = 2), ", ", format(round(exp(
    summary(mymodel, conf.int = T)[2, 8]
  ), digits = 2), nsmall = 2))
  pval_2 <- style_pvalue(summary(mymodel)[2, 6], digits = 3)
  x_2 <- cbind(Exposure_2, OR_2, CI_2, pval_2)
  ##############################################################################
  
  x <- rbind(x_1, x_2)
  colnames(x) <- c("Exposure", "OR", "CI", "pval")
  
  return(x)
}

## Create empty output tables
obs.tbl_VegDiet_bin <-
  c()  # For tabulated results
obs.res_VegDiet_bin <-
  c() # For forestplots and meta-analysis

################################################################################

## Loop for each binary outcome
for (myoutcome in Viva_out_bin) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 3 models for vegetarian diet
  #### Model 1 - Univariate (unadjusted) model
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_FFQ_self"]], family = binomial)
  }
  mod_1_pooled <- pool(mod_1)
  x_1 <- IMP_extract_log.res_bin(mod_1_pooled)
  y_1 <- IMP_extract_beta.SE_bin(mod_1_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_FFQ_self"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_2_pooled <- pool(mod_2)
  x_2 <- IMP_extract_log.res_bin(mod_2_pooled)
  y_2 <- IMP_extract_beta.SE_bin(mod_2_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors (including hPDIm as the indicator for the healthfulness of diet)
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      glm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_FFQ_self"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_DUR.p_con"]] + dat[[i]][["hPDIm"]],
        family = binomial
      )
  }
  mod_3_pooled <- pool(mod_3)
  x_3 <- IMP_extract_log.res_bin(mod_3_pooled)
  y_3 <- IMP_extract_beta.SE_bin(mod_3_pooled)
  
  ### Regression results for one outcome
  outcome_x <- as.data.frame(rep(outcome, 2))
  colnames(outcome_x) <- "Outcome"
  
  N_x <- as.data.frame(c(paste0(nrow(dat_1[dat_1$VegDiet_bin_FFQ_self == 0 &
                                             !is.na(dat_1[[myoutcome]]) &
                                             dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$VegDiet_bin_FFQ_self == 0 &
                                                                                              !is.na(dat_1[[myoutcome]]), ])), paste0(nrow(dat_1[dat_1$VegDiet_bin_FFQ_self == 1 &
                                                                                                                                                   !is.na(dat_1[[myoutcome]]) &
                                                                                                                                                   dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$VegDiet_bin_FFQ_self == 1 &
                                                                                                                                                                                                    !is.na(dat_1[[myoutcome]]), ]))))
  colnames(N_x) <- "N"
  
  x <-
    cbind(outcome_x, N_x, x_1, x_2, x_3)  # Columns: Outcome name | N | Model 1 | Model 2 | Model 3; rows: Non-vegetarian | Pesco-/full vegetarian
  
  outcome_col <- as.data.frame(rep(outcome, 3))
  colnames(outcome_col) <- "Outcome"
  model_col <-
    as.data.frame(c("Model 1", "Model 2", "Model 3"))
  colnames(model_col) <- "Model"
  y <- cbind(outcome_col, model_col, rbind(y_1, y_2, y_3))
  
  ### Rows continuously added to the output table
  obs.tbl_VegDiet_bin <- rbind(obs.tbl_VegDiet_bin, x)
  
  obs.res_VegDiet_bin <- rbind(obs.res_VegDiet_bin, y)
}

obs.tbl_VegDiet_bin
obs.res_VegDiet_bin

################################################################################

## View and save results
obs.tbl_VegDiet_bin <- as.data.frame(obs.tbl_VegDiet_bin)
obs.tbl_VegDiet_bin[, c(7, 11)] <-
  " " # For convenience when making tables
for (col_num in seq(4, ncol(obs.tbl_VegDiet_bin), 4)) {
  for (row_num in 1:nrow(obs.tbl_VegDiet_bin)) {
    if (obs.tbl_VegDiet_bin[row_num, col_num] == "0.00") {
      obs.tbl_VegDiet_bin[row_num, col_num] <- "N/E"
      obs.tbl_VegDiet_bin[row_num, col_num + 1] <- "N/E"
      obs.tbl_VegDiet_bin[row_num, col_num + 2] <- "N/E"
    }
  }
}  # If OR = 0.00 (model not converged), set OR, 95% CI, and p-value as N/E (not estimable)
obs.tbl_VegDiet_bin <-
  obs.tbl_VegDiet_bin[, c(1, 3, 2, 4:ncol(obs.tbl_VegDiet_bin))]  # For convenience when making tables
obs.tbl_VegDiet_bin
dim(obs.tbl_VegDiet_bin) # 17 outcomes * 2 categories = 34 obs in 3 models
write.xlsx(
  obs.tbl_VegDiet_bin,
  "results/Viva/IMP_SENS.FFQ.self_obs.tbl_VegDiet_bin.xlsx",
  overwrite = T
)
obs.tbl_VegDiet_bin <-
  read.xlsx("results/Viva/IMP_SENS.FFQ.self_obs.tbl_VegDiet_bin.xlsx")

obs.res_VegDiet_bin <- as.data.frame(obs.res_VegDiet_bin)
obs.res_VegDiet_bin$N_exp <- NA
obs.res_VegDiet_bin$N_ref <- NA
for (i in 1:nrow(obs.res_VegDiet_bin)) {
  if (obs.res_VegDiet_bin$Model[i] == "Model 1") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 3]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_bin$Model[i] == "Model 2") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 3]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_bin$Model[i] == "Model 3") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 3]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 17 outcomes * 1 category * 3 models = 51 obs.
write.xlsx(
  obs.res_VegDiet_bin,
  "results/Viva/IMP_SENS.FFQ.self_obs.res_VegDiet_bin.xlsx",
  overwrite = T
)
obs.res_VegDiet_bin <-
  read.xlsx("results/Viva/IMP_SENS.FFQ.self_obs.res_VegDiet_bin.xlsx")

################################################################################

## Forest plots - For Model 3 only

### Prepare data
obs.res_VegDiet_bin <-
  subset(obs.res_VegDiet_bin, Model == "Model 3")

obs.res_VegDiet_bin$Group <- NA
obs.res_VegDiet_bin$Group[obs.res_VegDiet_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_VegDiet_bin$Group[obs.res_VegDiet_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_VegDiet_bin$Group[obs.res_VegDiet_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_VegDiet_bin$Group <-
  factor(
    obs.res_VegDiet_bin$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_VegDiet_bin$Outcome <-
  factor(obs.res_VegDiet_bin$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin$Outcome])
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_VegDiet_bin$b <-
  as.numeric(obs.res_VegDiet_bin$b)
obs.res_VegDiet_bin$se <-
  as.numeric(obs.res_VegDiet_bin$se)
obs.res_VegDiet_bin$pval <-
  as.numeric(obs.res_VegDiet_bin$pval)

head(obs.res_VegDiet_bin)
dim(obs.res_VegDiet_bin)  # 17 outcomes * 1 category * 1 model = 17 obs.

################################################################################
### Remove some results with extremely huge 95% CIs
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin[which(obs.res_VegDiet_bin$se < 100), ]

head(obs.res_VegDiet_bin)
dim(obs.res_VegDiet_bin)  # 17 -> 13 obs.
################################################################################

### Nightingale forest plots
obs.forest_VegDiet_bin <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Exposure,
  xlab = "OR and 95% CI (ref: non-vegetarian)",
  title = "Binary outcomes in Project Viva",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_VegDiet_bin

ggsave(
  obs.forest_VegDiet_bin,
  file = "results/Viva/IMP_SENS.FFQ.self_obs.forest_VegDiet_bin.png",
  height = 4,
  width = 8
)

################################################################################

# Vegetarian diet and continuous outcomes - Linear regression

## Create a function to extract linear regression results (N, beta, 95% CI, p-value) - !!! Specifically for binary exposures !!!
IMP_extract_lin.res_bin <- function(mymodel) {
  ### Non-vegetarian (ref)
  Exposure_1 <- "Non-vegetarian (ref)"
  Beta_1 <- format(round(0.00, digits = 2), nsmall = 2)
  CI_1 <- "-"
  pval_1 <- "-"
  x_1 <- cbind(Exposure_1, Beta_1, CI_1, pval_1)
  ##############################################################################
  ### Pesco-/full vegetarian
  Exposure_2 <- "Pesco-/full vegetarian"
  Beta_2 <-
    format(round(summary(mymodel)[2, 2], digits = 2), nsmall = 2)
  CI_2 <- paste0(format(round(summary(
    mymodel, conf.int = T
  )[2, 7], digits = 2), nsmall = 2), ", ", format(round(summary(
    mymodel, conf.int = T
  )[2, 8], digits = 2), nsmall = 2))
  pval_2 <- style_pvalue(summary(mymodel)[2, 6], digits = 3)
  x_2 <- cbind(Exposure_2, Beta_2, CI_2, pval_2)
  ##############################################################################
  
  x <- rbind(x_1, x_2)
  colnames(x) <- c("Exposure", "Beta", "CI", "pval")
  
  return(x)
}

## Create empty output tables
obs.tbl_VegDiet_con <-
  c()  # For tabulated results
obs.res_VegDiet_con <-
  c() # For forestplots and meta-analysis

################################################################################

## Loop for each continuous outcome
for (myoutcome in Viva_out_con) {
  #### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 3 models for vegetarian diet
  #### Model 1 - Univariate (unadjusted) model
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <- lm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_FFQ_self"]])
  }
  mod_1_pooled <- pool(mod_1)
  x_1 <- IMP_extract_lin.res_bin(mod_1_pooled)
  y_1 <- IMP_extract_beta.SE_bin(mod_1_pooled)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_FFQ_self"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_2_pooled <- pool(mod_2)
  x_2 <- IMP_extract_lin.res_bin(mod_2_pooled)
  y_2 <- IMP_extract_beta.SE_bin(mod_2_pooled)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors (including hPDIm as the indicator for the healthfulness of diet)
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      lm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_FFQ_self"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["energy_Mat_DUR.p_con"]] + dat[[i]][["hPDIm"]]
      )
  }
  mod_3_pooled <- pool(mod_3)
  x_3 <- IMP_extract_lin.res_bin(mod_3_pooled)
  y_3 <- IMP_extract_beta.SE_bin(mod_3_pooled)
  
  ### Regression results for one outcome
  outcome_x <- as.data.frame(rep(outcome, 2))
  colnames(outcome_x) <- "Outcome"
  
  N_x <-
    as.data.frame(c(nrow(dat_1[dat_1$VegDiet_bin_FFQ_self == 0 &
                                 !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$VegDiet_bin_FFQ_self == 1 &
                                                                              !is.na(dat_1[[myoutcome]]), ])))
  colnames(N_x) <- "N"
  
  x <-
    cbind(outcome_x, N_x, x_1, x_2, x_3)  # Columns: Outcome name | N | Model 1 | Model 2 | Model 3; rows: Non-vegetarian | Pesco-/full vegetarian
  
  outcome_col <- as.data.frame(rep(outcome, 3))
  colnames(outcome_col) <- "Outcome"
  model_col <-
    as.data.frame(c("Model 1", "Model 2", "Model 3"))
  colnames(model_col) <- "Model"
  y <- cbind(outcome_col, model_col, rbind(y_1, y_2, y_3))
  
  ### Rows continuously added to the output table
  obs.tbl_VegDiet_con <- rbind(obs.tbl_VegDiet_con, x)
  
  obs.res_VegDiet_con <- rbind(obs.res_VegDiet_con, y)
}

obs.tbl_VegDiet_con
obs.res_VegDiet_con

################################################################################

## View and save results
obs.tbl_VegDiet_con <- as.data.frame(obs.tbl_VegDiet_con)
obs.tbl_VegDiet_con[, c(7, 11)] <-
  " "  # For convenience when making tables
obs.tbl_VegDiet_con <-
  obs.tbl_VegDiet_con[, c(1, 3, 2, 4:ncol(obs.tbl_VegDiet_con))]  # For convenience when making tables
obs.tbl_VegDiet_con
dim(obs.tbl_VegDiet_con)  # 2 outcomes for 2 categories in 3 models = 4 obs.
write.xlsx(
  obs.tbl_VegDiet_con,
  "results/Viva/IMP_SENS.FFQ.self_obs.tbl_VegDiet_con.xlsx",
  overwrite = T
)
obs.tbl_VegDiet_con <-
  read.xlsx("results/Viva/IMP_SENS.FFQ.self_obs.tbl_VegDiet_con.xlsx")

obs.res_VegDiet_con <- as.data.frame(obs.res_VegDiet_con)
obs.res_VegDiet_con$N_exp <- NA
obs.res_VegDiet_con$N_ref <- NA
for (i in 1:nrow(obs.res_VegDiet_con)) {
  if (obs.res_VegDiet_con$Model[i] == "Model 1") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 3]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_con$Model[i] == "Model 2") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 3]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_con$Model[i] == "Model 3") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 3]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 2 outcomes * 1 category * 3 models = 6 obs.
write.xlsx(
  obs.res_VegDiet_con,
  "results/Viva/IMP_SENS.FFQ.self_obs.res_VegDiet_con.xlsx",
  overwrite = T
)
obs.res_VegDiet_con <-
  read.xlsx("results/Viva/IMP_SENS.FFQ.self_obs.res_VegDiet_con.xlsx")

################################################################################

## Forest plots - For Model 3 only

### Prepare data
obs.res_VegDiet_con <-
  subset(obs.res_VegDiet_con, Model == "Model 3")

obs.res_VegDiet_con$Group <- NA
obs.res_VegDiet_con$Group[obs.res_VegDiet_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_VegDiet_con$Group[obs.res_VegDiet_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_VegDiet_con$Group[obs.res_VegDiet_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_VegDiet_con$Group <-
  factor(
    obs.res_VegDiet_con$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_VegDiet_con$Outcome <-
  factor(obs.res_VegDiet_con$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_con$Outcome])
obs.res_VegDiet_con <-
  obs.res_VegDiet_con %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_VegDiet_con$b <-
  as.numeric(obs.res_VegDiet_con$b)
obs.res_VegDiet_con$se <-
  as.numeric(obs.res_VegDiet_con$se)
obs.res_VegDiet_con$pval <-
  as.numeric(obs.res_VegDiet_con$pval)

head(obs.res_VegDiet_con)
dim(obs.res_VegDiet_con)  # 2 outcomes * 1 category * 1 model = 2 obs.

### Nightingale forest plots
obs.forest_VegDiet_con <- ggforestplot::forestplot(
  df = obs.res_VegDiet_con,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Exposure,
  xlab = "Beta and 95% CI (ref: non-vegetarian)",
  title = "Continuous outcomes in Project Viva",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21))

obs.forest_VegDiet_con

ggsave(
  obs.forest_VegDiet_con,
  file = "results/Viva/IMP_SENS.FFQ.self_obs.forest_VegDiet_con.png",
  height = 2,
  width = 6
)

################################################################################

# Vegetarian diet and breastfeeding duration (4 categories) - Ordinal logistic regression

## Create a function to extract regression results with SE (for ORDINAL logistic regression) - !!! Specifically for binary exposures
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

## Create a function to extract ORDINAL logistic regression results - !!! Specifically for binary exposures
IMP_extract_ord.log.res_bin <- function(mymodel) {
  ### Non-vegetarian (ref)
  Exposure_1 <- "Non-vegetarian (ref)"
  OR_1 <- format(round(1.00, digits = 2), nsmall = 2)
  CI_1 <- "-"
  pval_1 <- "-"
  x_1 <- cbind(Exposure_1, OR_1, CI_1, pval_1)
  ##############################################################################
  ### Pesco-vegetarian
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

## Model 1 - Univariate (unadjusted) model
mod_1 <- list()
for (i in 1:dat_imp$m) {
  mod_1[[i]] <-
    clm(bf_dur_4c ~ VegDiet_bin_FFQ_self, data = dat[[i]])
}
mod_1_pooled <- pool(mod_1)
x_1 <- IMP_extract_ord.log.res_bin(mod_1_pooled)
y_1 <- IMP_extract_ord.beta.SE_bin(mod_1_pooled)

## Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_2 <- list()
for (i in 1:dat_imp$m) {
  mod_2[[i]] <-
    clm(
      bf_dur_4c ~ VegDiet_bin_FFQ_self + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
      data = dat[[i]]
    )
}
mod_2_pooled <- pool(mod_2)
x_2 <- IMP_extract_ord.log.res_bin(mod_2_pooled)
y_2 <- IMP_extract_ord.beta.SE_bin(mod_2_pooled)

## Model 3 - Additionally adjusting for nutrition-related factors (including hPDIm as the indicator for the healthfulness of diet)
mod_3 <- list()
for (i in 1:dat_imp$m) {
  mod_3[[i]] <-
    clm(
      bf_dur_4c ~ VegDiet_bin_FFQ_self + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin + hPDIm,
      data = dat[[i]]
    )
}
mod_3_pooled <- pool(mod_3)
x_3 <- IMP_extract_ord.log.res_bin(mod_3_pooled)
y_3 <- IMP_extract_ord.beta.SE_bin(mod_3_pooled)

################################################################################
################################################################################

## Combine regression results
outcome <- var_lab(dat_1$bf_dur_4c)

outcome_x <- as.data.frame(rep(outcome, 2))
colnames(outcome_x) <- "Outcome"
N_x <-
  as.data.frame(c(nrow(dat_1[dat_1$VegDiet_bin_FFQ_self == 0 &
                               !is.na(dat_1$bf_dur_4c), ]), nrow(dat_1[dat_1$VegDiet_bin_FFQ_self == 1 &
                                                                         !is.na(dat_1$bf_dur_4c), ])))
colnames(N_x) <- "N"
x <-
  cbind(outcome_x, N_x, x_1, x_2, x_3)  # Columns: Outcome name | N | Model 1 | Model 2 | Model 3; rows: Non-vegetarian | Pesco-/full vegetarian

outcome_col <- as.data.frame(rep(outcome, 3))
colnames(outcome_col) <- "Outcome"
model_col <-
  as.data.frame(c("Model 1", "Model 2", "Model 3"))
colnames(model_col) <- "Model"
y <- cbind(outcome_col, model_col, rbind(y_1, y_2, y_3))

################################################################################
################################################################################

## View and save results
obs.tbl_VegDiet_ord <- as.data.frame(x)
obs.tbl_VegDiet_ord[, c(7, 11)] <-
  " "  # For convenience when making tables
obs.tbl_VegDiet_ord <-
  obs.tbl_VegDiet_ord[, c(1, 3, 2, 4:ncol(obs.tbl_VegDiet_ord))]  # For convenience when making tables
obs.tbl_VegDiet_ord
dim(obs.tbl_VegDiet_ord)  # 1 outcome for 2 categories in 3 models
write.xlsx(
  obs.tbl_VegDiet_ord,
  "results/Viva/IMP_SENS.FFQ.self_obs.tbl_VegDiet_ord.xlsx",
  overwrite = T
)
obs.tbl_VegDiet_ord <-
  read.xlsx("results/Viva/IMP_SENS.FFQ.self_obs.tbl_VegDiet_ord.xlsx")

obs.res_VegDiet_ord <- as.data.frame(y)
obs.res_VegDiet_ord$N_exp <- NA
obs.res_VegDiet_ord$N_ref <- NA
for (i in 1:nrow(obs.res_VegDiet_ord)) {
  if (obs.res_VegDiet_ord$Model[i] == "Model 1") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 3]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_ord$Model[i] == "Model 2") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 3]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_ord$Model[i] == "Model 3") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 3]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 3]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 1 outcome * 1 category * 3 models = 3 obs
write.xlsx(
  obs.res_VegDiet_ord,
  "results/Viva/IMP_SENS.FFQ.self_obs.res_VegDiet_ord.xlsx",
  overwrite = T
)
obs.res_VegDiet_ord <-
  read.xlsx("results/Viva/IMP_SENS.FFQ.self_obs.res_VegDiet_ord.xlsx")
