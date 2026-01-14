################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 12-Jun-2025
# This script is to perform paternal negative control analysis (with imputed data) for vegetarian diets in MoBa.

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
setwd("N:/durable/projects/Ran_MoBa_var")

################################################################################

# Load imputed data
load("dat_exp_cov_out_pat_IMP_PNC.RData")

## Create a list of imputed datasets
dat <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat[[i]] <- complete_dat
}

## Check the first imputed dataset
dat_1 <- dat[[1]]
head(dat_1)
dim(dat_1)  # 21952  XXX
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
## Model 1 (maternal model): VegDiet_bin + age, education, income, parity, BMI, smoking, alcohol, supplement use, offspring sex
## Model 2 (maternal model adjusted for paternal exposure): VegDiet_3cat + age, education, income, parity, BMI, smoking, alcohol, supplement use, offspring sex + VietDiet_3cat_Pat
## Model 3 (paternal model): VegDiet_bin_Pat + age, education, income, number of children, BMI, smoking, alcohol, supplement use, offspring sex
## Model 4 (paternal model adjusted for maternal exposure): VegDiet_3cat_Pat + age, education, income, number of children, BMI, smoking, alcohol, supplement use, offspring sex + VietDiet_3cat
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
  ### Pesco-vegetarian
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
for (myoutcome in MoBa_out_bin) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 4 models for vegetarian diet
  #### Model 1 (maternal model): VegDiet_bin + age, education, income, parity, BMI, smoking, alcohol, supplement use, offspring sex
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_1_pooled <- pool(mod_1)
  x_1 <- IMP_extract_log.res_bin(mod_1_pooled)
  y_1 <- IMP_extract_beta.SE_bin(mod_1_pooled)
  
  #### Model 2 (maternal model adjusted for paternal exposure): VegDiet_bin + age, education, income, parity, BMI, smoking, alcohol, supplement use, offspring sex + VietDiet_3cat_Pat
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      glm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["VegDiet_bin_Pat"]] + dat[[i]][["age_Pat_con"]] + dat[[i]][["edu_Pat_3cat"]] + dat[[i]][["parity_Pat_bin"]] + dat[[i]][["BMI_Pat_EAR.p_con"]] + dat[[i]][["smoking_Pat_EAR.p_bin"]] + dat[[i]][["alcohol_Pat_EAR.p_bin"]] + dat[[i]][["any.supp_Pat_EAR.p_bin"]],
        family = binomial
      )
  }
  mod_2_pooled <- pool(mod_2)
  x_2 <- IMP_extract_log.res_bin(mod_2_pooled)
  y_2 <- IMP_extract_beta.SE_bin(mod_2_pooled)
  
  #### Model 3 (paternal model): VegDiet_bin_Pat + age, education, income, number of children, BMI, smoking, alcohol, supplement use, offspring sex
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_Pat"]] + dat[[i]][["age_Pat_con"]] + dat[[i]][["edu_Pat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Pat_bin"]] + dat[[i]][["BMI_Pat_EAR.p_con"]] + dat[[i]][["smoking_Pat_EAR.p_bin"]] + dat[[i]][["alcohol_Pat_EAR.p_bin"]] + dat[[i]][["any.supp_Pat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]],
          family = binomial)
  }
  mod_3_pooled <- pool(mod_3)
  x_3 <- IMP_extract_log.res_bin(mod_3_pooled)
  y_3 <- IMP_extract_beta.SE_bin(mod_3_pooled)
  
  #### Model 4 (paternal model adjusted for maternal exposure): VegDiet_bin_Pat + age, education, income, number of children, BMI, smoking, alcohol, supplement use, offspring sex + VietDiet_3cat
  mod_4 <- list()
  for (i in 1:dat_imp$m) {
    mod_4[[i]] <-
      glm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_Pat"]] + dat[[i]][["age_Pat_con"]] + dat[[i]][["edu_Pat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Pat_bin"]] + dat[[i]][["BMI_Pat_EAR.p_con"]] + dat[[i]][["smoking_Pat_EAR.p_bin"]] + dat[[i]][["alcohol_Pat_EAR.p_bin"]] + dat[[i]][["any.supp_Pat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]],
        family = binomial
      )
  }
  mod_4_pooled <- pool(mod_4)
  x_4 <- IMP_extract_log.res_bin(mod_4_pooled)
  y_4 <- IMP_extract_beta.SE_bin(mod_4_pooled)
  
  ### Regression results for one outcome
  outcome_x <- as.data.frame(rep(outcome, 2))
  colnames(outcome_x) <- "Outcome"
  
  N_x_Mat <-
    as.data.frame(c(paste0(nrow(dat_1[dat_1$VegDiet_bin == 0 &
                                        !is.na(dat_1[[myoutcome]]) &
                                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$VegDiet_bin == 0 &
                                                                                         !is.na(dat_1[[myoutcome]]), ])), paste0(nrow(dat_1[dat_1$VegDiet_bin == 1 &
                                                                                                                                              !is.na(dat_1[[myoutcome]]) &
                                                                                                                                              dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$VegDiet_bin == 1 &
                                                                                                                                                                                               !is.na(dat_1[[myoutcome]]), ]))))
  colnames(N_x_Mat) <- "N_Mat"
  
  N_x_Pat <-
    as.data.frame(c(paste0(nrow(dat_1[dat_1$VegDiet_bin_Pat == 0 &
                                        !is.na(dat_1[[myoutcome]]) &
                                        dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$VegDiet_bin_Pat == 0 &
                                                                                         !is.na(dat_1[[myoutcome]]), ])), paste0(nrow(dat_1[dat_1$VegDiet_bin_Pat == 1 &
                                                                                                                                              !is.na(dat_1[[myoutcome]]) &
                                                                                                                                              dat_1[[myoutcome]] == 1, ]), " / ", nrow(dat_1[dat_1$VegDiet_bin_Pat == 1 &
                                                                                                                                                                                               !is.na(dat_1[[myoutcome]]), ]))))
  colnames(N_x_Pat) <- "N_Pat"
  
  x <-
    cbind(outcome_x, N_x_Mat, x_1, x_2, N_x_Pat, x_3, x_4)  # Columns: Outcome name | N | Model 1 | Model 2 | N | Model 3 | Model 4; rows: Non-vegetarian | Pesco-/full vegetarian
  
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
  
  ### Rows continuously added to the output table
  obs.tbl_VegDiet_bin <- rbind(obs.tbl_VegDiet_bin, x)
  
  obs.res_VegDiet_bin <- rbind(obs.res_VegDiet_bin, y)
}

obs.tbl_VegDiet_bin
obs.res_VegDiet_bin

################################################################################

## View and save results
obs.tbl_VegDiet_bin <- as.data.frame(obs.tbl_VegDiet_bin)
obs.tbl_VegDiet_bin[, c(7, 12, 16)] <-
  " "  # For convenience when making tables
for (col_num in c(4, 8, 13, 17)) {
  for (row_num in 1:nrow(obs.tbl_VegDiet_bin)) {
    if (obs.tbl_VegDiet_bin[row_num, col_num] == "0.00") {
      obs.tbl_VegDiet_bin[row_num, col_num] <- "N/E"
      obs.tbl_VegDiet_bin[row_num, col_num + 1] <- "N/E"
      obs.tbl_VegDiet_bin[row_num, col_num + 2] <- "N/E"
    }
  }
}  # If OR = 0.00 (model not converged), set OR, 95% CI, and p-value as N/E (not estimable)
obs.tbl_VegDiet_bin <-
  obs.tbl_VegDiet_bin[, c(1, 3, 2, 4:10, 12, 11, 13:ncol(obs.tbl_VegDiet_bin))]  # For convenience when making tables
obs.tbl_VegDiet_bin
dim(obs.tbl_VegDiet_bin) # 21 outcomes * 2 categories = 42 obs in 3 models
write.xlsx(obs.tbl_VegDiet_bin,
           "results/IMP_PNC_obs.tbl_VegDiet_bin.xlsx",
           overwrite = T)
obs.tbl_VegDiet_bin <-
  read.xlsx("results/IMP_PNC_obs.tbl_VegDiet_bin.xlsx")

obs.res_VegDiet_bin <- as.data.frame(obs.res_VegDiet_bin)
obs.res_VegDiet_bin$N_exp <- NA
obs.res_VegDiet_bin$N_ref <- NA
for (i in 1:nrow(obs.res_VegDiet_bin)) {
  if (obs.res_VegDiet_bin$Model[i] == "Maternal Model 1") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 3]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_bin$Model[i] == "Maternal Model 2") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 3]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_bin$Model[i] == "Paternal Model 1") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 12]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 12]
  } else if (obs.res_VegDiet_bin$Model[i] == "Paternal Model 2") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 12]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 12]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 21 outcomes * 1 category * 4 models = 84 obs
write.xlsx(obs.res_VegDiet_bin,
           "results/IMP_PNC_obs.res_VegDiet_bin.xlsx",
           overwrite = T)
obs.res_VegDiet_bin <-
  read.xlsx("results/IMP_PNC_obs.res_VegDiet_bin.xlsx")

################################################################################

## Forest plots

### Prepare data
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

obs.res_VegDiet_bin$Model <-
  factor(
    obs.res_VegDiet_bin$Model,
    levels = c(
      "Paternal Model 2",
      "Paternal Model 1",
      "Maternal Model 2",
      "Maternal Model 1"
    )
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
dim(obs.res_VegDiet_bin)  # 21 outcomes * 1 category * 4 models = 84 obs

################################################################################
### Remove some results with extremely huge 95% CIs
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin[which(obs.res_VegDiet_bin$se < 100), ]

head(obs.res_VegDiet_bin)
dim(obs.res_VegDiet_bin)  # 84 -> 80 obs
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
  shape = Model,
  xlab = "OR and 95% CI (ref: non-vegetarian)",
  title = "Binary outcomes in MoBa",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(25, 24, 23, 22)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_VegDiet_bin

ggsave(
  obs.forest_VegDiet_bin,
  file = "results/IMP_PNC_obs.forest_VegDiet_bin.png",
  height = 18,
  width = 7
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
  ### Pesco-vegetarian
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
for (myoutcome in MoBa_out_con) {
  #### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 4 models for vegetarian diet
  #### Model 1 (maternal model): VegDiet_bin + age, education, income, parity, BMI, smoking, alcohol, supplement use, offspring sex
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_1_pooled <- pool(mod_1)
  x_1 <- IMP_extract_lin.res_bin(mod_1_pooled)
  y_1 <- IMP_extract_beta.SE_bin(mod_1_pooled)
  
  #### Model 2 (maternal model adjusted for paternal exposure): VegDiet_bin + age, education, income, parity, BMI, smoking, alcohol, supplement use, offspring sex + VietDiet_3cat_Pat
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      lm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["edu_Mat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["VegDiet_bin_Pat"]] + dat[[i]][["age_Pat_con"]] + dat[[i]][["edu_Pat_3cat"]] + dat[[i]][["parity_Pat_bin"]] + dat[[i]][["BMI_Pat_EAR.p_con"]] + dat[[i]][["smoking_Pat_EAR.p_bin"]] + dat[[i]][["alcohol_Pat_EAR.p_bin"]] + dat[[i]][["any.supp_Pat_EAR.p_bin"]]
      )
  }
  mod_2_pooled <- pool(mod_2)
  x_2 <- IMP_extract_lin.res_bin(mod_2_pooled)
  y_2 <- IMP_extract_beta.SE_bin(mod_2_pooled)
  
  #### Model 3 (paternal model): VegDiet_bin_Pat + age, education, income, number of children, BMI, smoking, alcohol, supplement use, offspring sex
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_Pat"]] + dat[[i]][["age_Pat_con"]] + dat[[i]][["edu_Pat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Pat_bin"]] + dat[[i]][["BMI_Pat_EAR.p_con"]] + dat[[i]][["smoking_Pat_EAR.p_bin"]] + dat[[i]][["alcohol_Pat_EAR.p_bin"]] + dat[[i]][["any.supp_Pat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]])
  }
  mod_3_pooled <- pool(mod_3)
  x_3 <- IMP_extract_lin.res_bin(mod_3_pooled)
  y_3 <- IMP_extract_beta.SE_bin(mod_3_pooled)
  
  #### Model 4 (paternal model adjusted for maternal exposure): VegDiet_bin_Pat + age, education, income, number of children, BMI, smoking, alcohol, supplement use, offspring sex + VietDiet_3cat
  mod_4 <- list()
  for (i in 1:dat_imp$m) {
    mod_4[[i]] <-
      lm(
        dat[[i]][[myoutcome]] ~ dat[[i]][["VegDiet_bin_Pat"]] + dat[[i]][["age_Pat_con"]] + dat[[i]][["edu_Pat_3cat"]]  + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Pat_bin"]] + dat[[i]][["BMI_Pat_EAR.p_con"]] + dat[[i]][["smoking_Pat_EAR.p_bin"]] + dat[[i]][["alcohol_Pat_EAR.p_bin"]] + dat[[i]][["any.supp_Pat_EAR.p_bin"]] + dat[[i]][["sex_Chi_bin"]] + dat[[i]][["VegDiet_bin"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["parity_Mat_bin"]] + dat[[i]][["BMI_Mat_PRE.p_con"]] + dat[[i]][["smoking_Mat_EAR.p_bin"]] + dat[[i]][["alcohol_Mat_EAR.p_bin"]] + dat[[i]][["any.supp_Mat_EAR.p_bin"]]
      )
  }
  mod_4_pooled <- pool(mod_4)
  x_4 <- IMP_extract_lin.res_bin(mod_4_pooled)
  y_4 <- IMP_extract_beta.SE_bin(mod_4_pooled)
  
  ### Regression results for one outcome
  outcome_x <- as.data.frame(rep(outcome, 2))
  colnames(outcome_x) <- "Outcome"
  
  N_x_Mat <-
    as.data.frame(c(nrow(dat_1[dat_1$VegDiet_bin == 0 &
                                 !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$VegDiet_bin == 1 &
                                                                              !is.na(dat_1[[myoutcome]]), ])))
  colnames(N_x_Mat) <- "N_Mat"
  
  N_x_Pat <-
    as.data.frame(c(nrow(dat_1[dat_1$VegDiet_bin_Pat == 0 &
                                 !is.na(dat_1[[myoutcome]]), ]), nrow(dat_1[dat_1$VegDiet_bin_Pat == 1 &
                                                                              !is.na(dat_1[[myoutcome]]), ])))
  
  colnames(N_x_Pat) <- "N_Pat"
  
  x <-
    cbind(outcome_x, N_x_Mat, x_1, x_2, N_x_Pat, x_3, x_4)  # Columns: Outcome name | N | Model 1 | Model 2 | N | Model 3 | Model 4; rows: Non-vegetarian | Pesco-/full vegetarian
  
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
  
  ### Rows continuously added to the output table
  obs.tbl_VegDiet_con <- rbind(obs.tbl_VegDiet_con, x)
  
  obs.res_VegDiet_con <- rbind(obs.res_VegDiet_con, y)
}

obs.tbl_VegDiet_con
obs.res_VegDiet_con

################################################################################

## View and save results
obs.tbl_VegDiet_con <- as.data.frame(obs.tbl_VegDiet_con)
obs.tbl_VegDiet_con[, c(7, 12, 16)] <-
  " "  # For convenience when making tables
obs.tbl_VegDiet_con <-
  obs.tbl_VegDiet_con[, c(1, 3, 2, 4:10, 12, 11, 13:ncol(obs.tbl_VegDiet_con))]  # For convenience when making tables
obs.tbl_VegDiet_con
dim(obs.tbl_VegDiet_con)  # 4 outcomes for 2 categories in 4 models = 8 obs.
write.xlsx(obs.tbl_VegDiet_con,
           "results/IMP_PNC_obs.tbl_VegDiet_con.xlsx",
           overwrite = T)
obs.tbl_VegDiet_con <-
  read.xlsx("results/IMP_PNC_obs.tbl_VegDiet_con.xlsx")

obs.res_VegDiet_con <- as.data.frame(obs.res_VegDiet_con)
obs.res_VegDiet_con$N_exp <- NA
obs.res_VegDiet_con$N_ref <- NA
for (i in 1:nrow(obs.res_VegDiet_con)) {
  if (obs.res_VegDiet_con$Model[i] == "Maternal Model 1") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 3]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_con$Model[i] == "Maternal Model 2") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 3]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_con$Model[i] == "Paternal Model 1") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 12]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 12]
  } else if (obs.res_VegDiet_con$Model[i] == "Paternal Model 2") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 12]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 12]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 4 outcomes * 1 category * 4 models = 16 obs.
write.xlsx(obs.res_VegDiet_con,
           "results/IMP_PNC_obs.res_VegDiet_con.xlsx",
           overwrite = T)
obs.res_VegDiet_con <-
  read.xlsx("results/IMP_PNC_obs.res_VegDiet_con.xlsx")

################################################################################

## Forest plots - For Model 3 only

### Prepare data
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

obs.res_VegDiet_con$Model <-
  factor(
    obs.res_VegDiet_con$Model,
    levels = c(
      "Paternal Model 2",
      "Paternal Model 1",
      "Maternal Model 2",
      "Maternal Model 1"
    )
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
dim(obs.res_VegDiet_con)  # 4 outcomes * 1 category * 4 models = 16 obs.

### Nightingale forest plots
obs.forest_VegDiet_con <- ggforestplot::forestplot(
  df = obs.res_VegDiet_con,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Model,
  xlab = "Beta and 95% CI (ref: non-vegetarian)",
  title = "Continuous outcomes in MoBa",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(25, 24, 23, 22))

obs.forest_VegDiet_con

ggsave(
  obs.forest_VegDiet_con,
  file = "results/IMP_PNC_obs.forest_VegDiet_con.png",
  height = 5,
  width = 6
)
