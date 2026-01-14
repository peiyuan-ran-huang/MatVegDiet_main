################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - BiB        #
################################################################################

# Last edited date: 07-May-2025
# This script is to perform cross-context comparison analysis (with imputed data) for vegetarian diets in BiB.
## Part 2: Comparison across ALL ethnic groups

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
  mice,
  gridExtra,
  cowplot,
  grid
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load imputed data
load("data/BiB/dat_exp_cov_out_IMP.RData")

## Create a list of imputed datasets (!!! and separate the 3-category exposure VegDiet_bin into two binary exposures for confounding structure check later)
dat <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat[[i]] <- complete_dat
  var_lab(dat[[i]]$VegDiet_bin) <- "Pesco-/full vs. non-vegetarian"
}

################################################################################
### Stratify by ethnicity

#### White British (!!! and separate the 3-category exposure VegDiet_bin into two binary exposures for confounding structure check later)
dat_WB <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat_WB[[i]] <- subset(complete_dat, ethnic_Mat_cat == "White British")
  var_lab(dat_WB[[i]]$VegDiet_bin) <- "Pesco-/full vs. non-vegetarian"
}

#### South Asian (!!! and separate the 3-category exposure VegDiet_bin into two binary exposures for confounding structure check later)
dat_SA <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat_SA[[i]] <- subset(complete_dat, ethnic_Mat_cat == "Pakistani")
  var_lab(dat_SA[[i]]$VegDiet_bin) <- "Pesco-/full vs. non-vegetarian"
}

#### Other (!!! and separate the 3-category exposure VegDiet_bin into two binary exposures for confounding structure check later)
dat_other <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat_other[[i]] <- subset(complete_dat, ethnic_Mat_cat == "Other")
  var_lab(dat_other[[i]]$VegDiet_bin) <- "Pesco-/full vs. non-vegetarian"
}
################################################################################

## Check the first imputed dataset
dat_1 <- dat[[1]]
head(dat_1)
dim(dat_1)  # 3647   XX
for (varname in colnames(dat_1)) {
  print(varname)
  print(sum(is.na(dat_1[[varname]])))
}

################################################################################
dat_WB_1 <- dat_WB[[1]]
head(dat_WB_1)
dim(dat_WB_1)  # 1793
for (varname in colnames(dat_WB_1)) {
  print(varname)
  print(sum(is.na(dat_WB_1[[varname]])))
}

dat_SA_1 <- dat_SA[[1]]
head(dat_SA_1)
dim(dat_SA_1)  # 1239
for (varname in colnames(dat_SA_1)) {
  print(varname)
  print(sum(is.na(dat_SA_1[[varname]])))
}

dat_other_1 <- dat_other[[1]]
head(dat_other_1)
dim(dat_other_1)  # 613
for (varname in colnames(dat_other_1)) {
  print(varname)
  print(sum(is.na(dat_other_1[[varname]])))
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
BiB_primary_bin <-
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat_1))]
BiB_primary_bin  # 11 primary (binary) outcomes available in BiB

BiB_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat_1))]
BiB_secondary_bin  # 8 secondary binary outcomes available in BiB

BiB_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat_1))]
BiB_secondary_con  # 4 secondary continuous outcomes available in BiB

BiB_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat_1))]
BiB_secondary_cat  # 1 (primary) ordinal/categorical outcome available in BiB

## Group outcome variables
BiB_out_bin <- c(BiB_primary_bin, BiB_secondary_bin)
BiB_out_con <- BiB_secondary_con
BiB_out_cat <- BiB_secondary_cat

################################################################################
# Modelling
## Model 1 (for White British): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
## Model 2 (for South Asians): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
## Model 3 (for Other): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
################################################################################

# Create a function to extract beta, SE, and p-value (for both linear and logistic regression) - !!! Specifically for binary exposures !!!
IMP_extract_beta.SE <- function(mymodel) {
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
for (myoutcome in BiB_out_bin) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 3 models for vegetarian diet
  #### Model 1 (for White British): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(
        dat_WB[[i]][[myoutcome]] ~ dat_WB[[i]][["VegDiet_bin"]] + dat_WB[[i]][["age_Mat_con"]] + dat_WB[[i]][["edu_Mat_3cat"]]  + dat_WB[[i]][["IMD_Fam_cat"]] + dat_WB[[i]][["parity_Mat_bin"]] + dat_WB[[i]][["BMI_Mat_PRE.p_con"]] + dat_WB[[i]][["smoking_Mat_EAR.p_bin"]] + dat_WB[[i]][["alcohol_Mat_EAR.p_bin"]] + dat_WB[[i]][["sex_Chi_bin"]] + dat_WB[[i]][["any.supp_Mat_EAR.p_bin"]],
        family = binomial
      )
  }
  mod_1_pooled <- pool(mod_1)
  x_1 <- IMP_extract_log.res_bin(mod_1_pooled)
  y_1 <- IMP_extract_beta.SE(mod_1_pooled)
  
  #### Model 2 (for South Asians): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      glm(
        dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["VegDiet_bin"]] + dat_SA[[i]][["age_Mat_con"]] + dat_SA[[i]][["edu_Mat_3cat"]]  + dat_SA[[i]][["IMD_Fam_cat"]] + dat_SA[[i]][["parity_Mat_bin"]] + dat_SA[[i]][["BMI_Mat_PRE.p_con"]] + dat_SA[[i]][["smoking_Mat_EAR.p_bin"]] + dat_SA[[i]][["alcohol_Mat_EAR.p_bin"]] + dat_SA[[i]][["sex_Chi_bin"]] + dat_SA[[i]][["any.supp_Mat_EAR.p_bin"]],
        family = binomial
      )
  }
  mod_2_pooled <- pool(mod_2)
  x_2 <- IMP_extract_log.res_bin(mod_2_pooled)
  y_2 <- IMP_extract_beta.SE(mod_2_pooled)
  
  #### Model 3 (for Other): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      glm(
        dat_other[[i]][[myoutcome]] ~ dat_other[[i]][["VegDiet_bin"]] + dat_other[[i]][["age_Mat_con"]] + dat_other[[i]][["edu_Mat_3cat"]]  + dat_other[[i]][["IMD_Fam_cat"]] + dat_other[[i]][["parity_Mat_bin"]] + dat_other[[i]][["BMI_Mat_PRE.p_con"]] + dat_other[[i]][["smoking_Mat_EAR.p_bin"]] + dat_other[[i]][["alcohol_Mat_EAR.p_bin"]] + dat_other[[i]][["sex_Chi_bin"]] + dat_other[[i]][["any.supp_Mat_EAR.p_bin"]],
        family = binomial
      )
  }
  mod_3_pooled <- pool(mod_3)
  x_3 <- IMP_extract_log.res_bin(mod_3_pooled)
  y_3 <- IMP_extract_beta.SE(mod_3_pooled)
  
  ### Regression results for one outcome
  outcome_x <- as.data.frame(rep(outcome, 2))
  colnames(outcome_x) <- "Outcome"
  
  N_x_WB <- as.data.frame(c(paste0(nrow(dat_WB_1[dat_WB_1$VegDiet_bin == 0 &
                                                   !is.na(dat_WB_1[[myoutcome]]) &
                                                   dat_WB_1[[myoutcome]] == 1, ]), " / ", nrow(dat_WB_1[dat_WB_1$VegDiet_bin == 0 &
                                                                                                          !is.na(dat_WB_1[[myoutcome]]), ])), paste0(nrow(dat_WB_1[dat_WB_1$VegDiet_bin == 1 &
                                                                                                                                                                     !is.na(dat_WB_1[[myoutcome]]) &
                                                                                                                                                                     dat_WB_1[[myoutcome]] == 1, ]), " / ", nrow(dat_WB_1[dat_WB_1$VegDiet_bin == 1 &
                                                                                                                                                                                                                            !is.na(dat_WB_1[[myoutcome]]), ]))))
  colnames(N_x_WB) <- "N_WB"
  
  N_x_SA <- as.data.frame(c(paste0(nrow(dat_SA_1[dat_SA_1$VegDiet_bin == 0 &
                                                   !is.na(dat_SA_1[[myoutcome]]) &
                                                   dat_SA_1[[myoutcome]] == 1, ]), " / ", nrow(dat_SA_1[dat_SA_1$VegDiet_bin == 0 &
                                                                                                          !is.na(dat_SA_1[[myoutcome]]), ])), paste0(nrow(dat_SA_1[dat_SA_1$VegDiet_bin == 1 &
                                                                                                                                                                     !is.na(dat_SA_1[[myoutcome]]) &
                                                                                                                                                                     dat_SA_1[[myoutcome]] == 1, ]), " / ", nrow(dat_SA_1[dat_SA_1$VegDiet_bin == 1 &
                                                                                                                                                                                                                            !is.na(dat_SA_1[[myoutcome]]), ]))))
  colnames(N_x_SA) <- "N_SA"
  
  N_x_other <- as.data.frame(c(paste0(
    nrow(dat_other_1[dat_other_1$VegDiet_bin == 0 &
                       !is.na(dat_other_1[[myoutcome]]) &
                       dat_other_1[[myoutcome]] == 1, ]), " / ", nrow(dat_other_1[dat_other_1$VegDiet_bin == 0 &
                                                                                    !is.na(dat_other_1[[myoutcome]]), ])
  ), paste0(
    nrow(dat_other_1[dat_other_1$VegDiet_bin == 1 &
                       !is.na(dat_other_1[[myoutcome]]) &
                       dat_other_1[[myoutcome]] == 1, ]), " / ", nrow(dat_other_1[dat_other_1$VegDiet_bin == 1 &
                                                                                    !is.na(dat_other_1[[myoutcome]]), ])
  )))
  colnames(N_x_other) <- "N_other"
  
  x <-
    cbind(outcome_x, N_x_WB, x_1, N_x_SA, x_2, N_x_other, x_3)  # Columns: Outcome name | N | Model 1 | N | Model 2 | N | Model 3; rows: Non-vegetarian | Pesco-/full vegetarian
  
  outcome_col <- as.data.frame(rep(outcome, 3))
  colnames(outcome_col) <- "Outcome"
  model_col <-
    as.data.frame(c("White British", "South Asian", "Other"))
  colnames(model_col) <- "Ethnicity"
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
obs.tbl_VegDiet_bin[, 8] <-
  " "  # For convenience when making tables
for (col_num in c(4, 9)) {
  for (row_num in 1:nrow(obs.tbl_VegDiet_bin)) {
    if (obs.tbl_VegDiet_bin[row_num, col_num] == "0.00") {
      obs.tbl_VegDiet_bin[row_num, col_num] <- "N/E"
      obs.tbl_VegDiet_bin[row_num, col_num + 1] <- "N/E"
      obs.tbl_VegDiet_bin[row_num, col_num + 2] <- "N/E"
    }
  }
}  # If OR = 0.00 (model not converged), set OR, 95% CI, and p-value as N/E (not estimable)
obs.tbl_VegDiet_bin <-
  obs.tbl_VegDiet_bin[, c(1, 3, 2, 4:6, 8, 7, 9:11, 13, 12, 14:ncol(obs.tbl_VegDiet_bin))]  # For convenience when making tables
obs.tbl_VegDiet_bin
dim(obs.tbl_VegDiet_bin) # 19 outcomes * 2 categories = 38 obs in 3 models
# write.xlsx(obs.tbl_VegDiet_bin,
#            "results/BiB/IMP_CCC_obs.tbl_VegDiet_bin.xlsx",
#            overwrite = T)
# obs.tbl_VegDiet_bin <-
#   read.xlsx("results/BiB/IMP_CCC_obs.tbl_VegDiet_bin.xlsx")

obs.res_VegDiet_bin <- as.data.frame(obs.res_VegDiet_bin)
obs.res_VegDiet_bin$N_exp <- NA
obs.res_VegDiet_bin$N_ref <- NA
for (i in 1:nrow(obs.res_VegDiet_bin)) {
  if (obs.res_VegDiet_bin$Ethnicity[i] == "White British") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 3]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_bin$Ethnicity[i] == "South Asian") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 8]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 8]
  } else if (obs.res_VegDiet_bin$Ethnicity[i] == "Other") {
    obs.res_VegDiet_bin$N_exp[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == obs.res_VegDiet_bin$Exposure[i], 13]
    obs.res_VegDiet_bin$N_ref[i] <-
      obs.tbl_VegDiet_bin[obs.tbl_VegDiet_bin$Outcome == obs.res_VegDiet_bin$Outcome[i] &
                            obs.tbl_VegDiet_bin$Exposure == "Non-vegetarian (ref)", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 19 outcomes * 1 category * 3 models = 57 obs
# write.xlsx(obs.res_VegDiet_bin,
#            "results/BiB/IMP_CCC_obs.res_VegDiet_bin.xlsx",
#            overwrite = T)
# obs.res_VegDiet_bin <-
#   read.xlsx("results/BiB/IMP_CCC_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_ALL <- read.xlsx("results/BiB/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_ALL <- subset(obs.res_VegDiet_bin_ALL, Model == "Model 3")
obs.res_VegDiet_bin_ALL$Model <- "Whole cohort"
colnames(obs.res_VegDiet_bin_ALL)[colnames(obs.res_VegDiet_bin_ALL) == "Model"] <- "Ethnicity"
obs.res_VegDiet_bin_ALL

obs.res_VegDiet_bin <- rbind(obs.res_VegDiet_bin, obs.res_VegDiet_bin_ALL)
obs.res_VegDiet_bin

################################################################################
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

obs.res_VegDiet_bin$Ethnicity <-
  factor(
    obs.res_VegDiet_bin$Ethnicity,
    levels = c("Other", "South Asian", "White British", "Whole cohort")
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
dim(obs.res_VegDiet_bin)  # 19 outcomes * 1 category * 4 models = 76 obs

################################################################################
### Remove some results with extremely huge 95% CIs
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin[which(obs.res_VegDiet_bin$se < 100), ]
head(obs.res_VegDiet_bin)
dim(obs.res_VegDiet_bin)  # 76 -> 57 obs
################################################################################
# ### Remove if results only available in one ethnicity for any outcome and exposure
# obs.res_VegDiet_bin <- obs.res_VegDiet_bin %>% group_by(Outcome, Exposure) %>% filter(n() == 2) %>% ungroup()
# head(obs.res_VegDiet_bin)
# dim(obs.res_VegDiet_bin)  # 26 -> 20 obs
################################################################################

# ### Nightingale forest plots
# obs.forest_VegDiet_bin <- ggforestplot::forestplot(
#   df = obs.res_VegDiet_bin,
#   name = Outcome,
#   estimate = b,
#   se = se,
#   pvalue = pval,
#   psignif = 0.05,
#   colour = Exposure,
#   shape = Ethnicity,
#   xlab = "OR and 95% CI (ref: non-vegetarian)",
#   logodds = T
# ) +
#   ggplot2::scale_colour_manual(values = c("darkcyan")) +
#   ggplot2::scale_shape_manual(values = c(25, 24))
#
# obs.forest_VegDiet_bin
#
# ggsave(
#   obs.forest_VegDiet_bin,
#   file = "results/BiB/IMP_CCC_obs.forest_VegDiet_bin.png",
#   height = 5,
#   width = 8
# )

################################################################################

# Vegetarian diet and continuous outcomes - Linear regression

## Create a function to extract linear regression results (N, beta, 95% CI, p-value)
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
for (myoutcome in BiB_out_con) {
  #### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 3 models for vegetarian diet
  #### Model 1 (for White British): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <- lm(
      dat_WB[[i]][[myoutcome]] ~ dat_WB[[i]][["VegDiet_bin"]] + dat_WB[[i]][["age_Mat_con"]] + dat_WB[[i]][["edu_Mat_3cat"]]  + dat_WB[[i]][["IMD_Fam_cat"]] + dat_WB[[i]][["parity_Mat_bin"]] + dat_WB[[i]][["BMI_Mat_PRE.p_con"]] + dat_WB[[i]][["smoking_Mat_EAR.p_bin"]] + dat_WB[[i]][["alcohol_Mat_EAR.p_bin"]] + dat_WB[[i]][["sex_Chi_bin"]] + dat_WB[[i]][["any.supp_Mat_EAR.p_bin"]]
    )
  }
  mod_1_pooled <- pool(mod_1)
  x_1 <- IMP_extract_lin.res_bin(mod_1_pooled)
  y_1 <- IMP_extract_beta.SE(mod_1_pooled)
  
  #### Model 2 (for South Asians): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <- lm(
      dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["VegDiet_bin"]] + dat_SA[[i]][["age_Mat_con"]] + dat_SA[[i]][["edu_Mat_3cat"]]  + dat_SA[[i]][["IMD_Fam_cat"]] + dat_SA[[i]][["parity_Mat_bin"]] + dat_SA[[i]][["BMI_Mat_PRE.p_con"]] + dat_SA[[i]][["smoking_Mat_EAR.p_bin"]] + dat_SA[[i]][["alcohol_Mat_EAR.p_bin"]] + dat_SA[[i]][["sex_Chi_bin"]] + dat_SA[[i]][["any.supp_Mat_EAR.p_bin"]]
    )
  }
  mod_2_pooled <- pool(mod_2)
  x_2 <- IMP_extract_lin.res_bin(mod_2_pooled)
  y_2 <- IMP_extract_beta.SE(mod_2_pooled)
  
  #### Model 3 (for Other): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <- lm(
      dat_other[[i]][[myoutcome]] ~ dat_other[[i]][["VegDiet_bin"]] + dat_other[[i]][["age_Mat_con"]] + dat_other[[i]][["edu_Mat_3cat"]]  + dat_other[[i]][["IMD_Fam_cat"]] + dat_other[[i]][["parity_Mat_bin"]] + dat_other[[i]][["BMI_Mat_PRE.p_con"]] + dat_other[[i]][["smoking_Mat_EAR.p_bin"]] + dat_other[[i]][["alcohol_Mat_EAR.p_bin"]] + dat_other[[i]][["sex_Chi_bin"]] + dat_other[[i]][["any.supp_Mat_EAR.p_bin"]]
    )
  }
  mod_3_pooled <- pool(mod_3)
  x_3 <- IMP_extract_lin.res_bin(mod_3_pooled)
  y_3 <- IMP_extract_beta.SE(mod_3_pooled)
  
  ### Regression results for one outcome
  outcome_x <- as.data.frame(rep(outcome, 2))
  colnames(outcome_x) <- "Outcome"
  
  N_x_WB <-
    as.data.frame(c(nrow(dat_WB_1[dat_WB_1$VegDiet_bin == 0 &
                                    !is.na(dat_WB_1[[myoutcome]]), ]), nrow(dat_WB_1[dat_WB_1$VegDiet_bin == 1 &
                                                                                       !is.na(dat_WB_1[[myoutcome]]), ])))
  colnames(N_x_WB) <- "N_WB"
  
  N_x_SA <-
    as.data.frame(c(nrow(dat_SA_1[dat_SA_1$VegDiet_bin == 0 &
                                    !is.na(dat_SA_1[[myoutcome]]), ]), nrow(dat_SA_1[dat_SA_1$VegDiet_bin == 1 &
                                                                                       !is.na(dat_SA_1[[myoutcome]]), ])))
  
  colnames(N_x_SA) <- "N_SA"
  
  N_x_other <-
    as.data.frame(c(nrow(dat_other_1[dat_other_1$VegDiet_bin == 0 &
                                       !is.na(dat_other_1[[myoutcome]]), ]), nrow(dat_other_1[dat_other_1$VegDiet_bin == 1 &
                                                                                                !is.na(dat_other_1[[myoutcome]]), ])))
  colnames(N_x_other) <- "N_other"
  
  x <-
    cbind(outcome_x, N_x_WB, x_1, N_x_SA, x_2, N_x_other, x_3)  # Columns: Outcome name | N | Model 1 | N | Model 2 | N | Model 3; rows: Non-vegetarian | Pesco-/full vegetarian
  
  outcome_col <- as.data.frame(rep(outcome, 3))
  colnames(outcome_col) <- "Outcome"
  model_col <-
    as.data.frame(c("White British", "South Asian", "Other"))
  colnames(model_col) <- "Ethnicity"
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
obs.tbl_VegDiet_con[, 8] <-
  " "  # For convenience when making tables
obs.tbl_VegDiet_con <-
  obs.tbl_VegDiet_con[, c(1, 3, 2, 4:6, 8, 7, 9:11, 13, 12, 14:ncol(obs.tbl_VegDiet_con))]  # For convenience when making tables
obs.tbl_VegDiet_con
dim(obs.tbl_VegDiet_con)  # 4 outcomes for 2 categories in 2 models = 8 obs.
# write.xlsx(obs.tbl_VegDiet_con,
#            "results/BiB/IMP_CCC_obs.tbl_VegDiet_con.xlsx",
#            overwrite = T)
# obs.tbl_VegDiet_con <-
#   read.xlsx("results/BiB/IMP_CCC_obs.tbl_VegDiet_con.xlsx")

obs.res_VegDiet_con <- as.data.frame(obs.res_VegDiet_con)
obs.res_VegDiet_con$N_exp <- NA
obs.res_VegDiet_con$N_ref <- NA
for (i in 1:nrow(obs.res_VegDiet_con)) {
  if (obs.res_VegDiet_con$Ethnicity[i] == "White British") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 3]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_con$Ethnicity[i] == "South Asian") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 8]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 8]
  } else if (obs.res_VegDiet_con$Ethnicity[i] == "Other") {
    obs.res_VegDiet_con$N_exp[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == obs.res_VegDiet_con$Exposure[i], 13]
    obs.res_VegDiet_con$N_ref[i] <-
      obs.tbl_VegDiet_con[obs.tbl_VegDiet_con$Outcome == obs.res_VegDiet_con$Outcome[i] &
                            obs.tbl_VegDiet_con$Exposure == "Non-vegetarian (ref)", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 4 outcomes * 1 category * 3 models = 12 obs.
# write.xlsx(obs.res_VegDiet_con,
#            "results/BiB/IMP_CCC_obs.res_VegDiet_con.xlsx",
#            overwrite = T)
# obs.res_VegDiet_con <-
#   read.xlsx("results/BiB/IMP_CCC_obs.res_VegDiet_con.xlsx")
obs.res_VegDiet_con_ALL <- read.xlsx("results/BiB/IMP_MAIN_obs.res_VegDiet_con.xlsx")
obs.res_VegDiet_con_ALL <- subset(obs.res_VegDiet_con_ALL, Model == "Model 3")
obs.res_VegDiet_con_ALL$Model <- "Whole cohort"
colnames(obs.res_VegDiet_con_ALL)[colnames(obs.res_VegDiet_con_ALL) == "Model"] <- "Ethnicity"
obs.res_VegDiet_con_ALL

obs.res_VegDiet_con <- rbind(obs.res_VegDiet_con, obs.res_VegDiet_con_ALL)
obs.res_VegDiet_con

################################################################################
################################################################################

## Forest plots

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

obs.res_VegDiet_con$Ethnicity <-
  factor(
    obs.res_VegDiet_con$Ethnicity,
    levels = c("Other", "South Asian", "White British", "Whole cohort")
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

obs.res_VegDiet_con$Type <- "Secondary outcome\n(continuous)"

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
  shape = Ethnicity,
  xlab = "Beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(22, 25, 24, 23)) +
  ggforce::facet_col(facets = ~ Type,
                     scales = "free_y",
                     space = "free")

obs.forest_VegDiet_con

ggsave(
  obs.forest_VegDiet_con,
  file = "results/BiB/IMP_CCC.ALL_obs.forest_VegDiet_con.png",
  height = 5,
  width = 6
)

################################################################################

# Vegetarian diet and categorical/ordinal outcomes - ORDINAL logistic regression

## Create a function to extract regression results with SE (for ORDINAL logistic regression)
IMP_extract_ord.beta.SE <- function(mymodel) {
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

## Create empty output tables
obs.tbl_VegDiet_ord <-
  c()  # For tabulated results
obs.res_VegDiet_ord <-
  c() # For forestplots and meta-analysis

################################################################################

## Loop for each categorical/ordinal outcome
for (myoutcome in BiB_out_cat) {
  #### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 2 models for vegetarian diet
  #### Model 1 (for White British): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <- clm(
      dat_WB[[i]][[myoutcome]] ~ dat_WB[[i]][["VegDiet_bin"]] + dat_WB[[i]][["age_Mat_con"]] + dat_WB[[i]][["edu_Mat_3cat"]]  + dat_WB[[i]][["IMD_Fam_cat"]] + dat_WB[[i]][["parity_Mat_bin"]] + dat_WB[[i]][["BMI_Mat_PRE.p_con"]] + dat_WB[[i]][["smoking_Mat_EAR.p_bin"]] + dat_WB[[i]][["alcohol_Mat_EAR.p_bin"]] + dat_WB[[i]][["sex_Chi_bin"]] + dat_WB[[i]][["any.supp_Mat_EAR.p_bin"]]
    )
  }
  mod_1_pooled <- pool(mod_1)
  x_1 <- IMP_extract_ord.log.res_bin(mod_1_pooled)
  y_1 <- IMP_extract_ord.beta.SE(mod_1_pooled)
  
  #### Model 2 (for South Asians): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <- clm(
      dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["VegDiet_bin"]] + dat_SA[[i]][["age_Mat_con"]] + dat_SA[[i]][["edu_Mat_3cat"]]  + dat_SA[[i]][["IMD_Fam_cat"]] + dat_SA[[i]][["parity_Mat_bin"]] + dat_SA[[i]][["BMI_Mat_PRE.p_con"]] + dat_SA[[i]][["smoking_Mat_EAR.p_bin"]] + dat_SA[[i]][["alcohol_Mat_EAR.p_bin"]] + dat_SA[[i]][["sex_Chi_bin"]] + dat_SA[[i]][["any.supp_Mat_EAR.p_bin"]]
    )
  }
  mod_2_pooled <- pool(mod_2)
  x_2 <- IMP_extract_ord.log.res_bin(mod_2_pooled)
  y_2 <- IMP_extract_ord.beta.SE(mod_2_pooled)
  
  #### Model 3 (for Other): VegDiet_bin + age, education, IMD, parity, BMI, smoking, alcohol, offspring sex, supplementation
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <- clm(
      dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["VegDiet_bin"]] + dat_SA[[i]][["age_Mat_con"]] + dat_SA[[i]][["edu_Mat_3cat"]]  + dat_SA[[i]][["IMD_Fam_cat"]] + dat_SA[[i]][["parity_Mat_bin"]] + dat_SA[[i]][["BMI_Mat_PRE.p_con"]] + dat_SA[[i]][["smoking_Mat_EAR.p_bin"]] + dat_SA[[i]][["alcohol_Mat_EAR.p_bin"]] + dat_SA[[i]][["sex_Chi_bin"]] + dat_SA[[i]][["any.supp_Mat_EAR.p_bin"]]
    )
  }
  mod_3_pooled <- pool(mod_3)
  x_3 <- IMP_extract_ord.log.res_bin(mod_3_pooled)
  y_3 <- IMP_extract_ord.beta.SE(mod_3_pooled)
  
  ### Regression results for one outcome
  outcome_x <- as.data.frame(rep(outcome, 2))
  colnames(outcome_x) <- "Outcome"
  
  N_x_WB <-
    as.data.frame(c(nrow(dat_WB_1[dat_WB_1$VegDiet_bin == 0 &
                                    !is.na(dat_WB_1[[myoutcome]]), ]), nrow(dat_WB_1[dat_WB_1$VegDiet_bin == 1 &
                                                                                       !is.na(dat_WB_1[[myoutcome]]), ])))
  colnames(N_x_WB) <- "N_WB"
  
  N_x_SA <-
    as.data.frame(c(nrow(dat_SA_1[dat_SA_1$VegDiet_bin == 0 &
                                    !is.na(dat_SA_1[[myoutcome]]), ]), nrow(dat_SA_1[dat_SA_1$VegDiet_bin == 1 &
                                                                                       !is.na(dat_SA_1[[myoutcome]]), ])))
  
  colnames(N_x_SA) <- "N_SA"
  
  N_x_other <-
    as.data.frame(c(nrow(dat_other_1[dat_other_1$VegDiet_bin == 0 &
                                       !is.na(dat_other_1[[myoutcome]]), ]), nrow(dat_other_1[dat_other_1$VegDiet_bin == 1 &
                                                                                                !is.na(dat_other_1[[myoutcome]]), ])))
  colnames(N_x_other) <- "N_other"
  
  x <-
    cbind(outcome_x, N_x_WB, x_1, N_x_SA, x_2, N_x_other, x_3)  # Columns: Outcome name | N | Model 1 | N | Model 2 | N | Model 3; rows: Non-vegetarian | Pesco-/full vegetarian
  
  outcome_col <- as.data.frame(rep(outcome, 3))
  colnames(outcome_col) <- "Outcome"
  model_col <-
    as.data.frame(c("White British", "South Asian", "Other"))
  colnames(model_col) <- "Ethnicity"
  y <- cbind(outcome_col, model_col, rbind(y_1, y_2, y_3))
  
  ### Rows continuously added to the output table
  obs.tbl_VegDiet_ord <- rbind(obs.tbl_VegDiet_ord, x)
  
  obs.res_VegDiet_ord <- rbind(obs.res_VegDiet_ord, y)
}

obs.tbl_VegDiet_ord
obs.res_VegDiet_ord

################################################################################

## View and save results
obs.tbl_VegDiet_ord <- as.data.frame(obs.tbl_VegDiet_ord)
obs.tbl_VegDiet_ord[, 8] <-
  " "  # For convenience when making tables
obs.tbl_VegDiet_ord <-
  obs.tbl_VegDiet_ord[, c(1, 3, 2, 4:6, 8, 7, 9:11, 13, 12, 14:ncol(obs.tbl_VegDiet_ord))]  # For convenience when making tables
obs.tbl_VegDiet_ord
dim(obs.tbl_VegDiet_ord)  # 1 outcome for 2 categories in 2 models = 2 obs.
# write.xlsx(obs.tbl_VegDiet_ord,
#            "results/BiB/IMP_CCC_obs.tbl_VegDiet_ord.xlsx",
#            overwrite = T)
# obs.tbl_VegDiet_ord <-
#   read.xlsx("results/BiB/IMP_CCC_obs.tbl_VegDiet_ord.xlsx")

obs.res_VegDiet_ord <- as.data.frame(obs.res_VegDiet_ord)
obs.res_VegDiet_ord$N_exp <- NA
obs.res_VegDiet_ord$N_ref <- NA
for (i in 1:nrow(obs.res_VegDiet_ord)) {
  if (obs.res_VegDiet_ord$Ethnicity[i] == "White British") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 3]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 3]
  } else if (obs.res_VegDiet_ord$Ethnicity[i] == "South Asian") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 8]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 8]
  } else if (obs.res_VegDiet_ord$Ethnicity[i] == "Other") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 13]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 1 outcome * 1 category * 3 models = 3 obs.
# write.xlsx(obs.res_VegDiet_ord,
#            "results/BiB/IMP_CCC_obs.res_VegDiet_ord.xlsx",
#            overwrite = T)
# obs.res_VegDiet_ord <-
#   read.xlsx("results/BiB/IMP_CCC_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_ALL <- read.xlsx("results/BiB/IMP_BF_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_ALL <- subset(obs.res_VegDiet_ord_ALL, Model == "Model 3")
obs.res_VegDiet_ord_ALL$Model <- "Whole cohort"
colnames(obs.res_VegDiet_ord_ALL)[colnames(obs.res_VegDiet_ord_ALL) == "Model"] <- "Ethnicity"
obs.res_VegDiet_ord_ALL

obs.res_VegDiet_ord <- rbind(obs.res_VegDiet_ord, obs.res_VegDiet_ord_ALL)
obs.res_VegDiet_ord

################################################################################
################################################################################

## Forest plots

### Prepare data
obs.res_VegDiet_ord$Group <- NA
obs.res_VegDiet_ord$Group[obs.res_VegDiet_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_VegDiet_ord$Group[obs.res_VegDiet_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_VegDiet_ord$Group[obs.res_VegDiet_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_VegDiet_ord$Group <-
  factor(
    obs.res_VegDiet_ord$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_VegDiet_ord$Ethnicity <-
  factor(
    obs.res_VegDiet_ord$Ethnicity,
    levels = c("Other", "South Asian", "White British", "Whole cohort")
  )

obs.res_VegDiet_ord$Outcome <-
  factor(obs.res_VegDiet_ord$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_ord$Outcome])
obs.res_VegDiet_ord <-
  obs.res_VegDiet_ord %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_VegDiet_ord$b <-
  as.numeric(obs.res_VegDiet_ord$b)
obs.res_VegDiet_ord$se <-
  as.numeric(obs.res_VegDiet_ord$se)
obs.res_VegDiet_ord$pval <-
  as.numeric(obs.res_VegDiet_ord$pval)

head(obs.res_VegDiet_ord)
dim(obs.res_VegDiet_ord)  # 1 outcome * 1 category * 4 models = 4 obs.

################################################################################
## Combine with binary outcome results
obs.res_VegDiet_bin_ord <- rbind(obs.res_VegDiet_bin, obs.res_VegDiet_ord)
obs.res_VegDiet_bin_ord$Type <- "Primary/secondary outcome\n(binary)"
obs.res_VegDiet_bin_ord$Type[obs.res_VegDiet_bin_ord$Outcome == "Breastfeeding duration"] <- "Secondary outcome\n(ordinal)"
obs.res_VegDiet_bin_ord$Type <-
  factor(
    obs.res_VegDiet_bin_ord$Type,
    levels = c(
      "Primary/secondary outcome\n(binary)",
      "Secondary outcome\n(ordinal)"
    )
  )
obs.res_VegDiet_bin_ord
################################################################################

### Nightingale forest plots

################################################################################
#### Only keep outcomes with rows = 4 (i.e., estimable results for all ethnic groups)
obs.res_VegDiet_bin_ord <- obs.res_VegDiet_bin_ord %>%
  group_by(Outcome) %>%
  filter(n() == 4) %>%
  ungroup()

dim(obs.res_VegDiet_bin_ord)  # 61 -> 44
################################################################################

obs.forest_VegDiet_bin_ord <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin_ord,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Ethnicity,
  xlab = "OR and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(22, 25, 24, 23)) +
  ggforce::facet_col(facets = ~ Type,
                     scales = "free_y",
                     space = "free")


obs.forest_VegDiet_bin_ord

ggsave(
  obs.forest_VegDiet_bin_ord,
  file = "results/BiB/IMP_CCC.ALL_obs.forest_VegDiet_bin_ord.png",
  height = 10,
  width = 7
)

#------------------------------------------------------------------------------#
#                            Confounding Structure                             #----
#------------------------------------------------------------------------------#

# Effect of SEP on vegetarianism and perinatal outcomes

## Prepare data
for (i in 1:dat_imp$m) {
  dat_WB[[i]]$edu_Mat_3cat <- as.numeric(dat_WB[[i]]$edu_Mat_3cat)
  dat_WB[[i]]$IMD_Fam_cat <- as.numeric(dat_WB[[i]]$IMD_Fam_cat)
  dat_SA[[i]]$edu_Mat_3cat <- as.numeric(dat_SA[[i]]$edu_Mat_3cat)
  dat_SA[[i]]$IMD_Fam_cat <- as.numeric(dat_SA[[i]]$IMD_Fam_cat)
  dat_other[[i]]$edu_Mat_3cat <- as.numeric(dat_other[[i]]$edu_Mat_3cat)
  dat_other[[i]]$IMD_Fam_cat <- as.numeric(dat_other[[i]]$IMD_Fam_cat)
}  # Recode SEP indicators as numeric to avoid model failure

## Create a function to extract beta, SE, and p-value (for both linear and logistic regression)
IMP_extract_beta.SE <- function(mymodel) {
  b <- summary(mymodel)[2, 2]
  se <- summary(mymodel)[2, 3]
  pval <- summary(mymodel)[2, 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

## Create a function to extract beta, SE, and p-value (for ORDINAL logistic regression)
IMP_extract_ord.beta.SE <- function(mymodel) {
  b <- summary(mymodel)[4, 2]
  se <- summary(mymodel)[4, 3]
  pval <- summary(mymodel)[4, 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

## Create empty output tables (for forestplots and meta-analysis)
obs.res_VegDiet_bin <- c()
obs.res_VegDiet_con <- c()
obs.res_VegDiet_ord <- c()

################################################################################

## Loop for each binary outcome
for (myoutcome in c("VegDiet_bin", BiB_out_bin)) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 6 models - 2 SEP indicators * 3 strata
  #### Model 1 - Maternal education on vegetarianism and perinatal outcomes in White British
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(dat_WB[[i]][[myoutcome]] ~ dat_WB[[i]][["edu_Mat_3cat"]] + dat_WB[[i]][["age_Mat_con"]], family = binomial)
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE(mod_1_pooled)
  
  #### Model 2 - IMD/household income on vegetarianism and perinatal outcomes in White British
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      glm(dat_WB[[i]][[myoutcome]] ~ dat_WB[[i]][["IMD_Fam_cat"]] + dat_WB[[i]][["age_Mat_con"]], family = binomial)
  }
  mod_2_pooled <- pool(mod_2)
  y_2 <- IMP_extract_beta.SE(mod_2_pooled)
  
  #### Model 3 - Maternal education on vegetarianism and perinatal outcomes in South Asians
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      glm(dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["edu_Mat_3cat"]] + dat_SA[[i]][["age_Mat_con"]], family = binomial)
  }
  mod_3_pooled <- pool(mod_3)
  y_3 <- IMP_extract_beta.SE(mod_3_pooled)
  
  #### Model 4 - IMD/household income on vegetarianism and perinatal outcomes in South Asians
  mod_4 <- list()
  for (i in 1:dat_imp$m) {
    mod_4[[i]] <-
      glm(dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["IMD_Fam_cat"]] + dat_SA[[i]][["age_Mat_con"]], family = binomial)
  }
  mod_4_pooled <- pool(mod_4)
  y_4 <- IMP_extract_beta.SE(mod_4_pooled)
  
  #### Model 5 - Maternal education on vegetarianism and perinatal outcomes in Other
  mod_5 <- list()
  for (i in 1:dat_imp$m) {
    mod_5[[i]] <-
      glm(dat_other[[i]][[myoutcome]] ~ dat_other[[i]][["edu_Mat_3cat"]] + dat_other[[i]][["age_Mat_con"]], family = binomial)
  }
  mod_5_pooled <- pool(mod_5)
  y_5 <- IMP_extract_beta.SE(mod_5_pooled)
  
  #### Model 6 - IMD/household income on vegetarianism and perinatal outcomes in Other
  mod_6 <- list()
  for (i in 1:dat_imp$m) {
    mod_6[[i]] <-
      glm(dat_other[[i]][[myoutcome]] ~ dat_other[[i]][["IMD_Fam_cat"]] + dat_other[[i]][["age_Mat_con"]], family = binomial)
  }
  mod_6_pooled <- pool(mod_6)
  y_6 <- IMP_extract_beta.SE(mod_6_pooled)
  
  ### Regression results for one outcome
  outcome_col <- as.data.frame(rep(outcome, 6))
  colnames(outcome_col) <- "Outcome"
  SEP_col <-
    as.data.frame(
      c(
        "Maternal education (higher vs. lower)",
        "IMD (more vs. less affluent)",
        "Maternal education (higher vs. lower)",
        "IMD (more vs. less affluent)",
        "Maternal education (higher vs. lower)",
        "IMD (more vs. less affluent)"
      )
    )
  colnames(SEP_col) <- "SEP"
  ethnic_col <-
    as.data.frame(c(
      "White British",
      "White British",
      "South Asian",
      "South Asian",
      "Other",
      "Other"
    ))
  colnames(ethnic_col) <- "Ethnicity"
  y <- cbind(outcome_col,
             SEP_col,
             ethnic_col,
             rbind(y_1, y_2, y_3, y_4, y_5, y_6))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_bin <- rbind(obs.res_VegDiet_bin, y)
}

################################################################################
################################################################################

## Loop for each continuous outcome
for (myoutcome in BiB_out_con) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 6 models - 2 SEP indicators * 3 strata
  #### Model 1 - Maternal education on vegetarianism and perinatal outcomes in White British
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      lm(dat_WB[[i]][[myoutcome]] ~ dat_WB[[i]][["edu_Mat_3cat"]] + dat_WB[[i]][["age_Mat_con"]])
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE(mod_1_pooled)
  
  #### Model 2 - IMD/household income on vegetarianism and perinatal outcomes in White British
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      lm(dat_WB[[i]][[myoutcome]] ~ dat_WB[[i]][["IMD_Fam_cat"]] + dat_WB[[i]][["age_Mat_con"]])
  }
  mod_2_pooled <- pool(mod_2)
  y_2 <- IMP_extract_beta.SE(mod_2_pooled)
  
  #### Model 3 - Maternal education on vegetarianism and perinatal outcomes in South Asians
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      lm(dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["edu_Mat_3cat"]] + dat_SA[[i]][["age_Mat_con"]])
  }
  mod_3_pooled <- pool(mod_3)
  y_3 <- IMP_extract_beta.SE(mod_3_pooled)
  
  #### Model 4 - IMD/household income on vegetarianism and perinatal outcomes in South Asians
  mod_4 <- list()
  for (i in 1:dat_imp$m) {
    mod_4[[i]] <-
      lm(dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["IMD_Fam_cat"]] + dat_SA[[i]][["age_Mat_con"]])
  }
  mod_4_pooled <- pool(mod_4)
  y_4 <- IMP_extract_beta.SE(mod_4_pooled)
  
  #### Model 5 - Maternal education on vegetarianism and perinatal outcomes in Other
  mod_5 <- list()
  for (i in 1:dat_imp$m) {
    mod_5[[i]] <-
      lm(dat_other[[i]][[myoutcome]] ~ dat_other[[i]][["edu_Mat_3cat"]] + dat_other[[i]][["age_Mat_con"]])
  }
  mod_5_pooled <- pool(mod_5)
  y_5 <- IMP_extract_beta.SE(mod_5_pooled)
  
  #### Model 6 - IMD/household income on vegetarianism and perinatal outcomes in Other
  mod_6 <- list()
  for (i in 1:dat_imp$m) {
    mod_6[[i]] <-
      lm(dat_other[[i]][[myoutcome]] ~ dat_other[[i]][["IMD_Fam_cat"]] + dat_other[[i]][["age_Mat_con"]])
  }
  mod_6_pooled <- pool(mod_6)
  y_6 <- IMP_extract_beta.SE(mod_6_pooled)
  
  ### Regression results for one outcome
  outcome_col <- as.data.frame(rep(outcome, 6))
  colnames(outcome_col) <- "Outcome"
  SEP_col <-
    as.data.frame(
      c(
        "Maternal education (higher vs. lower)",
        "IMD (more vs. less affluent)",
        "Maternal education (higher vs. lower)",
        "IMD (more vs. less affluent)",
        "Maternal education (higher vs. lower)",
        "IMD (more vs. less affluent)"
      )
    )
  colnames(SEP_col) <- "SEP"
  ethnic_col <-
    as.data.frame(c(
      "White British",
      "White British",
      "South Asian",
      "South Asian",
      "Other",
      "Other"
    ))
  colnames(ethnic_col) <- "Ethnicity"
  y <- cbind(outcome_col,
             SEP_col,
             ethnic_col,
             rbind(y_1, y_2, y_3, y_4, y_5, y_6))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_con <- rbind(obs.res_VegDiet_con, y)
}

################################################################################
################################################################################

## Loop for each categorical/ordinal outcome
for (myoutcome in BiB_out_cat) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 6 models - 2 SEP indicators * 3 strata
  #### Model 1 - Maternal education on vegetarianism and negative control outcome in White British
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      clm(dat_WB[[i]][[myoutcome]] ~ dat_WB[[i]][["edu_Mat_3cat"]] + dat_WB[[i]][["age_Mat_con"]])
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_ord.beta.SE(mod_1_pooled)
  
  #### Model 2 - IMD/household income on vegetarianism and negative control outcome in White British
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      clm(dat_WB[[i]][[myoutcome]] ~ dat_WB[[i]][["IMD_Fam_cat"]] + dat_WB[[i]][["age_Mat_con"]])
  }
  mod_2_pooled <- pool(mod_2)
  y_2 <- IMP_extract_ord.beta.SE(mod_2_pooled)
  
  #### Model 3 - Maternal education on vegetarianism and negative control outcome in South Asians
  mod_3 <- list()
  for (i in 1:dat_imp$m) {
    mod_3[[i]] <-
      clm(dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["edu_Mat_3cat"]] + dat_SA[[i]][["age_Mat_con"]])
  }
  mod_3_pooled <- pool(mod_3)
  y_3 <- IMP_extract_ord.beta.SE(mod_3_pooled)
  
  #### Model 4 - IMD/household income on vegetarianism and negative control outcome in South Asians
  mod_4 <- list()
  for (i in 1:dat_imp$m) {
    mod_4[[i]] <-
      clm(dat_SA[[i]][[myoutcome]] ~ dat_SA[[i]][["IMD_Fam_cat"]] + dat_SA[[i]][["age_Mat_con"]])
  }
  mod_4_pooled <- pool(mod_4)
  y_4 <- IMP_extract_ord.beta.SE(mod_4_pooled)
  
  #### Model 5 - Maternal education on vegetarianism and negative control outcome in Other
  mod_5 <- list()
  for (i in 1:dat_imp$m) {
    mod_5[[i]] <-
      clm(dat_other[[i]][[myoutcome]] ~ dat_other[[i]][["edu_Mat_3cat"]] + dat_other[[i]][["age_Mat_con"]])
  }
  mod_5_pooled <- pool(mod_5)
  y_5 <- IMP_extract_ord.beta.SE(mod_5_pooled)
  
  #### Model 6 - IMD/household income on vegetarianism and negative control outcome in Other
  mod_6 <- list()
  for (i in 1:dat_imp$m) {
    mod_6[[i]] <-
      clm(dat_other[[i]][[myoutcome]] ~ dat_other[[i]][["IMD_Fam_cat"]] + dat_other[[i]][["age_Mat_con"]])
  }
  mod_6_pooled <- pool(mod_6)
  y_6 <- IMP_extract_ord.beta.SE(mod_6_pooled)
  
  ### Regression results for one outcome
  outcome_col <- as.data.frame(rep(outcome, 6))
  colnames(outcome_col) <- "Outcome"
  SEP_col <-
    as.data.frame(
      c(
        "Maternal education (higher vs. lower)",
        "IMD (more vs. less affluent)",
        "Maternal education (higher vs. lower)",
        "IMD (more vs. less affluent)",
        "Maternal education (higher vs. lower)",
        "IMD (more vs. less affluent)"
      )
    )
  colnames(SEP_col) <- "SEP"
  ethnic_col <-
    as.data.frame(c(
      "White British",
      "White British",
      "South Asian",
      "South Asian",
      "Other",
      "Other"
    ))
  colnames(ethnic_col) <- "Ethnicity"
  y <- cbind(outcome_col,
             SEP_col,
             ethnic_col,
             rbind(y_1, y_2, y_3, y_4, y_5, y_6))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_ord <- rbind(obs.res_VegDiet_ord, y)
}

################################################################################

## View and save results
obs.res_VegDiet_bin <- as.data.frame(obs.res_VegDiet_bin)
obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # (1 exposure + 19 outcomes) * 6 models = 120 obs.

obs.res_VegDiet_con <- as.data.frame(obs.res_VegDiet_con)
obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 4 outcomes * 6 models = 24 obs.

obs.res_VegDiet_ord <- as.data.frame(obs.res_VegDiet_ord)
obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 1 outcome * 6 models = 6 obs.

obs.res_VegDiet_bin_con_ord <- rbind(obs.res_VegDiet_bin,
                                     obs.res_VegDiet_con,
                                     obs.res_VegDiet_ord)

obs.res_VegDiet_bin_con_ord
dim(obs.res_VegDiet_bin_con_ord)  # 120 + 24 + 6 = 150 obs.

# write.xlsx(
#   obs.res_VegDiet_bin_con_ord,
#   "results/BiB/IMP_CCC_SEP_obs.res_VegDiet_bin_con_ord.xlsx",
#   overwrite = T
# )
# obs.res_VegDiet_bin_con_ord <-
#   read.xlsx("results/BiB/IMP_CCC_SEP_obs.res_VegDiet_bin_con_ord.xlsx")

################################################################################

## Forest plots

### Prepare data
obs.res_VegDiet_bin_con_ord$Group <- "Primary/secondary outcome\n(binary/continuous/ordinal)"
obs.res_VegDiet_bin_con_ord$Group[obs.res_VegDiet_bin_con_ord$Outcome %in% c("Pesco-/full vs. non-vegetarian")] <- "Vegetarianism\n(binary)"
obs.res_VegDiet_bin_con_ord$Group <-
  factor(
    obs.res_VegDiet_bin_con_ord$Group,
    levels = c(
      "Vegetarianism\n(binary)",
      "Primary/secondary outcome\n(binary/continuous/ordinal)"
    )
  )

obs.res_VegDiet_bin_con_ord$Outcome <-
  factor(
    obs.res_VegDiet_bin_con_ord$Outcome,
    levels = c(
      "Pesco-/full vs. non-vegetarian",
      unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin_con_ord$Outcome]
    )
  )
obs.res_VegDiet_bin_con_ord <-
  obs.res_VegDiet_bin_con_ord %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_VegDiet_bin_con_ord$b <-
  as.numeric(obs.res_VegDiet_bin_con_ord$b)
obs.res_VegDiet_bin_con_ord$se <-
  as.numeric(obs.res_VegDiet_bin_con_ord$se)
obs.res_VegDiet_bin_con_ord$pval <-
  as.numeric(obs.res_VegDiet_bin_con_ord$pval)

colnames(obs.res_VegDiet_bin_con_ord)[colnames(obs.res_VegDiet_bin_con_ord) == "SEP"] <- "SEP indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord$`SEP indicator` <- factor(
  obs.res_VegDiet_bin_con_ord$`SEP indicator`,
  levels = c(
    "IMD (more vs. less affluent)",
    "Maternal education (higher vs. lower)"
  )
)

head(obs.res_VegDiet_bin_con_ord)
dim(obs.res_VegDiet_bin_con_ord)  # 25 outcomes * 2 SEP indicators * 3 ethnic groups = 150 obs.

################################################################################
#### Separate White British and South Asian results
obs.res_VegDiet_bin_con_ord_WB <- obs.res_VegDiet_bin_con_ord[obs.res_VegDiet_bin_con_ord$Ethnicity == "White British", ]
obs.res_VegDiet_bin_con_ord_SA <- obs.res_VegDiet_bin_con_ord[obs.res_VegDiet_bin_con_ord$Ethnicity == "South Asian", ]
obs.res_VegDiet_bin_con_ord_other <- obs.res_VegDiet_bin_con_ord[obs.res_VegDiet_bin_con_ord$Ethnicity == "Other", ]

dim(obs.res_VegDiet_bin_con_ord_WB)  # 50 obs.
dim(obs.res_VegDiet_bin_con_ord_SA)  # 50 obs.
dim(obs.res_VegDiet_bin_con_ord_other)  # 50 obs.
################################################################################
#### Remove some results with extremely huge 95% CIs
obs.res_VegDiet_bin_con_ord_WB <- obs.res_VegDiet_bin_con_ord_WB[which(obs.res_VegDiet_bin_con_ord_WB$se < 90), ]
obs.res_VegDiet_bin_con_ord_SA <- obs.res_VegDiet_bin_con_ord_SA[which(obs.res_VegDiet_bin_con_ord_SA$se < 90), ]
obs.res_VegDiet_bin_con_ord_other <- obs.res_VegDiet_bin_con_ord_other[which(obs.res_VegDiet_bin_con_ord_other$se < 90), ]

dim(obs.res_VegDiet_bin_con_ord_WB)  # 50 -> 50 obs.
dim(obs.res_VegDiet_bin_con_ord_SA)  # 50 -> 50 obs.
dim(obs.res_VegDiet_bin_con_ord_other)  # 50 -> 48 obs.
################################################################################
# #### Keep those outcomes available for cross-context comparison
# obs.res_VegDiet_bin_con_ord_WB <- obs.res_VegDiet_bin_con_ord_WB[which(
#   obs.res_VegDiet_bin_con_ord_WB$Outcome %in% c(
#     "Pesco-/full vs. non-vegetarian",
#     as.character(obs.res_VegDiet_bin_ord$Outcome),
#     obs.res_VegDiet_con$Outcome
#   )
# ), ]
# obs.res_VegDiet_bin_con_ord_SA <- obs.res_VegDiet_bin_con_ord_SA[which(
#   obs.res_VegDiet_bin_con_ord_SA$Outcome %in% c(
#     "Pesco-/full vs. non-vegetarian",
#     as.character(obs.res_VegDiet_bin_ord$Outcome),
#     obs.res_VegDiet_con$Outcome
#   )
# ), ]
# dim(obs.res_VegDiet_bin_con_ord_WB)  # 50 -> 32 obs.
# dim(obs.res_VegDiet_bin_con_ord_SA)  # 50 -> 32 obs.
################################################################################

### Nightingale forest plots

#### White British
obs.forest_VegDiet_bin_con_ord_WB <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin_con_ord_WB,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `SEP indicator`,
  shape = `SEP indicator`,
  xlab = "LogOR or beta and 95% CI",
  title = "White British",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("blueviolet", "darkred")) +
  ggplot2::scale_shape_manual(values = c(21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_VegDiet_bin_con_ord_WB

ggsave(
  obs.forest_VegDiet_bin_con_ord_WB,
  file = "results/BiB/IMP_CCC.ALL_SEP_obs.forest_VegDiet_bin_con_ord_WB.png",
  height = 10,
  width = 10
)

################################################################################
################################################################################

#### South Asian
obs.forest_VegDiet_bin_con_ord_SA <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin_con_ord_SA,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `SEP indicator`,
  shape = `SEP indicator`,
  xlab = "LogOR or beta and 95% CI",
  title = "South Asian",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("blueviolet", "darkred")) +
  ggplot2::scale_shape_manual(values = c(21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_VegDiet_bin_con_ord_SA

ggsave(
  obs.forest_VegDiet_bin_con_ord_SA,
  file = "results/BiB/IMP_CCC.ALL_SEP_obs.forest_VegDiet_bin_con_ord_SA.png",
  height = 10,
  width = 10
)

################################################################################
################################################################################

#### Other
obs.forest_VegDiet_bin_con_ord_other <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin_con_ord_other,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `SEP indicator`,
  shape = `SEP indicator`,
  xlab = "LogOR or beta and 95% CI",
  title = "Other ethnic groups",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("blueviolet", "darkred")) +
  ggplot2::scale_shape_manual(values = c(21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_VegDiet_bin_con_ord_other

ggsave(
  obs.forest_VegDiet_bin_con_ord_other,
  file = "results/BiB/IMP_CCC.ALL_SEP_obs.forest_VegDiet_bin_con_ord_other.png",
  height = 10,
  width = 10
)

#------------------------------------------------------------------------------#
#                                Combine Plots                                 #----
#------------------------------------------------------------------------------#

# Combine confounding structure results for binary, continuous, and ordinal outcomes
obs.forest_VegDiet_bin_con_ord_WB <- obs.forest_VegDiet_bin_con_ord_WB + ggplot2::theme(legend.position = "bottom",
                                                                                        legend.justification = c(-1000, 0))
print(obs.forest_VegDiet_bin_con_ord_WB)
grob1 <- grid.grab()
dev.off()

obs.forest_VegDiet_bin_con_ord_SA <- obs.forest_VegDiet_bin_con_ord_SA + ggplot2::theme(legend.position = "bottom")
print(obs.forest_VegDiet_bin_con_ord_SA)
grob2 <- grid.grab()
dev.off()

obs.forest_VegDiet_bin_con_ord_other <- obs.forest_VegDiet_bin_con_ord_other + ggplot2::theme(legend.position = "bottom",
                                                                                              legend.justification = c(1000, 0))
print(obs.forest_VegDiet_bin_con_ord_other)
grob3 <- grid.grab()
dev.off()

png(
  "results/BiB/Comb_IMP_CCC.ALL_SEP_obs.forest_VegDiet.png",
  res = 300,
  height = 3000,
  width = 5500
)
grid.arrange(grob1, grob2, grob3, ncol = 3, widths = c(1, 1, 1))
dev.off()
