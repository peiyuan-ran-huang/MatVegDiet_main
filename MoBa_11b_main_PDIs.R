################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 08-Dec-2024
# This script is to perform main association analysis for plant-based diet indices (PDIs) in MoBa.

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
  VGAM
)

# Set working directory
setwd("N:/durable/projects/Ran_MoBa_var")

################################################################################

# Load data
dat <- readRDS("dat_exp_cov_out_pat.rds")
dat <- subset(dat, is.na(PDI) == F)  # Remove missing PDIs
head(dat)
dim(dat)  # 73868 -> 73868

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
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat))]
MoBa_primary_bin  # 13 primary (binary) outcomes available in MoBa

MoBa_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat))]
MoBa_secondary_bin <-
  MoBa_secondary_bin[MoBa_secondary_bin != "anaemia_preg_subsamp"]  # !!! Exclude maternal anaemia occurring in pregnancy for this analysis !!!
MoBa_secondary_bin  # 8 secondary binary outcomes available in MoBa

MoBa_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat))]
MoBa_secondary_con  # 4 secondary continuous outcomes available in MoBa

MoBa_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat))]
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

# Create a function to extract regression results with SE (for both linear and logistic regression) - !!! Specifically for continuous exposures !!!
extract_beta.SE_con <- function(mymodel) {
  b <- summary(mymodel)$coef[2, 1]
  se <- summary(mymodel)$coef[2, 2]
  pval <- summary(mymodel)$coef[2, 4]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

################################################################################

# PDIs and binary outcomes - Logistic regression

## Create a function to extract logistic regression results - !!! Specifically for continuous exposures !!!
extract_log.res_con <- function(mymodel) {
  N <- length(mymodel$fitted.values)  # Extract sample size
  
  OR <-
    format(round(exp(summary(mymodel)$coef[2, 1]), digits = 2), nsmall = 2)  # Extract OR (keep 2 decimal places)
  
  CI <-
    paste0(format(round(
      exp(summary(mymodel)$coef[2, 1] - 1.96 * summary(mymodel)$coef[2, 2]), digits = 2
    ), nsmall = 2), ", ", format(round(
      exp(summary(mymodel)$coef[2, 1] + 1.96 * summary(mymodel)$coef[2, 2]), digits = 2
    ), nsmall = 2))  # Extract 95% CI (keep 2 decimal places);   # confint() too time-consuming for logistic models - calculating 95% CI manually instead
  
  pval <-
    style_pvalue(summary(mymodel)$coef[2, 4], digits = 3)  # Extract p-value (keep 3 decimal places)
  
  x <- cbind(N, OR, CI, pval)
  
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
for (myoutcome in MoBa_out_bin) {
  ### Outcome name
  outcome <- var_lab(dat[myoutcome])
  
  ### PDI
  #### Model 1 - Univariate (unadjusted) model
  mod_1 <-
    glm(dat[[myoutcome]] ~ dat[["PDI_z"]], family = binomial)
  x_1 <- extract_log.res_con(mod_1)
  y_1 <- extract_beta.SE_con(mod_1)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_2 <-
    glm(dat[[myoutcome]] ~ dat[["PDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]],
        family = binomial)
  x_2 <- extract_log.res_con(mod_2)
  y_2 <- extract_beta.SE_con(mod_2)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_3 <-
    glm(dat[[myoutcome]] ~ dat[["PDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]] + dat[["any.supp_Mat_EAR.p_bin"]] + dat[["energy_Mat_DUR.p_con"]],
        family = binomial)
  x_3 <- extract_log.res_con(mod_3)
  y_3 <- extract_beta.SE_con(mod_3)
  
  ### hPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_4 <-
    glm(dat[[myoutcome]] ~ dat[["hPDI_z"]], family = binomial)
  x_4 <- extract_log.res_con(mod_4)
  y_4 <- extract_beta.SE_con(mod_4)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_5 <-
    glm(dat[[myoutcome]] ~ dat[["hPDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]],
        family = binomial)
  x_5 <- extract_log.res_con(mod_5)
  y_5 <- extract_beta.SE_con(mod_5)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_6 <-
    glm(dat[[myoutcome]] ~ dat[["hPDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]] + dat[["any.supp_Mat_EAR.p_bin"]] + dat[["energy_Mat_DUR.p_con"]],
        family = binomial)
  x_6 <- extract_log.res_con(mod_6)
  y_6 <- extract_beta.SE_con(mod_6)
  
  ### uPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_7 <-
    glm(dat[[myoutcome]] ~ dat[["uPDI_z"]], family = binomial)
  x_7 <- extract_log.res_con(mod_7)
  y_7 <- extract_beta.SE_con(mod_7)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_8 <-
    glm(dat[[myoutcome]] ~ dat[["uPDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]],
        family = binomial)
  x_8 <- extract_log.res_con(mod_8)
  y_8 <- extract_beta.SE_con(mod_8)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_9 <-
    glm(dat[[myoutcome]] ~ dat[["uPDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]] + dat[["any.supp_Mat_EAR.p_bin"]] + dat[["energy_Mat_DUR.p_con"]],
        family = binomial)
  x_9 <- extract_log.res_con(mod_9)
  y_9 <- extract_beta.SE_con(mod_9)
  
  ### Regression results for one outcome - Column 1: outcome name * 3; Column 2: Model 1 * 3; Column 3: Model 2 * 3; Column 4: Model 3 * 3
  x <-
    cbind(
      Outcome = as.data.frame(rep(outcome, 3)),
      Exposure = as.data.frame(c("PDI", "hPDI", "uPDI")),
      Model_1 = rbind(x_1, x_4, x_7),
      space = " ",
      Model_2 = rbind(x_2, x_5, x_8),
      space = " ",
      Model_3 = rbind(x_3, x_6, x_9)
    )  # PDI: 1 2 3; hPDI: 4 5 6; uPDI: 7 8 9
  colnames(x)[1] <- "Outcome"
  colnames(x)[2] <- "Exposure"
  
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
dim(obs.tbl_PDI.hPDI.uPDI_bin)  # 21 outcomes * 3 exposures = 63 obs
write.xlsx(
  obs.tbl_PDI.hPDI.uPDI_bin,
  "results/MAIN_obs.tbl_PDI.hPDI.uPDI_bin.xlsx",
  overwrite = T
)
obs.tbl_PDI.hPDI.uPDI_bin <-
  read.xlsx("results/MAIN_obs.tbl_PDI.hPDI.uPDI_bin.xlsx")

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
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "PDI", 8]
  } else if (obs.res_PDI_bin$Model[i] == "Model 3") {
    obs.res_PDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_PDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "PDI", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_PDI_bin
dim(obs.res_PDI_bin)  # 21 outcomes * 3 models = 63 obs
write.xlsx(obs.res_PDI_bin,
           "results/MAIN_obs.res_PDI_bin.xlsx",
           overwrite = T)
obs.res_PDI_bin <-
  read.xlsx("results/MAIN_obs.res_PDI_bin.xlsx")

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
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "hPDI", 8]
  } else if (obs.res_hPDI_bin$Model[i] == "Model 3") {
    obs.res_hPDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_hPDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "hPDI", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_hPDI_bin
dim(obs.res_hPDI_bin)  # 21 outcomes * 3 models = 63 obs
write.xlsx(obs.res_hPDI_bin,
           "results/MAIN_obs.res_hPDI_bin.xlsx",
           overwrite = T)
obs.res_hPDI_bin <-
  read.xlsx("results/MAIN_obs.res_hPDI_bin.xlsx")

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
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "uPDI", 8]
  } else if (obs.res_uPDI_bin$Model[i] == "Model 3") {
    obs.res_uPDI_bin$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_bin[obs.tbl_PDI.hPDI.uPDI_bin$Outcome == obs.res_uPDI_bin$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_bin$Exposure == "uPDI", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_uPDI_bin
dim(obs.res_uPDI_bin)  # 21 outcomes * 3 models = 63 obs
write.xlsx(obs.res_uPDI_bin,
           "results/MAIN_obs.res_uPDI_bin.xlsx",
           overwrite = T)
obs.res_uPDI_bin <-
  read.xlsx("results/MAIN_obs.res_uPDI_bin.xlsx")

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
dim(obs.res_PDI.hPDI.uPDI_bin)  # 21 outcomes * 3 exposures * 1 model = 63 obs

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
  title = "Binary outcomes in MoBa",
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
  file = "results/MAIN_obs.forest_PDI.hPDI.uPDI_bin.png",
  height = 12,
  width = 7
)

################################################################################

# PDIs and continuous outcomes - Linear regression

## Create a function to extract linear regression results - !!! Specifically for continuous exposures !!!
extract_lin.res_con <- function(mymodel) {
  N <- length(mymodel$fitted.values)  # Extract sample size
  
  Beta <-
    format(round(coef(mymodel)[[2]], digits = 2), nsmall = 2)  # Extract OR (keep 2 decimal places)
  
  CI_tmp <-
    format(round(confint(mymodel)[2, ], digits = 2), nsmall = 2)
  CI <-
    paste0(CI_tmp[[1]], ", ", CI_tmp[[2]])  # Extract 95% CI (keep 2 decimal places)
  
  pval <-
    style_pvalue(summary(mymodel)$coef[2, 4], digits = 3)  # Extract p-value (keep 3 decimal places)
  
  x <- cbind(N, Beta, CI, pval)
  
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
for (myoutcome in MoBa_out_con) {
  ### Outcome name
  outcome <- var_lab(dat[myoutcome])
  
  ### PDI
  #### Model 1 - Univariate (unadjusted) model
  mod_1 <-
    lm(dat[[myoutcome]] ~ dat[["PDI_z"]])
  x_1 <- extract_lin.res_con(mod_1)
  y_1 <- extract_beta.SE_con(mod_1)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_2 <-
    lm(dat[[myoutcome]] ~ dat[["PDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]])
  x_2 <- extract_lin.res_con(mod_2)
  y_2 <- extract_beta.SE_con(mod_2)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_3 <-
    lm(dat[[myoutcome]] ~ dat[["PDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]] + dat[["any.supp_Mat_EAR.p_bin"]] + dat[["energy_Mat_DUR.p_con"]])
  x_3 <- extract_lin.res_con(mod_3)
  y_3 <- extract_beta.SE_con(mod_3)
  
  ### hPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_4 <-
    lm(dat[[myoutcome]] ~ dat[["hPDI_z"]])
  x_4 <- extract_lin.res_con(mod_4)
  y_4 <- extract_beta.SE_con(mod_4)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_5 <-
    lm(dat[[myoutcome]] ~ dat[["hPDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]])
  x_5 <- extract_lin.res_con(mod_5)
  y_5 <- extract_beta.SE_con(mod_5)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_6 <-
    lm(dat[[myoutcome]] ~ dat[["hPDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]] + dat[["any.supp_Mat_EAR.p_bin"]] + dat[["energy_Mat_DUR.p_con"]])
  x_6 <- extract_lin.res_con(mod_6)
  y_6 <- extract_beta.SE_con(mod_6)
  
  ### uPDI
  #### Model 1 - Univariate (unadjusted) model
  mod_7 <-
    lm(dat[[myoutcome]] ~ dat[["uPDI_z"]])
  x_7 <- extract_lin.res_con(mod_7)
  y_7 <- extract_beta.SE_con(mod_7)
  
  #### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
  mod_8 <-
    lm(dat[[myoutcome]] ~ dat[["uPDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]])
  x_8 <- extract_lin.res_con(mod_8)
  y_8 <- extract_beta.SE_con(mod_8)
  
  #### Model 3 - Additionally adjusting for nutrition-related factors
  mod_9 <-
    lm(dat[[myoutcome]] ~ dat[["uPDI_z"]] + dat[["age_Mat_con"]] + dat[["ethnic_Mat_bin"]] + dat[["edu_Mat_3cat"]]  + dat[["income_Fam_3cat"]] + dat[["parity_Mat_bin"]] + dat[["BMI_Mat_PRE.p_con"]] + dat[["smoking_Mat_EAR.p_bin"]] + dat[["alcohol_Mat_EAR.p_bin"]] + dat[["sex_Chi_bin"]] + dat[["any.supp_Mat_EAR.p_bin"]] + dat[["energy_Mat_DUR.p_con"]])
  x_9 <- extract_lin.res_con(mod_9)
  y_9 <- extract_beta.SE_con(mod_9)
  
  ### Regression results for one outcome - Column 1: outcome name * 3; Column 2: Model 1 * 3; Column 3: Model 2 * 3; Column 4: Model 3 * 3
  x <-
    cbind(
      Outcome = as.data.frame(rep(outcome, 3)),
      Exposure = as.data.frame(c("PDI", "hPDI", "uPDI")),
      Model_1 = rbind(x_1, x_4, x_7),
      space = " ",
      Model_2 = rbind(x_2, x_5, x_8),
      space = " ",
      Model_3 = rbind(x_3, x_6, x_9)
    )  # PDI: 1 2 3; hPDI: 4 5 6; uPDI: 7 8 9
  colnames(x)[1] <- "Outcome"
  colnames(x)[2] <- "Exposure"
  
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
dim(obs.tbl_PDI.hPDI.uPDI_con)  # 4 outcomes with 3 exposures in 3 models
write.xlsx(
  obs.tbl_PDI.hPDI.uPDI_con,
  "results/MAIN_obs.tbl_PDI.hPDI.uPDI_con.xlsx",
  overwrite = T
)
obs.tbl_PDI.hPDI.uPDI_con <-
  read.xlsx("results/MAIN_obs.tbl_PDI.hPDI.uPDI_con.xlsx")

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
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "PDI", 8]
  } else if (obs.res_PDI_con$Model[i] == "Model 3") {
    obs.res_PDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_PDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "PDI", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_PDI_con
dim(obs.res_PDI_con)  # 4 outcomes * 3 models = 12 obs
write.xlsx(obs.res_PDI_con,
           "results/MAIN_obs.res_PDI_con.xlsx",
           overwrite = T)
obs.res_PDI_con <-
  read.xlsx("results/MAIN_obs.res_PDI_con.xlsx")

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
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "hPDI", 8]
  } else if (obs.res_hPDI_con$Model[i] == "Model 3") {
    obs.res_hPDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_hPDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "hPDI", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_hPDI_con
dim(obs.res_hPDI_con)  # 4 outcomes * 3 models = 12 obs
write.xlsx(obs.res_hPDI_con,
           "results/MAIN_obs.res_hPDI_con.xlsx",
           overwrite = T)
obs.res_hPDI_con <-
  read.xlsx("results/MAIN_obs.res_hPDI_con.xlsx")

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
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "uPDI", 8]
  } else if (obs.res_uPDI_con$Model[i] == "Model 3") {
    obs.res_uPDI_con$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_con[obs.tbl_PDI.hPDI.uPDI_con$Outcome == obs.res_uPDI_con$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_con$Exposure == "uPDI", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_uPDI_con
dim(obs.res_uPDI_con)  # 4 outcomes * 3 models = 12 obs
write.xlsx(obs.res_uPDI_con,
           "results/MAIN_obs.res_uPDI_con.xlsx",
           overwrite = T)
obs.res_uPDI_con <-
  read.xlsx("results/MAIN_obs.res_uPDI_con.xlsx")

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
dim(obs.res_PDI.hPDI.uPDI_con)  # 4 outcomes * 3 models = 12 obs

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
  title = "Continuous outcomes in MoBa",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkorange", "darkgreen", "yellowgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21))

obs.forest_PDI.hPDI.uPDI_con

ggsave(
  obs.forest_PDI.hPDI.uPDI_con,
  file = "results/MAIN_obs.forest_PDI.hPDI.uPDI_con.png",
  height = 3,
  width = 7
)

################################################################################
