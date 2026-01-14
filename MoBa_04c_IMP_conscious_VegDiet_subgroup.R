################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 17-Jun-2025
# This script is to examine the effect of health behaviours/consciousness on both vegetarianism (in subgroups) and perinatal outcomes (with imputed data) in MoBa.

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
  
  var_lab(dat[[i]]$VegDiet_bin) <- "Pesco-/full vs. non-vegetarian"
}

## Separate the 3-category exposure VegDiet_3cat into two binary exposures: pesco- vs. non-vegetarian and full vs. non-vegetarian
for (i in 1:dat_imp$m) {
  dat[[i]]$VegDiet_pesco.V_bin <- NA
  dat[[i]]$VegDiet_pesco.V_bin[dat[[i]]$VegDiet_3cat == "Non-vegetarian"] <-
    0
  dat[[i]]$VegDiet_pesco.V_bin[dat[[i]]$VegDiet_3cat == "Pesco-vegetarian"] <-
    1
  var_lab(dat[[i]]$VegDiet_pesco.V_bin) <-
    "Pesco- vs. non-vegetarian"
  
  dat[[i]]$VegDiet_full.V_bin <- NA
  dat[[i]]$VegDiet_full.V_bin[dat[[i]]$VegDiet_3cat == "Non-vegetarian"] <-
    0
  dat[[i]]$VegDiet_full.V_bin[dat[[i]]$VegDiet_3cat == "Full vegetarian"] <-
    1
  var_lab(dat[[i]]$VegDiet_full.V_bin) <- "Full vs. non-vegetarian"
}

## Generate composite health consciousness variable
for (i in 1:dat_imp$m) {
  score <- rowSums(
    cbind(
      dat[[i]]$smoking_Mat_EAR.p_bin == 0,
      dat[[i]]$alcohol_Mat_EAR.p_bin == 0,
      dat[[i]]$any.supp_Mat_EAR.p_bin == 1,
      dat[[i]]$BMI_Mat_PRE.p_con >= 18.5 &
        dat[[i]]$BMI_Mat_PRE.p_con < 25
    ),
    na.rm = F
  )
  dat[[i]]$conscious_Mat_ord <- as.numeric(score)
  var_lab(dat[[i]]$conscious_Mat_ord) <-
    "Maternal composite lifestyle index (0–4)"
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

# Create a function to extract beta, SE, and p-value (for both linear and logistic regression)
IMP_extract_beta.SE <- function(mymodel) {
  b <- summary(mymodel)[2, 2]
  se <- summary(mymodel)[2, 3]
  pval <- summary(mymodel)[2, 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

# Create a function to extract beta, SE, and p-value (for ORDINAL logistic regression)
IMP_extract_ord.beta.SE <- function(mymodel) {
  b <- summary(mymodel)[4, 2]
  se <- summary(mymodel)[4, 3]
  pval <- summary(mymodel)[4, 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

# Create empty output tables (for forestplots and meta-analysis)
obs.res_VegDiet_bin <- c()
obs.res_VegDiet_con <- c()
obs.res_VegDiet_ord <- c()

################################################################################

## Loop for each binary outcome
for (myoutcome in c("VegDiet_bin",
                    "VegDiet_pesco.V_bin",
                    "VegDiet_full.V_bin",
                    MoBa_out_bin)) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 1 model - for health behaviours/consciousness
  #### Model 1 - Maternal health behaviours/consciousness on binary vegetarianism and binary perinatal outcomes
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["conscious_Mat_ord"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]], family = binomial)
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE(mod_1_pooled)
  
  ### Regression results for one outcome
  outcome_col <- as.data.frame(rep(outcome, 1))
  colnames(outcome_col) <- "Outcome"
  HC_col <-
    as.data.frame(c("Maternal adherence to healthy behaviours (higher vs. lower)"))
  colnames(HC_col) <- "HC"
  y <- cbind(outcome_col, HC_col, rbind(y_1))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_bin <- rbind(obs.res_VegDiet_bin, y)
}

################################################################################
################################################################################

## Loop for each continuous outcome
for (myoutcome in MoBa_out_con) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 1 model - for health behaviours/consciousness
  #### Model 1 - Maternal health behaviours/consciousness on binary vegetarianism and binary perinatal outcomes
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["conscious_Mat_ord"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]])
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE(mod_1_pooled)
  
  ### Regression results for one outcome
  outcome_col <- as.data.frame(rep(outcome, 1))
  colnames(outcome_col) <- "Outcome"
  HC_col <-
    as.data.frame(c("Maternal adherence to healthy behaviours (higher vs. lower)"))
  colnames(HC_col) <- "HC"
  y <- cbind(outcome_col, HC_col, rbind(y_1))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_con <- rbind(obs.res_VegDiet_con, y)
}

################################################################################
################################################################################

## Loop for each ordinal outcome (breastfeeding duration only)
for (myoutcome in MoBa_out_cat) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 1 model - for health behaviours/consciousness
  #### Model 1 - Maternal health behaviours/consciousness on binary vegetarianism and binary perinatal outcomes
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      clm(dat[[i]][[myoutcome]] ~ dat[[i]][["conscious_Mat_ord"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]] + dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["income_Fam_3cat"]] + dat[[i]][["parity_Mat_bin"]])
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_ord.beta.SE(mod_1_pooled)
  
  ### Regression results for one outcome
  outcome_col <- as.data.frame(rep(outcome, 1))
  colnames(outcome_col) <- "Outcome"
  HC_col <-
    as.data.frame(c("Maternal adherence to healthy behaviours (higher vs. lower)"))
  colnames(HC_col) <- "HC"
  y <- cbind(outcome_col, HC_col, rbind(y_1))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_ord <- rbind(obs.res_VegDiet_ord, y)
}

################################################################################

## View and save results
obs.res_VegDiet_bin <- as.data.frame(obs.res_VegDiet_bin)
obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # (2 + 1 exposures + 21 outcomes) * 1 model = 24 obs.

obs.res_VegDiet_con <- as.data.frame(obs.res_VegDiet_con)
obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 4 outcomes * 1 model = 4 obs.

obs.res_VegDiet_bin_con <-
  rbind(obs.res_VegDiet_bin, obs.res_VegDiet_con)

obs.res_VegDiet_ord <- as.data.frame(obs.res_VegDiet_ord)
obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 1 outcome * 1 model = 1 obs.

obs.res_VegDiet_bin_con_ord <-
  rbind(obs.res_VegDiet_bin_con, obs.res_VegDiet_ord)

write.xlsx(
  obs.res_VegDiet_bin_con_ord,
  "results/IMP_conscious_obs.res_VegDiet.subgroup_bin_con_ord.xlsx",
  overwrite = T
)
obs.res_VegDiet_bin_con_ord <-
  read.xlsx("results/IMP_conscious_obs.res_VegDiet.subgroup_bin_con_ord.xlsx")

################################################################################

## Forest plots

### Prepare data
obs.res_VegDiet_bin_con_ord$Group <-
  "Perinatal outcome (binary/continuous)"
obs.res_VegDiet_bin_con_ord$Group[obs.res_VegDiet_bin_con_ord$Outcome %in% c(
  "Pesco-/full vs. non-vegetarian",
  "Pesco- vs. non-vegetarian",
  "Full vs. non-vegetarian"
)] <- "Vegetarianism (binary)"
obs.res_VegDiet_bin_con_ord$Group <-
  factor(
    obs.res_VegDiet_bin_con_ord$Group,
    levels = c(
      "Vegetarianism (binary)",
      "Perinatal outcome (binary/continuous)"
    )
  )

obs.res_VegDiet_bin_con_ord$Outcome <-
  factor(
    obs.res_VegDiet_bin_con_ord$Outcome,
    levels = c(
      "Pesco-/full vs. non-vegetarian",
      "Pesco- vs. non-vegetarian",
      "Full vs. non-vegetarian",
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

colnames(obs.res_VegDiet_bin_con_ord)[colnames(obs.res_VegDiet_bin_con_ord) == "HC"] <-
  "Indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord$Indicator <- factor(
  obs.res_VegDiet_bin_con_ord$Indicator,
  levels = c("Maternal adherence to healthy behaviours (higher vs. lower)")
)

head(obs.res_VegDiet_bin_con_ord)
dim(obs.res_VegDiet_bin_con_ord)  # 29 outcomes * 1 model = 29 obs.

################################################################################

### Nightingale forest plots
obs.forest_VegDiet_bin_con_ord <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin_con_ord,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Indicator,
  shape = Indicator,
  xlab = "LogOR or beta and 95% CI",
  title = "Effect of maternal health behaviours/consciousness in MoBa",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("seagreen")) +
  ggplot2::scale_shape_manual(values = c(21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_VegDiet_bin_con_ord

ggsave(
  obs.forest_VegDiet_bin_con_ord,
  file = "results/IMP_conscious_obs.forest_VegDiet.subgroup_bin_con_ord.png",
  height = 7,
  width = 12
)

################################################################################
