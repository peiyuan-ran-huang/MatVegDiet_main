################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 02-Dec-2024
# This script is to examine socioeconomic position (SEP) on both vegetarianism (in subgroups) and perinatal outcomes (with imputed data) in Project Viva.

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
  
  var_lab(dat[[i]]$VegDiet_bin) <- "Pesco-/full vs. non-vegetarian"
}

## Separate the 3-category exposure VegDiet_3cat into two binary exposures: pesco- vs. non-vegetarian and full vs. non-vegetarian
for (i in 1:dat_imp$m) {
  dat[[i]]$VegDiet_pesco.V_bin <- NA
  dat[[i]]$VegDiet_pesco.V_bin[dat[[i]]$VegDiet_3cat == "Non-vegetarian"] <- 0
  dat[[i]]$VegDiet_pesco.V_bin[dat[[i]]$VegDiet_3cat == "Pesco-vegetarian"] <- 1
  var_lab(dat[[i]]$VegDiet_pesco.V_bin) <- "Pesco- vs. non-vegetarian"
  
  dat[[i]]$VegDiet_full.V_bin <- NA
  dat[[i]]$VegDiet_full.V_bin[dat[[i]]$VegDiet_3cat == "Non-vegetarian"] <- 0
  dat[[i]]$VegDiet_full.V_bin[dat[[i]]$VegDiet_3cat == "Full vegetarian"] <- 1
  var_lab(dat[[i]]$VegDiet_full.V_bin) <- "Full vs. non-vegetarian"
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

# Create a function to extract beta, SE, and p-value (for both linear and logistic regression) - !!! Specifically for ordinal exposures !!!
IMP_extract_beta.SE_ord <- function(mymodel) {
  b <- summary(mymodel)[2, 2]
  se <- summary(mymodel)[2, 3]
  pval <- summary(mymodel)[2, 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

# Create empty output tables (for forestplots and meta-analysis)
obs.res_VegDiet_bin <- c()
obs.res_VegDiet_con <- c()

################################################################################

## Loop for each binary outcome
for (myoutcome in c("VegDiet_bin",
                    "VegDiet_pesco.V_bin",
                    "VegDiet_full.V_bin",
                    Viva_out_bin)) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 2 models - 1 for maternal education, 1 for IMD/household income
  #### Model 1 - Maternal education on binary vegetarianism and binary perinatal outcomes
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]], family = binomial)
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE_ord(mod_1_pooled)
  
  #### Model 2 - IMD/household income on binary vegetarianism and binary perinatal outcomes
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      glm(dat[[i]][[myoutcome]] ~ dat[[i]][["income_Fam_3cat"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]], family = binomial)
  }
  mod_2_pooled <- pool(mod_2)
  y_2 <- IMP_extract_beta.SE_ord(mod_2_pooled)
  
  ### Regression results for one outcome
  outcome_col <- as.data.frame(rep(outcome, 2))
  colnames(outcome_col) <- "Outcome"
  SEP_col <-
    as.data.frame(c(
      "Maternal education (higher vs. lower)",
      "Household income (higher vs. lower)"
    ))
  colnames(SEP_col) <- "SEP"
  y <- cbind(outcome_col, SEP_col, rbind(y_1, y_2))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_bin <- rbind(obs.res_VegDiet_bin, y)
}

################################################################################
################################################################################

for (myoutcome in Viva_out_con) {
  ### Outcome name
  outcome <- var_lab(dat_1[myoutcome])
  
  ### 2 models - 1 for maternal education, 1 for IMD/household income
  #### Model 1 - Maternal education on continuous perinatal outcomes
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["edu_Mat_3cat"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]])
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE_ord(mod_1_pooled)
  
  #### Model 2 - IMD/household income on continuous perinatal outcomes
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      lm(dat[[i]][[myoutcome]] ~ dat[[i]][["income_Fam_3cat"]] + dat[[i]][["age_Mat_con"]] + dat[[i]][["ethnic_Mat_bin"]])
  }
  mod_2_pooled <- pool(mod_2)
  y_2 <- IMP_extract_beta.SE_ord(mod_2_pooled)
  
  ### Regression results for one outcome
  outcome_col <- as.data.frame(rep(outcome, 2))
  colnames(outcome_col) <- "Outcome"
  SEP_col <-
    as.data.frame(c(
      "Maternal education (higher vs. lower)",
      "Household income (higher vs. lower)"
    ))
  colnames(SEP_col) <- "SEP"
  y <- cbind(outcome_col, SEP_col, rbind(y_1, y_2))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_con <- rbind(obs.res_VegDiet_con, y)
}

################################################################################

## View and save results
obs.res_VegDiet_bin <- as.data.frame(obs.res_VegDiet_bin)
obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # (2 + 1 exposures + 17 outcomes) * 2 models = 40 obs.

obs.res_VegDiet_con <- as.data.frame(obs.res_VegDiet_con)
obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 2 outcomes * 2 models = 4 obs.

obs.res_VegDiet_bin_con <- rbind(obs.res_VegDiet_bin, obs.res_VegDiet_con)

write.xlsx(
  obs.res_VegDiet_bin_con,
  "results/Viva/IMP_SEP_obs.res_VegDiet.subgroup_bin_con.xlsx",
  overwrite = T
)
obs.res_VegDiet_bin_con <-
  read.xlsx("results/Viva/IMP_SEP_obs.res_VegDiet.subgroup_bin_con.xlsx")

################################################################################

## Forest plots

### Prepare data
obs.res_VegDiet_bin_con$Group <- "Perinatal outcome (binary/continuous)"
obs.res_VegDiet_bin_con$Group[obs.res_VegDiet_bin_con$Outcome %in% c(
  "Pesco-/full vs. non-vegetarian",
  "Pesco- vs. non-vegetarian",
  "Full vs. non-vegetarian"
)] <- "Vegetarianism (binary)"
obs.res_VegDiet_bin_con$Group <-
  factor(
    obs.res_VegDiet_bin_con$Group,
    levels = c(
      "Vegetarianism (binary)",
      "Perinatal outcome (binary/continuous)"
    )
  )

obs.res_VegDiet_bin_con$Outcome <-
  factor(
    obs.res_VegDiet_bin_con$Outcome,
    levels = c(
      "Pesco-/full vs. non-vegetarian",
      "Pesco- vs. non-vegetarian",
      "Full vs. non-vegetarian",
      unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin_con$Outcome]
    )
  )
obs.res_VegDiet_bin_con <-
  obs.res_VegDiet_bin_con %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_VegDiet_bin_con$b <-
  as.numeric(obs.res_VegDiet_bin_con$b)
obs.res_VegDiet_bin_con$se <-
  as.numeric(obs.res_VegDiet_bin_con$se)
obs.res_VegDiet_bin_con$pval <-
  as.numeric(obs.res_VegDiet_bin_con$pval)

colnames(obs.res_VegDiet_bin_con)[colnames(obs.res_VegDiet_bin_con) == "SEP"] <- "SEP indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con$`SEP indicator` <- factor(
  obs.res_VegDiet_bin_con$`SEP indicator`,
  levels = c(
    "Household income (higher vs. lower)",
    "Maternal education (higher vs. lower)"
  )
)

head(obs.res_VegDiet_bin_con)
dim(obs.res_VegDiet_bin_con)  # 22 outcomes * 2 models = 44 obs.

################################################################################

### Nightingale forest plots
obs.forest_VegDiet_bin_con <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin_con,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `SEP indicator`,
  shape = `SEP indicator`,
  xlab = "LogOR or beta and 95% CI",
  title = "Effect of socioeconomic position (SEP) in Project Viva",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("blueviolet", "darkred")) +
  ggplot2::scale_shape_manual(values = c(21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

obs.forest_VegDiet_bin_con

ggsave(
  obs.forest_VegDiet_bin_con,
  file = "results/Viva/IMP_SEP_obs.forest_VegDiet.subgroup_bin_con.png",
  height = 8,
  width = 10
)

################################################################################
