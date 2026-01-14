################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 02-Dec-2024
# This script is to perform negative control outcome analysis on breastfeeding (with imputed data) for vegetarian diets in Project Viva.

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
# Modelling
## Model 1: exposure only - i.e., the unadjusted model
## Model 2: + age, ethnicity, education, income, parity, pre-pregnancy BMI, smoking, alcohol drinking, offspring sex - i.e., additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
## Model 3: + dietary supplementation, total energy intake, hPDIm - i.e., additionally adjusting for nutrition-related factors
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

# Create a function to extract ORDINAL logistic regression results - !!! Specifically for binary exposures
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

################################################################################
################################################################################

# Vegetarian diet and breastfeeding duration (3 categories) - Ordinal logistic regression

## Model 1 - Univariate (unadjusted) model
mod_1 <- list()
for (i in 1:dat_imp$m) {
  mod_1[[i]] <-
    clm(bf_dur_4c ~ VegDiet_bin, data = dat[[i]])
}
mod_1_pooled <- pool(mod_1)
x_1 <- IMP_extract_ord.log.res_bin(mod_1_pooled)
y_1 <- IMP_extract_ord.beta.SE_bin(mod_1_pooled)

## Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_2 <- list()
for (i in 1:dat_imp$m) {
  mod_2[[i]] <-
    clm(
      bf_dur_4c ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
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
      bf_dur_4c ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin + hPDIm,
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
  as.data.frame(c(nrow(dat_1[dat_1$VegDiet_bin == 0 &
                               !is.na(dat_1$bf_dur_4c), ]), nrow(dat_1[dat_1$VegDiet_bin == 1 &
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
write.xlsx(obs.tbl_VegDiet_ord,
           "results/Viva/IMP_BF_obs.tbl_VegDiet_ord.xlsx",
           overwrite = T)
obs.tbl_VegDiet_ord <-
  read.xlsx("results/Viva/IMP_BF_obs.tbl_VegDiet_ord.xlsx")

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
write.xlsx(obs.res_VegDiet_ord,
           "results/Viva/IMP_BF_obs.res_VegDiet_ord.xlsx",
           overwrite = T)
obs.res_VegDiet_ord <-
  read.xlsx("results/Viva/IMP_BF_obs.res_VegDiet_ord.xlsx")

################################################################################

# Show the effect of SES on vegetarianism and breastfeeding, respectively

## Effect of maternal education (ordinal)...

### ...on pesco-/full vegetarianism (binary)
mod <- list()

for (i in 1:dat_imp$m) {
  mod[[i]] <- glm(
    VegDiet_bin ~ edu_Mat_3cat + age_Mat_con + ethnic_Mat_bin,
    data = dat[[i]],
    family = binomial
  )
}

mod_pooled <- pool(mod)

edu_V <-
  data.frame(
    Exposure = "Maternal education",
    Outcome = "Pesco-/full vegetarianism",
    b = as.numeric(summary(mod_pooled)[2, 2]),
    se = as.numeric(summary(mod_pooled)[2, 3]),
    pval = as.numeric(summary(mod_pooled)[2, 6])
  )

edu_V

### ...on breastfeeding duration (ordinal)
mod <- list()

for (i in 1:dat_imp$m) {
  mod[[i]] <- clm(
    bf_dur_4c ~ edu_Mat_3cat + age_Mat_con + ethnic_Mat_bin,
    data = dat[[i]],
    family = binomial
  )
}

mod_pooled <- pool(mod)

edu_bf.dur <-
  data.frame(
    Exposure = "Maternal education",
    Outcome = "Breastfeeding duration",
    b = as.numeric(summary(mod_pooled)[4, 2]),
    se = as.numeric(summary(mod_pooled)[4, 3]),
    pval = as.numeric(summary(mod_pooled)[4, 6])
  )

edu_bf.dur

################################################################################
################################################################################

## Effect of household income (ordinal)...

### ...on pesco-/full vegetarianism (binary)
mod <- list()

for (i in 1:dat_imp$m) {
  mod[[i]] <- glm(
    VegDiet_bin ~ income_Fam_3cat + age_Mat_con + ethnic_Mat_bin,
    data = dat[[i]],
    family = binomial
  )
}

mod_pooled <- pool(mod)

income_V <-
  data.frame(
    Exposure = "Household income",
    Outcome = "Pesco-/full vegetarianism",
    b = as.numeric(summary(mod_pooled)[2, 2]),
    se = as.numeric(summary(mod_pooled)[2, 3]),
    pval = as.numeric(summary(mod_pooled)[2, 6])
  )

income_V

### ...on breastfeeding duration (ordinal)
mod <- list()

for (i in 1:dat_imp$m) {
  mod[[i]] <- clm(
    bf_dur_4c ~ income_Fam_3cat + age_Mat_con + ethnic_Mat_bin,
    data = dat[[i]],
    family = binomial
  )
}

mod_pooled <- pool(mod)

income_bf.dur <-
  data.frame(
    Exposure = "Household income",
    Outcome = "Breastfeeding duration",
    b = as.numeric(summary(mod_pooled)[4, 2]),
    se = as.numeric(summary(mod_pooled)[4, 3]),
    pval = as.numeric(summary(mod_pooled)[4, 6])
  )

income_bf.dur

################################################################################
################################################################################

## Combine, view, and save results
obs.res_VegDiet_SEP <-
  rbind(edu_V, edu_bf.dur, income_V, income_bf.dur)

obs.res_VegDiet_SEP
dim(obs.res_VegDiet_SEP)  # 2 exposures * 2 outcomes = 4 obs

write.xlsx(obs.res_VegDiet_SEP,
           "results/Viva/IMP_BF_obs.res_VegDiet_SEP.xlsx",
           overwrite = T)
obs.res_VegDiet_SEP <-
  read.xlsx("results/Viva/IMP_BF_obs.res_VegDiet_SEP.xlsx")

################################################################################
################################################################################

## Show all results in a forestplot

### Prepare data
obs.res_VegDiet_SEP <- obs.res_VegDiet_SEP %>%
  mutate(`Exposure  ->  Outcome` = paste0(Exposure, "  ->  ", Outcome))

obs.res_VegDiet_SEP$`Exposure  ->  Outcome` <-
  factor(obs.res_VegDiet_SEP$`Exposure  ->  Outcome`, levels = rev(unique(
    obs.res_VegDiet_SEP$`Exposure  ->  Outcome`
  )))  # Make sure each result appears in the plot in the right order

obs.res_VegDiet_SEP$Outcome <-
  factor(
    obs.res_VegDiet_SEP$Outcome,
    levels = c("Breastfeeding duration", "Pesco-/full vegetarianism")
  )  # Make sure each result appears in the plot in the right colour

### Nightingale forest plots
obs.forest_VegDiet_SEP <- ggforestplot::forestplot(
  df = obs.res_VegDiet_SEP,
  name = `Exposure  ->  Outcome`,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Outcome,
  shape = Outcome,
  xlab = "OR and 95% CI",
  title = "Effect of socioeconomic position in Project Viva",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("palevioletred1", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

obs.forest_VegDiet_SEP

ggsave(
  obs.forest_VegDiet_SEP,
  file = "results/Viva/IMP_BF_obs.forest_VegDiet_SEP.png",
  height = 2,
  width = 9
)

################################################################################
