################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 24-Nov-2024
# This script is to perform negative control outcome analysis on breastfeeding for vegetarian diets in ALSPAC.

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
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/ALSPAC/dat_exp_cov_out_pat.rds")
head(dat)
dim(dat)  # 11693  XXX

################################################################################
# Modelling
## Model 1: exposure only - i.e., the unadjusted model
## Model 2: + age, ethnicity, education, IMD, parity, pre-pregnancy BMI, smoking, alcohol drinking, offspring sex - i.e., additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
## Model 3: + dietary supplementation, total energy intake, hPDIm - i.e., additionally adjusting for nutrition-related factors
################################################################################

# Create a function to extract regression results with SE (for ORDINAL logistic regression) - !!! Specifically for binary exposures
extract_ord.beta.SE_bin <- function(mymodel) {
  exp <-
    as.data.frame(c("Pesco-/full vegetarian"))
  colnames(exp) <- "Exposure"
  
  b <- summary(mymodel)$coef[4, 1]
  se <- summary(mymodel)$coef[4, 2]
  pval <- summary(mymodel)$coef[4, 4]
  
  x <- cbind(b, se, pval, exp)
  
  return(x)
}

# Create a function to extract ORDINAL logistic regression results - !!! Specifically for binary exposures
extract_ord.log.res_bin <- function(mymodel) {
  ## Non-vegetarian (ref)
  Exposure_1 <- "Non-vegetarian (ref)"
  N_1 <- sum(mymodel$model[, 2] == "Non-vegetarian")
  OR_1 <- format(round(1.00, digits = 2), nsmall = 2)
  CI_1 <- "-"
  pval_1 <- "-"
  x_1 <- cbind(Exposure_1, N_1, OR_1, CI_1, pval_1)
  ##############################################################################
  ## Pesco-vegetarian
  Exposure_2 <- "Pesco-/full vegetarian"
  N_2 <- sum(mymodel$model[, 2] == "Vegetarian")
  OR_2 <-
    format(round(exp(summary(mymodel)$coef[4, 1]), digits = 2), nsmall = 2)
  CI_2 <-
    paste0(format(round(
      exp(summary(mymodel)$coef[4, 1] - 1.96 * summary(mymodel)$coef[4, 2]), digits = 2
    ), nsmall = 2), ", ", format(round(
      exp(summary(mymodel)$coef[4, 1] + 1.96 * summary(mymodel)$coef[4, 2]), digits = 2
    ), nsmall = 2))  # confint() too time consuming for logistic models - calculating 95% CI manually
  pval_2 <-
    style_pvalue(summary(mymodel)$coefficients[4, 4], digits = 3)
  x_2 <- cbind(Exposure_2, N_2, OR_2, CI_2, pval_2)
  ##############################################################################
  
  x <- rbind(x_1, x_2)
  colnames(x) <- c("Exposure", "N", "OR", "CI", "pval")
  
  return(x)
}

################################################################################
################################################################################

# Vegetarian diet and breastfeeding duration (3 categories) - Ordinal logistic regression

## Model 1 - Univariate (unadjusted) model
mod_1 <-
  clm(bf_dur_4c ~ VegDiet_bin, data = dat)
x_1 <- extract_ord.log.res_bin(mod_1)
y_1 <- extract_ord.beta.SE_bin(mod_1)

## Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_2 <-
  clm(
    bf_dur_4c ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
    data = dat
  )
x_2 <- extract_ord.log.res_bin(mod_2)
y_2 <- extract_ord.beta.SE_bin(mod_2)

## Model 3 - Additionally adjusting for nutrition-related factors (including hPDIm as the indicator for the healthfulness of diet)
mod_3 <-
  clm(
    bf_dur_4c ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + IMD_Fam_cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin + hPDIm,
    data = dat
  )
x_3 <- extract_ord.log.res_bin(mod_3)
y_3 <- extract_ord.beta.SE_bin(mod_3)

################################################################################
################################################################################

## Combine regression results
outcome <- var_lab(dat$bf_dur_4c)

outcome_x <- as.data.frame(rep(outcome, 2))
colnames(outcome_x) <- "Outcome"
x <-
  cbind(outcome_x, x_1, x_2, x_3)  # Columns: Outcome name | Model 1 | Model 2 | Model 3; rows: Non-vegetarian | Pesco-/full vegetarian

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
obs.tbl_VegDiet_ord[, c(7, 12)] <-
  " "  # For convenience when making tables
obs.tbl_VegDiet_ord
dim(obs.tbl_VegDiet_ord)  # 1 outcome for 2 categories in 3 models
write.xlsx(obs.tbl_VegDiet_ord,
           "results/ALSPAC/BF_obs.tbl_VegDiet_ord.xlsx",
           overwrite = T)
obs.tbl_VegDiet_ord <-
  read.xlsx("results/ALSPAC/BF_obs.tbl_VegDiet_ord.xlsx")

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
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 8]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 8]
  } else if (obs.res_VegDiet_ord$Model[i] == "Model 3") {
    obs.res_VegDiet_ord$N_exp[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == obs.res_VegDiet_ord$Exposure[i], 13]
    obs.res_VegDiet_ord$N_ref[i] <-
      obs.tbl_VegDiet_ord[obs.tbl_VegDiet_ord$Outcome == obs.res_VegDiet_ord$Outcome[i] &
                            obs.tbl_VegDiet_ord$Exposure == "Non-vegetarian (ref)", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 1 outcome * 1 category * 3 models = 3 obs
write.xlsx(obs.res_VegDiet_ord,
           "results/ALSPAC/BF_obs.res_VegDiet_ord.xlsx",
           overwrite = T)
obs.res_VegDiet_ord <-
  read.xlsx("results/ALSPAC/BF_obs.res_VegDiet_ord.xlsx")

################################################################################

# Show the effect of SES on vegetarianism and breastfeeding, respectively

## Effect of maternal education (ordinal)...

### ...on pesco-/full vegetarianism (binary)
mod <-
  glm(
    VegDiet_bin ~ edu_Mat_3cat + age_Mat_con + ethnic_Mat_bin,
    data = dat,
    family = binomial
  )

edu_V <-
  data.frame(
    Exposure = "Maternal education",
    Outcome = "Pesco-/full vegetarianism",
    b = as.numeric(summary(mod)$coef[2, 1]),
    se = as.numeric(summary(mod)$coef[2, 2]),
    pval = as.numeric(summary(mod)$coef[2, 4])
  )

edu_V

### ...on breastfeeding duration (ordinal)
mod <- clm(
  bf_dur_4c ~ edu_Mat_3cat + age_Mat_con + ethnic_Mat_bin,
  data = dat,
  family = binomial
)

edu_bf.dur <-
  data.frame(
    Exposure = "Maternal education",
    Outcome = "Breastfeeding duration",
    b = as.numeric(summary(mod)$coef[4, 1]),
    se = as.numeric(summary(mod)$coef[4, 2]),
    pval = as.numeric(summary(mod)$coef[4, 4])
  )

edu_bf.dur

################################################################################
################################################################################

## Effect of IMD (ordinal)...

### ...on pesco-/full vegetarianism (binary)
mod <-
  glm(
    VegDiet_bin ~ IMD_Fam_cat + age_Mat_con + ethnic_Mat_bin,
    data = dat,
    family = binomial
  )

IMD_V <-
  data.frame(
    Exposure = "IMD (more vs. less affluent)",
    Outcome = "Pesco-/full vegetarianism",
    b = as.numeric(summary(mod)$coef[2, 1]),
    se = as.numeric(summary(mod)$coef[2, 2]),
    pval = as.numeric(summary(mod)$coef[2, 4])
  )

IMD_V

### ...on breastfeeding duration (ordinal)
mod <-
  clm(
    bf_dur_4c ~ IMD_Fam_cat + age_Mat_con + ethnic_Mat_bin,
    data = dat,
    family = binomial
  )

IMD_bf.dur <-
  data.frame(
    Exposure = "IMD (more vs. less affluent)",
    Outcome = "Breastfeeding duration",
    b = as.numeric(summary(mod)$coef[4, 1]),
    se = as.numeric(summary(mod)$coef[4, 2]),
    pval = as.numeric(summary(mod)$coef[4, 4])
  )

IMD_bf.dur

################################################################################
################################################################################

## Combine, view, and save results
obs.res_VegDiet_SEP <-
  rbind(edu_V, edu_bf.dur, IMD_V, IMD_bf.dur)

obs.res_VegDiet_SEP
dim(obs.res_VegDiet_SEP)  # 2 exposures * 2 outcomes = 4 obs

write.xlsx(obs.res_VegDiet_SEP,
           "results/ALSPAC/BF_obs.res_VegDiet_SEP.xlsx",
           overwrite = T)
obs.res_VegDiet_SEP <-
  read.xlsx("results/ALSPAC/BF_obs.res_VegDiet_SEP.xlsx")

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
  title = "Effect of socioeconomic position in ALSPAC",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("palevioletred1", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

obs.forest_VegDiet_SEP

ggsave(
  obs.forest_VegDiet_SEP,
  file = "results/ALSPAC/BF_obs.forest_VegDiet_SEP.png",
  height = 2,
  width = 9
)

################################################################################

# Check
mod_1 <-
  clm(bf_dur_4c ~ VegDiet_bin, data = dat)  # Unadjusted model
as.data.frame(extract_ord.log.res_bin(mod_1))

mod_2 <-
  clm(bf_dur_4c ~ VegDiet_bin + edu_Mat_3cat, data = dat)  # Adjusted for maternal education only
as.data.frame(extract_ord.log.res_bin(mod_2))

mod_3 <-
  clm(bf_dur_4c ~ VegDiet_bin + IMD_Fam_cat, data = dat)  # Adjusted for IMD only
as.data.frame(extract_ord.log.res_bin(mod_3))

mod_4 <-
  clm(bf_dur_4c ~ VegDiet_bin + edu_Mat_3cat + IMD_Fam_cat, data = dat)  # Adjusted for both maternal education and IMD
as.data.frame(extract_ord.log.res_bin(mod_4))
