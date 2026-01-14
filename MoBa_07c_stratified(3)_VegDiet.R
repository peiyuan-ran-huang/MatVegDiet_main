################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 22-Dec-2024
# This script is to perform stratification analyses for vegetarian diets in MoBa.

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
head(dat)
dim(dat)  # 73868  XXX

################################################################################
# Outcomes for stratification analyses:
# 1) Maternal anaemia during pregnancy (highly relevant to iron status) - Stratified by iron supplement use
# 2) Maternal anaemia during pregnancy (highly relevant to iron status) - Stratified by DURATION of iron supplement use
# 3) Maternal anaemia during pregnancy (highly relevant to iron status) - Stratified by FREQUENCY of iron supplement use
################################################################################

# Create a function to extract beta, SE, and p-value (for both linear and logistic regression) - !!! Specifically for binary exposures !!!
extract_beta.SE_bin <- function(mymodel) {
  exp <-
    as.data.frame(c("Pesco-/full vegetarian"))
  colnames(exp) <- "Exposure"
  
  b <- summary(mymodel)$coef[2, 1]
  se <- summary(mymodel)$coef[2, 2]
  pval <- summary(mymodel)$coef[2, 4]
  
  x <- cbind(b, se, pval, exp)
  
  return(x)
}

# Create a function to extract logistic regression results (N, OR, 95% CI, p-value) - !!! Specifically for binary exposures !!!
extract_log.res_bin <- function(mymodel) {
  ## Non-vegetarian (ref)
  Exposure_1 <- "Non-vegetarian (ref)"
  N_1 <-
    paste0(
      sum(mymodel$model[, 2] == "Non-vegetarian" &
            mymodel$model[, 1] == "Yes"),
      " / ",
      sum(mymodel$model[, 2] == "Non-vegetarian")
    )
  OR_1 <- format(round(1.00, digits = 2), nsmall = 2)
  CI_1 <- "-"
  pval_1 <- "-"
  x_1 <- cbind(Exposure_1, N_1, OR_1, CI_1, pval_1)
  ##############################################################################
  ## Pesco-/full vegetarian
  Exposure_2 <- "Pesco-/full vegetarian"
  N_2 <-
    paste0(
      sum(mymodel$model[, 2] == "Vegetarian" &
            mymodel$model[, 1] == "Yes"),
      " / ",
      sum(mymodel$model[, 2] == "Vegetarian")
    )
  OR_2 <-
    format(round(exp(summary(mymodel)$coef[2, 1]), digits = 2), nsmall = 2)
  CI_2 <-
    paste0(format(round(
      exp(summary(mymodel)$coef[2, 1] - 1.96 * summary(mymodel)$coef[2, 2]), digits = 2
    ), nsmall = 2), ", ", format(round(
      exp(summary(mymodel)$coef[2, 1] + 1.96 * summary(mymodel)$coef[2, 2]), digits = 2
    ), nsmall = 2))  # confint() too time consuming for logistic models - calculating 95% CI manually
  pval_2 <-
    style_pvalue(summary(mymodel)$coefficients[2, 4], digits = 3)
  x_2 <- cbind(Exposure_2, N_2, OR_2, CI_2, pval_2)
  ##############################################################################
  
  x <- rbind(x_1, x_2)
  colnames(x) <- c("Exposure", "N", "OR", "CI", "pval")
  
  return(x)
}

################################################################################

# Maternal anaemia - Stratified by FREQUENCY of iron supplementation (measured before/up to dietary assessment)

## Subset samples
dat_iron_0 <-
  subset(dat,
         iron.supp.freq_Mat_EAR.p_cat == "No use")
dat_iron_1 <-
  subset(dat,
         iron.supp.freq_Mat_EAR.p_cat == "Less than daily")
dat_iron_2 <-
  subset(dat,
         iron.supp.freq_Mat_EAR.p_cat == "Daily")

################################################################################
################################################################################

## Analysis in each sub-sample

### Iron supplement use - No
mod_iron_0 <-
  glm(
    anaemia_preg_subsamp ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
    data = dat_iron_0,
    family = binomial
  )
x_iron_0 <-
  as.data.frame(extract_log.res_bin(mod_iron_0))
y_iron_0 <- extract_beta.SE_bin(mod_iron_0)

y_iron_0$Outcome <- "Maternal anaemia during pregnancy"
y_iron_0$`Iron supplement use` <- "No"

for (i in 1:nrow(y_iron_0)) {
  y_iron_0$N_exp[i] <-
    x_iron_0$N[x_iron_0$Exposure == y_iron_0$Exposure[i]]
  y_iron_0$N_ref[i] <-
    x_iron_0$N[x_iron_0$Exposure == "Non-vegetarian (ref)"]
}

y_iron_0

################################################################################
################################################################################

### Iron supplement use - Less than daily
mod_iron_1 <-
  glm(
    anaemia_preg_subsamp ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
    data = dat_iron_1,
    family = binomial
  )
x_iron_1 <-
  as.data.frame(extract_log.res_bin(mod_iron_1))
y_iron_1 <- extract_beta.SE_bin(mod_iron_1)

y_iron_1$Outcome <- "Maternal anaemia during pregnancy"
y_iron_1$`Iron supplement use` <- "Less than daily"

for (i in 1:nrow(y_iron_1)) {
  y_iron_1$N_exp[i] <-
    x_iron_1$N[x_iron_1$Exposure == y_iron_1$Exposure[i]]
  y_iron_1$N_ref[i] <-
    x_iron_1$N[x_iron_1$Exposure == "Non-vegetarian (ref)"]
}

y_iron_1

################################################################################
################################################################################

### Iron supplement use - Daily
mod_iron_2 <-
  glm(
    anaemia_preg_subsamp ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
    data = dat_iron_2,
    family = binomial
  )
x_iron_2 <-
  as.data.frame(extract_log.res_bin(mod_iron_2))
y_iron_2 <- extract_beta.SE_bin(mod_iron_2)

y_iron_2$Outcome <- "Maternal anaemia during pregnancy"
y_iron_2$`Iron supplement use` <- "Daily"

for (i in 1:nrow(y_iron_2)) {
  y_iron_2$N_exp[i] <-
    x_iron_2$N[x_iron_2$Exposure == y_iron_2$Exposure[i]]
  y_iron_2$N_ref[i] <-
    x_iron_2$N[x_iron_2$Exposure == "Non-vegetarian (ref)"]
}

y_iron_2

################################################################################

## Combine, view, and save results
obs.res_VegDiet_bin <-
  rbind(y_iron_0, y_iron_1, y_iron_2)

obs.res_VegDiet_bin

write.xlsx(
  obs.res_VegDiet_bin,
  "results/STRAT.iron.freq_obs.res_VegDiet_bin.xlsx",
  overwrite = T
)

################################################################################

x_iron_0  # Iron supplement use - No
x_iron_1  # Iron supplement use - Less than daily
x_iron_2  # Iron supplement use - Daily

################################################################################

## Nightingale forest plots
obs.forest_VegDiet_bin <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin,
  name = `Iron supplement use`,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Exposure,
  xlab = "OR and 95% CI (ref: non-vegetarian)",
  title = "Maternal anaemia in MoBa\n(occurring in pregnancy)",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21))

obs.forest_VegDiet_bin

ggsave(
  obs.forest_VegDiet_bin,
  file = "results/STRAT.iron.freq_obs.forest_VegDiet_bin.png",
  height = 3,
  width = 6
)
