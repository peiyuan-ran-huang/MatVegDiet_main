################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 08-Dec-2024
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
# 2)
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

# Maternal anaemia - Stratified by iron supplementation (measured before/up to dietary assessment)

## Subset samples
dat_N_iron <-
  subset(dat, iron.supp_Mat_EAR.p_bin == "No")
dat_iron <-
  subset(dat, iron.supp_Mat_EAR.p_bin == "Yes")

################################################################################
################################################################################

## Analysis in each sub-sample

### Iron supplement use - No
mod_N_iron <-
  glm(
    anaemia_preg_all ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
    data = dat_N_iron,
    family = binomial
  )
x_N_iron <-
  as.data.frame(extract_log.res_bin(mod_N_iron))
y_N_iron <- extract_beta.SE_bin(mod_N_iron)

y_N_iron$Outcome <- "Maternal anaemia during pregnancy"
y_N_iron$`Supplementation` <- "Iron supplement use - No"

for (i in 1:nrow(y_N_iron)) {
  y_N_iron$N_exp[i] <-
    x_N_iron$N[x_N_iron$Exposure == y_N_iron$Exposure[i]]
  y_N_iron$N_ref[i] <-
    x_N_iron$N[x_N_iron$Exposure == "Non-vegetarian (ref)"]
}

y_N_iron

################################################################################
################################################################################

### Iron supplement use - Yes
mod_iron <-
  glm(
    anaemia_preg_all ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
    data = dat_iron,
    family = binomial
  )
x_iron <- as.data.frame(extract_log.res_bin(mod_iron))
y_iron <- extract_beta.SE_bin(mod_iron)

y_iron$Outcome <- "Maternal anaemia during pregnancy"
y_iron$`Supplementation` <- "Iron supplement use - Yes"

for (i in 1:nrow(y_iron)) {
  y_iron$N_exp[i] <- x_iron$N[x_iron$Exposure == y_iron$Exposure[i]]
  y_iron$N_ref[i] <-
    x_iron$N[x_iron$Exposure == "Non-vegetarian (ref)"]
}

y_iron

################################################################################
################################################################################

## Combine, view, and save results
obs.res_VegDiet_bin <-
  rbind(y_N_iron, y_iron)

obs.res_VegDiet_bin

write.xlsx(obs.res_VegDiet_bin,
           "results/STRAT.iron_obs.res_VegDiet_bin.xlsx",
           overwrite = T)
obs.res_VegDiet_bin <-
  read.xlsx("results/STRAT.iron_obs.res_VegDiet_bin.xlsx")

################################################################################

x_N_iron  # Iron supplementation - No

x_iron  # Iron supplementation - Yes

################################################################################

## Nightingale forest plots
obs.forest_VegDiet_bin <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin,
  name = Supplementation,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Exposure,
  xlab = "OR and 95% CI (ref: non-vegetarian)",
  title = "Maternal anaemia in MoBa",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21))

obs.forest_VegDiet_bin

ggsave(
  obs.forest_VegDiet_bin,
  file = "results/STRAT.iron_obs.forest_VegDiet_bin.png",
  height = 2,
  width = 6
)
