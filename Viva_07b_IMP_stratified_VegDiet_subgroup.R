################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 09-Oct-2024
# This script is to perform stratification analyses (with imputed data) for vegetarian diets (in subgroups) in Project Viva.

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
# Outcomes for stratification analyses:
# 1) Maternal anaemia during pregnancy (highly relevant to iron status) - Stratified by iron supplement use
# 2)
################################################################################

# Create a function to extract beta, SE, and p-value (for both linear and logistic regression) - !!! Specifically for 3-categories exposures !!!
IMP_extract_beta.SE_3cat <- function(mymodel) {
  exp <-
    as.data.frame(c("Pesco-vegetarian", "Full vegetarian"))
  colnames(exp) <- "Exposure"
  
  b <- summary(mymodel)[2:3, 2]
  se <- summary(mymodel)[2:3, 3]
  pval <- summary(mymodel)[2:3, 6]
  
  x <- cbind(b, se, pval, exp)
  
  return(x)
}

# Create a function to extract logistic regression results (N, OR, 95% CI, p-value) - !!! Specifically for 3-categories exposures !!!
IMP_extract_log.res_3cat <- function(mymodel) {
  ## Non-vegetarian (ref)
  Exposure_1 <- "Non-vegetarian (ref)"
  OR_1 <- format(round(1.00, digits = 2), nsmall = 2)
  CI_1 <- "-"
  pval_1 <- "-"
  x_1 <- cbind(Exposure_1, OR_1, CI_1, pval_1)
  ##############################################################################
  ## Pesco-vegetarian
  Exposure_2 <- "Pesco-vegetarian"
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
  ## Full vegetarian
  Exposure_3 <- "Full vegetarian"
  OR_3 <-
    format(round(exp(summary(mymodel)[3, 2]), digits = 2), nsmall = 2)
  CI_3 <- paste0(format(round(exp(
    summary(mymodel, conf.int = T)[3, 7]
  ), digits = 2), nsmall = 2), ", ", format(round(exp(
    summary(mymodel, conf.int = T)[3, 8]
  ), digits = 2), nsmall = 2))
  pval_3 <-
    style_pvalue(summary(mymodel)[3, 6], digits = 3)
  x_3 <- cbind(Exposure_3, OR_3, CI_3, pval_3)
  ##############################################################################
  
  x <- rbind(x_1, x_2, x_3)
  colnames(x) <- c("Exposure", "OR", "CI", "pval")
  
  return(x)
}

################################################################################

# Maternal anaemia - Stratified by iron (+ prenatal multivitamins) supplementation (measured before/up to dietary assessment)

## Subset samples
dat_N_iron <-
  subset(dat_1, iron.multivit.supp_Mat_EAR.p_bin == 0)
dat_iron <-
  subset(dat_1, iron.multivit.supp_Mat_EAR.p_bin == 1)

################################################################################
################################################################################

## Analysis in each sub-sample

### Iron supplement use - No
mod_N_iron <- list()
for (i in 1:dat_imp$m) {
  mod_N_iron[[i]] <-
    glm(
      anaemia_preg_all ~ VegDiet_3cat + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.multivit.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
      data = subset(dat[[i]], iron.multivit.supp_Mat_EAR.p_bin == 0),
      family = binomial
    )
}
mod_N_iron_pooled <- pool(mod_N_iron)

x_N_iron <-
  as.data.frame(IMP_extract_log.res_3cat(mod_N_iron_pooled))
y_N_iron <-
  IMP_extract_beta.SE_3cat(mod_N_iron_pooled)

y_N_iron$Outcome <- "Maternal anaemia during pregnancy"
y_N_iron$`Supplementation` <- "Iron supplement use - No"

for (i in 1:nrow(y_N_iron)) {
  y_N_iron$N_exp[i] <- paste0(
    sum(
      dat_N_iron$VegDiet_3cat == y_N_iron$Exposure[i] &
        dat_N_iron$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_N_iron$VegDiet_3cat == y_N_iron$Exposure[i], na.rm = T)
  )
  
  y_N_iron$N_ref[i] <- paste0(
    sum(
      dat_N_iron$VegDiet_3cat == "Non-vegetarian" &
        dat_N_iron$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_N_iron$VegDiet_3cat == "Non-vegetarian", na.rm = T)
  )
}

y_N_iron

################################################################################
################################################################################

### Iron supplement use - Yes
mod_iron <- list()
for (i in 1:dat_imp$m) {
  mod_iron[[i]] <-
    glm(
      anaemia_preg_all ~ VegDiet_3cat + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.multivit.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
      data = subset(dat[[i]], iron.multivit.supp_Mat_EAR.p_bin == 1),
      family = binomial
    )
}
mod_iron_pooled <- pool(mod_iron)

x_iron <-
  as.data.frame(IMP_extract_log.res_3cat(mod_iron_pooled))
y_iron <-
  IMP_extract_beta.SE_3cat(mod_iron_pooled)

y_iron$Outcome <- "Maternal anaemia during pregnancy"
y_iron$`Supplementation` <- "Iron supplement use - Yes"

for (i in 1:nrow(y_iron)) {
  y_iron$N_exp[i] <- paste0(
    sum(
      dat_iron$VegDiet_3cat == y_iron$Exposure[i] &
        dat_iron$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron$VegDiet_3cat == y_iron$Exposure[i], na.rm = T)
  )
  
  y_iron$N_ref[i] <- paste0(
    sum(
      dat_iron$VegDiet_3cat == "Non-vegetarian" &
        dat_iron$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron$VegDiet_3cat == "Non-vegetarian", na.rm = T)
  )
}

y_iron

################################################################################
################################################################################

## Combine, view, and save results
obs.res_VegDiet_bin <-
  rbind(y_N_iron, y_iron)

obs.res_VegDiet_bin

write.xlsx(
  obs.res_VegDiet_bin,
  "results/Viva/IMP_STRAT.iron_obs.res_VegDiet.subgroup_bin.xlsx",
  overwrite = T
)
obs.res_VegDiet_bin <-
  read.xlsx("results/Viva/IMP_STRAT.iron_obs.res_VegDiet.subgroup_bin.xlsx")

################################################################################

x_N_iron  # Iron supplementation - No

x_iron  # Iron supplementation - Yes

################################################################################

## Nightingale forest plots
obs.res_VegDiet_bin <- subset(obs.res_VegDiet_bin, se < 100)  # Remove extreme SE values

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
  title = "Maternal anaemia in Project Viva",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("green4", "mediumblue")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

obs.forest_VegDiet_bin

ggsave(
  obs.forest_VegDiet_bin,
  file = "results/Viva/IMP_STRAT.iron_obs.forest_VegDiet.subgroup_bin.png",
  height = 2,
  width = 6
)
