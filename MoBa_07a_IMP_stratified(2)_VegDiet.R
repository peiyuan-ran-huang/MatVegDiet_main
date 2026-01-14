################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 22-Dec-2024
# This script is to perform stratification analyses (with imputed data) for vegetarian diets in MoBa.

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
# Outcomes for stratification analyses:
# 1) Maternal anaemia during pregnancy (highly relevant to iron status) - Stratified by iron supplement use
# 2) Maternal anaemia during pregnancy (highly relevant to iron status) - Stratified by DURATION of iron supplement use
# 3)
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

# Create a function to extract logistic regression results (N, OR, 95% CI, p-value) - !!! Specifically for binary exposures !!!
IMP_extract_log.res_bin <- function(mymodel) {
  ## Non-vegetarian (ref)
  Exposure_1 <- "Non-vegetarian (ref)"
  OR_1 <- format(round(1.00, digits = 2), nsmall = 2)
  CI_1 <- "-"
  pval_1 <- "-"
  x_1 <- cbind(Exposure_1, OR_1, CI_1, pval_1)
  ##############################################################################
  ## Pesco-/full vegetarian
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

################################################################################

# Maternal anaemia - Stratified by DURATION of iron supplementation (measured before/up to dietary assessment)

## Subset samples
dat_iron_0 <-
  subset(dat_1,
         iron.supp.dur_Mat_EAR.p_cat == "No use")
dat_iron_1 <-
  subset(dat_1,
         iron.supp.dur_Mat_EAR.p_cat == "Started after 1st trimester")
dat_iron_2 <-
  subset(dat_1,
         iron.supp.dur_Mat_EAR.p_cat == "Started in 1st trimester")
dat_iron_3 <-
  subset(dat_1,
         iron.supp.dur_Mat_EAR.p_cat == "Started before pregnancy")

################################################################################
################################################################################

## Analysis in each sub-sample

### Iron supplement use - No
mod_iron_0 <- list()
for (i in 1:dat_imp$m) {
  mod_iron_0[[i]] <-
    glm(
      anaemia_preg_subsamp ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
      data = subset(dat[[i]], iron.supp.dur_Mat_EAR.p_cat == "No use"),
      family = binomial
    )
}
mod_iron_0_pooled <- pool(mod_iron_0)

x_iron_0 <-
  as.data.frame(IMP_extract_log.res_bin(mod_iron_0_pooled))
y_iron_0 <-
  IMP_extract_beta.SE_bin(mod_iron_0_pooled)

y_iron_0$Outcome <- "Maternal anaemia occurring in pregnancy"
y_iron_0$`Iron supplement use` <- "No"

for (i in 1:nrow(y_iron_0)) {
  y_iron_0$N_exp[i] <- paste0(
    sum(
      dat_iron_0$VegDiet_bin == 1 &
        dat_iron_0$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron_0$VegDiet_bin == 1, na.rm = T)
  )
  
  y_iron_0$N_ref[i] <- paste0(
    sum(
      dat_iron_0$VegDiet_bin == 0 &
        dat_iron_0$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron_0$VegDiet_bin == 0, na.rm = T)
  )
}

y_iron_0

################################################################################
################################################################################

### Iron supplement use - Started after 1st trimester
mod_iron_1 <- list()
for (i in 1:dat_imp$m) {
  mod_iron_1[[i]] <-
    glm(
      anaemia_preg_subsamp ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
      data = subset(
        dat[[i]],
        iron.supp.dur_Mat_EAR.p_cat == "Started after 1st trimester"
      ),
      family = binomial
    )
}
mod_iron_1_pooled <- pool(mod_iron_1)

x_iron_1 <-
  as.data.frame(IMP_extract_log.res_bin(mod_iron_1_pooled))
y_iron_1 <-
  IMP_extract_beta.SE_bin(mod_iron_1_pooled)

y_iron_1$Outcome <- "Maternal anaemia occurring in pregnancy"
y_iron_1$`Iron supplement use` <- "Started after 1st trimester"

for (i in 1:nrow(y_iron_1)) {
  y_iron_1$N_exp[i] <- paste0(
    sum(
      dat_iron_1$VegDiet_bin == 1 &
        dat_iron_1$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron_1$VegDiet_bin == 1, na.rm = T)
  )
  
  y_iron_1$N_ref[i] <- paste0(
    sum(
      dat_iron_1$VegDiet_bin == 0 &
        dat_iron_1$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron_1$VegDiet_bin == 0, na.rm = T)
  )
}

y_iron_1

################################################################################
################################################################################

### Iron supplement use - Started in 1st trimester
mod_iron_2 <- list()
for (i in 1:dat_imp$m) {
  mod_iron_2[[i]] <-
    glm(
      anaemia_preg_subsamp ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
      data = subset(
        dat[[i]],
        iron.supp.dur_Mat_EAR.p_cat == "Started in 1st trimester"
      ),
      family = binomial
    )
}
mod_iron_2_pooled <- pool(mod_iron_2)

x_iron_2 <-
  as.data.frame(IMP_extract_log.res_bin(mod_iron_2_pooled))
y_iron_2 <-
  IMP_extract_beta.SE_bin(mod_iron_2_pooled)

y_iron_2$Outcome <- "Maternal anaemia occurring in pregnancy"
y_iron_2$`Iron supplement use` <- "Started in 1st trimester"

for (i in 1:nrow(y_iron_2)) {
  y_iron_2$N_exp[i] <- paste0(
    sum(
      dat_iron_2$VegDiet_bin == 1 &
        dat_iron_2$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron_2$VegDiet_bin == 1, na.rm = T)
  )
  
  y_iron_2$N_ref[i] <- paste0(
    sum(
      dat_iron_2$VegDiet_bin == 0 &
        dat_iron_2$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron_2$VegDiet_bin == 0, na.rm = T)
  )
}

y_iron_2

################################################################################
################################################################################

### Iron supplement use - Started before pregnancy
mod_iron_3 <- list()
for (i in 1:dat_imp$m) {
  mod_iron_3[[i]] <-
    glm(
      anaemia_preg_subsamp ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.iron.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
      data = subset(
        dat[[i]],
        iron.supp.dur_Mat_EAR.p_cat == "Started before pregnancy"
      ),
      family = binomial
    )
}
mod_iron_3_pooled <- pool(mod_iron_3)

x_iron_3 <-
  as.data.frame(IMP_extract_log.res_bin(mod_iron_3_pooled))
y_iron_3 <-
  IMP_extract_beta.SE_bin(mod_iron_3_pooled)

y_iron_3$Outcome <- "Maternal anaemia occurring in pregnancy"
y_iron_3$`Iron supplement use` <- "Started before pregnancy"

for (i in 1:nrow(y_iron_3)) {
  y_iron_3$N_exp[i] <- paste0(
    sum(
      dat_iron_3$VegDiet_bin == 1 &
        dat_iron_3$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron_3$VegDiet_bin == 1, na.rm = T)
  )
  
  y_iron_3$N_ref[i] <- paste0(
    sum(
      dat_iron_3$VegDiet_bin == 0 &
        dat_iron_3$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_iron_3$VegDiet_bin == 0, na.rm = T)
  )
}

y_iron_3

################################################################################

## Combine, view, and save results
obs.res_VegDiet_bin <-
  rbind(y_iron_0, y_iron_1, y_iron_2, y_iron_3)

obs.res_VegDiet_bin

write.xlsx(
  obs.res_VegDiet_bin,
  "results/IMP_STRAT.iron.dur_obs.res_VegDiet_bin.xlsx",
  overwrite = T
)

################################################################################

x_iron_0  # Iron supplement use - No
x_iron_1  # Iron supplement use - Started after 1st trimester
x_iron_2  # Iron supplement use - Started in 1st trimester
x_iron_3  # Iron supplement use - Started before pregnancy

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
  file = "results/IMP_STRAT.iron.dur_obs.forest_VegDiet_bin.png",
  height = 3,
  width = 6
)
