################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 08-Dec-2024
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
# 3) Maternal anaemia during pregnancy (highly relevant to iron status) - Stratified by FREQUENCY of iron supplement use

# 4) Maternal anaemia during pregnancy (highly relevant to vitamin B12 status) - Stratified by vitamin B12 supplement use
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

# Maternal anaemia - Stratified by vitamin B12 supplementation (measured before/up to dietary assessment)

## Subset samples
dat_N_vitB12 <-
  subset(dat_1, vitB12.supp_Mat_EAR.p_bin == 0)
dat_vitB12 <-
  subset(dat_1, vitB12.supp_Mat_EAR.p_bin == 1)

################################################################################
################################################################################

## Analysis in each sub-sample

### Vitamin B12 supplement use - No
mod_N_vitB12 <- list()
for (i in 1:dat_imp$m) {
  mod_N_vitB12[[i]] <-
    glm(
      anaemia_preg_all ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.vitB12.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
      data = subset(dat[[i]], vitB12.supp_Mat_EAR.p_bin == 0),
      family = binomial
    )
}
mod_N_vitB12_pooled <- pool(mod_N_vitB12)

x_N_vitB12 <-
  as.data.frame(IMP_extract_log.res_bin(mod_N_vitB12_pooled))
y_N_vitB12 <-
  IMP_extract_beta.SE_bin(mod_N_vitB12_pooled)

y_N_vitB12$Outcome <- "Maternal anaemia during pregnancy"
y_N_vitB12$`Supplementation` <- "Vitamin B12 supplement use - No"

for (i in 1:nrow(y_N_vitB12)) {
  y_N_vitB12$N_exp[i] <- paste0(
    sum(
      dat_N_vitB12$VegDiet_bin == 1 &
        dat_N_vitB12$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_N_vitB12$VegDiet_bin == 1, na.rm = T)
  )
  
  y_N_vitB12$N_ref[i] <- paste0(
    sum(
      dat_N_vitB12$VegDiet_bin == 0 &
        dat_N_vitB12$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_N_vitB12$VegDiet_bin == 0, na.rm = T)
  )
}

y_N_vitB12

################################################################################
################################################################################

### Vitamin B12 supplement use - Yes
mod_vitB12 <- list()
for (i in 1:dat_imp$m) {
  mod_vitB12[[i]] <-
    glm(
      anaemia_preg_all ~ VegDiet_bin + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + NON.vitB12.supp_Mat_EAR.p_bin + energy_Mat_DUR.p_con + hPDIm,
      data = subset(dat[[i]], vitB12.supp_Mat_EAR.p_bin == 1),
      family = binomial
    )
}
mod_vitB12_pooled <- pool(mod_vitB12)

x_vitB12 <-
  as.data.frame(IMP_extract_log.res_bin(mod_vitB12_pooled))
y_vitB12 <-
  IMP_extract_beta.SE_bin(mod_vitB12_pooled)

y_vitB12$Outcome <- "Maternal anaemia during pregnancy"
y_vitB12$`Supplementation` <- "Vitamin B12 supplement use - Yes"

for (i in 1:nrow(y_vitB12)) {
  y_vitB12$N_exp[i] <- paste0(
    sum(
      dat_vitB12$VegDiet_bin == 1 &
        dat_vitB12$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_vitB12$VegDiet_bin == 1, na.rm = T)
  )
  
  y_vitB12$N_ref[i] <- paste0(
    sum(
      dat_vitB12$VegDiet_bin == 0 &
        dat_vitB12$anaemia_preg_all == 1,
      na.rm = T
    ),
    " / ",
    sum(dat_vitB12$VegDiet_bin == 0, na.rm = T)
  )
}

y_vitB12

################################################################################
################################################################################

## Combine, view, and save results
obs.res_VegDiet_bin <-
  rbind(y_N_vitB12, y_vitB12)

obs.res_VegDiet_bin

write.xlsx(
  obs.res_VegDiet_bin,
  "results/IMP_STRAT.vitB12_obs.res_VegDiet_bin.xlsx",
  overwrite = T
)
obs.res_VegDiet_bin <-
  read.xlsx("results/IMP_STRAT.vitB12_obs.res_VegDiet_bin.xlsx")

################################################################################

x_N_vitB12  # Vitamin B12 supplementation - No

x_vitB12  # Vitamin B12 supplementation - Yes

################################################################################
### !!! Add total sample size for each stratum !!!
obs.res_VegDiet_bin <- obs.res_VegDiet_bin %>%
  separate(
    N_exp,
    into = c("case_exp", "total_exp"),
    sep = " / ",
    remove = F
  ) %>%
  separate(
    N_ref,
    into = c("case_ref", "total_ref"),
    sep = " / ",
    remove = F
  ) %>%
  mutate(
    case_exp = as.numeric(case_exp),
    total_exp = as.numeric(total_exp),
    case_ref = as.numeric(case_ref),
    total_ref = as.numeric(total_ref)
  ) %>% mutate(total = total_exp + total_ref)

obs.res_VegDiet_bin$Supplementation[obs.res_VegDiet_bin$Supplementation == "Vitamin B12 supplement use - No"] <-
  "Vitamin B12 supplement use: No"
obs.res_VegDiet_bin$Supplementation[obs.res_VegDiet_bin$Supplementation == "Vitamin B12 supplement use - Yes"] <-
  "Vitamin B12 supplement use: Yes"
obs.res_VegDiet_bin$Supplementation <- paste0(
  obs.res_VegDiet_bin$Supplementation,
  "\n(",
  obs.res_VegDiet_bin$case_exp,
  " in ",
  obs.res_VegDiet_bin$total_exp,
  " vs. ",
  obs.res_VegDiet_bin$case_ref,
  " in ",
  obs.res_VegDiet_bin$total_ref,
  ")"
)

obs.res_VegDiet_bin
################################################################################
### !!! Remove some results with extremely huge 95% CIs !!!
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin[which(obs.res_VegDiet_bin$se < 100), ]

obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 2 -> 2 obs.
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
  xlab = "OR and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "Risk of maternal anaemia in MoBa",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21)) + theme(legend.position = "none")

obs.forest_VegDiet_bin

ggsave(
  obs.forest_VegDiet_bin,
  file = "results/IMP_STRAT.vitB12_obs.forest_VegDiet_bin.png",
  height = 2,
  width = 6.5
)
