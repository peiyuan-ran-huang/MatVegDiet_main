################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - BiB        #
################################################################################

# Last edited date: 20-Jan-2025
# This script is to examine the association/effect of key covariates with/on vegetarianism (with imputed data) in BiB.

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
load("data/BiB/dat_exp_cov_out_IMP.RData")

## Create a list of imputed datasets
dat <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat[[i]] <- complete_dat
  
  var_lab(dat[[i]]$VegDiet_bin) <- "Pesco-/full vs. non-vegetarian"
  
  dat[[i]]$pesco.V_bin <- NA
  dat[[i]]$pesco.V_bin[dat[[i]]$VegDiet_3cat == "Non-vegetarian"] <- 0
  dat[[i]]$pesco.V_bin[dat[[i]]$VegDiet_3cat == "Pesco-vegetarian"] <- 1
  var_lab(dat[[i]]$pesco.V_bin) <- "Pesco- vs. non-vegetarian"
  
  dat[[i]]$full.V_bin <- NA
  dat[[i]]$full.V_bin[dat[[i]]$VegDiet_3cat == "Non-vegetarian"] <- 0
  dat[[i]]$full.V_bin[dat[[i]]$VegDiet_3cat == "Full vegetarian"] <- 1
  var_lab(dat[[i]]$full.V_bin) <- "Full vs. non-vegetarian"
}

## Check the first imputed dataset
dat_1 <- dat[[1]]
head(dat_1)
dim(dat_1)  # 3647  XXX
for (varname in colnames(dat_1)) {
  print(varname)
  print(sum(is.na(dat_1[[varname]])))
}

table(dat_1$pesco.V_bin, useNA = "ifany")
table(dat_1$full.V_bin, useNA = "ifany")

################################################################################

# Create a function to extract beta, SE, and p-value (for both linear and logistic regression)
IMP_extract_beta.SE <- function(mymodel) {
  b <- summary(mymodel)[2, 2]
  se <- summary(mymodel)[2, 3]
  pval <- summary(mymodel)[2, 6]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

# Create empty output tables (for forestplots and meta-analysis)
obs.res_VegDiet_bin <- c()

################################################################################

## Loop for each covariate
for (myoutcome in c(
  "age_Mat_con",
  "ethnic_Mat_bin",
  "edu_Mat_3cat",
  "IMD_Fam_cat",
  "parity_Mat_bin",
  "BMI_Mat_PRE.p_con",
  "smoking_Mat_EAR.p_bin",
  "alcohol_Mat_EAR.p_bin",
  "any.supp_Mat_EAR.p_bin",
  # "energy_Mat_DUR.p_con",
  # "PDI",
  # "hPDI",
  # "uPDI",
  "sex_Chi_bin"
)) {
  covariate <- myoutcome
  
  ### 2 models - 1 for pesco-vegetarianism, 1 for full vegetarianism
  #### Model 1 - Key covariates and pesco-vegetarianism (!!! UNADJUSTED !!!)
  mod_1 <- list()
  for (i in 1:dat_imp$m) {
    mod_1[[i]] <-
      glm(dat[[i]][["pesco.V_bin"]] ~ dat[[i]][[myoutcome]], family = binomial)
  }
  mod_1_pooled <- pool(mod_1)
  y_1 <- IMP_extract_beta.SE(mod_1_pooled)
  
  #### Model 2 - Key covariates and full vegetarianism (!!! UNADJUSTED !!!)
  mod_2 <- list()
  for (i in 1:dat_imp$m) {
    mod_2[[i]] <-
      glm(dat[[i]][["full.V_bin"]] ~ dat[[i]][[myoutcome]], family = binomial)
  }
  mod_2_pooled <- pool(mod_2)
  y_2 <- IMP_extract_beta.SE(mod_2_pooled)
  
  ### Regression results for one covariate
  cov_col <- as.data.frame(rep(covariate, 2))
  colnames(cov_col) <- "Covariate"
  VegDiet_col <-
    as.data.frame(c(
      "Pesco-vegetarian\nvs. non-vegetarian",
      "Full vegetarian\nvs. non-vegetarian"
    ))
  colnames(VegDiet_col) <- "Vegetarianism"
  y <- cbind(cov_col, VegDiet_col, rbind(y_1, y_2))
  
  ### Rows continuously added to the output table
  obs.res_VegDiet_bin <- rbind(obs.res_VegDiet_bin, y)
}

################################################################################

## View and save results
obs.res_VegDiet_bin <- as.data.frame(obs.res_VegDiet_bin)

## Convert beta into OR
obs.res_VegDiet_bin$OR <- exp(obs.res_VegDiet_bin$b)

## Rename covariates
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "age_Mat_con"] <- "Age\n(in years)"
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "ethnic_Mat_bin"] <- "Ethnicity\n(Other vs. White)"
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "edu_Mat_3cat"] <- "Education\n(higher vs. lower)"
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "IMD_Fam_cat"] <- "IMD\n(more vs. less affluent)"
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "parity_Mat_bin"] <- "Parity\n(≥1 vs. 0)"
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "BMI_Mat_PRE.p_con"] <- "Pre-pregnancy BMI\n(in kg/m^2)"
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "smoking_Mat_EAR.p_bin"] <- "Smoking\n(yes vs. no)"
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "alcohol_Mat_EAR.p_bin"] <- "Alcohol drinking\n(yes vs. no)"
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "any.supp_Mat_EAR.p_bin"] <- "Any supplement use\n(yes vs. no)"
# obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "energy_Mat_DUR.p_con"] <- "Total energy intake\n(in kcal/day)"
# obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "PDI"] <- "Overall PDI"
# obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "hPDI"] <- "Healthful PDI"
# obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "uPDI"] <- "Unhealthful PDI"
obs.res_VegDiet_bin$Covariate[obs.res_VegDiet_bin$Covariate == "sex_Chi_bin"] <- "Offspring sex\n(female vs. male)"

obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # (10 covariates) * 2 vegetarian subgroups = 20 obs.

## Save results
write.xlsx(obs.res_VegDiet_bin,
           "results/BiB/IMP_cov_obs.res_VegDiet_bin.xlsx",
           overwrite = T)
