################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa        #
################################################################################

# Last edited date: 08-Dec-2024
# This script is to perform negative control outcome analysis on breastfeeding for plant-based diet indices (PDIs) in MoBa.

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
dat <- subset(dat, is.na(PDI) == F)  # Remove missing PDIs
head(dat)
dim(dat)  # 73868 -> 73868

################################################################################
# Modelling
## Model 1: exposure only - i.e., the unadjusted model
## Model 2: + age, ethnicity, education, income, parity, pre-pregnancy BMI, smoking, alcohol drinking, offspring sex - i.e., additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
## Model 3: + dietary supplementation and total energy intake - i.e., additionally adjusting for nutrition-related factors
################################################################################

# Create a function to extract regression results with SE (for ORDINAL logistic regression) - !!! Specifically for continuous exposures !!!
extract_ord.beta.SE_con <- function(mymodel) {
  b <- summary(mymodel)$coef[4, 1]
  se <- summary(mymodel)$coef[4, 2]
  pval <- summary(mymodel)$coef[4, 4]
  
  x <- cbind(b, se, pval)
  
  return(x)
}

# Create a function to extract ORDINAL logistic regression results - !!! Specifically for continuous exposures !!!
extract_ord.log.res_con <- function(mymodel) {
  N <- length(mymodel$fitted.values)  # Extract sample size
  
  OR <-
    format(round(exp(summary(mymodel)$coef[4, 1]), digits = 2), nsmall = 2)  # Extract OR (keep 2 decimal places)
  
  CI <-
    paste0(format(round(
      exp(summary(mymodel)$coef[4, 1] - 1.96 * summary(mymodel)$coef[4, 2]), digits = 2
    ), nsmall = 2), ", ", format(round(
      exp(summary(mymodel)$coef[4, 1] + 1.96 * summary(mymodel)$coef[4, 2]), digits = 2
    ), nsmall = 2))  # Extract 95% CI (keep 2 decimal places);   # confint() too time-consuming for logistic models - calculating 95% CI manually instead
  
  pval <-
    style_pvalue(summary(mymodel)$coef[4, 4], digits = 3)  # Extract p-value (keep 3 decimal places)
  
  x <- cbind(N, OR, CI, pval)
  
  return(x)
}

################################################################################
################################################################################

# Vegetarian subgroups and breastfeeding duration (3 categories) - Ordinal logistic regression

## PDI

### Model 1 - Univariate (unadjusted) model
mod_1 <-
  clm(bf_dur_4c ~ PDI_z, data = dat)
x_1 <- extract_ord.log.res_con(mod_1)
y_1 <- extract_ord.beta.SE_con(mod_1)

### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_2 <-
  clm(
    bf_dur_4c ~ PDI_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
    data = dat
  )
x_2 <- extract_ord.log.res_con(mod_2)
y_2 <- extract_ord.beta.SE_con(mod_2)

### Model 3 - Additionally adjusting for nutrition-related factors (including hPDI as the indicator for the healthfulness of diet)
mod_3 <-
  clm(
    bf_dur_4c ~ PDI_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin,
    data = dat
  )
# Warning messages:
# 1: In x$code == 0L || action == "silent" :
#   'length(x) = 2 > 1' in coercion to 'logical(1)'
# 2: (2) Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?
# In addition: Absolute and relative convergence criteria were met
x_3 <- extract_ord.log.res_con(mod_3)
y_3 <- extract_ord.beta.SE_con(mod_3)

## hPDI

### Model 1 - Univariate (unadjusted) model
mod_4 <-
  clm(bf_dur_4c ~ hPDI_z, data = dat)
x_4 <- extract_ord.log.res_con(mod_4)
y_4 <- extract_ord.beta.SE_con(mod_4)

### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_5 <-
  clm(
    bf_dur_4c ~ hPDI_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
    data = dat
  )
x_5 <- extract_ord.log.res_con(mod_5)
y_5 <- extract_ord.beta.SE_con(mod_5)

### Model 3 - Additionally adjusting for nutrition-related factors (including hPDI as the indicator for the healthfulness of diet)
mod_6 <-
  clm(
    bf_dur_4c ~ hPDI_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin,
    data = dat
  )
# Warning messages:
# 1: In x$code == 0L || action == "silent" :
#   'length(x) = 2 > 1' in coercion to 'logical(1)'
# 2: (2) Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?
# In addition: Absolute and relative convergence criteria were met
x_6 <- extract_ord.log.res_con(mod_6)
y_6 <- extract_ord.beta.SE_con(mod_6)

## uPDI

### Model 1 - Univariate (unadjusted) model
mod_7 <-
  clm(bf_dur_4c ~ uPDI_z, data = dat)
x_7 <- extract_ord.log.res_con(mod_7)
y_7 <- extract_ord.beta.SE_con(mod_7)

### Model 2 - Additionally adjusting for sociodemographic, pregnancy-related, and lifestyle factors
mod_8 <-
  clm(
    bf_dur_4c ~ uPDI_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin,
    data = dat
  )
x_8 <- extract_ord.log.res_con(mod_8)
y_8 <- extract_ord.beta.SE_con(mod_8)

### Model 3 - Additionally adjusting for nutrition-related factors
mod_9 <-
  clm(
    bf_dur_4c ~ uPDI_z + age_Mat_con + ethnic_Mat_bin + edu_Mat_3cat  + income_Fam_3cat + parity_Mat_bin + BMI_Mat_PRE.p_con + smoking_Mat_EAR.p_bin + alcohol_Mat_EAR.p_bin + sex_Chi_bin + any.supp_Mat_EAR.p_bin,
    data = dat
  )
x_9 <- extract_ord.log.res_con(mod_9)
y_9 <- extract_ord.beta.SE_con(mod_9)

################################################################################
################################################################################

## Combine regression results
outcome <- var_lab(dat$bf_dur_4c)

x <-
  cbind(
    Outcome = as.data.frame(rep(outcome, 3)),
    Exposure = as.data.frame(c("PDI", "hPDI", "uPDI")),
    Model_1 = rbind(x_1, x_4, x_7),
    space = " ",
    Model_2 = rbind(x_2, x_5, x_8),
    space = " ",
    Model_3 = rbind(x_3, x_6, x_9)
  )  # PDI: 1 2 3; hPDI: 4 5 6; uPDI: 7 8 9
colnames(x)[1] <- "Outcome"
colnames(x)[2] <- "Exposure"

outcome_col <- as.data.frame(rep(outcome, 3))
colnames(outcome_col) <- "Outcome"
model_col <- as.data.frame(c("Model 1", "Model 2", "Model 3"))
colnames(model_col) <- "Model"
y_PDI <- cbind(outcome_col, model_col, rbind(y_1, y_2, y_3))
y_hPDI <- cbind(outcome_col, model_col, rbind(y_4, y_5, y_6))
y_uPDI <- cbind(outcome_col, model_col, rbind(y_7, y_8, y_9))

################################################################################
################################################################################

## View and save results
obs.tbl_PDI.hPDI.uPDI_ord <- as.data.frame(x)
obs.tbl_PDI.hPDI.uPDI_ord
dim(obs.tbl_PDI.hPDI.uPDI_ord)  # 1 outcome * 3 exposures = 3 obs
write.xlsx(
  obs.tbl_PDI.hPDI.uPDI_ord,
  "results/BF_obs.tbl_PDI.hPDI.uPDI_ord.xlsx",
  overwrite = T
)
obs.tbl_PDI.hPDI.uPDI_ord <-
  read.xlsx("results/BF_obs.tbl_PDI.hPDI.uPDI_ord.xlsx")

obs.res_PDI_ord <- as.data.frame(y_PDI)
obs.res_PDI_ord$N <- NA
for (i in 1:nrow(obs.res_PDI_ord)) {
  if (obs.res_PDI_ord$Model[i] == "Model 1") {
    obs.res_PDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_PDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "PDI", 3]
  } else if (obs.res_PDI_ord$Model[i] == "Model 2") {
    obs.res_PDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_PDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "PDI", 8]
  } else if (obs.res_PDI_ord$Model[i] == "Model 3") {
    obs.res_PDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_PDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "PDI", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_PDI_ord
dim(obs.res_PDI_ord)  # 1 outcome * 3 models = 3 obs
write.xlsx(obs.res_PDI_ord,
           "results/BF_obs.res_PDI_ord.xlsx",
           overwrite = T)
obs.res_PDI_ord <-
  read.xlsx("results/BF_obs.res_PDI_ord.xlsx")

obs.res_hPDI_ord <- as.data.frame(y_hPDI)
obs.res_hPDI_ord$N <- NA
for (i in 1:nrow(obs.res_hPDI_ord)) {
  if (obs.res_hPDI_ord$Model[i] == "Model 1") {
    obs.res_hPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_hPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "hPDI", 3]
  } else if (obs.res_hPDI_ord$Model[i] == "Model 2") {
    obs.res_hPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_hPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "hPDI", 8]
  } else if (obs.res_hPDI_ord$Model[i] == "Model 3") {
    obs.res_hPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_hPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "hPDI", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_hPDI_ord
dim(obs.res_hPDI_ord)  # 1 outcome * 3 models = 3 obs
write.xlsx(obs.res_hPDI_ord,
           "results/BF_obs.res_hPDI_ord.xlsx",
           overwrite = T)
obs.res_hPDI_ord <-
  read.xlsx("results/BF_obs.res_hPDI_ord.xlsx")

obs.res_uPDI_ord <- as.data.frame(y_uPDI)
obs.res_uPDI_ord$N <- NA
for (i in 1:nrow(obs.res_uPDI_ord)) {
  if (obs.res_uPDI_ord$Model[i] == "Model 1") {
    obs.res_uPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_uPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "uPDI", 3]
  } else if (obs.res_uPDI_ord$Model[i] == "Model 2") {
    obs.res_uPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_uPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "uPDI", 8]
  } else if (obs.res_uPDI_ord$Model[i] == "Model 3") {
    obs.res_uPDI_ord$N[i] <-
      obs.tbl_PDI.hPDI.uPDI_ord[obs.tbl_PDI.hPDI.uPDI_ord$Outcome == obs.res_uPDI_ord$Outcome[i] &
                                  obs.tbl_PDI.hPDI.uPDI_ord$Exposure == "uPDI", 13]
  }
}  # Add N for the exposed and reference groups
obs.res_uPDI_ord
dim(obs.res_uPDI_ord)  # 1 outcome * 3 models = 3 obs
write.xlsx(obs.res_uPDI_ord,
           "results/BF_obs.res_uPDI_ord.xlsx",
           overwrite = T)
obs.res_uPDI_ord <-
  read.xlsx("results/BF_obs.res_uPDI_ord.xlsx")

################################################################################

# Show the effect of SES on PDIs and breastfeeding, respectively

## Effect of maternal education (ordinal)...

### ...on PDI (continuous)
mod <-
  lm(PDI_z ~ edu_Mat_3cat + age_Mat_con + ethnic_Mat_bin, data = dat)

edu_PDI <-
  data.frame(
    Exposure = "Maternal education",
    Outcome = "PDI",
    b = as.numeric(summary(mod)$coef[2, 1]),
    se = as.numeric(summary(mod)$coef[2, 2]),
    pval = as.numeric(summary(mod)$coef[2, 4])
  )

edu_PDI

### ...on hPDI (continuous)
mod <-
  lm(hPDI_z ~ edu_Mat_3cat + age_Mat_con + ethnic_Mat_bin, data = dat)

edu_hPDI <-
  data.frame(
    Exposure = "Maternal education",
    Outcome = "hPDI",
    b = as.numeric(summary(mod)$coef[2, 1]),
    se = as.numeric(summary(mod)$coef[2, 2]),
    pval = as.numeric(summary(mod)$coef[2, 4])
  )

edu_hPDI

### ...on uPDI (continuous)
mod <-
  lm(uPDI_z ~ edu_Mat_3cat + age_Mat_con + ethnic_Mat_bin, data = dat)

edu_uPDI <-
  data.frame(
    Exposure = "Maternal education",
    Outcome = "uPDI",
    b = as.numeric(summary(mod)$coef[2, 1]),
    se = as.numeric(summary(mod)$coef[2, 2]),
    pval = as.numeric(summary(mod)$coef[2, 4])
  )

edu_uPDI

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
################################################################################

## Effect of household income (ordinal)...

### ...on PDI (continuous)
mod <-
  lm(PDI_z ~ income_Fam_3cat + age_Mat_con + ethnic_Mat_bin, data = dat)

income_PDI <-
  data.frame(
    Exposure = "Household income",
    Outcome = "PDI",
    b = as.numeric(summary(mod)$coef[2, 1]),
    se = as.numeric(summary(mod)$coef[2, 2]),
    pval = as.numeric(summary(mod)$coef[2, 4])
  )

income_PDI

### ...on hPDI (continuous)
mod <-
  lm(hPDI_z ~ income_Fam_3cat + age_Mat_con + ethnic_Mat_bin, data = dat)

income_hPDI <-
  data.frame(
    Exposure = "Household income",
    Outcome = "hPDI",
    b = as.numeric(summary(mod)$coef[2, 1]),
    se = as.numeric(summary(mod)$coef[2, 2]),
    pval = as.numeric(summary(mod)$coef[2, 4])
  )

income_hPDI

### ...on uPDI (continuous)
mod <-
  lm(uPDI_z ~ income_Fam_3cat + age_Mat_con + ethnic_Mat_bin, data = dat)

income_uPDI <-
  data.frame(
    Exposure = "Household income",
    Outcome = "uPDI",
    b = as.numeric(summary(mod)$coef[2, 1]),
    se = as.numeric(summary(mod)$coef[2, 2]),
    pval = as.numeric(summary(mod)$coef[2, 4])
  )

income_uPDI

### ...on breastfeeding duration (ordinal)
mod <-
  clm(
    bf_dur_4c ~ income_Fam_3cat + age_Mat_con + ethnic_Mat_bin,
    data = dat,
    family = binomial
  )

income_bf.dur <-
  data.frame(
    Exposure = "Household income",
    Outcome = "Breastfeeding duration",
    b = as.numeric(summary(mod)$coef[4, 1]),
    se = as.numeric(summary(mod)$coef[4, 2]),
    pval = as.numeric(summary(mod)$coef[4, 4])
  )

income_bf.dur

################################################################################
################################################################################

## Combine, view, and save results
obs.res_PDI.hPDI.uPDI_SEP <-
  rbind(edu_PDI,
        edu_hPDI,
        edu_uPDI,
        edu_bf.dur,
        income_PDI,
        income_hPDI,
        income_uPDI,
        income_bf.dur)

obs.res_PDI.hPDI.uPDI_SEP
dim(obs.res_PDI.hPDI.uPDI_SEP)  # 2 exposures * 4 outcomes = 8 obs

write.xlsx(
  obs.res_PDI.hPDI.uPDI_SEP,
  "results/BF_obs.res_PDI.hPDI.uPDI_SEP.xlsx",
  overwrite = T
)
obs.res_PDI.hPDI.uPDI_SEP <-
  read.xlsx("results/BF_obs.res_PDI.hPDI.uPDI_SEP.xlsx")

################################################################################
################################################################################

## Show all results in a forestplot

### Prepare data
obs.res_PDI.hPDI.uPDI_SEP <- obs.res_PDI.hPDI.uPDI_SEP %>%
  mutate(`Exposure  ->  Outcome` = paste0(Exposure, "  ->  ", Outcome))

obs.res_PDI.hPDI.uPDI_SEP$`Exposure  ->  Outcome` <-
  factor(obs.res_PDI.hPDI.uPDI_SEP$`Exposure  ->  Outcome`,
         levels = rev(unique(
           obs.res_PDI.hPDI.uPDI_SEP$`Exposure  ->  Outcome`
         )))  # Make sure each result appears in the plot in the right order

obs.res_PDI.hPDI.uPDI_SEP$Outcome <-
  factor(
    obs.res_PDI.hPDI.uPDI_SEP$Outcome,
    levels = c("Breastfeeding duration", "uPDI", "hPDI", "PDI")
  )  # Make sure each result appears in the plot in the right colour

### Nightingale forest plots
obs.forest_PDI.hPDI.uPDI_SEP <- ggforestplot::forestplot(
  df = obs.res_PDI.hPDI.uPDI_SEP,
  name = `Exposure  ->  Outcome`,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Outcome,
  shape = Outcome,
  xlab = "Beta (or log odds) and 95% CI",
  title = "Effect of socioeconomic position",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("palevioletred1", "darkorange", "darkgreen", "yellowgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21))

obs.forest_PDI.hPDI.uPDI_SEP

ggsave(
  "results/BF_obs.forest_PDI.hPDI.uPDI_SEP.png",
  plot = obs.forest_PDI.hPDI.uPDI_SEP,
  height = 3,
  width = 10
)

################################################################################
