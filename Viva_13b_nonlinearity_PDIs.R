################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 01-Aug-2025
# This script is to perform non-linearity analysis for plant-based diet indices (PDIs) in Project Viva.

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
  plotRCS,
  gridExtra
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/Viva/dat_exp_cov_out.rds")
head(dat)
dim(dat)  # 1872  606

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
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat))]
Viva_primary_bin  # 9 primary (binary) outcomes available in Project Viva

Viva_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat))]
Viva_secondary_bin  # 8 secondary binary outcomes available in Project Viva

Viva_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat))]
Viva_secondary_con  # 2 secondary continuous outcomes available in Project Viva

Viva_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat))]
Viva_secondary_cat  # 1 (primary) ordinal/categorical outcome available in Project Viva

## Group outcome variables
Viva_out_bin <- c(Viva_primary_bin, Viva_secondary_bin)
Viva_out_con <- Viva_secondary_con
Viva_out_cat <- Viva_secondary_cat

#------------------------------------------------------------------------------#
#                        Restricted Cubic Spline Plots                         #----
#------------------------------------------------------------------------------#

# PDI

## Create empty plot list
RCS.plot_PDI <- list()

## Plots for binary outcomes (and ordinal breastfeeding duration)
for (myoutcome in c(Viva_out_bin, "bf_dur_4c")) {
  dat_tmp <-
    dat[complete.cases(subset(
      dat,
      select = c(
        myoutcome,
        "PDI_z",
        "age_Mat_con",
        "ethnic_Mat_bin",
        "edu_Mat_3cat",
        "income_Fam_3cat",
        "parity_Mat_bin",
        "BMI_Mat_PRE.p_con",
        "smoking_Mat_EAR.p_bin",
        "alcohol_Mat_EAR.p_bin",
        "sex_Chi_bin",
        "energy_Mat_DUR.p_con"
      )
    )), c(
      myoutcome,
      "PDI_z",
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    )]
  
  dat_tmp[[myoutcome]] <- as.numeric(dat_tmp[[myoutcome]])
  dat_tmp[["edu_Mat_3cat"]] <- as.numeric(dat_tmp[["edu_Mat_3cat"]])
  dat_tmp[["income_Fam_3cat"]] <- as.numeric(dat_tmp[["income_Fam_3cat"]])
  
  p <- rcsplot(
    data = dat_tmp,
    outcome = myoutcome,
    exposure = "PDI_z",
    covariates = c(
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    ),
    knots = knot(5),
    ref.value = "k3",
    pvalue.label.overall = "P overall",
    pvalue.label.nonlinear = "P for non-linearity",
    xlab = "Standarised PDI",
    ylab = "OR (95% CI)",
  )
  
  p <- p + labs(title = var_lab(dat_tmp[[myoutcome]]))
  
  RCS.plot_PDI <- append(RCS.plot_PDI, list(p))
}

## Plots for continuous outcomes
for (myoutcome in Viva_out_con) {
  dat_tmp <-
    dat[complete.cases(subset(
      dat,
      select = c(
        myoutcome,
        "PDI_z",
        "age_Mat_con",
        "ethnic_Mat_bin",
        "edu_Mat_3cat",
        "income_Fam_3cat",
        "parity_Mat_bin",
        "BMI_Mat_PRE.p_con",
        "smoking_Mat_EAR.p_bin",
        "alcohol_Mat_EAR.p_bin",
        "sex_Chi_bin",
        "energy_Mat_DUR.p_con"
      )
    )), c(
      myoutcome,
      "PDI_z",
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    )]
  
  dat_tmp[[myoutcome]] <- as.numeric(dat_tmp[[myoutcome]])
  dat_tmp[["edu_Mat_3cat"]] <- as.numeric(dat_tmp[["edu_Mat_3cat"]])
  dat_tmp[["income_Fam_3cat"]] <- as.numeric(dat_tmp[["income_Fam_3cat"]])
  
  p <- rcsplot(
    data = dat_tmp,
    outcome = myoutcome,
    exposure = "PDI_z",
    covariates = c(
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    ),
    knots = knot(5),
    ref.value = "k3",
    pvalue.label.overall = "P overall",
    pvalue.label.nonlinear = "P for non-linearity",
    xlab = "Standarised PDI"
  )
  
  p <- p + labs(title = var_lab(dat_tmp[[myoutcome]]))
  
  RCS.plot_PDI <- append(RCS.plot_PDI, list(p))
}

RCS.plots_PDI <- do.call(grid.arrange, c(RCS.plot_PDI, ncol = 4))

ggsave(
  RCS.plots_PDI,
  filename = "results/Viva/RCS.plots_PDI.png",
  width = 16,
  height = 15,
  limitsize = F
)

################################################################################

# hPDI

## Create empty plot list
RCS.plot_hPDI <- list()

## Plots for binary outcomes (and ordinal breastfeeding duration)
for (myoutcome in c(Viva_out_bin, "bf_dur_4c")) {
  dat_tmp <-
    dat[complete.cases(subset(
      dat,
      select = c(
        myoutcome,
        "hPDI_z",
        "age_Mat_con",
        "ethnic_Mat_bin",
        "edu_Mat_3cat",
        "income_Fam_3cat",
        "parity_Mat_bin",
        "BMI_Mat_PRE.p_con",
        "smoking_Mat_EAR.p_bin",
        "alcohol_Mat_EAR.p_bin",
        "sex_Chi_bin",
        "energy_Mat_DUR.p_con"
      )
    )), c(
      myoutcome,
      "hPDI_z",
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    )]
  
  dat_tmp[[myoutcome]] <- as.numeric(dat_tmp[[myoutcome]])
  dat_tmp[["edu_Mat_3cat"]] <- as.numeric(dat_tmp[["edu_Mat_3cat"]])
  dat_tmp[["income_Fam_3cat"]] <- as.numeric(dat_tmp[["income_Fam_3cat"]])
  
  p <- rcsplot(
    data = dat_tmp,
    outcome = myoutcome,
    exposure = "hPDI_z",
    covariates = c(
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    ),
    knots = knot(5),
    ref.value = "k3",
    pvalue.label.overall = "P overall",
    pvalue.label.nonlinear = "P for non-linearity",
    xlab = "Standarised hPDI",
    ylab = "OR (95% CI)",
  )
  
  p <- p + labs(title = var_lab(dat_tmp[[myoutcome]]))
  
  RCS.plot_hPDI <- append(RCS.plot_hPDI, list(p))
}

## Plots for continuous outcomes
for (myoutcome in Viva_out_con) {
  dat_tmp <-
    dat[complete.cases(subset(
      dat,
      select = c(
        myoutcome,
        "hPDI_z",
        "age_Mat_con",
        "ethnic_Mat_bin",
        "edu_Mat_3cat",
        "income_Fam_3cat",
        "parity_Mat_bin",
        "BMI_Mat_PRE.p_con",
        "smoking_Mat_EAR.p_bin",
        "alcohol_Mat_EAR.p_bin",
        "sex_Chi_bin",
        "energy_Mat_DUR.p_con"
      )
    )), c(
      myoutcome,
      "hPDI_z",
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    )]
  
  dat_tmp[[myoutcome]] <- as.numeric(dat_tmp[[myoutcome]])
  dat_tmp[["edu_Mat_3cat"]] <- as.numeric(dat_tmp[["edu_Mat_3cat"]])
  dat_tmp[["income_Fam_3cat"]] <- as.numeric(dat_tmp[["income_Fam_3cat"]])
  
  p <- rcsplot(
    data = dat_tmp,
    outcome = myoutcome,
    exposure = "hPDI_z",
    covariates = c(
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    ),
    knots = knot(5),
    ref.value = "k3",
    pvalue.label.overall = "P overall",
    pvalue.label.nonlinear = "P for non-linearity",
    xlab = "Standarised hPDI"
  )
  
  p <- p + labs(title = var_lab(dat_tmp[[myoutcome]]))
  
  RCS.plot_hPDI <- append(RCS.plot_hPDI, list(p))
}

RCS.plots_hPDI <- do.call(grid.arrange, c(RCS.plot_hPDI, ncol = 4))

ggsave(
  RCS.plots_hPDI,
  filename = "results/Viva/RCS.plots_hPDI.png",
  width = 16,
  height = 15,
  limitsize = F
)

################################################################################

# uPDI

## Create empty plot list
RCS.plot_uPDI <- list()

## Plots for binary outcomes (and ordinal breastfeeding duration)
for (myoutcome in c(Viva_out_bin, "bf_dur_4c")) {
  dat_tmp <-
    dat[complete.cases(subset(
      dat,
      select = c(
        myoutcome,
        "uPDI_z",
        "age_Mat_con",
        "ethnic_Mat_bin",
        "edu_Mat_3cat",
        "income_Fam_3cat",
        "parity_Mat_bin",
        "BMI_Mat_PRE.p_con",
        "smoking_Mat_EAR.p_bin",
        "alcohol_Mat_EAR.p_bin",
        "sex_Chi_bin",
        "energy_Mat_DUR.p_con"
      )
    )), c(
      myoutcome,
      "uPDI_z",
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    )]
  
  dat_tmp[[myoutcome]] <- as.numeric(dat_tmp[[myoutcome]])
  dat_tmp[["edu_Mat_3cat"]] <- as.numeric(dat_tmp[["edu_Mat_3cat"]])
  dat_tmp[["income_Fam_3cat"]] <- as.numeric(dat_tmp[["income_Fam_3cat"]])
  
  p <- rcsplot(
    data = dat_tmp,
    outcome = myoutcome,
    exposure = "uPDI_z",
    covariates = c(
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    ),
    knots = knot(5),
    ref.value = "k3",
    pvalue.label.overall = "P overall",
    pvalue.label.nonlinear = "P for non-linearity",
    xlab = "Standarised uPDI",
    ylab = "OR (95% CI)",
  )
  
  p <- p + labs(title = var_lab(dat_tmp[[myoutcome]]))
  
  RCS.plot_uPDI <- append(RCS.plot_uPDI, list(p))
}

## Plots for continuous outcomes
for (myoutcome in Viva_out_con) {
  dat_tmp <-
    dat[complete.cases(subset(
      dat,
      select = c(
        myoutcome,
        "uPDI_z",
        "age_Mat_con",
        "ethnic_Mat_bin",
        "edu_Mat_3cat",
        "income_Fam_3cat",
        "parity_Mat_bin",
        "BMI_Mat_PRE.p_con",
        "smoking_Mat_EAR.p_bin",
        "alcohol_Mat_EAR.p_bin",
        "sex_Chi_bin",
        "energy_Mat_DUR.p_con"
      )
    )), c(
      myoutcome,
      "uPDI_z",
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    )]
  
  dat_tmp[[myoutcome]] <- as.numeric(dat_tmp[[myoutcome]])
  dat_tmp[["edu_Mat_3cat"]] <- as.numeric(dat_tmp[["edu_Mat_3cat"]])
  dat_tmp[["income_Fam_3cat"]] <- as.numeric(dat_tmp[["income_Fam_3cat"]])
  
  p <- rcsplot(
    data = dat_tmp,
    outcome = myoutcome,
    exposure = "uPDI_z",
    covariates = c(
      "age_Mat_con",
      "ethnic_Mat_bin",
      "edu_Mat_3cat",
      "income_Fam_3cat",
      "parity_Mat_bin",
      "BMI_Mat_PRE.p_con",
      "smoking_Mat_EAR.p_bin",
      "alcohol_Mat_EAR.p_bin",
      "sex_Chi_bin",
      "energy_Mat_DUR.p_con"
    ),
    knots = knot(5),
    ref.value = "k3",
    pvalue.label.overall = "P overall",
    pvalue.label.nonlinear = "P for non-linearity",
    xlab = "Standarised uPDI"
  )
  
  p <- p + labs(title = var_lab(dat_tmp[[myoutcome]]))
  
  RCS.plot_uPDI <- append(RCS.plot_uPDI, list(p))
}

RCS.plots_uPDI <- do.call(grid.arrange, c(RCS.plot_uPDI, ncol = 4))

ggsave(
  RCS.plots_uPDI,
  filename = "results/Viva/RCS.plots_uPDI.png",
  width = 16,
  height = 15,
  limitsize = F
)

################################################################################
