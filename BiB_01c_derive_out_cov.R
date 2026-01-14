################################################################################
#              Maternal Vegetarian Diets & Perinatal Health - BiB              #
################################################################################

# Last edited date: 01-May-2025
# This script is to derive/rename outcome variables, covariates, and other relevant variables of interest in BiB.

################################################################################

#------------------------------------------------------------------------------#
#                                 Housekeeping                                 #----
#------------------------------------------------------------------------------#

# Clear environment
rm(list = ls())

# Collect information about the current R session
sessionInfo()

# Load packages
pacman::p_load(
  tidyverse,
  haven,
  descr,
  expss,
  gtsummary,
  kableExtra,
  flextable,
  readr,
  magrittr,
  openxlsx,
  corrplot,
  ggcorrplot,
  tableone,
  labelled
)

# Set working directory
setwd("Z:/working/data/BiB")
source("paths.R")
Sys.setenv(datadir = BiB_latest_dir)
Sys.setenv(datadir_FFQ = BiB_new_dir)

#------------------------------------------------------------------------------#
#                             Load & Combine Data                              #----
#------------------------------------------------------------------------------#

# Load data

## MR-PREG outcomes
dat_MRPREG_out <- readRDS("dat_MRPREG_out.rds")

head(dat_MRPREG_out)
dim(dat_MRPREG_out)  # 11928    XX

length(dat_MRPREG_out$BiBMotherID) == length(unique(dat_MRPREG_out$BiBMotherID))  # TRUE: All mothers are unique
length(dat_MRPREG_out$BiBMotherID)  # 11928
length(unique(dat_MRPREG_out$BiBMotherID))  # 11928

## MR-PREG covariates
dat_MRPREG_cov <- readRDS("dat_MRPREG_cov.rds")

head(dat_MRPREG_cov)
dim(dat_MRPREG_cov)  # 12103    XX

length(dat_MRPREG_cov$BiBMotherID) == length(unique(dat_MRPREG_cov$BiBMotherID))  # TRUE: All mothers are unique
length(dat_MRPREG_cov$BiBMotherID)  # 12103
length(unique(dat_MRPREG_cov$BiBMotherID))  # 12103

# Combine data
mean(dat_MRPREG_out$BiBMotherID %in% dat_MRPREG_cov$BiBMotherID)  # 1
mean(dat_MRPREG_out$BiBPersonID %in% dat_MRPREG_cov$BiBPersonID)  # 0.9983233
mean(dat_MRPREG_out$ChildID %in% dat_MRPREG_cov$ChildID)  # 0.9983233

dat <-
  left_join(
    dat_MRPREG_out,
    dat_MRPREG_cov,
    by = c("BiBPersonID", "BiBMotherID", "BiBPregNumber", "ChildID")
  )

head(dat)
dim(dat)  # 11928    XX

length(dat$BiBMotherID) == length(unique(dat$BiBMotherID))  # TRUE
length(dat$BiBPersonID) == length(unique(dat$BiBPersonID))  # TRUE
length(dat$ChildID) == length(unique(dat$ChildID))  # TRUE

# Check inclusion criteria

## Live births
dat <- dat[which(dat$eclbrthocm == 1), ]  # Exclude non-live births: 11928 -> 11865
mean(dat$eclbrthocm == 1, na.rm = T) == 1  # TRUE

## Singletons
mean(dat$adminpnbirths == 1 |
       dat$eclnregbrt == 1) == 1  # TRUE

## From unique mothers
sum(is.na(dat$BiBMotherID))  # 0 with no maternal ID
sum(duplicated(dat$BiBMotherID))  # 0 with duplicated maternal ID

#------------------------------------------------------------------------------#
#                               MR-PREG Outcomes                               #----
#------------------------------------------------------------------------------#

# Rename outcome variables
dat$depr_subsamp <- dat$ante_depr_all  # !!! In BiB, antenatal depression in all used as perinatal depression in subsample !!!
dat$zbw_subsamp <- as.numeric(dat$zbw_subsamp)
dat$zbw_all <- as.numeric(dat$zbw_all)

# Modify definitions

## Breastfeeding NOT initiated
dat$bf_N_ini <-
  as.factor(1 - as.numeric(dat$bf_ini))  # Breastfeeding initiated -> Breastfeeding NOT initiated
table(dat$bf_ini, useNA = "always")
table(dat$bf_N_ini, useNA = "always")

## Breastfeeding NOT established
dat$bf_est[dat$bf_N_ini == 1 |
             dat$pretb_all == 1] <-
  NA  # !!! Exclude: 1) never breastfed, 2) preterm birth !!!
dat$bf_N_est <-
  as.factor(1 - as.numeric(dat$bf_est))  # Breastfeeding established -> Breastfeeding NOT established
table(dat$bf_est, useNA = "always")
table(dat$bf_N_est, useNA = "always")

## Breastfeeding NOT sustained
dat$bf_sus[dat$bf_N_ini == 1 |
             dat$pretb_all == 1] <-
  NA  # !!! Exclude: 1) never breastfed, 2) preterm birth !!!
dat$bf_N_sus <-
  as.factor(1 - as.numeric(dat$bf_sus))  # Breastfeeding sustained -> Breastfeeding NOT sustained
table(dat$bf_sus, useNA = "always")
table(dat$bf_N_sus, useNA = "always")

## Breastfeeding duration (4 categories)
table(dat$bf_dur_4c, useNA = "always")

# Label variables and values

## Load outcome lists and labels
MRPREG_outcome_labels <-
  read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

primary_bin <-
  read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Primary_bin")
primary_bin  # 13 primary (binary) outcomes

secondary_bin <-
  read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_bin")
secondary_bin  # 9 secondary binary outcomes

secondary_con <-
  read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_con")
secondary_con  # 4 secondary continuous outcomes

secondary_cat <-
  read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_cat")
secondary_cat  # 1 (secondary) ordinal/categorical outcome (bf_dur_4c as negative outcome)

## Identify available outcomes in the cohort
BiB_primary_bin <-
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat))]
BiB_primary_bin  # 11 primary (binary) outcomes available in BiB

BiB_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat))]
BiB_secondary_bin  # 8 secondary binary outcomes available in BiB

BiB_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat))]
BiB_secondary_con  # 4 secondary continuous outcomes available in BiB

BiB_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat))]
BiB_secondary_cat  # 1 (primary) ordinal/categorical outcome available in BiB

## Label values

### Binary
for (var_name in colnames(dat[, which(colnames(dat) %in% c(BiB_primary_bin, BiB_secondary_bin))])) {
  dat[[var_name]] <- factor(dat[[var_name]], labels = c("No", "Yes"))
}

### Ordinal/categorical
for (var_name in colnames(dat[, which(colnames(dat) %in% BiB_secondary_cat)])) {
  dat[[var_name]] <- factor(
    dat[[var_name]],
    label = c("0 to <1 month", "1 to <3 months", "3 to <6 months", ">=6 months"),
    # From Brion et al., 2011, IJE
    ordered = T
  )
}

### Label outcome variables
for (var_name in c(BiB_primary_bin,
                   BiB_secondary_bin,
                   BiB_secondary_con,
                   BiB_secondary_cat))
{
  var_lab(dat[, var_name]) <-
    MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$varname == var_name)]
}

# Sanity check
dat_check <- dat[, which(
  colnames(dat) %in% c(
    BiB_primary_bin,
    BiB_secondary_bin,
    BiB_secondary_con,
    BiB_secondary_cat
  )
)]

#------------------------------------------------------------------------------#
#                                  Covariates                                  #----
#------------------------------------------------------------------------------#

# Maternal age
dat$age_Mat_con <- as.numeric(dat$matage_yrs)

var_lab(dat$age_Mat_con) = "Maternal age (years)"

str(dat$age_Mat_con)
summary(dat$age_Mat_con)

# Ethnicity
ethnic <- as_tibble(read_dta(paste0(Sys.getenv('datadir'), ethnicity)))
head(ethnic)
dim(ethnic)

table(ethnic$deminfeth0eth3gp, useNA = "always")
table(ethnic$deminfeth0eth9gp, useNA = "always")
table(ethnic$deminfeth3gpcomb, useNA = "always")  # Questionnaire + GP record - !!! USE THIS !!!

## Detailed ethnic groups
ethnic_1 <- ethnic %>%
  mutate(ethnic_Mat_cat = deminfeth3gpcomb) %>%
  mutate(ethnic_Mat_cat = as_factor(ethnic_Mat_cat)) %>%
  set_variable_labels(ethnic_Mat_cat = "Maternal ethnicity (detailed)") %>%
  select(BiBPersonID, ethnic_Mat_cat) %>%
  distinct(BiBPersonID, .keep_all = T)

dat <- left_join(dat, ethnic_1, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$ethnic_Mat_cat)
table(dat$ethnic_Mat_cat, useNA = "always")

## Binary - White vs. Other
ethnic_2 <- ethnic %>%
  mutate(ethnic_Mat_bin = case_when(
    deminfeth3gpcomb == 1 ~ 0,
    deminfeth3gpcomb %in% c(2, 3) ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(ethnic_Mat_bin = factor(
    ethnic_Mat_bin,
    levels = c(0, 1),
    labels = c("White", "Other")
  )) %>%
  set_variable_labels(ethnic_Mat_bin = "Maternal ethnicity (binary)") %>%
  select(BiBPersonID, ethnic_Mat_bin) %>%
  distinct(BiBPersonID, .keep_all = T)

dat <- left_join(dat, ethnic_2, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$ethnic_Mat_bin)
table(dat$ethnic_Mat_bin, useNA = "always")

# Education level

## Ordinal / 3 categories
mbqall <- as_tibble(read_dta(paste0(Sys.getenv("datadir"), mbqall_dir)))
head(mbqall)
dim(mbqall)

table(mbqall$edu0mumede, useNA = "always")  # Educational qualification equivalised - !!! USE THIS !!!
# [1 <5 GCSE equivalent] [2 5 GCSE equivalent] [3 A-level equivalent] [4 Higher than A-level] [5 Other] [6 Don't know] [7 Foreign Unknown]
table(mbqall$edu0mumeuk, useNA = "always")

mbqall_edu <- mbqall %>%
  mutate(
    edu_Mat_3cat = case_when(
      edu0mumede == 1 ~ 1,
      edu0mumede %in% c(2, 3) ~ 2,
      edu0mumede == 4 ~ 3,
      TRUE ~ NA_real_
    )  # [5 Other] [6 Don't know] [7 Foreign Unknown] coded as NAs
  ) %>%
  mutate(edu_Mat_3cat = factor(
    edu_Mat_3cat,
    levels = c(1, 2, 3),
    labels = c("Low", "Medium", "High"),
    ordered = T
  )) %>%
  set_variable_labels(edu_Mat_3cat = "Maternal education attainment (3 categories)") %>%
  select(BiBPersonID, edu_Mat_3cat) %>%
  distinct(BiBPersonID, .keep_all = T)

dat <- left_join(dat, mbqall_edu, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$edu_Mat_3cat)
table(dat$edu_Mat_3cat, useNA = "always")

## Binary - Lower vs. Higher than college degree
dat <- dat %>%
  mutate(edu_Mat_bin = case_when(
    edu_Mat_3cat %in% c("Low", "Medium") ~ 0,
    edu_Mat_3cat == "High" ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(edu_Mat_bin = factor(
    edu_Mat_bin,
    levels = c(0, 1),
    labels = c("Lower than college degree", "College degree or higher")
  )) %>%
  set_variable_labels(edu_Mat_bin = "Maternal education attainment (binary)")

str(dat$edu_Mat_bin)
table(dat$edu_Mat_bin, useNA = "always")

# Index of multiple deprivation (IMD)
table(dat$imd_cat, useNA = "always")  # Highly skewed quintile distribution (N ~7000 in 1st and N ~200 in 5th quintile) - !!! NOT USED !!!
table(mbqall$imd_2007_decile_nat, useNA = "always")
table(mbqall$imd_2007_quintile_nat, useNA = "always")
summary(mbqall$imd_2007_score)  # !!! USE THIS !!! to calculate sample quintiles

mbqall_IMD <- mbqall %>%
  mutate(IMD_Fam_cat = ntile(imd_2007_score, 5)) %>%
  mutate(IMD_Fam_cat = factor(
    6 - IMD_Fam_cat,
    levels = 1:5,
    labels = c(
      "5th quintile (most deprived)",
      "4th quintile",
      "3rd quintile",
      "2nd quintile",
      "1st quintile (least deprived)"
    ),
    ordered = T
  )) %>%
  set_variable_labels(IMD_Fam_cat = "Townsend index (less vs. more deprived)") %>%
  select(BiBPersonID, IMD_Fam_cat) %>%
  distinct(BiBPersonID, .keep_all = T)

dat <- left_join(dat, mbqall_IMD, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$IMD_Fam_cat)
table(dat$IMD_Fam_cat, useNA = "always")

# Parity
table(dat$parity_b_multi, useNA = "always")  # Already derived in MR-PREG

dat$parity_Mat_bin <- as.character(dat$parity_b_multi)

dat$parity_Mat_bin[which(dat$parity_Mat_bin == "0")] <-
  "0"
dat$parity_Mat_bin[which(dat$parity_Mat_bin == "1")] <-
  ">=1"
dat$parity_Mat_bin <- factor(dat$parity_Mat_bin, levels = c("0", ">=1"))

var_lab(dat$parity_Mat_bin) = "Maternal parity (binary)"

str(dat$parity_Mat_bin)
CrossTable(dat$parity_Mat_bin)

# Pre-pregnancy BMI
summary(dat$matbmi_kgm2)  # Already derived in MR-PREG

dat$BMI_Mat_PRE.p_con <- as.numeric(dat$matbmi_kgm2)

var_lab(dat$BMI_Mat_PRE.p_con) = "Maternal pre-pregnancy BMI (kg/m^2)"

str(dat$BMI_Mat_PRE.p_con)
summary(dat$BMI_Mat_PRE.p_con)

# Smoking - Binary: Yes vs. No (in early to mid-pregnancy)
table(dat$matsmoke_b_dur, useNA = "always")  # Already derived in MR-PREG

dat <-
  dat %>% mutate(smoking_Mat_EAR.p_bin = case_when(matsmoke_b_dur == 0 ~ 0, matsmoke_b_dur == 1 ~ 1, TRUE ~ NA_real_))

val_lab(dat$smoking_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$smoking_Mat_EAR.p_bin <-
  as.factor(dat$smoking_Mat_EAR.p_bin)

var_lab(dat$smoking_Mat_EAR.p_bin) = "Maternal smoking in early pregnancy (binary)"

str(dat$smoking_Mat_EAR.p_bin)
CrossTable(dat$smoking_Mat_EAR.p_bin)

# Alcohol drinking - Binary: Yes vs. No (in early to mid-pregnancy)
table(dat$matalcohol_b_dur, useNA = "always")  # Already derived in MR-PREG

dat <-
  dat %>% mutate(
    alcohol_Mat_EAR.p_bin = case_when(matalcohol_b_dur == 0 ~ 0, matalcohol_b_dur == 1 ~ 1, TRUE ~ NA_real_)
  )

val_lab(dat$alcohol_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$alcohol_Mat_EAR.p_bin <-
  as.factor(dat$alcohol_Mat_EAR.p_bin)

var_lab(dat$alcohol_Mat_EAR.p_bin) = "Maternal alcohol drinking in early pregnancy (binary)"

str(dat$alcohol_Mat_EAR.p_bin)
CrossTable(dat$alcohol_Mat_EAR.p_bin)

# Physical activity - !!! TO BE ADDED !!!
# General Practice Physical Activity Score (BiB_Baseline.base_m23_exercise.GGPAQ_score)

# Dietary supplement use
## Categories:
### 1 : Daily
### 2 : 5-6 per week
### 3 : 2-4 per week
### 4 : Once a week
### 5 : Less often - !!! Recoded as "No" !!!
### NA - !!! Recoded as "No" !!!

table(mbqall$vit0ironpr, useNA = "always")
table(mbqall$vit0pregca, useNA = "always")
table(mbqall$vit0sanatp, useNA = "always")
table(mbqall$vit0vitcpr, useNA = "always")
table(mbqall$vit0vitdpr, useNA = "always")
table(mbqall$vit0vitepr, useNA = "always")

## Iron
mbqall_sup <- mbqall %>%
  mutate(iron.supp_Mat_EAR.p_bin = case_when(vit0ironpr %in% seq(1, 5) ~ 1, TRUE ~ 0)) %>%
  mutate(iron.supp_Mat_EAR.p_bin = replace_na(iron.supp_Mat_EAR.p_bin, 0)) %>%
  mutate(iron.supp_Mat_EAR.p_bin = factor(
    iron.supp_Mat_EAR.p_bin,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )) %>%
  set_variable_labels(iron.supp_Mat_EAR.p_bin = "Maternal iron supplement use during pregnancy (binary)") %>%
  select(BiBPersonID, iron.supp_Mat_EAR.p_bin) %>%
  distinct(BiBPersonID, .keep_all = TRUE)

dat <- left_join(dat, mbqall_sup, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$iron.supp_Mat_EAR.p_bin)
table(dat$iron.supp_Mat_EAR.p_bin, useNA = "always")

## Prenatal multivitamins (Pregnacare, Sanatogen, and other multivitamins)
mbqall_sup <- mbqall %>%
  mutate(
    multivit.supp_Mat_EAR.p_bin = case_when(
      vit0pregca %in% seq(1, 5) |
        vit0sanatp %in% seq(1, 5) |
        vit0othmvp == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(multivit.supp_Mat_EAR.p_bin = replace_na(multivit.supp_Mat_EAR.p_bin, 0)) %>%
  mutate(multivit.supp_Mat_EAR.p_bin = factor(
    multivit.supp_Mat_EAR.p_bin,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )) %>%
  set_variable_labels(multivit.supp_Mat_EAR.p_bin = "Maternal prenatal multivitamins use during pregnancy (binary)") %>%
  select(BiBPersonID, multivit.supp_Mat_EAR.p_bin) %>%
  distinct(BiBPersonID, .keep_all = TRUE)

dat <- left_join(dat, mbqall_sup, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$multivit.supp_Mat_EAR.p_bin)
table(dat$multivit.supp_Mat_EAR.p_bin, useNA = "always")

## Vitamin C
mbqall_sup <- mbqall %>%
  mutate(vitC.supp_Mat_EAR.p_bin = case_when(vit0vitcpr %in% seq(1, 5) ~ 1, TRUE ~ 0)) %>%
  mutate(vitC.supp_Mat_EAR.p_bin = replace_na(vitC.supp_Mat_EAR.p_bin, 0)) %>%
  mutate(vitC.supp_Mat_EAR.p_bin = factor(
    vitC.supp_Mat_EAR.p_bin,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )) %>%
  set_variable_labels(vitC.supp_Mat_EAR.p_bin = "Maternal vitamin C supplement use during pregnancy (binary)") %>%
  select(BiBPersonID, vitC.supp_Mat_EAR.p_bin) %>%
  distinct(BiBPersonID, .keep_all = TRUE)

dat <- left_join(dat, mbqall_sup, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$vitC.supp_Mat_EAR.p_bin)
table(dat$vitC.supp_Mat_EAR.p_bin, useNA = "always")

## Vitamin D
mbqall_sup <- mbqall %>%
  mutate(vitD.supp_Mat_EAR.p_bin = case_when(vit0vitdpr %in% seq(1, 5) ~ 1, TRUE ~ 0)) %>%
  mutate(vitD.supp_Mat_EAR.p_bin = replace_na(vitD.supp_Mat_EAR.p_bin, 0)) %>%
  mutate(vitD.supp_Mat_EAR.p_bin = factor(
    vitD.supp_Mat_EAR.p_bin,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )) %>%
  set_variable_labels(vitD.supp_Mat_EAR.p_bin = "Maternal vitamin D supplement use during pregnancy (binary)") %>%
  select(BiBPersonID, vitD.supp_Mat_EAR.p_bin) %>%
  distinct(BiBPersonID, .keep_all = TRUE)

dat <- left_join(dat, mbqall_sup, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$vitD.supp_Mat_EAR.p_bin)
table(dat$vitD.supp_Mat_EAR.p_bin, useNA = "always")

## Vitamin E
mbqall_sup <- mbqall %>%
  mutate(vitE.supp_Mat_EAR.p_bin = case_when(vit0vitepr %in% seq(1, 5) ~ 1, TRUE ~ 0)) %>%
  mutate(vitE.supp_Mat_EAR.p_bin = replace_na(vitE.supp_Mat_EAR.p_bin, 0)) %>%
  mutate(vitE.supp_Mat_EAR.p_bin = factor(
    vitE.supp_Mat_EAR.p_bin,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )) %>%
  set_variable_labels(vitE.supp_Mat_EAR.p_bin = "Maternal vitamin E supplement use during pregnancy (binary)") %>%
  select(BiBPersonID, vitE.supp_Mat_EAR.p_bin) %>%
  distinct(BiBPersonID, .keep_all = TRUE)

dat <- left_join(dat, mbqall_sup, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$vitE.supp_Mat_EAR.p_bin)
table(dat$vitE.supp_Mat_EAR.p_bin, useNA = "always")

## "Other vitamins and dietary supplements during pregnancy"
mbqall_sup <- mbqall %>%
  mutate(other.supp_Mat_EAR.p_bin = case_when(vit0othvpr == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(other.supp_Mat_EAR.p_bin = replace_na(other.supp_Mat_EAR.p_bin, 0)) %>%
  mutate(other.supp_Mat_EAR.p_bin = factor(
    other.supp_Mat_EAR.p_bin,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )) %>%
  set_variable_labels(other.supp_Mat_EAR.p_bin = "Other dietary supplement use during pregnancy (binary)") %>%
  select(BiBPersonID, other.supp_Mat_EAR.p_bin) %>%
  distinct(BiBPersonID, .keep_all = TRUE)

dat <- left_join(dat, mbqall_sup, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$other.supp_Mat_EAR.p_bin)
table(dat$other.supp_Mat_EAR.p_bin, useNA = "always")

## Any supplements
dat$any.supp_Mat_EAR.p_bin <- ifelse(
  dat$iron.supp_Mat_EAR.p_bin == "Yes" |
    dat$multivit.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitC.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitD.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitE.supp_Mat_EAR.p_bin == "Yes" |
    dat$other.supp_Mat_EAR.p_bin == "Yes",
  1,
  0
)

val_lab(dat$any.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$any.supp_Mat_EAR.p_bin <-
  as.factor(dat$any.supp_Mat_EAR.p_bin)

var_lab(dat$any.supp_Mat_EAR.p_bin) = "Maternal any supplement use during pregnancy (binary)"

str(dat$any.supp_Mat_EAR.p_bin)
table(dat$any.supp_Mat_EAR.p_bin, useNA = "always")

################################################################################
## Any supplements (excluding iron)
dat$NON.iron.supp_Mat_EAR.p_bin <- ifelse(
  dat$multivit.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitC.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitD.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitE.supp_Mat_EAR.p_bin == "Yes" |
    dat$other.supp_Mat_EAR.p_bin == "Yes",
  1,
  0
)

val_lab(dat$NON.iron.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.iron.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.iron.supp_Mat_EAR.p_bin)

var_lab(dat$NON.iron.supp_Mat_EAR.p_bin) = "Maternal any non-iron supplement use in early pregnancy (binary)"

str(dat$NON.iron.supp_Mat_EAR.p_bin)
table(dat$NON.iron.supp_Mat_EAR.p_bin, useNA = "always")
################################################################################
## Any supplements (excluding prenatal multivitamins)
dat$NON.multivit.supp_Mat_EAR.p_bin <- ifelse(
  dat$iron.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitC.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitD.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitE.supp_Mat_EAR.p_bin == "Yes" |
    dat$other.supp_Mat_EAR.p_bin == "Yes",
  1,
  0
)

val_lab(dat$NON.multivit.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.multivit.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.multivit.supp_Mat_EAR.p_bin)

var_lab(dat$NON.multivit.supp_Mat_EAR.p_bin) = "Maternal any non-multivitamins supplement use in early pregnancy (binary)"

str(dat$NON.multivit.supp_Mat_EAR.p_bin)
################################################################################
## Iron-containing supplements (iron + prenatal multivitamins) - !!! Used for iron supplementation-stratification analysis (both supplements contain iron) !!!
dat$iron.multivit.supp_Mat_EAR.p_bin <- ifelse(dat$iron.supp_Mat_EAR.p_bin == "Yes" |
                                                 dat$multivit.supp_Mat_EAR.p_bin == "Yes",
                                               1,
                                               0)

val_lab(dat$iron.multivit.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$iron.multivit.supp_Mat_EAR.p_bin <-
  as.factor(dat$iron.multivit.supp_Mat_EAR.p_bin)

var_lab(dat$iron.multivit.supp_Mat_EAR.p_bin) = "Maternal iron + prenatal multivitamins supplement use in early pregnancy (binary)"

str(dat$iron.multivit.supp_Mat_EAR.p_bin)
table(dat$iron.multivit.supp_Mat_EAR.p_bin, useNA = "always")
################################################################################
## Any supplements (excluding iron + prenatal multivitamins) - !!! Used for iron supplementation-stratification analysis (as a covariate) !!!
dat$NON.iron.multivit.supp_Mat_EAR.p_bin <- ifelse(
  dat$vitC.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitD.supp_Mat_EAR.p_bin == "Yes" |
    dat$vitE.supp_Mat_EAR.p_bin == "Yes" |
    dat$other.supp_Mat_EAR.p_bin == "Yes",
  1,
  0
)

val_lab(dat$NON.iron.multivit.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.iron.multivit.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.iron.multivit.supp_Mat_EAR.p_bin)

var_lab(dat$NON.iron.multivit.supp_Mat_EAR.p_bin) = "Maternal any non-iron/non-prenatal multivitamins supplement use in early pregnancy (binary)"

str(dat$NON.iron.multivit.supp_Mat_EAR.p_bin)
table(dat$NON.iron.multivit.supp_Mat_EAR.p_bin, useNA = "always")
################################################################################
## !!! "Taken any vitamins or iron tablets in last 4 weeks of pregnancy" !!!
mbqall_sup <- mbqall %>%
  mutate(vit0vitipr = case_when(vit0vitipr == 1 ~ 1, vit0vitipr == 2 ~ 0, TRUE ~ NA_real_)) %>%
  mutate(vit0vitipr = replace_na(vit0vitipr, 0)) %>%
  mutate(vit0vitipr = factor(
    vit0vitipr,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )) %>%
  set_variable_labels(vit0vitipr = "Taken any vitamins or iron tablets in last 4 weeks of pregnancy") %>%
  select(BiBPersonID, vit0vitipr) %>%
  distinct(BiBPersonID, .keep_all = TRUE)

dat <- left_join(dat, mbqall_sup, by = c("BiBMotherID" = "BiBPersonID"))

str(dat$vit0vitipr)
table(dat$vit0vitipr, useNA = "always")
################################################################################
### !!! Check !!!
CrossTable(dat$any.supp_Mat_EAR.p_bin, dat$vit0vitipr)
CrossTable(dat$iron.multivit.supp_Mat_EAR.p_bin, dat$vit0vitipr)
################################################################################

# Self-defined vegetarianism - !!! NOT AVAILABLE (due to little overlap between FFQ1 and FFQ2) !!!

# mFFQ1 <- as_tibble(read_dta(paste0(Sys.getenv("datadir_FFQ"), mFFQ1_dir)))
# head(mFFQ1)
# dim(mFFQ1)
#
# table(mFFQ1$ffq1Vegetarian, useNA = "always")
#
# ## 3 categories - Non-vegetarian, Vegetarian, Vegan
# mFFQ1$self.VegDiet_Mat_DUR.p_3cat <- mFFQ1$ffq1Vegetarian
#
# val_lab(mFFQ1$self.VegDiet_Mat_DUR.p_3cat) = c(
#   "Non-vegetarian" = 0,
#   "Vegetarian" = 1,
#   "Vegan" = 2
# )
#
# mFFQ1$self.VegDiet_Mat_DUR.p_3cat <-
#   as.factor(mFFQ1$self.VegDiet_Mat_DUR.p_3cat)
#
# var_lab(mFFQ1$self.VegDiet_Mat_DUR.p_3cat) = "Maternal self-defined vegetarianism during pregnancy (3 categories)"
#
# ## Binary - Non-vegetarian, Vegetarian
# mFFQ1 <-
#   mFFQ1 %>% mutate(
#     self.VegDiet_Mat_DUR.p_bin = case_when(
#       ffq1Vegetarian == 0 ~ 0,
#       ffq1Vegetarian %in% c(1, 2) ~ 1,
#       TRUE ~ NA_real_
#     )
#   )
#
# val_lab(mFFQ1$self.VegDiet_Mat_DUR.p_bin) = c("Non-vegetarian" = 0, "Vegetarian" = 1)
#
# mFFQ1$self.VegDiet_Mat_DUR.p_bin <-
#   as.factor(mFFQ1$self.VegDiet_Mat_DUR.p_bin)
#
# var_lab(mFFQ1$self.VegDiet_Mat_DUR.p_bin) = "Maternal self-defined vegetarianism during pregnancy (binary)"
#
# ################################################################################
# mFFQ1 <- mFFQ1 %>%
#   select(BiBPersonID,
#          self.VegDiet_Mat_DUR.p_3cat,
#          self.VegDiet_Mat_DUR.p_bin) %>%
#   distinct(BiBPersonID, .keep_all = T)
#
# dat <- left_join(dat, mFFQ1, by = c("BiBMotherID" = "BiBPersonID"))
#
# str(dat$self.VegDiet_Mat_DUR.p_3cat)
# table(dat$self.VegDiet_Mat_DUR.p_3cat, useNA = "always")
# CrossTable(dat$self.VegDiet_Mat_DUR.p_3cat)
#
# str(dat$self.VegDiet_Mat_DUR.p_bin)
# table(dat$self.VegDiet_Mat_DUR.p_bin, useNA = "always")
# ################################################################################

# Total energy intake - !!! NOT AVAILABLE !!!

# Nausea or vomiting - !!! NOT AVAILABLE !!! - Hyperemesis may be used?

# Nutrient intakes - !!! NOT AVAILABLE !!!

# Offspring sex
dat <-
  dat %>% mutate(sex_Chi_bin = case_when(sex_b_male == 0 ~ 1, sex_b_male == 1 ~ 0, TRUE ~ NA_real_))

val_lab(dat$sex_Chi_bin) = c("Male" = 0, "Female" = 1)

dat$sex_Chi_bin <- as.factor(dat$sex_Chi_bin)

var_lab(dat$sex_Chi_bin) = "Offspring sex"

str(dat$sex_Chi_bin)
table(dat$sex_Chi_bin, useNA = "always")

#------------------------------------------------------------------------------#
#                                 Check & Save                                 #----
#------------------------------------------------------------------------------#

head(dat)
dim(dat)  # 11865    XX

saveRDS(dat, "dat_cov_out.rds")
