################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 01-May-2025
# This script is to derive/rename outcome variables, covariates, and other relevant variables of interest in ALSPAC.

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
  tableone
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load data
dat <- as.tibble(readRDS("data/ALSPAC/dat_MRPREG_cov.rds"))

head(dat)
dim(dat)  # 14744   XXX

# Check inclusion criteria

## Live births

## Labels:
##  value                                 label
##  -9999                     Consent withdrawn
##    -11                  Triplet / quadruplet
##     -2               Birth outcome not known
##     -1                          Not enrolled
##      0                    fetal loss <20 wks
##      1                fetal death/sb 20+ wks
##      2                neonatal death <7 days
##      3           neonatal death 7 to 27 days
##      4 post-neonatal death 28 days to 1 year
##      5
##      7                              survivor
##     16                      recode - miscarr

dat <- dat[which(dat$kz010 >= 2), ]  # Exclude non-live births: 14744 -> 14345
mean(dat$kz010 >= 2) == 1  # TRUE

## Singletons
mean(dat$singleton == 1) == 1  # TRUE

## From unique mothers
mean(dat$unique == 1) == 1  # TRUE
sum(is.na(dat$aln))  # 0 with no pregnancy ID
sum(duplicated(dat$aln))  # 0 with duplicated pregnancy ID

#------------------------------------------------------------------------------#
#                               MR-PREG Outcomes                               #----
#------------------------------------------------------------------------------#

# Rename outcome variables
dat$sga <- dat$sga_all
dat$lga <- dat$lga_all

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
ALSPAC_primary_bin <-
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat))]
ALSPAC_primary_bin  # 13 primary (binary) outcomes available in ALSPAC

ALSPAC_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat))]
ALSPAC_secondary_bin  # 8 secondary binary outcomes available in ALSPAC

ALSPAC_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat))]
ALSPAC_secondary_con  # 4 secondary continuous outcomes available in ALSPAC

ALSPAC_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat))]
ALSPAC_secondary_cat  # 1 (primary) ordinal/categorical outcome available in ALSPAC

## Label values

### Binary
for (var_name in colnames(dat[, which(colnames(dat) %in% c(ALSPAC_primary_bin, ALSPAC_secondary_bin))])) {
  dat[[var_name]] <- factor(dat[[var_name]], labels = c("No", "Yes"))
}

### Ordinal/categorical
for (var_name in colnames(dat[, which(colnames(dat) %in% ALSPAC_secondary_cat)])) {
  dat[[var_name]] <- factor(
    dat[[var_name]],
    label = c("0 to <1 month", "1 to <3 months", "3 to <6 months", ">=6 months"),
    # From Brion et al., 2011, IJE
    ordered = T
  )
}

### Label outcome variables
for (var_name in c(
  ALSPAC_primary_bin,
  ALSPAC_secondary_bin,
  ALSPAC_secondary_con,
  ALSPAC_secondary_cat
))
{
  var_lab(dat[, var_name]) <-
    MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$varname == var_name)]
}

# Sanity check
dat_check <- dat[, which(
  colnames(dat) %in% c(
    ALSPAC_primary_bin,
    ALSPAC_secondary_bin,
    ALSPAC_secondary_con,
    ALSPAC_secondary_cat
  )
)]

#------------------------------------------------------------------------------#
#                        Early/Mid-Pregnancy Covariates                        #----
#------------------------------------------------------------------------------#

# Maternal age
summary(dat$matage_yrs)  # Already derived in MR-PREG

dat$age_Mat_con <- as.numeric(dat$matage_yrs)

var_lab(dat$age_Mat_con) = "Maternal age (years) at delivery"

str(dat$age_Mat_con)
summary(dat$age_Mat_con)

# Ethnicity

## Detailed ethnic groups
dat$ethnic_Mat_cat <- NA
dat$ethnic_Mat_cat[which(dat$c800 == 1)] <- "White"
dat$ethnic_Mat_cat[which(dat$c800 %in% c(2, 3, 4))] <- "Black"
dat$ethnic_Mat_cat[which(dat$c800 %in% c(5, 6, 7, 8))] <- "Asian"
dat$ethnic_Mat_cat[which(dat$c800 == 9)] <- "Mixed or Other"

dat$ethnic_Mat_cat <- factor(dat$ethnic_Mat_cat,
                             levels = c("White", "Black", "Asian", "Mixed or Other"))

var_lab(dat$ethnic_Mat_cat) = "Maternal ethnicity (detailed)"

str(dat$ethnic_Mat_cat)
CrossTable(dat$ethnic_Mat_cat)

## Binary - White vs. Other
table(dat$mateth_b_other, useNA = "always")  # Already derived in MR-PREG

dat$ethnic_Mat_bin <- as.character(dat$mateth_b_other)

dat$ethnic_Mat_bin[which(dat$ethnic_Mat_bin == "0")] <- "White"
dat$ethnic_Mat_bin[which(dat$ethnic_Mat_bin == "1")] <- "Other"

dat$ethnic_Mat_bin <- factor(dat$ethnic_Mat_bin, levels = c("White", "Other"))

var_lab(dat$ethnic_Mat_bin) = "Maternal ethnicity (binary)"

str(dat$ethnic_Mat_bin)
CrossTable(dat$ethnic_Mat_bin)

# Education level

## Ordinal / 3 categories
table(dat$matedu_cat3, useNA = "always")  # Already derived in MR-PREG

dat$edu_Mat_3cat <- as.character(dat$matedu_cat3)

dat$edu_Mat_3cat[dat$edu_Mat_3cat == "0"] <-
  "Low"  # Low: None/CSE
dat$edu_Mat_3cat[dat$edu_Mat_3cat == "1"] <-
  "Medium"  # Medium: O-level / A-level / vocational
dat$edu_Mat_3cat[dat$edu_Mat_3cat == "2"] <-
  "High"  # High: Degree

dat$edu_Mat_3cat <-
  factor(dat$edu_Mat_3cat,
         levels = c("Low", "Medium", "High"),
         ordered = T)  # Set as ordinal variable

var_lab(dat$edu_Mat_3cat) = "Maternal education attainment (3 categories)"

str(dat$edu_Mat_3cat)
CrossTable(dat$edu_Mat_3cat)

## Binary
table(dat$matedu_b_uni, useNA = "always")  # Already derived in MR-PREG

dat$edu_Mat_bin <- as.character(dat$matedu_b_uni)

dat$edu_Mat_bin[which(dat$edu_Mat_bin == "0")] <-
  "Lower than college degree"
dat$edu_Mat_bin[which(dat$edu_Mat_bin == "1")] <-
  "College degree or higher"

dat$edu_Mat_bin <- factor(dat$edu_Mat_bin,
                          levels = c("Lower than college degree", "College degree or higher"))

var_lab(dat$edu_Mat_bin) = "Maternal education attainment (binary)"

str(dat$edu_Mat_bin)
CrossTable(dat$edu_Mat_bin)

# Index of multiple deprivation (IMD)
table(dat$imd_cat, useNA = "always")  # Already derived in MR-PREG

dat$IMD_Fam_cat <- as.character(dat$imd_cat)

dat$IMD_Fam_cat[dat$IMD_Fam_cat == "1"] <- "5th quintile (most deprived)"
dat$IMD_Fam_cat[dat$IMD_Fam_cat == "2"] <- "4th quintile"
dat$IMD_Fam_cat[dat$IMD_Fam_cat == "3"] <- "3rd quintile"
dat$IMD_Fam_cat[dat$IMD_Fam_cat == "4"] <- "2nd quintile"
dat$IMD_Fam_cat[dat$IMD_Fam_cat == "5"] <- "1st quintile (least deprived)"

dat$IMD_Fam_cat <-
  factor(
    dat$IMD_Fam_cat,
    levels = c(
      "5th quintile (most deprived)",
      "4th quintile",
      "3rd quintile",
      "2nd quintile",
      "1st quintile (least deprived)"
    ),
    ordered = T
  )  # Set as ordinal variable

var_lab(dat$IMD_Fam_cat) = "Index of multiple deprivation (less vs. more deprived)"

str(dat$IMD_Fam_cat)
CrossTable(dat$IMD_Fam_cat)
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

# Smoking - Collected at 18 weeks of gestation

## Already derived in MR-PREG
table(dat$matsmoke_b_prior, useNA = "always")
table(dat$matsmoke_b_dur, useNA = "always")

## 3 categories - Never/Former/Current
dat <-
  dat %>% mutate(
    smoking_Mat_EAR.p_3cat = case_when(
      matsmoke_b_prior == 0 & matsmoke_b_dur == 0 ~ 0,
      matsmoke_b_prior == 1 & matsmoke_b_dur == 0 ~ 1,
      matsmoke_b_dur == 1 ~ 2,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$smoking_Mat_EAR.p_3cat) = c("Never" = 0,
                                        "Former" = 1,
                                        "Current" = 2)

dat$smoking_Mat_EAR.p_3cat <-
  as.factor(dat$smoking_Mat_EAR.p_3cat)

var_lab(dat$smoking_Mat_EAR.p_3cat) = "Maternal smoking (3 categories)"

str(dat$smoking_Mat_EAR.p_3cat)
CrossTable(dat$smoking_Mat_EAR.p_3cat)

## Binary - Yes vs. No (in early to mid-pregnancy)
dat <-
  dat %>% mutate(smoking_Mat_EAR.p_bin = case_when(matsmoke_b_dur == 0 ~ 0, matsmoke_b_dur == 1 ~ 1, TRUE ~ NA_real_))

val_lab(dat$smoking_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$smoking_Mat_EAR.p_bin <-
  as.factor(dat$smoking_Mat_EAR.p_bin)

var_lab(dat$smoking_Mat_EAR.p_bin) = "Maternal smoking in early pregnancy (binary)"

str(dat$smoking_Mat_EAR.p_bin)
CrossTable(dat$smoking_Mat_EAR.p_bin)

# Alcohol drinking (Yes vs. No) - Collected at 18 weeks of gestation

## Already derived in MR-PREG
table(dat$matalcohol_b_prior, useNA = "always")
table(dat$matalcohol_b_dur, useNA = "always")

### Binary - Yes (<1 glass per week) vs. No (>=1 glass per week) (in early to mid-pregnancy)
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

# Physical activity - Collected at 18 weeks of gestation

## Continuous (in hours/week)
dat$phys.act_Mat_EAR.p_con <- dat$b635
dat$phys.act_Mat_EAR.p_con[dat$phys.act_Mat_EAR.p_con < 0] <- NA

var_lab(dat$phys.act_Mat_EAR.p_con) = "Maternal physical activity in early pregnancy (hours/week)"

str(dat$phys.act_Mat_EAR.p_con)
summary(dat$phys.act_Mat_EAR.p_con)

## Categorical (4 categories, ordered)
dat$phys.act_Mat_EAR.p_cat <- dat$b636
dat$phys.act_Mat_EAR.p_cat[dat$phys.act_Mat_EAR.p_cat < 0] <- NA

val_lab(dat$phys.act_Mat_EAR.p_cat) = c(
  "1-4" = 1,
  "5-9" = 2,
  "10-14" = 3,
  "15+" = 4
)

dat$phys.act_Mat_EAR.p_cat <-
  factor(
    dat$phys.act_Mat_EAR.p_cat,
    levels = c("1-4", "5-9", "10-14", "15+"),
    ordered = T
  )  # Set as ordinal variable

var_lab(dat$phys.act_Mat_EAR.p_cat) = "Maternal physical activity in early pregnancy (4 categories)"

str(dat$phys.act_Mat_EAR.p_cat)
CrossTable(dat$phys.act_Mat_EAR.p_cat)

# Dietary supplement use  (before dietary measurement)

# --Collected at 18 weeks of gestation
# b140	Taking iron during this PREG
# b141	Taking zinc during this PREG
# b142	Taking calcium during this PREG
# b143	Taking folic acid during this PREG
# b144	Taking vitamins during this PREG
# b149	Other supplements or diet food this PREG
dat[, c("b140", "b141", "b142", "b143", "b144", "b149")] <-
  replace(dat[, c("b140", "b141", "b142", "b143", "b144", "b149")], dat[, c("b140", "b141", "b142", "b143", "b144", "b149")] < 0, NA)  # Values < 0 are recoded as missing

dat[, c("b140", "b141", "b142", "b143", "b144", "b149")] <- lapply(dat[, c("b140", "b141", "b142", "b143", "b144", "b149")], function(x)
  2 - x)  # Recode: 2 as 0, 1 as 1

table(dat$b140, useNA = "always")

################################################################################

## Iron
dat <-
  dat %>% mutate(iron.supp_Mat_EAR.p_bin = case_when(b140 == 0 ~ 0, b140 == 1 ~ 1, TRUE ~ NA_real_))

val_lab(dat$iron.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$iron.supp_Mat_EAR.p_bin <-
  as.factor(dat$iron.supp_Mat_EAR.p_bin)

var_lab(dat$iron.supp_Mat_EAR.p_bin) = "Maternal iron supplement use in early pregnancy (binary)"

str(dat$iron.supp_Mat_EAR.p_bin)
CrossTable(dat$iron.supp_Mat_EAR.p_bin)
################################################################################
## Any supplements (excluding iron)
dat$NON.iron.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("b141", "b142", "b143", "b144", "b149")], na.rm = T)

dat <-
  dat %>% mutate(
    NON.iron.supp_Mat_EAR.p_bin = case_when(
      NON.iron.supp_Mat_EAR.p_bin == 0 ~ 0,
      NON.iron.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$NON.iron.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.iron.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.iron.supp_Mat_EAR.p_bin)

var_lab(dat$NON.iron.supp_Mat_EAR.p_bin) = "Maternal any non-iron supplement use in early pregnancy (binary)"

str(dat$NON.iron.supp_Mat_EAR.p_bin)
CrossTable(dat$NON.iron.supp_Mat_EAR.p_bin)
################################################################################

## Zinc
dat <-
  dat %>% mutate(zinc.supp_Mat_EAR.p_bin = case_when(b141 == 0 ~ 0, b141 == 1 ~ 1, TRUE ~ NA_real_))

val_lab(dat$zinc.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$zinc.supp_Mat_EAR.p_bin <-
  as.factor(dat$zinc.supp_Mat_EAR.p_bin)

var_lab(dat$zinc.supp_Mat_EAR.p_bin) = "Maternal zinc supplement use in early pregnancy (binary)"

str(dat$zinc.supp_Mat_EAR.p_bin)
CrossTable(dat$zinc.supp_Mat_EAR.p_bin)

################################################################################

## Calcium
dat <-
  dat %>% mutate(calcium.supp_Mat_EAR.p_bin = case_when(b142 == 0 ~ 0, b142 == 1 ~ 1, TRUE ~ NA_real_))

val_lab(dat$calcium.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$calcium.supp_Mat_EAR.p_bin <-
  as.factor(dat$calcium.supp_Mat_EAR.p_bin)

var_lab(dat$calcium.supp_Mat_EAR.p_bin) = "Maternal calcium supplement use in early pregnancy (binary)"

str(dat$calcium.supp_Mat_EAR.p_bin)
CrossTable(dat$calcium.supp_Mat_EAR.p_bin)
################################################################################
## Any supplements (excluding calcium)
dat$NON.calcium.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("b140", "b141", "b143", "b144", "b149")], na.rm = T)

dat <-
  dat %>% mutate(
    NON.calcium.supp_Mat_EAR.p_bin = case_when(
      NON.calcium.supp_Mat_EAR.p_bin == 0 ~ 0,
      NON.calcium.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$NON.calcium.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.calcium.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.calcium.supp_Mat_EAR.p_bin)

var_lab(dat$NON.calcium.supp_Mat_EAR.p_bin) = "Maternal any non-calcium supplement use in early pregnancy (binary)"

str(dat$NON.calcium.supp_Mat_EAR.p_bin)
CrossTable(dat$NON.calcium.supp_Mat_EAR.p_bin)
################################################################################

## Folic acid
dat <-
  dat %>% mutate(folate.supp_Mat_EAR.p_bin = case_when(b143 == 0 ~ 0, b143 == 1 ~ 1, TRUE ~ NA_real_))

val_lab(dat$folate.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$folate.supp_Mat_EAR.p_bin <-
  as.factor(dat$folate.supp_Mat_EAR.p_bin)

var_lab(dat$folate.supp_Mat_EAR.p_bin) = "Maternal folic acid supplement use in early pregnancy (binary)"

str(dat$folate.supp_Mat_EAR.p_bin)
CrossTable(dat$folate.supp_Mat_EAR.p_bin)
################################################################################
## Any supplements (excluding folic acid)
dat$NON.folate.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("b140", "b141", "b142", "b144", "b149")], na.rm = T)

dat <-
  dat %>% mutate(
    NON.folate.supp_Mat_EAR.p_bin = case_when(
      NON.folate.supp_Mat_EAR.p_bin == 0 ~ 0,
      NON.folate.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$NON.folate.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.folate.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.folate.supp_Mat_EAR.p_bin)

var_lab(dat$NON.folate.supp_Mat_EAR.p_bin) = "Maternal any non-folic acid supplement use in early pregnancy (binary)"

str(dat$NON.folate.supp_Mat_EAR.p_bin)
CrossTable(dat$NON.folate.supp_Mat_EAR.p_bin)
################################################################################

## Vitamins
dat <-
  dat %>% mutate(vits.supp_Mat_EAR.p_bin = case_when(b144 == 0 ~ 0, b144 == 1 ~ 1, TRUE ~ NA_real_))

val_lab(dat$vits.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$vits.supp_Mat_EAR.p_bin <-
  as.factor(dat$vits.supp_Mat_EAR.p_bin)

var_lab(dat$vits.supp_Mat_EAR.p_bin) = "Maternal vitamin supplement use in early pregnancy (binary)"

str(dat$vits.supp_Mat_EAR.p_bin)
CrossTable(dat$vits.supp_Mat_EAR.p_bin)

################################################################################

## Other supplements
dat <-
  dat %>% mutate(other.supp_Mat_EAR.p_bin = case_when(b149 == 0 ~ 0, b149 == 1 ~ 1, TRUE ~ NA_real_))

val_lab(dat$other.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$other.supp_Mat_EAR.p_bin <-
  as.factor(dat$other.supp_Mat_EAR.p_bin)

var_lab(dat$other.supp_Mat_EAR.p_bin) = "Maternal other supplement use in early pregnancy (binary)"

str(dat$other.supp_Mat_EAR.p_bin)
CrossTable(dat$other.supp_Mat_EAR.p_bin)

################################################################################

## Any supplements
dat$any.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("b140", "b141", "b142", "b143", "b144", "b149")], na.rm = T)

dat <-
  dat %>% mutate(
    any.supp_Mat_EAR.p_bin = case_when(
      any.supp_Mat_EAR.p_bin == 0 ~ 0,
      any.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$any.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$any.supp_Mat_EAR.p_bin <-
  as.factor(dat$any.supp_Mat_EAR.p_bin)

var_lab(dat$any.supp_Mat_EAR.p_bin) = "Maternal any supplement use in early pregnancy (binary)"

str(dat$any.supp_Mat_EAR.p_bin)
CrossTable(dat$any.supp_Mat_EAR.p_bin)

#------------------------------------------------------------------------------#
#                 Late Pregnancy Covariates (Same Time as FFQ)                 #----
#------------------------------------------------------------------------------#

# Self-defined vegetarianism
table(dat$c340, useNA = "always")  # Self-defined vegetarian
table(dat$c342, useNA = "always")  # Self-defined vegan

## 3 categories - Non-vegetarian, Vegetarian, Vegan
dat <-
  dat %>% mutate(
    self.VegDiet_Mat_DUR.p_3cat = case_when(
      c340 %in% c(2, 3) & c342 %in% c(2, 3) ~ 0,
      c340 == 1 ~ 1,
      c342 == 1 ~ 2,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$self.VegDiet_Mat_DUR.p_3cat) = c(
  "Non-vegetarian" = 0,
  "Vegetarian" = 1,
  "Vegan" = 2
)

dat$self.VegDiet_Mat_DUR.p_3cat <-
  as.factor(dat$self.VegDiet_Mat_DUR.p_3cat)

var_lab(dat$self.VegDiet_Mat_DUR.p_3cat) = "Maternal self-defined vegetarianism during pregnancy (3 categories)"

str(dat$self.VegDiet_Mat_DUR.p_3cat)
CrossTable(dat$self.VegDiet_Mat_DUR.p_3cat)

## Binary - Non-vegetarian, Vegetarian
dat <-
  dat %>% mutate(
    self.VegDiet_Mat_DUR.p_bin = case_when(
      c340 %in% c(2, 3) & c342 %in% c(2, 3) ~ 0,
      c340 == 1 | c342 == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$self.VegDiet_Mat_DUR.p_bin) = c("Non-vegetarian" = 0, "Vegetarian" = 1)

dat$self.VegDiet_Mat_DUR.p_bin <-
  as.factor(dat$self.VegDiet_Mat_DUR.p_bin)

var_lab(dat$self.VegDiet_Mat_DUR.p_bin) = "Maternal self-defined vegetarianism during pregnancy (binary)"

str(dat$self.VegDiet_Mat_DUR.p_bin)
CrossTable(dat$self.VegDiet_Mat_DUR.p_bin)

# Daily nutrient intakes

## Load label list
nutrient_labels <-
  read.xlsx(
    "data/ALSPAC/ALSPAC_data_catalog_C.xlsx",
    sheet = "Nutrients",
    colNames = T,
    rowNames = F
  )
nutrient_labels
str(nutrient_labels)  # 36 variables ("trypt60" not included)

## Assign label to each variable
for (var_name in nutrient_labels$Varname) {
  var_lab(dat[, var_name]) = nutrient_labels$VarLabel[nutrient_labels$Varname ==
                                                        var_name]
}

## Convert -1 into NA
for (var_name in nutrient_labels$Varname) {
  dat[, var_name][dat[, var_name] < 0] <- NA
}

## Sanity check
head(dat[, nutrient_labels$Varname])

# Total energy intake (kcal/day)
dat$energy_Mat_DUR.p_con <- as.numeric(dat$c3804) / 4.184  # Convert kJ into kcal

var_lab(dat$energy_Mat_DUR.p_con) = "Maternal total energy intake (kcal/day) during pregnancy"

str(dat$energy_Mat_DUR.p_con)
summary(dat$energy_Mat_DUR.p_con)

################################################################################
# ## Energy-adjusted nutrient intakes - Residual Method - !!! NO LONGER USED !!!
# head(dat[, nutrient_labels$varname])
#
# for (nutrient in nutrient_labels$varname) {
#   ### Exclude rows with NA
#   temp_data <-
#     dat[!is.na(dat[[nutrient]]) &
#           !is.na(dat[["energy_Mat_DUR.p_con"]]), ]
#
#   ### Set up linear regression model
#   model <-
#     lm(temp_data[[nutrient]] ~ temp_data[["energy_Mat_DUR.p_con"]])
#
#   ### Extract residuals
#   residuals <- resid(model)
#
#   ### Calculate sample mean of nutrient intake
#   nutrient_mean <- mean(temp_data[[nutrient]], na.rm = T)
#
#   ### Calculate energy-adjusted nutrient intake
#   nutrient_adjusted <- residuals + nutrient_mean
#
#   ### Replace nutrient intake values
#   dat[!is.na(dat[[nutrient]]) &
#         !is.na(dat[["energy_Mat_DUR.p_con"]]), nutrient] <-
#     nutrient_adjusted
# }
#
# head(dat[, nutrient_labels$varname])
################################################################################

# Smoking (Yes vs. No) - Collected at 32 weeks of gestation
dat <-
  dat %>% mutate(smoking_Mat_LAT.p_bin = case_when(c482 == 0 ~ 0, c482 > 0 ~ 1, TRUE ~ NA_real_))

val_lab(dat$smoking_Mat_LAT.p_bin) = c("No" = 0, "Yes" = 1)

dat$smoking_Mat_LAT.p_bin <-
  as.factor(dat$smoking_Mat_LAT.p_bin)

var_lab(dat$smoking_Mat_LAT.p_bin) = "Maternal smoking in late pregnancy (binary)"

str(dat$smoking_Mat_LAT.p_bin)
CrossTable(dat$smoking_Mat_LAT.p_bin)

# Alcohol drinking (Yes vs. No) - Collected at 32 weeks of gestation
dat <-
  dat %>% mutate(alcohol_Mat_LAT.p_bin = case_when(e220 %in% c(1, 2) ~ 0, e220 > 2 ~ 1, TRUE ~ NA_real_))  # No (<1 glass per week); Yes (>=1 glass per week)

val_lab(dat$alcohol_Mat_LAT.p_bin) = c("No" = 0, "Yes" = 1)

dat$alcohol_Mat_LAT.p_bin <-
  as.factor(dat$alcohol_Mat_LAT.p_bin)

var_lab(dat$alcohol_Mat_LAT.p_bin) = "Maternal alcohol drinking in late pregnancy (binary)"

str(dat$alcohol_Mat_LAT.p_bin)
CrossTable(dat$alcohol_Mat_LAT.p_bin)

# Physical activity - Collected at 18 weeks of gestation

## Continuous (in hours/week)
dat$phys.act_Mat_LAT.p_con <- dat$c504
dat$phys.act_Mat_LAT.p_con[dat$phys.act_Mat_LAT.p_con < 0] <- NA

var_lab(dat$phys.act_Mat_LAT.p_con) = "Maternal physical activity in late pregnancy (hours/week)"

str(dat$phys.act_Mat_LAT.p_con)
summary(dat$phys.act_Mat_LAT.p_con)

# Dietary supplement use  (before dietary measurement)

# --After 18 weeks of gestation
# c110	Taken iron in last 3MTHS
# c111	Taken zinc in last 3MTHS
# c112	Taken calcium in last 3MTHS
# c113	Taken folic acid in last 3MTHS
# c114	Taken vitamins in last 3MTHS
# c115	Taken other supplements in last 3MTHS
dat[, c("c110", "c111", "c112", "c113", "c114", "c115")] <-
  replace(dat[, c("c110", "c111", "c112", "c113", "c114", "c115")], dat[, c("c110", "c111", "c112", "c113", "c114", "c115")] < 0, NA)  # Values < 0 are recoded as missing

dat[, c("c110", "c111", "c112", "c113", "c114", "c115")] <- lapply(dat[, c("c110", "c111", "c112", "c113", "c114", "c115")], function(x)
  2 - x)  # Recode: 2 as 0, 1 as 1

table(dat$c110, useNA = "always")

## Any supplements
dat$any.supp_Mat_LAT.p_bin <-
  rowSums(dat[, c("c110", "c111", "c112", "c113", "c114", "c115")], na.rm = T)

dat <-
  dat %>% mutate(
    any.supp_Mat_LAT.p_bin = case_when(
      any.supp_Mat_LAT.p_bin == 0 ~ 0,
      any.supp_Mat_LAT.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$any.supp_Mat_LAT.p_bin) = c("No" = 0, "Yes" = 1)

dat$any.supp_Mat_LAT.p_bin <-
  as.factor(dat$any.supp_Mat_LAT.p_bin)

var_lab(dat$any.supp_Mat_LAT.p_bin) = "Maternal any supplement use in late pregnancy (binary)"

str(dat$any.supp_Mat_LAT.p_bin)
CrossTable(dat$any.supp_Mat_LAT.p_bin)

# Offspring sex
dat <-
  dat %>% mutate(sex_Chi_bin = case_when(sex_b_male == 0 ~ 1, sex_b_male == 1 ~ 0, TRUE ~ NA_real_))

val_lab(dat$sex_Chi_bin) = c("Male" = 0, "Female" = 1)

dat$sex_Chi_bin <- as.factor(dat$sex_Chi_bin)

var_lab(dat$sex_Chi_bin) = "Offspring sex"

str(dat$sex_Chi_bin)
CrossTable(dat$sex_Chi_bin)

#------------------------------------------------------------------------------#
#                                 Check & Save                                 #----
#------------------------------------------------------------------------------#

head(dat)
dim(dat)  # 14345   XXX

saveRDS(dat, "data/ALSPAC/dat_cov_out.rds")

################################################################################
