################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 31-Oct-2024
# This script is to derive MR-PREG covariates in ALSPAC.

# ADAPTED FROM: "MRPREG_ALSPAC_covars_derive_20240313.R" written by the MR-PREG team

################################################################################

################################################################################
#                          MR-PREG ALSPAC Covariates                           #
################################################################################

# This script is to derive ALSPAC covariates for the MR-PREG collaboration.
# Covariates include: maternal age, ethnicity, education, deprivation, parity, smoking, alcohol consumption, BMI, offspring sex.

# Author: Peiyuan (Ran) Huang
# Checker: Ana Goncalves Soares
# Acknowledgement: Alba Fernandez-Sanles, Gemma Sharp

# Last edited date: 18 Sep 2023
## Changelog on 12 Sep 2023: Defined and renamed all covariates according to the moba tab;
##                           created two variables (i.e., before and during pregnancy) for smoking and alcohol drinking, respectively.
## Changelog on 18 Sep 2023: Removed the continuous parity variable as per the moba tab;
##                           combined Townsend deprivation index at different timepoints into one variable as suggested by Ana - used C and replaced missing values with B.
## Changelog on 25 Sep 2023: Rename and recode Townsend index as per the bib tab.
## Changelog on 31 Oct 2024: Use IMD instead of Townsend index

# [Note: This script is partly based on Alba's script (4.distribution_vars_20220117.R),
#        and aims to derive "simple versions" of the covariates.
#        For more detailed and complex classification/categorisation of each covariate,
#        please refer to Gemma Sharp's EPoCH project script:
#        https://github.com/ammegandchips/EPoCH/blob/main/data_prep/cohorts/alspac/alspac_derive.R ]

#------------------------------------------------------------------------------#
#                                 Housekeeping                                 #----
#------------------------------------------------------------------------------#

rm(list = ls())

sessionInfo()

library(tidyverse)
library(haven)
library(tableone)

# [PLEASE CHANCE] Set your working directory
setwd("Z:/working/data/ALSPAC/")

#------------------------------------------------------------------------------#
#                           Read Phenotype Datasets                            #----
#------------------------------------------------------------------------------#

# [NOTE: First of all, please use your own methods and preferred software to complete the following steps:
#        - Merge different ALSPAC files
#        - Extract raw variables
#        - Remove WoC (see details in this folder: Syntax/Withdrawal of consent)]

# [PLEASE CHANCE] Read ALSPAC raw phenotype dataset (merged and extracted, with WoC removed)
covars <- readRDS("dat_out.rds")
table(covars$unique)
head(covars)
dim(covars)

#------------------------------------------------------------------------------#
#                              Derive Covariates                               #----
#------------------------------------------------------------------------------#

# Maternal age at delivery (in years) - mz028b, from dataset mz_6a

# Valid values 15~44
# Triplet / quadruplet -11
# Not in core sample -10
# Outcome NK -4
# Miscarried -2

table(covars$mz028b, useNA = "always")

covars$matage_yrs <- ifelse(covars$mz028b > 0, covars$mz028b, NA)

summary(covars$matage_yrs)

################################################################################

# Maternal ethnicity - c800, from dataset c_8a

# White 1
# Black Caribbean 2
# Black African 3
# Other black 4
# Indian 5
# Pakistani 6
# Bangladeshi 7
# Chinese 8
# Other 9
# Missing -1

table(covars$c800, useNA = "always")

covars$mateth_b_other <- ifelse(covars$c800 == 1, 0, # White
                                ifelse(covars$c800 > 1, 1, # Non-White
                                       NA))

table(covars$mateth_b_other, useNA = "always")

################################################################################

# Maternal education - c645a, from dataset c_8a

# CSE/none 1
# Vocational 2
# O-level 3
# A-level 4
# Degree 5
# Missing -1

table(covars$c645a, useNA = "always")

covars$matedu_UK <-
  ifelse(covars$c645a == 1,
         0,
         # None/CSE
         ifelse(
           covars$c645a == 2 | covars$c645a == 3,
           1,
           # O-level or vocational
           ifelse(covars$c645a == 4, 2, # A-level
                  ifelse(covars$c645a == 5, 3, # Degree
                         NA))
         ))
# [NOTE on 12 Sept 2023: I still kept the variable for UK education levels and renamed it with "_UK" suffix.]

covars$matedu_cat3 <-
  ifelse(covars$c645a == 1, 0, # Low: None/CSE
         ifelse(covars$c645a %in% c(2, 3, 4), 1, # Medium: O-level / A-level / vocational
                ifelse(covars$c645a == 5, 2, # High: Degree
                       NA)))

covars$matedu_b_uni <-
  ifelse(covars$c645a %in% c(1, 2, 3, 4), 0, # Lower than university level
         ifelse(covars$c645a == 5, 1, # University level or higher
                NA))

table(covars$matedu_cat3, useNA = "always")
table(covars$matedu_b_uni, useNA = "always")

################################################################################

# Index of multiple deprivation (IMD)
# [NOTE on 31 Oct 2024: use IMD instead of TDI; use IMD 2000 to reflect the time when ALSPAC was established.]

table(covars$aimd2000q5, useNA = "always")
table(covars$bimd2000q5, useNA = "always")
table(covars$cimd2000q5, useNA = "always")

covars$imd_cat <- covars$cimd2000q5  # 1: 1st quintile (the least deprived); 5: 5th quintile (the most deprived)
covars$imd_cat <- ifelse(covars$imd_cat < 0, covars$bimd2000q5, covars$imd_cat)  # Replace missing values (-1) with IMD values at timepoint B
covars$imd_cat <- ifelse(covars$imd_cat < 0, covars$aimd2000q5, covars$imd_cat)  # Replace missing values (-1) with IMD values at timepoint A

covars$imd_cat[covars$imd_cat < 0] <- NA  # Replace remaining missing values (-1) with NA

covars$imd_cat <- 6 - covars$imd_cat  # Recoded as: 1 = the most deprived; 5 = the least deprived

table(covars$imd_cat, useNA = "always")

################################################################################

# Parity - b032, from dataset b_4f
# [NOTE on 12 Sept 2023: Parity (variable B032) is defined as the number of previous pregnancies resulting in either a livebirth or a stillbirth.]

# Valid values 0~22
# HaB short -7
# Inconsistent data -2
# Missing -1

table(covars$b032, useNA = "always")

covars$parity_preg <- ifelse(covars$b032 >= 0, covars$b032, NA)

covars$parity_cat5 <- covars$b032
covars$parity_cat5 <- ifelse(covars$parity_cat5 >= 4,
                             4,
                             ifelse(covars$parity_cat5 < 0, NA, covars$parity_cat5))

covars$parity_b_multi <- ifelse(covars$b032 == 0, 0, # Nulliparous
                                ifelse(covars$b032 > 0, 1, # Multiparous
                                       NA))

table(covars$parity_preg, useNA = "always")
table(covars$parity_cat5, useNA = "always")
table(covars$parity_b_multi, useNA = "always")

################################################################################

# Maternal smoking - b663 b665 b667, from dataset b_4f

# G3. f) Did you smoke regularly at any of the following times in the last 9 months?
#                 Before pregnancy   First 3 months of pregnancy     Last 2 weeks
# No
# Yes, cigarettes
# Yes, cigars
# Yes, pipe
# Yes, other (please describe)

# B663 Tobacco smoked REG PRE PREG
# N 1
# Y CIGS 2
# Y cigars 3
# Y pipe 4
# Y other 5

# B665 Tobacco smoked in 1ST 3MTHS of PREG
# N 1
# Y CIGS 2
# Y cigars 3
# Y other 5

# B667 Tobacco smoked in last 2WKS
# N 1
# Y CIGS 2
# Y cigars 3
# Y other 5

table(covars$b663, useNA = "always")
table(covars$b665, useNA = "always")
table(covars$b667, useNA = "always")

## Maternal smoking status prior to pregnancy
covars$matsmoke_b_prior <- ifelse(covars$b663 == 1, 0,  # No
                                  ifelse(covars$b663 > 1, 1,  # Yes
                                         NA))

## Maternal smoking status during pregnancy
covars$matsmoke_b_dur <- ifelse(covars$b665 == 1 & covars$b667 == 1, 0,  # No
                                ifelse(covars$b665 > 1 |
                                         covars$b667 > 1, 1,  # Yes
                                       NA))

table(covars$matsmoke_b_prior, useNA = "always")
table(covars$matsmoke_b_dur, useNA = "always")

################################################################################

# Maternal alcohol consumption - b720 b721 b722, from dataset b_4f

# G9. [YHL G7]
# How often have you drunk alcoholic drinks? Please indicate for each of the following times:
#   Never       Less than       At least       1-2 glasses*       At least       At least
#               1 glass*        1 glass*       every day          3-9 glasses*   10 glasses*
#               a week          a week         every day          every day
# [From HAB 1.8.91 the following footnote was included *By glass we mean a pub measure of spirits, half a pint of lager or cider, a wine glass of wine, etc. This was not included in YHL]
# a) Before this pregnancy
# b) 1st 3 months of this pregnancy
# c) At around the time you first felt the baby move

# B720 Alcohol consumption before this PREG
# never 1
# <1 glass PWK 2
# 1+ glasses PWK 3
# 1-2 glasses PDAY 4
# 3-9 glasses PDAY 5
# 10+ glasses PDAY 6
# Missing -1

# B721 Alcohol consumption in 1-3MTHS this PREG
# never 1
# <1 glass PWK 2
# 1+ glasses PWK 3
# 1-2 glasses PDAY 4
# 3-9 glasses PDAY 5
# 10+ glasses PDAY 6
# Missing -1

# B722 Alcohol consumption C baby 1ST moved
# never 1
# <1 glass PWK 2
# 1+ glasses PWK 3
# 1-2 glasses PDAY 4
# 3-9 glasses PDAY 5
# 10+ glasses PDAY 6
# Missing -1

table(covars$b720, useNA = "always")
table(covars$b721, useNA = "always")
table(covars$b722, useNA = "always")

## Maternal alcohol status prior to pregnancy
covars$matalcohol_b_prior <- ifelse(covars$b720 %in% c(1, 2), 0,  # No (<1 glass per week)
                                    ifelse(covars$b720 > 2, 1,  # Yes (>=1 glass per week)
                                           NA))

## Maternal alcohol status during pregnancy
covars$matalcohol_b_dur <- ifelse(covars$b721 %in% c(1, 2) &
                                    covars$b722 %in% c(1, 2), 0,  # No (<1 glass per week)
                                  ifelse(covars$b721 > 2 |
                                           covars$b722 > 2, 1,  # Yes (>=1 glass per week)
                                         NA))

table(covars$matalcohol_b_prior, useNA = "always")
table(covars$matalcohol_b_dur, useNA = "always")

################################################################################

# Maternal pre-pregnancy body mass index (BMI, in kg/m^2) - dw042, from dataset d_4b
# [Note: Otherwise, you can also use dw002 (pre-pregnancy weight in kg) and dw021 (height in cm) to calculate BMI.]

range(covars$dw042, na.rm = T)

covars$matbmi_kgm2 <- ifelse(covars$dw042 > 0, covars$dw042, NA)

summary(covars$matbmi_kgm2)

################################################################################
# covars_child <- covars %>% filter(singleton==1)

# Offspring sex - kz021, from dataset cp_3a

# Male 1
# Female 2
# Not known -2
# Not enrolled -1

table(covars$kz021, useNA = "always")

covars$sex_b_male <- ifelse(covars$kz021 == 2, 0, # Female
                            ifelse(covars$kz021 == 1, 1, # Male
                                   NA))

table(covars$sex_b_male, useNA = "always")

################################################################################

# [PLEASE CHANCE] Save derived covariate data
saveRDS(covars, "dat_MRPREG_cov.rds")

#------------------------------------------------------------------------------#
#                             Descriptive Results                              #----
#------------------------------------------------------------------------------#

covars_selected <- subset(
  covars,
  select = c(
    matage_yrs,
    mateth_b_other,
    matedu_cat3,
    matedu_b_uni,
    parity_cat5,
    parity_b_multi,
    matsmoke_b_prior,
    matsmoke_b_dur,
    matalcohol_b_prior,
    matalcohol_b_dur,
    matbmi_kgm2,
    sex_b_male
  )
)  # Only select those included in the moba tab

tableone <- as.data.frame(print(CreateTableOne(
  data = covars_selected,
  factorVars = c(
    "mateth_b_other",
    "matedu_cat3",
    "matedu_b_uni",
    "parity_cat5",
    "parity_b_multi",
    "matsmoke_b_prior",
    "matsmoke_b_dur",
    "matalcohol_b_prior",
    "matalcohol_b_dur",
    "sex_b_male"
  ),
  test = F
)))

write.csv(tableone, "MRPREG_ALSPAC_covars_descriptive.csv", row.names = T)
