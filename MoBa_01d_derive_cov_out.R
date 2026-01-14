################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa       #
################################################################################

# Last edited date: 22-Dec-2024
# This script is to derive/modify/rename covariates (including relevant variables of interest) and perinatal outcome variables in MoBa.

################################################################################

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
  openxlsx
)

# Set working directory
setwd("N:/durable/projects/Ran_MoBa_var")

################################################################################

# Load data
dat <- readRDS("dat_exp.rds")
head(dat)
dim(dat)  # 73868  XXX

#------------------------------------------------------------------------------#
#                          Early Pregnancy Covariates                          #----
#------------------------------------------------------------------------------#

# Maternal age
summary(dat$matage_yrs)  # Already derived by Gemma C

dat$age_Mat_con <- as.numeric(dat$matage_yrs)

var_lab(dat$age_Mat_con) = "Maternal age (years) at delivery"

dat$age_Mat_con <-
  ifelse(dat$age_Mat_con > 900, dat$age_Mat_con - 900, dat$age_Mat_con)  # Remove the additional "9" in front of some entries

str(dat$age_Mat_con)
summary(dat$age_Mat_con)

# Ethnicity - !!! Use birth place as a proxy !!!

## Detailed ethnic groups
table(dat$FODELAND_KAT_NOR_GBD, useNA = "always")

dat <- dat %>%
  mutate(
    ethnic_Mat_cat = case_when(
      FODELAND_KAT_NOR_GBD %in% c(201, 202, 203) ~ "European",
      FODELAND_KAT_NOR_GBD %in% c(204, 205) ~ "African and Middle Eastern",
      FODELAND_KAT_NOR_GBD %in% c(206, 207) ~ "Asian and Oceanian",
      FODELAND_KAT_NOR_GBD == 208 ~ "Latin American and Caribbean",
      TRUE ~ NA_character_
    )
  )

dat$ethnic_Mat_cat <- factor(
  dat$ethnic_Mat_cat,
  levels = c(
    "European",
    "African and Middle Eastern",
    "Asian and Oceanian",
    "Latin American and Caribbean"
  )
)

var_lab(dat$ethnic_Mat_cat) = "Maternal ethnicity (detailed)"

str(dat$ethnic_Mat_cat)
CrossTable(dat$ethnic_Mat_cat)

## Binary - European vs. Non-European
table(dat$mateth_b_other, useNA = "always")  # Already derived by Gemma C

dat$ethnic_Mat_bin <- as.character(dat$mateth_b_other)

dat$ethnic_Mat_bin[which(dat$ethnic_Mat_bin == "0")] <- "European"
dat$ethnic_Mat_bin[which(dat$ethnic_Mat_bin == "1")] <-
  "Non-European"

dat$ethnic_Mat_bin <- factor(dat$ethnic_Mat_bin,
                             levels = c("European", "Non-European"))

var_lab(dat$ethnic_Mat_bin) = "Maternal ethnicity (binary)"

str(dat$ethnic_Mat_bin)
CrossTable(dat$ethnic_Mat_bin)

# Education level

## Ordinal / 3 categories (from Torjusen et al., 2010, BMC Public Health)
dat$edu_Mat_3cat <- as.character(dat$AA1124)

dat$edu_Mat_3cat[dat$edu_Mat_3cat %in% c("1", "2", "3", "4")] <-
  "Low"  # <10 y - 12 y: 1) 9-year secondary school & 2) 1-2 year high school & 3) Vocational high school & 4) 3-year high school general studies, junior college
dat$edu_Mat_3cat[dat$edu_Mat_3cat == "5"] <-
  "Medium"  # 13-16 y: 5) Regional technical college, 4-year university degree (Bachelor’s degree, nurse, teacher, engineer)
dat$edu_Mat_3cat[dat$edu_Mat_3cat == "6"] <-
  "High"  # 17+ y: 6) University, technical college, more than 4 years (Master’s degree, medical doctor, PhD)

dat$edu_Mat_3cat <-
  factor(dat$edu_Mat_3cat,
         levels = c("Low", "Medium", "High"),
         ordered = T)  # Set as ordinal variable

var_lab(dat$edu_Mat_3cat) = "Maternal education attainment (3 categories)"

str(dat$edu_Mat_3cat)
CrossTable(dat$edu_Mat_3cat)

## Binary
table(dat$matedu_b_uni, useNA = "always")  # Already derived by Gemma C

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

# Household income
table(dat$AA1315, useNA = "always")  # Maternal income
table(dat$AA1316, useNA = "always")  # Paternal income (reported by mothers)
table(dat$FF341, useNA = "always")  # Paternal income (self-reported by fathers)

# 1) No income
# 2) Under 150.000 NOK
# 3) 151.000-199.999 NOK
# 4) 200.000-299.999 NOK
# 5) 300.000-399.999 NOK
# 6) 400.000-499.999 NOK
# 7) Over 500.000 NOK

dat$AA1316[!is.na(dat$AA1316) &
             dat$AA1316 == 8] <-
  dat$FF341[!is.na(dat$AA1316) &
              dat$AA1316 == 8]  # Replace mother-reported "8) Don't know" with father-reported values

## 3 categories (from Torjusen et al., 2010, BMC Public Health)
dat <-
  dat %>% mutate(
    income_Fam_3cat = case_when(
      AA1315 %in% 1:4 & AA1316 %in% 1:4 ~ 0,
      # Low (both <NOK 300 000)
      (AA1315 %in% 5:7 &
         AA1316 %in% 1:4) |
        (AA1315 %in% 1:4 &
           AA1316 %in% 5:7) ~ 1,
      # Medium (one ≥NOK 300 000)
      AA1315 %in% 5:7 &
        AA1316 %in% 5:7 ~ 2,
      # High (both ≥NOK 300 000)
      TRUE ~ NA_real_
    )
  )

val_lab(dat$income_Fam_3cat) = c("Low" = 0,
                                 "Medium" = 1,
                                 "High" = 2)

dat$income_Fam_3cat <-
  factor(
    dat$income_Fam_3cat,
    levels = c("Low", "Medium", "High"),
    ordered = T
  )  # Set as ordinal variable

var_lab(dat$income_Fam_3cat) = "Household income (3 categories)"

str(dat$income_Fam_3cat)
CrossTable(dat$income_Fam_3cat)
table(dat$income_Fam_3cat, useNA = "always")

## Binary (High vs. Low + Medium)
dat <-
  dat %>% mutate(
    income_Fam_bin = case_when(
      income_Fam_3cat %in% c("Low", "Medium") ~ 0,
      # Lower (Low + Medium)
      income_Fam_3cat == "High" ~ 1,
      # Higher (High)
      TRUE ~ NA_real_
    )
  )

val_lab(dat$income_Fam_bin) = c("Lower" = 0,
                                "Higher" = 1)

dat$income_Fam_bin <-
  as.factor(dat$income_Fam_bin)

var_lab(dat$income_Fam_bin) = "Household income (binary)"

str(dat$income_Fam_bin)
CrossTable(dat$income_Fam_bin)
table(dat$income_Fam_bin, useNA = "always")

################################################################################
# Being a student - !!! Additionally added as a potential SEP indicator !!!

## Maternal
table(dat$AA1132, useNA = "always")

dat$student_Mat_bin <- 0
dat$student_Mat_bin[dat$AA1132 == 1] <- 1

val_lab(dat$student_Mat_bin) = c("No" = 0,
                                 "Yes" = 1)

dat$student_Mat_bin <-
  as.factor(dat$student_Mat_bin)

var_lab(dat$student_Mat_bin) = "Being a student (binary)"

str(dat$student_Mat_bin)
CrossTable(dat$student_Mat_bin)
table(dat$student_Mat_bin, useNA = "always")

## Paternal (reported by the mother)
table(dat$AA1133, useNA = "always")

dat$student_Pat_bin <- 0
dat$student_Pat_bin[dat$AA1133 == 1] <- 1

val_lab(dat$student_Pat_bin) = c("No" = 0,
                                 "Yes" = 1)

dat$student_Pat_bin <-
  as.factor(dat$student_Pat_bin)

var_lab(dat$student_Pat_bin) = "Being a student (binary)"

str(dat$student_Pat_bin)
CrossTable(dat$student_Pat_bin)
table(dat$student_Pat_bin, useNA = "always")
################################################################################

# Parity
dat$parity_Mat_bin <- as.character(dat$parity_b_multi)

dat$parity_Mat_bin[which(dat$parity_Mat_bin == "0")] <-
  "0"
dat$parity_Mat_bin[which(dat$parity_Mat_bin == "1")] <-
  ">=1"
dat$parity_Mat_bin <- factor(dat$parity_Mat_bin,
                             levels = c("0", ">=1"))

var_lab(dat$parity_Mat_bin) = "Maternal parity (binary)"

str(dat$parity_Mat_bin)
CrossTable(dat$parity_Mat_bin)

# Pre-pregnancy BMI
dat$BMI_Mat_PRE.p_con <- as.numeric(dat$matbmi_kgm2)

var_lab(dat$BMI_Mat_PRE.p_con) = "Maternal pre-pregnancy BMI (kg/m^2)"

str(dat$BMI_Mat_PRE.p_con)
summary(dat$BMI_Mat_PRE.p_con)

# Smoking - Collected before or up to the time of dietary measurement (MoBa Q2: 17-22 weeks)

## Already derived by Gemma C
table(dat$matsmoke_b_early, useNA = "always")  # Derived from AA1356
table(dat$matsmoke_b_during, useNA = "always")  # Derived from CC1037

## Derive using variables collected at MoBa Q1 (15 weeks) (smoking not available in Q2)

### 3 categories - Never/Former/Current
dat <-
  dat %>% mutate(
    smoking_Mat_EAR.p_3cat = case_when(
      AA1355 == 1 & AA1356 == 1 ~ 0,
      AA1355 == 2 & AA1356 == 1 ~ 1,
      AA1356 == 2 | AA1356 == 3 ~ 2,
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

### Binary - Yes vs. No (in early pregnancy)
dat <-
  dat %>% mutate(smoking_Mat_EAR.p_bin = case_when(AA1356 == 1 ~ 0,
                                                   AA1356 == 2 |
                                                     AA1356 == 3 ~ 1,
                                                   TRUE ~ NA_real_))

val_lab(dat$smoking_Mat_EAR.p_bin) = c("No" = 0,
                                       "Yes" = 1)

dat$smoking_Mat_EAR.p_bin <-
  as.factor(dat$smoking_Mat_EAR.p_bin)

var_lab(dat$smoking_Mat_EAR.p_bin) = "Maternal smoking in early pregnancy (binary)"

str(dat$smoking_Mat_EAR.p_bin)
CrossTable(dat$smoking_Mat_EAR.p_bin)

# Alcohol drinking (Yes vs. No) - Collected before or up to the time of dietary measurement (MoBa Q2: 17-22 weeks)

## Already derived by Gemma C
table(dat$mat_alcohol_1, useNA = "always")  # Derived from CC1157
table(dat$mat_alcohol_2, useNA = "always")  # Derived from CC1158
table(dat$mat_alcohol_3, useNA = "always")  # Derived from CC1159
table(dat$mat_alcohol_any, useNA = "always")  # Derived from all above

## Derive using variables collected at MoBa Q1 (15 weeks) (alcohol consumption measured at Q2 (FFQ) not combinable with Q1)
## [NOTE: CC1157 at Q3 also asked about alcohol consumption during 0-12 weeks but is not used here due to possible recall bias.
##        Alcohol drinking - "No" defined as < 1 time/week; "Yes" defined as >=1 time/week.]
dat <-
  dat %>% mutate(alcohol_Mat_EAR.p_bin = case_when(AA1454 %in% 5:7 ~ 0,
                                                   AA1454 %in% 1:4 ~ 1,
                                                   TRUE ~ NA_real_))

dat$alcohol_Mat_EAR.p_bin[is.na(dat$alcohol_Mat_EAR.p_bin)] <-
  dat$mat_alcohol_1[is.na(dat$alcohol_Mat_EAR.p_bin)]  # Replace Q1 missing values with Q3 measurements (for 0-12 weeks)

val_lab(dat$alcohol_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$alcohol_Mat_EAR.p_bin <-
  as.factor(dat$alcohol_Mat_EAR.p_bin)

var_lab(dat$alcohol_Mat_EAR.p_bin) = "Maternal alcohol in early pregnancy (binary)"

str(dat$alcohol_Mat_EAR.p_bin)
CrossTable(dat$alcohol_Mat_EAR.p_bin)

# Physical activity - Derive using variables collected at MoBa Q1 (15 weeks) (physical activity not available in Q2)

## Calculate physical activity frequency (times/week)

## 1) Never
## 2) 1-3 times a month
## 3) Once a week
## 4) Twice a week
## 5) 3 times or more a week

phys.act_freq_map <- c(
  "1" = 0,
  "2" = 0.5,
  "3" = 1,
  "4" = 2,
  "5" = 3
)

var_list <- paste0("AA", c(seq(1490, 1516, 2)))

for (var in var_list) {
  dat[[var]] <-
    phys.act_freq_map[as.character(as.numeric(dat[[var]]))]
}

## Derive continuous physical activity variable (times/week)
dat$phys.act_Mat_EAR.p_con <-
  rowSums(dat[, paste0("AA", c(seq(1490, 1516, 2)))], na.rm = T)

var_lab(dat$phys.act_Mat_EAR.p_con) = "Maternal physical activity in early pregnancy (times/week)"

str(dat$phys.act_Mat_EAR.p_con)
summary(dat$phys.act_Mat_EAR.p_con)

## Derive binary physical activity variable (based on distribution; NOT from Torjusen et al., 2010, BMC Public Health)
dat <-
  dat %>% mutate(
    phys.act_Mat_EAR.p_bin = case_when(
      phys.act_Mat_EAR.p_con < 3 ~ 0,
      # Less than 3 times a week
      phys.act_Mat_EAR.p_con >= 3 ~ 1,
      # 3 times or more a week
      TRUE ~ NA_real_
    )
  )
val_lab(dat$phys.act_Mat_EAR.p_bin) = c("Less than 3 times a week" = 0,
                                        "3 times or more a week" = 1)

dat$phys.act_Mat_EAR.p_bin <-
  as.factor(dat$phys.act_Mat_EAR.p_bin)

var_lab(dat$phys.act_Mat_EAR.p_bin) = "Maternal physical activity in early pregnancy (binary)"

str(dat$phys.act_Mat_EAR.p_bin)
CrossTable(dat$phys.act_Mat_EAR.p_bin)

# Dietary supplement use  (before or up to dietary measurement) - Iron, Vitamin B12, vitamin D, calcium, iodine, any supplements, and any non-iron supplements

# MoBa Q1:
# During this pregnancy
# In pregnancy weeks
# 0-4 5-8 9-12 13+
# 1 as yes and missing if not used (from Anne-Lise)

## Iron
dat$iron.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("AA1039", "AA1040", "AA1041", "AA1042")], na.rm = T)

dat <-
  dat %>% mutate(
    iron.supp_Mat_EAR.p_bin = case_when(
      iron.supp_Mat_EAR.p_bin == 0 ~ 0,
      iron.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$iron.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$iron.supp_Mat_EAR.p_bin <-
  as.factor(dat$iron.supp_Mat_EAR.p_bin)

var_lab(dat$iron.supp_Mat_EAR.p_bin) = "Maternal iron supplement use in early pregnancy (binary)"

str(dat$iron.supp_Mat_EAR.p_bin)
table(dat$iron.supp_Mat_EAR.p_bin, useNA = "always")

################################################################################
### Iron - Duration (within those with iron supplementation in early pregnancy)
dat <-
  dat %>% mutate(
    iron.supp.dur_Mat_EAR.p_cat = case_when(
      iron.supp_Mat_EAR.p_bin == "No" ~ 0,
      iron.supp_Mat_EAR.p_bin == "Yes" &
        is.na(AA1036) &
        is.na(AA1037) &
        is.na(AA1038) &
        is.na(AA1039) &
        is.na(AA1040) &
        is.na(AA1041) &
        (AA1042 == 1) ~ 1,
      iron.supp_Mat_EAR.p_bin == "Yes" &
        is.na(AA1036) &
        is.na(AA1037) &
        is.na(AA1038) &
        ((AA1039 == 1) |
           (AA1040 == 1) |
           (AA1041 == 1)) ~ 2,
      iron.supp_Mat_EAR.p_bin == "Yes" &
        ((AA1036 == 1) |
           (AA1037 == 1) | (AA1038 == 1)) ~ 3,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$iron.supp.dur_Mat_EAR.p_cat) = c(
  "No use" = 0,
  "Started after 1st trimester" = 1,
  "Started in 1st trimester" = 2,
  "Started before pregnancy" = 3
)

dat$iron.supp.dur_Mat_EAR.p_cat <-
  as.factor(dat$iron.supp.dur_Mat_EAR.p_cat)

var_lab(dat$iron.supp.dur_Mat_EAR.p_cat) = "Maternal iron supplement use duration in early pregnancy (categorical)"

str(dat$iron.supp.dur_Mat_EAR.p_cat)
table(dat$iron.supp.dur_Mat_EAR.p_cat, useNA = "always")

### Iron - Frequency (within those with iron supplementation in early pregnancy)
dat <-
  dat %>% mutate(
    iron.supp.freq_Mat_EAR.p_cat = case_when(
      iron.supp_Mat_EAR.p_bin == "No" ~ 0,
      iron.supp_Mat_EAR.p_bin == "Yes" & AA1043 > 1 ~ 1,
      iron.supp_Mat_EAR.p_bin == "Yes" & AA1043 == 1 ~ 2,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$iron.supp.freq_Mat_EAR.p_cat) = c("No use" = 0,
                                              "Less than daily" = 1,
                                              "Daily" = 2)

dat$iron.supp.freq_Mat_EAR.p_cat <-
  as.factor(dat$iron.supp.freq_Mat_EAR.p_cat)

var_lab(dat$iron.supp.freq_Mat_EAR.p_cat) = "Maternal iron supplement use frequency in early pregnancy (categorical)"

str(dat$iron.supp.freq_Mat_EAR.p_cat)
table(dat$iron.supp.freq_Mat_EAR.p_cat, useNA = "always")

################################################################################
## Any supplements (excluding iron)
dat$NON.iron.supp_Mat_EAR.p_bin <-
  rowSums(dat[, paste0("AA", setdiff(c(
    seq(943, 1111, 8),
    seq(944, 1112, 8),
    seq(945, 1113, 8),
    seq(946, 1114, 8)
  ), c(1039:1042)))], na.rm = T)

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
table(dat$NON.iron.supp_Mat_EAR.p_bin, useNA = "always")
################################################################################

## Vitamin B12
dat$vitB12.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("AA975", "AA976", "AA977", "AA978")], na.rm = T)

dat <-
  dat %>% mutate(
    vitB12.supp_Mat_EAR.p_bin = case_when(
      vitB12.supp_Mat_EAR.p_bin == 0 ~ 0,
      vitB12.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$vitB12.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$vitB12.supp_Mat_EAR.p_bin <-
  as.factor(dat$vitB12.supp_Mat_EAR.p_bin)

var_lab(dat$vitB12.supp_Mat_EAR.p_bin) = "Maternal vitamin B12 supplement use in early pregnancy (binary)"

str(dat$vitB12.supp_Mat_EAR.p_bin)
CrossTable(dat$vitB12.supp_Mat_EAR.p_bin)
################################################################################
### Any supplements (excluding vitamin B12)
dat$NON.vitB12.supp_Mat_EAR.p_bin <-
  rowSums(dat[, paste0("AA", setdiff(c(
    seq(943, 1111, 8),
    seq(944, 1112, 8),
    seq(945, 1113, 8),
    seq(946, 1114, 8)
  ), c(975:978)))], na.rm = T)

dat <-
  dat %>% mutate(
    NON.vitB12.supp_Mat_EAR.p_bin = case_when(
      NON.vitB12.supp_Mat_EAR.p_bin == 0 ~ 0,
      NON.vitB12.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$NON.vitB12.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.vitB12.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.vitB12.supp_Mat_EAR.p_bin)

var_lab(dat$NON.vitB12.supp_Mat_EAR.p_bin) = "Maternal any non-vitamin B12 supplement use in early pregnancy (binary)"

str(dat$NON.vitB12.supp_Mat_EAR.p_bin)
CrossTable(dat$NON.vitB12.supp_Mat_EAR.p_bin)
################################################################################

## Vitamin D
dat$vitD.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("AA1023", "AA1024", "AA1025", "AA1026")], na.rm = T)

dat <-
  dat %>% mutate(
    vitD.supp_Mat_EAR.p_bin = case_when(
      vitD.supp_Mat_EAR.p_bin == 0 ~ 0,
      vitD.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$vitD.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$vitD.supp_Mat_EAR.p_bin <-
  as.factor(dat$vitD.supp_Mat_EAR.p_bin)

var_lab(dat$vitD.supp_Mat_EAR.p_bin) = "Maternal vitamin D supplement use in early pregnancy (binary)"

str(dat$vitD.supp_Mat_EAR.p_bin)
CrossTable(dat$vitD.supp_Mat_EAR.p_bin)
################################################################################
## Any supplements (excluding vitamin D)
dat$NON.vitD.supp_Mat_EAR.p_bin <-
  rowSums(dat[, paste0("AA", setdiff(c(
    seq(943, 1111, 8),
    seq(944, 1112, 8),
    seq(945, 1113, 8),
    seq(946, 1114, 8)
  ), c(1023:1026)))], na.rm = T)

dat <-
  dat %>% mutate(
    NON.vitD.supp_Mat_EAR.p_bin = case_when(
      NON.vitD.supp_Mat_EAR.p_bin == 0 ~ 0,
      NON.vitD.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$NON.vitD.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.vitD.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.vitD.supp_Mat_EAR.p_bin)

var_lab(dat$NON.vitD.supp_Mat_EAR.p_bin) = "Maternal any non-vitamin D supplement use in early pregnancy (binary)"

str(dat$NON.vitD.supp_Mat_EAR.p_bin)
CrossTable(dat$NON.vitD.supp_Mat_EAR.p_bin)
################################################################################

## Calcium
dat$calcium.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("AA1047", "AA1048", "AA1049", "AA1050")], na.rm = T)

dat <-
  dat %>% mutate(
    calcium.supp_Mat_EAR.p_bin = case_when(
      calcium.supp_Mat_EAR.p_bin == 0 ~ 0,
      calcium.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$calcium.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$calcium.supp_Mat_EAR.p_bin <-
  as.factor(dat$calcium.supp_Mat_EAR.p_bin)

var_lab(dat$calcium.supp_Mat_EAR.p_bin) = "Maternal calcium supplement use in early pregnancy (binary)"

str(dat$calcium.supp_Mat_EAR.p_bin)
CrossTable(dat$calcium.supp_Mat_EAR.p_bin)
################################################################################
## Any supplements (excluding calcium)
dat$NON.calcium.supp_Mat_EAR.p_bin <-
  rowSums(dat[, paste0("AA", setdiff(c(
    seq(943, 1111, 8),
    seq(944, 1112, 8),
    seq(945, 1113, 8),
    seq(946, 1114, 8)
  ), c(1047:1050)))], na.rm = T)

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

## Iodine
dat$iodine.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("AA1055", "AA1056", "AA1057", "AA1058")], na.rm = T)

dat <-
  dat %>% mutate(
    iodine.supp_Mat_EAR.p_bin = case_when(
      iodine.supp_Mat_EAR.p_bin == 0 ~ 0,
      iodine.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$iodine.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$iodine.supp_Mat_EAR.p_bin <-
  as.factor(dat$iodine.supp_Mat_EAR.p_bin)

var_lab(dat$iodine.supp_Mat_EAR.p_bin) = "Maternal iodine supplement use in early pregnancy (binary)"

str(dat$iodine.supp_Mat_EAR.p_bin)
CrossTable(dat$iodine.supp_Mat_EAR.p_bin)
################################################################################
## Any supplements (excluding iodine)
dat$NON.iodine.supp_Mat_EAR.p_bin <-
  rowSums(dat[, paste0("AA", setdiff(c(
    seq(943, 1111, 8),
    seq(944, 1112, 8),
    seq(945, 1113, 8),
    seq(946, 1114, 8)
  ), c(1055:1058)))], na.rm = T)

dat <-
  dat %>% mutate(
    NON.iodine.supp_Mat_EAR.p_bin = case_when(
      NON.iodine.supp_Mat_EAR.p_bin == 0 ~ 0,
      NON.iodine.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$NON.iodine.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.iodine.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.iodine.supp_Mat_EAR.p_bin)

var_lab(dat$NON.iodine.supp_Mat_EAR.p_bin) = "Maternal any non-iodine supplement use in early pregnancy (binary)"

str(dat$NON.iodine.supp_Mat_EAR.p_bin)
CrossTable(dat$NON.iodine.supp_Mat_EAR.p_bin)

################################################################################

## Folic acid
dat$folate.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("AA943", "AA944", "AA945", "AA946")], na.rm = T)

dat <-
  dat %>% mutate(
    folate.supp_Mat_EAR.p_bin = case_when(
      folate.supp_Mat_EAR.p_bin == 0 ~ 0,
      folate.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$folate.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$folate.supp_Mat_EAR.p_bin <-
  as.factor(dat$folate.supp_Mat_EAR.p_bin)

var_lab(dat$folate.supp_Mat_EAR.p_bin) = "Maternal folic acid supplement use in early pregnancy (binary)"

str(dat$folate.supp_Mat_EAR.p_bin)
CrossTable(dat$folate.supp_Mat_EAR.p_bin)
################################################################################
## Any supplements (excluding folic acid)
dat$NON.folate.supp_Mat_EAR.p_bin <-
  rowSums(dat[, paste0("AA", setdiff(c(
    seq(943, 1111, 8),
    seq(944, 1112, 8),
    seq(945, 1113, 8),
    seq(946, 1114, 8)
  ), c(943:946)))], na.rm = T)

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

## Omega-3 fatty acid
dat$omega3FA.supp_Mat_EAR.p_bin <-
  rowSums(dat[, c("AA1111", "AA1112", "AA1113", "AA1114")], na.rm = T)

dat <-
  dat %>% mutate(
    omega3FA.supp_Mat_EAR.p_bin = case_when(
      omega3FA.supp_Mat_EAR.p_bin == 0 ~ 0,
      omega3FA.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$omega3FA.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$omega3FA.supp_Mat_EAR.p_bin <-
  as.factor(dat$omega3FA.supp_Mat_EAR.p_bin)

var_lab(dat$omega3FA.supp_Mat_EAR.p_bin) = "Maternal Omega-3 fatty acid supplement use in early pregnancy (binary)"

str(dat$omega3FA.supp_Mat_EAR.p_bin)
CrossTable(dat$omega3FA.supp_Mat_EAR.p_bin)
################################################################################
## Any supplements (excluding Omega-3 fatty acid)
dat$NON.omega3FA.supp_Mat_EAR.p_bin <-
  rowSums(dat[, paste0("AA", setdiff(c(
    seq(943, 1111, 8),
    seq(944, 1112, 8),
    seq(945, 1113, 8),
    seq(946, 1114, 8)
  ), c(1111:1114)))], na.rm = T)

dat <-
  dat %>% mutate(
    NON.omega3FA.supp_Mat_EAR.p_bin = case_when(
      NON.omega3FA.supp_Mat_EAR.p_bin == 0 ~ 0,
      NON.omega3FA.supp_Mat_EAR.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$NON.omega3FA.supp_Mat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$NON.omega3FA.supp_Mat_EAR.p_bin <-
  as.factor(dat$NON.omega3FA.supp_Mat_EAR.p_bin)

var_lab(dat$NON.omega3FA.supp_Mat_EAR.p_bin) = "Maternal any non-Omega-3 fatty acid supplement use in early pregnancy (binary)"

str(dat$NON.omega3FA.supp_Mat_EAR.p_bin)
CrossTable(dat$NON.omega3FA.supp_Mat_EAR.p_bin)

################################################################################

## Any supplements
dat$any.supp_Mat_EAR.p_bin <-
  rowSums(dat[, paste0("AA", c(
    seq(943, 1111, 8),
    seq(944, 1112, 8),
    seq(945, 1113, 8),
    seq(946, 1114, 8)
  ))], na.rm = T)

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
#                 Mid-Pregnancy Covariates (Same Time as FFQ)                  #----
#------------------------------------------------------------------------------#

# Self-defined vegetarianism

## 4 categories - Non-vegetarian, Pesco-vegetarian, Lacto-ovo-vegetarian, Vegan
table(dat$BB15)

dat <-
  dat %>% mutate(
    self.VegDiet_Mat_DUR.p_4cat = case_when(
      BB15 %in% c(1, 3) ~ 0,
      BB15 == 2 ~ 1,
      BB15 %in% c(4, 5) ~ 2,
      BB15 == 6 ~ 3,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$self.VegDiet_Mat_DUR.p_4cat) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Lacto-ovo-vegetarian" = 2,
  "Vegan" = 3
)

dat$self.VegDiet_Mat_DUR.p_4cat <-
  as.factor(dat$self.VegDiet_Mat_DUR.p_4cat)

var_lab(dat$self.VegDiet_Mat_DUR.p_4cat) = "Maternal self-defined vegetarianism during pregnancy (4 categories)"

str(dat$self.VegDiet_Mat_DUR.p_4cat)
CrossTable(dat$self.VegDiet_Mat_DUR.p_4cat)

## 3 categories - Non-vegetarian, Pesco-vegetarian, Full vegetarian
dat <-
  dat %>% mutate(
    self.VegDiet_Mat_DUR.p_3cat = case_when(
      self.VegDiet_Mat_DUR.p_4cat == "Non-vegetarian" ~ 0,
      self.VegDiet_Mat_DUR.p_4cat == "Pesco-vegetarian" ~ 1,
      self.VegDiet_Mat_DUR.p_4cat == "Lacto-ovo-vegetarian" |
        self.VegDiet_Mat_DUR.p_4cat == "Vegan" ~ 2,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$self.VegDiet_Mat_DUR.p_3cat) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)

dat$self.VegDiet_Mat_DUR.p_3cat <-
  as.factor(dat$self.VegDiet_Mat_DUR.p_3cat)

var_lab(dat$self.VegDiet_Mat_DUR.p_3cat) = "Maternal self-defined vegetarianism during pregnancy (3 categories)"

str(dat$self.VegDiet_Mat_DUR.p_3cat)
CrossTable(dat$self.VegDiet_Mat_DUR.p_3cat)

## Binary - Non-vegetarian, Pesco-/Full vegetarian
dat <-
  dat %>% mutate(
    self.VegDiet_Mat_DUR.p_bin = case_when(
      self.VegDiet_Mat_DUR.p_4cat == "Non-vegetarian" ~ 0,
      self.VegDiet_Mat_DUR.p_4cat == "Pesco-vegetarian" |
        self.VegDiet_Mat_DUR.p_4cat == "Lacto-ovo-vegetarian" |
        self.VegDiet_Mat_DUR.p_4cat == "Vegan" ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$self.VegDiet_Mat_DUR.p_bin) = c("Non-vegetarian" = 0,
                                            "Vegetarian" = 1)

dat$self.VegDiet_Mat_DUR.p_bin <-
  as.factor(dat$self.VegDiet_Mat_DUR.p_bin)

var_lab(dat$self.VegDiet_Mat_DUR.p_bin) = "Maternal self-defined vegetarianism during pregnancy (binary)"

str(dat$self.VegDiet_Mat_DUR.p_bin)
CrossTable(dat$self.VegDiet_Mat_DUR.p_bin)

################################################################################
# !!! Diet-based + self-defined vegetarianism !!!

## 3-category
dat <-
  dat %>% mutate(
    VegDiet_3cat_FFQ_self = case_when(
      VegDiet_3cat == "Non-vegetarian" &
        self.VegDiet_Mat_DUR.p_3cat == "Non-vegetarian" ~ 0,
      VegDiet_3cat == "Pesco-vegetarian" &
        self.VegDiet_Mat_DUR.p_3cat == "Pesco-vegetarian" ~ 1,
      VegDiet_3cat == "Full vegetarian" &
        self.VegDiet_Mat_DUR.p_3cat == "Full vegetarian" ~ 2,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$VegDiet_3cat_FFQ_self) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)
dat$VegDiet_3cat_FFQ_self <-
  as.factor(dat$VegDiet_3cat_FFQ_self)
var_lab(dat$VegDiet_3cat_FFQ_self) <-
  "Diet-based + self-defined vegetarianism (3 categories) during pregnancy"

table(dat$VegDiet_3cat_FFQ_self, useNA = "always")

## Binary
dat <- dat %>% mutate(
  VegDiet_bin_FFQ_self = case_when(
    VegDiet_3cat_FFQ_self == "Non-vegetarian" ~ 0,
    VegDiet_3cat_FFQ_self %in% c("Pesco-vegetarian", "Full vegetarian") ~ 1,
    TRUE ~ NA_real_
  )
)

val_lab(dat$VegDiet_bin_FFQ_self) = c("Non-vegetarian" = 0, "Vegetarian" = 1)

dat$VegDiet_bin_FFQ_self <-
  as.factor(dat$VegDiet_bin_FFQ_self)
var_lab(dat$VegDiet_bin_FFQ_self) <-
  "Diet-based + self-defined vegetarianism (binary) during pregnancy"

table(dat$VegDiet_bin_FFQ_self, useNA = "always")
################################################################################

# Daily nutrient intakes

## Load label list
nutrient_labels <-
  read.xlsx("nutrient_varlab.xlsx",
            sheet = "Sheet1")
nutrient_labels
str(nutrient_labels)  # 34 nutrients

## Assign label to each variable
for (var_name in nutrient_labels$varname) {
  var_lab(dat[, var_name]) <-
    paste0(nutrient_labels$label[which(nutrient_labels$varname == var_name)],
           " (",
           nutrient_labels$unit[which(nutrient_labels$varname == var_name)],
           ")")
}

## Sanity check
head(dat[, nutrient_labels$varname])

# Total energy intake (kcal/day)
dat$energy_Mat_DUR.p_con <- as.numeric(dat$KCAL)
dat$energy_Mat_DUR.p_con[dat$energy_Mat_DUR.p_con < 1075.5 |
                           dat$energy_Mat_DUR.p_con > 4780] <-
  NA  # 4.5-20 MJ/day (equivalent to 1,075.5-4,780 kcal/day) (ref: https://onlinelibrary.wiley.com/doi/10.1111/j.1740-8709.2007.00104.x).

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

#------------------------------------------------------------------------------#
#                          Late Pregnancy Covariates                           #----
#------------------------------------------------------------------------------#

# Smoking
table(dat$CC1037, useNA = "always")

dat <-
  dat %>% mutate(smoking_Mat_LAT.p_bin = case_when(CC1037 == 1 ~ 0,
                                                   CC1037 > 1 ~ 1,
                                                   TRUE ~ NA_real_))

val_lab(dat$smoking_Mat_LAT.p_bin) = c("No" = 0,
                                       "Yes" = 1)

dat$smoking_Mat_LAT.p_bin <-
  as.factor(dat$smoking_Mat_LAT.p_bin)

var_lab(dat$smoking_Mat_LAT.p_bin) = "Maternal smoking in late pregnancy (binary)"

str(dat$smoking_Mat_LAT.p_bin)
CrossTable(dat$smoking_Mat_LAT.p_bin)

# Alcohol drinking - Already derived by Gemma C
table(dat$mat_alcohol_1, useNA = "always")  # Derived from CC1157
table(dat$mat_alcohol_2, useNA = "always")  # Derived from CC1158
table(dat$mat_alcohol_3, useNA = "always")  # Derived from CC1159
table(dat$mat_alcohol_any, useNA = "always")  # Derived from all above

dat$alcohol_Mat_MID.p_bin <- dat$mat_alcohol_2
val_lab(dat$alcohol_Mat_MID.p_bin) = c("No" = 0,
                                       "Yes" = 1)
dat$alcohol_Mat_MID.p_bin <-
  as.factor(dat$alcohol_Mat_MID.p_bin)
var_lab(dat$alcohol_Mat_MID.p_bin) = "Maternal alcohol drinking in mid-pregnancy (binary)"
str(dat$alcohol_Mat_MID.p_bin)
CrossTable(dat$alcohol_Mat_MID.p_bin)

dat$alcohol_Mat_LAT.p_bin <- dat$mat_alcohol_3
val_lab(dat$alcohol_Mat_LAT.p_bin) = c("No" = 0,
                                       "Yes" = 1)
dat$alcohol_Mat_LAT.p_bin <-
  as.factor(dat$alcohol_Mat_LAT.p_bin)
var_lab(dat$alcohol_Mat_LAT.p_bin) = "Maternal alcohol drinking in late pregnancy (binary)"
str(dat$alcohol_Mat_LAT.p_bin)
CrossTable(dat$alcohol_Mat_LAT.p_bin)

# Physical activity - Derived in the same way as in early pregnancy
var_list <- paste0("CC", 984:997)

for (var in var_list) {
  dat[[var]] <-
    phys.act_freq_map[as.character(as.numeric(dat[[var]]))]
}

## Derive continuous physical activity variable (times/week)
dat$phys.act_Mat_LAT.p_con <-
  rowSums(dat[, paste0("CC", 984:997)], na.rm = T)

var_lab(dat$phys.act_Mat_LAT.p_con) = "Maternal physical activity in late pregnancy (times/week)"

str(dat$phys.act_Mat_LAT.p_con)
summary(dat$phys.act_Mat_LAT.p_con)

## Derive binary physical activity variable (based on distribution; NOT from Torjusen et al., 2010, BMC Public Health)
dat <-
  dat %>% mutate(
    phys.act_Mat_LAT.p_bin = case_when(
      phys.act_Mat_LAT.p_con < 3 ~ 0,
      # Less than 3 times a week
      phys.act_Mat_LAT.p_con >= 3 ~ 1,
      # 3 times or more a week
      TRUE ~ NA_real_
    )
  )
val_lab(dat$phys.act_Mat_LAT.p_bin) = c("Less than 3 times a week" = 0,
                                        "3 times or more a week" = 1)

dat$phys.act_Mat_LAT.p_bin <-
  as.factor(dat$phys.act_Mat_LAT.p_bin)

var_lab(dat$phys.act_Mat_LAT.p_bin) = "Maternal physical activity in late pregnancy (binary)"

str(dat$phys.act_Mat_LAT.p_bin)
CrossTable(dat$phys.act_Mat_LAT.p_bin)

# Dietary supplementation (from mid- to late pregnancy)
dat$any.supp_Mat_MID.LAT.p_bin <-
  rowSums(dat[, paste0("CC", c(
    seq(771, 897, 6),
    seq(772, 898, 6),
    seq(773, 899, 6),
    seq(774, 900, 6),
    seq(775, 901, 6)
  ))], na.rm = T)

dat <-
  dat %>% mutate(
    any.supp_Mat_MID.LAT.p_bin = case_when(
      any.supp_Mat_MID.LAT.p_bin == 0 ~ 0,
      any.supp_Mat_MID.LAT.p_bin > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$any.supp_Mat_MID.LAT.p_bin) = c("No" = 0, "Yes" = 1)

dat$any.supp_Mat_MID.LAT.p_bin <-
  as.factor(dat$any.supp_Mat_MID.LAT.p_bin)

var_lab(dat$any.supp_Mat_MID.LAT.p_bin) = "Maternal any supplement use in mid-to-late pregnancy (binary)"

str(dat$any.supp_Mat_MID.LAT.p_bin)
CrossTable(dat$any.supp_Mat_MID.LAT.p_bin)

# Offspring sex
dat <-
  dat %>% mutate(sex_Chi_bin = case_when(sex_b_male == 0 ~ 1,
                                         sex_b_male == 1 ~ 0,
                                         TRUE ~ NA_real_))
val_lab(dat$sex_Chi_bin) = c("Male" = 0, "Female" = 1)

dat$sex_Chi_bin <- as.factor(dat$sex_Chi_bin)

var_lab(dat$sex_Chi_bin) = "Offspring sex"

str(dat$sex_Chi_bin)
CrossTable(dat$sex_Chi_bin)

#------------------------------------------------------------------------------#
#                        Pregnancy & Perinatal Outcomes                        #----
#------------------------------------------------------------------------------#

# Prepare outcome variables

## Modify definitions

### Breastfeeding NOT initiated
dat$bf_N_ini <-
  1 - dat$bf_ini  # Breastfeeding initiated -> Breastfeeding NOT initiated
table(dat$bf_ini, useNA = "always")
table(dat$bf_N_ini, useNA = "always")

### Breastfeeding NOT established
dat$bf_est[dat$bf_N_ini == 1 |
             dat$pretb_all == 1] <-
  NA  # !!! Exclude: 1) never breastfed, 2) preterm birth !!!
dat$bf_N_est <-
  1 - dat$bf_est  # Breastfeeding established -> Breastfeeding NOT established
table(dat$bf_est, useNA = "always")
table(dat$bf_N_est, useNA = "always")

### Breastfeeding NOT sustained
dat$bf_sus[dat$bf_N_ini == 1 |
             dat$pretb_all == 1] <-
  NA  # !!! Exclude: 1) never breastfed, 2) preterm birth !!!
dat$bf_N_sus <-
  1 - dat$bf_sus  # Breastfeeding sustained -> Breastfeeding NOT sustained
table(dat$bf_sus, useNA = "always")
table(dat$bf_N_sus, useNA = "always")

### Breastfeeding duration (4 categories)
table(dat$bf_dur_4c, useNA = "always")

## Label variables and values

### Load outcome lists and labels
MRPREG_outcome_labels <-
  read.xlsx("MRPREG_outcome_labels.xlsx",
            sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

primary_bin <-
  read.xlsx("MRPREG_outcome_labels.xlsx",
            sheet = "Primary_bin")
primary_bin  # 14 primary (binary) outcomes

secondary_bin <-
  read.xlsx("MRPREG_outcome_labels.xlsx",
            sheet = "Secondary_bin")
secondary_bin  # 8 secondary binary outcomes

secondary_con <-
  read.xlsx("MRPREG_outcome_labels.xlsx",
            sheet = "Secondary_con")
secondary_con  # 4 secondary continuous outcomes

secondary_cat <-
  read.xlsx("MRPREG_outcome_labels.xlsx",
            sheet = "Secondary_cat")
secondary_cat  # 1 (secondary) ordinal/categorical outcome (bf_dur_4c as negative outcome)

### Identify available outcomes in the cohort
MoBa_primary_bin <-
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat))]
MoBa_primary_bin  # 14 primary (binary) outcomes available in MoBa

MoBa_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat))]
MoBa_secondary_bin  # 8 secondary binary outcomes available in MoBa

MoBa_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat))]
MoBa_secondary_con  # 4 secondary continuous outcomes available in MoBa

MoBa_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat))]
MoBa_secondary_cat  # 1 (primary) ordinal/categorical outcome available in MoBa

### Label values

#### Binary
for (var_name in colnames(dat[, which(colnames(dat) %in% c(MoBa_primary_bin, MoBa_secondary_bin))])) {
  val_lab(dat[, var_name]) = c("No" = 0,
                               "Yes" = 1)
  
  dat[[var_name]] <- factor(dat[[var_name]],
                            label = c("No", "Yes"))
}

#### Ordinal/categorical
for (var_name in colnames(dat[, which(colnames(dat) %in% MoBa_secondary_cat)])) {
  val_lab(dat[, var_name]) = c(
    "0 to <1 month" = 0,
    "1 to <3 months" = 1,
    "3 to <6 months" = 2,
    ">=6 months" = 3
  )  # From Brion et al., 2011, IJE
  
  dat[[var_name]] <- factor(
    dat[[var_name]],
    label = c("0 to <1 month", "1 to <3 months", "3 to <6 months", ">=6 months"),
    ordered = T
  )
}

### Label outcome variables
for (var_name in c(MoBa_primary_bin,
                   MoBa_secondary_bin,
                   MoBa_secondary_con,
                   MoBa_secondary_cat))
{
  var_lab(dat[, var_name]) <-
    MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$varname == var_name)]
}

### Sanity check
dat_check <- dat[, which(
  colnames(dat) %in% c(
    MoBa_primary_bin,
    MoBa_secondary_bin,
    MoBa_secondary_con,
    MoBa_secondary_cat
  )
)]

# View(dat_check)

################################################################################

# Save dataset
head(dat)
dim(dat)  # 73868   XXX

saveRDS(dat, "dat_exp_cov_out.rds")
