################################################################################
#              Maternal Vegetarian Diets & Perinatal Health - BiB              #
################################################################################

# Last edited date: 31-Jul-2024
# This script is to derive dietary exposure variables in BiB.

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
#                          Load BiB phenotype dataset                          #----
#------------------------------------------------------------------------------#

# Load data

## Main dataset
dat <- readRDS("dat_cov_out.rds")
dat <- zap_labels(dat)

head(dat)
dim(dat)  # 11865    XX

## FFQ data
mFFQ2 <- as_tibble(read_dta(paste0(Sys.getenv("datadir_FFQ"), mFFQ2_dir)))

head(mFFQ2)
dim(mFFQ2)  # 4494   44

## Baseline data (for breads, drinks, etc.)
mbqall <- as_tibble(read_dta(paste0(Sys.getenv("datadir"), mbqall_dir)))

head(mbqall)
dim(mbqall)  # 11395   342

## FFQ and baseline data
mFFQ2 <- mFFQ2 %>%
  distinct(BiBPersonID, .keep_all = T)

mbqall_FFQ2 <- left_join(mbqall, mFFQ2, by = "BiBPersonID")

head(mbqall_FFQ2)
dim(mbqall_FFQ2)  # 11395   XXX

#------------------------------------------------------------------------------#
#                          Prepare Dietary Variables                           #----
#------------------------------------------------------------------------------#

# Recode frequencies

## Group FFQ variables
vars_8cat = c(
  "ffq2pchips",
  "ffq2othpot",
  "ffq2fibcer",
  "ffq2oatcer",
  "ffq2othcer",
  "ffq2crisbr",
  "ffq2noodle",
  "ffq2savory",
  "ffq2potcrs",
  "ffq2othsal",
  "ffq2cakbun",
  "ffq2swepas",
  "ffq2chocbr",
  "ffq2swebic"
)  # N = 14

vars_5cat <- c(
  "ffq2whbeef",
  "ffq2whpork",
  "ffq2whlamb",
  "ffq2whchik",
  "ffq2prsaus",
  "ffq2prburg",
  "ffq2prkebb",
  "ffq2prhotd",
  "ffq2prbacn",
  "ffq2prpies",
  "ffq2prchng",
  "ffq2proham",
  "ffq2prdrsa",
  "ffq2sauchk",
  "ffq2saubef",
  "ffq2sauprk",
  "ffq2saugvy",
  "ffq2fshbat",
  "ffq2fshnob",
  "ffq2tintun",
  "ffq2fshoil",
  "ffq2fshsmk",
  "ffq2fshsal"
)  # N = 23

## Weekly intake frequency

### 8 categories
#### 1: Rarely or never          0
#### 2: Less than 1 in a week    0.5
#### 3: Once a week              1
#### 4: 2-3 times a week         2.5
#### 5: 4-6 times a week         5
#### 6: 1-2 times a day          10.5 (1.5 * 7)
#### 7: 3-4 times a day          24.5 (3.5 * 7)
#### 8: 5+ times a day           35 (5 * 7)
mbqall_FFQ2[, vars_8cat] <-
  sapply(
    mbqall_FFQ2[, vars_8cat],
    dplyr::recode,
    "1" = 0,
    "2" = 0.5,
    "3" = 1,
    "4" = 2.5,
    "5" = 5,
    "6" = 10.5,
    "7" = 24.5,
    "8" = 35
  )

summary(mbqall_FFQ2$ffq2pchips)

### 5 categories
#### 1: Rarely or never          0
#### 2: Less than 1 in a week    0.5
#### 3: 2-3 times a week         2.5
#### 4: 4-5 times a week         4.5
#### 5: 7+ times a week          10
mbqall_FFQ2[, vars_5cat] <-
  sapply(
    mbqall_FFQ2[, vars_5cat],
    dplyr::recode,
    "1" = 0,
    "2" = 0.5,
    "3" = 2.5,
    "4" = 4.5,
    "5" = 10,
  )

summary(mbqall_FFQ2$ffq2whbeef)

################################################################################

# Daily intake frequency (times/day) - Used for deriving vegetarian subgroups and descriptive purposes
# [NOTE: FFQ asked "Frequency consumed one portion", but portion sizes were not defined.
#        Here, we use "times" instead of "portions" as the unit to distinguish with cohorts with pre-defined portion sizes (e.g., Project Viva).
#        Also, non-FFQ items (except for cola/coffee/tea) asked quantities (e.g., slices) of consumption, we also use "times" instead as the unit.
#        Now, both FFQ and non-FFQ (except for cola/coffee/tea) items are in "times/week"; divided by 7 to convert into "times/day".]

## Plant food groups (healthy) - 7

### Whole grains
mbqall_FFQ2$wholegrain <-
  rowSums(mbqall_FFQ2[, c("ffq2fibcer", "ffq2oatcer", "ffq2crisbr", "brd0brwbrd")], na.rm = T) / 7  # 3 from FFQ + 1 from baseline survey

var_lab(mbqall_FFQ2$wholegrain) = "Whole grains (times/day)"
summary(mbqall_FFQ2$wholegrain)

### Fruits - !!! NOT AVAILABLE !!!

### Vegetables - !!! NOT AVAILABLE !!!

### Nuts - !!! NOT AVAILABLE !!!

### Legumes - !!! NOT AVAILABLE !!!

### Vegetable oils - !!! NOT AVAILABLE !!!

### Tea & coffee
mbqall_FFQ2$teacoffee <-
  rowSums(mbqall_FFQ2[, c(
    "cdr0fccfpd",
    "cdr0fcdcpd",
    "cdr0htcfpd",
    "cdr0htdcpd",
    "cdr0iccfpd",
    "cdr0icdcpd",
    "cdr0ktcfpd",
    "cdr0ktdcpd",
    "cdr0tecfpd",
    "cdr0tedcpd"
  )], na.rm = T)  # 10 from baseline survey; no need to add "/ 7" as already "cups/day"

var_lab(mbqall_FFQ2$teacoffee) = "Tea and coffee (cups/day)"
summary(mbqall_FFQ2$teacoffee)

################################################################################
################################################################################

## Plant food groups (less healthy) - 5

### Fruit juices - !!! NOT AVAILABLE !!!

### Refined grains
mbqall_FFQ2$refinedgrain <-
  rowSums(mbqall_FFQ2[, c(
    "ffq2othcer",
    "ffq2noodle",
    "ffq2savory",
    "ffq2othsal",
    "brd0brolls",
    "brd0nnptbg",
    "brd0othbrd",
    "brd0parath",
    "brd0rotich",
    "brd0whtbrd"
  )], na.rm = T) / 7  # 4 from FFQ + 6 from baseline survey

var_lab(mbqall_FFQ2$refinedgrain) = "Refined grains (times/day)"
summary(mbqall_FFQ2$refinedgrain)

### Potatoes
mbqall_FFQ2$potato <-
  rowSums(mbqall_FFQ2[, c("ffq2pchips", "ffq2othpot", "ffq2potcrs")], na.rm = T) / 7  # 3 from FFQ

var_lab(mbqall_FFQ2$potato) = "Potatoes (times/day)"
summary(mbqall_FFQ2$potato)

### Sugar sweetened beverages
mbqall_FFQ2$sugarbeverage <-
  rowSums(mbqall_FFQ2[, c("cdr0clcfpd", "cdr0cldcpd")], na.rm = T)  # 2 from baseline survey; no need to add "/ 7" as already "cups/day"

var_lab(mbqall_FFQ2$sugarbeverage) = "Sugar sweetened beverages (cups/day)"
summary(mbqall_FFQ2$sugarbeverage)

### Sweets & desserts
mbqall_FFQ2$sweetdessert <-
  rowSums(mbqall_FFQ2[, c("ffq2cakbun", "ffq2swepas", "ffq2chocbr", "ffq2swebic")], na.rm = T) / 7  # 4 from FFQ

var_lab(mbqall_FFQ2$sweetdessert) = "Sweets and desserts (times/day)"
summary(mbqall_FFQ2$sweetdessert)

################################################################################
################################################################################

## Animal food groups - 6

### Animal fat - !!! NOT AVAILABLE !!!

### Dairy - !!! NOT AVAILABLE !!!

### Egg - !!! NOT AVAILABLE !!!

### Fish & seafood
mbqall_FFQ2$fishseafood <-
  rowSums(mbqall_FFQ2[, c("ffq2fshbat",
                          "ffq2fshnob",
                          "ffq2tintun",
                          "ffq2fshoil",
                          "ffq2fshsmk",
                          "ffq2fshsal")]) / 7  # 6 from FFQ

var_lab(mbqall_FFQ2$fishseafood) = "Fish and seafood (times/day)"
summary(mbqall_FFQ2$fishseafood)

### Meat
mbqall_FFQ2$meat <-
  rowSums(mbqall_FFQ2[, c(
    "ffq2whbeef",
    "ffq2whpork",
    "ffq2whlamb",
    "ffq2whchik",
    "ffq2prsaus",
    "ffq2prburg",
    "ffq2prkebb",
    "ffq2prhotd",
    "ffq2prbacn",
    "ffq2prpies",
    "ffq2prchng",
    "ffq2proham",
    "ffq2prdrsa",
    "ffq2sauchk",
    "ffq2saubef",
    "ffq2sauprk",
    "ffq2saugvy"
  )]) / 7  # 17 from FFQ

var_lab(mbqall_FFQ2$meat) = "Meat (times/day)"
summary(mbqall_FFQ2$meat)

### Miscellaneous animal-based foods
mbqall_FFQ2$misc.animal <-
  rowSums(mbqall_FFQ2[, c("brd0pizzas")], na.rm = T) / 7  # 1 from baseline survey

var_lab(mbqall_FFQ2$misc.animal) = "Miscellaneous animal-based foods (times/day)"
summary(mbqall_FFQ2$misc.animal)

################################################################################
# Do you consume 5 portions of fruit and vegetables per day?
## 1 : Always
## 2 : Sometimes
## 3 : Never
mbqall_FFQ2$FiveADay_Mat_DUR.p_3cat <- mbqall_FFQ2$fdk2consfv
mbqall_FFQ2 <- mbqall_FFQ2 %>%
  mutate(FiveADay_Mat_DUR.p_3cat = case_when(fdk2consfv == 3 ~ 0, fdk2consfv == 2 ~ 1, fdk2consfv == 1 ~ 2))

val_lab(mbqall_FFQ2$FiveADay_Mat_DUR.p_3cat) = c("Never" = 0,
                                                 "Sometimes" = 1,
                                                 "Always" = 2)

mbqall_FFQ2$FiveADay_Mat_DUR.p_3cat <-
  as.factor(mbqall_FFQ2$FiveADay_Mat_DUR.p_3cat)

var_lab(mbqall_FFQ2$FiveADay_Mat_DUR.p_3cat) = "Consumed 5 portions of fruit and vegetables per day"

table(mbqall_FFQ2$FiveADay_Mat_DUR.p_3cat, useNA = "always")
################################################################################
# Merge with the main dataset
mbqall_FFQ2 <- mbqall_FFQ2 %>%
  select(
    BiBPersonID,
    wholegrain,
    teacoffee,
    refinedgrain,
    potato,
    sugarbeverage,
    sweetdessert,
    fishseafood,
    meat,
    misc.animal,
    FiveADay_Mat_DUR.p_3cat
  ) %>%
  distinct(BiBPersonID, .keep_all = TRUE)

head(mbqall_FFQ2)

dat <- left_join(dat, mbqall_FFQ2, by = c("BiBMotherID" = "BiBPersonID"))

head(dat)
dim(dat)  # 11865   XXX
################################################################################

# Diet-based vegetarianism (vegetarian subgroups)

## data preparation: Generate weekly intake frequency of meat & poultry, fish & seafood, egg, and dairy
dat$meat_freq  <-
  dat$meat * 7 # Weekly intake frequency (times/week) of meat & poultry
dat$fishseafood_freq <-
  dat$fishseafood * 7  # Weekly intake frequency (times/week) of fish & seafood
# dat$egg_freq  # Weekly intake frequency (times/week) of egg - !!! data NOT available !!!
# dat$dairy_freq  # Weekly intake frequency (times/week) of dairy - !!! data NOT available !!!

dat$meatfish_freq <-
  rowSums(dat[, c("meat_freq", "fishseafood_freq")])  # Weekly intake frequency (times/week) of meat & poultry + fish & seafood
# dat$eggdairy_freq <-
#   rowSums(dat[, c("egg_freq", "dairy_freq")])  # Weekly intake frequency (times/week) of egg + dairy - !!! data NOT available !!!
# dat$animal_freq <-
#   rowSums(dat[, c("meat_freq", "fishseafood_freq", "egg_freq", "dairy_freq")])  # Weekly intake frequency (times/week) of meat & poultry + fish & seafood + egg + dairy - !!! data NOT available !!!

## Vegetarian subgroups (detailed) - Egg and dairy intakes not available; use the method of exclusion to identify (full) vegetarians

### 2 - Full vegetarian (including lacto-ovo-vegetarian & vegan) - meat & poultry + fish & seafood < 1 time/month
dat$VegDiet_subgroup <- 2

dat$VegDiet_subgroup[is.na(dat$meat_freq) |
                       is.na(dat$fishseafood_freq)] <-
  NA  # !!! Those with no dietary data already removed here !!!

### 0 - Non-vegetarian - meat & poultry: no restriction; fish & seafood: no restriction
###                      now including semi-vegetarians
###                      From Orlich et al, 2013, JAMA Intern Med: "Semi-vegetarians consumed non-fish meats 1 time/mo or more and
###                                                                 all meats combined (fish included) 1 time/mo or more but no more than 1 time/wk;
###                                                                 non-vegetarians consumed non-fish meats 1 time/mo or more and
###                                                                 all meats combined (fish included) more than 1 time/wk."
dat$VegDiet_subgroup[which(dat$meat_freq >= 0.25)] <-
  0  # If non-fish meats >= 1 time/mo, all meats combined (fish included) must already >= 1 time/mo

### 1 - Pesco-vegetarian - meat & poultry < 1 time/month; fish & seafood >= 1 time/month
dat$VegDiet_subgroup[which(dat$meat_freq < 0.25 &
                             dat$fishseafood_freq >= 0.25)] <- 1

################################################################################
################################################################################

## Vegetarian subgroups (combined into 3 categories) - Used for surrogate variable analysis below
### 0 - Non-vegetarian
### 1 - Pesco-vegetarian
### 2 - Full vegetarian (including lacto-ovo-vegetarian & vegan)
dat$VegDiet_3cat <- NA
dat$VegDiet_3cat[dat$VegDiet_subgroup == 0] <- 0
dat$VegDiet_3cat[dat$VegDiet_subgroup == 1] <- 1
dat$VegDiet_3cat[dat$VegDiet_subgroup %in% c(2, 3)] <- 2

################################################################################
################################################################################

## Vegetarianism (binary)
### 0 - Non-vegetarian
### 1 - Full vegetarian + pesco-vegetarian
dat$VegDiet_bin <- NA
dat$VegDiet_bin[dat$VegDiet_3cat == 0] <- 0
dat$VegDiet_bin[dat$VegDiet_3cat %in% c(1, 2)] <- 1

################################################################################
################################################################################

## Descriptive results
val_lab(dat$VegDiet_subgroup) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)  # Vegans and lacto-ovo-vegetarians not distinguishable
str(dat$VegDiet_subgroup)
CrossTable(dat$VegDiet_subgroup)  # 127 "vegetarians" = 73 full-vegetarians (vegans not distinguishable) + 54 pesco-vegetarians
# |   Non-vegetarian | Pesco-vegetarian |  Full vegetarian |
# |------------------|------------------|------------------|
# |             3520 |               54 |               73 |
# |            0.965 |            0.015 |            0.020 |
# |------------------|------------------|------------------|

val_lab(dat$VegDiet_3cat) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)
str(dat$VegDiet_3cat)
CrossTable(dat$VegDiet_3cat)
# |   Non-vegetarian | Pesco-vegetarian |  Full vegetarian |
# |------------------|------------------|------------------|
# |             3520 |               54 |               73 |
# |            0.965 |            0.015 |            0.020 |
# |------------------|------------------|------------------|

val_lab(dat$VegDiet_bin) = c("Non-vegetarian" = 0, "Vegetarian" = 1)
str(dat$VegDiet_bin)
CrossTable(dat$VegDiet_bin)
# | Non-vegetarian |     Vegetarian |
# |----------------|----------------|
# |           3520 |            127 |
# |          0.965 |          0.035 |
# |----------------|----------------|

################################################################################
################################################################################

## Convert and label vegetarianism variables
dat$VegDiet_subgroup <- as.factor(dat$VegDiet_subgroup)
var_lab(dat$VegDiet_subgroup) = "Diet-based vegetarianism (subgroups) during pregnancy"

dat$VegDiet_3cat <- as.factor(dat$VegDiet_3cat)
var_lab(dat$VegDiet_3cat) = "Diet-based vegetarianism (3 catetories) during pregnancy"

dat$VegDiet_bin <- as.factor(dat$VegDiet_bin)
var_lab(dat$VegDiet_bin) = "Diet-based vegetarianism (binary) during pregnancy"

#------------------------------------------------------------------------------#
#                                 Check & Save                                 #----
#------------------------------------------------------------------------------#

# Subset data - !!! Only keep those with dietary exposure data !!!
dat <-
  subset(dat, is.na(dat$VegDiet_subgroup) == F)

sum(is.na(dat$BiBMotherID))  # 0 with no maternal ID
sum(duplicated(dat$BiBMotherID))  # 0 with duplicated maternal ID

head(dat)
dim(dat)  # 11865 -> 3647  XXX

saveRDS(dat, "dat_exp_cov_out.rds")
