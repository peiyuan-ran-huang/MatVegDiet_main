################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 27-Jun-2024
# This script is to derive dietary exposure variables in ALSPAC.

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
dat <- as.tibble(readRDS("data/ALSPAC/dat_cov_out.rds"))

head(dat)
dim(dat)  # 14345   XXX

dat <-
  as.data.frame(dat)  # Remember to convert tibble into data frame, or you will run into error when deriving PDIs below

################################################################################

#------------------------------------------------------------------------------#
#                          Prepare Dietary Variables                           #----
#------------------------------------------------------------------------------#

# Dietary data (adapted from Giulia's script "ALSPAC_med_diet_calc.R")

## Recode missing values (<0)
vars = c(
  "c200",
  "c201",
  "c202",
  "c203",
  "c204",
  "c205",
  "c206",
  "c207",
  "c208",
  "c209",
  "c210",
  "c211",
  "c215",
  "c216",
  "c217",
  "c218",
  "c219",
  "c220",
  "c221",
  "c222",
  "c223",
  "c224",
  "c225",
  "c226",
  "c227",
  "c228",
  "c229",
  "c230",
  "c231",
  "c232",
  "c233",
  "c234",
  "c235",
  "c236",
  "c237",
  "c238",
  "c239",
  "c240",
  "c241",
  "c242",
  "c243",
  "c244",
  "c245",
  "c246",
  "c247",
  "c251",
  "c252",
  "c253",
  "c254",
  "c255",
  "c256",
  "c260",
  "c261",
  "c263",
  "c264",
  "c265",
  "c266",
  "c267",
  "c268",
  "c269",
  "c270",
  "c271",
  "c272",
  "c273",
  "c274",
  "c276",
  "c277",
  "c278",
  "c279",
  "c280",
  "c281",
  "c282",
  "c283",
  "c284",
  "c285",
  "c286",
  "c287",
  "c288",
  "c289",
  "c300",
  "c301",
  "c303",
  "c305",
  "c306",
  "c308",
  "c310",
  "c312",
  "c316"
)

summary(dat$c200)  # Was: 2289 missing in FFQ data
# dat[, vars] <- replace(dat[, vars], dat[, vars] < 0, 0)
dat[, vars] <-
  replace(dat[, vars], dat[, vars] < 0, NA)  # !!! Values < 0 NOT recoded as 0 as in Giulia's script because this will affect the classification of vegetarian subgroups (0 intake of animal-based foods will be counted as vegans) and the calculation of PDIs (the distribution will be highly skewed by lots of 0 intake)
summary(dat$c200)  # Now: 2652 missing in FFQ data

## Select weekly frequency variables (43 FFQ variables)
FFQ_weekly <- openxlsx::read.xlsx(
  "data/ALSPAC/ALSPAC_data_catalog_C.xlsx",
  sheet = "FFQ",
  colNames = T,
  rowNames = F
)
FFQ_weekly  # 43 variables
mean(FFQ_weekly$Varname %in% colnames(dat))  # All included

## Recode frequencies

### Weekly intake frequency
#### 1  Never or rarely         0
#### 2  Once in 2WKS            0.5
#### 3  1-3 times PWK           2
#### 4  4-7 times PWK           5.5
#### 5  More than once a day    10
dat[, FFQ_weekly$Varname] <-
  sapply(
    dat[, FFQ_weekly$Varname],
    dplyr::recode,
    "1" = 0,
    "2" = 0.5,
    "3" = 2,
    "4" = 5.5,
    "5" = 10
  )

################################################################################
### c247 Frequency of choosing diet soft drink
### !!! This was NOT included in Giulia's script
### !!! In Pauline's original script, it was used as the intake frequency of soft drinks
### !!! BUT it was actually "When you have a soft drink, how often do you choose low calorie or diet drinks?"
### !!! So previous codes seemed not to be correct...
### !!! In this script, I combined c247 (frequency of choosing diet drinks), c310 (drinks of caffeinated cola per week), and c312 (drinks of decaf cola per week) for SSBs

#### Missing (<0)        Already recoded as NA above
#### 1  Always           0 (0%: always choosing ASBs = never choosing SSBs)
#### 2  Sometimes        0.5 (50%: half ASBs = half SSBs)
#### 3  Never            1 (100%: having soft drinks but never choosing ASBs = always choosing SSBs)
#### 7  No soft drink    0
dat$c247 <- dplyr::recode(
  dat$c247,
  "1" = 0,
  "2" = 0.5,
  "3" = 1,
  "7" = 0
)
################################################################################

### c250 - Slices of bread
#### Missing (<0)    1.5 (!!! Not sure why but just follow what Giulia's script did)
#### 1  <1           0
#### 2  1-2          1.5
#### 3  3-4          3.5
#### 4  5 or more    6
dat$c250[which(dat$c250 < 0)] <- 1.5
dat$c250 <- dplyr::recode(
  dat$c250,
  "0" = 0,
  "1" = 0,
  "2" = 1.5,
  "3" = 3.5,
  "4" = 6
)

### c275 - Slices of bread with fat
#### Missing     2 (!!! Not sure why but just follow what Giulia's script did)
#### Values > 7  6
dat$c275[which(dat$c275 < 0)] <- 2
dat$c275[which(dat$c275 > 7)] <- 6

### c252-255 - Eat types of bread
#### 1  Yes    1
#### 2  No     0
dat$c252[which(dat$c252 != 1)] <- 0
dat$c253[which(dat$c253 != 1)] <- 0
dat$c254[which(dat$c254 != 1)] <- 0
dat$c255[which(dat$c255 != 1)] <- 0

### c284 c285 c287 c288 c289 - Milk in tea, coffee, pudding, as drink, in milky drink
#### 0  (Recoded from missing)    0 (!!! This category not existing because all missing data already recoded as NA)
#### 1  Usually                   1
#### 2  Sometimes                 0.5
#### 3  Never                     0
vars = c("c284", "c285", "c287", "c288", "c289")
dat[, vars] <- sapply(dat[, vars],
                      dplyr::recode,
                      # "0" = 0,
                      "1" = 1,
                      "2" = 0.5,
                      "3" = 0)

### c286 - Milk in cereal
#### 0  (Recoded from missing)    0 (!!! This category not existing because all missing data already recoded as NA)
#### 1  Usually                   1
#### 2  Sometimes                 1
#### 3  Never                     0
dat$c286 = dplyr::recode(dat$c286,
                         # "0" = 0,
                         "1" = 1,
                         "2" = 1,
                         "3" = 0)

################################################################################
### c276 c277 c278 c279 c280 c281 c282 - Types of milk (whole milk, semi-skimmed milk, skimmed milk, sterilised milk, dried milk, goat/sheep milk, soya milk)
### !!! This was NOT included in Giulia's script but mainly based on Pauline's original script

#### 0  (Recoded from missing)    0 (!!! This category not existing because all missing data already recoded as NA)
#### 1  Usually                   1
#### 2  Sometimes                 1
#### 3  Never                     0
vars = c("c276", "c277", "c278", "c279", "c280", "c281", "c282")
dat[, vars] <- sapply(dat[, vars],
                      dplyr::recode,
                      # "0" = 0,
                      "1" = 1,
                      "2" = 1,
                      "3" = 0)
################################################################################

### c269 & c270 - Oil on bread/for frying
#### 1  Yes    1
#### 2  No     0
dat$c269[which(dat$c269 != 1)] <- 0
dat$c270[which(dat$c270 != 1)] <- 0

## Sanity check
check <- c()

for (varname in c(
  FFQ_weekly$Varname,
  "c250",
  "c252",
  "c253",
  "c254",
  "c255",
  "c269",
  "c270",
  "c275",
  "c276",
  "c277",
  "c278",
  "c279",
  "c280",
  "c281",
  "c282",
  "c284",
  "c285",
  "c286",
  "c287",
  "c288",
  "c289"
)) {
  x <- mean(dat[, varname] < 0, na.rm = T)
  check <- c(check, x)
}

sum(check)  # All values >= 0

head(dat)
dim(dat)  # 14345   XXX

################################################################################

# Derive 18 food groups for plant-based diet indices
# Based on non-quantitative FFQ (only weekly intake frequency, without portion size)

## Plant food groups (healthy) - 7

### Whole grains (refined grains also derived here)

#### Bread - portions and types measured separately

##### c250 Portions eaten per day

##### Types of bread eaten
###### c252 - White: 36g (counted as refined grains)
###### c253 - Brown: 18 + 18 = 36g (counted as whole grains)
###### c254 - Wholemeal: 36g (counted as whole grains)
###### c255 - Chapati: 28 + 40 = 68g (counted as refined grains)
bread_portion <-
  matrix(c(36, 36, 36, 68), 4, 1)  # Vector of portions for each type of bread (c252 c253 c254 c255)

bread_Matrix <-
  as.matrix(dat[, c("c252", "c253", "c254", "c255")])  # Matrix of bread types eaten: rows for observations, 4 columns for bread types; matrix contains 1 if the type is eaten, 0 otherwise

bread_type_n <-
  apply(bread_Matrix, 1, sum)  # Count the number of different types of bread eaten

bread_Matrix[bread_type_n > 0 &
               is.na(bread_type_n) == F, ] <-
  bread_Matrix[bread_type_n > 0 &
                 is.na(bread_type_n) == F, ] / bread_type_n[bread_type_n > 0 &
                                                              is.na(bread_type_n) == F]  # Scale bread matrix into proportions

dat$wholebread <-
  bread_Matrix[, 2:3] %*% bread_portion[2:3, ]  # Calculate total portion of whole grain bread (brown bread + wholemeal bread) eaten by each individual
dat$wholebread <- as.numeric(dat$wholebread)
dat$wholebread
str(dat$wholebread)

dat$refinebread <-
  bread_Matrix[, c(1, 4)] %*% bread_portion[c(1, 4), ]  # Calculate the portion of refined grain bread (white bread + chapati) eaten by each individual
dat$refinebread <- as.numeric(dat$refinebread)
dat$refinebread
str(dat$refinebread)

dat$wholebread <-
  dat$wholebread * dat$c250  # Daily intake of whole grain bread
summary(dat$wholebread)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00    0.00   36.00   44.06   63.00  216.00    2656

dat$refinebread <-
  dat$refinebread * dat$c250  # Daily intake of refined grain bread
summary(dat$refinebread)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00    0.00   27.00   34.55   54.00  312.00    2656

#### Other cereals (weekly intakes need to be converted into daily intakes)

##### Whole grains
######! c217 - Rice (brown): 27g
######! c218 - Pasta (wholemeal): 30g
###### c233 - Oat cereals: 17 + 53 + 10 = 80g
###### c234 - Bran cereals: 10 + 8 + 8 + 10 = 36g
###### c237 - Crispbread: 20g
dat$wholecereals <-
  dat$c217 * 27 +
  dat$c218 * 30 +
  dat$c233 * 80 +
  dat$c234 * 36 +
  dat$c237 * 20

dat$wholecereals <- dat$wholecereals / 7

##### Refined grains
######! c217 - Rice (white): 108g
######! c218 - Pasta (white): 120g
###### c235 - Other cereals: 6 + 6 + 6 + 9 + 6 = 33g
dat$refinecereals <- dat$c217 * 108 + dat$c218 * 120 + dat$c235 * 33

dat$refinecereals <- dat$refinecereals / 7

#### Combine bread and cereals
dat$wholegrain <- dat$wholebread + dat$wholecereals
summary(dat$wholegrain)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   32.36   77.36   87.20  131.50  418.00    2685
var_lab(dat$wholegrain) = "Whole grains (g/day)"

dat$refinedgrain <- dat$refinebread + dat$refinecereals
summary(dat$refinedgrain)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   44.36   70.29   81.74  105.43  461.14    2685

### Fruits
#### c229 - Fresh fruit: 17 + 17 + 17 + 21 + 18 + 28 = 118g
dat$fruit <- dat$c229 * 118

dat$fruit <- dat$fruit / 7

summary(dat$fruit)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   33.71   92.71   91.81  168.57  168.57    2640
var_lab(dat$fruit) = "Fruits (g/day)"

### Vegetables
#### c224 - Green leafy: 36 + 38 + 19 = 93g
#### c225 - Other green: 23 + 21 + 23 + 19 = 86g
#### c226 - Carrots: 60g
#### c227 - Other root: 26 + 24 + 12 = 62g
#### c228 - Salad: 23 + 30 + 60 = 113g
####! c223 - Sweetcorn (from legumes): 21g
####! c230 - Canned tomatoes (from fruit juice (tinned)): 80g
dat$vegetable <-
  dat$c224 * 93 +
  dat$c225 * 86 +
  dat$c226 * 60 +
  dat$c227 * 62 +
  dat$c228 * 113 +
  dat$c223 * 21 +
  dat$c230 * 80

dat$vegetable <- dat$vegetable / 7

summary(dat$vegetable)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  0.0    80.0   110.9   123.0   154.6   609.5    2640
var_lab(dat$vegetable) = "Vegetables (g/day)"

### Nuts
#### c241 - Nuts: 8 + 8 + 8 + 28 = 52g
#### c243 - Tahini: 10 + 10 = 20g
dat$nut <- dat$c241 * 52 + dat$c243 * 20

dat$nut <- dat$nut / 7

summary(dat$nut)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   0.000   0.000   2.543   3.714 102.857    2640
var_lab(dat$nut) = "Nuts (g/day)"

### Legumes
#### c222 - Baked beans: 135g
####! c223 - Legumes: 35 + 18 = 53g (excluding sweetcorn 21g)
#### c240 - Pulses: 30 + 30 + 30 = 120g
#### c242 - Bean curd: 80g
#### c244 - Meat replacement: 37 + 47 + 36 = 120g
####! c282 - Soya milk (added later) = 100g
dat$legume <-
  dat$c222 * 135 +
  dat$c223 * 53 +
  dat$c240 * 120 +
  dat$c242 * 80 +
  dat$c244 * 120

dat$legume <- dat$legume / 7

summary(dat$legume)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   24.79   53.71   47.88   53.71  542.50    2640
var_lab(dat$legume) = "Legumes (g/day)"

### Vegetable oils (animal fat also derived here)

#### c260 - Butter on bread: 100g (counted as animal fat)
#### c261 - Butter for frying: 10 + 10 + 5 + 5 + 35 + 35 = 100g (counted as animal fat)

#### c263 - Margarine on bread
#### c264 - Margarine for frying: NOT in the "Foods within items" sheet

#### c265 - Polyunsaturated margarine on bread
#### c266 - Polyunsaturated margarine for frying: NOT in the "Foods within items" sheet

#### c267 - Low fat spread on bread
#### c268 - Low fat spread for frying: NOT in the "Foods within items" sheet

#### c269 - Corn, olive, soya, sunflower oil on bread: NOT USED
#### c270 - Corn, olive, soya, sunflower oil for frying: 30 + 10 + 30 + 30 = 100g (counted as vegetable oil)

#### c271 - Other vegetable oil on bread: NOT USED
#### c272 - Other vegetable oil for frying: 100g (counted as vegetable oil)

#### c273 - Other fat on bread: NOT USED
#### c274 - Other fat for frying: NOT USED

#### c275 - Slices of bread with fat eaten per day
#### c220 - FREQ of eating fried food per week

#### !!! Margarine (c263/c264 margarine, c265/c266 polyunsaturated margarine, and c267/c268 low fat spread) not used for PDI calculation \
#### because Satija et al.(2016) said: "We also excluded margarine from the indices, \
#### as its fatty acid composition has changed over time from high trans fat to high unsaturated fat. \
#### We controlled for alcoholic beverages and margarine consumption in the analysis."
#### !!! Here, we do not include it because in ALSPAC FFQ it is difficult to separate between margarine from vegetable fat and from animal fat. \
#### But we use it to estimate the intake of butter and vegetable oils as it still accounts for a proportion in oil used on bread/for cooking.

##### On bread
fat_Matrix <-
  as.matrix(dat[, c("c260", "c263", "c265", "c267")])  # Matrix of fat types (spreadtotal = sum of c260 c263 c265 c267): rows for observations, columns for fat types

fat_type_n <-
  apply(fat_Matrix, 1, sum)  # Count the number of different types of fat

fat_Matrix[fat_type_n > 0 &
             is.na(fat_type_n) == F, ] <-
  fat_Matrix[fat_type_n > 0 &
               is.na(fat_type_n) == F, ] / fat_type_n[fat_type_n > 0 &
                                                        is.na(fat_type_n) == F]  # Scale fat matrix into proportions
###### Animal fat on bread
dat$animalfat_bread <-
  fat_Matrix[, 1] * dat$c275 * 100  # Only the first column (c260) used for calculation
summary(dat$animalfat_bread)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   33.33   57.14   61.53   85.71  240.00    2696

###### !!! Vegetable oils on bread not estimated because margarine not used

##### For frying
fat_portion <-
  matrix(c(100, 100), 2, 1)  # Vector of portions for each type of fat (c270, c272)

fat_Matrix <-
  as.matrix(dat[, c("c261", "c264", "c266", "c268", "c270", "c272")])  # Matrix of fat types (ckfattotal = sum of c261 c264 c266 c268 c270 c272): rows for observations, columns for fat types

fat_type_n <-
  apply(fat_Matrix, 1, sum)  # Count the number of different types of fat

fat_Matrix[fat_type_n > 0 &
             is.na(fat_type_n) == F, ] <-
  fat_Matrix[fat_type_n > 0 &
               is.na(fat_type_n) == F, ] / fat_type_n[fat_type_n > 0 &
                                                        is.na(fat_type_n) == F]  # Scale fat matrix into proportions

###### Animal fat for frying
dat$animalfat_fried <-
  fat_Matrix[, 1] * dat$c220 * 100  # Only the first column (c261) used for calculation; no need for "/ 7" in Giulia's script
summary(dat$animalfat_fried)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00    0.00    5.00   10.64   11.11  222.22    2721

###### Vegetable oils for frying
dat$vegetableoil_fried <-
  as.numeric(fat_Matrix[, 5:6] %*% fat_portion) * dat$c220  # Only the last two columns (c270, c272) used for calculation; no need for "/ 7" in Giulia's script
summary(dat$vegetableoil_fried)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   0.000   5.556  13.277  13.636 300.000    2721

##### Derive vegetable oils and animal fat
dat$vegetableoil <-
  dat$vegetableoil_fried  # Vegetable oils on bread not existing
summary(dat$vegetableoil)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   0.000   5.556  13.277  13.636 300.000    2721
var_lab(dat$vegetableoil) = "Vegetable oils (g/day)"

dat$animalfat <-
  dat$animalfat_bread + dat$animalfat_fried
summary(dat$animalfat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   42.86   66.23   72.18   96.83  393.65    2721

### Tea & coffee (unit: cups/day)
#### c300 - Cups of tea per day
#### c303 - Cups of decaf tea per day
#### c305 - Cups of coffee per day
#### c308 - Cups of decaf coffee per day
#### c316 - Cups of herbal tea per week
dat$teacoffee <-
  sum_row(dat$c300, dat$c303, dat$c305, dat$c308, (dat$c316 / 7), na.rm = T)  # Skip NAs to maximise sample size

summary(dat$teacoffee)  # Without NAs!
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.000   2.000   4.000   4.055   6.000  35.000
dat$teacoffee <-
  dat$teacoffee * 225  # 1 cup = 225 grams for both tea and coffee
summary(dat$teacoffee)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.0   450.0   900.0   912.4  1350.0  7875.0
var_lab(dat$teacoffee) = "Tea and coffee (g/day)"

################################################################################

## Plant food groups (less healthy) - 5

### Fruit juices
####! c230 - Fruit juice (tinned): 80 (excluding canned tomatoes 80g)
#### c231 - Fruit juice (not tinned): 32 + 96 + 32 = 160g
dat$fruitjuice <-
  dat$c230 * 80 +
  dat$c231 * 160

dat$fruitjuice <- dat$fruitjuice / 7

summary(dat$fruitjuice)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   11.43   45.71   76.27  125.71  342.86    2640
var_lab(dat$fruitjuice) = "Fruit juices (g/day)"

### Refined grains (already derived along with whole grains above)
summary(dat$refinedgrain)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   44.36   70.29   81.74  105.43  461.14    2685
var_lab(dat$refinedgrain) = "Refined grains (g/day)"

### Potatoes
#### c211 - Chips: 42 + 33 + 23 + 33 + 33 = 164g
#### c215 - Roast potatoes: 103g
#### c216 - Potatoes (other): 20 + 30 + 27 + 60 = 137g
#### c219 - Crisps: 27g
dat$potato <-
  dat$c211 * 164 +
  dat$c215 * 103 +
  dat$c216 * 137 +
  dat$c219 * 27

dat$potato <- dat$potato / 7

summary(dat$potato)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   65.93  107.64  106.49  136.21  615.71    2652
var_lab(dat$potato) = "Potatoes (g/day)"

### Sugar sweetened beverages
#### c247 - Soft drinks: 110 + 110 + 110 = 330g
#### !!! In Pauline's original script, c247 was directly used as the intake frequency of soft drinks
#### !!! BUT it was actually "When you have a soft drink, how often do you choose low calorie or diet drinks?"
#### !!! So previous codes seemed not to be correct...
#### !!! In this script, I combined c247 (frequency of choosing diet drinks), c310 (drinks of caffeinated cola per week), and c312 (drinks of decaf cola per week) for SSBs

##### c247 - Frequency of choosing diet soft drink (already recoded above)
###### Missing (<0)        NA
###### Always           0 (0% SSB)
###### Sometimes        0.5 (50% SSB)
###### Never            1 (100% SSB)
###### No soft drink    0

dat$sugarbeverage <-
  sum_row(dat$c310, dat$c312, na.rm = T) / 7 * dat$c247 * 330  # Caffeinated & decaf cola PER DAY (considered as total soft drinks) * proportion of SSBs * grams per drink

summary(dat$sugarbeverage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00    0.00    0.00   38.05   47.14 1414.29    2660
var_lab(dat$sugarbeverage) = "Sugar sweetened beverages (g/day)"

### Sweets & desserts
#### c232 - Pudding: 14 + 28 + 18 + 18 + 20 + 25 = 123g
#### c236 - Cakes: 6 + 14 + 12 + 15 + 9 = 56g
#### c238 - Biscuits: 3 + 2 + 3 + 6 = 14g
#### c239 - Chocolate bars: 33 + 28 = 61g
#### c245 - Chocolate: 10 + 30 + 10 = 50g
#### c246 - Sweets: 6 + 7 + 5 + 9 + 5 + 8 = 40g
dat$sweetdessert <-
  dat$c232 * 123 +
  dat$c236 * 56 +
  dat$c238 * 14 +
  dat$c239 * 61 +
  dat$c245 * 50 +
  dat$c246 * 40

dat$sweetdessert <- dat$sweetdessert / 7

summary(dat$sweetdessert)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   34.57   60.71   72.16   95.57  491.43    2640
var_lab(dat$sweetdessert) = "Sweets and desserts (g/day)"

################################################################################

## Animal food groups - 6

### Animal fat (already derived along with vegetable oils above)
summary(dat$animalfat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   42.86   66.23   72.18   96.83  393.65    2721
var_lab(dat$animalfat) = "Animal fat (g/day)"

### Dairy

#### Milk (estimated based on types of milk consumed) - !!! NOT based on Giulia's script, which used sources of milk (e.g., in tea/coffee/breakfast cereal etc.); mainly based on Pauline's original script
##### c276 - Whole milk: 100g
##### c277 - Semi-skimmed milk: 100g
##### c278 - Skimmed milk: 100g
##### c279 - Sterilised milk: 100g
##### c280 - Dried milk (NOT in the "Foods within items" sheet - NOT USED)
##### c281 - Goat/sheep milk: 50 + 50 = 100g
#####! c282 - Soya milk: 100g (counted as legumes)
milk_portion <-
  matrix(c(100, 100, 100, 100, 100, 100), 6, 1)  # Vector of frequency for each type of milk (c276-c282 excluding c280)

milk_Matrix <-
  as.matrix(dat[, c("c276", "c277", "c278", "c279", "c281", "c282")])  # Matrix of milk types consumed: rows for observations, 6 columns for milk types; matrix contains 1 if the type is consumed, 0 otherwise; NO CAP for total intake (unlike bread)!!!

dat$milk <-
  milk_Matrix[, 1:5] %*% milk_portion[1:5, ]  # Calculate total portion size of animal-sourced milk (whole milk + semi-skimmed milk + skimmed milk + sterilised milk + goat/sheep milk) consumed by each individual
dat$milk <- as.numeric(dat$milk)
dat$milk
str(dat$milk)

dat$soyamilk <-
  milk_Matrix[, 6] * milk_portion[6, ]  # Calculate the portion size of plant-sourced milk (soya milk) consumed by each individual
dat$soyamilk
str(dat$soyamilk)

dat$milk  # Daily intake of (animal-sourced) milk
summary(dat$milk)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  0.0   100.0   100.0   149.9   200.0   500.0    2617

dat$soyamilk  # Daily intake of soya milk
summary(dat$soyamilk)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00    0.00    0.00    1.45    0.00  100.00    2617

#### Other dairy (weekly intakes need to be converted into daily intakes):
##### c209 - Cheese: 23 + 23 = 46g
dat$dairy <- (dat$c209 * 46 / 7) + dat$milk

summary(dat$dairy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  0.0   113.1   136.1   170.9   213.1   565.7    2652
var_lab(dat$dairy) = "Dairy (g/day)"

################################################################################
#### Add soya milk to legumes
dat$legume <- dat$legume + dat$soyamilk

summary(dat$legume)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   24.79   53.71   49.33   53.71  559.43    2640
################################################################################

### Eggs
#### c208 - Eggs/quiche: 17 + 40 + 47 = 104g
dat$egg <- dat$c208 * 104

dat$egg <- dat$egg / 7

summary(dat$egg)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   7.429  29.714  20.772  29.714 148.571    2652
var_lab(dat$egg) = "Egg (g/day)"

### Fish & seafood
#### c205 - White fish: 30 + 30 + 50 + 20 = 130g
#### c206 - Oily fish: 20 + 22 + 27 + 18 + 17 + 15 = 119g
#### c207 - Shellfish: 21 + 15 + 43 + 10 = 89g
dat$fishseafood <-
  dat$c205 * 130 +
  dat$c206 * 119 +
  dat$c207 * 89

dat$fishseafood <- dat$fishseafood / 7

summary(dat$fishseafood)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   9.286  24.143  34.015  45.643 482.857    2652
var_lab(dat$fishseafood) = "Fish and seafood (g/day)"

### Meat
#### c200 - Sausages/burgers: 40 + 40 = 80g
#### c201 - Meat pies: 39 + 35 + 15 + 40 = 129g
#### c202 - Red meat: 23 + 23 + 30 + 12 = 88g
#### c203 - Poultry: 100g
#### c204 - Offal: 25 + 28 + 25 + 20 = 98g
dat$meat <-
  dat$c200 * 80 +
  dat$c201 * 129 +
  dat$c202 * 88 +
  dat$c203 * 100 +
  dat$c204 * 98

dat$meat <- dat$meat / 7

summary(dat$meat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00   47.21   66.43   71.02   96.29  567.14    2652
var_lab(dat$meat) = "Meat (g/day)"

### Miscellaneous animal-based foods (pizza as an example for this food group given by Satija et al.; not decomposing it into cereals, vegetables, and dairy as in Giulia's script)
#### c210 - Pizza: 115 + 115 = 230g
dat$misc.animal <- dat$c210 * 230

dat$misc.animal <- dat$misc.animal / 7

summary(dat$misc.animal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00    0.00   16.43   16.33   16.43  328.57    2652
var_lab(dat$misc.animal) = "Miscellaneous animal-based foods (g/day)"

################################################################################
### Weekly intake frequency (times/week) of meat & poultry, fish & seafood, egg, and dairy - Used for deriving vegetarian subgroups

#### Meat & poultry
##### c200 - Sausages/burgers
##### c201 - Meat pies
##### c202 - Red meat
##### c203 - Poultry
##### c204 - Offal
dat$meat_freq <-
  rowSums(dat[, c("c200", "c201", "c202", "c203", "c204")])  # No need for "* 7" or "/ 7" as it is already weekly intake frequency
summary(dat$meat_freq)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   3.500   5.000   5.209   6.500  40.000    2652

################################################################################
#### !!! Poultry only - For deriving pollo-vegetarians - !!! NOT considered in the final analysis !!!
dat$poultry_freq <-
  dat$c203  # No need for "* 7" or "/ 7" as it is already weekly intake frequency
summary(dat$poultry_freq)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   0.500   2.000   1.663   2.000  10.000    2652

#### !!! Meat excluding poultry - For deriving pollo-vegetarians - !!! NOT considered in the final analysis !!!
dat$meat_excl.poultry_freq <-
  rowSums(dat[, c("c200", "c201", "c202", "c204")])  # No need for "* 7" or "/ 7" as it is already weekly intake frequency
summary(dat$meat_excl.poultry_freq)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   2.000   3.000   3.546   5.500  30.500    2652
################################################################################

#### Fish & seafood
##### c205 - White fish
##### c206 - Oily fish
##### c207 - Shellfish
dat$fishseafood_freq <-
  rowSums(dat[, c("c205", "c206", "c207")])  # No need for "* 7" or "/ 7" as it is already weekly intake frequency
summary(dat$fishseafood_freq)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   0.500   1.500   1.941   2.500  30.000    2652

#### Egg
##### c208 - Eggs/quiche
dat$egg_freq <-
  dat$c208  # No need for "* 7" or "/ 7" as it is already weekly intake frequency
summary(dat$egg_freq)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.000   0.500   2.000   1.398   2.000  10.000    2652

#### Dairy
##### c276 - Whole milk
##### c277 - Semi-skimmed milk
##### c278 - Skimmed milk
##### c279 - Sterilised milk
##### c280 - Dried milk (NOT in the "Foods within items" sheet - NOT USED)
##### c281 - Goat/sheep milk
##### c209 - Cheese
dat$milk_freq <-
  rowSums(dat[, c("c276", "c277", "c278", "c279", "c281")], na.rm = T)  # Need to * 7 to get weekly intake frequency
dat$dairy_freq <-
  dat$milk_freq * 7 + dat$c209  # No need for "* 7" or "/ 7" as it is already weekly intake frequency
summary(dat$dairy_freq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00    9.00   12.50   13.68   16.00   45.00    2652
################################################################################

food_group_name <- c(
  "wholegrain",
  "fruit",
  "vegetable",
  "nut",
  "legume",
  "vegetableoil",
  "teacoffee",
  "fruitjuice",
  "refinedgrain",
  "potato",
  "sugarbeverage",
  "sweetdessert",
  "animalfat",
  "dairy",
  "egg",
  "fishseafood",
  "meat",
  "misc.animal"
)

# for (var_name in food_group_name) {
#   print(dat[, var_name])
# }
# dat$meat_freq
# dat$poultry_freq  # !!! NOT considered in the final analysis !!!
# dat$meat_excl.poultry_freq  # !!! NOT considered in the final analysis !!!
# dat$fishseafood_freq
# dat$egg_freq
# dat$dairy_freq

head(dat)
dim(dat)  # 14345   XXX

#------------------------------------------------------------------------------#
#                           Derive Dietary Exposures                           #----
#------------------------------------------------------------------------------#

# Summarise data

## Create function to extract quantile cutoffs
show_quantile_values <- function(var_name) {
  my_var <- dat[, var_name]
  
  x <- quantile(
    my_var,
    probs = c(
      0,
      0.1,
      0.2,
      0.25,
      0.3,
      0.333,
      0.4,
      0.5,
      0.6,
      0.667,
      0.7,
      0.75,
      0.8,
      0.9,
      1
    ),
    na.rm = T
  )
  y <- data.frame(
    "Food_group" = var_name,
    "N" = length(na.omit(my_var)),
    `Min` = x[1],
    `P10` = x[2],
    `P20` = x[3],
    `P25` = x[4],
    `P30` = x[5],
    `P33.3` = x[6],
    `P40` = x[7],
    `P50` = x[8],
    `P60` = x[9],
    `P66.7` = x[10],
    `P70` = x[11],
    `P75` = x[12],
    `P80` = x[13],
    `P90` = x[14],
    `Max` = x[15]
  )
  
  return(y)
}

## Apply function to extract quantile cutoffs for all 18 food groups
dat <-
  as.data.frame(dat)  # Converted to dataframe for subsequent analyses

Qx_value_sub <- data.frame(matrix(ncol = 17, nrow = 0))

for (var_name in food_group_name) {
  x <- show_quantile_values(var_name)
  Qx_value_sub <- rbind(Qx_value_sub, x)
}

print(Qx_value_sub, row.names = F)  # !!! NOT enough granularity to distinguish between quintiles (5Q) - Use tertiles instead

## Save results
write.csv(Qx_value_sub,
          "results/ALSPAC/food_groups_distribution.csv",
          row.names = F)

################################################################################

# Calculate tertiles

## Positive and negative scoring

### Create function for positive scoring
p_score_3 <- function(var_name) {
  my_var <- dat[, var_name]
  
  #### First obtain the cutoffs for tertiles
  P0 <- min(my_var, na.rm = T)
  P33.3 <- quantile(my_var, probs = 0.333, na.rm = T)
  P66.7 <- quantile(my_var, probs = 0.667, na.rm = T)
  P100 <- max(my_var, na.rm = T)
  
  #### When >33.3% with the same lowest intake (e.g. 0)
  if (P0 == P33.3) {
    ##### Create a warning message and introduce the solution
    writeLines(
      paste0(
        "Warning for ",
        var_name,
        ": >33.3% with the same lowest intake (",
        P0,
        ");\nSolution: Set all of the lowest as Q1, then take the median of the rest to split Q2 and Q3."
      )
    )
    
    ##### Modify the range of tertiles and assign positive scores (1-3)
    dat[, "my_var_3"] <-
      NA  # Create an empty column
    
    dat[which(my_var == P0), "my_var_3"] <-
      1  # Set all of those with the lowest intake as Q1
    dat[which(my_var > P0 &
                my_var <= median(my_var[which(my_var > P0)], na.rm = T)), "my_var_3"] <-
      2  # For the rest, set those with intake <=median as Q2
    dat[which(my_var > median(my_var[which(my_var > P0)], na.rm = T)), "my_var_3"] <-
      3  # And then set those with intake >median as Q3
    
    my_var_3 <-
      c(dat[, "my_var_3"])  # Convert column into vector for subsequent combination
    
    dat <-
      subset(dat, select = -c(my_var_3))  # Remove the newly created column from the main dataset
  }
  
  #### Other situations - Take tertiles and assign positive scores
  else {
    my_var_3 <-
      findInterval(my_var, c(-Inf, quantile(
        my_var, probs = c(0.333, 0.667), na.rm = T
      ), Inf))
  }
  
  return(my_var_3)
}

### Create function for negative scoring
r_score_3 <- function(var_name) {
  my_var_3 <-
    p_score_3(var_name)  # Same as above for setting tertiles
  
  my_var_3 <- dplyr::recode(my_var_3,
                            "1" = 3,
                            "2" = 2,
                            "3" = 1)  # Recoded as reverse scores
  
  return(my_var_3)
}

################################################################################

## Create tertile variables for all 18 food groups
for (var_name in food_group_name) {
  assign(paste0(var_name, "_3"), factor(p_score_3(var_name), label = c("T1", "T2", "T3")))
  
  dat <- cbind(dat, get(paste0(var_name, "_3")))
}

for (i in 1:18) {
  colnames(dat)[ncol(dat) - 18 + i] <-
    paste0(food_group_name[i], "_3")
}

head(dat)
dim(dat)  # 14345

################################################################################

## Extract tertile cutoffs for all 18 food groups

### Create function
tertile_cutoff <- function(var_name) {
  my_var <- dat[, var_name]
  
  my_var_3 <- p_score_3(var_name)
  
  T1_lo <-
    round(min(my_var[which(my_var_3 == 1)], na.rm = T), digits = 2)
  T1_up <-
    round(max(my_var[which(my_var_3 == 1)], na.rm = T), digits = 2)
  
  T2_lo <-
    round(min(my_var[which(my_var_3 == 2)], na.rm = T), digits = 2)
  T2_up <-
    round(max(my_var[which(my_var_3 == 2)], na.rm = T), digits = 2)
  
  T3_lo <-
    round(min(my_var[which(my_var_3 == 3)], na.rm = T), digits = 2)
  T3_up <-
    round(max(my_var[which(my_var_3 == 3)], na.rm = T), digits = 2)
  
  x <- paste0("T1: ",
              T1_lo,
              "-",
              T1_up,
              " | T2: ",
              T2_lo,
              "-",
              T2_up,
              " | T3: ",
              T3_lo,
              "-",
              T3_up)
  
  return(x)
}

describe_food_group <- function(var_name) {
  x <- c(
    `Food group` = var_name,
    "Cutoffs" = tertile_cutoff(var_name),
    table(factor(
      p_score_3(var_name), label = c("T1", "T2", "T3")
    ), useNA = "always")
  )
  
  return(x)
}

### Apply function
food_group_3_tab <- data.frame(matrix(ncol = 6, nrow = 0))

for (var_name in food_group_name) {
  x <- describe_food_group(var_name)
  
  food_group_3_tab <- rbind(food_group_3_tab, x)
}

colnames(food_group_3_tab) <-
  c("Food_group", "Cutoffs", "N_T1", "N_T2", "N_T3", "N_missing")

food_group_3_tab

### Save results
write.csv(food_group_3_tab,
          "results/ALSPAC/food_groups_cutoffs.csv",
          row.names = F)

################################################################################

# Calculation of plant-based diet indices

## Overall plant-based diet index (PDI)
dat$PDI <-
  ### 7 healthy plant food groups assigned positive scores
  p_score_3("wholegrain") +
  p_score_3("fruit") +
  p_score_3("vegetable") +
  p_score_3("nut") +
  p_score_3("legume") +
  p_score_3("vegetableoil") +
  p_score_3("teacoffee") +
  
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3("fruitjuice") +
  p_score_3("refinedgrain") +
  p_score_3("potato") +
  p_score_3("sugarbeverage") +
  p_score_3("sweetdessert") +
  
  ### 6 animal food groups assigned reverse scores
  r_score_3("animalfat") +
  r_score_3("dairy") +
  r_score_3("egg") +
  r_score_3("fishseafood") +
  r_score_3("meat") +
  r_score_3("misc.animal")

dat$PDI
summary(dat$PDI)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 24.0    33.0    35.0    35.4    38.0    47.0    2756

## Healthful plant-based diet index (hPDI)
dat$hPDI <-
  ### 7 healthy plant food groups assigned positive scores
  p_score_3("wholegrain") +
  p_score_3("fruit") +
  p_score_3("vegetable") +
  p_score_3("nut") +
  p_score_3("legume") +
  p_score_3("vegetableoil") +
  p_score_3("teacoffee") +
  
  ### 5 less healthy plant food groups assigned reverse scores
  r_score_3("fruitjuice") +
  r_score_3("refinedgrain") +
  r_score_3("potato") +
  r_score_3("sugarbeverage") +
  r_score_3("sweetdessert") +
  
  ### 6 animal food groups assigned reverse scores
  r_score_3("animalfat") +
  r_score_3("dairy") +
  r_score_3("egg") +
  r_score_3("fishseafood") +
  r_score_3("meat") +
  r_score_3("misc.animal")

dat$hPDI
summary(dat$hPDI)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 23.00   34.00   36.00   36.23   39.00   49.00    2756

## Unhealthful plant-based diet index (uPDI)
dat$uPDI <-
  ### 7 healthy plant food groups assigned reverse scores
  r_score_3("wholegrain") +
  r_score_3("fruit") +
  r_score_3("vegetable") +
  r_score_3("nut") +
  r_score_3("legume") +
  r_score_3("vegetableoil") +
  r_score_3("teacoffee") +
  
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3("fruitjuice") +
  p_score_3("refinedgrain") +
  p_score_3("potato") +
  p_score_3("sugarbeverage") +
  p_score_3("sweetdessert") +
  
  ### 6 animal food groups assigned reverse scores
  r_score_3("animalfat") +
  r_score_3("dairy") +
  r_score_3("egg") +
  r_score_3("fishseafood") +
  r_score_3("meat") +
  r_score_3("misc.animal")

dat$uPDI
summary(dat$uPDI)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 23.00   32.00   35.00   34.79   37.00   48.00    2756

# # Check the distribution of PDIs
#
# ## PDI
# png("results/ALSPAC/histogram_PDI.png",
#     height = 500,
#     width = 800)
#
# hist(
#   dat$PDI,
#   freq = F,
#   breaks = 20,
#   main = paste0("Histogram of PDI (N = ", sum(is.na(dat$PDI) == F), ")"),
#   xlab = "PDI"
# )
#
# dev.off()
#
# ## hPDI
# png("results/ALSPAC/histogram_hPDI.png",
#     height = 500,
#     width = 800)
#
# hist(
#   dat$hPDI,
#   freq = F,
#   breaks = 20,
#   main = paste0("Histogram of hPDI (N = ", sum(is.na(dat$hPDI) == F), ")"),
#   xlab = "hPDI"
# )
#
# dev.off()
#
# ## uPDI
# png("results/ALSPAC/histogram_uPDI.png",
#     height = 500,
#     width = 800)
#
# hist(
#   dat$uPDI,
#   freq = F,
#   breaks = 20,
#   main = paste0("Histogram of uPDI (N = ", sum(is.na(dat$uPDI) == F), ")"),
#   xlab = "uPDI"
# )
#
# dev.off()

# Histograms of PDI/hPDI/uPDI
for (my_exp in c("PDI", "hPDI", "uPDI")) {
  jpeg(paste0("results/ALSPAC/", "histogram_", my_exp, ".jpg"))
  hist(
    dat[, my_exp],
    freq = F,
    main = paste0("Histogram of ", my_exp, " (N = ", sum(is.na(dat[, my_exp]) == F), ")"),
    xlab = my_exp
  )
  dev.off()
}

################################################################################

# PDI quintiles

## PDI
dat$PDI_5Q <-
  findInterval(dat$PDI, c(-Inf, quantile(
    dat$PDI, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = T
  ), Inf))


for (i in 1:5) {
  assign(paste0("T", i, "_lab"),
         paste0(
           "Quintile ",
           i,
           " (",
           min(dat$PDI[which(dat$PDI_5Q == i)], na.rm = T),
           "-",
           max(dat$PDI[which(dat$PDI_5Q == i)], na.rm = T),
           ")"
         ))
}

dat$PDI_5Q[which(dat$PDI_5Q == 1)] <- T1_lab
dat$PDI_5Q[which(dat$PDI_5Q == 2)] <- T2_lab
dat$PDI_5Q[which(dat$PDI_5Q == 3)] <- T3_lab
dat$PDI_5Q[which(dat$PDI_5Q == 4)] <- T4_lab
dat$PDI_5Q[which(dat$PDI_5Q == 5)] <- T5_lab

dat$PDI_5Q <-
  factor(dat$PDI_5Q, levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$PDI_5Q) = "Quintiles of PDI"

str(dat$PDI_5Q)
CrossTable(dat$PDI_5Q)

################################################################################
################################################################################
################################################################################

## hPDI
dat$hPDI_5Q <-
  findInterval(dat$hPDI, c(-Inf, quantile(
    dat$hPDI,
    probs = c(0.2, 0.4, 0.6, 0.8),
    na.rm = T
  ), Inf))

for (i in 1:5) {
  assign(paste0("T", i, "_lab"),
         paste0(
           "Quintile ",
           i,
           " (",
           min(dat$hPDI[which(dat$hPDI_5Q == i)], na.rm = T),
           "-",
           max(dat$hPDI[which(dat$hPDI_5Q == i)], na.rm = T),
           ")"
         ))
}

dat$hPDI_5Q[which(dat$hPDI_5Q == 1)] <- T1_lab
dat$hPDI_5Q[which(dat$hPDI_5Q == 2)] <- T2_lab
dat$hPDI_5Q[which(dat$hPDI_5Q == 3)] <- T3_lab
dat$hPDI_5Q[which(dat$hPDI_5Q == 4)] <- T4_lab
dat$hPDI_5Q[which(dat$hPDI_5Q == 5)] <- T5_lab

dat$hPDI_5Q <-
  factor(dat$hPDI_5Q, levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$hPDI_5Q) = "Quintiles of hPDI"

str(dat$hPDI_5Q)
CrossTable(dat$hPDI_5Q)

################################################################################
################################################################################
################################################################################

## uPDI
dat$uPDI_5Q <-
  findInterval(dat$uPDI, c(-Inf, quantile(
    dat$uPDI,
    probs = c(0.2, 0.4, 0.6, 0.8),
    na.rm = T
  ), Inf))

for (i in 1:5) {
  assign(paste0("T", i, "_lab"),
         paste0(
           "Quintile ",
           i,
           " (",
           min(dat$uPDI[which(dat$uPDI_5Q == i)], na.rm = T),
           "-",
           max(dat$uPDI[which(dat$uPDI_5Q == i)], na.rm = T),
           ")"
         ))
}

dat$uPDI_5Q[which(dat$uPDI_5Q == 1)] <- T1_lab
dat$uPDI_5Q[which(dat$uPDI_5Q == 2)] <- T2_lab
dat$uPDI_5Q[which(dat$uPDI_5Q == 3)] <- T3_lab
dat$uPDI_5Q[which(dat$uPDI_5Q == 4)] <- T4_lab
dat$uPDI_5Q[which(dat$uPDI_5Q == 5)] <- T5_lab

dat$uPDI_5Q <-
  factor(dat$uPDI_5Q, levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$uPDI_5Q) = "Quintiles of uPDI"

str(dat$uPDI_5Q)
CrossTable(dat$uPDI_5Q)

################################################################################

# Diet-based vegetarianism (vegetarian subgroups)

## Data preparation: Generate weekly intake frequency of meat & poultry, fish & seafood, egg, and dairy
dat$meat_freq  # Weekly intake frequency (times/week) of meat & poultry
dat$poultry_freq  # Weekly intake frequency (times/week) of poultry only - !!! NOT considered in the final analysis !!!
dat$meat_excl.poultry_freq  # Weekly intake frequency (times/week) of meat excluding poultry - !!! NOT considered in the final analysis !!!
dat$fishseafood_freq  # Weekly intake frequency (times/week) of fish & seafood
dat$egg_freq  # Weekly intake frequency (times/week) of egg
dat$dairy_freq  # Weekly intake frequency (times/week) of dairy

dat$meatfish_freq <-
  rowSums(dat[, c("meat_freq", "fishseafood_freq")])  # Weekly intake frequency (times/week) of meat & poultry + fish & seafood
dat$meatfish_excl.poultry_freq <-
  rowSums(dat[, c("meat_excl.poultry_freq", "fishseafood_freq")])  # Weekly intake frequency (times/week) of meat (excluding poultry) + fish & seafood - !!! NOT considered in the final analysis !!!
dat$eggdairy_freq <-
  rowSums(dat[, c("egg_freq", "dairy_freq")])  # Weekly intake frequency (times/week) of egg + dairy
dat$animal_freq <-
  rowSums(dat[, c("meat_freq", "fishseafood_freq", "egg_freq", "dairy_freq")])  # Weekly intake frequency (times/week) of meat & poultry + fish & seafood + egg + dairy

## Vegetarian subgroups (detailed) - Coded from the least to the most restricted subgroup

### 0 - Non-vegetarian - meat & poultry: no restriction; fish & seafood: no restriction; egg & dairy: no restriction
dat$VegDiet_subgroup <- 0
dat$VegDiet_subgroup[is.na(dat$meat_freq) |
                       is.na(dat$fishseafood_freq) |
                       is.na(dat$egg_freq) |
                       is.na(dat$dairy_freq)] <- NA

### 1 - Pesco-vegetarian - meat & poultry < 1 time/month (i.e., 0.25 time/week); fish & seafood >= 1 time/month; egg & dairy: no restriction
dat$VegDiet_subgroup[dat$meat_freq < 0.25 &
                       dat$fishseafood_freq >= 0.25] <- 1

### 2 - Lacto-ovo-vegetarian - meat & poultry + fish & seafood < 1 time/month; egg + dairy >= 1 time/month
dat$VegDiet_subgroup[dat$meatfish_freq < 0.25 &
                       dat$eggdairy_freq >= 0.25] <- 2

### 3 - Vegan - meat & poultry + fish & seafood + egg + dairy < 1 time/month
dat$VegDiet_subgroup[dat$animal_freq < 0.25] <- 3

## Vegetarian subgroups (combined into 3 categories) - Used for surrogate variable analysis below
### 0 - Non-vegetarian
### 1 - Pesco-vegetarian
### 2 - Full vegetarian (including lacto-ovo-vegetarian & vegan)
dat$VegDiet_3cat <- NA
dat$VegDiet_3cat[dat$VegDiet_subgroup == 0] <- 0
dat$VegDiet_3cat[dat$VegDiet_subgroup == 1] <- 1
dat$VegDiet_3cat[dat$VegDiet_subgroup %in% c(2, 3)] <- 2

## Vegetarianism (binary)
### 0 - Non-vegetarian
### 1 - Full vegetarian + pesco-vegetarian
dat$VegDiet_bin <- NA
dat$VegDiet_bin[dat$VegDiet_3cat == 0] <- 0
dat$VegDiet_bin[dat$VegDiet_3cat %in% c(1, 2)] <- 1

## Check the numbers
table(dat$VegDiet_subgroup)
table(dat$VegDiet_3cat)
table(dat$VegDiet_bin)

# Descriptive results
val_lab(dat$VegDiet_subgroup) = num_lab("
                                        0 Non-vegetarian
                                        1 Pesco-vegetarian
                                        2 Lacto-ovo-vegetarian
                                        3 Vegan
                                        ")
str(dat$VegDiet_subgroup)
CrossTable(dat$VegDiet_subgroup)  # 568 "vegetarians" = 244 full-vegetarians (8 vegans) + 324 pesco-vegetarians
# |       Non-vegetarian |     Pesco-vegetarian | Lacto-ovo-vegetarian |                Vegan |
# |----------------------|----------------------|----------------------|----------------------|
# |                11125 |                  324 |                  236 |                    8 |
# |                0.951 |                0.028 |                0.020 |                0.001 |
# |----------------------|----------------------|----------------------|----------------------|

val_lab(dat$VegDiet_3cat) = num_lab("
                                    0 Non-vegetarian
                                    1 Pesco-vegetarian
                                    2 Full vegetarian
                                    ")
str(dat$VegDiet_3cat)
CrossTable(dat$VegDiet_3cat)  # 568 "vegetarians" = 244 full-vegetarians (8 vegans) + 324 pesco-vegetarians
# |   Non-vegetarian | Pesco-vegetarian |  Full vegetarian |
# |------------------|------------------|------------------|
# |            11125 |              324 |              244 |
# |            0.951 |            0.028 |            0.021 |
# |------------------|------------------|------------------|

val_lab(dat$VegDiet_bin) = num_lab("
                                   0 Non-vegetarian
                                   1 Vegetarian
                                   ")
str(dat$VegDiet_bin)
CrossTable(dat$VegDiet_bin)  # 568 "vegetarians" = 244 full-vegetarians (8 vegans) + 324 pesco-vegetarians
# | Non-vegetarian |     Vegetarian |
# |----------------|----------------|
# |          11125 |            568 |
# |          0.951 |          0.049 |
# |----------------|----------------|

################################################################################

## Summary
dat <- as.tibble(dat)  # Converted back to tibble

dat$VegDiet_subgroup <- as.factor(dat$VegDiet_subgroup)
var_lab(dat$VegDiet_subgroup) = "Diet-based vegetarianism (subgroups) during pregnancy"
levels(dat$VegDiet_subgroup)

dat$VegDiet_3cat <- as.factor(dat$VegDiet_3cat)
var_lab(dat$VegDiet_3cat) = "Diet-based vegetarianism (3 categories) during pregnancy"
levels(dat$VegDiet_3cat)

dat$VegDiet_bin <- as.factor(dat$VegDiet_bin)
var_lab(dat$VegDiet_bin) = "Diet-based vegetarianism (binary) during pregnancy"
levels(dat$VegDiet_bin)

var_lab(dat$PDI) = "Overall plant-based diet index (PDI)"
var_lab(dat$hPDI) = "Healthful plant-based diet index (hPDI)"
var_lab(dat$uPDI) = "Unhealthful plant-based diet index (uPDI)"

################################################################################

# Standardise PDIs - Per 1 SD increase
dat$PDI_z <- as.vector(scale(dat$PDI))
var_lab(dat$PDI_z) = "Standardised overall plant-based diet index (PDI)"

dat$hPDI_z <- as.vector(scale(dat$hPDI))
var_lab(dat$hPDI_z) = "Standardised healthful plant-based diet index (hPDI)"

dat$uPDI_z <- as.vector(scale(dat$uPDI))
var_lab(dat$uPDI_z) = "Standardised unhealthful plant-based diet index (uPDI)"

################################################################################
# !!! Diet-based + self-defined vegetarianism !!!
dat <- dat %>% mutate(
  VegDiet_bin_FFQ_self = case_when(
    VegDiet_bin == "Non-vegetarian" &
      self.VegDiet_Mat_DUR.p_bin == "Non-vegetarian" ~ 0,
    VegDiet_bin == "Vegetarian" &
      self.VegDiet_Mat_DUR.p_bin == "Vegetarian" ~ 1,
    TRUE ~ NA_real_
  )
)

val_lab(dat$VegDiet_bin_FFQ_self) = c("Non-vegetarian" = 0, "Vegetarian" = 1)

dat$VegDiet_bin_FFQ_self <-
  as.factor(dat$VegDiet_bin_FFQ_self)
var_lab(dat$VegDiet_bin_FFQ_self) <- "Diet-based + self-defined vegetarianism (binary) during pregnancy"

table(dat$VegDiet_bin_FFQ_self, useNA = "always")
################################################################################

################################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Restrict to those without missing data in vegetarian diet variables
dat <- dat[!is.na(dat$VegDiet_3cat), ]  # 14345 -> 11693
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
################################################################################

################################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Calculation of MODIFIED plant-based diet indices

dat <- as.data.frame(dat)

## MODIFIED overall plant-based diet index (PDIm)
dat$PDIm <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3("wholegrain") +
  p_score_3("fruit") +
  p_score_3("vegetable") +
  p_score_3("nut") +
  p_score_3("legume") +
  p_score_3("vegetableoil") +
  
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3("fruitjuice") +
  p_score_3("refinedgrain") +
  p_score_3("potato") +
  p_score_3("sugarbeverage") +
  p_score_3("sweetdessert") +
  
  ### 6 animal food groups assigned reverse scores
  r_score_3("animalfat") +
  r_score_3("dairy") +
  r_score_3("egg") +
  r_score_3("fishseafood") +
  r_score_3("meat") +
  r_score_3("misc.animal")

dat$PDIm
summary(dat$PDIm)

## MODIFIED healthful plant-based diet index (hPDIm)
dat$hPDIm <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3("wholegrain") +
  p_score_3("fruit") +
  p_score_3("vegetable") +
  p_score_3("nut") +
  p_score_3("legume") +
  p_score_3("vegetableoil") +
  
  ### 5 less healthy plant food groups assigned reverse scores
  r_score_3("fruitjuice") +
  r_score_3("refinedgrain") +
  r_score_3("potato") +
  r_score_3("sugarbeverage") +
  r_score_3("sweetdessert") +
  
  ### 6 animal food groups assigned reverse scores
  r_score_3("animalfat") +
  r_score_3("dairy") +
  r_score_3("egg") +
  r_score_3("fishseafood") +
  r_score_3("meat") +
  r_score_3("misc.animal")

dat$hPDIm
summary(dat$hPDIm)

## MODIFIED unhealthful plant-based diet index (uPDIm)
dat$uPDIm <-
  ### 6 healthy plant food groups assigned reverse scores (excluding tea and coffee)
  r_score_3("wholegrain") +
  r_score_3("fruit") +
  r_score_3("vegetable") +
  r_score_3("nut") +
  r_score_3("legume") +
  r_score_3("vegetableoil") +
  
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3("fruitjuice") +
  p_score_3("refinedgrain") +
  p_score_3("potato") +
  p_score_3("sugarbeverage") +
  p_score_3("sweetdessert") +
  
  ### 6 animal food groups assigned reverse scores
  r_score_3("animalfat") +
  r_score_3("dairy") +
  r_score_3("egg") +
  r_score_3("fishseafood") +
  r_score_3("meat") +
  r_score_3("misc.animal")

dat$uPDIm
summary(dat$uPDIm)

################################################################################

## Summary
dat <- as.tibble(dat)  # Converted back to tibble

var_lab(dat$PDIm) = "MODIFIED overall plant-based diet index (PDIm)"
var_lab(dat$hPDIm) = "MODIFIED healthful plant-based diet index (hPDIm)"
var_lab(dat$uPDIm) = "MODIFIED unhealthful plant-based diet index (uPDIm)"

################################################################################

# Standardise PDIs - Per 1 SD increase
dat$PDIm_z <- as.vector(scale(dat$PDIm))
var_lab(dat$PDIm_z) = "Standardised MODIFIED overall plant-based diet index (PDIm)"

dat$hPDIm_z <- as.vector(scale(dat$hPDIm))
var_lab(dat$hPDIm_z) = "Standardised MODIFIED healthful plant-based diet index (hPDIm)"

dat$uPDIm_z <- as.vector(scale(dat$uPDIm))
var_lab(dat$uPDIm_z) = "Standardised MODIFIED unhealthful plant-based diet index (uPDIm)"

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
################################################################################

dat <- as.tibble(dat)
head(dat)
dim(dat)  # 11693   XXX

saveRDS(dat, "data/ALSPAC/dat_exp_cov_out.rds")
