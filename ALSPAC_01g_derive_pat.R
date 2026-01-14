################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 24-Jul-2024
# This script is to derive paternal dietary exposure variables and covariates for paternal negative control analysis in ALSPAC.

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
  openxlsx,
  tableone
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/ALSPAC/dat_exp_cov_out.rds")
head(dat)
dim(dat)  # 11693   XXX

################################################################################

# Paternal diet-based vegetarianism

## Derive the 4 key food groups - meat, fish/seafood, eggs, dairy (in times/week)

### Raw FFQ items

#### PB020 FREQ of eating sausages or burgers
#### PB021 FREQ of eating pies or pasties
#### PB022 FREQ of eating meat
#### PB023 FREQ of eating poultry
#### PB024 FREQ of eating offal

#### PB025 FREQ of eating white fish
#### PB026 FREQ of eating oily fish
#### PB027 FREQ of eating shellfish

#### PB028 FREQ of eating fried food (e.g., fried fish, eggs, bacon, chops, etc)

##### Never or rarely 1
##### Once in 2WKS 2
##### 1-3 times PWK 3
##### 4-7 times PWK 4
##### >=1 per day 5
##### Missing -1

intake_freq_map <- c(
  "1" = 0,
  # Never or rarely
  "2" = 0.5,
  # Once in 2WKS
  "3" = 2,
  # 1-3 times PWK
  "4" = 5.5,
  # 4-7 times PWK
  "5" = 7
  # >=1 per day
)

FFQ_list <-
  c("pb020",
    "pb021",
    "pb022",
    "pb023",
    "pb024",
    "pb025",
    "pb026",
    "pb027",
    "pb028")

for (var in FFQ_list) {
  dat[[var]] <- intake_freq_map[as.character(as.numeric(dat[[var]]))]
  dat[, var][dat[, var] < 0] <- NA
}

sum(dat$pb020 < 0, na.rm = T)
summary(dat$pb020)
summary(dat$pb025)

#### PB047 Milk in tea
#### PB048 Milk in coffee
#### PB049 Milk on breakfast cereal
#### PB050 Milk as pudding
#### PB051 Milk to drink on its own
#### PB052 Milk as a milky drink

##### Yes 1.00
##### No 2.00
##### Missing -1.00

intake_freq_map <- c("1" = 1, # Yes
                     "2" = 0)  # No

FFQ_list <-
  c("pb047", "pb048", "pb049", "pb050", "pb051", "pb052")

for (var in FFQ_list) {
  dat[[var]] <- intake_freq_map[as.character(as.numeric(dat[[var]]))]
  dat[, var][dat[, var] < 0] <- NA
}

sum(dat$pb047 < 0, na.rm = T)
summary(dat$pb047)

################################################################################
################################################################################

### Meat
dat$meat_freq_Pat <- dat$pb020 + dat$pb021 + dat$pb022 + dat$pb023 + dat$pb024
dat$meat_freq_Pat

### Fish/seafood
dat$fishseafood_freq_Pat <- dat$pb025 + dat$pb026 + dat$pb027
dat$fishseafood_freq_Pat

### Eggs
dat$egg_freq_Pat <- dat$pb028
dat$egg_freq_Pat

### Dairy
dat$dairy_freq_Pat <- dat$pb047 + dat$pb048 + dat$pb049 + dat$pb050 + dat$pb051 + dat$pb052
dat$dairy_freq_Pat

################################################################################
################################################################################

## Derive paternal Vegetarian diets

### Calculate intake frequencies
dat$meatfish_freq_Pat <-
  rowSums(dat[, c("meat_freq_Pat", "fishseafood_freq_Pat")])  # Meat & poultry + fish & seafood
dat$eggdairy_freq_Pat <-
  rowSums(dat[, c("egg_freq_Pat", "dairy_freq_Pat")])  # Egg + dairy
dat$animal_freq_Pat <-
  rowSums(dat[, c("meat_freq_Pat",
                  "fishseafood_freq_Pat",
                  "egg_freq_Pat",
                  "dairy_freq_Pat")])  # Meat & poultry + fish & seafood + egg + dairy (all animal-based foods)

### Vegetarian subgroups (combined into 3 categories) - Coded from the least to the most restricted subgroup

#### 0 - Non-vegetarian - meat & poultry: no restriction; fish & seafood: no restriction; egg & dairy: no restriction
dat$VegDiet_3cat_Pat <- 0
dat$VegDiet_3cat_Pat[is.na(dat$meat_freq_Pat) |
                       is.na(dat$fishseafood_freq_Pat)] <- NA

#### 1 - Pesco-vegetarian - meat & poultry < 1 time/month (i.e., < 0.25 time/week); fish & seafood >= 1 time/month (i.e., >= 0.25 time/week); egg & dairy: no restriction
dat$VegDiet_3cat_Pat[dat$meat_freq_Pat < 0.25 &
                       dat$fishseafood_freq_Pat >= 0.25] <- 1

#### 2 - Full vegetarian - meat & poultry + fish & seafood < 1 time/month; egg & dairy: no restriction
dat$VegDiet_3cat_Pat[dat$meatfish_freq_Pat < 0.25] <- 2

### Vegetarianism (binary)
#### 0 - Non-vegetarian
#### 1 - Full vegetarian + pesco-vegetarian
dat$VegDiet_bin_Pat <- NA
dat$VegDiet_bin_Pat[dat$VegDiet_3cat_Pat == 0] <- 0
dat$VegDiet_bin_Pat[dat$VegDiet_3cat_Pat %in% c(1, 2)] <- 1

### Check the numbers
table(dat$VegDiet_3cat_Pat, useNA = "always")
table(dat$VegDiet_bin_Pat, useNA = "always")

### Descriptive results
val_lab(dat$VegDiet_3cat_Pat) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)
str(dat$VegDiet_3cat_Pat)
CrossTable(dat$VegDiet_3cat_Pat)
# |   Non-vegetarian | Pesco-vegetarian |  Full vegetarian |
# |------------------|------------------|------------------|
# |             8305 |              146 |              106 |
# |            0.971 |            0.017 |            0.012 |
# |------------------|------------------|------------------|

val_lab(dat$VegDiet_bin_Pat) = c("Non-vegetarian" = 0, "Vegetarian" = 1)
str(dat$VegDiet_bin_Pat)
CrossTable(dat$VegDiet_bin_Pat)
# | Non-vegetarian |     Vegetarian |
# |----------------|----------------|
# |           8305 |            252 |
# |          0.971 |          0.029 |
# |----------------|----------------|

################################################################################
################################################################################

## Convert and label vegetarianism variables
dat$VegDiet_3cat_Pat <- as.factor(dat$VegDiet_3cat_Pat)
var_lab(dat$VegDiet_3cat_Pat) = "Paternal diet-based vegetarianism (3 categories) during pregnancy"

dat$VegDiet_bin_Pat <- as.factor(dat$VegDiet_bin_Pat)
var_lab(dat$VegDiet_bin_Pat) = "Paternal diet-based vegetarianism (binary) during pregnancy"

################################################################################

# Paternal self-defined vegetarianism
table(dat$pb069, useNA = "always")
# Y vegetarian 1
# Y vegan 2
# N 3
# Missing -1

## 3 categories - Non-vegetarian, Vegetarian, Vegan
dat$self.VegDiet_Pat_EAR.p_3cat <- NA
dat$self.VegDiet_Pat_EAR.p_3cat[dat$pb069 == 3] <- 0
dat$self.VegDiet_Pat_EAR.p_3cat[dat$pb069 == 1] <- 1
dat$self.VegDiet_Pat_EAR.p_3cat[dat$pb069 == 2] <- 2
dat$self.VegDiet_Pat_EAR.p_3cat[dat$pb069 == -1] <- NA

val_lab(dat$self.VegDiet_Pat_EAR.p_3cat) = c(
  "Non-vegetarian" = 0,
  "Vegetarian" = 1,
  "Vegan" = 2
)

dat$self.VegDiet_Pat_EAR.p_3cat <-
  as.factor(dat$self.VegDiet_Pat_EAR.p_3cat)

var_lab(dat$self.VegDiet_Pat_EAR.p_3cat) = "Paternal self-defined vegetarianism (3 categories) in early pregnancy"

str(dat$self.VegDiet_Pat_EAR.p_3cat)
CrossTable(dat$self.VegDiet_Pat_EAR.p_3cat)

## Binary - Non-vegetarian, Vegetarian
dat$self.VegDiet_Pat_EAR.p_bin <- NA
dat$self.VegDiet_Pat_EAR.p_bin[dat$pb069 == 3] <- 0
dat$self.VegDiet_Pat_EAR.p_bin[dat$pb069 == 1] <- 1
dat$self.VegDiet_Pat_EAR.p_bin[dat$pb069 == 2] <- 1
dat$self.VegDiet_Pat_EAR.p_bin[dat$pb069 == -1] <- NA

val_lab(dat$self.VegDiet_Pat_EAR.p_bin) = c("Non-vegetarian" = 0, "Vegetarian" = 1)

dat$self.VegDiet_Pat_EAR.p_bin <-
  as.factor(dat$self.VegDiet_Pat_EAR.p_bin)

var_lab(dat$self.VegDiet_Pat_EAR.p_bin) = "Paternal self-defined vegetarianism (binary) in early pregnancy"

str(dat$self.VegDiet_Pat_EAR.p_bin)
CrossTable(dat$self.VegDiet_Pat_EAR.p_bin)

################################################################################
################################################################################

CrossTable(
  dat$VegDiet_3cat_Pat,
  dat$self.VegDiet_Pat_EAR.p_bin,
  prop.r = T,
  prop.c = F,
  prop.t = F,
  prop.chisq = F,
  chisq = F
)  # How many diet-based vegetarians defined themselves as "vegetarians"

CrossTable(
  dat$VegDiet_3cat_Pat,
  dat$self.VegDiet_Pat_EAR.p_bin,
  prop.r = F,
  prop.c = T,
  prop.t = F,
  prop.chisq = F,
  chisq = F
)  # How many self-defined "vegetarians" were supported by their FFQ data

CrossTable(
  dat$VegDiet_3cat_Pat,
  dat$self.VegDiet_Pat_EAR.p_bin,
  prop.r = T,
  prop.c = T,
  prop.t = F,
  prop.chisq = F,
  chisq = F
)  # Row %: self-defined in diet-based; column %: diet-based in self-defined

################################################################################

# Paternal age
summary(dat$pb910)  # years

dat$age_Pat_con <- dat$pb910
dat$age_Pat_con[which(dat$age_Pat_con < 0)] <- NA

var_lab(dat$age_Pat_con) = "Paternal age (years) at delivery"

str(dat$age_Pat_con)
summary(dat$age_Pat_con)

# Paternal ethnicity

## Detailed ethnic groups
dat$ethnic_Pat_cat <- NA
dat$ethnic_Pat_cat[which(dat$c801 == 1)] <- "White"
dat$ethnic_Pat_cat[which(dat$c801 %in% c(2, 3, 4))] <- "Black"
dat$ethnic_Pat_cat[which(dat$c801 %in% c(5, 6, 7, 8))] <- "Asian"
dat$ethnic_Pat_cat[which(dat$c801 == 9)] <- "Mixed or Other"

dat$ethnic_Pat_cat <- factor(dat$ethnic_Pat_cat,
                             levels = c("White", "Black", "Asian", "Mixed or Other"))

var_lab(dat$ethnic_Pat_cat) = "Paternal ethnicity (detailed)"

str(dat$ethnic_Pat_cat)
CrossTable(dat$ethnic_Pat_cat)

## Binary - White vs. Other
dat$ethnic_Pat_bin <- NA
dat$ethnic_Pat_bin[which(dat$c801 == 1)] <- "White"
dat$ethnic_Pat_bin[which(dat$c801 > 1)] <- "Other"

dat$ethnic_Pat_bin <- factor(dat$ethnic_Pat_bin, levels = c("White", "Other"))

var_lab(dat$ethnic_Pat_bin) = "Paternal ethnicity (binary)"

str(dat$ethnic_Pat_bin)
CrossTable(dat$ethnic_Pat_bin)

# Paternal education level

## Ordinal / 3 categories
dat$edu_Pat_3cat <-
  ifelse(dat$c666a == 1,
         "Low",
         # Low: None/CSE
         ifelse(
           dat$c666a %in% c(2, 3, 4),
           "Medium",
           # Medium: O-level / A-level / vocational
           ifelse(dat$c666a == 5, "High", # High: Degree
                  NA)
         ))
dat$edu_Pat_3cat <-
  factor(dat$edu_Pat_3cat,
         levels = c("Low", "Medium", "High"),
         ordered = T)  # Set as ordinal variable

var_lab(dat$edu_Pat_3cat) = "Paternal education attainment (3 categories)"

str(dat$edu_Pat_3cat)
CrossTable(dat$edu_Pat_3cat)

## Binary
dat$edu_Pat_bin <-
  ifelse(
    dat$c666a %in% c(1, 2, 3, 4),
    "Lower than college degree",
    # Lower than college degree
    ifelse(dat$c666a == 5, "College degree or higher", # College degree or higher
           NA)
  )

dat$edu_Pat_bin <- factor(dat$edu_Pat_bin,
                          levels = c("Lower than college degree", "College degree or higher"))

var_lab(dat$edu_Pat_bin) = "Paternal education attainment (binary)"

str(dat$edu_Pat_bin)
CrossTable(dat$edu_Pat_bin)

# Index of multiple deprivation (Townsend deprivation index [TDI])
table(dat$IMD_Fam_cat, useNA = "always")

# Paternal "parity" (number of children)
# PA070 Dad & PTNR NP of CH at home
# PA071 Only dad NP of CH at home
dat$parity_Pat_bin <- dat$pa070 + dat$pa071

dat <-
  dat %>% mutate(parity_Pat_bin = case_when(parity_Pat_bin == 0 ~ 0, parity_Pat_bin > 0 ~ 1, TRUE ~ NA_real_))

val_lab(dat$parity_Pat_bin) = c("0" = 0, ">=1" = 1)

dat$parity_Pat_bin <- as.factor(dat$parity_Pat_bin)

var_lab(dat$parity_Pat_bin) = "Paternal number of children (binary)"

str(dat$parity_Pat_bin)
CrossTable(dat$parity_Pat_bin)

# Paternal BMI
dat$height_Pat_EAR.p_con <- dat$paw010
dat$weight_Pat_EAR.p_con <- dat$paw002
dat$height_Pat_EAR.p_con[which(dat$paw010 < 1)] <- NA
dat$weight_Pat_EAR.p_con[which(dat$paw002 < 1)] <- NA
summary(dat$height_Pat_EAR.p_con)
summary(dat$weight_Pat_EAR.p_con)

dat$BMI_Pat_EAR.p_con <- dat$weight_Pat_EAR.p_con / (dat$height_Pat_EAR.p_con /
                                                       100) ^ 2

dat$BMI_Pat_EAR.p_con <-
  as.numeric(dat$BMI_Pat_EAR.p_con)

var_lab(dat$BMI_Pat_EAR.p_con) = "Paternal BMI in early pregnancy (kg/m^2)"

str(dat$BMI_Pat_EAR.p_con)
summary(dat$BMI_Pat_EAR.p_con)

# Paternal smoking
table(dat$pb071, useNA = "always")  # Ever
table(dat$pb079, useNA = "always")  # In last 2 weeks

## 3 categories - Never/Former/Current
dat <-
  dat %>% mutate(
    smoking_Pat_EAR.p_3cat = case_when(
      pb071 == 2 & pb079 == 0 ~ 0,
      pb071 == 1 & pb079 == 0 ~ 1,
      pb079 > 0 ~ 2,
      TRUE ~ NA_real_
    )
  )

val_lab(dat$smoking_Pat_EAR.p_3cat) = c("Never" = 0,
                                        "Former" = 1,
                                        "Current" = 2)

dat$smoking_Pat_EAR.p_3cat <-
  as.factor(dat$smoking_Pat_EAR.p_3cat)

var_lab(dat$smoking_Pat_EAR.p_3cat) = "Paternal smoking in early pregnancy (3 categories)"

str(dat$smoking_Pat_EAR.p_3cat)
CrossTable(dat$smoking_Pat_EAR.p_3cat)

## Binary (during (partner's) pregnancy) - Yes vs. No
dat <-
  dat %>% mutate(smoking_Pat_EAR.p_bin = case_when(pb079 == 0 ~ 0, pb079 >
                                                     0 ~ 1, TRUE ~ NA_real_))

val_lab(dat$smoking_Pat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$smoking_Pat_EAR.p_bin <-
  as.factor(dat$smoking_Pat_EAR.p_bin)

var_lab(dat$smoking_Pat_EAR.p_bin) = "Paternal smoking in early pregnancy (binary)"

str(dat$smoking_Pat_EAR.p_bin)
CrossTable(dat$smoking_Pat_EAR.p_bin)

# Paternal alcohol drinking during (partner's) pregnancy - Yes (>=1 glass per week) vs. No (<1 glass per week)
table(dat$pb099, useNA = "always")  # Pre-pregnancy
table(dat$pb100, useNA = "always")  # In past 3 months

dat <-
  dat %>% mutate(alcohol_Pat_EAR.p_bin = case_when(pb100 %in% 1:2 ~ 0, pb100 %in% 3:6 ~ 1, TRUE ~ NA_real_))

val_lab(dat$alcohol_Pat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$alcohol_Pat_EAR.p_bin <-
  as.factor(dat$alcohol_Pat_EAR.p_bin)

var_lab(dat$alcohol_Pat_EAR.p_bin) = "Paternal alcohol drinking in early pregnancy (binary)"

str(dat$alcohol_Pat_EAR.p_bin)
CrossTable(dat$alcohol_Pat_EAR.p_bin)

# Paternal dietary supplement use - NOT AVAILABLE

# Total energy intake (kcal/day) - NOT AVAILABLE

# Offspring sex
str(dat$sex_Chi_bin)
CrossTable(dat$sex_Chi_bin)

################################################################################

# Save dataset
head(dat)
dim(dat)  # 11693   XXX

saveRDS(dat, "data/ALSPAC/dat_exp_cov_out_pat.rds")
