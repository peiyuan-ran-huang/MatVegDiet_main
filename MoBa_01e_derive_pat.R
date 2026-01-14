################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa       #
################################################################################

# Last edited date: 30-May-2024
# This script is to derive paternal dietary exposure variables and covariates for paternal negative control analysis in MoBa.

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
dat <- readRDS("dat_exp_cov_out.rds")
head(dat)
dim(dat)  # 73868  XXX

################################################################################

# Paternal diet-based vegetarianism

## Derive the 4 key food groups - meat, fish/seafood, eggs, dairy (in times/week)

### Raw FFQ items

#### Food added to bread

#### 1) Seldom/never
#### 2) 1-2 times per week
#### 3) 3-4 times per week
#### 4) 5-7 times per week
#### 5) Several times per day

#### Reduced fat cheese FF408
#### Regular cheese (yellow/brown) FF409
#### Prawns/Italian salad or similar FF410
#### Lean meat FF411
#### Servelat sausage, salami or similar FF412
#### Liver pate or similar FF413
#### Fish FF414
#### Egg (boiled, fried, scrambled) FF416

intake_freq_map <- c(
  "1" = 0,
  # Seldom/never
  "2" = 1.5,
  # 1-2 times per week
  "3" = 3.5,
  # 3-4 times per week
  "4" = 6,
  # 5-7 times per week
  "5" = 14
  # Several times per day - Assumed 2 times per day
)

FFQ_list <-
  c("FF408",
    "FF409",
    "FF410",
    "FF411",
    "FF412",
    "FF413",
    "FF414",
    "FF416")

for (var in FFQ_list) {
  dat[[var]] <- intake_freq_map[as.character(as.numeric(dat[[var]]))]
}

#### Drink

#### 1) Seldom/never
#### 2) 1-6 glasses per week
#### 3) 1 glass per day
#### 4) 2-3 glass per day
#### 5) 4 glasses or more per day

#### Whole milk, buttermilk, yogurt FF417
#### Low-fat and skimmed milk FF418

intake_freq_map <- c(
  "1" = 0,
  # Seldom/never
  "2" = 3.5,
  # 1-6 glasses per week
  "3" = 7,
  # 1 glass per day
  "4" = 17.5,
  # 2-3 glass per day
  "5" = 28
  # 4 glasses or more per day - Assumed the minimum (i.e., 4 glasses per day)
)

FFQ_list <-
  c("FF417",
    "FF418")

for (var in FFQ_list) {
  dat[[var]] <- intake_freq_map[as.character(as.numeric(dat[[var]]))]
}

#### Dinner

#### 1) Seldom/never
#### 2) 1-2 times per month
#### 3) 3-4 times per month
#### 4) 2-3 times per week
#### 5) 4 times or more per week

#### Sausages, hamburger FF428
#### Kebab FF429
#### Meals with minced meat FF431
#### Pure meat FF432
#### Chicken/turkey FF433
#### Lean fish (cod, Pollock, haddock etc.) FF434
#### Fatty fish (trout, salmon, mackerel, herring) FF435
#### Fish balls/fish cakes FF436

intake_freq_map <- c(
  "1" = 0,
  # Seldom/never
  "2" = 0.375,
  # 1-2 times per month
  "3" = 0.875,
  # 3-4 times per month
  "4" = 2.5,
  # 2-3 times per week
  "5" = 4
  # 4 times or more per week - Assumed the minimum (i.e., 4 times per week)
)

FFQ_list <-
  c("FF428",
    "FF429",
    "FF431",
    "FF432",
    "FF433",
    "FF434",
    "FF435",
    "FF436")

for (var in FFQ_list) {
  dat[[var]] <- intake_freq_map[as.character(as.numeric(dat[[var]]))]
}

################################################################################
################################################################################

### Meat
dat$meat_freq_Pat <-
  dat$FF411 + dat$FF412 + dat$FF413 + dat$FF428 + dat$FF429 + dat$FF431 +
  dat$FF432 + dat$FF433

### Fish/seafood
dat$fishseafood_freq_Pat <-
  dat$FF410 + dat$FF414 + dat$FF434 + dat$FF435 + dat$FF436

### Eggs
dat$egg_freq_Pat <- dat$FF416

### Dairy
dat$dairy_freq_Pat <- dat$FF408 + dat$FF409 + dat$FF417 + dat$FF418

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

### Vegetarian subgroups (detailed) - Coded from the least to the most restricted subgroup

#### 0 - Non-vegetarian - meat & poultry: no restriction; fish & seafood: no restriction; egg & dairy: no restriction
dat$VegDiet_subgroup_Pat <- 0
dat$VegDiet_subgroup_Pat[is.na(dat$meat_freq_Pat) |
                           is.na(dat$fishseafood_freq_Pat) |
                           is.na(dat$egg_freq_Pat) |
                           is.na(dat$dairy_freq_Pat)] <- NA

#### 1 - Pesco-vegetarian - meat & poultry < 1 time/month (i.e., = 0 gram/day); fish & seafood >= 1 time/month (i.e., > 0 gram/day); egg & dairy: no restriction
dat$VegDiet_subgroup_Pat[dat$meat_freq_Pat == 0 &
                           dat$fishseafood_freq_Pat > 0] <- 1

#### 2 - Lacto-ovo-vegetarian - meat & poultry + fish & seafood < 1 time/month; egg + dairy >= 1 time/month
dat$VegDiet_subgroup_Pat[dat$meatfish_freq_Pat == 0 &
                           dat$eggdairy_freq_Pat > 0] <- 2

#### 3 - Vegan - meat & poultry + fish & seafood + egg + dairy < 1 time/month
dat$VegDiet_subgroup_Pat[dat$animal_freq_Pat == 0] <- 3

### Vegetarian subgroups (combined into 3 categories) - Used for surrogate variable analysis below
#### 0 - Non-vegetarian
#### 1 - Pesco-vegetarian
#### 2 - Full vegetarian (including lacto-ovo-vegetarian & vegan)
dat$VegDiet_3cat_Pat <- NA
dat$VegDiet_3cat_Pat[dat$VegDiet_subgroup_Pat == 0] <- 0
dat$VegDiet_3cat_Pat[dat$VegDiet_subgroup_Pat == 1] <- 1
dat$VegDiet_3cat_Pat[dat$VegDiet_subgroup_Pat %in% c(2, 3)] <- 2

### Vegetarianism (binary)
#### 0 - Non-vegetarian
#### 1 - Full vegetarian + pesco-vegetarian
dat$VegDiet_bin_Pat <- NA
dat$VegDiet_bin_Pat[dat$VegDiet_3cat_Pat == 0] <- 0
dat$VegDiet_bin_Pat[dat$VegDiet_3cat_Pat %in% c(1, 2)] <- 1

### Check the numbers
table(dat$VegDiet_subgroup_Pat, useNA = "always")
table(dat$VegDiet_3cat_Pat, useNA = "always")
table(dat$VegDiet_bin_Pat, useNA = "always")

### Descriptive results
val_lab(dat$VegDiet_subgroup_Pat) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Lacto-ovo-vegetarian" = 2,
  "Vegan" = 3
)
str(dat$VegDiet_subgroup_Pat)
CrossTable(dat$VegDiet_subgroup_Pat)
# |       Non-vegetarian |     Pesco-vegetarian | Lacto-ovo-vegetarian |                Vegan |
# |----------------------|----------------------|----------------------|----------------------|
# |                21831 |                   63 |                   24 |                    4 |
# |                0.996 |                0.003 |                0.001 |                0.000 |
# |----------------------|----------------------|----------------------|----------------------|

val_lab(dat$VegDiet_3cat_Pat) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)
str(dat$VegDiet_3cat_Pat)
CrossTable(dat$VegDiet_3cat_Pat)
# |   Non-vegetarian | Pesco-vegetarian |  Full vegetarian |
# |------------------|------------------|------------------|
# |            21831 |               63 |               28 |
# |            0.996 |            0.003 |            0.001 |
# |------------------|------------------|------------------|

val_lab(dat$VegDiet_bin_Pat) = c("Non-vegetarian" = 0,
                                 "Vegetarian" = 1)
str(dat$VegDiet_bin_Pat)
CrossTable(dat$VegDiet_bin_Pat)
# | Non-vegetarian |     Vegetarian |
# |----------------|----------------|
# |          21831 |             91 |
# |          0.996 |          0.004 |
# |----------------|----------------|

################################################################################
################################################################################

## Convert and label vegetarianism variables
dat$VegDiet_subgroup_Pat <- as.factor(dat$VegDiet_subgroup_Pat)
var_lab(dat$VegDiet_subgroup_Pat) = "Paternal diet-based vegetarianism (subgroups) during pregnancy"

dat$VegDiet_3cat_Pat <- as.factor(dat$VegDiet_3cat_Pat)
var_lab(dat$VegDiet_3cat_Pat) = "Paternal diet-based vegetarianism (3 categories) during pregnancy"

dat$VegDiet_bin_Pat <- as.factor(dat$VegDiet_bin_Pat)
var_lab(dat$VegDiet_bin_Pat) = "Paternal diet-based vegetarianism (binary) during pregnancy"

################################################################################

# Paternal self-defined vegetarianism
table(dat$FF448, useNA = "always")  # I have a varied diet
table(dat$FF449, useNA = "always")  # I do not eat fish
table(dat$FF450, useNA = "always")  # I do not eat meat
table(dat$FF451, useNA = "always")  # I am a vegetarian

## 3-categories
dat$self.VegDiet_Pat_EAR.p_3cat <- NA
dat$self.VegDiet_Pat_EAR.p_3cat[dat$FF448 == 1 |
                                  dat$FF449 == 1] <- 0
dat$self.VegDiet_Pat_EAR.p_3cat[dat$FF450 == 1] <- 1
dat$self.VegDiet_Pat_EAR.p_3cat[dat$FF451 == 1] <- 2

val_lab(dat$self.VegDiet_Pat_EAR.p_3cat) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)

dat$self.VegDiet_Pat_EAR.p_3cat <-
  as.factor(dat$self.VegDiet_Pat_EAR.p_3cat)

var_lab(dat$self.VegDiet_Pat_EAR.p_3cat) = "Paternal self-defined vegetarianism (3 categories) in early pregnancy"

str(dat$self.VegDiet_Pat_EAR.p_3cat)
CrossTable(dat$self.VegDiet_Pat_EAR.p_3cat)

## Binary
dat$self.VegDiet_Pat_EAR.p_bin <- NA
dat$self.VegDiet_Pat_EAR.p_bin[dat$FF448 == 1 |
                                 dat$FF449 == 1] <- 0
dat$self.VegDiet_Pat_EAR.p_bin[dat$FF450 == 1 |
                                 dat$FF451 == 1] <- 1

val_lab(dat$self.VegDiet_Pat_EAR.p_bin) = c("Non-vegetarian" = 0,
                                            "vegetarian" = 1)

dat$self.VegDiet_Pat_EAR.p_bin <-
  as.factor(dat$self.VegDiet_Pat_EAR.p_bin)

var_lab(dat$self.VegDiet_Pat_EAR.p_bin) = "Paternal self-defined vegetarianism (binary) in early pregnancy"

str(dat$self.VegDiet_Pat_EAR.p_bin)
CrossTable(dat$self.VegDiet_Pat_EAR.p_bin)

################################################################################
################################################################################

CrossTable(
  dat$VegDiet_3cat_Pat,
  dat$self.VegDiet_Pat_EAR.p_3cat,
  prop.r = T,
  prop.c = F,
  prop.t = F,
  prop.chisq = F,
  chisq = F
)  # How many diet-based vegetarians defined themselves as "vegetarians"

CrossTable(
  dat$VegDiet_3cat_Pat,
  dat$self.VegDiet_Pat_EAR.p_3cat,
  prop.r = F,
  prop.c = T,
  prop.t = F,
  prop.chisq = F,
  chisq = F
)  # How many self-defined "vegetarians" were supported by their FFQ data

CrossTable(
  dat$VegDiet_3cat_Pat,
  dat$self.VegDiet_Pat_EAR.p_3cat,
  prop.r = T,
  prop.c = T,
  prop.t = F,
  prop.chisq = F,
  chisq = F
)  # Row %: self-defined in diet-based; column %: diet-based in self-defined

################################################################################

# Paternal age
summary(dat$FARS_ALDER)  # years

dat$age_Pat_con <- as.numeric(dat$FARS_ALDER)

dat$age_Pat_con <-
  ifelse(dat$age_Pat_con < 15 |
           dat$age_Pat_con > 80, NA, dat$age_Pat_con)  # Remove outliers

var_lab(dat$age_Pat_con) = "Paternal age (years) at delivery"

str(dat$age_Pat_con)
summary(dat$age_Pat_con)

# Paternal ethnicity - !!! NOT AVAILABLE in MoBa !!!

# Paternal education level
table(dat$FF16, useNA = "always")  # Completed (self-reported)
table(dat$FF17, useNA = "always")  # Doing (self-reported)
table(dat$AA1126, useNA = "always")  # Completed (mother-reported)
table(dat$AA1127, useNA = "always")  # Doing (mother-reported)

dat$FF16[is.na(dat$FF16)] <-
  dat$AA1126[is.na(dat$FF16)]  # Replace self-reported missing values with mother-reported values

## Ordinal / 3 categories (from Torjusen et al., 2010, BMC Public Health)
dat$edu_Pat_3cat <- as.character(dat$FF16)

dat$edu_Pat_3cat[dat$edu_Pat_3cat %in% c("1", "2", "3", "4")] <-
  "Low"  # <10 y - 12 y: 1) 9-year secondary school & 2) 1-2 year high school & 3) Vocational high school & 4) 3-year high school general studies, junior college
dat$edu_Pat_3cat[dat$edu_Pat_3cat == "5"] <-
  "Medium"  # 13-16 y: 5) Regional technical college, 4-year university degree (Bachelor’s degree, nurse, teacher, engineer)
dat$edu_Pat_3cat[dat$edu_Pat_3cat == "6"] <-
  "High"  # 17+ y: 6) University, technical college, more than 4 years (Master’s degree, medical doctor, PhD)

dat$edu_Pat_3cat <-
  factor(dat$edu_Pat_3cat,
         levels = c("Low", "Medium", "High"),
         ordered = T)  # Set as ordinal variable

var_lab(dat$edu_Pat_3cat) = "Paternal education attainment (3 categories)"

str(dat$edu_Pat_3cat)
CrossTable(dat$edu_Pat_3cat)

## Binary
dat$edu_Pat_bin <- NA
dat$edu_Pat_bin[which(dat$edu_Pat_3cat == "Low")] <-
  "Lower than college degree"
dat$edu_Pat_bin[which(dat$edu_Pat_3cat == "Medium" |
                        dat$edu_Pat_3cat == "High")] <-
  "College degree or higher"

dat$edu_Pat_bin <- factor(dat$edu_Pat_bin,
                          levels = c("Lower than college degree", "College degree or higher"))

var_lab(dat$edu_Pat_bin) = "Paternal education attainment (binary)"

str(dat$edu_Pat_bin)
CrossTable(dat$edu_Pat_bin)

# Household income - Same as the maternal one

## Ordinal / 3 categories (from Torjusen et al., 2010, BMC Public Health)
str(dat$income_Fam_3cat)
CrossTable(dat$income_Fam_3cat)

## Binary (High vs. Low + Medium)
str(dat$income_Fam_bin)
CrossTable(dat$income_Fam_bin)

# Paternal "parity" (number of children)
dat$FF401_G <- as.numeric(dat$FF401_G)
table(dat$FF401_G, useNA = "always")

dat <-
  dat %>% mutate(parity_Pat_bin = case_when(FF401_G == 0 ~ 0,
                                            FF401_G > 0 ~ 1,
                                            TRUE ~ NA_real_))

val_lab(dat$parity_Pat_bin) = c("0" = 0, ">=1" = 1)

dat$parity_Pat_bin <- as.factor(dat$parity_Pat_bin)

var_lab(dat$parity_Pat_bin) = "Paternal number of children (binary)"

str(dat$parity_Pat_bin)
CrossTable(dat$parity_Pat_bin)

# Paternal BMI
summary(dat$FF333)  # cm
dat$FF333[is.na(dat$FF333)] <-
  as.numeric(dat$AA88)[is.na(dat$FF333)]  # Replace self-reported missing values with mother-reported values

summary(dat$FF334)  # kg
dat$FF334[is.na(dat$FF334)] <-
  as.numeric(dat$AA89)[is.na(dat$FF334)]  # Replace self-reported missing values with mother-reported values

dat$height_Pat_EAR.p_con <- as.numeric(dat$FF333) / 100
dat$weight_Pat_EAR.p_con <- as.numeric(dat$FF334)

dat$height_Pat_EAR.p_con <-
  ifelse(
    dat$height_Pat_EAR.p_con < 1.2 |
      dat$height_Pat_EAR.p_con > 2.5,
    NA,
    dat$height_Pat_EAR.p_con
  )  # Remove outliers

dat$weight_Pat_EAR.p_con <-
  ifelse(
    dat$weight_Pat_EAR.p_con < 30 |
      dat$weight_Pat_EAR.p_con > 300,
    NA,
    dat$weight_Pat_EAR.p_con
  )  # Remove outliers

dat$BMI_Pat_EAR.p_con <-
  dat$weight_Pat_EAR.p_con / dat$height_Pat_EAR.p_con ^ 2

dat$BMI_Pat_EAR.p_con <-
  as.numeric(dat$BMI_Pat_EAR.p_con)

var_lab(dat$BMI_Pat_EAR.p_con) = "Paternal BMI in early pregnancy (kg/m^2)"

str(dat$BMI_Pat_EAR.p_con)
summary(dat$BMI_Pat_EAR.p_con)

# Paternal smoking
table(dat$FF214, useNA = "always")  # Ever
dat$FF214[is.na(dat$FF214)] <-
  dat$AA1353[is.na(dat$FF214)]  # Replace self-reported missing values with mother-reported values

table(dat$FF215, useNA = "always")  # 6 months before

table(dat$FF218, useNA = "always")  # Now
dat$FF218[is.na(dat$FF218)] <-
  dat$AA1354[is.na(dat$FF218)]  # Replace self-reported missing values with mother-reported values

## 3 categories - Never/Former/Current
dat <-
  dat %>% mutate(
    smoking_Pat_EAR.p_3cat = case_when(
      FF214 == 1 & FF218 == 1 ~ 0,
      FF214 == 2 & FF218 == 1 ~ 1,
      FF218 == 2 | FF218 == 3 ~ 2,
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
  dat %>% mutate(smoking_Pat_EAR.p_bin = case_when(FF218 == 1 ~ 0,
                                                   FF218 == 2 |
                                                     FF218 == 3 ~ 1,
                                                   TRUE ~ NA_real_))

val_lab(dat$smoking_Pat_EAR.p_bin) = c("No" = 0,
                                       "Yes" = 1)

dat$smoking_Pat_EAR.p_bin <-
  as.factor(dat$smoking_Pat_EAR.p_bin)

var_lab(dat$smoking_Pat_EAR.p_bin) = "Paternal smoking in early pregnancy (binary)"

str(dat$smoking_Pat_EAR.p_bin)
CrossTable(dat$smoking_Pat_EAR.p_bin)

# Paternal alcohol drinking during (partner's) pregnancy - Yes vs. No
table(dat$FF242, useNA = "always")  # Ever
table(dat$FF243, useNA = "always")  # 6 months before
table(dat$FF244, useNA = "always")  # Now
# [NOTE: Mother-reported information not available.]

dat <-
  dat %>% mutate(alcohol_Pat_EAR.p_bin = case_when(FF244 %in% 5:7 ~ 0,
                                                   FF244 %in% 1:4 ~ 1,
                                                   TRUE ~ NA_real_))

val_lab(dat$alcohol_Pat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$alcohol_Pat_EAR.p_bin <-
  as.factor(dat$alcohol_Pat_EAR.p_bin)

var_lab(dat$alcohol_Pat_EAR.p_bin) = "Paternal alcohol drinking in early pregnancy (binary)"

str(dat$alcohol_Pat_EAR.p_bin)
CrossTable(dat$alcohol_Pat_EAR.p_bin)

# Paternal dietary supplement use
table(dat$FF452, useNA = "always")  # Any
table(dat$FF453, useNA = "always")  # Multivitamin/mineral
table(dat$FF454, useNA = "always")  # Cod-liver oil/fish oil
table(dat$FF455, useNA = "always")  # Protein
# [NOTE: Mother-reported information not available.]

dat <-
  dat %>% mutate(any.supp_Pat_EAR.p_bin = case_when(FF452 == 1 ~ 0,
                                                    FF452 == 2 ~ 1,
                                                    TRUE ~ NA_real_))
val_lab(dat$any.supp_Pat_EAR.p_bin) = c("No" = 0, "Yes" = 1)

dat$any.supp_Pat_EAR.p_bin <-
  as.factor(dat$any.supp_Pat_EAR.p_bin)

var_lab(dat$any.supp_Pat_EAR.p_bin) = "Paternal any supplement use in early pregnancy (binary)"

str(dat$any.supp_Pat_EAR.p_bin)
CrossTable(dat$any.supp_Pat_EAR.p_bin)

# Total energy intake (kcal/day) - NOT AVAILABLE

# Offspring sex
str(dat$sex_Chi_bin)
CrossTable(dat$sex_Chi_bin)

################################################################################

# Save dataset
head(dat)
dim(dat)  # 73868   XXX

saveRDS(dat, "dat_exp_cov_out_pat.rds")
