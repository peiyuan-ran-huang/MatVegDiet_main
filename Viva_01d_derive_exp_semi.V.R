################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 18-May-2024
# This script is to derive vegetarian subgroups (***including semi-vegetarians***) in Project Viva.

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
  ggcorrplot
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load raw data
dat <- read_sas("data/Viva/ran_huang_071823.sas7bdat")
dat <- zap_labels(dat)

dat <- as.data.frame(dat)

################################################################################
head(dat)
dim(dat)  # N = 2624 (somehow additionally including 496 participants with no family ID and with NAs across all variables)
dat <-
  dat[is.na(dat$age_mom_enroll_d) == F, ]  # Remove the additional 496
################################################################################

head(dat)
dim(dat)  # 369 variables in 2128 singleton live births and their mothers

################################################################################

# Exclusion

## Exclude: multiple gestation - !!! Project Viva only recruited singleton pregnancies !!!

## Exclude: non-live birth - !!! All live in the 2128 sample !!!

## Exclude: duplicated pregnancies – For mothers with more than one pregnancies registered, keep ONE RANDOM (!!!) pregnancy for each mother
set.seed(19705)
dat <- dat %>% group_by(familyid) %>% slice_sample(., n = 1)

dat <- as.data.frame(dat)
head(dat)
dim(dat)  # 2128 -> 2100 (from unique mothers)

################################################################################

summary(dat$gest_age_weeks_ffq1)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 4.957   9.714  11.143  11.873  13.143  33.571     350

summary(dat$gest_age_weeks_ffq2)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 16.86   27.57   28.86   29.21   30.43   40.14     459

sum(dat$gest_age_weeks_ffq1 < dat$gest_age_weeks_ffq2, na.rm = T)  # 1518 with FFQ1 earlier than FFQ2
sum(dat$gest_age_weeks_ffq1 > dat$gest_age_weeks_ffq2, na.rm = T)  # 0 with FFQ1 later than FFQ2
sum(dat$gest_age_weeks_ffq1 == dat$gest_age_weeks_ffq2, na.rm = T)  # 1 with FFQ1 at the same time as FFQ2

#------------------------------------------------------------------------------#
#                               Data Preparation                               #----
#------------------------------------------------------------------------------#

# Convert and rename FFQ variables

## In early pregnancy (labelled as "_1")

### Weekly intake frequency (in times/week) for meat & poultry, fish & seafood, egg, and dairy - Based on the assumption of eating one portion as a time
dat$meat_freq_1 <- dat$meat_f1d * 7
dat$fishseafood_freq_1 <- dat$fish_f1d * 7
dat$egg_freq_1 <- dat$egg_f1d * 7
dat$dairy_freq_1 <- dat$dairy_f1d * 7

### Daily intake (in portions/day) for all 18 food groups
dat$wholegrain_1 <- dat$wgrain_f1d
dat$fruit_1 <- dat$fruit_f1d
dat$vegetable_1 <- dat$veg_f1d
dat$nut_1 <- dat$nuts_f1d
dat$legume_1 <- dat$legume_f1d
dat$vegetableoil_1 <- dat$vegetable_oils_f1d
dat$teacoffee_1 <- dat$tea_coffee_f1d
dat$fruitjuice_1 <- dat$fruit_juice_f1d
dat$refinedgrain_1 <- dat$refined_f1d
dat$potato_1 <- dat$potatoes_f1d
dat$sugarbeverage_1 <- dat$ssb_f1d
dat$sweetdessert_1 <- dat$sweets_f1d
dat$animalfat_1 <- dat$animal_fat_f1d
dat$dairy_1 <- dat$dairy_f1d
dat$egg_1 <- dat$egg_f1d
dat$fishseafood_1 <- dat$fish_f1d
dat$meat_1 <- dat$meat_f1d
dat$misc.animal_1 <- dat$oth_animal_based_f1d

## In late pregnancy (labelled as "_2")

### Weekly intake frequency (in times/week) for meat & poultry, fish & seafood, egg, and dairy - Based on the assumption of eating one portion as a time
dat$meat_freq_2 <- dat$meat_f2d * 7
dat$fishseafood_freq_2 <- dat$fish_f2d * 7
dat$egg_freq_2 <- dat$egg_f2d * 7
dat$dairy_freq_2 <- dat$dairy_f2d * 7

### Daily intake (in portions/day) for all 18 food groups
dat$wholegrain_2 <- dat$wgrain_f2d
dat$fruit_2 <- dat$fruit_f2d
dat$vegetable_2 <- dat$veg_f2d
dat$nut_2 <- dat$nuts_f2d
dat$legume_2 <- dat$legume_f2d
dat$vegetableoil_2 <- dat$vegetable_oils_f2d
dat$teacoffee_2 <- dat$tea_coffee_f2d
dat$fruitjuice_2 <- dat$fruit_juice_f2d
dat$refinedgrain_2 <- dat$refined_f2d
dat$potato_2 <- dat$potatoes_f2d
dat$sugarbeverage_2 <- dat$ssb_f2d
dat$sweetdessert_2 <- dat$sweets_f2d
dat$animalfat_2 <- dat$animal_fat_f2d
dat$dairy_2 <- dat$dairy_f2d
dat$egg_2 <- dat$egg_f2d
dat$fishseafood_2 <- dat$fish_f2d
dat$meat_2 <- dat$meat_f2d
dat$misc.animal_2 <- dat$oth_animal_based_f2d

#------------------------------------------------------------------------------#
#                               Vegetarian Diets                               #----
#------------------------------------------------------------------------------#

# In early pregnancy (labelled as "_1")

## Data preparation: Generate weekly intake frequency (times/week) of meat & poultry, fish & seafood, egg, and dairy
dat$meatfish_freq_1 <-
  rowSums(dat[, c("meat_freq_1", "fishseafood_freq_1")])  # Meat & poultry + fish & seafood
dat$eggdairy_freq_1 <-
  rowSums(dat[, c("egg_freq_1", "dairy_freq_1")])  # Egg + dairy
dat$animal_freq_1 <-
  rowSums(dat[, c("meat_freq_1",
                  "fishseafood_freq_1",
                  "egg_freq_1",
                  "dairy_freq_1")])  # Meat & poultry + fish & seafood + egg + dairy (all animal-based foods)

## Vegetarian subgroups (detailed) - Coded from the least to the most restricted subgroup

### 0 - Non-vegetarian - meat & poultry: no restriction; fish & seafood: no restriction; egg & dairy: no restriction
dat$VegDiet_subgroup_1 <- 0
dat$VegDiet_subgroup_1[is.na(dat$meat_freq_1) |
                         is.na(dat$fishseafood_freq_1) |
                         is.na(dat$egg_freq_1) |
                         is.na(dat$dairy_freq_1)] <- NA

### 1 - Semi-vegetarian - meat & poultry + fish & seafood >= 1 time/month but <1 time/week; egg & dairy: no restriction
dat$VegDiet_subgroup_1[dat$meatfish_freq_1 >= 0.25 &
                         dat$meatfish_freq_1 < 1] <- 1

### 2 - Pesco-vegetarian - meat & poultry < 1 time/month (i.e., 0.25 time/week); fish & seafood >= 1 time/month; egg & dairy: no restriction
dat$VegDiet_subgroup_1[dat$meat_freq_1 < 0.25 &
                         dat$fishseafood_freq_1 >= 0.25] <- 2

### 3 - Lacto-ovo-vegetarian - meat & poultry + fish & seafood < 1 time/month; egg + dairy >= 1 time/month
dat$VegDiet_subgroup_1[dat$meatfish_freq_1 < 0.25 &
                         dat$eggdairy_freq_1 >= 0.25] <- 3

### 4 - Vegan - meat & poultry + fish & seafood + egg + dairy < 1 time/month
dat$VegDiet_subgroup_1[dat$animal_freq_1 < 0.25] <- 4

################################################################################

# In mid-pregnancy (labelled as "_2")

## Data preparation: Generate weekly intake frequency (times/week) of meat & poultry, fish & seafood, egg, and dairy
dat$meatfish_freq_2 <-
  rowSums(dat[, c("meat_freq_2", "fishseafood_freq_2")])  # Meat & poultry + fish & seafood
dat$eggdairy_freq_2 <-
  rowSums(dat[, c("egg_freq_2", "dairy_freq_2")])  # Egg + dairy
dat$animal_freq_2 <-
  rowSums(dat[, c("meat_freq_2",
                  "fishseafood_freq_2",
                  "egg_freq_2",
                  "dairy_freq_2")])  # Meat & poultry + fish & seafood + egg + dairy (all animal-based foods)

## Vegetarian subgroups (detailed) - Coded from the least to the most restricted subgroup

### 0 - Non-vegetarian - meat & poultry: no restriction; fish & seafood: no restriction; egg & dairy: no restriction
dat$VegDiet_subgroup_2 <- 0
dat$VegDiet_subgroup_2[is.na(dat$meat_freq_2) |
                         is.na(dat$fishseafood_freq_2) |
                         is.na(dat$egg_freq_2) |
                         is.na(dat$dairy_freq_2)] <- NA

### 1 - Semi-vegetarian - meat & poultry + fish & seafood >= 1 time/month but <1 time/week; egg & dairy: no restriction
dat$VegDiet_subgroup_2[dat$meatfish_freq_2 >= 0.25 &
                         dat$meatfish_freq_2 < 1] <- 1

### 2 - Pesco-vegetarian - meat & poultry < 1 time/month (i.e., 0.25 time/week); fish & seafood >= 1 time/month; egg & dairy: no restriction
dat$VegDiet_subgroup_2[dat$meat_freq_2 < 0.25 &
                         dat$fishseafood_freq_2 >= 0.25] <- 2

### 3 - Lacto-ovo-vegetarian - meat & poultry + fish & seafood < 1 time/month; egg + dairy >= 1 time/month
dat$VegDiet_subgroup_2[dat$meatfish_freq_2 < 0.25 &
                         dat$eggdairy_freq_2 >= 0.25] <- 3

### 4 - Vegan - meat & poultry + fish & seafood + egg + dairy < 1 time/month
dat$VegDiet_subgroup_2[dat$animal_freq_2 < 0.25] <- 4

################################################################################

# Descriptive results

## Early pregnancy
val_lab(dat$VegDiet_subgroup_1) = num_lab("
                                        0 Non-vegetarian
                                        1 Semi-vegetarian
                                        2 Pesco-vegetarian
                                        3 Lacto-ovo-vegetarian
                                        4 Vegan
                                        ")
str(dat$VegDiet_subgroup_1)
CrossTable(dat$VegDiet_subgroup_1)

## Mid-pregnancy
val_lab(dat$VegDiet_subgroup_2) = num_lab("
                                        0 Non-vegetarian
                                        1 Semi-vegetarian
                                        2 Pesco-vegetarian
                                        3 Lacto-ovo-vegetarian
                                        4 Vegan
                                        ")
str(dat$VegDiet_subgroup_2)
CrossTable(dat$VegDiet_subgroup_2)

## From early to mid-pregnancy

### Proportions
dat_12 <-
  dat[complete.cases(subset(dat, select = c(VegDiet_subgroup_1, VegDiet_subgroup_2))), ]

dim(dat_12)  # 1518  416

sum(is.na(dat_12$VegDiet_subgroup_1))  # 0 missing
sum(is.na(dat_12$VegDiet_subgroup_2))  # 0 missing

CrossTable(dat_12$VegDiet_subgroup_1)

CrossTable(dat_12$VegDiet_subgroup_2)

################################################################################

### Sankey diagrams
dat_sankey <-
  subset(dat_12, select = c(aid, VegDiet_subgroup_1, VegDiet_subgroup_2)) %>% na.omit()

val_lab(dat_sankey$VegDiet_subgroup_1) = num_lab(
  "
                                        0 Non-vegetarian (early pregnancy)
                                        1 Semi-vegetarian (early pregnancy)
                                        2 Pesco-vegetarian (early pregnancy)
                                        3 Lacto-ovo-vegetarian (early pregnancy)
                                        4 Vegan (early pregnancy)
                                        "
)

val_lab(dat_sankey$VegDiet_subgroup_2) = num_lab(
  "
                                        0 Non-vegetarian (mid-pregnancy)
                                        1 Semi-vegetarian (mid-pregnancy)
                                        2 Pesco-vegetarian (mid-pregnancy)
                                        3 Lacto-ovo-vegetarian (mid-pregnancy)
                                        4 Vegan (mid-pregnancy)
                                        "
)

head(dat_sankey)
dim(dat_sankey)  # 1540    3
CrossTable(dat_sankey$VegDiet_subgroup_1)
CrossTable(dat_sankey$VegDiet_subgroup_2)

dat_sankey$sankey <-
  paste(dat_sankey$VegDiet_subgroup_1,
        dat_sankey$VegDiet_subgroup_2,
        sep = "_")

sort(unique(dat_sankey$sankey))
nrow(dat_sankey)

y <- c()
for (i in 1:length(sort(unique(dat_sankey$sankey)))) {
  x <- data.frame(
    x_1 = str_split(sort(unique(
      dat_sankey$sankey
    ))[i], "_")[[1]][1],
    x_2 = str_split(sort(unique(
      dat_sankey$sankey
    ))[i], "_")[[1]][2],
    val = nrow(subset(dat_sankey, sankey == sort(
      unique(dat_sankey$sankey)
    )[i]))
  )
  
  y <- rbind(y, x)
}

y

links <- data.frame(
  source = c(y$x_1),
  target = c(y$x_2),
  value = c(y$val)
)

links$source <-
  factor(
    links$source,
    levels = c(
      "Vegan (early pregnancy)",
      "Lacto-ovo-vegetarian (early pregnancy)",
      "Pesco-vegetarian (early pregnancy)",
      "Semi-vegetarian (early pregnancy)",
      "Non-vegetarian (early pregnancy)"
    )
  )

links$target <-
  factor(
    links$target,
    levels = c(
      "Vegan (mid-pregnancy)",
      "Lacto-ovo-vegetarian (mid-pregnancy)",
      "Pesco-vegetarian (mid-pregnancy)",
      "Semi-vegetarian (mid-pregnancy)",
      "Non-vegetarian (mid-pregnancy)"
    )
  )

links <- arrange(links, source, target)
links
nodes <- data.frame(name = c(as.character(links$source), as.character(links$target)) %>% unique())
nodes$name
nodes$name <- factor(nodes$name, levels = nodes$name)
nodes$name

# With networkD3, connection must be provided using id, not using real name like in the links dataframe. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1
links

links <-
  subset(
    links,
    links$source != "Non-vegetarian (early pregnancy)" |
      links$target != "Non-vegetarian (mid-pregnancy)"
  )
links

# Make the Network
VegDiet_12_sankey <- networkD3::sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  fontSize = 10,
  sinksRight = FALSE
)

VegDiet_12_sankey
