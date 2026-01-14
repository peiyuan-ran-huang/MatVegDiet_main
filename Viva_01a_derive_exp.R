################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 18-May-2024
# This script is to derive dietary exposure variables in Project Viva.

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

### 1 - Pesco-vegetarian - meat & poultry < 1 time/month (i.e., 0.25 time/week); fish & seafood >= 1 time/month; egg & dairy: no restriction
dat$VegDiet_subgroup_1[dat$meat_freq_1 < 0.25 &
                         dat$fishseafood_freq_1 >= 0.25] <- 1

### 2 - Lacto-ovo-vegetarian - meat & poultry + fish & seafood < 1 time/month; egg + dairy >= 1 time/month
dat$VegDiet_subgroup_1[dat$meatfish_freq_1 < 0.25 &
                         dat$eggdairy_freq_1 >= 0.25] <- 2

### 3 - Vegan - meat & poultry + fish & seafood + egg + dairy < 1 time/month
dat$VegDiet_subgroup_1[dat$animal_freq_1 < 0.25] <- 3

## Vegetarian subgroups (combined into 3 categories) - Used for surrogate variable analysis below
### 0 - Non-vegetarian
### 1 - Pesco-vegetarian
### 2 - Full vegetarian (including lacto-ovo-vegetarian & vegan)
dat$VegDiet_3cat_1 <- NA
dat$VegDiet_3cat_1[dat$VegDiet_subgroup_1 == 0] <- 0
dat$VegDiet_3cat_1[dat$VegDiet_subgroup_1 == 1] <- 1
dat$VegDiet_3cat_1[dat$VegDiet_subgroup_1 %in% c(2, 3)] <- 2

## Vegetarianism (binary)
### 0 - Non-vegetarian
### 1 - Full vegetarian + pesco-vegetarian
dat$VegDiet_bin_1 <- NA
dat$VegDiet_bin_1[dat$VegDiet_3cat_1 == 0] <- 0
dat$VegDiet_bin_1[dat$VegDiet_3cat_1 %in% c(1, 2)] <- 1

## Check the numbers
table(dat$VegDiet_subgroup_1)
table(dat$VegDiet_3cat_1)
table(dat$VegDiet_bin_1)

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

### 1 - Pesco-vegetarian - meat & poultry < 1 time/month (i.e., 0.25 time/week); fish & seafood >= 1 time/month; egg & dairy: no restriction
dat$VegDiet_subgroup_2[dat$meat_freq_2 < 0.25 &
                         dat$fishseafood_freq_2 >= 0.25] <- 1

### 2 - Lacto-ovo-vegetarian - meat & poultry + fish & seafood < 1 time/month; egg + dairy >= 1 time/month
dat$VegDiet_subgroup_2[dat$meatfish_freq_2 < 0.25 &
                         dat$eggdairy_freq_2 >= 0.25] <- 2

### 3 - Vegan - meat & poultry + fish & seafood + egg + dairy < 1 time/month
dat$VegDiet_subgroup_2[dat$animal_freq_2 < 0.25] <- 3

## Vegetarian subgroups (combined into 3 categories) - Used for surrogate variable analysis below
### 0 - Non-vegetarian
### 1 - Pesco-vegetarian
### 2 - Full vegetarian (including lacto-ovo-vegetarian & vegan)
dat$VegDiet_3cat_2 <- NA
dat$VegDiet_3cat_2[dat$VegDiet_subgroup_2 == 0] <- 0
dat$VegDiet_3cat_2[dat$VegDiet_subgroup_2 == 1] <- 1
dat$VegDiet_3cat_2[dat$VegDiet_subgroup_2 %in% c(2, 3)] <- 2

## Vegetarianism (binary)
### 0 - Non-vegetarian
### 1 - Full vegetarian + pesco-vegetarian
dat$VegDiet_bin_2 <- NA
dat$VegDiet_bin_2[dat$VegDiet_3cat_2 == 0] <- 0
dat$VegDiet_bin_2[dat$VegDiet_3cat_2 %in% c(1, 2)] <- 1

## Check the numbers
table(dat$VegDiet_subgroup_2)
table(dat$VegDiet_3cat_2)
table(dat$VegDiet_bin_2)

################################################################################

# Descriptive results

## Early pregnancy
val_lab(dat$VegDiet_subgroup_1) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Lacto-ovo-vegetarian" = 2,
  "Vegan" = 3
)
str(dat$VegDiet_subgroup_1)
CrossTable(dat$VegDiet_subgroup_1)  # 50 "vegetarians" = 19 full-vegetarians (2 vegans) + 31 pesco-vegetarians
# |       Non-vegetarian |     Pesco-vegetarian | Lacto-ovo-vegetarian |                Vegan |
# |----------------------|----------------------|----------------------|----------------------|
# |                 1700 |                   31 |                   17 |                    2 |
# |                0.971 |                0.018 |                0.010 |                0.001 |
# |----------------------|----------------------|----------------------|----------------------|

val_lab(dat$VegDiet_3cat_1) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)
str(dat$VegDiet_3cat_1)
CrossTable(dat$VegDiet_3cat_1)  # 50 "vegetarians" = 19 full-vegetarians (2 vegans) + 31 pesco-vegetarians
# |   Non-vegetarian | Pesco-vegetarian |  Full vegetarian |
# |------------------|------------------|------------------|
# |             1700 |               31 |               19 |
# |            0.971 |            0.017 |            0.011 |
# |------------------|------------------|------------------|

val_lab(dat$VegDiet_bin_1) = c("Non-vegetarian" = 0, "Vegetarian" = 1)
str(dat$VegDiet_bin_1)
CrossTable(dat$VegDiet_bin_1)  # 50 "vegetarians" = 19 full-vegetarians (2 vegans) + 31 pesco-vegetarians
# | Non-vegetarian |     Vegetarian |
# |----------------|----------------|
# |           1700 |             50 |
# |          0.971 |          0.029 |
# |----------------|----------------|

## Mid-pregnancy
val_lab(dat$VegDiet_subgroup_2) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Lacto-ovo-vegetarian" = 2,
  "Vegan" = 3
)
str(dat$VegDiet_subgroup_2)
CrossTable(dat$VegDiet_subgroup_2)  # 43 "vegetarians" = 18 full-vegetarians (0 vegan) + 25 pesco-vegetarians
# |       Non-vegetarian |     Pesco-vegetarian | Lacto-ovo-vegetarian |
# |----------------------|----------------------|----------------------|
# |                 1598 |                   25 |                   18 |
# |                0.974 |                0.015 |                0.011 |
# |----------------------|----------------------|----------------------|

val_lab(dat$VegDiet_3cat_2) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)
str(dat$VegDiet_3cat_2)
CrossTable(dat$VegDiet_3cat_2)  # 43 "vegetarians" = 18 full-vegetarians (0 vegan) + 25 pesco-vegetarians
# |   Non-vegetarian | Pesco-vegetarian |  Full vegetarian |
# |------------------|------------------|------------------|
# |             1598 |               25 |               18 |
# |            0.974 |            0.015 |            0.011 |
# |------------------|------------------|------------------|

val_lab(dat$VegDiet_bin_2) = c("Non-vegetarian" = 0, "Vegetarian" = 1)
str(dat$VegDiet_bin_2)
CrossTable(dat$VegDiet_bin_2)  # 43 "vegetarians" = 18 full-vegetarians (0 vegan) + 25 pesco-vegetarians
# | Non-vegetarian |     Vegetarian |
# |----------------|----------------|
# |           1598 |             43 |
# |          0.974 |          0.026 |
# |----------------|----------------|

## From early to mid-pregnancy

### Proportions
dat_12 <-
  dat[complete.cases(subset(dat, select = c(VegDiet_subgroup_1, VegDiet_subgroup_2))), ]

dim(dat_12)  # 1519  425

sum(is.na(dat_12$VegDiet_subgroup_1))  # 0 missing
sum(is.na(dat_12$VegDiet_subgroup_2))  # 0 missing

CrossTable(dat_12$VegDiet_subgroup_1)  # 41 "vegetarians" = 15 full-vegetarians (1 vegan) + 26 pesco-vegetarians
# |       Non-vegetarian |     Pesco-vegetarian | Lacto-ovo-vegetarian |                Vegan |
# |----------------------|----------------------|----------------------|----------------------|
# |                 1478 |                   26 |                   14 |                    1 |
# |                0.973 |                0.017 |                0.009 |                0.001 |
# |----------------------|----------------------|----------------------|----------------------|

CrossTable(dat_12$VegDiet_subgroup_2)  # 40 "vegetarians" = 17 full-vegetarians (0 vegan) + 23 pesco-vegetarians
# |       Non-vegetarian |     Pesco-vegetarian | Lacto-ovo-vegetarian |
# |----------------------|----------------------|----------------------|
# |                 1479 |                   23 |                   17 |
# |                0.974 |                0.015 |                0.011 |
# |----------------------|----------------------|----------------------|

################################################################################

### Sankey diagrams
dat_sankey <-
  subset(dat_12, select = c(aid, VegDiet_subgroup_1, VegDiet_subgroup_2)) %>% na.omit()

val_lab(dat_sankey$VegDiet_subgroup_1) = c(
  "Non-vegetarian (early pregnancy)" = 0,
  "Pesco-vegetarian (early pregnancy)" = 1,
  "Lacto-ovo-vegetarian (early pregnancy)" = 2,
  "Vegan (early pregnancy)" = 3
)

val_lab(dat_sankey$VegDiet_subgroup_2) = c(
  "Non-vegetarian (mid-pregnancy)" = 0,
  "Pesco-vegetarian (mid-pregnancy)" = 1,
  "Lacto-ovo-vegetarian (mid-pregnancy)" = 2,
  "Vegan (mid-pregnancy)" = 3
)

head(dat_sankey)
dim(dat_sankey)  # 1519    3
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

#------------------------------------------------------------------------------#
#                           Plant-based Diet Indices                           #----
#------------------------------------------------------------------------------#

# PDI / hPDI / uPDI in early pregnancy (labelled as "_1")

## Check distribution of 18 food groups
food_group_name <-
  c(
    "wholegrain_1",
    "fruit_1",
    "vegetable_1",
    "nut_1",
    "legume_1",
    "vegetableoil_1",
    "teacoffee_1",
    "fruitjuice_1",
    "refinedgrain_1",
    "potato_1",
    "sugarbeverage_1",
    "sweetdessert_1",
    "animalfat_1",
    "dairy_1",
    "egg_1",
    "fishseafood_1",
    "meat_1",
    "misc.animal_1"
  )

show_quantile_values <- function(var_name) {
  my_var <- dat[, var_name]
  x <-
    quantile(
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
    `P30`  = x[5],
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

food_group_distribute <- data.frame(matrix(ncol = 17, nrow = 0))

for (var_name in food_group_name) {
  x <- show_quantile_values(var_name)
  food_group_distribute <- rbind(food_group_distribute, x)
}

write.csv(food_group_distribute,
          "results/Viva/food_group_distribute_1.csv",
          row.names = F)

## Set up functions for positive and negative scoring based on tertiles of each food group

### Positive scoring
p_score_3Q <- function(var_name) {
  my_var <- dat[, var_name]
  
  #### First obtain the cutoffs for tertiles
  P0 <- min(my_var, na.rm = T)
  P33.3 <- quantile(my_var, probs = 0.333, na.rm = T)
  P66.7 <- quantile(my_var, probs = 0.667, na.rm = T)
  P100 <- max(my_var, na.rm = T)
  
  #### When >33.3% with the same lowest intake (most likely to be 0)
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
    dat[, "my_var_3Q"] <- NA  # Create an empty column
    dat[which(my_var == P0), "my_var_3Q"] <-
      1  # Set all of those with the lowest intake as Q1
    dat[which(my_var > P0 &
                my_var <= median(my_var[my_var > P0], na.rm = T)), "my_var_3Q"] <-
      2  # For the rest, set those with intake <=median as Q2
    dat[which(my_var > median(my_var[my_var > P0], na.rm = T)), "my_var_3Q"] <-
      3  # And then set those with intake >median as Q3
    my_var_3Q <-
      c(dat[, "my_var_3Q"])  # Convert column into vector for subsequent combination
    dat <-
      subset(dat, select = -c(my_var_3Q))  # Remove the newly created column from the main dataset
  }
  
  #### In other situations - Take tertiles and assign positive scores
  else {
    my_var_3Q <-
      findInterval(my_var, c(-Inf, quantile(
        my_var, probs = c(0.333, 0.667), na.rm = T
      ), Inf))
  }
  
  #### Return outputs
  return(my_var_3Q)
}

### Negative scoring
r_score_3Q <- function(var_name) {
  my_var_3Q <-
    p_score_3Q(var_name)  # Same as above for setting tertiles
  my_var_3Q <-
    dplyr::recode(my_var_3Q,
                  "1" = 3,
                  "2" = 2,
                  "3" = 1)  # Recoded as reverse scores
  return(my_var_3Q)
}

## Apply function to create tertiles for each of the 18 food groups
for (var_name in food_group_name) {
  assign(paste0(var_name, "_3Q"), factor(p_score_3Q(var_name), label = c("T1", "T2", "T3")))
  dat <- cbind(dat, get(paste0(var_name, "_3Q")))
}
for (i in 1:18) {
  colnames(dat)[ncol(dat) - 18 + i] <-
    paste0(food_group_name[i], "_3Q")  # Add 18 food group tertile variables (columns) to the end of dat
}
head(dat)  # Tertile variables should have been added to the last 18 columns

## Set up functions for showing tertile cutoffs and generating descriptive table for the 18 food groups
tertile_cutoff <- function(var_name) {
  my_var <- dat[, var_name]
  my_var_3Q <- p_score_3Q(var_name)
  T1_lo <- round(min(my_var[my_var_3Q == 1], na.rm = T), digits = 2)
  T1_up <- round(max(my_var[my_var_3Q == 1], na.rm = T), digits = 2)
  T2_lo <- round(min(my_var[my_var_3Q == 2], na.rm = T), digits = 2)
  T2_up <- round(max(my_var[my_var_3Q == 2], na.rm = T), digits = 2)
  T3_lo <- round(min(my_var[my_var_3Q == 3], na.rm = T), digits = 2)
  T3_up <- round(max(my_var[my_var_3Q == 3], na.rm = T), digits = 2)
  x <-
    paste0("T1: ",
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
      p_score_3Q(var_name), label = c("T1", "T2", "T3")
    ), useNA = "always")
  )
  return(x)
}

## Apply function to generate descriptive table for the 18 food groups
food_group_tertile <- data.frame(matrix(ncol = 6, nrow = 0))
for (var_name in food_group_name) {
  x <- describe_food_group(var_name)
  food_group_tertile <- rbind(food_group_tertile, x)
}
colnames(food_group_tertile) <-
  c("Food_group", "Cutoffs", "N_T1", "N_T2", "N_T3", "N_missing")
write.csv(food_group_tertile,
          "results/Viva/food_group_tertile_1.csv",
          row.names = F)

## Apply functions to calculate PDIs

### Overall plant-based diet index (PDI)
dat$PDI_1 <-
  #### 7 healthy plant food groups assigned positive scores
  p_score_3Q("wholegrain_1") + p_score_3Q("fruit_1") + p_score_3Q("vegetable_1") + p_score_3Q("nut_1") + p_score_3Q("legume_1") + p_score_3Q("vegetableoil_1") + p_score_3Q("teacoffee_1") +
  #### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice_1") + p_score_3Q("refinedgrain_1") + p_score_3Q("potato_1") + p_score_3Q("sugarbeverage_1") + p_score_3Q("sweetdessert_1") +
  #### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_1") + r_score_3Q("dairy_1") + r_score_3Q("egg_1") + r_score_3Q("fishseafood_1") + r_score_3Q("meat_1") + r_score_3Q("misc.animal_1")

### Healthful plant-based diet index (hPDI)
dat$hPDI_1 <-
  #### 7 healthy plant food groups assigned positive scores
  p_score_3Q("wholegrain_1") + p_score_3Q("fruit_1") + p_score_3Q("vegetable_1") + p_score_3Q("nut_1") + p_score_3Q("legume_1") + p_score_3Q("vegetableoil_1") + p_score_3Q("teacoffee_1") +
  #### 5 less healthy plant food groups assigned reverse scores
  r_score_3Q("fruitjuice_1") + r_score_3Q("refinedgrain_1") + r_score_3Q("potato_1") + r_score_3Q("sugarbeverage_1") + r_score_3Q("sweetdessert_1") +
  #### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_1") + r_score_3Q("dairy_1") + r_score_3Q("egg_1") + r_score_3Q("fishseafood_1") + r_score_3Q("meat_1") + r_score_3Q("misc.animal_1")

### Unhealthful plant-based diet index (uPDI)
dat$uPDI_1 <-
  #### 7 healthy plant food groups assigned reverse scores
  r_score_3Q("wholegrain_1") + r_score_3Q("fruit_1") + r_score_3Q("vegetable_1") + r_score_3Q("nut_1") + r_score_3Q("legume_1") + r_score_3Q("vegetableoil_1") + r_score_3Q("teacoffee_1") +
  #### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice_1") + p_score_3Q("refinedgrain_1") + p_score_3Q("potato_1") + p_score_3Q("sugarbeverage_1") + p_score_3Q("sweetdessert_1") +
  #### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_1") + r_score_3Q("dairy_1") + r_score_3Q("egg_1") + r_score_3Q("fishseafood_1") + r_score_3Q("meat_1") + r_score_3Q("misc.animal_1")

## Histograms of PDI/hPDI/uPDI
for (my_exp in c("PDI_1", "hPDI_1", "uPDI_1")) {
  jpeg(paste0("results/Viva/", "histogram_", my_exp, ".jpg"))
  hist(
    dat[, my_exp],
    freq = F,
    main = paste0("Histogram of ", my_exp, " (N = ", sum(is.na(dat[, my_exp]) == F), ")"),
    xlab = my_exp
  )
  dev.off()
}

################################################################################

## PDI quintiles

### PDI
dat$PDI_5Q_1 <-
  findInterval(dat$PDI_1, c(-Inf, quantile(
    dat$PDI_1,
    probs = c(0.2, 0.4, 0.6, 0.8),
    na.rm = T
  ), Inf))


for (i in 1:5) {
  assign(paste0("T", i, "_lab"),
         paste0(
           "Quintile ",
           i,
           " (",
           min(dat$PDI_1[which(dat$PDI_5Q_1 == i)], na.rm = T),
           "-",
           max(dat$PDI_1[which(dat$PDI_5Q_1 == i)], na.rm = T),
           ")"
         ))
}

dat$PDI_5Q_1[which(dat$PDI_5Q_1 == 1)] <- T1_lab
dat$PDI_5Q_1[which(dat$PDI_5Q_1 == 2)] <- T2_lab
dat$PDI_5Q_1[which(dat$PDI_5Q_1 == 3)] <- T3_lab
dat$PDI_5Q_1[which(dat$PDI_5Q_1 == 4)] <- T4_lab
dat$PDI_5Q_1[which(dat$PDI_5Q_1 == 5)] <- T5_lab

dat$PDI_5Q_1 <-
  factor(dat$PDI_5Q_1, levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$PDI_5Q_1) = "Quintiles of PDI in early pregnancy"

str(dat$PDI_5Q_1)
CrossTable(dat$PDI_5Q_1)

################################################################################
################################################################################
################################################################################

### hPDI
dat$hPDI_5Q_1 <-
  findInterval(dat$hPDI_1, c(-Inf, quantile(
    dat$hPDI_1,
    probs = c(0.2, 0.4, 0.6, 0.8),
    na.rm = T
  ), Inf))

for (i in 1:5) {
  assign(paste0("T", i, "_lab"),
         paste0(
           "Quintile ",
           i,
           " (",
           min(dat$hPDI_1[which(dat$hPDI_5Q_1 == i)], na.rm = T),
           "-",
           max(dat$hPDI_1[which(dat$hPDI_5Q_1 == i)], na.rm = T),
           ")"
         ))
}

dat$hPDI_5Q_1[which(dat$hPDI_5Q_1 == 1)] <- T1_lab
dat$hPDI_5Q_1[which(dat$hPDI_5Q_1 == 2)] <- T2_lab
dat$hPDI_5Q_1[which(dat$hPDI_5Q_1 == 3)] <- T3_lab
dat$hPDI_5Q_1[which(dat$hPDI_5Q_1 == 4)] <- T4_lab
dat$hPDI_5Q_1[which(dat$hPDI_5Q_1 == 5)] <- T5_lab

dat$hPDI_5Q_1 <-
  factor(dat$hPDI_5Q_1, levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$hPDI_5Q_1) = "Quintiles of hPDI in early pregnancy"

str(dat$hPDI_5Q_1)
CrossTable(dat$hPDI_5Q_1)

################################################################################
################################################################################
################################################################################

### uPDI
dat$uPDI_5Q_1 <-
  findInterval(dat$uPDI_1, c(-Inf, quantile(
    dat$uPDI_1,
    probs = c(0.2, 0.4, 0.6, 0.8),
    na.rm = T
  ), Inf))

for (i in 1:5) {
  assign(paste0("T", i, "_lab"),
         paste0(
           "Quintile ",
           i,
           " (",
           min(dat$uPDI_1[which(dat$uPDI_5Q_1 == i)], na.rm = T),
           "-",
           max(dat$uPDI_1[which(dat$uPDI_5Q_1 == i)], na.rm = T),
           ")"
         ))
}

dat$uPDI_5Q_1[which(dat$uPDI_5Q_1 == 1)] <- T1_lab
dat$uPDI_5Q_1[which(dat$uPDI_5Q_1 == 2)] <- T2_lab
dat$uPDI_5Q_1[which(dat$uPDI_5Q_1 == 3)] <- T3_lab
dat$uPDI_5Q_1[which(dat$uPDI_5Q_1 == 4)] <- T4_lab
dat$uPDI_5Q_1[which(dat$uPDI_5Q_1 == 5)] <- T5_lab

dat$uPDI_5Q_1 <-
  factor(dat$uPDI_5Q_1, levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$uPDI_5Q_1) = "Quintiles of uPDI in early pregnancy"

str(dat$uPDI_5Q_1)
CrossTable(dat$uPDI_5Q_1)

################################################################################

# PDI / hPDI / uPDI in mid-pregnancy (labelled as "_2")

## Check distribution of 18 food groups
food_group_name <-
  c(
    "wholegrain_2",
    "fruit_2",
    "vegetable_2",
    "nut_2",
    "legume_2",
    "vegetableoil_2",
    "teacoffee_2",
    "fruitjuice_2",
    "refinedgrain_2",
    "potato_2",
    "sugarbeverage_2",
    "sweetdessert_2",
    "animalfat_2",
    "dairy_2",
    "egg_2",
    "fishseafood_2",
    "meat_2",
    "misc.animal_2"
  )

show_quantile_values <- function(var_name) {
  my_var <- dat[, var_name]
  x <-
    quantile(
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
    `P30`  = x[5],
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

food_group_distribute <- data.frame(matrix(ncol = 17, nrow = 0))

for (var_name in food_group_name) {
  x <- show_quantile_values(var_name)
  food_group_distribute <- rbind(food_group_distribute, x)
}

write.csv(food_group_distribute,
          "results/Viva/food_group_distribute_2.csv",
          row.names = F)

## Set up functions for positive and negative scoring based on tertiles of each food group

### Positive scoring
p_score_3Q <- function(var_name) {
  my_var <- dat[, var_name]
  
  #### First obtain the cutoffs for tertiles
  P0 <- min(my_var, na.rm = T)
  P33.3 <- quantile(my_var, probs = 0.333, na.rm = T)
  P66.7 <- quantile(my_var, probs = 0.667, na.rm = T)
  P100 <- max(my_var, na.rm = T)
  
  #### When >33.3% with the same lowest intake (most likely to be 0)
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
    dat[, "my_var_3Q"] <- NA  # Create an empty column
    dat[which(my_var == P0), "my_var_3Q"] <-
      1  # Set all of those with the lowest intake as Q1
    dat[which(my_var > P0 &
                my_var <= median(my_var[my_var > P0], na.rm = T)), "my_var_3Q"] <-
      2  # For the rest, set those with intake <=median as Q2
    dat[which(my_var > median(my_var[my_var > P0], na.rm = T)), "my_var_3Q"] <-
      3  # And then set those with intake >median as Q3
    my_var_3Q <-
      c(dat[, "my_var_3Q"])  # Convert column into vector for subsequent combination
    dat <-
      subset(dat, select = -c(my_var_3Q))  # Remove the newly created column from the main dataset
  }
  
  #### In other situations - Take tertiles and assign positive scores
  else {
    my_var_3Q <-
      findInterval(my_var, c(-Inf, quantile(
        my_var, probs = c(0.333, 0.667), na.rm = T
      ), Inf))
  }
  
  #### Return outputs
  return(my_var_3Q)
}

### Negative scoring
r_score_3Q <- function(var_name) {
  my_var_3Q <-
    p_score_3Q(var_name)  # Same as above for setting tertiles
  my_var_3Q <-
    dplyr::recode(my_var_3Q,
                  "1" = 3,
                  "2" = 2,
                  "3" = 1)  # Recoded as reverse scores
  return(my_var_3Q)
}

## Apply function to create tertiles for each of the 18 food groups
for (var_name in food_group_name) {
  assign(paste0(var_name, "_3Q"), factor(p_score_3Q(var_name), label = c("T1", "T2", "T3")))
  dat <- cbind(dat, get(paste0(var_name, "_3Q")))
}
for (i in 1:18) {
  colnames(dat)[ncol(dat) - 18 + i] <-
    paste0(food_group_name[i], "_3Q")  # Add 18 food group tertile variables (columns) to the end of dat
}
head(dat)  # Tertile variables should have been added to the last 18 columns

## Set up functions for showing tertile cutoffs and generating descriptive table for the 18 food groups
tertile_cutoff <- function(var_name) {
  my_var <- dat[, var_name]
  my_var_3Q <- p_score_3Q(var_name)
  T1_lo <- round(min(my_var[my_var_3Q == 1], na.rm = T), digits = 2)
  T1_up <- round(max(my_var[my_var_3Q == 1], na.rm = T), digits = 2)
  T2_lo <- round(min(my_var[my_var_3Q == 2], na.rm = T), digits = 2)
  T2_up <- round(max(my_var[my_var_3Q == 2], na.rm = T), digits = 2)
  T3_lo <- round(min(my_var[my_var_3Q == 3], na.rm = T), digits = 2)
  T3_up <- round(max(my_var[my_var_3Q == 3], na.rm = T), digits = 2)
  x <-
    paste0("T1: ",
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
      p_score_3Q(var_name), label = c("T1", "T2", "T3")
    ), useNA = "always")
  )
  return(x)
}

## Apply function to generate descriptive table for the 18 food groups
food_group_tertile <- data.frame(matrix(ncol = 6, nrow = 0))
for (var_name in food_group_name) {
  x <- describe_food_group(var_name)
  food_group_tertile <- rbind(food_group_tertile, x)
}
colnames(food_group_tertile) <-
  c("Food_group", "Cutoffs", "N_T1", "N_T2", "N_T3", "N_missing")
write.csv(food_group_tertile,
          "results/Viva/food_group_tertile_2.csv",
          row.names = F)

## Apply functions to calculate PDIs

### Overall plant-based diet index (PDI)
dat$PDI_2 <-
  #### 7 healthy plant food groups assigned positive scores
  p_score_3Q("wholegrain_2") + p_score_3Q("fruit_2") + p_score_3Q("vegetable_2") + p_score_3Q("nut_2") + p_score_3Q("legume_2") + p_score_3Q("vegetableoil_2") + p_score_3Q("teacoffee_2") +
  #### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice_2") + p_score_3Q("refinedgrain_2") + p_score_3Q("potato_2") + p_score_3Q("sugarbeverage_2") + p_score_3Q("sweetdessert_2") +
  #### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_2") + r_score_3Q("dairy_2") + r_score_3Q("egg_2") + r_score_3Q("fishseafood_2") + r_score_3Q("meat_2") + r_score_3Q("misc.animal_2")

### Healthful plant-based diet index (hPDI)
dat$hPDI_2 <-
  #### 7 healthy plant food groups assigned positive scores
  p_score_3Q("wholegrain_2") + p_score_3Q("fruit_2") + p_score_3Q("vegetable_2") + p_score_3Q("nut_2") + p_score_3Q("legume_2") + p_score_3Q("vegetableoil_2") + p_score_3Q("teacoffee_2") +
  #### 5 less healthy plant food groups assigned reverse scores
  r_score_3Q("fruitjuice_2") + r_score_3Q("refinedgrain_2") + r_score_3Q("potato_2") + r_score_3Q("sugarbeverage_2") + r_score_3Q("sweetdessert_2") +
  #### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_2") + r_score_3Q("dairy_2") + r_score_3Q("egg_2") + r_score_3Q("fishseafood_2") + r_score_3Q("meat_2") + r_score_3Q("misc.animal_2")

### Unhealthful plant-based diet index (uPDI)
dat$uPDI_2 <-
  #### 7 healthy plant food groups assigned reverse scores
  r_score_3Q("wholegrain_2") + r_score_3Q("fruit_2") + r_score_3Q("vegetable_2") + r_score_3Q("nut_2") + r_score_3Q("legume_2") + r_score_3Q("vegetableoil_2") + r_score_3Q("teacoffee_2") +
  #### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice_2") + p_score_3Q("refinedgrain_2") + p_score_3Q("potato_2") + p_score_3Q("sugarbeverage_2") + p_score_3Q("sweetdessert_2") +
  #### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_2") + r_score_3Q("dairy_2") + r_score_3Q("egg_2") + r_score_3Q("fishseafood_2") + r_score_3Q("meat_2") + r_score_3Q("misc.animal_2")

## Histograms of PDI/hPDI/uPDI
for (my_exp in c("PDI_2", "hPDI_2", "uPDI_2")) {
  jpeg(paste0("results/Viva/", "histogram_", my_exp, ".jpg"))
  hist(
    dat[, my_exp],
    freq = F,
    main = paste0("Histogram of ", my_exp, " (N = ", sum(is.na(dat[, my_exp]) == F), ")"),
    xlab = my_exp
  )
  dev.off()
}

################################################################################

## PDI quintiles

### PDI
dat$PDI_5Q_2 <-
  findInterval(dat$PDI_2, c(-Inf, quantile(
    dat$PDI_2,
    probs = c(0.2, 0.4, 0.6, 0.8),
    na.rm = T
  ), Inf))


for (i in 1:5) {
  assign(paste0("T", i, "_lab"),
         paste0(
           "Quintile ",
           i,
           " (",
           min(dat$PDI_2[which(dat$PDI_5Q_2 == i)], na.rm = T),
           "-",
           max(dat$PDI_2[which(dat$PDI_5Q_2 == i)], na.rm = T),
           ")"
         ))
}

dat$PDI_5Q_2[which(dat$PDI_5Q_2 == 1)] <- T1_lab
dat$PDI_5Q_2[which(dat$PDI_5Q_2 == 2)] <- T2_lab
dat$PDI_5Q_2[which(dat$PDI_5Q_2 == 3)] <- T3_lab
dat$PDI_5Q_2[which(dat$PDI_5Q_2 == 4)] <- T4_lab
dat$PDI_5Q_2[which(dat$PDI_5Q_2 == 5)] <- T5_lab

dat$PDI_5Q_2 <-
  factor(dat$PDI_5Q_2, levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$PDI_5Q_2) = "Quintiles of PDI in mid-pregnancy"

str(dat$PDI_5Q_2)
CrossTable(dat$PDI_5Q_2)

################################################################################
################################################################################
################################################################################

### hPDI
dat$hPDI_5Q_2 <-
  findInterval(dat$hPDI_2, c(-Inf, quantile(
    dat$hPDI_2,
    probs = c(0.2, 0.4, 0.6, 0.8),
    na.rm = T
  ), Inf))

for (i in 1:5) {
  assign(paste0("T", i, "_lab"),
         paste0(
           "Quintile ",
           i,
           " (",
           min(dat$hPDI_2[which(dat$hPDI_5Q_2 == i)], na.rm = T),
           "-",
           max(dat$hPDI_2[which(dat$hPDI_5Q_2 == i)], na.rm = T),
           ")"
         ))
}

dat$hPDI_5Q_2[which(dat$hPDI_5Q_2 == 1)] <- T1_lab
dat$hPDI_5Q_2[which(dat$hPDI_5Q_2 == 2)] <- T2_lab
dat$hPDI_5Q_2[which(dat$hPDI_5Q_2 == 3)] <- T3_lab
dat$hPDI_5Q_2[which(dat$hPDI_5Q_2 == 4)] <- T4_lab
dat$hPDI_5Q_2[which(dat$hPDI_5Q_2 == 5)] <- T5_lab

dat$hPDI_5Q_2 <-
  factor(dat$hPDI_5Q_2, levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$hPDI_5Q_2) = "Quintiles of hPDI in mid-pregnancy"

str(dat$hPDI_5Q_2)
CrossTable(dat$hPDI_5Q_2)

################################################################################
################################################################################
################################################################################

### uPDI
dat$uPDI_5Q_2 <-
  findInterval(dat$uPDI_2, c(-Inf, quantile(
    dat$uPDI_2,
    probs = c(0.2, 0.4, 0.6, 0.8),
    na.rm = T
  ), Inf))

for (i in 1:5) {
  assign(paste0("T", i, "_lab"),
         paste0(
           "Quintile ",
           i,
           " (",
           min(dat$uPDI_2[which(dat$uPDI_5Q_2 == i)], na.rm = T),
           "-",
           max(dat$uPDI_2[which(dat$uPDI_5Q_2 == i)], na.rm = T),
           ")"
         ))
}

dat$uPDI_5Q_2[which(dat$uPDI_5Q_2 == 1)] <- T1_lab
dat$uPDI_5Q_2[which(dat$uPDI_5Q_2 == 2)] <- T2_lab
dat$uPDI_5Q_2[which(dat$uPDI_5Q_2 == 3)] <- T3_lab
dat$uPDI_5Q_2[which(dat$uPDI_5Q_2 == 4)] <- T4_lab
dat$uPDI_5Q_2[which(dat$uPDI_5Q_2 == 5)] <- T5_lab

dat$uPDI_5Q_2 <-
  factor(dat$uPDI_5Q_2, levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$uPDI_5Q_2) = "Quintiles of uPDI in mid-pregnancy"

str(dat$uPDI_5Q_2)
CrossTable(dat$uPDI_5Q_2)

#------------------------------------------------------------------------------#
#                   Average Between Early And Mid-Pregnancy                    #----
#------------------------------------------------------------------------------#

# Average food intakes in early and mid-pregnancy
dat$wholegrain <-
  rowMeans(dat[, c("wgrain_f1d", "wgrain_f2d")] , na.rm = T)
dat$fruit <-
  rowMeans(dat[, c("fruit_f1d", "fruit_f2d")] , na.rm = T)
dat$vegetable <-
  rowMeans(dat[, c("veg_f1d", "veg_f2d")] , na.rm = T)
dat$nut <-
  rowMeans(dat[, c("nuts_f1d", "nuts_f2d")] , na.rm = T)
dat$legume <-
  rowMeans(dat[, c("legume_f1d", "legume_f2d")] , na.rm = T)
dat$vegetableoil <-
  rowMeans(dat[, c("vegetable_oils_f1d", "vegetable_oils_f2d")] , na.rm = T)
dat$teacoffee <-
  rowMeans(dat[, c("tea_coffee_f1d", "tea_coffee_f2d")] , na.rm = T)
dat$fruitjuice <-
  rowMeans(dat[, c("fruit_juice_f1d", "fruit_juice_f2d")] , na.rm = T)
dat$refinedgrain <-
  rowMeans(dat[, c("refined_f1d", "refined_f2d")] , na.rm = T)
dat$potato <-
  rowMeans(dat[, c("potatoes_f1d", "potatoes_f2d")] , na.rm = T)
dat$sugarbeverage <-
  rowMeans(dat[, c("ssb_f1d", "ssb_f2d")] , na.rm = T)
dat$sweetdessert <-
  rowMeans(dat[, c("sweets_f1d", "sweets_f2d")] , na.rm = T)
dat$animalfat <-
  rowMeans(dat[, c("animal_fat_f1d", "animal_fat_f2d")] , na.rm = T)
dat$dairy <-
  rowMeans(dat[, c("dairy_f1d", "dairy_f2d")] , na.rm = T)
dat$egg <-
  rowMeans(dat[, c("egg_f1d", "egg_f2d")] , na.rm = T)
dat$fishseafood <-
  rowMeans(dat[, c("fish_f1d", "fish_f2d")] , na.rm = T)
dat$meat <-
  rowMeans(dat[, c("meat_f1d", "meat_f2d")] , na.rm = T)
dat$misc.animal <-
  rowMeans(dat[, c("oth_animal_based_f1d", "oth_animal_based_f2d")] , na.rm = T)

################################################################################

# Vegetarian diets - Based on average food intakes between early and mid-pregnancy

## Weekly intake frequency (in times/week) for meat & poultry, fish & seafood, egg, and dairy - Based on the assumption of eating one portion as a time
dat$meat_freq <- dat$meat * 7
dat$fishseafood_freq <- dat$fishseafood * 7
dat$egg_freq <- dat$egg * 7
dat$dairy_freq <- dat$dairy * 7

## Data preparation: Generate weekly intake frequency (times/week) of meat & poultry, fish & seafood, egg, and dairy
dat$meatfish_freq <-
  rowSums(dat[, c("meat_freq", "fishseafood_freq")])  # Meat & poultry + fish & seafood
dat$eggdairy_freq <-
  rowSums(dat[, c("egg_freq", "dairy_freq")])  # Egg + dairy
dat$animal_freq <-
  rowSums(dat[, c("meat_freq", "fishseafood_freq", "egg_freq", "dairy_freq")])  # Meat & poultry + fish & seafood + egg + dairy (all animal-based foods)

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
val_lab(dat$VegDiet_subgroup) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Lacto-ovo-vegetarian" = 2,
  "Vegan" = 3
)
str(dat$VegDiet_subgroup)
CrossTable(dat$VegDiet_subgroup)  # 51 "vegetarians" = 21 full-vegetarians (1 vegan) + 30 pesco-vegetarians
# |       Non-vegetarian |     Pesco-vegetarian | Lacto-ovo-vegetarian |                Vegan |
# |----------------------|----------------------|----------------------|----------------------|
# |                 1821 |                   30 |                   20 |                    1 |
# |                0.973 |                0.016 |                0.011 |                0.001 |
# |----------------------|----------------------|----------------------|----------------------|

val_lab(dat$VegDiet_3cat) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)
str(dat$VegDiet_3cat)
CrossTable(dat$VegDiet_3cat)  # 51 "vegetarians" = 21 full-vegetarians (1 vegan) + 30 pesco-vegetarians
# |   Non-vegetarian | Pesco-vegetarian |  Full vegetarian |
# |------------------|------------------|------------------|
# |             1821 |               30 |               21 |
# |            0.973 |            0.016 |            0.011 |
# |------------------|------------------|------------------|

val_lab(dat$VegDiet_bin) = c("Non-vegetarian" = 0, "Vegetarian" = 1)
str(dat$VegDiet_bin)
CrossTable(dat$VegDiet_bin)  # 51 "vegetarians" = 21 full-vegetarians (1 vegan) + 30 pesco-vegetarians
# | Non-vegetarian |     Vegetarian |
# |----------------|----------------|
# |           1821 |             51 |
# |          0.973 |          0.027 |
# |----------------|----------------|

################################################################################

# PDI / hPDI / uPDI - Based on average food intakes between early and mid-pregnancy

## Check distribution of 18 food groups
food_group_name <-
  c(
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

show_quantile_values <- function(var_name) {
  my_var <- dat[, var_name]
  x <-
    quantile(
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
    `P30`  = x[5],
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

food_group_distribute <- data.frame(matrix(ncol = 17, nrow = 0))

for (var_name in food_group_name) {
  x <- show_quantile_values(var_name)
  food_group_distribute <- rbind(food_group_distribute, x)
}

write.csv(food_group_distribute,
          "results/Viva/food_group_distribute.csv",
          row.names = F)

## Set up functions for positive and negative scoring based on tertiles of each food group

### Positive scoring
p_score_3Q <- function(var_name) {
  my_var <- dat[, var_name]
  
  #### First obtain the cutoffs for tertiles
  P0 <- min(my_var, na.rm = T)
  P33.3 <- quantile(my_var, probs = 0.333, na.rm = T)
  P66.7 <- quantile(my_var, probs = 0.667, na.rm = T)
  P100 <- max(my_var, na.rm = T)
  
  #### When >33.3% with the same lowest intake (most likely to be 0)
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
    dat[, "my_var_3Q"] <- NA  # Create an empty column
    dat[which(my_var == P0), "my_var_3Q"] <-
      1  # Set all of those with the lowest intake as Q1
    dat[which(my_var > P0 &
                my_var <= median(my_var[my_var > P0], na.rm = T)), "my_var_3Q"] <-
      2  # For the rest, set those with intake <=median as Q2
    dat[which(my_var > median(my_var[my_var > P0], na.rm = T)), "my_var_3Q"] <-
      3  # And then set those with intake >median as Q3
    my_var_3Q <-
      c(dat[, "my_var_3Q"])  # Convert column into vector for subsequent combination
    dat <-
      subset(dat, select = -c(my_var_3Q))  # Remove the newly created column from the main dataset
  }
  
  #### In other situations - Take tertiles and assign positive scores
  else {
    my_var_3Q <-
      findInterval(my_var, c(-Inf, quantile(
        my_var, probs = c(0.333, 0.667), na.rm = T
      ), Inf))
  }
  
  #### Return outputs
  return(my_var_3Q)
}

### Negative scoring
r_score_3Q <- function(var_name) {
  my_var_3Q <-
    p_score_3Q(var_name)  # Same as above for setting tertiles
  my_var_3Q <-
    dplyr::recode(my_var_3Q,
                  "1" = 3,
                  "2" = 2,
                  "3" = 1)  # Recoded as reverse scores
  return(my_var_3Q)
}

## Apply function to create tertiles for each of the 18 food groups
for (var_name in food_group_name) {
  assign(paste0(var_name, "_3Q"), factor(p_score_3Q(var_name), label = c("T1", "T2", "T3")))
  dat <- cbind(dat, get(paste0(var_name, "_3Q")))
}
for (i in 1:18) {
  colnames(dat)[ncol(dat) - 18 + i] <-
    paste0(food_group_name[i], "_3Q")  # Add 18 food group tertile variables (columns) to the end of dat
}
head(dat)  # Tertile variables should have been added to the last 18 columns

## Set up functions for showing tertile cutoffs and generating descriptive table for the 18 food groups
tertile_cutoff <- function(var_name) {
  my_var <- dat[, var_name]
  my_var_3Q <- p_score_3Q(var_name)
  T1_lo <- round(min(my_var[my_var_3Q == 1], na.rm = T), digits = 2)
  T1_up <- round(max(my_var[my_var_3Q == 1], na.rm = T), digits = 2)
  T2_lo <- round(min(my_var[my_var_3Q == 2], na.rm = T), digits = 2)
  T2_up <- round(max(my_var[my_var_3Q == 2], na.rm = T), digits = 2)
  T3_lo <- round(min(my_var[my_var_3Q == 3], na.rm = T), digits = 2)
  T3_up <- round(max(my_var[my_var_3Q == 3], na.rm = T), digits = 2)
  x <-
    paste0("T1: ",
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
      p_score_3Q(var_name), label = c("T1", "T2", "T3")
    ), useNA = "always")
  )
  return(x)
}

## Apply function to generate descriptive table for the 18 food groups
food_group_tertile <- data.frame(matrix(ncol = 6, nrow = 0))
for (var_name in food_group_name) {
  x <- describe_food_group(var_name)
  food_group_tertile <- rbind(food_group_tertile, x)
}
colnames(food_group_tertile) <-
  c("Food_group", "Cutoffs", "N_T1", "N_T2", "N_T3", "N_missing")
write.csv(food_group_tertile,
          "results/Viva/food_group_tertile.csv",
          row.names = F)

## Apply functions to calculate PDIs

### Overall plant-based diet index (PDI)
dat$PDI <-
  #### 7 healthy plant food groups assigned positive scores
  p_score_3Q("wholegrain") + p_score_3Q("fruit") + p_score_3Q("vegetable") + p_score_3Q("nut") + p_score_3Q("legume") + p_score_3Q("vegetableoil") + p_score_3Q("teacoffee") +
  #### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice") + p_score_3Q("refinedgrain") + p_score_3Q("potato") + p_score_3Q("sugarbeverage") + p_score_3Q("sweetdessert") +
  #### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat") + r_score_3Q("dairy") + r_score_3Q("egg") + r_score_3Q("fishseafood") + r_score_3Q("meat") + r_score_3Q("misc.animal")

### Healthful plant-based diet index (hPDI)
dat$hPDI <-
  #### 7 healthy plant food groups assigned positive scores
  p_score_3Q("wholegrain") + p_score_3Q("fruit") + p_score_3Q("vegetable") + p_score_3Q("nut") + p_score_3Q("legume") + p_score_3Q("vegetableoil") + p_score_3Q("teacoffee") +
  #### 5 less healthy plant food groups assigned reverse scores
  r_score_3Q("fruitjuice") + r_score_3Q("refinedgrain") + r_score_3Q("potato") + r_score_3Q("sugarbeverage") + r_score_3Q("sweetdessert") +
  #### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat") + r_score_3Q("dairy") + r_score_3Q("egg") + r_score_3Q("fishseafood") + r_score_3Q("meat") + r_score_3Q("misc.animal")

### Unhealthful plant-based diet index (uPDI)
dat$uPDI <-
  #### 7 healthy plant food groups assigned reverse scores
  r_score_3Q("wholegrain") + r_score_3Q("fruit") + r_score_3Q("vegetable") + r_score_3Q("nut") + r_score_3Q("legume") + r_score_3Q("vegetableoil") + r_score_3Q("teacoffee") +
  #### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice") + p_score_3Q("refinedgrain") + p_score_3Q("potato") + p_score_3Q("sugarbeverage") + p_score_3Q("sweetdessert") +
  #### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat") + r_score_3Q("dairy") + r_score_3Q("egg") + r_score_3Q("fishseafood") + r_score_3Q("meat") + r_score_3Q("misc.animal")

## Histograms of PDI/hPDI/uPDI
for (my_exp in c("PDI", "hPDI", "uPDI")) {
  jpeg(paste0("results/Viva/", "histogram_", my_exp, ".jpg"))
  hist(
    dat[, my_exp],
    freq = F,
    main = paste0("Histogram of ", my_exp, " (N = ", sum(is.na(dat[, my_exp]) == F), ")"),
    xlab = my_exp
  )
  dev.off()
}

################################################################################

## PDI quintiles

### PDI
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

### hPDI
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

### uPDI
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
# Label dietary variables

## Load label list
labels <-
  read.xlsx("data/food.group_varlab.xlsx", sheet = "Sheet1")
labels
str(labels)  # (18 food groups + 3 PDIs) * (2 timepoints + 1 average) = 63 obs.

## Assign label to each variable
for (var_name in labels$varname) {
  var_lab(dat[, var_name]) <-
    labels$label[which(labels$varname == var_name)]
}
################################################################################

# Convert and label vegetarianism variables
dat$VegDiet_subgroup_1 <- as.factor(dat$VegDiet_subgroup_1)
var_lab(dat$VegDiet_subgroup_1) = "Diet-based vegetarianism (subgroups) in early pregnancy"

dat$VegDiet_3cat_1 <- as.factor(dat$VegDiet_3cat_1)
var_lab(dat$VegDiet_3cat_1) = "Diet-based vegetarianism (3 categories) in early pregnancy"

dat$VegDiet_bin_1 <- as.factor(dat$VegDiet_bin_1)
var_lab(dat$VegDiet_bin_1) = "Diet-based vegetarianism (binary) in early pregnancy"

dat$VegDiet_subgroup_2 <- as.factor(dat$VegDiet_subgroup_2)
var_lab(dat$VegDiet_subgroup_2) = "Diet-based vegetarianism (subgroups) in mid-pregnancy"

dat$VegDiet_3cat_2 <- as.factor(dat$VegDiet_3cat_2)
var_lab(dat$VegDiet_3cat_2) = "Diet-based vegetarianism (3 categories) in mid-pregnancy"

dat$VegDiet_bin_2 <- as.factor(dat$VegDiet_bin_2)
var_lab(dat$VegDiet_bin_2) = "Diet-based vegetarianism (binary) in mid-pregnancy"

dat$VegDiet_subgroup <- as.factor(dat$VegDiet_subgroup)
var_lab(dat$VegDiet_subgroup) = "Diet-based vegetarianism (subgroups) during pregnancy"

dat$VegDiet_3cat <- as.factor(dat$VegDiet_3cat)
var_lab(dat$VegDiet_3cat) = "Diet-based vegetarianism (3 categories) during pregnancy"

dat$VegDiet_bin <- as.factor(dat$VegDiet_bin)
var_lab(dat$VegDiet_bin) = "Diet-based vegetarianism (binary) during pregnancy"

################################################################################
# Standardise PDIs - Per 1 SD increase
dat$PDI_1_z <- as.vector(scale(dat$PDI_1))
var_lab(dat$PDI_1_z) = "Standardised overall plant-based diet index (PDI) in early pregnancy"
summary(dat$PDI_1_z)

dat$hPDI_1_z <- as.vector(scale(dat$hPDI_1))
var_lab(dat$hPDI_1_z) = "Standardised healthful plant-based diet index (hPDI) in early pregnancy"
summary(dat$hPDI_1_z)

dat$uPDI_1_z <- as.vector(scale(dat$uPDI_1))
var_lab(dat$uPDI_1_z) = "Standardised unhealthful plant-based diet index (uPDI) in early pregnancy"
summary(dat$uPDI_1_z)

dat$PDI_2_z <- as.vector(scale(dat$PDI_2))
var_lab(dat$PDI_2_z) = "Standardised overall plant-based diet index (PDI) in mid-pregnancy"
summary(dat$PDI_2_z)

dat$hPDI_2_z <- as.vector(scale(dat$hPDI_2))
var_lab(dat$hPDI_2_z) = "Standardised healthful plant-based diet index (hPDI) in mid-pregnancy"
summary(dat$hPDI_2_z)

dat$uPDI_2_z <- as.vector(scale(dat$uPDI_2))
var_lab(dat$uPDI_2_z) = "Standardised unhealthful plant-based diet index (uPDI) in mid-pregnancy"
summary(dat$uPDI_2_z)

dat$PDI_z <- as.vector(scale(dat$PDI))
var_lab(dat$PDI_z) = "Standardised overall plant-based diet index (PDI) during pregnancy"
summary(dat$PDI_z)

dat$hPDI_z <- as.vector(scale(dat$hPDI))
var_lab(dat$hPDI_z) = "Standardised healthful plant-based diet index (hPDI) during pregnancy"
summary(dat$hPDI_z)

dat$uPDI_z <- as.vector(scale(dat$uPDI))
var_lab(dat$uPDI_z) = "Standardised unhealthful plant-based diet index (uPDI) during pregnancy"
summary(dat$uPDI_z)
################################################################################

#------------------------------------------------------------------------------#
#                                 Check & Save                                 #----
#------------------------------------------------------------------------------#

table(dat$VegDiet_3cat, useNA = "always")
summary(dat$PDI)
summary(dat$hPDI)
summary(dat$uPDI)

################################################################################
# Restrict to those without missing data in dietary exposure variables
dat <- dat[!is.na(dat$VegDiet_3cat), ]  # 2100 -> 1872
################################################################################

################################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Calculation of MODIFIED plant-based diet indices

dat <- as.data.frame(dat)

## MODIFIED overall plant-based diet index (PDIm)
dat$PDIm_1 <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3Q("wholegrain_1") + p_score_3Q("fruit_1") + p_score_3Q("vegetable_1") + p_score_3Q("nut_1") + p_score_3Q("legume_1") + p_score_3Q("vegetableoil_1") +
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice_1") + p_score_3Q("refinedgrain_1") + p_score_3Q("potato_1") + p_score_3Q("sugarbeverage_1") + p_score_3Q("sweetdessert_1") +
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_1") + r_score_3Q("dairy_1") + r_score_3Q("egg_1") + r_score_3Q("fishseafood_1") + r_score_3Q("meat_1") + r_score_3Q("misc.animal_1")
dat$PDIm_1
summary(dat$PDIm_1)

dat$PDIm_2 <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3Q("wholegrain_2") + p_score_3Q("fruit_2") + p_score_3Q("vegetable_2") + p_score_3Q("nut_2") + p_score_3Q("legume_2") + p_score_3Q("vegetableoil_2") +
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice_2") + p_score_3Q("refinedgrain_2") + p_score_3Q("potato_2") + p_score_3Q("sugarbeverage_2") + p_score_3Q("sweetdessert_2") +
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_2") + r_score_3Q("dairy_2") + r_score_3Q("egg_2") + r_score_3Q("fishseafood_2") + r_score_3Q("meat_2") + r_score_3Q("misc.animal_2")
dat$PDIm_2
summary(dat$PDIm_2)

dat$PDIm <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3Q("wholegrain") + p_score_3Q("fruit") + p_score_3Q("vegetable") + p_score_3Q("nut") + p_score_3Q("legume") + p_score_3Q("vegetableoil") +
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice") + p_score_3Q("refinedgrain") + p_score_3Q("potato") + p_score_3Q("sugarbeverage") + p_score_3Q("sweetdessert") +
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat") + r_score_3Q("dairy") + r_score_3Q("egg") + r_score_3Q("fishseafood") + r_score_3Q("meat") + r_score_3Q("misc.animal")
dat$PDIm
summary(dat$PDIm)

## MODIFIED healthful plant-based diet index (hPDIm)
dat$hPDIm_1 <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3Q("wholegrain_1") + p_score_3Q("fruit_1") + p_score_3Q("vegetable_1") + p_score_3Q("nut_1") + p_score_3Q("legume_1") + p_score_3Q("vegetableoil_1") +
  ### 5 less healthy plant food groups assigned reverse scores
  r_score_3Q("fruitjuice_1") + r_score_3Q("refinedgrain_1") + r_score_3Q("potato_1") + r_score_3Q("sugarbeverage_1") + r_score_3Q("sweetdessert_1") +
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_1") + r_score_3Q("dairy_1") + r_score_3Q("egg_1") + r_score_3Q("fishseafood_1") + r_score_3Q("meat_1") + r_score_3Q("misc.animal_1")
dat$hPDIm_1
summary(dat$hPDIm_1)

dat$hPDIm_2 <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3Q("wholegrain_2") + p_score_3Q("fruit_2") + p_score_3Q("vegetable_2") + p_score_3Q("nut_2") + p_score_3Q("legume_2") + p_score_3Q("vegetableoil_2") +
  ### 5 less healthy plant food groups assigned reverse scores
  r_score_3Q("fruitjuice_2") + r_score_3Q("refinedgrain_2") + r_score_3Q("potato_2") + r_score_3Q("sugarbeverage_2") + r_score_3Q("sweetdessert_2") +
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_2") + r_score_3Q("dairy_2") + r_score_3Q("egg_2") + r_score_3Q("fishseafood_2") + r_score_3Q("meat_2") + r_score_3Q("misc.animal_2")
dat$hPDIm_2
summary(dat$hPDIm_2)

dat$hPDIm <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3Q("wholegrain") + p_score_3Q("fruit") + p_score_3Q("vegetable") + p_score_3Q("nut") + p_score_3Q("legume") + p_score_3Q("vegetableoil") +
  ### 5 less healthy plant food groups assigned reverse scores
  r_score_3Q("fruitjuice") + r_score_3Q("refinedgrain") + r_score_3Q("potato") + r_score_3Q("sugarbeverage") + r_score_3Q("sweetdessert") +
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat") + r_score_3Q("dairy") + r_score_3Q("egg") + r_score_3Q("fishseafood") + r_score_3Q("meat") + r_score_3Q("misc.animal")
dat$hPDIm
summary(dat$hPDIm)

## MODIFIED unhealthful plant-based diet index (uPDIm)
dat$uPDIm_1 <-
  ### 6 healthy plant food groups assigned reverse scores (excluding tea and coffee)
  r_score_3Q("wholegrain_1") + r_score_3Q("fruit_1") + r_score_3Q("vegetable_1") + r_score_3Q("nut_1") + r_score_3Q("legume_1") + r_score_3Q("vegetableoil_1") +
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice_1") + p_score_3Q("refinedgrain_1") + p_score_3Q("potato_1") + p_score_3Q("sugarbeverage_1") + p_score_3Q("sweetdessert_1") +
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_1") + r_score_3Q("dairy_1") + r_score_3Q("egg_1") + r_score_3Q("fishseafood_1") + r_score_3Q("meat_1") + r_score_3Q("misc.animal_1")
dat$uPDIm_1
summary(dat$uPDIm_1)

dat$uPDIm_2 <-
  ### 6 healthy plant food groups assigned reverse scores (excluding tea and coffee)
  r_score_3Q("wholegrain_2") + r_score_3Q("fruit_2") + r_score_3Q("vegetable_2") + r_score_3Q("nut_2") + r_score_3Q("legume_2") + r_score_3Q("vegetableoil_2") +
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice_2") + p_score_3Q("refinedgrain_2") + p_score_3Q("potato_2") + p_score_3Q("sugarbeverage_2") + p_score_3Q("sweetdessert_2") +
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat_2") + r_score_3Q("dairy_2") + r_score_3Q("egg_2") + r_score_3Q("fishseafood_2") + r_score_3Q("meat_2") + r_score_3Q("misc.animal_2")
dat$uPDIm_2
summary(dat$uPDIm_2)

dat$uPDIm <-
  ### 6 healthy plant food groups assigned reverse scores (excluding tea and coffee)
  r_score_3Q("wholegrain") + r_score_3Q("fruit") + r_score_3Q("vegetable") + r_score_3Q("nut") + r_score_3Q("legume") + r_score_3Q("vegetableoil") +
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice") + p_score_3Q("refinedgrain") + p_score_3Q("potato") + p_score_3Q("sugarbeverage") + p_score_3Q("sweetdessert") +
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat") + r_score_3Q("dairy") + r_score_3Q("egg") + r_score_3Q("fishseafood") + r_score_3Q("meat") + r_score_3Q("misc.animal")
dat$uPDIm
summary(dat$uPDIm)

################################################################################

## Summary
dat <- as.tibble(dat)  # Converted back to tibble

var_lab(dat$PDIm_1) = "MODIFIED overall plant-based diet index (PDIm) in early pregnancy"
var_lab(dat$hPDIm_1) = "MODIFIED healthful plant-based diet index (hPDIm) in early pregnancy"
var_lab(dat$uPDIm_1) = "MODIFIED unhealthful plant-based diet index (uPDIm) in early pregnancy"

var_lab(dat$PDIm_2) = "MODIFIED overall plant-based diet index (PDIm) in mid-pregnancy"
var_lab(dat$hPDIm_2) = "MODIFIED healthful plant-based diet index (hPDIm) in mid-pregnancy"
var_lab(dat$uPDIm_2) = "MODIFIED unhealthful plant-based diet index (uPDIm) in mid-pregnancy"

var_lab(dat$PDIm) = "MODIFIED overall plant-based diet index (PDIm) during pregnancy"
var_lab(dat$hPDIm) = "MODIFIED healthful plant-based diet index (hPDIm) during pregnancy"
var_lab(dat$uPDIm) = "MODIFIED unhealthful plant-based diet index (uPDIm) during pregnancy"

################################################################################

# Standardise PDIs - Per 1 SD increase
dat$PDIm_1_z <- as.vector(scale(dat$PDIm_1))
var_lab(dat$PDIm_1_z) = "Standardised MODIFIED overall plant-based diet index (PDIm) in early pregnancy"
dat$hPDIm_1_z <- as.vector(scale(dat$hPDIm_1))
var_lab(dat$hPDIm_1_z) = "Standardised MODIFIED healthful plant-based diet index (hPDIm) in early pregnancy"
dat$uPDIm_1_z <- as.vector(scale(dat$uPDIm_1))
var_lab(dat$uPDIm_1_z) = "Standardised MODIFIED unhealthful plant-based diet index (uPDIm) in early pregnancy"

dat$PDIm_2_z <- as.vector(scale(dat$PDIm_2))
var_lab(dat$PDIm_2_z) = "Standardised MODIFIED overall plant-based diet index (PDIm) in mid-pregnancy"
dat$hPDIm_2_z <- as.vector(scale(dat$hPDIm_2))
var_lab(dat$hPDIm_2_z) = "Standardised MODIFIED healthful plant-based diet index (hPDIm) in mid-pregnancy"
dat$uPDIm_2_z <- as.vector(scale(dat$uPDIm_2))
var_lab(dat$uPDIm_2_z) = "Standardised MODIFIED unhealthful plant-based diet index (uPDIm) in mid-pregnancy"

dat$PDIm_z <- as.vector(scale(dat$PDIm))
var_lab(dat$PDIm_z) = "Standardised MODIFIED overall plant-based diet index (PDIm) during pregnancy"
dat$hPDIm_z <- as.vector(scale(dat$hPDIm))
var_lab(dat$hPDIm_z) = "Standardised MODIFIED healthful plant-based diet index (hPDIm) during pregnancy"
dat$uPDIm_z <- as.vector(scale(dat$uPDIm))
var_lab(dat$uPDIm_z) = "Standardised MODIFIED unhealthful plant-based diet index (uPDIm) during pregnancy"

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
################################################################################

dat <- as.tibble(dat)
head(dat)
dim(dat)  # 1872  552

saveRDS(dat, "data/Viva/dat_exp.rds")
