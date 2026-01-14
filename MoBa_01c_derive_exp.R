################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa       #
################################################################################

# Last edited date: 18-May-2024
# This script is to derive dietary exposure variables in MoBa.

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
  corrplot,
  ggcorrplot
)

# Set working directory
setwd("N:/durable/projects/Ran_MoBa_var")

################################################################################

# Load data
dat <- read_dta("dat_MRPREG_out_cov.dta")
dat <-
  dat[!is.na(dat$PREG_ID_2552),]  # Removed 1 with empty Pregnancy ID (don't know where it came from)
head(dat)
dim(dat)  # 73869 -> 73868

dat <-
  as.data.frame(dat)  # Remember to convert tibble into data frame, or you will run into error when deriving PDIs below

# Check inclusion criteria

## Live births
mean(dat$livebirth == 1) == 1  # TRUE

## Singletons
mean(dat$from_multiplebirth == 0) == 1  # TRUE

## From unique mothers
sum(is.na(dat$PREG_ID_2552))  # 0 with no pregnancy ID
sum(duplicated(dat$PREG_ID_2552))  # 0 with duplicated pregnancy ID

sum(dat$M_ID_2552 == "")  # 0 with no maternal ID
sum(duplicated(dat$M_ID_2552))  # 0 with duplicated maternal ID

sum(dat$F_ID_2552 == "")  # 11333 with no paternal ID
sum(dat$F_ID_2552 != "")  # 62535 with paternal ID (i.e., paternal data available)
sum(duplicated(dat$F_ID_2552[dat$F_ID_2552 != ""]))  # 60 with duplicated paternal ID - re-married after divorce???

################################################################################

# Group 255 food items (spis01 ~ spis255) into 18 food groups
# [NOTE: Each spis var represents the intake (in g/day) of each food item.]

## Load grouping file (provided along with the scripts)
## [NOTE: Before running the code, please have a look at the "README" tab in the spreadsheet.]
food_groups <-
  read.xlsx("MoBa_FFQ_grouping.xlsx",
            sheet = "Sheet1")
head(food_groups)
dim(food_groups)  # 265   6
length(unique(food_groups$varname))  # 253 unique spis variables (255 - 2 dummy variables)

food_groups <-
  subset(food_groups, group != "NOT INCLUDED")  # Remove spis variables not used for grouping
head(food_groups)
dim(food_groups)  # 244   6
length(unique(food_groups$varname))  # 232 unique spis variables left

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
  )  # Specify varnames of the 18 food groups

## Iterate through each of the 18 food groups
## [NOTE: Here we will generate 18 new variables, representing the total intake (in g/day) of each food group.]
for (name in food_group_name) {
  group_data <-
    food_groups[food_groups$group == name, c("varname", "coef")]  # Obtain all spis vars and corresponding coefs belonging to this food group
  
  group_label <-
    unique(food_groups$group_label[food_groups$group == name])  # Obtain the right label for this food group
  
  dat[[name]] <-
    0  # For each individual, generate the food group variable and set it as 0 first
  
  ### Iterate through each food item (i.e., spis var) belonging to this food group
  for (var in group_data$varname) {
    coef <-
      group_data$coef[group_data$varname == var]  # Obtain the coefficient of this food item
    
    dat[[name]] <-
      dat[[name]] + dat[[var]] * as.numeric(coef)  # For each individual, multiply each spis var by the corresponding coef and add to the sum for this food group
  }
  
  Hmisc::label(dat[[name]]) <-
    group_label  # Lastly, label each food group
}

# Intake frequency for the 4 key food groups (for generating vegetarian diets variables)
# [NOTE: In MoBa, the raw FFQ data were originally measured as frequencies.
#        But they were then combined with (assumed) portion sizes to generate the 255 food items for use.
#        For convenience, we will directly use the food groups generated above, instead of bothering to deal with the raw FFQ data.
#        Given the measurement granularity of the MoBa FFQ, this compromise will NOT influence the process to derive vegetarian diet variables.
#        Here we still keep the frequency varnames (with "_freq") to be consistent with the code for the other cohorts (but they are actually g/day).]
dat$meat_freq <- dat$meat
dat$fishseafood_freq <- dat$fishseafood
dat$egg_freq <- dat$egg
dat$dairy_freq <- dat$dairy

################################################################################

# Vegetarian diets

# Calculate intake frequencies
dat$meatfish_freq <-
  rowSums(dat[, c("meat_freq", "fishseafood_freq")])  # Meat & poultry + fish & seafood
dat$eggdairy_freq <-
  rowSums(dat[, c("egg_freq", "dairy_freq")])  # Egg + dairy
dat$animal_freq <-
  rowSums(dat[, c("meat_freq",
                  "fishseafood_freq",
                  "egg_freq",
                  "dairy_freq")])  # Meat & poultry + fish & seafood + egg + dairy (all animal-based foods)

## Vegetarian subgroups (detailed) - Coded from the least to the most restricted subgroup

### 0 - Non-vegetarian - meat & poultry: no restriction; fish & seafood: no restriction; egg & dairy: no restriction
dat$VegDiet_subgroup <- 0
dat$VegDiet_subgroup[is.na(dat$meat_freq) |
                       is.na(dat$fishseafood_freq) |
                       is.na(dat$egg_freq) |
                       is.na(dat$dairy_freq)] <- NA

### 1 - Pesco-vegetarian - meat & poultry < 1 time/month (i.e., = 0 gram/day); fish & seafood >= 1 time/month (i.e., > 0 gram/day); egg & dairy: no restriction
dat$VegDiet_subgroup[dat$meat_freq == 0 &
                       dat$fishseafood_freq > 0] <- 1

### 2 - Lacto-ovo-vegetarian - meat & poultry + fish & seafood < 1 time/month; egg + dairy >= 1 time/month
dat$VegDiet_subgroup[dat$meatfish_freq == 0 &
                       dat$eggdairy_freq > 0] <- 2

### 3 - Vegan - meat & poultry + fish & seafood + egg + dairy < 1 time/month
dat$VegDiet_subgroup[dat$animal_freq == 0] <- 3

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
CrossTable(dat$VegDiet_subgroup)
# |       Non-vegetarian |     Pesco-vegetarian | Lacto-ovo-vegetarian |                Vegan |
# |----------------------|----------------------|----------------------|----------------------|
# |                73278 |                  416 |                  166 |                    8 |
# |                0.992 |                0.006 |                0.002 |                0.000 |
# |----------------------|----------------------|----------------------|----------------------|

val_lab(dat$VegDiet_3cat) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)
str(dat$VegDiet_3cat)
CrossTable(dat$VegDiet_3cat)
# |   Non-vegetarian | Pesco-vegetarian |  Full vegetarian |
# |------------------|------------------|------------------|
# |            73278 |              416 |              174 |
# |            0.992 |            0.006 |            0.002 |
# |------------------|------------------|------------------|

val_lab(dat$VegDiet_bin) = c("Non-vegetarian" = 0,
                             "Vegetarian" = 1)
str(dat$VegDiet_bin)
CrossTable(dat$VegDiet_bin)
# | Non-vegetarian |     Vegetarian |
# |----------------|----------------|
# |          73278 |            590 |
# |          0.992 |          0.008 |
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

write.xlsx(
  food_group_distribute,
  "N:/durable/projects/Ran_MoBa_var/results/food_group_distribute.xlsx",
  overwrite = T
)

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
  
  #### When the same intake ranges from < 33.3th percentile to > 66.7th percentile
  else if (P33.3 == P66.7) {
    ##### Create a warning message and introduce the solution
    writeLines(
      paste0(
        "Warning for ",
        var_name,
        ": The same intake (",
        P33.3,
        ") from < P33.3 to > P66.7;\nSolution: Set this value as Q2, then values < and > this value as Q1 and Q3, respectively."
      )
    )
    
    ##### Modify the range of tertiles and assign positive scores (1-3)
    dat[, "my_var_3Q"] <- NA  # Create an empty column
    dat[which(my_var == P33.3), "my_var_3Q"] <-
      2  # Set all of those in the middle as Q2
    dat[which(my_var < P33.3), "my_var_3Q"] <-
      1  # Set those < Q2 as Q1
    dat[which(my_var > P66.7), "my_var_3Q"] <-
      3  # Set those > Q2 as Q3
    my_var_3Q <-
      c(dat[, "my_var_3Q"])  # Convert column into vector for subsequent combination
    dat <-
      subset(dat, select = -c(my_var_3Q))  # Remove the newly created column from the main dataset
  }
  
  #### In all other situations - Take tertiles and assign positive scores
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
write.xlsx(
  food_group_tertile,
  "N:/durable/projects/Ran_MoBa_var/results/food_group_tertile.xlsx",
  overwrite = T
)

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
  jpeg(
    paste0(
      "N:/durable/projects/Ran_MoBa_var/results/",
      "histogram_",
      my_exp,
      ".jpg"
    )
  )
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
    dat$PDI,
    probs = c(0.2, 0.4, 0.6, 0.8),
    na.rm = T
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
  factor(dat$PDI_5Q,
         levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

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
  factor(dat$hPDI_5Q,
         levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

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
  factor(dat$uPDI_5Q,
         levels = c(T1_lab, T2_lab, T3_lab, T4_lab, T5_lab))

var_lab(dat$uPDI_5Q) = "Quintiles of uPDI"

str(dat$uPDI_5Q)
CrossTable(dat$uPDI_5Q)

################################################################################

# Label dietary variables

## Load label list
labels <-
  read.xlsx("N:/durable/projects/Ran_MoBa_var/food.group_varlab.xlsx",
            sheet = "Sheet1")
labels
str(labels)  # 18 food groups + 3 PDIs = 21 obs.

## Assign label to each variable
for (var_name in labels$varname) {
  var_lab(dat[, var_name]) <-
    labels$label[which(labels$varname == var_name)]
}

################################################################################

# Convert and label vegetarianism variables
dat$VegDiet_subgroup <- as.factor(dat$VegDiet_subgroup)
var_lab(dat$VegDiet_subgroup) = "Diet-based vegetarianism (subgroups) during pregnancy"

dat$VegDiet_3cat <- as.factor(dat$VegDiet_3cat)
var_lab(dat$VegDiet_3cat) = "Diet-based vegetarianism (3 categories) during pregnancy"

dat$VegDiet_bin <- as.factor(dat$VegDiet_bin)
var_lab(dat$VegDiet_bin) = "Diet-based vegetarianism (binary) during pregnancy"

################################################################################
# Standardise PDIs - Per 1 SD increase
dat$PDI_z <- as.vector(scale(dat$PDI))
var_lab(dat$PDI_z) = "Standardised overall plant-based diet index (PDI)"

dat$hPDI_z <- as.vector(scale(dat$hPDI))
var_lab(dat$hPDI_z) = "Standardised healthful plant-based diet index (hPDI)"

dat$uPDI_z <- as.vector(scale(dat$uPDI))
var_lab(dat$uPDI_z) = "Standardised unhealthful plant-based diet index (uPDI)"
################################################################################

################################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Calculation of MODIFIED plant-based diet indices

dat <- as.data.frame(dat)

## MODIFIED overall plant-based diet index (PDIm)
dat$PDIm <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3Q("wholegrain") +
  p_score_3Q("fruit") +
  p_score_3Q("vegetable") +
  p_score_3Q("nut") +
  p_score_3Q("legume") +
  p_score_3Q("vegetableoil") +
  
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice") +
  p_score_3Q("refinedgrain") +
  p_score_3Q("potato") +
  p_score_3Q("sugarbeverage") +
  p_score_3Q("sweetdessert") +
  
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat") +
  r_score_3Q("dairy") +
  r_score_3Q("egg") +
  r_score_3Q("fishseafood") +
  r_score_3Q("meat") +
  r_score_3Q("misc.animal")

dat$PDIm
summary(dat$PDIm)

## MODIFIED healthful plant-based diet index (hPDIm)
dat$hPDIm <-
  ### 6 healthy plant food groups assigned positive scores (excluding tea and coffee)
  p_score_3Q("wholegrain") +
  p_score_3Q("fruit") +
  p_score_3Q("vegetable") +
  p_score_3Q("nut") +
  p_score_3Q("legume") +
  p_score_3Q("vegetableoil") +
  
  ### 5 less healthy plant food groups assigned reverse scores
  r_score_3Q("fruitjuice") +
  r_score_3Q("refinedgrain") +
  r_score_3Q("potato") +
  r_score_3Q("sugarbeverage") +
  r_score_3Q("sweetdessert") +
  
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat") +
  r_score_3Q("dairy") +
  r_score_3Q("egg") +
  r_score_3Q("fishseafood") +
  r_score_3Q("meat") +
  r_score_3Q("misc.animal")

dat$hPDIm
summary(dat$hPDIm)

## MODIFIED unhealthful plant-based diet index (uPDIm)
dat$uPDIm <-
  ### 6 healthy plant food groups assigned reverse scores (excluding tea and coffee)
  r_score_3Q("wholegrain") +
  r_score_3Q("fruit") +
  r_score_3Q("vegetable") +
  r_score_3Q("nut") +
  r_score_3Q("legume") +
  r_score_3Q("vegetableoil") +
  
  ### 5 less healthy plant food groups assigned positive scores
  p_score_3Q("fruitjuice") +
  p_score_3Q("refinedgrain") +
  p_score_3Q("potato") +
  p_score_3Q("sugarbeverage") +
  p_score_3Q("sweetdessert") +
  
  ### 6 animal food groups assigned reverse scores
  r_score_3Q("animalfat") +
  r_score_3Q("dairy") +
  r_score_3Q("egg") +
  r_score_3Q("fishseafood") +
  r_score_3Q("meat") +
  r_score_3Q("misc.animal")

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

# Check and save data
dat <- as.tibble(dat)
head(dat)
dim(dat)  # 73868   XXX

saveRDS(dat, "dat_exp.rds")

################################################################################
