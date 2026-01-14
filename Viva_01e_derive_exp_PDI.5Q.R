################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 18-May-2024
# This script is to derive plant-based diet indices (PDIs) based on food intake quintiles (instead of tertiles) in Project Viva.

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

# Load data
dat <- readRDS("data/Viva/dat_exp.rds")
dat <- zap_labels(dat)

head(dat)
dim(dat)  # 1872  534

dat <- as.data.frame(dat)

#------------------------------------------------------------------------------#
#                   Quintile-based Plant-based Diet Indices                    #----
#------------------------------------------------------------------------------#

# Create functions for positive and negative scoring
p_score_5Q <- function(var_name) {
  my_var <- dat[, var_name]
  
  my_var_sorted <- sort(my_var, na.last = TRUE)
  
  q_breaks <- quantile(my_var_sorted, probs = (0:5) / 5, na.rm = T)
  
  for (i in 2:5) {
    idx <- which.min(abs(my_var_sorted - q_breaks[i]))
    while (idx < length(my_var_sorted) &&
           my_var_sorted[idx] == my_var_sorted[idx + 1]) {
      idx <- idx + 1
    }
    if (idx < length(my_var_sorted)) {
      while (my_var_sorted[idx + 1] == q_breaks[i - 1] &&
             idx < length(my_var_sorted)) {
        idx <- idx + 1
      }
      q_breaks[i] <- my_var_sorted[idx + 1]
    }
  }
  
  quintile_labels <-
    cut(
      my_var_sorted,
      breaks = q_breaks,
      labels = FALSE,
      include.lowest = TRUE
    )
  
  my_var_5Q <- quintile_labels[order(order(my_var))]
  
  return(my_var_5Q)
}

r_score_5Q <- function(var_name) {
  my_var_5Q <-
    p_score_5Q(var_name)
  my_var_5Q <-
    dplyr::recode(
      my_var_5Q,
      "1" = 5,
      "2" = 4,
      "3" = 3,
      "4" = 2,
      "5" = 1
    )
  return(my_var_5Q)
}

quintile_cutoff <- function(var_name) {
  my_var <- dat[, var_name]
  my_var_5Q <- p_score_5Q(var_name)
  Q1_lo <- round(min(my_var[my_var_5Q == 1], na.rm = T), digits = 2)
  Q1_up <- round(max(my_var[my_var_5Q == 1], na.rm = T), digits = 2)
  Q2_lo <- round(min(my_var[my_var_5Q == 2], na.rm = T), digits = 2)
  Q2_up <- round(max(my_var[my_var_5Q == 2], na.rm = T), digits = 2)
  Q3_lo <- round(min(my_var[my_var_5Q == 3], na.rm = T), digits = 2)
  Q3_up <- round(max(my_var[my_var_5Q == 3], na.rm = T), digits = 2)
  Q4_lo <- round(min(my_var[my_var_5Q == 4], na.rm = T), digits = 2)
  Q4_up <- round(max(my_var[my_var_5Q == 4], na.rm = T), digits = 2)
  Q5_lo <- round(min(my_var[my_var_5Q == 5], na.rm = T), digits = 2)
  Q5_up <- round(max(my_var[my_var_5Q == 5], na.rm = T), digits = 2)
  x <-
    paste0(
      "Q1: ",
      Q1_lo,
      "-",
      Q1_up,
      " | Q2: ",
      Q2_lo,
      "-",
      Q2_up,
      " | Q3: ",
      Q3_lo,
      "-",
      Q3_up,
      " | Q4: ",
      Q4_lo,
      "-",
      Q4_up,
      " | Q5: ",
      Q5_lo,
      "-",
      Q5_up
    )
  return(x)
}

describe_food_group <- function(var_name) {
  x <- c(
    `Food group` = var_name,
    "Cutoffs" = quintile_cutoff(var_name),
    table(factor(
      p_score_5Q(var_name), label = c("Q1", "Q2", "Q3", "Q4", "Q5")
    ), useNA = "always")
  )
  return(x)
}

################################################################################

# Apply functions to early pregnancy diets

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

for (var_name in food_group_name) {
  assign(paste0(var_name, "_5Q"), factor(p_score_5Q(var_name), label = c("Q1", "Q2", "Q3", "Q4", "Q5")))
  dat <- cbind(dat, get(paste0(var_name, "_5Q")))
}
for (i in 1:18) {
  colnames(dat)[ncol(dat) - 18 + i] <-
    paste0(food_group_name[i], "_5Q")
}
head(dat)

food_group_quintile <- data.frame(matrix(ncol = 8, nrow = 0))
for (var_name in food_group_name) {
  x <- describe_food_group(var_name)
  food_group_quintile <- rbind(food_group_quintile, x)
}
colnames(food_group_quintile) <-
  c("Food_group",
    "Cutoffs",
    "N_Q1",
    "N_Q2",
    "N_Q3",
    "N_Q4",
    "N_Q5",
    "N_missing")
write.csv(food_group_quintile,
          "results/Viva/food_group_quintile_1.csv",
          row.names = F)

dat$PDI_5Q.based_1 <-
  p_score_5Q("wholegrain_1") + p_score_5Q("fruit_1") + p_score_5Q("vegetable_1") + p_score_5Q("nut_1") + p_score_5Q("legume_1") + p_score_5Q("vegetableoil_1") + p_score_5Q("teacoffee_1") +
  p_score_5Q("fruitjuice_1") + p_score_5Q("refinedgrain_1") + p_score_5Q("potato_1") + p_score_5Q("sugarbeverage_1") + p_score_5Q("sweetdessert_1") +
  r_score_5Q("animalfat_1") + r_score_5Q("dairy_1") + r_score_5Q("egg_1") + r_score_5Q("fishseafood_1") + r_score_5Q("meat_1") + r_score_5Q("misc.animal_1")

dat$hPDI_5Q.based_1 <-
  p_score_5Q("wholegrain_1") + p_score_5Q("fruit_1") + p_score_5Q("vegetable_1") + p_score_5Q("nut_1") + p_score_5Q("legume_1") + p_score_5Q("vegetableoil_1") + p_score_5Q("teacoffee_1") +
  r_score_5Q("fruitjuice_1") + r_score_5Q("refinedgrain_1") + r_score_5Q("potato_1") + r_score_5Q("sugarbeverage_1") + r_score_5Q("sweetdessert_1") +
  r_score_5Q("animalfat_1") + r_score_5Q("dairy_1") + r_score_5Q("egg_1") + r_score_5Q("fishseafood_1") + r_score_5Q("meat_1") + r_score_5Q("misc.animal_1")

dat$uPDI_5Q.based_1 <-
  r_score_5Q("wholegrain_1") + r_score_5Q("fruit_1") + r_score_5Q("vegetable_1") + r_score_5Q("nut_1") + r_score_5Q("legume_1") + r_score_5Q("vegetableoil_1") + r_score_5Q("teacoffee_1") +
  p_score_5Q("fruitjuice_1") + p_score_5Q("refinedgrain_1") + p_score_5Q("potato_1") + p_score_5Q("sugarbeverage_1") + p_score_5Q("sweetdessert_1") +
  r_score_5Q("animalfat_1") + r_score_5Q("dairy_1") + r_score_5Q("egg_1") + r_score_5Q("fishseafood_1") + r_score_5Q("meat_1") + r_score_5Q("misc.animal_1")

################################################################################

# Apply functions to mid-pregnancy diets

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

for (var_name in food_group_name) {
  assign(paste0(var_name, "_5Q"), factor(p_score_5Q(var_name), label = c("Q1", "Q2", "Q3", "Q4", "Q5")))
  dat <- cbind(dat, get(paste0(var_name, "_5Q")))
}
for (i in 1:18) {
  colnames(dat)[ncol(dat) - 18 + i] <-
    paste0(food_group_name[i], "_5Q")
}
head(dat)

food_group_quintile <- data.frame(matrix(ncol = 8, nrow = 0))
for (var_name in food_group_name) {
  x <- describe_food_group(var_name)
  food_group_quintile <- rbind(food_group_quintile, x)
}
colnames(food_group_quintile) <-
  c("Food_group",
    "Cutoffs",
    "N_Q1",
    "N_Q2",
    "N_Q3",
    "N_Q4",
    "N_Q5",
    "N_missing")
write.csv(food_group_quintile,
          "results/Viva/food_group_quintile_2.csv",
          row.names = F)

dat$PDI_5Q.based_2 <-
  p_score_5Q("wholegrain_2") + p_score_5Q("fruit_2") + p_score_5Q("vegetable_2") + p_score_5Q("nut_2") + p_score_5Q("legume_2") + p_score_5Q("vegetableoil_2") + p_score_5Q("teacoffee_2") +
  p_score_5Q("fruitjuice_2") + p_score_5Q("refinedgrain_2") + p_score_5Q("potato_2") + p_score_5Q("sugarbeverage_2") + p_score_5Q("sweetdessert_2") +
  r_score_5Q("animalfat_2") + r_score_5Q("dairy_2") + r_score_5Q("egg_2") + r_score_5Q("fishseafood_2") + r_score_5Q("meat_2") + r_score_5Q("misc.animal_2")

dat$hPDI_5Q.based_2 <-
  p_score_5Q("wholegrain_2") + p_score_5Q("fruit_2") + p_score_5Q("vegetable_2") + p_score_5Q("nut_2") + p_score_5Q("legume_2") + p_score_5Q("vegetableoil_2") + p_score_5Q("teacoffee_2") +
  r_score_5Q("fruitjuice_2") + r_score_5Q("refinedgrain_2") + r_score_5Q("potato_2") + r_score_5Q("sugarbeverage_2") + r_score_5Q("sweetdessert_2") +
  r_score_5Q("animalfat_2") + r_score_5Q("dairy_2") + r_score_5Q("egg_2") + r_score_5Q("fishseafood_2") + r_score_5Q("meat_2") + r_score_5Q("misc.animal_2")

dat$uPDI_5Q.based_2 <-
  r_score_5Q("wholegrain_2") + r_score_5Q("fruit_2") + r_score_5Q("vegetable_2") + r_score_5Q("nut_2") + r_score_5Q("legume_2") + r_score_5Q("vegetableoil_2") + r_score_5Q("teacoffee_2") +
  p_score_5Q("fruitjuice_2") + p_score_5Q("refinedgrain_2") + p_score_5Q("potato_2") + p_score_5Q("sugarbeverage_2") + p_score_5Q("sweetdessert_2") +
  r_score_5Q("animalfat_2") + r_score_5Q("dairy_2") + r_score_5Q("egg_2") + r_score_5Q("fishseafood_2") + r_score_5Q("meat_2") + r_score_5Q("misc.animal_2")

################################################################################

# Apply functions to diets from early to mid-pregnancy

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

for (var_name in food_group_name) {
  assign(paste0(var_name, "_5Q"), factor(p_score_5Q(var_name), label = c("Q1", "Q2", "Q3", "Q4", "Q5")))
  dat <- cbind(dat, get(paste0(var_name, "_5Q")))
}
for (i in 1:18) {
  colnames(dat)[ncol(dat) - 18 + i] <-
    paste0(food_group_name[i], "_5Q")
}
head(dat)

food_group_quintile <- data.frame(matrix(ncol = 8, nrow = 0))
for (var_name in food_group_name) {
  x <- describe_food_group(var_name)
  food_group_quintile <- rbind(food_group_quintile, x)
}
colnames(food_group_quintile) <-
  c("Food_group",
    "Cutoffs",
    "N_Q1",
    "N_Q2",
    "N_Q3",
    "N_Q4",
    "N_Q5",
    "N_missing")
write.csv(food_group_quintile,
          "results/Viva/food_group_quintile.csv",
          row.names = F)

dat$PDI_5Q.based <-
  p_score_5Q("wholegrain") + p_score_5Q("fruit") + p_score_5Q("vegetable") + p_score_5Q("nut") + p_score_5Q("legume") + p_score_5Q("vegetableoil") + p_score_5Q("teacoffee") +
  p_score_5Q("fruitjuice") + p_score_5Q("refinedgrain") + p_score_5Q("potato") + p_score_5Q("sugarbeverage") + p_score_5Q("sweetdessert") +
  r_score_5Q("animalfat") + r_score_5Q("dairy") + r_score_5Q("egg") + r_score_5Q("fishseafood") + r_score_5Q("meat") + r_score_5Q("misc.animal")

dat$hPDI_5Q.based <-
  p_score_5Q("wholegrain") + p_score_5Q("fruit") + p_score_5Q("vegetable") + p_score_5Q("nut") + p_score_5Q("legume") + p_score_5Q("vegetableoil") + p_score_5Q("teacoffee") +
  r_score_5Q("fruitjuice") + r_score_5Q("refinedgrain") + r_score_5Q("potato") + r_score_5Q("sugarbeverage") + r_score_5Q("sweetdessert") +
  r_score_5Q("animalfat") + r_score_5Q("dairy") + r_score_5Q("egg") + r_score_5Q("fishseafood") + r_score_5Q("meat") + r_score_5Q("misc.animal")

dat$uPDI_5Q.based <-
  r_score_5Q("wholegrain") + r_score_5Q("fruit") + r_score_5Q("vegetable") + r_score_5Q("nut") + r_score_5Q("legume") + r_score_5Q("vegetableoil") + r_score_5Q("teacoffee") +
  p_score_5Q("fruitjuice") + p_score_5Q("refinedgrain") + p_score_5Q("potato") + p_score_5Q("sugarbeverage") + p_score_5Q("sweetdessert") +
  r_score_5Q("animalfat") + r_score_5Q("dairy") + r_score_5Q("egg") + r_score_5Q("fishseafood") + r_score_5Q("meat") + r_score_5Q("misc.animal")

#------------------------------------------------------------------------------#
#                                 Correlogram                                  #----
#------------------------------------------------------------------------------#

# Correlation between tertile-based and quintile-based PDIs
corr_data_1 <- subset(
  dat,
  select = c(
    "PDI_1",
    "hPDI_1",
    "uPDI_1",
    "PDI_2",
    "hPDI_2",
    "uPDI_2",
    "PDI",
    "hPDI",
    "uPDI"
  )
)
colnames(corr_data_1) <- c(
  "PDI (early pregnancy)",
  "hPDI (early pregnancy)",
  "uPDI (early pregnancy)",
  "PDI (mid-pregnancy)",
  "hPDI (mid-pregnancy)",
  "uPDI (mid-pregnancy)",
  "PDI (during pregnancy)",
  "hPDI (during pregnancy)",
  "uPDI (during pregnancy)"
)

corr_data_2 <- subset(
  dat,
  select = c(
    "PDI_5Q.based_1",
    "hPDI_5Q.based_1",
    "uPDI_5Q.based_1",
    "PDI_5Q.based_2",
    "hPDI_5Q.based_2",
    "uPDI_5Q.based_2",
    "PDI_5Q.based",
    "hPDI_5Q.based",
    "uPDI_5Q.based"
  )
)
colnames(corr_data_2) <- c(
  "PDI (early pregnancy)",
  "hPDI (early pregnancy)",
  "uPDI (early pregnancy)",
  "PDI (mid-pregnancy)",
  "hPDI (mid-pregnancy)",
  "uPDI (mid-pregnancy)",
  "PDI (during pregnancy)",
  "hPDI (during pregnancy)",
  "uPDI (during pregnancy)"
)

correlation_matrix <-
  cor(corr_data_1, corr_data_2, method = "pearson", use = "pairwise.complete.obs")

png(
  "results/Viva/correlogram_PDI.hPDI.uPDI_3Q.vs.5Q.based.png",
  width = 900,
  height = 770
)

p <- ggcorrplot(
  correlation_matrix,
  hc.order = F,
  outline.col = "white",
  ggtheme = ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(
      t = 0,
      r = 20,
      b = 0,
      l = 0
    ))),
  lab = T,
  lab_size = 5,
  lab_col = "black"
) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 13,
      colour = "black"
    ),
    axis.text.y = element_text(size = 13, colour = "black")
  )

print(p)

grid::grid.text(
  "Derived from tertiles",
  x = 0.5,
  y = 0.015,
  gp = grid::gpar(fontsize = 18)
)
grid::grid.text(
  "Derived from quintiles",
  x = 0.01,
  y = 0.5,
  rot = 90,
  gp = grid::gpar(fontsize = 18)
)

dev.off()
