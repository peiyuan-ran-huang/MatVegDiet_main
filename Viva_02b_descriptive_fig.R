################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 18-May-2024
# This script is to produce main descriptive figures in Project Viva.

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
  openxlsx,
  haven,
  expss,
  gtsummary,
  kableExtra,
  flextable,
  readr,
  magrittr,
  ggplot2,
  hrbrthemes
)


# Set working directory
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/Viva/dat_exp_cov_out.rds")
dat <- zap_labels(dat)

head(dat)
dim(dat)  # 1872  606

#------------------------------------------------------------------------------#
#                             Descriptive Figures                              #----
#------------------------------------------------------------------------------#

# Correlation between early and mid-pregnancy dietary variables
corr_data_1 <- subset(
  dat,
  select = c(
    "PDI_1",
    "hPDI_1",
    "uPDI_1",
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
)
colnames(corr_data_1) <- as.character(sapply(corr_data_1, var_lab))

corr_data_2 <- subset(
  dat,
  select = c(
    "PDI_2",
    "hPDI_2",
    "uPDI_2",
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
)
colnames(corr_data_2) <- as.character(sapply(corr_data_2, var_lab))

correlation_matrix <-
  cor(corr_data_1, corr_data_2, method = "pearson", use = "pairwise.complete.obs")

png(
  "Z:/working/results/Viva/corr_diet_EAR.vs.MID.p.png",
  width = 1000,
  height = 1000
)

p <- ggcorrplot(
  correlation_matrix,
  hc.order = F,
  outline.col = "white",
  ggtheme = ggplot2::theme_minimal(),
  lab = T,
  lab_size = 4,
  lab_col = "black"
) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 14,
      colour = "black"
    ),
    axis.text.y = element_text(size = 14, colour = "black")
  )

print(p)

grid::grid.text(
  "In early pregnancy",
  x = 0.5,
  y = 0.05,
  gp = grid::gpar(fontsize = 20)
)
grid::grid.text(
  "In mid-pregnancy",
  x = 0.005,
  y = 0.5,
  rot = 90,
  gp = grid::gpar(fontsize = 20)
)

dev.off()

################################################################################

# Correlation matrix between PDIs and intakes of the 18 food groups

## Early pregnancy
corr_diet_EAR.p <-
  GGally::ggcorr(
    data = subset(
      dat,
      select = c(
        "misc.animal_1",
        "meat_1",
        "fishseafood_1",
        "egg_1",
        "dairy_1",
        "animalfat_1",
        "sweetdessert_1",
        "sugarbeverage_1",
        "potato_1",
        "refinedgrain_1",
        "fruitjuice_1",
        "teacoffee_1",
        "vegetableoil_1",
        "legume_1",
        "nut_1",
        "vegetable_1",
        "fruit_1",
        "wholegrain_1",
        "uPDI_1",
        "hPDI_1",
        "PDI_1"
      )
    ),
    method = c("pairwise", "pearson"),
    label = T,
    label_alpha = T,
    label_round = 2
  )

ggplot2::ggsave(
  corr_diet_EAR.p,
  file = paste0("Z:/working/results/Viva/corr_diet_EAR.p.png"),
  height = 16,
  width = 18
)

################################################################################
################################################################################
################################################################################

## Mid-pregnancy
corr_diet_MID.p <-
  GGally::ggcorr(
    data = subset(
      dat,
      select = c(
        "misc.animal_2",
        "meat_2",
        "fishseafood_2",
        "egg_2",
        "dairy_2",
        "animalfat_2",
        "sweetdessert_2",
        "sugarbeverage_2",
        "potato_2",
        "refinedgrain_2",
        "fruitjuice_2",
        "teacoffee_2",
        "vegetableoil_2",
        "legume_2",
        "nut_2",
        "vegetable_2",
        "fruit_2",
        "wholegrain_2",
        "uPDI_2",
        "hPDI_2",
        "PDI_2"
      )
    ),
    method = c("pairwise", "pearson"),
    label = T,
    label_alpha = T,
    label_round = 2
  )

ggplot2::ggsave(
  corr_diet_MID.p,
  file = paste0("Z:/working/results/Viva/corr_diet_MID.p.png"),
  height = 16,
  width = 18
)

################################################################################
################################################################################
################################################################################

## From early to mid-pregnancy
corr_data <- subset(
  dat,
  select = c(
    "misc.animal",
    "meat",
    "fishseafood",
    "egg",
    "dairy",
    "animalfat",
    "sweetdessert",
    "sugarbeverage",
    "potato",
    "refinedgrain",
    "fruitjuice",
    "teacoffee",
    "vegetableoil",
    "legume",
    "nut",
    "vegetable",
    "fruit",
    "wholegrain",
    "uPDI",
    "hPDI",
    "PDI"
  )
)
colnames(corr_data) <-
  c(
    "Misc. animal",
    "Meat",
    "Fish seafood",
    "Eggs",
    "Dairy",
    "Animal fat",
    "Sweets desserts",
    "SSB",
    "Potatoes",
    "Refined grains",
    "Fruit juices",
    "Tea coffee",
    "Vegetable oils",
    "Legumes",
    "Nuts",
    "Vegetables",
    "Fruits",
    "Whole grains",
    "uPDI",
    "hPDI",
    "PDI"
  )

corr_diet_DUR.p <-
  GGally::ggcorr(
    data = corr_data,
    method = c("pairwise", "pearson"),
    label = T,
    label_alpha = T,
    label_round = 2,
    label_size = 5,
    label_color = "black"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 14,
      colour = "black"
    ),
    axis.text.y = element_text(size = 14, colour = "black")
  )

ggplot2::ggsave(
  corr_diet_DUR.p,
  file = paste0("Z:/working/results/Viva/corr_diet_DUR.p.png"),
  height = 16,
  width = 18
)

################################################################################
