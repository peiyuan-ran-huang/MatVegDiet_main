################################################################################
#       Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC      #
################################################################################

# Last edited date: 25-Jul-2024
# This script is to produce main descriptive figures in ALSPAC.

################################################################################

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
  hrbrthemes,
  ggcorrplot
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/ALSPAC/dat_exp_cov_out_pat.rds")
head(dat)
dim(dat)  # 11693  XXX

###############################################################################

# Correlation matrix between PDIs and intakes of the 18 food groups
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

correlogram_PDIs_foods_DUR.p <-
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

ggsave(
  filename = "results/ALSPAC/corr_PDIs_foods_DUR.p.png",
  plot = correlogram_PDIs_foods_DUR.p,
  height = 16,
  width = 18
)

################################################################################

# Correlation matrix between PDIs and nutrient intakes

## Rename variables as labels
nutrient_labels <-
  read.xlsx(
    "data/ALSPAC/ALSPAC_data_catalog_C.xlsx",
    sheet = "Nutrients",
    colNames = T,
    rowNames = F
  )
nutrient_labels
str(nutrient_labels)  # 36 nutrients
nutrient_labels <- nutrient_labels[2:36, ]  # Drop energy

corr_data <- subset(dat, select = c("PDI", "hPDI", "uPDI", nutrient_labels$Varname))

colnames(corr_data) <- c("PDI", "hPDI", "uPDI", nutrient_labels$VarLabel)

## Compute the correlation matrix
corr_matrix <-
  cor(corr_data, use = "pairwise.complete.obs", method = "pearson")

## Use ggcorrplot to generate half correlation matrix plot
correlogram_PDIs_nutrients_DUR.p <- ggcorrplot(
  corr_matrix,
  method = "square",
  type = "lower",
  # Only show the lower half
  lab = TRUE,
  lab_size = 4,
  lab_col = "black",
  show.legend = TRUE,
  tl.cex = 10,
  tl.srt = 45,
  # Rotate labels
  hc.order = FALSE
) + # Set to FALSE to keep the original order
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 14,
      color = "black"
    ),
    # Rotate x-axis labels
    axis.text.y = element_text(size = 14, color = "black"),
    # Adjust y-axis label size
    axis.title.x = element_blank(),
    # Hide x-axis title
    axis.title.y = element_blank(),
    # Hide y-axis title
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  # Reduce plot margins
  )

## Save the plot
ggsave(
  filename = "results/ALSPAC/corr_PDIs_nutrients_DUR.p.png",
  plot = correlogram_PDIs_nutrients_DUR.p,
  height = 22,
  width = 26
)
