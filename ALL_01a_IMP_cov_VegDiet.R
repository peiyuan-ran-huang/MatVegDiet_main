################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 20-May-2025
# This script is to combine cohort-specific results for the association/effect of key covariates with/on vegetarianism (with imputed data).

#------------------------------------------------------------------------------#
#                                 Housekeeping                                 #----
#------------------------------------------------------------------------------#

# Clear environment
rm(list = ls())

# Collect information about the current R session
sessionInfo()

# Load packages
pacman::p_load(tidyverse,
               openxlsx,
               gtsummary,
               ggplot2,
               reshape2,
               gridExtra,
               patchwork)

# Set working directory
setwd("Z:/working/results/")

#------------------------------------------------------------------------------#
#                               Data Preparation                               #----
#------------------------------------------------------------------------------#

# Load and prepare cohort-specific results

## ALSPAC
obs.res_VegDiet_bin_ALSPAC <-
  read.xlsx("ALSPAC/IMP_cov_obs.res_VegDiet_bin.xlsx")

obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval", "OR")] <-
  sapply(obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval", "OR")], as.numeric)
obs.res_VegDiet_bin_ALSPAC$Study <- "ALSPAC"

obs.res_VegDiet_bin_ALSPAC$Vegetarianism  <- factor(
  obs.res_VegDiet_bin_ALSPAC$Vegetarianism ,
  levels = c(
    "Pesco-vegetarian\nvs. non-vegetarian",
    "Full vegetarian\nvs. non-vegetarian"
  )
)

obs.res_VegDiet_bin_ALSPAC$Covariate <- ifelse(
  obs.res_VegDiet_bin_ALSPAC$Covariate == "Ethnicity\n(Other vs. White)",
  "Race/ethnicity\n(Other vs. White)",
  obs.res_VegDiet_bin_ALSPAC$Covariate
)

obs.res_VegDiet_bin_ALSPAC
str(obs.res_VegDiet_bin_ALSPAC)  # 14 outcomes * 2 models = 28 obs.

## BiB
obs.res_VegDiet_bin_BiB <-
  read.xlsx("BiB/IMP_cov_obs.res_VegDiet_bin.xlsx")

obs.res_VegDiet_bin_BiB[, c("b", "se", "pval", "OR")] <-
  sapply(obs.res_VegDiet_bin_BiB[, c("b", "se", "pval", "OR")], as.numeric)
obs.res_VegDiet_bin_BiB$Study <- "BiB"

obs.res_VegDiet_bin_BiB$Vegetarianism  <- factor(
  obs.res_VegDiet_bin_BiB$Vegetarianism ,
  levels = c(
    "Pesco-vegetarian\nvs. non-vegetarian",
    "Full vegetarian\nvs. non-vegetarian"
  )
)

obs.res_VegDiet_bin_BiB$Covariate <- ifelse(
  obs.res_VegDiet_bin_BiB$Covariate == "Ethnicity\n(Other vs. White)",
  "Race/ethnicity\n(Other vs. White)",
  obs.res_VegDiet_bin_BiB$Covariate
)

obs.res_VegDiet_bin_BiB
str(obs.res_VegDiet_bin_BiB)  # 10 outcomes * 2 models = 20 obs.

## MoBa
obs.res_VegDiet_bin_MoBa <-
  read.xlsx("MoBa/IMP_cov_obs.res_VegDiet_bin.xlsx")

obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval", "OR")] <-
  sapply(obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval", "OR")], as.numeric)
obs.res_VegDiet_bin_MoBa$Study <- "MoBa"

obs.res_VegDiet_bin_MoBa$Vegetarianism  <- factor(
  obs.res_VegDiet_bin_MoBa$Vegetarianism ,
  levels = c(
    "Pesco-vegetarian\nvs. non-vegetarian",
    "Full vegetarian\nvs. non-vegetarian"
  )
)

obs.res_VegDiet_bin_MoBa$Covariate <- ifelse(
  obs.res_VegDiet_bin_MoBa$Covariate == "Ethnicity\n(Other vs. White)",
  "Race/ethnicity\n(Other vs. White)",
  obs.res_VegDiet_bin_MoBa$Covariate
)

obs.res_VegDiet_bin_MoBa
str(obs.res_VegDiet_bin_MoBa)  # 14 outcomes * 2 models = 28 obs.

## Project Viva
obs.res_VegDiet_bin_Viva <-
  read.xlsx("Viva/IMP_cov_obs.res_VegDiet_bin.xlsx")

obs.res_VegDiet_bin_Viva[, c("b", "se", "pval", "OR")] <-
  sapply(obs.res_VegDiet_bin_Viva[, c("b", "se", "pval", "OR")], as.numeric)
obs.res_VegDiet_bin_Viva$Study <- "Viva"

obs.res_VegDiet_bin_Viva$Vegetarianism  <- factor(
  obs.res_VegDiet_bin_Viva$Vegetarianism ,
  levels = c(
    "Pesco-vegetarian\nvs. non-vegetarian",
    "Full vegetarian\nvs. non-vegetarian"
  )
)

obs.res_VegDiet_bin_Viva$Covariate <- ifelse(
  obs.res_VegDiet_bin_Viva$Covariate == "Ethnicity\n(Other vs. White)",
  "Race/ethnicity\n(Other vs. White)",
  obs.res_VegDiet_bin_Viva$Covariate
)

obs.res_VegDiet_bin_Viva
str(obs.res_VegDiet_bin_Viva)  # 14 outcomes * 2 models = 28 obs.

################################################################################

# Create template (including all available covariates across all cohorts)
template_all_cov <- subset(obs.res_VegDiet_bin_ALSPAC,
                           select = c(Covariate, Vegetarianism))

template_all_cov <- rbind(
  template_all_cov[1:8, ],
  data.frame(Covariate = "Household income\n(higher vs. lower)", Vegetarianism = "Pesco-vegetarian\nvs. non-vegetarian"),
  data.frame(Covariate = "Household income\n(higher vs. lower)", Vegetarianism = "Full vegetarian\nvs. non-vegetarian"),
  template_all_cov[9:nrow(template_all_cov), ]
)

template_all_cov

# Combine cohort-specific results with template
obs.res_VegDiet_bin_ALSPAC <- left_join(template_all_cov,
                                        obs.res_VegDiet_bin_ALSPAC,
                                        by = c("Covariate", "Vegetarianism"))
obs.res_VegDiet_bin_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_bin_ALSPAC$Group <- c(
  rep("Demographic", 2),
  rep("Demographic", 2),
  rep("Socioeconomic", 2),
  rep("Socioeconomic", 2),
  rep("Socioeconomic", 2),
  rep("Demographic", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Demographic", 2)
)
obs.res_VegDiet_bin_ALSPAC$Group <-
  factor(
    obs.res_VegDiet_bin_ALSPAC$Group,
    levels = c("Demographic", "Socioeconomic", "Lifestyle")
  )
obs.res_VegDiet_bin_ALSPAC

obs.res_VegDiet_bin_BiB <- left_join(template_all_cov,
                                     obs.res_VegDiet_bin_BiB,
                                     by = c("Covariate", "Vegetarianism"))
obs.res_VegDiet_bin_BiB$Study <- "BiB"
obs.res_VegDiet_bin_BiB$Group <- c(
  rep("Demographic", 2),
  rep("Demographic", 2),
  rep("Socioeconomic", 2),
  rep("Socioeconomic", 2),
  rep("Socioeconomic", 2),
  rep("Demographic", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Demographic", 2)
)
obs.res_VegDiet_bin_BiB$Group <-
  factor(
    obs.res_VegDiet_bin_BiB$Group,
    levels = c("Demographic", "Socioeconomic", "Lifestyle")
  )
obs.res_VegDiet_bin_BiB

obs.res_VegDiet_bin_MoBa <- left_join(template_all_cov,
                                      obs.res_VegDiet_bin_MoBa,
                                      by = c("Covariate", "Vegetarianism"))
obs.res_VegDiet_bin_MoBa$Study <- "MoBa"
obs.res_VegDiet_bin_MoBa$Group <- c(
  rep("Demographic", 2),
  rep("Demographic", 2),
  rep("Socioeconomic", 2),
  rep("Socioeconomic", 2),
  rep("Socioeconomic", 2),
  rep("Demographic", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Demographic", 2)
)
obs.res_VegDiet_bin_MoBa$Group <-
  factor(
    obs.res_VegDiet_bin_MoBa$Group,
    levels = c("Demographic", "Socioeconomic", "Lifestyle")
  )
obs.res_VegDiet_bin_MoBa

obs.res_VegDiet_bin_Viva <- left_join(template_all_cov,
                                      obs.res_VegDiet_bin_Viva,
                                      by = c("Covariate", "Vegetarianism"))
obs.res_VegDiet_bin_Viva$Study <- "Project Viva"
obs.res_VegDiet_bin_Viva$Group <- c(
  rep("Demographic", 2),
  rep("Demographic", 2),
  rep("Socioeconomic", 2),
  rep("Socioeconomic", 2),
  rep("Socioeconomic", 2),
  rep("Demographic", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Lifestyle", 2),
  rep("Demographic", 2)
)
obs.res_VegDiet_bin_Viva$Group <-
  factor(
    obs.res_VegDiet_bin_Viva$Group,
    levels = c("Demographic", "Socioeconomic", "Lifestyle")
  )
obs.res_VegDiet_bin_Viva

# # Calculate beta/SE (i.e., logOR/SE) - !!! Use "beta or logOR" now !!!
# obs.res_VegDiet_bin_ALSPAC$`Beta/SE` <- obs.res_VegDiet_bin_ALSPAC$b / obs.res_VegDiet_bin_ALSPAC$se
# obs.res_VegDiet_bin_BiB$`Beta/SE` <- obs.res_VegDiet_bin_BiB$b / obs.res_VegDiet_bin_BiB$se
# obs.res_VegDiet_bin_MoBa$`Beta/SE` <- obs.res_VegDiet_bin_MoBa$b / obs.res_VegDiet_bin_MoBa$se
# obs.res_VegDiet_bin_Viva$`Beta/SE` <- obs.res_VegDiet_bin_Viva$b / obs.res_VegDiet_bin_Viva$se

# Combine cohort-specific results
obs.res_VegDiet_bin <- rbind(
  obs.res_VegDiet_bin_ALSPAC,
  obs.res_VegDiet_bin_BiB,
  obs.res_VegDiet_bin_MoBa,
  obs.res_VegDiet_bin_Viva
)

obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 120 obs. = 30 + 30 + 30 + 30

################################################################################
## Check the upper and lower bound of heatmap legend
min(obs.res_VegDiet_bin$b, na.rm = T)  # -16.22886 (BUT an extreme outlier - !!! TRUNCATED TO ln(0.1) !!!)
min(obs.res_VegDiet_bin$OR, na.rm = T)  # 8.951488e-08 (BUT an extreme outlier - !!! TRUNCATED TO 0.1 !!!)

sort(obs.res_VegDiet_bin$b,
     na.last = NA,
     partial = 2)[2]  # -1.916933 as the second smallest value
sort(obs.res_VegDiet_bin$OR,
     na.last = NA,
     partial = 2)[2]  # 0.1470573 as the second smallest value

max(obs.res_VegDiet_bin$b, na.rm = T)  # 1.705808
max(obs.res_VegDiet_bin$OR, na.rm = T)  # 5.50583
################################################################################

#------------------------------------------------------------------------------#
#                               Heatmap matrices                               #----
#------------------------------------------------------------------------------#

# Unify breaks and labels (logOR used but displayed as OR)
ln_breaks <- c(log(0.1), log(0.2), log(0.5), log(1), log(2), log(4), log(6))
ln_breaks
ln_limit <- c(log(0.1), log(6))

OR_labels <- c(0.1, 0.2, 0.5, 1, 2, 4, 6)
OR_labels

# Cohort-specific heatmaps

## ALSPAC

################################################################################
### Rename covariates
obs.res_VegDiet_bin_ALSPAC$Covariate <- ifelse(
  obs.res_VegDiet_bin_ALSPAC$Covariate == "Education\n(higher vs. lower)",
  "Educational attainment\n(higher vs. lower)",
  obs.res_VegDiet_bin_ALSPAC$Covariate
)

obs.res_VegDiet_bin_ALSPAC$Covariate <- ifelse(
  obs.res_VegDiet_bin_ALSPAC$Covariate == "IMD\n(more vs. less affluent)",
  "Index of multiple deprivation\n(more vs. less affluent)",
  obs.res_VegDiet_bin_ALSPAC$Covariate
)

obs.res_VegDiet_bin_ALSPAC$Covariate <- ifelse(
  obs.res_VegDiet_bin_ALSPAC$Covariate == "Pre-pregnancy BMI\n(in kg/m^2)",
  "Body mass index\n(in kg/m^2)",
  obs.res_VegDiet_bin_ALSPAC$Covariate
)

obs.res_VegDiet_bin_ALSPAC$Covariate <- ifelse(
  obs.res_VegDiet_bin_ALSPAC$Covariate == "Overall PDI",
  "Overall\nplant-based diet index",
  obs.res_VegDiet_bin_ALSPAC$Covariate
)

obs.res_VegDiet_bin_ALSPAC$Covariate <- ifelse(
  obs.res_VegDiet_bin_ALSPAC$Covariate == "Healthful PDI",
  "Healthful\nplant-based diet index",
  obs.res_VegDiet_bin_ALSPAC$Covariate
)

obs.res_VegDiet_bin_ALSPAC$Covariate <- ifelse(
  obs.res_VegDiet_bin_ALSPAC$Covariate == "Unhealthful PDI",
  "Unhealthful\nplant-based diet index",
  obs.res_VegDiet_bin_ALSPAC$Covariate
)
################################################################################

### Create a new column for significance levels
obs.res_VegDiet_bin_ALSPAC$Sig <- cut(
  obs.res_VegDiet_bin_ALSPAC$pval,
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
)
obs.res_VegDiet_bin_ALSPAC$Sig <- as.character(obs.res_VegDiet_bin_ALSPAC$Sig)
obs.res_VegDiet_bin_ALSPAC$Sig[is.na(obs.res_VegDiet_bin_ALSPAC$pval)] <- "NA"

### Arrange row order
obs.res_VegDiet_bin_ALSPAC$Covariate  <- factor(obs.res_VegDiet_bin_ALSPAC$Covariate, levels = rev(unique(obs.res_VegDiet_bin_ALSPAC$Covariate)))

### Create heatmap
obs.heatmap_VegDiet_bin_ALSPAC <- ggplot(obs.res_VegDiet_bin_ALSPAC,
                                         aes(x = Vegetarianism, y = Covariate)) +
  geom_tile(aes(fill = b), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = ln_limit,
    breaks = ln_breaks,
    labels = OR_labels,
    name = "Odds ratio",
    na.value = "grey"
  ) +
  geom_text(
    aes(label = Sig),
    color = "black",
    size = ifelse(obs.res_VegDiet_bin_ALSPAC$Sig == "NA", 3, 4),
    fontface = ifelse(obs.res_VegDiet_bin_ALSPAC$Sig == "NA", "plain", "bold"),
    vjust = 0.5
  ) +
  ggforce::facet_col(
    facets = ~ Group,
    scales = "free_y",
    space = "free",
    strip.position = "top"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    legend.position = "none"
  ) +
  labs(x = "", y = "")

obs.heatmap_VegDiet_bin_ALSPAC

### Save heatmap
ggsave(
  "ALSPAC/IMP_cov_obs.heatmap_VegDiet_bin.png",
  obs.heatmap_VegDiet_bin_ALSPAC,
  width = 5,
  height = 8
)

################################################################################
################################################################################

## BiB

################################################################################
### Rename covariates
obs.res_VegDiet_bin_BiB$Covariate <- ifelse(
  obs.res_VegDiet_bin_BiB$Covariate == "Education\n(higher vs. lower)",
  "Educational attainment\n(higher vs. lower)",
  obs.res_VegDiet_bin_BiB$Covariate
)

obs.res_VegDiet_bin_BiB$Covariate <- ifelse(
  obs.res_VegDiet_bin_BiB$Covariate == "IMD\n(more vs. less affluent)",
  "Index of multiple deprivation\n(more vs. less affluent)",
  obs.res_VegDiet_bin_BiB$Covariate
)

obs.res_VegDiet_bin_BiB$Covariate <- ifelse(
  obs.res_VegDiet_bin_BiB$Covariate == "Pre-pregnancy BMI\n(in kg/m^2)",
  "Body mass index\n(in kg/m^2)",
  obs.res_VegDiet_bin_BiB$Covariate
)

obs.res_VegDiet_bin_BiB$Covariate <- ifelse(
  obs.res_VegDiet_bin_BiB$Covariate == "Overall PDI",
  "Overall\nplant-based diet index",
  obs.res_VegDiet_bin_BiB$Covariate
)

obs.res_VegDiet_bin_BiB$Covariate <- ifelse(
  obs.res_VegDiet_bin_BiB$Covariate == "Healthful PDI",
  "Healthful\nplant-based diet index",
  obs.res_VegDiet_bin_BiB$Covariate
)

obs.res_VegDiet_bin_BiB$Covariate <- ifelse(
  obs.res_VegDiet_bin_BiB$Covariate == "Unhealthful PDI",
  "Unhealthful\nplant-based diet index",
  obs.res_VegDiet_bin_BiB$Covariate
)
################################################################################

### Create a new column for significance levels, including NAs
obs.res_VegDiet_bin_BiB$Sig <- NA
obs.res_VegDiet_bin_BiB$Sig[!is.na(obs.res_VegDiet_bin_BiB$pval)] <- as.character(cut(
  obs.res_VegDiet_bin_BiB$pval[!is.na(obs.res_VegDiet_bin_BiB$pval)],
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
))
obs.res_VegDiet_bin_BiB$Sig <- as.character(obs.res_VegDiet_bin_BiB$Sig)
obs.res_VegDiet_bin_BiB$Sig[is.na(obs.res_VegDiet_bin_BiB$pval)] <- "NA"

### Arrange row order
obs.res_VegDiet_bin_BiB$Covariate  <- factor(obs.res_VegDiet_bin_BiB$Covariate, levels = rev(unique(obs.res_VegDiet_bin_BiB$Covariate)))

### Create heatmap
obs.heatmap_VegDiet_bin_BiB <- ggplot(obs.res_VegDiet_bin_BiB, aes(x = Vegetarianism, y = Covariate)) +
  geom_tile(aes(fill = b), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = ln_limit,
    breaks = ln_breaks,
    labels = OR_labels,
    name = "Odds ratio",
    na.value = "grey"
  ) +
  geom_text(
    aes(label = Sig),
    color = "black",
    size = ifelse(obs.res_VegDiet_bin_BiB$Sig == "NA", 3, 4),
    fontface = ifelse(obs.res_VegDiet_bin_BiB$Sig == "NA", "plain", "bold"),
    vjust = 0.5
  ) +
  ggforce::facet_col(
    facets = ~ Group,
    scales = "free_y",
    space = "free",
    strip.position = "top"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    legend.position = "bottom"
  ) +
  labs(x = "", y = "")

obs.heatmap_VegDiet_bin_BiB

# Save heatmap
ggsave(
  "BiB/IMP_cov_obs.heatmap_VegDiet_bin.png",
  obs.heatmap_VegDiet_bin_BiB,
  width = 5,
  height = 8
)

################################################################################
################################################################################

## MoBa

################################################################################
### Rename covariates
obs.res_VegDiet_bin_MoBa$Covariate <- ifelse(
  obs.res_VegDiet_bin_MoBa$Covariate == "Education\n(higher vs. lower)",
  "Educational attainment\n(higher vs. lower)",
  obs.res_VegDiet_bin_MoBa$Covariate
)

obs.res_VegDiet_bin_MoBa$Covariate <- ifelse(
  obs.res_VegDiet_bin_MoBa$Covariate == "IMD\n(more vs. less affluent)",
  "Index of multiple deprivation\n(more vs. less affluent)",
  obs.res_VegDiet_bin_MoBa$Covariate
)

obs.res_VegDiet_bin_MoBa$Covariate <- ifelse(
  obs.res_VegDiet_bin_MoBa$Covariate == "Pre-pregnancy BMI\n(in kg/m^2)",
  "Body mass index\n(in kg/m^2)",
  obs.res_VegDiet_bin_MoBa$Covariate
)

obs.res_VegDiet_bin_MoBa$Covariate <- ifelse(
  obs.res_VegDiet_bin_MoBa$Covariate == "Overall PDI",
  "Overall\nplant-based diet index",
  obs.res_VegDiet_bin_MoBa$Covariate
)

obs.res_VegDiet_bin_MoBa$Covariate <- ifelse(
  obs.res_VegDiet_bin_MoBa$Covariate == "Healthful PDI",
  "Healthful\nplant-based diet index",
  obs.res_VegDiet_bin_MoBa$Covariate
)

obs.res_VegDiet_bin_MoBa$Covariate <- ifelse(
  obs.res_VegDiet_bin_MoBa$Covariate == "Unhealthful PDI",
  "Unhealthful\nplant-based diet index",
  obs.res_VegDiet_bin_MoBa$Covariate
)
################################################################################

### Create a new column for significance levels
obs.res_VegDiet_bin_MoBa$Sig <- cut(
  obs.res_VegDiet_bin_MoBa$pval,
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
)
obs.res_VegDiet_bin_MoBa$Sig <- as.character(obs.res_VegDiet_bin_MoBa$Sig)
obs.res_VegDiet_bin_MoBa$Sig[is.na(obs.res_VegDiet_bin_MoBa$pval)] <- "NA"

### Arrange row order
obs.res_VegDiet_bin_MoBa$Covariate  <- factor(obs.res_VegDiet_bin_MoBa$Covariate, levels = rev(unique(obs.res_VegDiet_bin_MoBa$Covariate)))

### Create heatmap
obs.heatmap_VegDiet_bin_MoBa <- ggplot(obs.res_VegDiet_bin_MoBa, aes(x = Vegetarianism, y = Covariate)) +
  geom_tile(aes(fill = b), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = ln_limit,
    breaks = ln_breaks,
    labels = OR_labels,
    name = "Odds ratio",
    na.value = "grey"
  ) +
  geom_text(
    aes(label = Sig),
    color = "black",
    size = ifelse(obs.res_VegDiet_bin_MoBa$Sig == "NA", 3, 4),
    fontface = ifelse(obs.res_VegDiet_bin_MoBa$Sig == "NA", "plain", "bold"),
    vjust = 0.5
  ) +
  ggforce::facet_col(
    facets = ~ Group,
    scales = "free_y",
    space = "free",
    strip.position = "top"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    legend.position = "none"
  ) +
  labs(x = "", y = "")

obs.heatmap_VegDiet_bin_MoBa

### Save heatmap
ggsave(
  "MoBa/IMP_cov_obs.heatmap_VegDiet_bin.png",
  obs.heatmap_VegDiet_bin_MoBa,
  width = 5,
  height = 8
)

################################################################################
################################################################################

## Project Viva

################################################################################
### Rename covariates
obs.res_VegDiet_bin_Viva$Covariate <- ifelse(
  obs.res_VegDiet_bin_Viva$Covariate == "Education\n(higher vs. lower)",
  "Educational attainment\n(higher vs. lower)",
  obs.res_VegDiet_bin_Viva$Covariate
)

obs.res_VegDiet_bin_Viva$Covariate <- ifelse(
  obs.res_VegDiet_bin_Viva$Covariate == "IMD\n(more vs. less affluent)",
  "Index of multiple deprivation\n(more vs. less affluent)",
  obs.res_VegDiet_bin_Viva$Covariate
)

obs.res_VegDiet_bin_Viva$Covariate <- ifelse(
  obs.res_VegDiet_bin_Viva$Covariate == "Pre-pregnancy BMI\n(in kg/m^2)",
  "Body mass index\n(in kg/m^2)",
  obs.res_VegDiet_bin_Viva$Covariate
)

obs.res_VegDiet_bin_Viva$Covariate <- ifelse(
  obs.res_VegDiet_bin_Viva$Covariate == "Overall PDI",
  "Overall\nplant-based diet index",
  obs.res_VegDiet_bin_Viva$Covariate
)

obs.res_VegDiet_bin_Viva$Covariate <- ifelse(
  obs.res_VegDiet_bin_Viva$Covariate == "Healthful PDI",
  "Healthful\nplant-based diet index",
  obs.res_VegDiet_bin_Viva$Covariate
)

obs.res_VegDiet_bin_Viva$Covariate <- ifelse(
  obs.res_VegDiet_bin_Viva$Covariate == "Unhealthful PDI",
  "Unhealthful\nplant-based diet index",
  obs.res_VegDiet_bin_Viva$Covariate
)
################################################################################

### Create a new column for significance levels, including NAs
obs.res_VegDiet_bin_Viva$Sig <- NA
obs.res_VegDiet_bin_Viva$Sig[!is.na(obs.res_VegDiet_bin_Viva$pval)] <- as.character(cut(
  obs.res_VegDiet_bin_Viva$pval[!is.na(obs.res_VegDiet_bin_Viva$pval)],
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
))
obs.res_VegDiet_bin_Viva$Sig <- as.character(obs.res_VegDiet_bin_Viva$Sig)
obs.res_VegDiet_bin_Viva$Sig[is.na(obs.res_VegDiet_bin_Viva$pval)] <- "NA"

### Arrange row order
obs.res_VegDiet_bin_Viva$Covariate  <- factor(obs.res_VegDiet_bin_Viva$Covariate, levels = rev(unique(obs.res_VegDiet_bin_Viva$Covariate)))

################################################################################
### !!! Truncate outlier !!!
# obs.res_VegDiet_bin_Viva$Sig[obs.res_VegDiet_bin_Viva$b == min(obs.res_VegDiet_bin$b, na.rm = T)] <- "†"
obs.res_VegDiet_bin_Viva$b[obs.res_VegDiet_bin_Viva$b == min(obs.res_VegDiet_bin$b, na.rm = T)] <- log(0.1)
################################################################################

### Create heatmap
obs.heatmap_VegDiet_bin_Viva <- ggplot(obs.res_VegDiet_bin_Viva, aes(x = Vegetarianism, y = Covariate)) +
  geom_tile(aes(fill = b), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = ln_limit,
    breaks = ln_breaks,
    labels = OR_labels,
    name = "Odds ratio",
    na.value = "grey"
  ) +
  geom_text(
    aes(label = Sig),
    color = "black",
    size = ifelse(obs.res_VegDiet_bin_Viva$Sig == "NA", 3, 4),
    fontface = ifelse(obs.res_VegDiet_bin_Viva$Sig == "NA", "plain", "bold"),
    vjust = 0.5
  ) +
  ggforce::facet_col(
    facets = ~ Group,
    scales = "free_y",
    space = "free",
    strip.position = "top"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    legend.position = "none"
  ) +
  labs(x = "", y = "")

obs.heatmap_VegDiet_bin_Viva

# Save heatmap
ggsave(
  "Viva/IMP_cov_obs.heatmap_VegDiet_bin.png",
  obs.heatmap_VegDiet_bin_Viva,
  width = 5,
  height = 8
)

################################################################################
################################################################################

# Combine
obs.heatmap_VegDiet_bin_ALSPAC <- obs.heatmap_VegDiet_bin_ALSPAC +
  labs(title = "ALSPAC (N = 11693)") +
  theme(plot.title = element_text(size = 10))

obs.heatmap_VegDiet_bin_BiB <- obs.heatmap_VegDiet_bin_BiB +
  labs(title = "BiB (N = 3647)") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    strip.text = element_text(hjust = 99),
    # Move facet labels outside of the plot (for combination)
  )

obs.heatmap_VegDiet_bin_MoBa <- obs.heatmap_VegDiet_bin_MoBa +
  labs(title = "MoBa (N = 73868)") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    strip.text = element_text(hjust = 99),
    # Move facet labels outside of the plot (for combination)
  )

obs.heatmap_VegDiet_bin_Viva <- obs.heatmap_VegDiet_bin_Viva +
  labs(title = "Project Viva (N = 1872)") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    strip.text = element_text(hjust = 99),
    # Move facet labels outside of the plot (for combination)
  )

obs.heatmap_VegDiet_bin <- obs.heatmap_VegDiet_bin_ALSPAC +
  obs.heatmap_VegDiet_bin_BiB +
  obs.heatmap_VegDiet_bin_MoBa +
  obs.heatmap_VegDiet_bin_Viva +
  plot_layout(ncol = 4, widths = c(1, 1, 1, 1)) &
  theme(plot.margin = margin(5, 0, 0, 0))

obs.heatmap_VegDiet_bin

ggsave(
  "ALL/Comb_IMP_cov_obs.heatmap_VegDiet_bin.png",
  obs.heatmap_VegDiet_bin,
  width = 13,
  height = 8
)
