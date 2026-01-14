################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 16-Jun-2025
# This script is to perform meta-analysis on paternal negative control analysis (with imputed data) results for vegetarian diets.
## Part 4: Paternal negative control results for primary outcomes in the main text

### Update on 07-May-2025: Add main meta-analysis results from all cohorts.
### Update on 16-Jun-2025: Add post-term birth and breastfeeding duration as primary outcomes.

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
               meta,
               metafor,
               gridExtra,
               cowplot,
               grid)

# Set working directory
setwd("Z:/working/results/")

#------------------------------------------------------------------------------#
#                               Data Preparation                               #----
#------------------------------------------------------------------------------#

# Load and combine observational results
metagen_obs.tbl_VegDiet_bin_MAIN <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_bin.xlsx")[, c("Outcome", "b", "se", "pval")]
metagen_obs.tbl_VegDiet_bin_Mat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Mat1.xlsx")[, c("Outcome", "b", "se", "pval")]
metagen_obs.tbl_VegDiet_bin_Mat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Mat2.xlsx")[, c("Outcome", "b", "se", "pval")]
metagen_obs.tbl_VegDiet_bin_Pat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Pat1.xlsx")[, c("Outcome", "b", "se", "pval")]
metagen_obs.tbl_VegDiet_bin_Pat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Pat2.xlsx")[, c("Outcome", "b", "se", "pval")]

metagen_obs.tbl_VegDiet_bin_MAIN <- rbind(
  metagen_obs.tbl_VegDiet_bin_MAIN,
  read.xlsx("ALL/IMP_BF_meta.tbl_VegDiet_ord.xlsx")[, c("Outcome", "b", "se", "pval")]
)
metagen_obs.tbl_VegDiet_bin_Mat1 <- rbind(
  metagen_obs.tbl_VegDiet_bin_Mat1,
  read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Mat1.xlsx")[, c("Outcome", "b", "se", "pval")]
)
metagen_obs.tbl_VegDiet_bin_Mat2 <- rbind(
  metagen_obs.tbl_VegDiet_bin_Mat2,
  read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Mat2.xlsx")[, c("Outcome", "b", "se", "pval")]
)
metagen_obs.tbl_VegDiet_bin_Pat1 <- rbind(
  metagen_obs.tbl_VegDiet_bin_Pat1,
  read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Pat1.xlsx")[, c("Outcome", "b", "se", "pval")]
)
metagen_obs.tbl_VegDiet_bin_Pat2 <- rbind(
  metagen_obs.tbl_VegDiet_bin_Pat2,
  read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Pat2.xlsx")[, c("Outcome", "b", "se", "pval")]
)

# Combine
metagen_obs.tbl_VegDiet_bin_MAIN$Analysis <- "Conventional multivariable regression (all studies)"
metagen_obs.tbl_VegDiet_bin_Mat1$Analysis <- "Maternal Model 1 (ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_bin_Mat2$Analysis <- "Maternal Model 2 (ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_bin_Pat1$Analysis <- "Paternal Model 1 (ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_bin_Pat2$Analysis <- "Paternal Model 2 (ALSPAC & MoBa)"

metagen_obs.tbl_VegDiet_bin <- rbind(
  metagen_obs.tbl_VegDiet_bin_MAIN,
  metagen_obs.tbl_VegDiet_bin_Mat1,
  metagen_obs.tbl_VegDiet_bin_Mat2,
  metagen_obs.tbl_VegDiet_bin_Pat1,
  metagen_obs.tbl_VegDiet_bin_Pat2
)

metagen_obs.tbl_VegDiet_bin

################################################################################

# Outcome grouping - !!! May be a little different from cohort-specific analyses !!!
MRPREG_outcome_labels <-
  read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

primary_bin <- subset(
  MRPREG_outcome_labels,
  varname %in% read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Primary_bin")$varname
)
primary_bin
dim(primary_bin)  # 13 primary (binary) outcomes

secondary_bin <- subset(
  MRPREG_outcome_labels,
  varname %in% read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_bin")$varname
)
secondary_bin
dim(secondary_bin)  # 8 secondary binary outcomes

secondary_con <- subset(
  MRPREG_outcome_labels,
  varname %in% read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_con")$varname
)
secondary_con
dim(secondary_con)  # 4 secondary continuous outcomes

secondary_ord <- subset(
  MRPREG_outcome_labels,
  varname %in% read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_cat")$varname
)
secondary_ord  # 1 (secondary) ordinal/categorical outcome (bf_dur_4c as negative outcome)

#------------------------------------------------------------------------------#
#                                 Forest Plots                                 #----
#------------------------------------------------------------------------------#

# Prepare data

## Binary outcomes
metagen_obs.tbl_VegDiet_bin$Group <- NA
metagen_obs.tbl_VegDiet_bin$Group[metagen_obs.tbl_VegDiet_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
metagen_obs.tbl_VegDiet_bin$Group[metagen_obs.tbl_VegDiet_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
metagen_obs.tbl_VegDiet_bin$Group[metagen_obs.tbl_VegDiet_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
metagen_obs.tbl_VegDiet_bin$Group <-
  factor(
    metagen_obs.tbl_VegDiet_bin$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

metagen_obs.tbl_VegDiet_bin$Analysis <-
  factor(
    metagen_obs.tbl_VegDiet_bin$Analysis,
    levels = c(
      "Paternal Model 2 (ALSPAC & MoBa)",
      "Paternal Model 1 (ALSPAC & MoBa)",
      "Maternal Model 2 (ALSPAC & MoBa)",
      "Maternal Model 1 (ALSPAC & MoBa)",
      "Conventional multivariable regression (all studies)"
    )
  )

metagen_obs.tbl_VegDiet_bin$Outcome <-
  factor(metagen_obs.tbl_VegDiet_bin$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% metagen_obs.tbl_VegDiet_bin$Outcome])
metagen_obs.tbl_VegDiet_bin <-
  metagen_obs.tbl_VegDiet_bin %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

metagen_obs.tbl_VegDiet_bin$b <-
  as.numeric(metagen_obs.tbl_VegDiet_bin$b)
metagen_obs.tbl_VegDiet_bin$se <-
  as.numeric(metagen_obs.tbl_VegDiet_bin$se)
metagen_obs.tbl_VegDiet_bin$pval <-
  as.numeric(metagen_obs.tbl_VegDiet_bin$pval)

metagen_obs.tbl_VegDiet_bin
dim(metagen_obs.tbl_VegDiet_bin)  # (18 outcomes * 1 exposure * 4 models) + 21 (from main meta-analysis) = 93 obs.

################################################################################
## Exclude outcomes with <7 models
metagen_obs.tbl_VegDiet_bin <- metagen_obs.tbl_VegDiet_bin %>%
  group_by(Outcome) %>%
  filter(n() >= 5) %>%
  ungroup()
################################################################################

metagen_obs.tbl_VegDiet_bin
dim(metagen_obs.tbl_VegDiet_bin)  # 93 -> 90 obs.

################################################################################

# Nightingale forest plots

## Primary (binary) outcomes
obs.forest_VegDiet_bin <- ggforestplot::forestplot(
  df = subset(
    metagen_obs.tbl_VegDiet_bin,
    Outcome %in% c(primary_bin$label, "Post-term birth", "Breastfeeding duration")
  ),
  # Only show primary outcomes
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Analysis,
  shape = Analysis,
  xlab = "OR and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "Primary outcomes",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkblue", "blue", "darkred", "red", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21))

obs.forest_VegDiet_bin

ggsave(
  obs.forest_VegDiet_bin,
  file = "ALL/IMP_PNC_meta.forest_VegDiet_Prim.Out.png",
  height = 12,
  width = 10
)

################################################################################
