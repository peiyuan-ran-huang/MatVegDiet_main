################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 20-Jan-2025
# This script is to perform meta-analysis on paternal negative control analysis (with imputed data) results for vegetarian diets.
## Part 7: Comparison of paternal negative control results for ALL outcomes (for interpretation)

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
metagen_obs.tbl_VegDiet_bin_MAIN <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_bin.xlsx")
metagen_obs.tbl_VegDiet_bin_ALSPAC.MoBa <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_bin_ALSPAC.MoBa.xlsx")
metagen_obs.tbl_VegDiet_bin_Mat1.FULL <- read.xlsx("ALL/IMP_PNC.FULL_meta.tbl_VegDiet_bin_Mat1.xlsx")
metagen_obs.tbl_VegDiet_bin_Mat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Mat1.xlsx")
metagen_obs.tbl_VegDiet_bin_Mat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Mat2.xlsx")
metagen_obs.tbl_VegDiet_bin_Pat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Pat1.xlsx")
metagen_obs.tbl_VegDiet_bin_Pat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Pat2.xlsx")

metagen_obs.tbl_VegDiet_con_MAIN <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_con.xlsx")
metagen_obs.tbl_VegDiet_con_ALSPAC.MoBa <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_con_ALSPAC.MoBa.xlsx")
metagen_obs.tbl_VegDiet_con_Mat1.FULL <- read.xlsx("ALL/IMP_PNC.FULL_meta.tbl_VegDiet_con_Mat1.xlsx")
metagen_obs.tbl_VegDiet_con_Mat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_con_Mat1.xlsx")
metagen_obs.tbl_VegDiet_con_Mat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_con_Mat2.xlsx")
metagen_obs.tbl_VegDiet_con_Pat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_con_Pat1.xlsx")
metagen_obs.tbl_VegDiet_con_Pat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_con_Pat2.xlsx")

metagen_obs.tbl_VegDiet_ord_MAIN <- read.xlsx("ALL/IMP_BF_meta.tbl_VegDiet_ord.xlsx")
metagen_obs.tbl_VegDiet_ord_ALSPAC.MoBa <- read.xlsx("ALL/IMP_BF_meta.tbl_VegDiet_ord_ALSPAC.MoBa.xlsx")
metagen_obs.tbl_VegDiet_ord_Mat1.FULL <- read.xlsx("ALL/IMP_PNC.FULL_meta.tbl_VegDiet_ord_Mat1.xlsx")
metagen_obs.tbl_VegDiet_ord_Mat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Mat1.xlsx")
metagen_obs.tbl_VegDiet_ord_Mat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Mat2.xlsx")
metagen_obs.tbl_VegDiet_ord_Pat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Pat1.xlsx")
metagen_obs.tbl_VegDiet_ord_Pat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Pat2.xlsx")

# Combine
metagen_obs.tbl_VegDiet_bin_MAIN$Analysis <- "Conventional multivariable regression (all studies)"
metagen_obs.tbl_VegDiet_bin_ALSPAC.MoBa$Analysis <- "Conventional multivariable regression (ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_bin_Mat1.FULL$Analysis <- "Maternal Model 1 (full sample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_bin_Mat1$Analysis <- "Maternal Model 1 (subsample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_bin_Mat2$Analysis <- "Maternal Model 2 (subsample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_bin_Pat1$Analysis <- "Paternal Model 1 (subsample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_bin_Pat2$Analysis <- "Paternal Model 2 (subsample in ALSPAC & MoBa)"

metagen_obs.tbl_VegDiet_con_MAIN$Analysis <- "Conventional multivariable regression (all studies)"
metagen_obs.tbl_VegDiet_con_ALSPAC.MoBa$Analysis <- "Conventional multivariable regression (ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_con_Mat1.FULL$Analysis <- "Maternal Model 1 (full sample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_con_Mat1$Analysis <- "Maternal Model 1 (subsample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_con_Mat2$Analysis <- "Maternal Model 2 (subsample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_con_Pat1$Analysis <- "Paternal Model 1 (subsample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_con_Pat2$Analysis <- "Paternal Model 2 (subsample in ALSPAC & MoBa)"

metagen_obs.tbl_VegDiet_ord_MAIN$Analysis <- "Conventional multivariable regression (all studies)"
metagen_obs.tbl_VegDiet_ord_ALSPAC.MoBa$Analysis <- "Conventional multivariable regression (ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_ord_Mat1.FULL$Analysis <- "Maternal Model 1 (full sample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_ord_Mat1$Analysis <- "Maternal Model 1 (subsample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_ord_Mat2$Analysis <- "Maternal Model 2 (subsample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_ord_Pat1$Analysis <- "Paternal Model 1 (subsample in ALSPAC & MoBa)"
metagen_obs.tbl_VegDiet_ord_Pat2$Analysis <- "Paternal Model 2 (subsample in ALSPAC & MoBa)"

metagen_obs.tbl_VegDiet_bin_con_ord <- rbind(
  metagen_obs.tbl_VegDiet_bin_MAIN[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_bin_ALSPAC.MoBa[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_bin_Mat1.FULL[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_bin_Mat1[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_bin_Mat2[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_bin_Pat1[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_bin_Pat2[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_con_MAIN[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_con_ALSPAC.MoBa[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_con_Mat1.FULL[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_con_Mat1[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_con_Mat2[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_con_Pat1[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_con_Pat2[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_ord_MAIN[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_ord_ALSPAC.MoBa[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_ord_Mat1.FULL[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_ord_Mat1[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_ord_Mat2[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_ord_Pat1[, c("Outcome", "Analysis", "b", "se", "pval")],
  metagen_obs.tbl_VegDiet_ord_Pat2[, c("Outcome", "Analysis", "b", "se", "pval")]
)

metagen_obs.tbl_VegDiet_bin_con_ord

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
dim(secondary_bin)  # 9 secondary binary outcomes

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
metagen_obs.tbl_VegDiet_bin_con_ord$Group <- NA
metagen_obs.tbl_VegDiet_bin_con_ord$Group[metagen_obs.tbl_VegDiet_bin_con_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
metagen_obs.tbl_VegDiet_bin_con_ord$Group[metagen_obs.tbl_VegDiet_bin_con_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
metagen_obs.tbl_VegDiet_bin_con_ord$Group[metagen_obs.tbl_VegDiet_bin_con_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
metagen_obs.tbl_VegDiet_bin_con_ord$Group <-
  factor(
    metagen_obs.tbl_VegDiet_bin_con_ord$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

metagen_obs.tbl_VegDiet_bin_con_ord$Analysis <-
  factor(
    metagen_obs.tbl_VegDiet_bin_con_ord$Analysis,
    levels = c(
      "Paternal Model 2 (subsample in ALSPAC & MoBa)",
      "Paternal Model 1 (subsample in ALSPAC & MoBa)",
      "Maternal Model 2 (subsample in ALSPAC & MoBa)",
      "Maternal Model 1 (subsample in ALSPAC & MoBa)",
      "Maternal Model 1 (full sample in ALSPAC & MoBa)",
      "Conventional multivariable regression (ALSPAC & MoBa)",
      "Conventional multivariable regression (all studies)"
    )
  )

metagen_obs.tbl_VegDiet_bin_con_ord$Outcome <-
  factor(
    metagen_obs.tbl_VegDiet_bin_con_ord$Outcome,
    levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% metagen_obs.tbl_VegDiet_bin_con_ord$Outcome]
  )
metagen_obs.tbl_VegDiet_bin_con_ord <-
  metagen_obs.tbl_VegDiet_bin_con_ord %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

metagen_obs.tbl_VegDiet_bin_con_ord$b <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_con_ord$b)
metagen_obs.tbl_VegDiet_bin_con_ord$se <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_con_ord$se)
metagen_obs.tbl_VegDiet_bin_con_ord$pval <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_con_ord$pval)

metagen_obs.tbl_VegDiet_bin_con_ord
dim(metagen_obs.tbl_VegDiet_bin_con_ord)  # 170 obs.

################################################################################
## Exclude outcomes with <7 models
metagen_obs.tbl_VegDiet_bin_con_ord <- metagen_obs.tbl_VegDiet_bin_con_ord %>%
  group_by(Outcome) %>%
  filter(n() >= 7) %>%
  ungroup()
################################################################################

metagen_obs.tbl_VegDiet_bin_con_ord
dim(metagen_obs.tbl_VegDiet_bin_con_ord)  # 161 obs. = 23 outcomes (18 binary + 4 continuous + 1 ordinal) * 1 exposure * 7 models

################################################################################

# Nightingale forest plots
obs.forest_VegDiet_bin_con_ord <- ggforestplot::forestplot(
  df = metagen_obs.tbl_VegDiet_bin_con_ord,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Analysis,
  shape = Analysis,
  xlab = "LogOR or beta and 95% CI (pesco-/full vegetarian vs. non-vegetarian)",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c(
    "darkblue",
    "blue",
    "darkred",
    "red",
    "orange",
    "purple4",
    "darkcyan"
  )) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21))

obs.forest_VegDiet_bin_con_ord

ggsave(
  obs.forest_VegDiet_bin_con_ord,
  file = "ALL/IMP_PNC_meta.forest_VegDiet_bin_con_ord.png",
  height = 22,
  width = 14
)

################################################################################
