################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 14-Dec-2024
# This script is to combine cohort-specific results of stratification (by iron supplementation) analysis (with imputed data) for vegetarian diets.

#------------------------------------------------------------------------------#
#                                 Housekeeping                                 #----
#------------------------------------------------------------------------------#

# Clear environment
rm(list = ls())

# Collect information about the current R session
sessionInfo()

# Load packages
pacman::p_load(tidyverse, openxlsx, gtsummary, ggplot2)

# Set working directory
setwd("Z:/working/results/")

#------------------------------------------------------------------------------#
#                               Data Preparation                               #----
#------------------------------------------------------------------------------#

# Load and combine observational results - iron supplementation-stratified analysis

## ALSPAC
obs.res_VegDiet_bin_ALSPAC <-
  read.xlsx("ALSPAC/IMP_STRAT.iron_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_ALSPAC
str(obs.res_VegDiet_bin_ALSPAC)  # 2 results (1 outcome * 1 category * 2 strata)

## BiB
obs.res_VegDiet_bin_BiB <-
  read.xlsx("BiB/IMP_STRAT.iron_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_BiB[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_BiB[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_BiB
str(obs.res_VegDiet_bin_BiB)  # 2 results (1 outcome * 1 category * 2 strata)

## MoBa
obs.res_VegDiet_bin_MoBa <-
  read.xlsx("MoBa/IMP_STRAT.iron_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_MoBa
str(obs.res_VegDiet_bin_MoBa)  # 2 results (1 outcome * 1 category * 2 strata)

## Project Viva
obs.res_VegDiet_bin_Viva <-
  read.xlsx("Viva/IMP_STRAT.iron_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_Viva
str(obs.res_VegDiet_bin_Viva)  # 2 results (1 outcome * 1 category * 2 strata)

################################################################################

## Combine
obs.res_VegDiet_bin_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_bin_BiB$Study <- "BiB"
obs.res_VegDiet_bin_MoBa$Study <- "MoBa"
obs.res_VegDiet_bin_Viva$Study <- "Project Viva"

obs.res_VegDiet_bin <-
  rbind(
    obs.res_VegDiet_bin_ALSPAC,
    obs.res_VegDiet_bin_BiB,
    obs.res_VegDiet_bin_MoBa,
    obs.res_VegDiet_bin_Viva
  )
obs.res_VegDiet_bin
str(obs.res_VegDiet_bin)  # 8 results = 2 + 2 + 2 + 2

## Load outcome lists and labels
MRPREG_outcome_labels <-
  read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

################################################################################

## Select and prepare results
obs.res_VegDiet_bin$Group <- NA
obs.res_VegDiet_bin$Group[obs.res_VegDiet_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_VegDiet_bin$Group[obs.res_VegDiet_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_VegDiet_bin$Group[obs.res_VegDiet_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_VegDiet_bin$Group <-
  factor(
    obs.res_VegDiet_bin$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_VegDiet_bin$Outcome <-
  factor(obs.res_VegDiet_bin$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin$Outcome])
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_VegDiet_bin$b <-
  as.numeric(obs.res_VegDiet_bin$b)
obs.res_VegDiet_bin$se <-
  as.numeric(obs.res_VegDiet_bin$se)
obs.res_VegDiet_bin$pval <-
  as.numeric(obs.res_VegDiet_bin$pval)

obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 8 obs.

################################################################################
### !!! Add total sample size for each stratum !!!
obs.res_VegDiet_bin <- obs.res_VegDiet_bin %>%
  separate(
    N_exp,
    into = c("case_exp", "total_exp"),
    sep = " / ",
    remove = F
  ) %>%
  separate(
    N_ref,
    into = c("case_ref", "total_ref"),
    sep = " / ",
    remove = F
  ) %>%
  mutate(
    case_exp = as.numeric(case_exp),
    total_exp = as.numeric(total_exp),
    case_ref = as.numeric(case_ref),
    total_ref = as.numeric(total_ref)
  ) %>% mutate(total = total_exp + total_ref)

obs.res_VegDiet_bin$Supplementation[obs.res_VegDiet_bin$Supplementation == "Iron supplement use - No"] <- "Iron supplement use: No"
obs.res_VegDiet_bin$Supplementation[obs.res_VegDiet_bin$Supplementation == "Iron supplement use - Yes"] <- "Iron supplement use: Yes"
obs.res_VegDiet_bin$Supplementation <- paste0(
  obs.res_VegDiet_bin$Supplementation,
  "\n(",
  obs.res_VegDiet_bin$case_exp,
  " in ",
  obs.res_VegDiet_bin$total_exp,
  " vs. ",
  obs.res_VegDiet_bin$case_ref,
  " in ",
  obs.res_VegDiet_bin$total_ref,
  ")"
)

obs.res_VegDiet_bin
################################################################################
### !!! Remove some results with extremely huge 95% CIs !!!
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin[which(obs.res_VegDiet_bin$se < 100), ]

obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 8 -> 8 obs.
################################################################################

### Nightingale forest plots
obs.forest_VegDiet_bin <- ggforestplot::forestplot(
  df = obs.res_VegDiet_bin,
  name = Supplementation,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Exposure,
  xlab = "OR and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "Risk of maternal anaemia",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21)) +
  ggforce::facet_col(facets = ~ Study,
                     scales = "free_y",
                     space = "free") + theme(legend.position = "none")

obs.forest_VegDiet_bin

ggsave(
  obs.forest_VegDiet_bin,
  file = "ALL/Comb_IMP_STRAT.iron_obs.forest_VegDiet_bin.png",
  height = 7,
  width = 6
)
