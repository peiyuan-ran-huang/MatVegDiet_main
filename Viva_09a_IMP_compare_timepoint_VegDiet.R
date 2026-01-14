################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 07-May-2025
# This script is to compare results across different timepoints (with imputed data) for vegetarian diets in Project Viva.

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
               grid,
               forestplot)

# Set working directory
setwd("Z:/working/results/")

#------------------------------------------------------------------------------#
#                               Data Preparation                               #----
#------------------------------------------------------------------------------#

# Load and combine analysis results

## Main results (from all studies)
meta_bin <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_bin.xlsx")
meta_bin <- meta_bin %>% select(Outcome, Exposure, b, se, pval)
meta_bin$Timepoint <- "Main results (from all studies)"
meta_bin

meta_con <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_con.xlsx")
meta_con <- meta_con %>% select(Outcome, Exposure, b, se, pval)
meta_con$Timepoint <- "Main results (from all studies)"
meta_con

meta_ord <- read.xlsx("ALL/IMP_BF_meta.tbl_VegDiet_ord.xlsx")
meta_ord <- meta_ord %>% select(Outcome, Exposure, b, se, pval)
meta_ord$Timepoint <- "Main results (from all studies)"
meta_ord

## Average during pregnancy (Project Viva)
Viva_bin <- read.xlsx("Viva/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
Viva_bin <- Viva_bin %>% subset(Model == "Model 3") %>% select(Outcome, Exposure, b, se, pval)
Viva_bin$Timepoint <- "Average during pregnancy (Project Viva)"
Viva_bin

Viva_con <- read.xlsx("Viva/IMP_MAIN_obs.res_VegDiet_con.xlsx")
Viva_con <- Viva_con %>% subset(Model == "Model 3") %>% select(Outcome, Exposure, b, se, pval)
Viva_con$Timepoint <- "Average during pregnancy (Project Viva)"
Viva_con

Viva_ord <- read.xlsx("Viva/IMP_BF_obs.res_VegDiet_ord.xlsx")
Viva_ord <- Viva_ord %>% subset(Model == "Model 3") %>% select(Outcome, Exposure, b, se, pval)
Viva_ord$Timepoint <- "Average during pregnancy (Project Viva)"
Viva_ord

## In early pregnancy (Project Viva)
EAR.p_bin <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_VegDiet_bin.xlsx")
EAR.p_bin <- EAR.p_bin %>% subset(Model == "Model 3") %>% select(Outcome, Exposure, b, se, pval)
EAR.p_bin$Timepoint <- "In early pregnancy (Project Viva)"
EAR.p_bin

EAR.p_con <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_VegDiet_con.xlsx")
EAR.p_con <- EAR.p_con %>% subset(Model == "Model 3") %>% select(Outcome, Exposure, b, se, pval)
EAR.p_con$Timepoint <- "In early pregnancy (Project Viva)"
EAR.p_con

EAR.p_ord <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_VegDiet_ord.xlsx")
EAR.p_ord <- EAR.p_ord %>% subset(Model == "Model 3") %>% select(Outcome, Exposure, b, se, pval)
EAR.p_ord$Timepoint <- "In early pregnancy (Project Viva)"
EAR.p_ord

## In mid-pregnancy (Project Viva)
MID.p_bin <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_VegDiet_bin.xlsx")
MID.p_bin <- MID.p_bin %>% subset(Model == "Model 3") %>% select(Outcome, Exposure, b, se, pval)
MID.p_bin$Timepoint <- "In mid-pregnancy (Project Viva)"
MID.p_bin

MID.p_con <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_VegDiet_con.xlsx")
MID.p_con <- MID.p_con %>% subset(Model == "Model 3") %>% select(Outcome, Exposure, b, se, pval)
MID.p_con$Timepoint <- "In mid-pregnancy (Project Viva)"
MID.p_con

MID.p_ord <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_VegDiet_ord.xlsx")
MID.p_ord <- MID.p_ord %>% subset(Model == "Model 3") %>% select(Outcome, Exposure, b, se, pval)
MID.p_ord$Timepoint <- "In mid-pregnancy (Project Viva)"
MID.p_ord

################################################################################

## Combine
timepoint_bin_con_ord <- rbind(
  meta_bin,
  meta_con,
  meta_ord,
  Viva_bin,
  Viva_con,
  Viva_ord,
  EAR.p_bin,
  EAR.p_con,
  EAR.p_ord,
  MID.p_bin,
  MID.p_con,
  MID.p_ord
)

################################################################################

## Outcome grouping - !!! May be a little different from cohort-specific analyses !!!

### Load outcome lists and labels
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

### Identify available outcomes in the combined results
ALL_primary_bin <- subset(primary_bin, label %in% meta_bin$Outcome)
ALL_primary_bin
dim(ALL_primary_bin)  # 13 primary (binary) outcomes available in the combined results

ALL_secondary_bin <- subset(secondary_bin,
                            label %in% meta_bin$Outcome &
                              varname != "anaemia_preg_subsamp")  # Maternal anaemia (occurring during pregnancy) is only available for sensitivity analysis in MoBa
ALL_secondary_bin
dim(ALL_secondary_bin)  # 8 secondary binary outcomes available in the combined results

ALL_secondary_con <- subset(secondary_con, label %in% meta_con$Outcome)
ALL_secondary_con
dim(ALL_secondary_con)  # 4 secondary continuous outcomes available in the combined results

#------------------------------------------------------------------------------#
#                                 Forest Plots                                 #----
#------------------------------------------------------------------------------#

# Prepare data
timepoint_bin_con_ord$Group <- NA
timepoint_bin_con_ord$Group[timepoint_bin_con_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
timepoint_bin_con_ord$Group[timepoint_bin_con_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
timepoint_bin_con_ord$Group[timepoint_bin_con_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
timepoint_bin_con_ord$Group <-
  factor(
    timepoint_bin_con_ord$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

timepoint_bin_con_ord$Timepoint <-
  factor(
    timepoint_bin_con_ord$Timepoint,
    levels = c(
      "In mid-pregnancy (Project Viva)",
      "In early pregnancy (Project Viva)",
      "Average during pregnancy (Project Viva)",
      "Main results (from all studies)"
    )
  )

timepoint_bin_con_ord$Outcome <-
  factor(timepoint_bin_con_ord$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% timepoint_bin_con_ord$Outcome])
timepoint_bin_con_ord <-
  timepoint_bin_con_ord %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

timepoint_bin_con_ord$b <-
  as.numeric(timepoint_bin_con_ord$b)
timepoint_bin_con_ord$se <-
  as.numeric(timepoint_bin_con_ord$se)
timepoint_bin_con_ord$pval <-
  as.numeric(timepoint_bin_con_ord$pval)

timepoint_bin_con_ord
dim(timepoint_bin_con_ord)  # 86 obs.

################################################################################
## Remove some results with extremely huge 95% CIs
timepoint_bin_con_ord <-
  timepoint_bin_con_ord[which(timepoint_bin_con_ord$se < 90), ]
################################################################################
## Exclude outcomes only available in meta-analysis
timepoint_bin_con_ord <- timepoint_bin_con_ord %>%
  group_by(Outcome) %>%
  filter(n() >= 2) %>%
  ungroup()
################################################################################

timepoint_bin_con_ord
dim(timepoint_bin_con_ord)  # 86 -> 63 obs

################################################################################

# Nightingale forest plots
obs.forest_VegDiet_bin_con_ord <- ggforestplot::forestplot(
  df = timepoint_bin_con_ord,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Timepoint,
  shape = Timepoint,
  xlab = "LogOR or beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkgreen", "darkorange", "navy", "darkred")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21))

obs.forest_VegDiet_bin_con_ord

ggsave(
  obs.forest_VegDiet_bin_con_ord,
  file = "Viva/compare.timepoint.Viva_obs.forest_VegDiet.png",
  height = 12,
  width = 8
)
