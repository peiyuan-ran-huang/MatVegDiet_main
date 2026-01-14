################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 21-Jan-2025
# This script is to compare the results (with imputed data) for different vegetarian subgroups.

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

## Main analysis
IMP_main_meta_bin <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_bin.xlsx")
IMP_main_meta_bin <- IMP_main_meta_bin %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_bin$Exposure <- "Pesco-/full vegetarian (main)"
IMP_main_meta_bin

IMP_main_meta_con <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_con.xlsx")
IMP_main_meta_con <- IMP_main_meta_con %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_con$Exposure <- "Pesco-/full vegetarian (main)"
IMP_main_meta_con

IMP_main_meta_ord <- read.xlsx("ALL/IMP_BF_meta.tbl_VegDiet_ord.xlsx")
IMP_main_meta_ord <- IMP_main_meta_ord %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_ord$Exposure <- "Pesco-/full vegetarian (main)"
IMP_main_meta_ord

## Subgroup analysis
IMP_main_meta_bin_pesco.V <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet.subgroup_bin_pesco.V.xlsx")
IMP_main_meta_bin_pesco.V <- IMP_main_meta_bin_pesco.V %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_bin_pesco.V$Exposure <- "Pesco-vegetarian"
IMP_main_meta_bin_pesco.V

IMP_main_meta_con_pesco.V <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet.subgroup_con_pesco.V.xlsx")
IMP_main_meta_con_pesco.V <- IMP_main_meta_con_pesco.V %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_con_pesco.V$Exposure <- "Pesco-vegetarian"
IMP_main_meta_con_pesco.V

IMP_main_meta_ord_pesco.V <- read.xlsx("ALL/IMP_BF_meta.tbl_VegDiet.subgroup_ord.xlsx")
IMP_main_meta_ord_pesco.V <- subset(IMP_main_meta_ord_pesco.V,
                                    Exposure == "Pesco- vs. non-vegetarian")
IMP_main_meta_ord_pesco.V <- IMP_main_meta_ord_pesco.V %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_ord_pesco.V$Exposure <- "Pesco-vegetarian"
IMP_main_meta_ord_pesco.V

IMP_main_meta_bin_full.V <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet.subgroup_bin_full.V.xlsx")
IMP_main_meta_bin_full.V <- IMP_main_meta_bin_full.V %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_bin_full.V$Exposure <- "Full vegetarian"
IMP_main_meta_bin_full.V

IMP_main_meta_con_full.V <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet.subgroup_con_full.V.xlsx")
IMP_main_meta_con_full.V <- IMP_main_meta_con_full.V %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_con_full.V$Exposure <- "Full vegetarian"
IMP_main_meta_con_full.V

IMP_main_meta_ord_full.V <- read.xlsx("ALL/IMP_BF_meta.tbl_VegDiet.subgroup_ord.xlsx")
IMP_main_meta_ord_full.V <- subset(IMP_main_meta_ord_full.V, Exposure == "Full vs. non-vegetarian")
IMP_main_meta_ord_full.V <- IMP_main_meta_ord_full.V %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_ord_full.V$Exposure <- "Full vegetarian"
IMP_main_meta_ord_full.V

## Combine
IMP_main_meta_bin_con <- rbind(
  IMP_main_meta_bin,
  IMP_main_meta_bin_pesco.V,
  IMP_main_meta_bin_full.V,
  IMP_main_meta_con,
  IMP_main_meta_con_pesco.V,
  IMP_main_meta_con_full.V,
  IMP_main_meta_ord,
  IMP_main_meta_ord_pesco.V,
  IMP_main_meta_ord_full.V
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
ALL_primary_bin <- subset(primary_bin, label %in% IMP_main_meta_bin$Outcome)
ALL_primary_bin
dim(ALL_primary_bin)  # 13 primary (binary) outcomes available in the combined results

ALL_secondary_bin <- subset(
  secondary_bin,
  label %in% IMP_main_meta_bin$Outcome &
    varname != "anaemia_preg_subsamp"
)  # Maternal anaemia (occurring during pregnancy) is only available for sensitivity analysis in MoBa
ALL_secondary_bin
dim(ALL_secondary_bin)  # 8 secondary binary outcomes available in the combined results

ALL_secondary_con <- subset(secondary_con, label %in% IMP_main_meta_con$Outcome)
ALL_secondary_con
dim(ALL_secondary_con)  # 4 secondary continuous outcomes available in the combined results

################################################################################

# Prepare results for comparison

## Main analysis
IMP_main_meta_bin_con$Outcome <-
  factor(IMP_main_meta_bin_con$Outcome,
         levels =
           unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% IMP_main_meta_bin_con$Outcome])
IMP_main_meta_bin_con <-
  IMP_main_meta_bin_con %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

################################################################################
## Change character into numeric
IMP_main_meta_bin_con <- type.convert(IMP_main_meta_bin_con, as.is = T)
################################################################################

IMP_main_meta_bin_con$Exposure <- factor(
  IMP_main_meta_bin_con$Exposure,
  levels = c(
    "Full vegetarian",
    "Pesco-vegetarian",
    "Pesco-/full vegetarian (main)"
  )
)

#------------------------------------------------------------------------------#
#                                    Z-Test                                    #----
#------------------------------------------------------------------------------#

# Compute Z-test
Z.test_bin_con <- c()

for (my_out in unique(IMP_main_meta_bin_con$Outcome)) {
  Outcome <- my_out
  
  b1 <- IMP_main_meta_bin_con[which(
    IMP_main_meta_bin_con$Outcome == my_out &
      IMP_main_meta_bin_con$Exposure == "Pesco-vegetarian"
  ), "b"]
  se1 <- IMP_main_meta_bin_con[which(
    IMP_main_meta_bin_con$Outcome == my_out &
      IMP_main_meta_bin_con$Exposure == "Pesco-vegetarian"
  ), "se"]
  b2 <- IMP_main_meta_bin_con[which(
    IMP_main_meta_bin_con$Outcome == my_out &
      IMP_main_meta_bin_con$Exposure == "Full vegetarian"
  ), "b"]
  se2 <- IMP_main_meta_bin_con[which(
    IMP_main_meta_bin_con$Outcome == my_out &
      IMP_main_meta_bin_con$Exposure == "Full vegetarian"
  ), "se"]
  Z_value <- (b1 - b2) / sqrt(se1 ^ 2 + se2 ^ 2)
  
  p_value <- 2 * pnorm(-abs(Z_value))
  Sig <- if (p_value < 0.001)
    "***"
  else if (p_value < 0.01)
    "**"
  else if (p_value < 0.05)
    "*"
  else
    ""
  
  Z.test_bin_con <- rbind(
    Z.test_bin_con,
    c(
      Outcome = Outcome,
      `Z-value` = Z_value,
      `p-value` = p_value,
      Sig = Sig
    )
  )
}

Z.test_bin_con

# Add significance stars to the forest plot
IMP_main_meta_bin_con$No <- 1:nrow(IMP_main_meta_bin_con)

IMP_main_meta_bin_con <- merge(IMP_main_meta_bin_con,
                               Z.test_bin_con,
                               by = "Outcome",
                               all.x = T)

IMP_main_meta_bin_con$Outcome <- paste0(IMP_main_meta_bin_con$Outcome, " ", IMP_main_meta_bin_con$Sig)

IMP_main_meta_bin_con <-
  IMP_main_meta_bin_con %>% arrange(No)  # Make sure the outcomes appear in the right order

IMP_main_meta_bin_con

#------------------------------------------------------------------------------#
#                                 Forest Plots                                 #----
#------------------------------------------------------------------------------#

# Main analysis
IMP_main_forest_bin_con <- ggforestplot::forestplot(
  df = IMP_main_meta_bin_con,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Exposure,
  shape = Exposure,
  xlab = "LogOR or beta and 95% CI (ref: non-vegetarian)",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("green4", "mediumblue", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21))

IMP_main_forest_bin_con

ggsave(
  IMP_main_forest_bin_con,
  file = "ALL/compare.subgroup_obs.forest_VegDiet.png",
  height = 13,
  width = 8
)
