################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 18-Dec-2024
# This script is to compare imputed vs. complete-case analysis results for vegetarian diets.

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

## Imputed

### Main analysis
IMP_main_meta_bin <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_bin.xlsx")
IMP_main_meta_bin <- IMP_main_meta_bin %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_bin$Analysis <- "Imputed (main)"
IMP_main_meta_bin

IMP_main_meta_con <- read.xlsx("ALL/IMP_MAIN_meta.tbl_VegDiet_con.xlsx")
IMP_main_meta_con <- IMP_main_meta_con %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_con$Analysis <- "Imputed (main)"
IMP_main_meta_con

IMP_main_meta_ord <- read.xlsx("ALL/IMP_BF_meta.tbl_VegDiet_ord.xlsx")
IMP_main_meta_ord <- IMP_main_meta_ord %>% select(Outcome, Exposure, b, se, pval)
IMP_main_meta_ord$Analysis <- "Imputed (main)"
IMP_main_meta_ord

### Paternal negative control
IMP_PNC_meta_bin_Mat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Mat1.xlsx")
IMP_PNC_meta_bin_Mat1$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_bin_Mat1 <- IMP_PNC_meta_bin_Mat1 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_bin_Mat1$Analysis <- "Imputed (main)"
IMP_PNC_meta_bin_Mat1

IMP_PNC_meta_bin_Mat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Mat2.xlsx")
IMP_PNC_meta_bin_Mat2$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_bin_Mat2 <- IMP_PNC_meta_bin_Mat2 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_bin_Mat2$Analysis <- "Imputed (main)"
IMP_PNC_meta_bin_Mat2

IMP_PNC_meta_bin_Pat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Pat1.xlsx")
IMP_PNC_meta_bin_Pat1$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_bin_Pat1 <- IMP_PNC_meta_bin_Pat1 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_bin_Pat1$Analysis <- "Imputed (main)"
IMP_PNC_meta_bin_Pat1

IMP_PNC_meta_bin_Pat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_bin_Pat2.xlsx")
IMP_PNC_meta_bin_Pat2$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_bin_Pat2 <- IMP_PNC_meta_bin_Pat2 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_bin_Pat2$Analysis <- "Imputed (main)"
IMP_PNC_meta_bin_Pat2

IMP_PNC_meta_con_Mat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_con_Mat1.xlsx")
IMP_PNC_meta_con_Mat1$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_con_Mat1 <- IMP_PNC_meta_con_Mat1 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_con_Mat1$Analysis <- "Imputed (main)"
IMP_PNC_meta_con_Mat1

IMP_PNC_meta_con_Mat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_con_Mat2.xlsx")
IMP_PNC_meta_con_Mat2$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_con_Mat2 <- IMP_PNC_meta_con_Mat2 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_con_Mat2$Analysis <- "Imputed (main)"
IMP_PNC_meta_con_Mat2

IMP_PNC_meta_con_Pat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_con_Pat1.xlsx")
IMP_PNC_meta_con_Pat1$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_con_Pat1 <- IMP_PNC_meta_con_Pat1 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_con_Pat1$Analysis <- "Imputed (main)"
IMP_PNC_meta_con_Pat1

IMP_PNC_meta_con_Pat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_con_Pat2.xlsx")
IMP_PNC_meta_con_Pat2$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_con_Pat2 <- IMP_PNC_meta_con_Pat2 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_con_Pat2$Analysis <- "Imputed (main)"
IMP_PNC_meta_con_Pat2

IMP_PNC_meta_ord_Mat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Mat1.xlsx")
IMP_PNC_meta_ord_Mat1$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_ord_Mat1 <- IMP_PNC_meta_ord_Mat1 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_ord_Mat1$Analysis <- "Imputed (main)"
IMP_PNC_meta_ord_Mat1

IMP_PNC_meta_ord_Mat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Mat2.xlsx")
IMP_PNC_meta_ord_Mat2$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_ord_Mat2 <- IMP_PNC_meta_ord_Mat2 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_ord_Mat2$Analysis <- "Imputed (main)"
IMP_PNC_meta_ord_Mat2

IMP_PNC_meta_ord_Pat1 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Pat1.xlsx")
IMP_PNC_meta_ord_Pat1$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_ord_Pat1 <- IMP_PNC_meta_ord_Pat1 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_ord_Pat1$Analysis <- "Imputed (main)"
IMP_PNC_meta_ord_Pat1

IMP_PNC_meta_ord_Pat2 <- read.xlsx("ALL/IMP_PNC_meta.tbl_VegDiet_ord_Pat2.xlsx")
IMP_PNC_meta_ord_Pat2$Exposure <- "Pesco-/full vs. non-vegetarian"
IMP_PNC_meta_ord_Pat2 <- IMP_PNC_meta_ord_Pat2 %>% select(Outcome, Exposure, b, se, pval)
IMP_PNC_meta_ord_Pat2$Analysis <- "Imputed (main)"
IMP_PNC_meta_ord_Pat2

## Complete-case

### Main analysis
main_meta_bin <- read.xlsx("ALL/MAIN_meta.tbl_VegDiet_bin.xlsx")
main_meta_bin <- main_meta_bin %>% select(Outcome, Exposure, b, se, pval)
main_meta_bin$Analysis <- "Complete-case"
main_meta_bin

main_meta_con <- read.xlsx("ALL/MAIN_meta.tbl_VegDiet_con.xlsx")
main_meta_con <- main_meta_con %>% select(Outcome, Exposure, b, se, pval)
main_meta_con$Analysis <- "Complete-case"
main_meta_con

main_meta_ord <- read.xlsx("ALL/BF_meta.tbl_VegDiet_ord.xlsx")
main_meta_ord <- main_meta_ord %>% select(Outcome, Exposure, b, se, pval)
main_meta_ord$Analysis <- "Complete-case"
main_meta_ord

### Paternal negative control
PNC_meta_bin_Mat1 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_bin_Mat1.xlsx")
PNC_meta_bin_Mat1$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_bin_Mat1 <- PNC_meta_bin_Mat1 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_bin_Mat1$Analysis <- "Complete-case"
PNC_meta_bin_Mat1

PNC_meta_bin_Mat2 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_bin_Mat2.xlsx")
PNC_meta_bin_Mat2$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_bin_Mat2 <- PNC_meta_bin_Mat2 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_bin_Mat2$Analysis <- "Complete-case"
PNC_meta_bin_Mat2

PNC_meta_bin_Pat1 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_bin_Pat1.xlsx")
PNC_meta_bin_Pat1$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_bin_Pat1 <- PNC_meta_bin_Pat1 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_bin_Pat1$Analysis <- "Complete-case"
PNC_meta_bin_Pat1

PNC_meta_bin_Pat2 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_bin_Pat2.xlsx")
PNC_meta_bin_Pat2$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_bin_Pat2 <- PNC_meta_bin_Pat2 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_bin_Pat2$Analysis <- "Complete-case"
PNC_meta_bin_Pat2

PNC_meta_con_Mat1 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_con_Mat1.xlsx")
PNC_meta_con_Mat1$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_con_Mat1 <- PNC_meta_con_Mat1 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_con_Mat1$Analysis <- "Complete-case"
PNC_meta_con_Mat1

PNC_meta_con_Mat2 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_con_Mat2.xlsx")
PNC_meta_con_Mat2$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_con_Mat2 <- PNC_meta_con_Mat2 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_con_Mat2$Analysis <- "Complete-case"
PNC_meta_con_Mat2

PNC_meta_con_Pat1 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_con_Pat1.xlsx")
PNC_meta_con_Pat1$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_con_Pat1 <- PNC_meta_con_Pat1 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_con_Pat1$Analysis <- "Complete-case"
PNC_meta_con_Pat1

PNC_meta_con_Pat2 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_con_Pat2.xlsx")
PNC_meta_con_Pat2$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_con_Pat2 <- PNC_meta_con_Pat2 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_con_Pat2$Analysis <- "Complete-case"
PNC_meta_con_Pat2

PNC_meta_ord_Mat1 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_ord_Mat1.xlsx")
PNC_meta_ord_Mat1$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_ord_Mat1 <- PNC_meta_ord_Mat1 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_ord_Mat1$Analysis <- "Complete-case"
PNC_meta_ord_Mat1

PNC_meta_ord_Mat2 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_ord_Mat2.xlsx")
PNC_meta_ord_Mat2$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_ord_Mat2 <- PNC_meta_ord_Mat2 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_ord_Mat2$Analysis <- "Complete-case"
PNC_meta_ord_Mat2

PNC_meta_ord_Pat1 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_ord_Pat1.xlsx")
PNC_meta_ord_Pat1$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_ord_Pat1 <- PNC_meta_ord_Pat1 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_ord_Pat1$Analysis <- "Complete-case"
PNC_meta_ord_Pat1

PNC_meta_ord_Pat2 <- read.xlsx("ALL/PNC_meta.tbl_VegDiet_ord_Pat2.xlsx")
PNC_meta_ord_Pat2$Exposure <- "Pesco-/full vs. non-vegetarian"
PNC_meta_ord_Pat2 <- PNC_meta_ord_Pat2 %>% select(Outcome, Exposure, b, se, pval)
PNC_meta_ord_Pat2$Analysis <- "Complete-case"
PNC_meta_ord_Pat2

################################################################################

## Combine
main_meta_bin <- rbind(IMP_main_meta_bin, main_meta_bin)
main_meta_con <- rbind(IMP_main_meta_con, main_meta_con)
main_meta_ord <- rbind(IMP_main_meta_ord, main_meta_ord)
main_meta_bin_con_ord <- rbind(main_meta_bin, main_meta_con, main_meta_ord)

PNC_meta_bin_Mat1 <- rbind(IMP_PNC_meta_bin_Mat1, PNC_meta_bin_Mat1)
PNC_meta_con_Mat1 <- rbind(IMP_PNC_meta_con_Mat1, PNC_meta_con_Mat1)
PNC_meta_ord_Mat1 <- rbind(IMP_PNC_meta_ord_Mat1, PNC_meta_ord_Mat1)
PNC_meta_bin_con_ord_Mat1 <- rbind(PNC_meta_bin_Mat1, PNC_meta_con_Mat1, PNC_meta_ord_Mat1)

PNC_meta_bin_Mat2 <- rbind(IMP_PNC_meta_bin_Mat2, PNC_meta_bin_Mat2)
PNC_meta_con_Mat2 <- rbind(IMP_PNC_meta_con_Mat2, PNC_meta_con_Mat2)
PNC_meta_ord_Mat2 <- rbind(IMP_PNC_meta_ord_Mat2, PNC_meta_ord_Mat2)
PNC_meta_bin_con_ord_Mat2 <- rbind(PNC_meta_bin_Mat2, PNC_meta_con_Mat2, PNC_meta_ord_Mat2)

PNC_meta_bin_Pat1 <- rbind(IMP_PNC_meta_bin_Pat1, PNC_meta_bin_Pat1)
PNC_meta_con_Pat1 <- rbind(IMP_PNC_meta_con_Pat1, PNC_meta_con_Pat1)
PNC_meta_ord_Pat1 <- rbind(IMP_PNC_meta_ord_Pat1, PNC_meta_ord_Pat1)
PNC_meta_bin_con_ord_Pat1 <- rbind(PNC_meta_bin_Pat1, PNC_meta_con_Pat1, PNC_meta_ord_Pat1)

PNC_meta_bin_Pat2 <- rbind(IMP_PNC_meta_bin_Pat2, PNC_meta_bin_Pat2)
PNC_meta_con_Pat2 <- rbind(IMP_PNC_meta_con_Pat2, PNC_meta_con_Pat2)
PNC_meta_ord_Pat2 <- rbind(IMP_PNC_meta_ord_Pat2, PNC_meta_ord_Pat2)
PNC_meta_bin_con_ord_Pat2 <- rbind(PNC_meta_bin_Pat2, PNC_meta_con_Pat2, PNC_meta_ord_Pat2)

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
ALL_primary_bin <- subset(primary_bin, label %in% main_meta_bin$Outcome)
ALL_primary_bin
dim(ALL_primary_bin)  # 13 primary (binary) outcomes available in the combined results

ALL_secondary_bin <- subset(secondary_bin,
                            label %in% main_meta_bin$Outcome &
                              varname != "anaemia_preg_subsamp")  # Maternal anaemia (occurring during pregnancy) is only available for sensitivity analysis in MoBa
ALL_secondary_bin
dim(ALL_secondary_bin)  # 8 secondary binary outcomes available in the combined results

ALL_secondary_con <- subset(secondary_con, label %in% main_meta_con$Outcome)
ALL_secondary_con
dim(ALL_secondary_con)  # 4 secondary continuous outcomes available in the combined results

################################################################################

# Prepare results for comparison

################################################################################
## Change character into numeric
main_meta_bin_con_ord <- type.convert(main_meta_bin_con_ord, as.is = T)

PNC_meta_bin_con_ord_Mat1 <- type.convert(PNC_meta_bin_con_ord_Mat1, as.is = T)
PNC_meta_bin_con_ord_Mat2 <- type.convert(PNC_meta_bin_con_ord_Mat2, as.is = T)
PNC_meta_bin_con_ord_Pat1 <- type.convert(PNC_meta_bin_con_ord_Pat1, as.is = T)
PNC_meta_bin_con_ord_Pat2 <- type.convert(PNC_meta_bin_con_ord_Pat2, as.is = T)
################################################################################

## Main analysis
main_meta_bin_con_ord$Outcome <-
  factor(main_meta_bin_con_ord$Outcome,
         levels =
           unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% main_meta_bin_con_ord$Outcome])
main_meta_bin_con_ord <-
  main_meta_bin_con_ord %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

main_meta_bin_con_ord$Analysis <- factor(main_meta_bin_con_ord$Analysis,
                                         levels = c("Complete-case", "Imputed (main)"))

## Paternal negative control
PNC_meta_bin_con_ord_Mat1$Outcome <-
  factor(PNC_meta_bin_con_ord_Mat1$Outcome,
         levels =
           unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% PNC_meta_bin_con_ord_Mat1$Outcome])
PNC_meta_bin_con_ord_Mat1 <-
  PNC_meta_bin_con_ord_Mat1 %>% arrange(Outcome)  # Make sure the outcomes appear in the right order
PNC_meta_bin_con_ord_Mat1$Analysis <- factor(PNC_meta_bin_con_ord_Mat1$Analysis,
                                             levels = c("Complete-case", "Imputed (main)"))

PNC_meta_bin_con_ord_Mat2$Outcome <-
  factor(PNC_meta_bin_con_ord_Mat2$Outcome,
         levels =
           unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% PNC_meta_bin_con_ord_Mat2$Outcome])
PNC_meta_bin_con_ord_Mat2 <-
  PNC_meta_bin_con_ord_Mat2 %>% arrange(Outcome)  # Make sure the outcomes appear in the right order
PNC_meta_bin_con_ord_Mat2$Analysis <- factor(PNC_meta_bin_con_ord_Mat2$Analysis,
                                             levels = c("Complete-case", "Imputed (main)"))

PNC_meta_bin_con_ord_Pat1$Outcome <-
  factor(PNC_meta_bin_con_ord_Pat1$Outcome,
         levels =
           unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% PNC_meta_bin_con_ord_Pat1$Outcome])
PNC_meta_bin_con_ord_Pat1 <-
  PNC_meta_bin_con_ord_Pat1 %>% arrange(Outcome)  # Make sure the outcomes appear in the right order
PNC_meta_bin_con_ord_Pat1$Analysis <- factor(PNC_meta_bin_con_ord_Pat1$Analysis,
                                             levels = c("Complete-case", "Imputed (main)"))

PNC_meta_bin_con_ord_Pat2$Outcome <-
  factor(PNC_meta_bin_con_ord_Pat2$Outcome,
         levels =
           unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% PNC_meta_bin_con_ord_Pat2$Outcome])
PNC_meta_bin_con_ord_Pat2 <-
  PNC_meta_bin_con_ord_Pat2 %>% arrange(Outcome)  # Make sure the outcomes appear in the right order
PNC_meta_bin_con_ord_Pat2$Analysis <- factor(PNC_meta_bin_con_ord_Pat2$Analysis,
                                             levels = c("Complete-case", "Imputed (main)"))

#------------------------------------------------------------------------------#
#                                 Forest Plots                                 #----
#------------------------------------------------------------------------------#

# Main analysis
main_forest_bin_con_ord <- ggforestplot::forestplot(
  df = main_meta_bin_con_ord,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Analysis,
  shape = Analysis,
  xlab = "LogOR or beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "Main Analysis",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("firebrick", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

main_forest_bin_con_ord

# Paternal negative control

## Maternal Model 1
PNC_forest_bin_con_ord_Mat1 <- ggforestplot::forestplot(
  df = PNC_meta_bin_con_ord_Mat1,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Analysis,
  shape = Analysis,
  xlab = "LogOR or beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "Maternal Model 1",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("firebrick", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

PNC_forest_bin_con_ord_Mat1

## Maternal Model 2
PNC_forest_bin_con_ord_Mat2 <- ggforestplot::forestplot(
  df = PNC_meta_bin_con_ord_Mat2,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Analysis,
  shape = Analysis,
  xlab = "LogOR or beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "Maternal Model 2",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("firebrick", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

PNC_forest_bin_con_ord_Mat2

## Paternal Model 1
PNC_forest_bin_con_ord_Pat1 <- ggforestplot::forestplot(
  df = PNC_meta_bin_con_ord_Pat1,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Analysis,
  shape = Analysis,
  xlab = "LogOR or beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "Paternal Model 1",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("firebrick", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

PNC_forest_bin_con_ord_Pat1

## Paternal Model 2
PNC_forest_bin_con_ord_Pat2 <- ggforestplot::forestplot(
  df = PNC_meta_bin_con_ord_Pat2,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Analysis,
  shape = Analysis,
  xlab = "LogOR or beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "Paternal Model 2",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("firebrick", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

PNC_forest_bin_con_ord_Pat2

#------------------------------------------------------------------------------#
#                                Combine Plots                                 #----
#------------------------------------------------------------------------------#

# Combine cross-context comparison results for both binary and continuous outcomes
main_forest_bin_con_ord <- main_forest_bin_con_ord + ggplot2::theme(legend.position = "bottom",
                                                                    legend.justification = c(-1000, 0))
print(main_forest_bin_con_ord)
grob1 <- grid.grab()
dev.off()

PNC_forest_bin_con_ord_Mat1 <- PNC_forest_bin_con_ord_Mat1 + ggplot2::theme(legend.position = "bottom",
                                                                            legend.justification = c(-1000, 0))
print(PNC_forest_bin_con_ord_Mat1)
grob2 <- grid.grab()
dev.off()

PNC_forest_bin_con_ord_Mat2 <- PNC_forest_bin_con_ord_Mat2 + ggplot2::theme(legend.position = "bottom")
print(PNC_forest_bin_con_ord_Mat2)
grob3 <- grid.grab()
dev.off()

PNC_forest_bin_con_ord_Pat1 <- PNC_forest_bin_con_ord_Pat1 + ggplot2::theme(legend.position = "bottom",
                                                                            legend.justification = c(1000, 0))
print(PNC_forest_bin_con_ord_Pat1)
grob4 <- grid.grab()
dev.off()

PNC_forest_bin_con_ord_Pat2 <- PNC_forest_bin_con_ord_Pat2 + ggplot2::theme(legend.position = "bottom",
                                                                            legend.justification = c(1000, 0))
print(PNC_forest_bin_con_ord_Pat2)
grob5 <- grid.grab()
dev.off()

png(
  "ALL/Comb_compare.IMP_obs.forest_VegDiet.png",
  res = 300,
  height = 3000,
  width = 7000
)
grid.arrange(grob1,
             grob2,
             grob3,
             grob4,
             grob5,
             ncol = 5,
             widths = c(1, 1, 1, 1, 1))
dev.off()
