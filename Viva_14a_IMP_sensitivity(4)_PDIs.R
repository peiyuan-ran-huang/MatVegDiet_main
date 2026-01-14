################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 18-Aug-2025
# This script is to perform sensitivity analysis (with imputed data) for plant-based diet indices (PDIs) in Project Viva.
## (3) Forest plots to compare results across different timepoints

################################################################################

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

################################################################################

# For the overall plant-based diet index (PDI)

## Binary outcomes
meta.res_PDI_bin <- read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_bin.xlsx")
meta.res_PDI_bin <- subset(meta.res_PDI_bin, select = c("Outcome", "b", "se", "pval"))
meta.res_PDI_bin$Timepoint <- "Main results (from all 3 cohorts)"
meta.res_PDI_bin

obs.res_PDI_bin_1 <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_PDI_bin.xlsx")
obs.res_PDI_bin_1 <- subset(obs.res_PDI_bin_1,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_bin_1$Timepoint <- "Early pregnancy"
obs.res_PDI_bin_1

obs.res_PDI_bin_2 <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_PDI_bin.xlsx")
obs.res_PDI_bin_2 <- subset(obs.res_PDI_bin_2,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_bin_2$Timepoint <- "Mid-pregnancy"
obs.res_PDI_bin_2

obs.res_PDI_bin_3 <- read.xlsx("Viva/IMP_SENS.MID.p_adj.EAR.p_obs.res_PDI_bin.xlsx")
obs.res_PDI_bin_3 <- subset(obs.res_PDI_bin_3,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_bin_3$Timepoint <- "Mid-pregnancy (adjusted for early pregnancy exposure)"
obs.res_PDI_bin_3

obs.res_PDI_bin_4 <- read.xlsx("Viva/IMP_MAIN_obs.res_PDI_bin.xlsx")
obs.res_PDI_bin_4 <- subset(obs.res_PDI_bin_4,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_bin_4$Timepoint <- "During pregnancy (mean intakes across timepoints)"
obs.res_PDI_bin_4

obs.res_PDI_bin <- rbind(
  subset(meta.res_PDI_bin, Outcome %in% obs.res_PDI_bin_1$Outcome),
  obs.res_PDI_bin_1,
  obs.res_PDI_bin_2,
  obs.res_PDI_bin_3,
  obs.res_PDI_bin_4
)

obs.res_PDI_bin[, c("b", "se", "pval")] <-
  sapply(obs.res_PDI_bin[, c("b", "se", "pval")], as.numeric)

obs.res_PDI_bin$Timepoint <- factor(
  obs.res_PDI_bin$Timepoint,
  levels = c(
    "Mid-pregnancy (adjusted for early pregnancy exposure)",
    "Mid-pregnancy",
    "Early pregnancy",
    "During pregnancy (mean intakes across timepoints)",
    "Main results (from all 3 cohorts)"
  ),
  ordered = T
)

obs.res_PDI_bin$Group <- "Binary outcome"

obs.res_PDI_bin
str(obs.res_PDI_bin)

################################################################################
################################################################################

## Continuous outcomes
meta.res_PDI_con <- read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_con.xlsx")
meta.res_PDI_con <- subset(meta.res_PDI_con, select = c("Outcome", "b", "se", "pval"))
meta.res_PDI_con$Timepoint <- "Main results (from all 3 cohorts)"
meta.res_PDI_con

obs.res_PDI_con_1 <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_PDI_con.xlsx")
obs.res_PDI_con_1 <- subset(obs.res_PDI_con_1,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_con_1$Timepoint <- "Early pregnancy"
obs.res_PDI_con_1

obs.res_PDI_con_2 <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_PDI_con.xlsx")
obs.res_PDI_con_2 <- subset(obs.res_PDI_con_2,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_con_2$Timepoint <- "Mid-pregnancy"
obs.res_PDI_con_2

obs.res_PDI_con_3 <- read.xlsx("Viva/IMP_SENS.MID.p_adj.EAR.p_obs.res_PDI_con.xlsx")
obs.res_PDI_con_3 <- subset(obs.res_PDI_con_3,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_con_3$Timepoint <- "Mid-pregnancy (adjusted for early pregnancy exposure)"
obs.res_PDI_con_3

obs.res_PDI_con_4 <- read.xlsx("Viva/IMP_MAIN_obs.res_PDI_con.xlsx")
obs.res_PDI_con_4 <- subset(obs.res_PDI_con_4,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_con_4$Timepoint <- "During pregnancy (mean intakes across timepoints)"
obs.res_PDI_con_4

obs.res_PDI_con <- rbind(
  subset(meta.res_PDI_con, Outcome %in% obs.res_PDI_con_1$Outcome),
  obs.res_PDI_con_1,
  obs.res_PDI_con_2,
  obs.res_PDI_con_3,
  obs.res_PDI_con_4
)

obs.res_PDI_con[, c("b", "se", "pval")] <-
  sapply(obs.res_PDI_con[, c("b", "se", "pval")], as.numeric)

obs.res_PDI_con$Timepoint <- factor(
  obs.res_PDI_con$Timepoint,
  levels = c(
    "Mid-pregnancy (adjusted for early pregnancy exposure)",
    "Mid-pregnancy",
    "Early pregnancy",
    "During pregnancy (mean intakes across timepoints)",
    "Main results (from all 3 cohorts)"
  ),
  ordered = T
)

obs.res_PDI_con$Group <- "Continuous outcome"

obs.res_PDI_con
str(obs.res_PDI_con)

################################################################################
################################################################################

## Ordinal outcome (i.e., breastfeeding duration)
meta.res_PDI_ord <- read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_ord.xlsx")
meta.res_PDI_ord <- subset(meta.res_PDI_ord, select = c("Outcome", "b", "se", "pval"))
meta.res_PDI_ord$Timepoint <- "Main results (from all 3 cohorts)"
meta.res_PDI_ord

obs.res_PDI_ord_1 <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_PDI_ord.xlsx")
obs.res_PDI_ord_1 <- subset(obs.res_PDI_ord_1,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_ord_1$Timepoint <- "Early pregnancy"
obs.res_PDI_ord_1

obs.res_PDI_ord_2 <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_PDI_ord.xlsx")
obs.res_PDI_ord_2 <- subset(obs.res_PDI_ord_2,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_ord_2$Timepoint <- "Mid-pregnancy"
obs.res_PDI_ord_2

obs.res_PDI_ord_3 <- read.xlsx("Viva/IMP_SENS.MID.p_adj.EAR.p_obs.res_PDI_ord.xlsx")
obs.res_PDI_ord_3 <- subset(obs.res_PDI_ord_3,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_ord_3$Timepoint <- "Mid-pregnancy (adjusted for early pregnancy exposure)"
obs.res_PDI_ord_3

obs.res_PDI_ord_4 <- read.xlsx("Viva/IMP_BF_obs.res_PDI_ord.xlsx")
obs.res_PDI_ord_4 <- subset(obs.res_PDI_ord_4,
                            Model == "Model 3",
                            select = c("Outcome", "b", "se", "pval"))
obs.res_PDI_ord_4$Timepoint <- "During pregnancy (mean intakes across timepoints)"
obs.res_PDI_ord_4

obs.res_PDI_ord <- rbind(
  subset(meta.res_PDI_ord, Outcome %in% obs.res_PDI_ord_1$Outcome),
  obs.res_PDI_ord_1,
  obs.res_PDI_ord_2,
  obs.res_PDI_ord_3,
  obs.res_PDI_ord_4
)

obs.res_PDI_ord[, c("b", "se", "pval")] <-
  sapply(obs.res_PDI_ord[, c("b", "se", "pval")], as.numeric)

obs.res_PDI_ord$Timepoint <- factor(
  obs.res_PDI_ord$Timepoint,
  levels = c(
    "Mid-pregnancy (adjusted for early pregnancy exposure)",
    "Mid-pregnancy",
    "Early pregnancy",
    "During pregnancy (mean intakes across timepoints)",
    "Main results (from all 3 cohorts)"
  ),
  ordered = T
)

obs.res_PDI_ord$Group <- "Ordinal outcome"

obs.res_PDI_ord
str(obs.res_PDI_ord)

################################################################################

## Nightingale forestplots
obs.res_PDI <- rbind(obs.res_PDI_bin, obs.res_PDI_con, obs.res_PDI_ord)

obs.res_PDI$Group <- factor(
  obs.res_PDI$Group,
  levels = c("Binary outcome", "Continuous outcome", "Ordinal outcome"),
  ordered = T
)

meta.forest_PDI <- ggforestplot::forestplot(
  df = obs.res_PDI,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Timepoint,
  shape = Timepoint,
  xlab = "logOR or beta and 95% CI",
  title = "Overall plant-based diet index (PDI)",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c(
    "mediumblue",
    "darkorange",
    "blueviolet",
    "darkred",
    "darkcyan"
  )) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space  = "free")

meta.forest_PDI

ggsave(meta.forest_PDI,
       file = "Viva/compare.timepoint_obs.forest_PDI.png",
       height = 16,
       width = 11)

################################################################################

# For the healthful plant-based diet index (hPDI)

## Binary outcomes
meta.res_hPDI_bin <- read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_bin.xlsx")
meta.res_hPDI_bin <- subset(meta.res_hPDI_bin, select = c("Outcome", "b", "se", "pval"))
meta.res_hPDI_bin$Timepoint <- "Main results (from all 3 cohorts)"
meta.res_hPDI_bin

obs.res_hPDI_bin_1 <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_hPDI_bin.xlsx")
obs.res_hPDI_bin_1 <- subset(obs.res_hPDI_bin_1,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_bin_1$Timepoint <- "Early pregnancy"
obs.res_hPDI_bin_1

obs.res_hPDI_bin_2 <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_hPDI_bin.xlsx")
obs.res_hPDI_bin_2 <- subset(obs.res_hPDI_bin_2,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_bin_2$Timepoint <- "Mid-pregnancy"
obs.res_hPDI_bin_2

obs.res_hPDI_bin_3 <- read.xlsx("Viva/IMP_SENS.MID.p_adj.EAR.p_obs.res_hPDI_bin.xlsx")
obs.res_hPDI_bin_3 <- subset(obs.res_hPDI_bin_3,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_bin_3$Timepoint <- "Mid-pregnancy (adjusted for early pregnancy exposure)"
obs.res_hPDI_bin_3

obs.res_hPDI_bin_4 <- read.xlsx("Viva/IMP_MAIN_obs.res_hPDI_bin.xlsx")
obs.res_hPDI_bin_4 <- subset(obs.res_hPDI_bin_4,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_bin_4$Timepoint <- "During pregnancy (mean intakes across timepoints)"
obs.res_hPDI_bin_4

obs.res_hPDI_bin <- rbind(
  subset(meta.res_hPDI_bin, Outcome %in% obs.res_hPDI_bin_1$Outcome),
  obs.res_hPDI_bin_1,
  obs.res_hPDI_bin_2,
  obs.res_hPDI_bin_3,
  obs.res_hPDI_bin_4
)

obs.res_hPDI_bin[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_bin[, c("b", "se", "pval")], as.numeric)

obs.res_hPDI_bin$Timepoint <- factor(
  obs.res_hPDI_bin$Timepoint,
  levels = c(
    "Mid-pregnancy (adjusted for early pregnancy exposure)",
    "Mid-pregnancy",
    "Early pregnancy",
    "During pregnancy (mean intakes across timepoints)",
    "Main results (from all 3 cohorts)"
  ),
  ordered = T
)

obs.res_hPDI_bin$Group <- "Binary outcome"

obs.res_hPDI_bin
str(obs.res_hPDI_bin)

################################################################################
################################################################################

## Continuous outcomes
meta.res_hPDI_con <- read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_con.xlsx")
meta.res_hPDI_con <- subset(meta.res_hPDI_con, select = c("Outcome", "b", "se", "pval"))
meta.res_hPDI_con$Timepoint <- "Main results (from all 3 cohorts)"
meta.res_hPDI_con

obs.res_hPDI_con_1 <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_hPDI_con.xlsx")
obs.res_hPDI_con_1 <- subset(obs.res_hPDI_con_1,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_con_1$Timepoint <- "Early pregnancy"
obs.res_hPDI_con_1

obs.res_hPDI_con_2 <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_hPDI_con.xlsx")
obs.res_hPDI_con_2 <- subset(obs.res_hPDI_con_2,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_con_2$Timepoint <- "Mid-pregnancy"
obs.res_hPDI_con_2

obs.res_hPDI_con_3 <- read.xlsx("Viva/IMP_SENS.MID.p_adj.EAR.p_obs.res_hPDI_con.xlsx")
obs.res_hPDI_con_3 <- subset(obs.res_hPDI_con_3,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_con_3$Timepoint <- "Mid-pregnancy (adjusted for early pregnancy exposure)"
obs.res_hPDI_con_3

obs.res_hPDI_con_4 <- read.xlsx("Viva/IMP_MAIN_obs.res_hPDI_con.xlsx")
obs.res_hPDI_con_4 <- subset(obs.res_hPDI_con_4,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_con_4$Timepoint <- "During pregnancy (mean intakes across timepoints)"
obs.res_hPDI_con_4

obs.res_hPDI_con <- rbind(
  subset(meta.res_hPDI_con, Outcome %in% obs.res_hPDI_con_1$Outcome),
  obs.res_hPDI_con_1,
  obs.res_hPDI_con_2,
  obs.res_hPDI_con_3,
  obs.res_hPDI_con_4
)

obs.res_hPDI_con[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_con[, c("b", "se", "pval")], as.numeric)

obs.res_hPDI_con$Timepoint <- factor(
  obs.res_hPDI_con$Timepoint,
  levels = c(
    "Mid-pregnancy (adjusted for early pregnancy exposure)",
    "Mid-pregnancy",
    "Early pregnancy",
    "During pregnancy (mean intakes across timepoints)",
    "Main results (from all 3 cohorts)"
  ),
  ordered = T
)

obs.res_hPDI_con$Group <- "Continuous outcome"

obs.res_hPDI_con
str(obs.res_hPDI_con)

################################################################################
################################################################################

## Ordinal outcome (i.e., breastfeeding duration)
meta.res_hPDI_ord <- read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_ord.xlsx")
meta.res_hPDI_ord <- subset(meta.res_hPDI_ord, select = c("Outcome", "b", "se", "pval"))
meta.res_hPDI_ord$Timepoint <- "Main results (from all 3 cohorts)"
meta.res_hPDI_ord

obs.res_hPDI_ord_1 <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_hPDI_ord.xlsx")
obs.res_hPDI_ord_1 <- subset(obs.res_hPDI_ord_1,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_ord_1$Timepoint <- "Early pregnancy"
obs.res_hPDI_ord_1

obs.res_hPDI_ord_2 <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_hPDI_ord.xlsx")
obs.res_hPDI_ord_2 <- subset(obs.res_hPDI_ord_2,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_ord_2$Timepoint <- "Mid-pregnancy"
obs.res_hPDI_ord_2

obs.res_hPDI_ord_3 <- read.xlsx("Viva/IMP_SENS.MID.p_adj.EAR.p_obs.res_hPDI_ord.xlsx")
obs.res_hPDI_ord_3 <- subset(obs.res_hPDI_ord_3,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_ord_3$Timepoint <- "Mid-pregnancy (adjusted for early pregnancy exposure)"
obs.res_hPDI_ord_3

obs.res_hPDI_ord_4 <- read.xlsx("Viva/IMP_BF_obs.res_hPDI_ord.xlsx")
obs.res_hPDI_ord_4 <- subset(obs.res_hPDI_ord_4,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_hPDI_ord_4$Timepoint <- "During pregnancy (mean intakes across timepoints)"
obs.res_hPDI_ord_4

obs.res_hPDI_ord <- rbind(
  subset(meta.res_hPDI_ord, Outcome %in% obs.res_hPDI_ord_1$Outcome),
  obs.res_hPDI_ord_1,
  obs.res_hPDI_ord_2,
  obs.res_hPDI_ord_3,
  obs.res_hPDI_ord_4
)

obs.res_hPDI_ord[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_ord[, c("b", "se", "pval")], as.numeric)

obs.res_hPDI_ord$Timepoint <- factor(
  obs.res_hPDI_ord$Timepoint,
  levels = c(
    "Mid-pregnancy (adjusted for early pregnancy exposure)",
    "Mid-pregnancy",
    "Early pregnancy",
    "During pregnancy (mean intakes across timepoints)",
    "Main results (from all 3 cohorts)"
  ),
  ordered = T
)

obs.res_hPDI_ord$Group <- "Ordinal outcome"

obs.res_hPDI_ord
str(obs.res_hPDI_ord)

################################################################################

## Nightingale forestplots
obs.res_hPDI <- rbind(obs.res_hPDI_bin, obs.res_hPDI_con, obs.res_hPDI_ord)

obs.res_hPDI$Group <- factor(
  obs.res_hPDI$Group,
  levels = c("Binary outcome", "Continuous outcome", "Ordinal outcome"),
  ordered = T
)

meta.forest_hPDI <- ggforestplot::forestplot(
  df = obs.res_hPDI,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Timepoint,
  shape = Timepoint,
  xlab = "logOR or beta and 95% CI",
  title = "Healthful plant-based diet index (hPDI)",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c(
    "mediumblue",
    "darkorange",
    "blueviolet",
    "darkred",
    "darkcyan"
  )) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space  = "free")

meta.forest_hPDI

ggsave(meta.forest_hPDI,
       file = "Viva/compare.timepoint_obs.forest_hPDI.png",
       height = 16,
       width = 11)

################################################################################

# For the unhealthful plant-based diet index (uPDI)

## Binary outcomes
meta.res_uPDI_bin <- read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_bin.xlsx")
meta.res_uPDI_bin <- subset(meta.res_uPDI_bin, select = c("Outcome", "b", "se", "pval"))
meta.res_uPDI_bin$Timepoint <- "Main results (from all 3 cohorts)"
meta.res_uPDI_bin

obs.res_uPDI_bin_1 <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_uPDI_bin.xlsx")
obs.res_uPDI_bin_1 <- subset(obs.res_uPDI_bin_1,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_bin_1$Timepoint <- "Early pregnancy"
obs.res_uPDI_bin_1

obs.res_uPDI_bin_2 <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_uPDI_bin.xlsx")
obs.res_uPDI_bin_2 <- subset(obs.res_uPDI_bin_2,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_bin_2$Timepoint <- "Mid-pregnancy"
obs.res_uPDI_bin_2

obs.res_uPDI_bin_3 <- read.xlsx("Viva/IMP_SENS.MID.p_adj.EAR.p_obs.res_uPDI_bin.xlsx")
obs.res_uPDI_bin_3 <- subset(obs.res_uPDI_bin_3,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_bin_3$Timepoint <- "Mid-pregnancy (adjusted for early pregnancy exposure)"
obs.res_uPDI_bin_3

obs.res_uPDI_bin_4 <- read.xlsx("Viva/IMP_MAIN_obs.res_uPDI_bin.xlsx")
obs.res_uPDI_bin_4 <- subset(obs.res_uPDI_bin_4,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_bin_4$Timepoint <- "During pregnancy (mean intakes across timepoints)"
obs.res_uPDI_bin_4

obs.res_uPDI_bin <- rbind(
  subset(meta.res_uPDI_bin, Outcome %in% obs.res_uPDI_bin_1$Outcome),
  obs.res_uPDI_bin_1,
  obs.res_uPDI_bin_2,
  obs.res_uPDI_bin_3,
  obs.res_uPDI_bin_4
)

obs.res_uPDI_bin[, c("b", "se", "pval")] <-
  sapply(obs.res_uPDI_bin[, c("b", "se", "pval")], as.numeric)

obs.res_uPDI_bin$Timepoint <- factor(
  obs.res_uPDI_bin$Timepoint,
  levels = c(
    "Mid-pregnancy (adjusted for early pregnancy exposure)",
    "Mid-pregnancy",
    "Early pregnancy",
    "During pregnancy (mean intakes across timepoints)",
    "Main results (from all 3 cohorts)"
  ),
  ordered = T
)

obs.res_uPDI_bin$Group <- "Binary outcome"

obs.res_uPDI_bin
str(obs.res_uPDI_bin)

################################################################################
################################################################################

## Continuous outcomes
meta.res_uPDI_con <- read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_con.xlsx")
meta.res_uPDI_con <- subset(meta.res_uPDI_con, select = c("Outcome", "b", "se", "pval"))
meta.res_uPDI_con$Timepoint <- "Main results (from all 3 cohorts)"
meta.res_uPDI_con

obs.res_uPDI_con_1 <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_uPDI_con.xlsx")
obs.res_uPDI_con_1 <- subset(obs.res_uPDI_con_1,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_con_1$Timepoint <- "Early pregnancy"
obs.res_uPDI_con_1

obs.res_uPDI_con_2 <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_uPDI_con.xlsx")
obs.res_uPDI_con_2 <- subset(obs.res_uPDI_con_2,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_con_2$Timepoint <- "Mid-pregnancy"
obs.res_uPDI_con_2

obs.res_uPDI_con_3 <- read.xlsx("Viva/IMP_SENS.MID.p_adj.EAR.p_obs.res_uPDI_con.xlsx")
obs.res_uPDI_con_3 <- subset(obs.res_uPDI_con_3,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_con_3$Timepoint <- "Mid-pregnancy (adjusted for early pregnancy exposure)"
obs.res_uPDI_con_3

obs.res_uPDI_con_4 <- read.xlsx("Viva/IMP_MAIN_obs.res_uPDI_con.xlsx")
obs.res_uPDI_con_4 <- subset(obs.res_uPDI_con_4,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_con_4$Timepoint <- "During pregnancy (mean intakes across timepoints)"
obs.res_uPDI_con_4

obs.res_uPDI_con <- rbind(
  subset(meta.res_uPDI_con, Outcome %in% obs.res_uPDI_con_1$Outcome),
  obs.res_uPDI_con_1,
  obs.res_uPDI_con_2,
  obs.res_uPDI_con_3,
  obs.res_uPDI_con_4
)

obs.res_uPDI_con[, c("b", "se", "pval")] <-
  sapply(obs.res_uPDI_con[, c("b", "se", "pval")], as.numeric)

obs.res_uPDI_con$Timepoint <- factor(
  obs.res_uPDI_con$Timepoint,
  levels = c(
    "Mid-pregnancy (adjusted for early pregnancy exposure)",
    "Mid-pregnancy",
    "Early pregnancy",
    "During pregnancy (mean intakes across timepoints)",
    "Main results (from all 3 cohorts)"
  ),
  ordered = T
)

obs.res_uPDI_con$Group <- "Continuous outcome"

obs.res_uPDI_con
str(obs.res_uPDI_con)

################################################################################
################################################################################

## Ordinal outcome (i.e., breastfeeding duration)
meta.res_uPDI_ord <- read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_ord.xlsx")
meta.res_uPDI_ord <- subset(meta.res_uPDI_ord, select = c("Outcome", "b", "se", "pval"))
meta.res_uPDI_ord$Timepoint <- "Main results (from all 3 cohorts)"
meta.res_uPDI_ord

obs.res_uPDI_ord_1 <- read.xlsx("Viva/IMP_SENS.EAR.p_obs.res_uPDI_ord.xlsx")
obs.res_uPDI_ord_1 <- subset(obs.res_uPDI_ord_1,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_ord_1$Timepoint <- "Early pregnancy"
obs.res_uPDI_ord_1

obs.res_uPDI_ord_2 <- read.xlsx("Viva/IMP_SENS.MID.p_obs.res_uPDI_ord.xlsx")
obs.res_uPDI_ord_2 <- subset(obs.res_uPDI_ord_2,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_ord_2$Timepoint <- "Mid-pregnancy"
obs.res_uPDI_ord_2

obs.res_uPDI_ord_3 <- read.xlsx("Viva/IMP_SENS.MID.p_adj.EAR.p_obs.res_uPDI_ord.xlsx")
obs.res_uPDI_ord_3 <- subset(obs.res_uPDI_ord_3,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_ord_3$Timepoint <- "Mid-pregnancy (adjusted for early pregnancy exposure)"
obs.res_uPDI_ord_3

obs.res_uPDI_ord_4 <- read.xlsx("Viva/IMP_BF_obs.res_uPDI_ord.xlsx")
obs.res_uPDI_ord_4 <- subset(obs.res_uPDI_ord_4,
                             Model == "Model 3",
                             select = c("Outcome", "b", "se", "pval"))
obs.res_uPDI_ord_4$Timepoint <- "During pregnancy (mean intakes across timepoints)"
obs.res_uPDI_ord_4

obs.res_uPDI_ord <- rbind(
  subset(meta.res_uPDI_ord, Outcome %in% obs.res_uPDI_ord_1$Outcome),
  obs.res_uPDI_ord_1,
  obs.res_uPDI_ord_2,
  obs.res_uPDI_ord_3,
  obs.res_uPDI_ord_4
)

obs.res_uPDI_ord[, c("b", "se", "pval")] <-
  sapply(obs.res_uPDI_ord[, c("b", "se", "pval")], as.numeric)

obs.res_uPDI_ord$Timepoint <- factor(
  obs.res_uPDI_ord$Timepoint,
  levels = c(
    "Mid-pregnancy (adjusted for early pregnancy exposure)",
    "Mid-pregnancy",
    "Early pregnancy",
    "During pregnancy (mean intakes across timepoints)",
    "Main results (from all 3 cohorts)"
  ),
  ordered = T
)

obs.res_uPDI_ord$Group <- "Ordinal outcome"

obs.res_uPDI_ord
str(obs.res_uPDI_ord)

################################################################################

## Nightingale forestplots
obs.res_uPDI <- rbind(obs.res_uPDI_bin, obs.res_uPDI_con, obs.res_uPDI_ord)

obs.res_uPDI$Group <- factor(
  obs.res_uPDI$Group,
  levels = c("Binary outcome", "Continuous outcome", "Ordinal outcome"),
  ordered = T
)

meta.forest_uPDI <- ggforestplot::forestplot(
  df = obs.res_uPDI,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = Timepoint,
  shape = Timepoint,
  xlab = "logOR or beta and 95% CI",
  title = "Unhealthful plant-based diet index (uPDI)",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c(
    "mediumblue",
    "darkorange",
    "blueviolet",
    "darkred",
    "darkcyan"
  )) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space  = "free")

meta.forest_uPDI

ggsave(meta.forest_uPDI,
       file = "Viva/compare.timepoint_obs.forest_uPDI.png",
       height = 16,
       width = 11)

################################################################################
