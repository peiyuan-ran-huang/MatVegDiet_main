################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 07-May-2025
# This script is to perform meta-analysis on (negative control outcome ???) analysis on breastfeeding (with imputed data) results for vegetarian diets (in subgroups).
# Part 1: For Supplementary Figures (separately for pesco- and full vegetarianism)

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

# Load and combine observational results - 4-category breastfeeding duration as ordinal outcome

## ALSPAC
obs.res_VegDiet_ord_ALSPAC <-
  read.xlsx("ALSPAC/IMP_BF_obs.res_VegDiet.subgroup_ord.xlsx")
obs.res_VegDiet_ord_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_ALSPAC
str(obs.res_VegDiet_ord_ALSPAC)  # 6 results (1 outcome * 2 categories* 3 models)

## BiB
obs.res_VegDiet_ord_BiB <-
  read.xlsx("BiB/IMP_BF_obs.res_VegDiet.subgroup_ord.xlsx")
obs.res_VegDiet_ord_BiB[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_BiB[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_BiB
str(obs.res_VegDiet_ord_BiB)  # 6 results (1 outcome * 2 categories * 3 models)

## MoBa
obs.res_VegDiet_ord_MoBa <-
  read.xlsx("MoBa/IMP_BF_obs.res_VegDiet.subgroup_ord.xlsx")
obs.res_VegDiet_ord_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_MoBa
str(obs.res_VegDiet_ord_MoBa)  # 6 results (1 outcome * 2 categories * 3 models)

## Project Viva
obs.res_VegDiet_ord_Viva <-
  read.xlsx("Viva/IMP_BF_obs.res_VegDiet.subgroup_ord.xlsx")
obs.res_VegDiet_ord_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_Viva
str(obs.res_VegDiet_ord_Viva)  # 6 results (1 outcome * 2 categories * 3 models)

################################################################################

## Combine
obs.res_VegDiet_ord_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_ord_BiB$Study <- "BiB"
obs.res_VegDiet_ord_MoBa$Study <- "MoBa"
obs.res_VegDiet_ord_Viva$Study <- "Project Viva"

obs.res_VegDiet_ord <-
  rbind(
    obs.res_VegDiet_ord_ALSPAC,
    obs.res_VegDiet_ord_BiB,
    obs.res_VegDiet_ord_MoBa,
    obs.res_VegDiet_ord_Viva
  )
obs.res_VegDiet_ord
str(obs.res_VegDiet_ord)  # 24 results = 6 + 6 + 6 + 6

## Load outcome lists and labels
MRPREG_outcome_labels <-
  read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

################################################################################

## Select and prepare results for meta-analysis
obs.res_VegDiet_ord <-
  subset(obs.res_VegDiet_ord, Model == "Model 3")

obs.res_VegDiet_ord$Group <- NA
obs.res_VegDiet_ord$Group[obs.res_VegDiet_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_VegDiet_ord$Group[obs.res_VegDiet_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_VegDiet_ord$Group[obs.res_VegDiet_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_VegDiet_ord$Group <-
  factor(
    obs.res_VegDiet_ord$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_VegDiet_ord$Outcome <-
  factor(obs.res_VegDiet_ord$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_ord$Outcome])
obs.res_VegDiet_ord <-
  obs.res_VegDiet_ord %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_VegDiet_ord$b <-
  as.numeric(obs.res_VegDiet_ord$b)
obs.res_VegDiet_ord$se <-
  as.numeric(obs.res_VegDiet_ord$se)
obs.res_VegDiet_ord$pval <-
  as.numeric(obs.res_VegDiet_ord$pval)
obs.res_VegDiet_ord$N_exp <-
  as.numeric(obs.res_VegDiet_ord$N_exp)
obs.res_VegDiet_ord$N_ref <-
  as.numeric(obs.res_VegDiet_ord$N_ref)

################################################################################
### Exposure labels shown in the forest plot
obs.res_VegDiet_ord$Exposure[obs.res_VegDiet_ord$Exposure == "Pesco-vegetarian"] <- "Pesco- vs. non-vegetarian"
obs.res_VegDiet_ord$Exposure[obs.res_VegDiet_ord$Exposure == "Full vegetarian"] <- "Full vs. non-vegetarian"
obs.res_VegDiet_ord$Exposure <- factor(
  obs.res_VegDiet_ord$Exposure,
  levels = c("Pesco- vs. non-vegetarian", "Full vs. non-vegetarian")
)
################################################################################

obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 8 obs.

################################################################################

# ## Separate results for full vegetarian and pesco-vegetarian
# obs.res_VegDiet_ord_full.V <- subset(obs.res_VegDiet_ord, Exposure == "Full vegetarian")
# obs.res_VegDiet_ord_pesco.V <- subset(obs.res_VegDiet_ord, Exposure == "Pesco-vegetarian")
#
# obs.res_VegDiet_ord_full.V
# obs.res_VegDiet_ord_pesco.V

################################################################################
## !!! Add total sample size columns for meta-analysed results (for final result tables) !!!
totals_VegDiet_ord <- obs.res_VegDiet_ord %>%
  group_by(Exposure) %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))

totals_VegDiet_ord
################################################################################

## Separate results for full vegetarian and pesco-vegetarian
obs.res_VegDiet_ord_full.V <- subset(obs.res_VegDiet_ord, Exposure == "Full vs. non-vegetarian")
obs.res_VegDiet_ord_pesco.V <- subset(obs.res_VegDiet_ord, Exposure == "Pesco- vs. non-vegetarian")

totals_VegDiet_ord_full.V <- subset(totals_VegDiet_ord, Exposure == "Full vs. non-vegetarian")
totals_VegDiet_ord_pesco.V <- subset(totals_VegDiet_ord, Exposure == "Pesco- vs. non-vegetarian")

#------------------------------------------------------------------------------#
#                                Meta-analysis                                 #----
#------------------------------------------------------------------------------#

# Full vegetarian

## Run meta-analysis
metagen_obs.res_VegDiet_ord_full.V <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_ord_full.V,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_ord_full.V

## Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_ord_full.V <-
  as.data.frame(
    cbind(
      metagen_obs.res_VegDiet_ord_full.V[["bylevs"]],
      "Outcome" = rep(
        unique(as.character(obs.res_VegDiet_ord_full.V$Outcome)),
        length(metagen_obs.res_VegDiet_ord_full.V[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_ord_full.V[["k.w"]],
      totals_VegDiet_ord_full.V$N_exp,
      totals_VegDiet_ord_full.V$N_ref,
      metagen_obs.res_VegDiet_ord_full.V[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_ord_full.V[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_ord_full.V[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_ord_full.V[["I2.w"]],
      metagen_obs.res_VegDiet_ord_full.V[["tau2.w"]],
      as.numeric(metagen_obs.res_VegDiet_ord_full.V[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_ord_full.V[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_ord_full.V[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_VegDiet_ord_full.V) <-
  c(
    "Exposure",
    "Outcome",
    "N of studies",
    "N exposed",
    "N reference",
    "Beta",
    "SE",
    "p-value",
    "I²",
    "tau²",
    "b",
    "se",
    "pval"
  )

metagen_obs.tbl_VegDiet_ord_full.V$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_full.V$Beta)
metagen_obs.tbl_VegDiet_ord_full.V$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_full.V$SE)
metagen_obs.tbl_VegDiet_ord_full.V$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_ord_full.V$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_ord_full.V$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_ord_full.V$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_ord_full.V$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_ord_full.V$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_ord_full.V$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_full.V$`N of studies`)

metagen_obs.tbl_VegDiet_ord_full.V$OR <-
  format(round(exp(
    metagen_obs.tbl_VegDiet_ord_full.V$Beta
  ), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_ord_full.V$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord_full.V$Beta - 1.96 * metagen_obs.tbl_VegDiet_ord_full.V$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord_full.V$Beta + 1.96 * metagen_obs.tbl_VegDiet_ord_full.V$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_ord_full.V <- metagen_obs.tbl_VegDiet_ord_full.V %>%
  select(
    Outcome,
    Exposure,
    `N of studies`,
    `N exposed`,
    `N reference`,
    OR,
    `95% CI`,
    `p-value`,
    `I²`,
    `tau²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_ord_full.V
str(metagen_obs.tbl_VegDiet_ord_full.V)  # 2 obs. = 1 outcome * 2 exposures

write.xlsx(
  metagen_obs.tbl_VegDiet_ord_full.V,
  "ALL/IMP_BF_meta.tbl_VegDiet.subgroup_ord_full.V.xlsx",
  rowNames = F
)

################################################################################
################################################################################

## Meta-analysis forest plot
metaforest_obs.res_VegDiet_ord_full.V <- forest(
  metagen_obs.res_VegDiet_ord_full.V,
  studlab = T,
  text.common = "Total",
  text.common.w = "Total",
  col.study = "black",
  col.square = "grey",
  col.diamond.lines = "black",
  col.label.right = "black",
  col.label.left = "black",
  col.by = "black",
  smlab = "OR [95% CI]",
  leftcols = c("studlab", "N_exp", "N_ref", "w.common"),
  leftlabs = c("Study", "N exposed", "N ref", "Weight"),
  rightcols = c("effect", "ci"),
  rightlabs = c("OR", "[95% CI]"),
  test.overall = F,
  lwd = 0.9,
  plotwidth = "6cm",
  colgap = "0.5cm",
  print.tau2 = F,
  print.Q = F,
  digits = 2,
  fontsize = 10,
  overall = F,
  overall.hetstat = F,
  subgroup = T,
  subgroup.hetstat = T,
  test.subgroup = F,
  print.I2 = T,
  print.I2.ci = F,
  print.pval.Q = T,
  digits.pval.Q = 3,
  digits.pval = 3
)

metaforest_obs.res_VegDiet_ord_full.V <- recordPlot()

png(
  "ALL/IMP_BF_meta.forest_VegDiet.subgroup_ord_full.V.png",
  res = 300,
  height = 800,
  width = 2200
)
print(metaforest_obs.res_VegDiet_ord_full.V)
dev.off()

################################################################################

# Pesco-vegetarian

## Run meta-analysis
metagen_obs.res_VegDiet_ord_pesco.V <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_ord_pesco.V,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_ord_pesco.V

## Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_ord_pesco.V <-
  as.data.frame(
    cbind(
      metagen_obs.res_VegDiet_ord_pesco.V[["bylevs"]],
      "Outcome" = rep(
        unique(as.character(obs.res_VegDiet_ord_pesco.V$Outcome)),
        length(metagen_obs.res_VegDiet_ord_pesco.V[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_ord_pesco.V[["k.w"]],
      totals_VegDiet_ord_pesco.V$N_exp,
      totals_VegDiet_ord_pesco.V$N_ref,
      metagen_obs.res_VegDiet_ord_pesco.V[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_ord_pesco.V[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_ord_pesco.V[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_ord_pesco.V[["I2.w"]],
      metagen_obs.res_VegDiet_ord_pesco.V[["tau2.w"]],
      as.numeric(metagen_obs.res_VegDiet_ord_pesco.V[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_ord_pesco.V[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_ord_pesco.V[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_VegDiet_ord_pesco.V) <-
  c(
    "Exposure",
    "Outcome",
    "N of studies",
    "N exposed",
    "N reference",
    "Beta",
    "SE",
    "p-value",
    "I²",
    "tau²",
    "b",
    "se",
    "pval"
  )

metagen_obs.tbl_VegDiet_ord_pesco.V$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_pesco.V$Beta)
metagen_obs.tbl_VegDiet_ord_pesco.V$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_pesco.V$SE)
metagen_obs.tbl_VegDiet_ord_pesco.V$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_ord_pesco.V$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_ord_pesco.V$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_ord_pesco.V$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_ord_pesco.V$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_ord_pesco.V$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_ord_pesco.V$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_pesco.V$`N of studies`)

metagen_obs.tbl_VegDiet_ord_pesco.V$OR <-
  format(round(exp(
    metagen_obs.tbl_VegDiet_ord_pesco.V$Beta
  ), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_ord_pesco.V$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord_pesco.V$Beta - 1.96 * metagen_obs.tbl_VegDiet_ord_pesco.V$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord_pesco.V$Beta + 1.96 * metagen_obs.tbl_VegDiet_ord_pesco.V$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_ord_pesco.V <- metagen_obs.tbl_VegDiet_ord_pesco.V %>%
  select(
    Outcome,
    Exposure,
    `N of studies`,
    `N exposed`,
    `N reference`,
    OR,
    `95% CI`,
    `p-value`,
    `I²`,
    `tau²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_ord_pesco.V
str(metagen_obs.tbl_VegDiet_ord_pesco.V)  # 2 obs. = 1 outcome * 2 exposures

write.xlsx(
  metagen_obs.tbl_VegDiet_ord_pesco.V,
  "ALL/IMP_BF_meta.tbl_VegDiet.subgroup_ord_pesco.V.xlsx",
  rowNames = F
)

################################################################################
################################################################################

## Meta-analysis forest plot
metaforest_obs.res_VegDiet_ord_pesco.V <- forest(
  metagen_obs.res_VegDiet_ord_pesco.V,
  studlab = T,
  text.common = "Total",
  text.common.w = "Total",
  col.study = "black",
  col.square = "grey",
  col.diamond.lines = "black",
  col.label.right = "black",
  col.label.left = "black",
  col.by = "black",
  smlab = "OR [95% CI]",
  leftcols = c("studlab", "N_exp", "N_ref", "w.common"),
  leftlabs = c("Study", "N exposed", "N ref", "Weight"),
  rightcols = c("effect", "ci"),
  rightlabs = c("OR", "[95% CI]"),
  test.overall = F,
  lwd = 0.9,
  plotwidth = "6cm",
  colgap = "0.5cm",
  print.tau2 = F,
  print.Q = F,
  digits = 2,
  fontsize = 10,
  overall = F,
  overall.hetstat = F,
  subgroup = T,
  subgroup.hetstat = T,
  test.subgroup = F,
  print.I2 = T,
  print.I2.ci = F,
  print.pval.Q = T,
  digits.pval.Q = 3,
  digits.pval = 3
)

metaforest_obs.res_VegDiet_ord_pesco.V <- recordPlot()

png(
  "ALL/IMP_BF_meta.forest_VegDiet.subgroup_ord_pesco.V.png",
  res = 300,
  height = 800,
  width = 2200
)
print(metaforest_obs.res_VegDiet_ord_pesco.V)
dev.off()
