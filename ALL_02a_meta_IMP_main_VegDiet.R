################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 16-Jun-2025
# This script is to perform meta-analysis on main association analysis (with imputed data) results for vegetarian diets.

## Update on 16-Jun-2025: Add post-term birth and breastfeeding duration as primary outcomes.

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

# Load and combine observational results

## ALSPAC
obs.res_VegDiet_bin_ALSPAC <-
  read.xlsx("ALSPAC/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_ALSPAC
str(obs.res_VegDiet_bin_ALSPAC)  # 63 results (21 outcomes * 1 category * 3 models)

obs.res_VegDiet_con_ALSPAC <-
  read.xlsx("ALSPAC/IMP_MAIN_obs.res_VegDiet_con.xlsx")
obs.res_VegDiet_con_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_ALSPAC
str(obs.res_VegDiet_con_ALSPAC)  # 12 results (4 outcomes * 1 category * 3 models)

## BiB
obs.res_VegDiet_bin_BiB <-
  read.xlsx("BiB/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_BiB[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_BiB[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_BiB
str(obs.res_VegDiet_bin_BiB)  # 57 results (19 outcomes * 1 category * 3 models)

obs.res_VegDiet_con_BiB <-
  read.xlsx("BiB/IMP_MAIN_obs.res_VegDiet_con.xlsx")
obs.res_VegDiet_con_BiB[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_BiB[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_BiB
str(obs.res_VegDiet_con_BiB)  # 12 results (4 outcomes * 1 category * 3 models)

## MoBa
obs.res_VegDiet_bin_MoBa <-
  read.xlsx("MoBa/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_MoBa
str(obs.res_VegDiet_bin_MoBa)  # 66 results (22 outcomes * 1 category * 3 models)

obs.res_VegDiet_con_MoBa <-
  read.xlsx("MoBa/IMP_MAIN_obs.res_VegDiet_con.xlsx")
obs.res_VegDiet_con_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_MoBa
str(obs.res_VegDiet_con_MoBa)  # 12 results (4 outcomes * 1 category * 3 models)

## Project Viva
obs.res_VegDiet_bin_Viva <-
  read.xlsx("Viva/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_Viva
str(obs.res_VegDiet_bin_Viva)  # 51 results (17 outcomes * 1 category * 3 models)

obs.res_VegDiet_con_Viva <-
  read.xlsx("Viva/IMP_MAIN_obs.res_VegDiet_con.xlsx")
obs.res_VegDiet_con_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_Viva
str(obs.res_VegDiet_con_Viva)  # 6 results (2 outcomes * 1 category * 3 models)

################################################################################

## Combine

### Binary outcomes
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
str(obs.res_VegDiet_bin)  # 237 results = 63 + 57 + 66 + 51

### Continuous outcomes
obs.res_VegDiet_con_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_con_BiB$Study <- "BiB"
obs.res_VegDiet_con_MoBa$Study <- "MoBa"
obs.res_VegDiet_con_Viva$Study <- "Project Viva"

obs.res_VegDiet_con <-
  rbind(
    obs.res_VegDiet_con_ALSPAC,
    obs.res_VegDiet_con_BiB,
    obs.res_VegDiet_con_MoBa,
    obs.res_VegDiet_con_Viva
  )
obs.res_VegDiet_con
str(obs.res_VegDiet_con)  # 42 results = 12 + 12 + 12 + 6

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
ALL_primary_bin <- subset(primary_bin, label %in% obs.res_VegDiet_bin$Outcome)
ALL_primary_bin
dim(ALL_primary_bin)  # 13 primary (binary) outcomes available in the combined results

ALL_secondary_bin <- subset(
  secondary_bin,
  label %in% obs.res_VegDiet_bin$Outcome &
    varname != "anaemia_preg_subsamp"
)  # Maternal anaemia (occurring during pregnancy) is only available for sensitivity analysis in MoBa
ALL_secondary_bin
dim(ALL_secondary_bin)  # 8 secondary binary outcomes available in the combined results

ALL_secondary_con <- subset(secondary_con, label %in% obs.res_VegDiet_con$Outcome)
ALL_secondary_con
dim(ALL_secondary_con)  # 4 secondary continuous outcomes available in the combined results

################################################################################

## Select and prepare results for meta-analysis

### Binary outcomes
obs.res_VegDiet_bin <-
  subset(obs.res_VegDiet_bin, Model == "Model 3")

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
dim(obs.res_VegDiet_bin)  # 237 / 3 = 79 obs.

################################################################################
#### Exclude outcomes with only one study - e.g., "anaemia_preg_subsamp"
obs.res_VegDiet_bin <- obs.res_VegDiet_bin %>%
  group_by(Outcome) %>%
  filter(n() >= 2) %>%
  ungroup()
################################################################################
#### Remove some results with extremely huge 95% CIs
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin[which(obs.res_VegDiet_bin$se < 90), ]
################################################################################
#### Exposure labels shown in the forest plot
obs.res_VegDiet_bin$Exposure[obs.res_VegDiet_bin$Exposure == "Pesco-/full vegetarian"] <- "Pesco-/full vs. non-vegetarian"
################################################################################

obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 79 -> 72 obs.

################################################################################
################################################################################

### Continuous outcomes
obs.res_VegDiet_con <-
  subset(obs.res_VegDiet_con, Model == "Model 3")

obs.res_VegDiet_con$Group <- NA
obs.res_VegDiet_con$Group[obs.res_VegDiet_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_VegDiet_con$Group[obs.res_VegDiet_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_VegDiet_con$Group[obs.res_VegDiet_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_VegDiet_con$Group <-
  factor(
    obs.res_VegDiet_con$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_VegDiet_con$Outcome <-
  factor(obs.res_VegDiet_con$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_con$Outcome])
obs.res_VegDiet_con <-
  obs.res_VegDiet_con %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_VegDiet_con$b <-
  as.numeric(obs.res_VegDiet_con$b)
obs.res_VegDiet_con$se <-
  as.numeric(obs.res_VegDiet_con$se)
obs.res_VegDiet_con$pval <-
  as.numeric(obs.res_VegDiet_con$pval)
obs.res_VegDiet_con$N_exp <-
  as.numeric(obs.res_VegDiet_con$N_exp)
obs.res_VegDiet_con$N_ref <-
  as.numeric(obs.res_VegDiet_con$N_ref)

################################################################################
#### Exposure labels shown in the forest plot
obs.res_VegDiet_con$Exposure[obs.res_VegDiet_con$Exposure == "Pesco-/full vegetarian"] <- "Pesco-/full vs. non-vegetarian"
################################################################################

obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 14 obs.

################################################################################
## !!! Add total sample size columns for meta-analysed results (for final result tables) !!!

### Binary outcomes
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
  )
totals_VegDiet_bin <- obs.res_VegDiet_bin %>%
  group_by(Outcome) %>%
  summarise(
    case_exp_total = sum(case_exp, na.rm = T),
    total_exp_total = sum(total_exp, na.rm = T),
    case_ref_total = sum(case_ref, na.rm = T),
    total_ref_total = sum(total_ref, na.rm = T)
  ) %>%
  mutate(
    N_exp = paste(case_exp_total, " / ", total_exp_total, sep = ""),
    N_ref = paste(case_ref_total, " / ", total_ref_total, sep = "")
  ) %>%
  select(Outcome, N_exp, N_ref)

totals_VegDiet_bin

### Continuous outcomes
totals_VegDiet_con <- obs.res_VegDiet_con %>%
  group_by(Outcome) %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))

totals_VegDiet_con
################################################################################

#------------------------------------------------------------------------------#
#                                Meta-analysis                                 #----
#------------------------------------------------------------------------------#

# Binary outcomes

## Run meta-analysis
metagen_obs.res_VegDiet_bin <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_bin,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_bin

## Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_bin <-
  as.data.frame(
    cbind(
      "Exposure" = rep(unique(
        as.character(obs.res_VegDiet_bin$Exposure)
      ), length(metagen_obs.res_VegDiet_bin[["bylevs"]])),
      metagen_obs.res_VegDiet_bin[["bylevs"]],
      metagen_obs.res_VegDiet_bin[["k.w"]],
      totals_VegDiet_bin$N_exp,
      totals_VegDiet_bin$N_ref,
      metagen_obs.res_VegDiet_bin[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_bin[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_bin[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_bin[["I2.w"]],
      metagen_obs.res_VegDiet_bin[["tau2.w"]],
      as.numeric(metagen_obs.res_VegDiet_bin[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_bin[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_bin[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_VegDiet_bin) <-
  c(
    "Exposure",
    "Outcome",
    "N of studies",
    "Cases/total in exposed",
    "Cases/total in reference",
    "Beta",
    "SE",
    "p-value",
    "I²",
    "τ²",
    "b",
    "se",
    "pval"
  )

metagen_obs.tbl_VegDiet_bin$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_bin$Beta)
metagen_obs.tbl_VegDiet_bin$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_bin$SE)
metagen_obs.tbl_VegDiet_bin$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_bin$`p-value`), digits = 3)
metagen_obs.tbl_VegDiet_bin$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_bin$`I²`) * 100, digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_bin$`τ²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_bin$`τ²`))
################################################################################
metagen_obs.tbl_VegDiet_bin$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_bin$`N of studies`)

metagen_obs.tbl_VegDiet_bin$OR <-
  format(round(exp(metagen_obs.tbl_VegDiet_bin$Beta), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_bin$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin$Beta - 1.96 * metagen_obs.tbl_VegDiet_bin$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin$Beta + 1.96 * metagen_obs.tbl_VegDiet_bin$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_bin <- metagen_obs.tbl_VegDiet_bin %>%
  select(
    Outcome,
    Exposure,
    `N of studies`,
    `Cases/total in exposed`,
    `Cases/total in reference`,
    OR,
    `95% CI`,
    `p-value`,
    `I²`,
    `τ²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_bin
str(metagen_obs.tbl_VegDiet_bin)  # 21 obs. = 21 outcomes * 1 exposure

write.xlsx(metagen_obs.tbl_VegDiet_bin,
           "ALL/IMP_MAIN_meta.tbl_VegDiet_bin.xlsx",
           rowNames = F)

################################################################################
################################################################################

## Meta-analysis forest plot for ALL binary outcomes
metaforest_obs.res_VegDiet_bin <- forest(
  metagen_obs.res_VegDiet_bin,
  studlab = T,
  text.common = "Total",
  text.common.w = "Total",
  col.study = "black",
  col.square = "grey",
  col.diamond = "white",
  col.diamond.lines = "black",
  col.label.right = "black",
  col.label.left = "black",
  col.by = "black",
  smlab = "OR [95% CI]",
  leftcols = c("studlab", "N_exp", "N_ref", "w.common"),
  leftlabs = c("Study", "Cases/total in exposed", "Cases/total in ref", "Weight"),
  rightcols = c("effect", "ci"),
  rightlabs = c("OR", "[95% CI]"),
  test.overall = F,
  lwd = 0.9,
  plotwidth = "6cm",
  colgap = "0.5cm",
  print.tau2 = F,
  print.Q = T,
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

metaforest_obs.res_VegDiet_bin <- recordPlot()

png(
  "ALL/IMP_MAIN_meta.forest_VegDiet_bin.png",
  res = 300,
  height = 9550,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin)
dev.off()

################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for pregnancy outcomes
metaforest_obs.res_VegDiet_bin_Preg <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin, Group == "Pregnancy outcome"),
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  ),
  studlab = T,
  text.common = "Total",
  text.common.w = "Total",
  col.study = "black",
  col.square = "grey",
  col.diamond = "white",
  col.diamond.lines = "black",
  col.label.right = "black",
  col.label.left = "black",
  col.by = "black",
  smlab = "OR [95% CI]",
  leftcols = c("studlab", "N_exp", "N_ref", "w.common"),
  leftlabs = c("Study", "Cases/total in exposed", "Cases/total in ref", "Weight"),
  rightcols = c("effect", "ci"),
  rightlabs = c("OR", "[95% CI]"),
  test.overall = F,
  lwd = 0.9,
  plotwidth = "6cm",
  colgap = "0.5cm",
  print.tau2 = F,
  print.Q = T,
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
metaforest_obs.res_VegDiet_bin_Preg <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_VegDiet_bin_Preg.png",
  res = 300,
  height = 3010,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Preg)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for delivery outcomes
metaforest_obs.res_VegDiet_bin_Deli <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin, Group == "Delivery outcome"),
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  ),
  studlab = T,
  text.common = "Total",
  text.common.w = "Total",
  col.study = "black",
  col.square = "grey",
  col.diamond = "white",
  col.diamond.lines = "black",
  col.label.right = "black",
  col.label.left = "black",
  col.by = "black",
  smlab = "OR [95% CI]",
  leftcols = c("studlab", "N_exp", "N_ref", "w.common"),
  leftlabs = c("Study", "Cases/total in exposed", "Cases/total in ref", "Weight"),
  rightcols = c("effect", "ci"),
  rightlabs = c("OR", "[95% CI]"),
  test.overall = F,
  lwd = 0.9,
  plotwidth = "6cm",
  colgap = "0.5cm",
  print.tau2 = F,
  print.Q = T,
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
metaforest_obs.res_VegDiet_bin_Deli <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_VegDiet_bin_Deli.png",
  res = 300,
  height = 5550,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Deli)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for postnatal outcomes
metaforest_obs.res_VegDiet_bin_Post <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin, Group == "Postnatal outcome"),
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  ),
  studlab = T,
  text.common = "Total",
  text.common.w = "Total",
  col.study = "black",
  col.square = "grey",
  col.diamond = "white",
  col.diamond.lines = "black",
  col.label.right = "black",
  col.label.left = "black",
  col.by = "black",
  smlab = "OR [95% CI]",
  leftcols = c("studlab", "N_exp", "N_ref", "w.common"),
  leftlabs = c("Study", "Cases/total in exposed", "Cases/total in ref", "Weight"),
  rightcols = c("effect", "ci"),
  rightlabs = c("OR", "[95% CI]"),
  test.overall = F,
  lwd = 0.9,
  plotwidth = "6cm",
  colgap = "0.5cm",
  print.tau2 = F,
  print.Q = T,
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
metaforest_obs.res_VegDiet_bin_Post <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_VegDiet_bin_Post.png",
  res = 300,
  height = 1450,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Post)
dev.off()

################################################################################

# Continuous outcomes

## Run meta-analysis
metagen_obs.res_VegDiet_con <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_con,
    sm = "",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_con

## Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_con <-
  as.data.frame(
    cbind(
      "Exposure" = rep(unique(
        as.character(obs.res_VegDiet_con$Exposure)
      ), length(metagen_obs.res_VegDiet_con[["bylevs"]])),
      metagen_obs.res_VegDiet_con[["bylevs"]],
      metagen_obs.res_VegDiet_con[["k.w"]],
      totals_VegDiet_con$N_exp,
      totals_VegDiet_con$N_ref,
      metagen_obs.res_VegDiet_con[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_con[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_con[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_con[["I2.w"]],
      metagen_obs.res_VegDiet_con[["tau2.w"]],
      as.numeric(metagen_obs.res_VegDiet_con[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_con[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_con[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_VegDiet_con) <-
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
    "τ²",
    "b",
    "se",
    "pval"
  )

metagen_obs.tbl_VegDiet_con$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_con$Beta)
metagen_obs.tbl_VegDiet_con$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_con$SE)
metagen_obs.tbl_VegDiet_con$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_con$`p-value`), digits = 3)
metagen_obs.tbl_VegDiet_con$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_con$`I²`) * 100, digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_con$`τ²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_con$`τ²`))
################################################################################
metagen_obs.tbl_VegDiet_con$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_con$`N of studies`)

metagen_obs.tbl_VegDiet_con$`95% CI` <- paste0(format(
  round(
    metagen_obs.tbl_VegDiet_con$Beta - 1.96 * metagen_obs.tbl_VegDiet_con$SE,
    digits = 2
  ),
  nsmall = 2
), ", ", format(
  round(
    metagen_obs.tbl_VegDiet_con$Beta + 1.96 * metagen_obs.tbl_VegDiet_con$SE,
    digits = 2
  ),
  nsmall = 2
))
metagen_obs.tbl_VegDiet_con$Beta <-
  format(round(metagen_obs.tbl_VegDiet_con$Beta, digits = 2), nsmall = 2)

metagen_obs.tbl_VegDiet_con <- metagen_obs.tbl_VegDiet_con %>%
  select(
    Outcome,
    Exposure,
    `N of studies`,
    `N exposed`,
    `N reference`,
    Beta,
    `95% CI`,
    `p-value`,
    `I²`,
    `τ²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_con
str(metagen_obs.tbl_VegDiet_con)  # 4 obs. = 4 outcomes * 1 exposure

write.xlsx(metagen_obs.tbl_VegDiet_con,
           "ALL/IMP_MAIN_meta.tbl_VegDiet_con.xlsx",
           rowNames = F)

################################################################################
################################################################################

## Meta-analysis forest plot
metaforest_obs.res_VegDiet_con <- forest(
  metagen_obs.res_VegDiet_con,
  studlab = T,
  text.common = "Total",
  text.common.w = "Total",
  col.study = "black",
  col.square = "grey",
  col.diamond = "white",
  col.diamond.lines = "black",
  col.label.right = "black",
  col.label.left = "black",
  col.by = "black",
  smlab = "Beta [95% CI]",
  leftcols = c("studlab", "N_exp", "N_ref", "w.common"),
  leftlabs = c("Study", "N exposed", "N ref", "Weight"),
  rightcols = c("effect", "ci"),
  rightlabs = c("Beta", "[95% CI]"),
  test.overall = F,
  lwd = 0.9,
  plotwidth = "6cm",
  colgap = "0.5cm",
  print.tau2 = F,
  print.Q = T,
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

metaforest_obs.res_VegDiet_con <- recordPlot()

png(
  "ALL/IMP_MAIN_meta.forest_VegDiet_con.png",
  res = 300,
  height = 2000,
  width = 2600
)
print(metaforest_obs.res_VegDiet_con)
dev.off()

#------------------------------------------------------------------------------#
#                                Combine Plots                                 #----
#------------------------------------------------------------------------------#

# Finalise result presentation

## FDR correction for multiple testing
pval_bin <-
  as.data.frame(
    cbind(
      "Exposure" = rep(unique(
        as.character(obs.res_VegDiet_bin$Exposure)
      ), length(metagen_obs.res_VegDiet_bin[["bylevs"]])),
      "Outcome" = metagen_obs.res_VegDiet_bin[["bylevs"]],
      "pval" = as.numeric(metagen_obs.res_VegDiet_bin[["pval.fixed.w"]])
    )
  )

pval_con <-
  as.data.frame(
    cbind(
      "Exposure" = rep(unique(
        as.character(obs.res_VegDiet_con$Exposure)
      ), length(metagen_obs.res_VegDiet_con[["bylevs"]])),
      "Outcome" = metagen_obs.res_VegDiet_con[["bylevs"]],
      "pval" = as.numeric(metagen_obs.res_VegDiet_con[["pval.fixed.w"]])
    )
  )

pval_all <- rbind(pval_bin, pval_con)

################################################################################
### Add p-value for breastfeeding
try(pval_ord <- read.xlsx("Z:/working/results/ALL/IMP_BF_meta.tbl_VegDiet_ord.xlsx") %>%
      select(Outcome, pval) %>%
      mutate(Exposure = "Pesco-/full vs. non-vegetarian") %>%
      select(Exposure, Outcome, pval))

try(pval_all <- rbind(pval_all, pval_ord))
################################################################################
### Only apply FDR correction for primary outcomes - !!! Updated on 01-Sep-2025 !!!
pval_all <- subset(
  pval_all,
  Outcome %in% c(
    ALL_primary_bin$label,
    "Post-term birth",
    "Breastfeeding duration"
  )
)  # Update on 16-Jun-2025: Added post-term birth and breastfeeding duration as primary outcomes.
################################################################################

pval_all$`Adjusted p-value` <- style_pvalue(as.numeric(p.adjust(pval_all$pval, method = "fdr")), digits = 3)
pval_all <- pval_all %>%
  select(Outcome, Exposure, `Adjusted p-value`)
pval_all  # FDR-adjusted p-value for breastfeeding duration: 0.247

## Combine results with FDR-adjusted p-values

### Binary outcomes
metagen_obs.tbl_VegDiet_bin <- left_join(metagen_obs.tbl_VegDiet_bin,
                                         pval_all,
                                         by = c("Outcome", "Exposure"))
metagen_obs.tbl_VegDiet_bin <- metagen_obs.tbl_VegDiet_bin %>%
  relocate(c(`I²`, `τ²`, `b`, `se`, `pval`), .after = last_col())
metagen_obs.tbl_VegDiet_bin
str(metagen_obs.tbl_VegDiet_bin)
write.xlsx(metagen_obs.tbl_VegDiet_bin,
           "ALL/IMP_MAIN_meta.tbl_VegDiet_bin.xlsx",
           rowNames = F)

### Continuous outcomes
metagen_obs.tbl_VegDiet_con <- left_join(metagen_obs.tbl_VegDiet_con,
                                         pval_all,
                                         by = c("Outcome", "Exposure"))
metagen_obs.tbl_VegDiet_con <- metagen_obs.tbl_VegDiet_con %>%
  relocate(c(`I²`, `τ²`, `b`, `se`, `pval`), .after = last_col())
metagen_obs.tbl_VegDiet_con
str(metagen_obs.tbl_VegDiet_con)
write.xlsx(metagen_obs.tbl_VegDiet_con,
           "ALL/IMP_MAIN_meta.tbl_VegDiet_con.xlsx",
           rowNames = F)

################################################################################
### Ordinal outcomes (from scripts "ALL_03")
try(metagen_obs.tbl_VegDiet_ord <- read.xlsx("Z:/working/results/ALL/IMP_BF_meta.tbl_VegDiet_ord.xlsx"))
try(metagen_obs.tbl_VegDiet_ord$Exposure <- "Pesco-/full vs. non-vegetarian")
try(colnames(metagen_obs.tbl_VegDiet_ord)[colnames(metagen_obs.tbl_VegDiet_ord) ==
                                            "N.of.studies"] <- "N of studies")
try(metagen_obs.tbl_VegDiet_ord$N.exposed <- "201 / 49 / 131 / 737")
try(colnames(metagen_obs.tbl_VegDiet_ord)[colnames(metagen_obs.tbl_VegDiet_ord) ==
                                            "N.exposed"] <- "Cases/total in exposed")
try(metagen_obs.tbl_VegDiet_ord$N.reference <- "16720 / 4737 / 7831 / 53975")
try(colnames(metagen_obs.tbl_VegDiet_ord)[colnames(metagen_obs.tbl_VegDiet_ord) ==
                                            "N.reference"] <- "Cases/total in reference")
try(colnames(metagen_obs.tbl_VegDiet_ord)[colnames(metagen_obs.tbl_VegDiet_ord) ==
                                            "95%.CI"] <- "95% CI")
try(colnames(metagen_obs.tbl_VegDiet_ord)[colnames(metagen_obs.tbl_VegDiet_ord) ==
                                            "tau²"] <- "τ²")
try(metagen_obs.tbl_VegDiet_ord <- left_join(metagen_obs.tbl_VegDiet_ord,
                                             pval_all,
                                             by = c("Outcome", "Exposure")))
try(metagen_obs.tbl_VegDiet_ord <- metagen_obs.tbl_VegDiet_ord %>%
      relocate(c(`I²`, `τ²`, `b`, `se`, `pval`), .after = last_col()))
try(metagen_obs.tbl_VegDiet_ord)
try(str(metagen_obs.tbl_VegDiet_ord))
try(write.xlsx(
  metagen_obs.tbl_VegDiet_ord,
  "ALL/IMP_MAIN_meta.tbl_VegDiet_ord_FINAL.xlsx",
  rowNames = F
))
################################################################################

################################################################################
################################################################################

## Main result forest plot with texts

### Combine binary and ordinal results
try(metagen_obs.tbl_VegDiet_bin <- rbind(metagen_obs.tbl_VegDiet_bin, metagen_obs.tbl_VegDiet_ord))
try(metagen_obs.tbl_VegDiet_bin)

### Select results for primary outcomes
metagen_obs.tbl_VegDiet_main <- subset(
  metagen_obs.tbl_VegDiet_bin,
  Outcome %in% c(
    ALL_primary_bin$label,
    "Post-term birth",
    "Breastfeeding duration"
  )
)  # Update on 16-Jun-2025: Added post-term birth and breastfeeding duration as primary outcomes.

metagen_obs.tbl_VegDiet_main

################################################################################
#### Change format of 95% CI
metagen_obs.tbl_VegDiet_main$`95% CI` <- gsub(",", " to", metagen_obs.tbl_VegDiet_main$`95% CI`)
################################################################################

metagen_obs.tbl_VegDiet_main$Outcome <-
  factor(metagen_obs.tbl_VegDiet_main$Outcome, levels =
           c(unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% metagen_obs.tbl_VegDiet_main$Outcome]))

metagen_obs.tbl_VegDiet_main <-
  metagen_obs.tbl_VegDiet_main %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

metagen_obs.tbl_VegDiet_main
str(metagen_obs.tbl_VegDiet_main)

## Make forest plot with texts

## Add beta and se values for the forest plot
metagen_obs.tbl_VegDiet_main$b <- as.numeric(metagen_obs.tbl_VegDiet_main$b)
metagen_obs.tbl_VegDiet_main$se <- as.numeric(metagen_obs.tbl_VegDiet_main$se)
metagen_obs.tbl_VegDiet_main$pval <- as.numeric(metagen_obs.tbl_VegDiet_main$pval)

## Prepare data
### Update on 04-Aug-2025: Remove the tau^2 column from the final plot

outcomes <- metagen_obs.tbl_VegDiet_main$Outcome

mydata <- data.frame(
  Outcome = outcomes,
  N_studies = metagen_obs.tbl_VegDiet_main$`N of studies`,
  N_exp = metagen_obs.tbl_VegDiet_main$`Cases/total in exposed`,
  N_ref = metagen_obs.tbl_VegDiet_main$`Cases/total in reference`,
  OR = metagen_obs.tbl_VegDiet_main$OR,
  CI = metagen_obs.tbl_VegDiet_main$`95% CI`,
  pval = metagen_obs.tbl_VegDiet_main$`p-value`,
  a.pval = metagen_obs.tbl_VegDiet_main$`Adjusted p-value`,
  I2 = metagen_obs.tbl_VegDiet_main$`I²`,
  b = metagen_obs.tbl_VegDiet_main$b,
  se = metagen_obs.tbl_VegDiet_main$se
)
mydata

tabletext <- rbind(
  c(
    "Outcome",
    "N of studies",
    "N of cases/total in",
    "N of cases/total in",
    "OR",
    "95% CI",
    "p-value",
    "Adjusted p-value",
    "I²"
  ),
  c(
    "",
    "",
    "pesco-/full vegetarians",
    "non-vegetarians",
    "",
    "",
    "",
    "",
    ""
  ),
  cbind(
    as.character(mydata[, "Outcome"]),
    as.character(mydata[, "N_studies"]),
    as.character(mydata[, "N_exp"]),
    as.character(mydata[, "N_ref"]),
    as.character(mydata[, "OR"]),
    as.character(mydata[, "CI"]),
    as.character(mydata[, "pval"]),
    as.character(mydata[, "a.pval"]),
    as.character(mydata[, "I2"])
  )
)
tabletext

hrzl_lines <- list()
hrzl_lines[["3"]] <- gpar(lty = 1,
                          lwd = 2,
                          col = "black")
for (i in seq(4, 17)) {
  hrzl_lines[[as.character(i)]] <- gpar(lty = 1,
                                        lwd = 1,
                                        col = "grey")
}

## Create forest plot with texts
png(
  "ALL/Comb_IMP_MAIN_meta.tbl.forest_VegDiet_Prim.Out.png",
  height = 650,
  width = 1900
)

forestplot(
  tabletext,
  graph.pos = 5,
  mean = rbind(NA, NA, cbind(exp(mydata$b))),
  lower = rbind(NA, NA, cbind(exp(
    mydata$b - 1.96 * mydata$se
  ))),
  upper = rbind(NA, NA, cbind(exp(
    mydata$b + 1.96 * mydata$se
  ))),
  new_page = F,
  xlog = T,
  xticks = c(log(0.50), log(0.75), log(1.0), log(1.5), log(2.0), log(2.5)),
  lwd.xaxis = 2,
  hrzl_lines = hrzl_lines,
  txt_gp = fpTxtGp(
    label = gpar(cex = 1.5),
    ticks = gpar(cex = 1.7),
    xlab = gpar(cex = 1.8)
  ),
  boxsize = 0.15,
  line.margin = 0.5,
  lty.ci = 1,
  col = fpColors(box = "black", line = "gray30"),
  lwd.ci = 2,
  ci.vertices = T,
  ci.vertices.height = 0.1,
  graphwidth = unit(120, "mm"),
  is.summary = c(T, T, rep(F, nrow(mydata))),
  colgap = unit (10, "mm"),
  zero = 1.00,
  lwd.zero = 2,
  xlab = "OR and 95% CI"
)

dev.off()
