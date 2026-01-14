################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 15-Oct-2024
# This script is to perform meta-analysis on main association analysis (with imputed data) results for vegetarian diets (in subgroups).

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
  read.xlsx("ALSPAC/IMP_MAIN_obs.res_VegDiet.subgroup_bin.xlsx")
obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_ALSPAC
str(obs.res_VegDiet_bin_ALSPAC)  # 126 results (21 outcomes * 2 category * 3 models)

obs.res_VegDiet_con_ALSPAC <-
  read.xlsx("ALSPAC/IMP_MAIN_obs.res_VegDiet.subgroup_con.xlsx")
obs.res_VegDiet_con_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_ALSPAC
str(obs.res_VegDiet_con_ALSPAC)  # 24 results (4 outcomes * 2 category * 3 models)

## BiB
obs.res_VegDiet_bin_BiB <-
  read.xlsx("BiB/IMP_MAIN_obs.res_VegDiet.subgroup_bin.xlsx")
obs.res_VegDiet_bin_BiB[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_BiB[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_BiB
str(obs.res_VegDiet_bin_BiB)  # 114 results (19 outcomes * 2 category * 3 models)

obs.res_VegDiet_con_BiB <-
  read.xlsx("BiB/IMP_MAIN_obs.res_VegDiet.subgroup_con.xlsx")
obs.res_VegDiet_con_BiB[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_BiB[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_BiB
str(obs.res_VegDiet_con_BiB)  # 24 results (4 outcomes * 2 category * 3 models)

## MoBa
obs.res_VegDiet_bin_MoBa <-
  read.xlsx("MoBa/IMP_MAIN_obs.res_VegDiet.subgroup_bin.xlsx")
obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_MoBa
str(obs.res_VegDiet_bin_MoBa)  # 126 results (21 outcomes * 2 category * 3 models)

obs.res_VegDiet_con_MoBa <-
  read.xlsx("MoBa/IMP_MAIN_obs.res_VegDiet.subgroup_con.xlsx")
obs.res_VegDiet_con_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_MoBa
str(obs.res_VegDiet_con_MoBa)  # 24 results (4 outcomes * 2 category * 3 models)

## Project Viva
obs.res_VegDiet_bin_Viva <-
  read.xlsx("Viva/IMP_MAIN_obs.res_VegDiet.subgroup_bin.xlsx")
obs.res_VegDiet_bin_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_Viva
str(obs.res_VegDiet_bin_Viva)  # 102 results (17 outcomes * 2 category * 3 models)

obs.res_VegDiet_con_Viva <-
  read.xlsx("Viva/IMP_MAIN_obs.res_VegDiet.subgroup_con.xlsx")
obs.res_VegDiet_con_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_Viva
str(obs.res_VegDiet_con_Viva)  # 12 results (2 outcomes * 2 category * 3 models)

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
str(obs.res_VegDiet_bin)  # 468 results = 126 + 114 + 126 + 102

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
str(obs.res_VegDiet_con)  # 84 results = 24 + 24 + 24 + 12

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
dim(obs.res_VegDiet_bin)  # 468 -> 156 obs.

################################################################################
#### Exclude outcomes with only one study (i.e., two exposures) - e.g., "anaemia_preg_subsamp"
obs.res_VegDiet_bin <- obs.res_VegDiet_bin %>%
  group_by(Outcome) %>%
  filter(n() > 2) %>%
  ungroup()
################################################################################
#### Remove some results with extremely huge 95% CIs
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin[which(obs.res_VegDiet_bin$se < 90), ]
################################################################################
#### Exposure labels shown in the forest plot
obs.res_VegDiet_bin$Exposure[obs.res_VegDiet_bin$Exposure == "Pesco-vegetarian"] <- "Pesco- vs. non-vegetarian"
obs.res_VegDiet_bin$Exposure[obs.res_VegDiet_bin$Exposure == "Full vegetarian"] <- "Full vs. non-vegetarian"
obs.res_VegDiet_bin$Exposure <- factor(
  obs.res_VegDiet_bin$Exposure,
  levels = c("Pesco- vs. non-vegetarian", "Full vs. non-vegetarian")
)
################################################################################

obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 156 -> 135 obs.

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
obs.res_VegDiet_con$Exposure[obs.res_VegDiet_con$Exposure == "Pesco-vegetarian"] <- "Pesco- vs. non-vegetarian"
obs.res_VegDiet_con$Exposure[obs.res_VegDiet_con$Exposure == "Full vegetarian"] <- "Full vs. non-vegetarian"
obs.res_VegDiet_con$Exposure <- factor(
  obs.res_VegDiet_con$Exposure,
  levels = c("Pesco- vs. non-vegetarian", "Full vs. non-vegetarian")
)
################################################################################

obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 28 obs.

################################################################################

## Separate results for full vegetarian and pesco-vegetarian
obs.res_VegDiet_bin_full.V <- subset(obs.res_VegDiet_bin, Exposure == "Full vs. non-vegetarian")
obs.res_VegDiet_bin_pesco.V <- subset(obs.res_VegDiet_bin, Exposure == "Pesco- vs. non-vegetarian")

obs.res_VegDiet_con_full.V <- subset(obs.res_VegDiet_con, Exposure == "Full vs. non-vegetarian")
obs.res_VegDiet_con_pesco.V <- subset(obs.res_VegDiet_con, Exposure == "Pesco- vs. non-vegetarian")

obs.res_VegDiet_bin_full.V
obs.res_VegDiet_bin_pesco.V
obs.res_VegDiet_con_full.V
obs.res_VegDiet_con_pesco.V

################################################################################
## !!! Add total sample size columns for meta-analysed results (for final result tables) !!!

### Binary outcomes
obs.res_VegDiet_bin_full.V <- obs.res_VegDiet_bin_full.V %>%
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
totals_VegDiet_bin_full.V <- obs.res_VegDiet_bin_full.V %>%
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

obs.res_VegDiet_bin_pesco.V <- obs.res_VegDiet_bin_pesco.V %>%
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
totals_VegDiet_bin_pesco.V <- obs.res_VegDiet_bin_pesco.V %>%
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

totals_VegDiet_bin_full.V
totals_VegDiet_bin_pesco.V

### Continuous outcomes
totals_VegDiet_con_full.V <- obs.res_VegDiet_con_full.V %>%
  group_by(Outcome) %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))

totals_VegDiet_con_pesco.V <- obs.res_VegDiet_con_pesco.V %>%
  group_by(Outcome) %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))

totals_VegDiet_con_full.V
totals_VegDiet_con_pesco.V
################################################################################

#------------------------------------------------------------------------------#
#                                Meta-analysis                                 #----
#------------------------------------------------------------------------------#

# Binary outcomes

## Full vegetarian vs. non-vegetarian

### Run meta-analysis
metagen_obs.res_VegDiet_bin_full.V <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_bin_full.V,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_bin_full.V

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_bin_full.V <-
  as.data.frame(
    cbind(
      "Exposure" = rep(
        unique(as.character(obs.res_VegDiet_bin_full.V$Exposure)),
        length(metagen_obs.res_VegDiet_bin_full.V[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_bin_full.V[["bylevs"]],
      metagen_obs.res_VegDiet_bin_full.V[["k.w"]],
      totals_VegDiet_bin_full.V$N_exp,
      totals_VegDiet_bin_full.V$N_ref,
      metagen_obs.res_VegDiet_bin_full.V[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_bin_full.V[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_bin_full.V[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_bin_full.V[["I2.w"]],
      metagen_obs.res_VegDiet_bin_full.V[["tau2.w"]],
      as.numeric(metagen_obs.res_VegDiet_bin_full.V[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_bin_full.V[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_bin_full.V[["pval.fixed.w"]])
      
    )
  )

colnames(metagen_obs.tbl_VegDiet_bin_full.V) <-
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
    "tau²",
    "b",
    "se",
    "pval"
  )

metagen_obs.tbl_VegDiet_bin_full.V$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_full.V$Beta)
metagen_obs.tbl_VegDiet_bin_full.V$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_full.V$SE)
metagen_obs.tbl_VegDiet_bin_full.V$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_bin_full.V$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_bin_full.V$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_bin_full.V$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_bin_full.V$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_bin_full.V$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_bin_full.V$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_full.V$`N of studies`)

metagen_obs.tbl_VegDiet_bin_full.V$OR <-
  format(round(exp(
    metagen_obs.tbl_VegDiet_bin_full.V$Beta
  ), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_bin_full.V$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin_full.V$Beta - 1.96 * metagen_obs.tbl_VegDiet_bin_full.V$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin_full.V$Beta + 1.96 * metagen_obs.tbl_VegDiet_bin_full.V$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_bin_full.V <- metagen_obs.tbl_VegDiet_bin_full.V %>%
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
    `tau²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_bin_full.V
str(metagen_obs.tbl_VegDiet_bin_full.V)  # 21 obs. = 21 outcomes * 1 exposure

write.xlsx(
  metagen_obs.tbl_VegDiet_bin_full.V,
  "ALL/IMP_MAIN_meta.tbl_VegDiet.subgroup_bin_full.V.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot for ALL binary outcomes
metaforest_obs.res_VegDiet_bin_full.V <- forest(
  metagen_obs.res_VegDiet_bin_full.V,
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

metaforest_obs.res_VegDiet_bin_full.V <- recordPlot()

png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_bin_full.V.png",
  res = 300,
  height = 9350,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_full.V)
dev.off()

################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for pregnancy outcomes
metaforest_obs.res_VegDiet_bin_full.V_Preg <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_full.V, Group == "Pregnancy outcome"),
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
metaforest_obs.res_VegDiet_bin_full.V_Preg <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_bin_full.V_Preg.png",
  res = 300,
  height = 2950,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_full.V_Preg)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for delivery outcomes
metaforest_obs.res_VegDiet_bin_full.V_Deli <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_full.V, Group == "Delivery outcome"),
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
metaforest_obs.res_VegDiet_bin_full.V_Deli <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_bin_full.V_Deli.png",
  res = 300,
  height = 5410,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_full.V_Deli)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for postnatal outcomes
metaforest_obs.res_VegDiet_bin_full.V_Post <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_full.V, Group == "Postnatal outcome"),
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
metaforest_obs.res_VegDiet_bin_full.V_Post <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_bin_full.V_Post.png",
  res = 300,
  height = 1400,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_full.V_Post)
dev.off()
################################################################################

## Pesco-vegetarian vs. non-vegetarian

### Run meta-analysis
metagen_obs.res_VegDiet_bin_pesco.V <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_bin_pesco.V,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_bin_pesco.V

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_bin_pesco.V <-
  as.data.frame(
    cbind(
      "Exposure" = rep(
        unique(as.character(obs.res_VegDiet_bin_pesco.V$Exposure)),
        length(metagen_obs.res_VegDiet_bin_pesco.V[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_bin_pesco.V[["bylevs"]],
      metagen_obs.res_VegDiet_bin_pesco.V[["k.w"]],
      totals_VegDiet_bin_pesco.V$N_exp,
      totals_VegDiet_bin_pesco.V$N_ref,
      metagen_obs.res_VegDiet_bin_pesco.V[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_bin_pesco.V[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_bin_pesco.V[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_bin_pesco.V[["I2.w"]],
      metagen_obs.res_VegDiet_bin_pesco.V[["tau2.w"]],
      as.numeric(metagen_obs.res_VegDiet_bin_pesco.V[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_bin_pesco.V[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_bin_pesco.V[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_VegDiet_bin_pesco.V) <-
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
    "tau²",
    "b",
    "se",
    "pval"
  )

metagen_obs.tbl_VegDiet_bin_pesco.V$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_pesco.V$Beta)
metagen_obs.tbl_VegDiet_bin_pesco.V$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_pesco.V$SE)
metagen_obs.tbl_VegDiet_bin_pesco.V$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_bin_pesco.V$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_bin_pesco.V$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_bin_pesco.V$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_bin_pesco.V$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_bin_pesco.V$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_bin_pesco.V$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_pesco.V$`N of studies`)

metagen_obs.tbl_VegDiet_bin_pesco.V$OR <-
  format(round(exp(
    metagen_obs.tbl_VegDiet_bin_pesco.V$Beta
  ), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_bin_pesco.V$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin_pesco.V$Beta - 1.96 * metagen_obs.tbl_VegDiet_bin_pesco.V$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin_pesco.V$Beta + 1.96 * metagen_obs.tbl_VegDiet_bin_pesco.V$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_bin_pesco.V <- metagen_obs.tbl_VegDiet_bin_pesco.V %>%
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
    `tau²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_bin_pesco.V
str(metagen_obs.tbl_VegDiet_bin_pesco.V)  # 21 obs. = 21 outcomes * 1 exposure

write.xlsx(
  metagen_obs.tbl_VegDiet_bin_pesco.V,
  "ALL/IMP_MAIN_meta.tbl_VegDiet.subgroup_bin_pesco.V.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot for ALL binary outcomes
metaforest_obs.res_VegDiet_bin_pesco.V <- forest(
  metagen_obs.res_VegDiet_bin_pesco.V,
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

metaforest_obs.res_VegDiet_bin_pesco.V <- recordPlot()

png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_bin_pesco.V.png",
  res = 300,
  height = 9350,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_pesco.V)
dev.off()

################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for pregnancy outcomes
metaforest_obs.res_VegDiet_bin_pesco.V_Preg <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_pesco.V, Group == "Pregnancy outcome"),
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
metaforest_obs.res_VegDiet_bin_pesco.V_Preg <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_bin_pesco.V_Preg.png",
  res = 300,
  height = 2950,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_pesco.V_Preg)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for delivery outcomes
metaforest_obs.res_VegDiet_bin_pesco.V_Deli <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_pesco.V, Group == "Delivery outcome"),
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
metaforest_obs.res_VegDiet_bin_pesco.V_Deli <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_bin_pesco.V_Deli.png",
  res = 300,
  height = 5410,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_pesco.V_Deli)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for postnatal outcomes
metaforest_obs.res_VegDiet_bin_pesco.V_Post <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_pesco.V, Group == "Postnatal outcome"),
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
metaforest_obs.res_VegDiet_bin_pesco.V_Post <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_bin_pesco.V_Post.png",
  res = 300,
  height = 1400,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_pesco.V_Post)
dev.off()
################################################################################

# Continuous outcomes

## Full vegetarian vs. non-vegetarian

### Run meta-analysis
metagen_obs.res_VegDiet_con_full.V <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_con_full.V,
    sm = "",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_con_full.V

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_con_full.V <-
  as.data.frame(
    cbind(
      "Exposure" = rep(
        unique(as.character(obs.res_VegDiet_con_full.V$Exposure)),
        length(metagen_obs.res_VegDiet_con_full.V[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_con_full.V[["bylevs"]],
      metagen_obs.res_VegDiet_con_full.V[["k.w"]],
      totals_VegDiet_con_full.V$N_exp,
      totals_VegDiet_con_full.V$N_ref,
      metagen_obs.res_VegDiet_con_full.V[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_con_full.V[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_con_full.V[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_con_full.V[["I2.w"]],
      metagen_obs.res_VegDiet_con_full.V[["tau2.w"]],
      as.numeric(metagen_obs.res_VegDiet_con_full.V[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_con_full.V[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_con_full.V[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_VegDiet_con_full.V) <-
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

metagen_obs.tbl_VegDiet_con_full.V$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_con_full.V$Beta)
metagen_obs.tbl_VegDiet_con_full.V$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_con_full.V$SE)
metagen_obs.tbl_VegDiet_con_full.V$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_con_full.V$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_con_full.V$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_con_full.V$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_con_full.V$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_con_full.V$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_con_full.V$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_con_full.V$`N of studies`)

metagen_obs.tbl_VegDiet_con_full.V$`95% CI` <- paste0(format(
  round(
    metagen_obs.tbl_VegDiet_con_full.V$Beta - 1.96 * metagen_obs.tbl_VegDiet_con_full.V$SE,
    digits = 2
  ),
  nsmall = 2
), ", ", format(
  round(
    metagen_obs.tbl_VegDiet_con_full.V$Beta + 1.96 * metagen_obs.tbl_VegDiet_con_full.V$SE,
    digits = 2
  ),
  nsmall = 2
))
metagen_obs.tbl_VegDiet_con_full.V$Beta <-
  format(round(metagen_obs.tbl_VegDiet_con_full.V$Beta, digits = 2),
         nsmall = 2)

metagen_obs.tbl_VegDiet_con_full.V <- metagen_obs.tbl_VegDiet_con_full.V %>%
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
    `tau²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_con_full.V
str(metagen_obs.tbl_VegDiet_con_full.V)  # 4 obs. = 4 outcomes * 1 exposure

write.xlsx(
  metagen_obs.tbl_VegDiet_con_full.V,
  "ALL/IMP_MAIN_meta.tbl_VegDiet.subgroup_con_full.V.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot
metaforest_obs.res_VegDiet_con_full.V <- forest(
  metagen_obs.res_VegDiet_con_full.V,
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

metaforest_obs.res_VegDiet_con_full.V <- recordPlot()

png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_con_full.V.png",
  res = 300,
  height = 2000,
  width = 2600
)
print(metaforest_obs.res_VegDiet_con_full.V)
dev.off()

################################################################################

## Pesco-vegetarian vs. non-vegetarian

### Run meta-analysis
metagen_obs.res_VegDiet_con_pesco.V <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_con_pesco.V,
    sm = "",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_con_pesco.V

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_con_pesco.V <-
  as.data.frame(
    cbind(
      "Exposure" = rep(
        unique(as.character(obs.res_VegDiet_con_pesco.V$Exposure)),
        length(metagen_obs.res_VegDiet_con_pesco.V[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_con_pesco.V[["bylevs"]],
      metagen_obs.res_VegDiet_con_pesco.V[["k.w"]],
      totals_VegDiet_con_pesco.V$N_exp,
      totals_VegDiet_con_pesco.V$N_ref,
      metagen_obs.res_VegDiet_con_pesco.V[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_con_pesco.V[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_con_pesco.V[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_con_pesco.V[["I2.w"]],
      metagen_obs.res_VegDiet_con_pesco.V[["tau2.w"]],
      as.numeric(metagen_obs.res_VegDiet_con_pesco.V[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_con_pesco.V[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_con_pesco.V[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_VegDiet_con_pesco.V) <-
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

metagen_obs.tbl_VegDiet_con_pesco.V$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_con_pesco.V$Beta)
metagen_obs.tbl_VegDiet_con_pesco.V$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_con_pesco.V$SE)
metagen_obs.tbl_VegDiet_con_pesco.V$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_con_pesco.V$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_con_pesco.V$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_con_pesco.V$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_con_pesco.V$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_con_pesco.V$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_con_pesco.V$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_con_pesco.V$`N of studies`)

metagen_obs.tbl_VegDiet_con_pesco.V$`95% CI` <- paste0(format(
  round(
    metagen_obs.tbl_VegDiet_con_pesco.V$Beta - 1.96 * metagen_obs.tbl_VegDiet_con_pesco.V$SE,
    digits = 2
  ),
  nsmall = 2
), ", ", format(
  round(
    metagen_obs.tbl_VegDiet_con_pesco.V$Beta + 1.96 * metagen_obs.tbl_VegDiet_con_pesco.V$SE,
    digits = 2
  ),
  nsmall = 2
))
metagen_obs.tbl_VegDiet_con_pesco.V$Beta <-
  format(round(metagen_obs.tbl_VegDiet_con_pesco.V$Beta, digits = 2),
         nsmall = 2)

metagen_obs.tbl_VegDiet_con_pesco.V <- metagen_obs.tbl_VegDiet_con_pesco.V %>%
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
    `tau²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_con_pesco.V
str(metagen_obs.tbl_VegDiet_con_pesco.V)  # 4 obs. = 4 outcomes * 1 exposure

write.xlsx(
  metagen_obs.tbl_VegDiet_con_pesco.V,
  "ALL/IMP_MAIN_meta.tbl_VegDiet.subgroup_con_pesco.V.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot
metaforest_obs.res_VegDiet_con_pesco.V <- forest(
  metagen_obs.res_VegDiet_con_pesco.V,
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

metaforest_obs.res_VegDiet_con_pesco.V <- recordPlot()

png(
  "ALL/IMP_MAIN_meta.forest_VegDiet.subgroup_con_pesco.V.png",
  res = 300,
  height = 2000,
  width = 2600
)
print(metaforest_obs.res_VegDiet_con_pesco.V)
dev.off()

#------------------------------------------------------------------------------#
#                                Combine Plots                                 #----
#------------------------------------------------------------------------------#

# Binary outcomes

## ALL outcomes
png("dummy.png")
replayPlot(metaforest_obs.res_VegDiet_bin_pesco.V)
grob1 <- grid.grab()
replayPlot(metaforest_obs.res_VegDiet_bin_full.V)
grob2 <- grid.grab()
dev.off()

title1 <- textGrob("Pesco- vs. non-vegetarian for binary outcomes",
                   gp = gpar(fontsize = 12, fontface = "bold"))
title2 <- textGrob("Full vs. non-vegetarian for binary outcomes",
                   gp = gpar(fontsize = 12, fontface = "bold"))

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.03, 0.97))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.03, 0.97))

png(
  "ALL/Comb_IMP_MAIN_meta.forest_VegDiet.subgroup_bin.png",
  res = 300,
  height = 9600,
  width = 5200
)
grid.arrange(grob1_with_title, grob2_with_title, ncol = 2)
dev.off()

################################################################################
################################################################################
## Pregnancy outcomes
png("dummy.png")
replayPlot(metaforest_obs.res_VegDiet_bin_pesco.V_Preg)
grob1 <- grid.grab()
replayPlot(metaforest_obs.res_VegDiet_bin_full.V_Preg)
grob2 <- grid.grab()
dev.off()

title1 <- textGrob(
  "Pesco- vs. non-vegetarian for binary pregnancy outcomes",
  gp = gpar(fontsize = 12, fontface = "bold")
)
title2 <- textGrob(
  "Full vs. non-vegetarian for binary pregnancy outcomes",
  gp = gpar(fontsize = 12, fontface = "bold")
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.03, 0.97))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.03, 0.97))

png(
  "ALL/Comb_IMP_MAIN_meta.forest_VegDiet.subgroup_bin_Preg.png",
  res = 300,
  height = 3050,
  width = 5200
)
grid.arrange(grob1_with_title, grob2_with_title, ncol = 2)
dev.off()
################################################################################
################################################################################
## Delivery outcomes
png("dummy.png")
replayPlot(metaforest_obs.res_VegDiet_bin_pesco.V_Deli)
grob1 <- grid.grab()
replayPlot(metaforest_obs.res_VegDiet_bin_full.V_Deli)
grob2 <- grid.grab()
dev.off()

title1 <- textGrob(
  "Pesco- vs. non-vegetarian for binary delivery outcomes",
  gp = gpar(fontsize = 12, fontface = "bold")
)
title2 <- textGrob(
  "Full vs. non-vegetarian for binary delivery outcomes",
  gp = gpar(fontsize = 12, fontface = "bold")
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.03, 0.97))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.03, 0.97))

png(
  "ALL/Comb_IMP_MAIN_meta.forest_VegDiet.subgroup_bin_Deli.png",
  res = 300,
  height = 5600,
  width = 5200
)
grid.arrange(grob1_with_title, grob2_with_title, ncol = 2)
dev.off()
################################################################################
################################################################################
## Postnatal outcomes
png("dummy.png")
replayPlot(metaforest_obs.res_VegDiet_bin_pesco.V_Post)
grob1 <- grid.grab()
replayPlot(metaforest_obs.res_VegDiet_bin_full.V_Post)
grob2 <- grid.grab()
dev.off()

title1 <- textGrob(
  "Pesco- vs. non-vegetarian for binary postnatal outcomes",
  gp = gpar(fontsize = 12, fontface = "bold")
)
title2 <- textGrob(
  "Full vs. non-vegetarian for binary postnatal outcomes",
  gp = gpar(fontsize = 12, fontface = "bold")
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.03, 0.97))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.03, 0.97))

png(
  "ALL/Comb_IMP_MAIN_meta.forest_VegDiet.subgroup_bin_Post.png",
  res = 300,
  height = 1500,
  width = 5100
)
grid.arrange(grob1_with_title, grob2_with_title, ncol = 2)
dev.off()
################################################################################

# Continuous outcomes
png("dummy.png")
replayPlot(metaforest_obs.res_VegDiet_con_pesco.V)
grob1 <- grid.grab()
replayPlot(metaforest_obs.res_VegDiet_con_full.V)
grob2 <- grid.grab()
dev.off()

title1 <- textGrob(
  "Pesco- vs. non-vegetarian for continuous outcomes",
  gp = gpar(fontsize = 12, fontface = "bold")
)
title2 <- textGrob("Full vs. non-vegetarian for continuous outcomes",
                   gp = gpar(fontsize = 12, fontface = "bold"))

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.03, 0.97))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.03, 0.97))

png(
  "ALL/Comb_IMP_MAIN_meta.forest_VegDiet.subgroup_con.png",
  res = 300,
  height = 2100,
  width = 4500
)
grid.arrange(grob1_with_title, grob2_with_title, ncol = 2)
dev.off()

################################################################################
