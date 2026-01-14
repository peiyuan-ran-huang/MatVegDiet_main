################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 14-Dec-2024
# This script is to perform meta-analysis on paternal negative control analysis (with imputed data) results for vegetarian diets.
## Part 2: Maternal Model 2 and Paternal Model 2

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

## ALSPAC
obs.res_VegDiet_bin_ALSPAC <-
  read.xlsx("ALSPAC/IMP_PNC_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_ALSPAC
str(obs.res_VegDiet_bin_ALSPAC)  # 84 results (21 outcomes * 1 category * 4 models)

obs.res_VegDiet_con_ALSPAC <-
  read.xlsx("ALSPAC/IMP_PNC_obs.res_VegDiet_con.xlsx")
obs.res_VegDiet_con_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_ALSPAC
str(obs.res_VegDiet_con_ALSPAC)  # 16 results (4 outcomes * 1 category * 4 models)

obs.res_VegDiet_ord_ALSPAC <-
  read.xlsx("ALSPAC/IMP_PNC_BF_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_ALSPAC
str(obs.res_VegDiet_ord_ALSPAC)  # 4 results (1 outcome * 1 category * 4 models)

## MoBa
obs.res_VegDiet_bin_MoBa <-
  read.xlsx("MoBa/IMP_PNC_obs.res_VegDiet_bin.xlsx")
obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_MoBa
str(obs.res_VegDiet_bin_MoBa)  # 84 results (21 outcomes * 1 category * 4 models)

obs.res_VegDiet_con_MoBa <-
  read.xlsx("MoBa/IMP_PNC_obs.res_VegDiet_con.xlsx")
obs.res_VegDiet_con_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_con_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_con_MoBa
str(obs.res_VegDiet_con_MoBa)  # 16 results (4 outcomes * 1 category * 4 models)

obs.res_VegDiet_ord_MoBa <-
  read.xlsx("MoBa/IMP_PNC_BF_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_MoBa
str(obs.res_VegDiet_ord_MoBa)  # 4 results (1 outcome * 1 category * 4 models)

################################################################################

## Combine

### Binary outcomes
obs.res_VegDiet_bin_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_bin_MoBa$Study <- "MoBa"

obs.res_VegDiet_bin <-
  rbind(obs.res_VegDiet_bin_ALSPAC, obs.res_VegDiet_bin_MoBa)
obs.res_VegDiet_bin
str(obs.res_VegDiet_bin)  # 168 results = 84 + 84

### Continuous outcomes
obs.res_VegDiet_con_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_con_MoBa$Study <- "MoBa"

obs.res_VegDiet_con <-
  rbind(obs.res_VegDiet_con_ALSPAC, obs.res_VegDiet_con_MoBa)
obs.res_VegDiet_con
str(obs.res_VegDiet_con)  # 32 results = 16 + 16

### Ordinal outcomes - !!! Added to double check residual confounding observed in negative control outcome analysis !!!
obs.res_VegDiet_ord_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_ord_MoBa$Study <- "MoBa"

obs.res_VegDiet_ord <-
  rbind(obs.res_VegDiet_ord_ALSPAC, obs.res_VegDiet_ord_MoBa)
obs.res_VegDiet_ord
str(obs.res_VegDiet_ord)  # 8 results = 4 + 4

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

secondary_ord <- subset(
  MRPREG_outcome_labels,
  varname %in% read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_cat")$varname
)
secondary_ord  # 1 (secondary) ordinal/categorical outcome (bf_dur_4c as negative outcome)

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

ALL_secondary_ord <- subset(secondary_ord, label %in% obs.res_VegDiet_ord$Outcome)
ALL_secondary_ord
dim(ALL_secondary_ord)  # 1 (primary) ordinal/categorical outcome available in the combined results

################################################################################

## Select and prepare results for meta-analysis

### Binary outcomes
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

################################################################################
#### Remove some results with extremely huge 95% CIs
obs.res_VegDiet_bin <-
  obs.res_VegDiet_bin[which(obs.res_VegDiet_bin$se < 90), ]
################################################################################
#### !!! Keep only outcomes with all 4 models available in both studies (for binary outcomes only)
obs.res_VegDiet_bin <- obs.res_VegDiet_bin %>%
  group_by(Outcome) %>%
  filter(n() == 8) %>%
  ungroup()
################################################################################
#### Exposure labels shown in the forest plot
obs.res_VegDiet_bin$Exposure <- "Pesco-/full vs. non-vegetarian"
################################################################################

obs.res_VegDiet_bin
dim(obs.res_VegDiet_bin)  # 168 -> 144 obs. (18 outcomes available)

################################################################################
################################################################################

### Continuous outcomes
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
obs.res_VegDiet_con$Exposure <- "Pesco-/full vs. non-vegetarian"
################################################################################

obs.res_VegDiet_con
dim(obs.res_VegDiet_con)  # 32 obs.

################################################################################
################################################################################

### Ordinal outcomes
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
#### Exposure labels shown in the forest plot
obs.res_VegDiet_ord$Exposure <- "Pesco-/full vs. non-vegetarian"
################################################################################

obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 8 obs.

################################################################################

## Separate results for Maternal Model 2 / Paternal Model 2
obs.res_VegDiet_bin_Mat <- subset(obs.res_VegDiet_bin, Model == "Maternal Model 2")
obs.res_VegDiet_bin_Pat <- subset(obs.res_VegDiet_bin, Model == "Paternal Model 2")

obs.res_VegDiet_con_Mat <- subset(obs.res_VegDiet_con, Model == "Maternal Model 2")
obs.res_VegDiet_con_Pat <- subset(obs.res_VegDiet_con, Model == "Paternal Model 2")

obs.res_VegDiet_ord_Mat <- subset(obs.res_VegDiet_ord, Model == "Maternal Model 2")
obs.res_VegDiet_ord_Pat <- subset(obs.res_VegDiet_ord, Model == "Paternal Model 2")

################################################################################

obs.res_VegDiet_bin_Mat
obs.res_VegDiet_bin_Pat

obs.res_VegDiet_con_Mat
obs.res_VegDiet_con_Pat

obs.res_VegDiet_ord_Mat
obs.res_VegDiet_ord_Pat

################################################################################
## !!! Add total sample size columns for meta-analysed results (for final result tables) !!!

### Binary outcomes

#### Maternal Model
obs.res_VegDiet_bin_Mat <- obs.res_VegDiet_bin_Mat %>%
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

totals_VegDiet_bin_Mat <- obs.res_VegDiet_bin_Mat %>%
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

################################################################################
################################################################################

#### Paternal Model
obs.res_VegDiet_bin_Pat <- obs.res_VegDiet_bin_Pat %>%
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

totals_VegDiet_bin_Pat <- obs.res_VegDiet_bin_Pat %>%
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

################################################################################
################################################################################

### Continuous outcomes

#### Maternal Model
totals_VegDiet_con_Mat <- obs.res_VegDiet_con_Mat %>%
  group_by(Outcome) %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))

#### Paternal Model
totals_VegDiet_con_Pat <- obs.res_VegDiet_con_Pat %>%
  group_by(Outcome) %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))

totals_VegDiet_con_Mat
totals_VegDiet_con_Pat

################################################################################
################################################################################

### Ordinal outcomes

#### Maternal Model
totals_VegDiet_ord_Mat <- obs.res_VegDiet_ord_Mat %>%
  group_by(Outcome) %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))

#### Paternal Model
totals_VegDiet_ord_Pat <- obs.res_VegDiet_ord_Pat %>%
  group_by(Outcome) %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))

totals_VegDiet_ord_Mat
totals_VegDiet_ord_Pat

################################################################################

#------------------------------------------------------------------------------#
#                                Meta-analysis                                 #----
#------------------------------------------------------------------------------#

# Binary outcomes

## Maternal Model

### Run meta-analysis
metagen_obs.res_VegDiet_bin_Mat <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_bin_Mat,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_bin_Mat

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_bin_Mat <-
  as.data.frame(
    cbind(
      metagen_obs.res_VegDiet_bin_Mat[["bylevs"]],
      "Model" = rep(
        unique(as.character(obs.res_VegDiet_bin_Mat$Model)),
        length(metagen_obs.res_VegDiet_bin_Mat[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_bin_Mat[["k.w"]],
      totals_VegDiet_bin_Mat$N_exp,
      totals_VegDiet_bin_Mat$N_ref,
      metagen_obs.res_VegDiet_bin_Mat[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_bin_Mat[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_bin_Mat[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_bin_Mat[["I2.w"]],
      metagen_obs.res_VegDiet_bin_Mat[["tau2.w"]],
      b = metagen_obs.res_VegDiet_bin_Mat[["TE.fixed.w"]],
      se = metagen_obs.res_VegDiet_bin_Mat[["seTE.fixed.w"]],
      pval = metagen_obs.res_VegDiet_bin_Mat[["pval.fixed.w"]]
    )
  )

colnames(metagen_obs.tbl_VegDiet_bin_Mat) <-
  c(
    "Outcome",
    "Model",
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

metagen_obs.tbl_VegDiet_bin_Mat$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_Mat$Beta)
metagen_obs.tbl_VegDiet_bin_Mat$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_Mat$SE)
metagen_obs.tbl_VegDiet_bin_Mat$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_bin_Mat$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_bin_Mat$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_bin_Mat$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_bin_Mat$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_bin_Mat$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_bin_Mat$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_Mat$`N of studies`)

metagen_obs.tbl_VegDiet_bin_Mat$OR <-
  format(round(exp(metagen_obs.tbl_VegDiet_bin_Mat$Beta), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_bin_Mat$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin_Mat$Beta - 1.96 * metagen_obs.tbl_VegDiet_bin_Mat$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin_Mat$Beta + 1.96 * metagen_obs.tbl_VegDiet_bin_Mat$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_bin_Mat <- metagen_obs.tbl_VegDiet_bin_Mat %>%
  select(
    Outcome,
    Model,
    `N of studies`,
    `Cases/total in exposed`,
    `Cases/total in reference`,
    OR,
    `95% CI`,
    `p-value`,
    `I²`,
    `tau²`,
    b,
    se,
    pval
  )

metagen_obs.tbl_VegDiet_bin_Mat
str(metagen_obs.tbl_VegDiet_bin_Mat)  # 18 obs. = 18 outcomes * 1 exposure * 1 model

write.xlsx(
  metagen_obs.tbl_VegDiet_bin_Mat,
  "ALL/IMP_PNC_meta.tbl_VegDiet_bin_Mat2.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot for ALL binary outcomes
metaforest_obs.res_VegDiet_bin_Mat <- forest(
  metagen_obs.res_VegDiet_bin_Mat,
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

metaforest_obs.res_VegDiet_bin_Mat <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat2.png",
  res = 300,
  height = 6700,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Mat)
dev.off()

################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for pregnancy outcomes
metaforest_obs.res_VegDiet_bin_Mat_Preg <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_Mat, Group == "Pregnancy outcome"),
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

metaforest_obs.res_VegDiet_bin_Mat_Preg <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat2_Preg.png",
  res = 300,
  height = 2000,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Mat_Preg)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for delivery outcomes
metaforest_obs.res_VegDiet_bin_Mat_Deli <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_Mat, Group == "Delivery outcome"),
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

metaforest_obs.res_VegDiet_bin_Mat_Deli <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat2_Deli.png",
  res = 300,
  height = 3800,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Mat_Deli)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for postnatal outcomes
metaforest_obs.res_VegDiet_bin_Mat_Post <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_Mat, Group == "Postnatal outcome"),
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

metaforest_obs.res_VegDiet_bin_Mat_Post <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat2_Post.png",
  res = 300,
  height = 1300,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Mat_Post)
dev.off()
################################################################################

## Paternal Model
### Run meta-analysis
metagen_obs.res_VegDiet_bin_Pat <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_bin_Pat,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_bin_Pat

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_bin_Pat <-
  as.data.frame(
    cbind(
      metagen_obs.res_VegDiet_bin_Pat[["bylevs"]],
      "Model" = rep(
        unique(as.character(obs.res_VegDiet_bin_Pat$Model)),
        length(metagen_obs.res_VegDiet_bin_Pat[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_bin_Pat[["k.w"]],
      totals_VegDiet_bin_Pat$N_exp,
      totals_VegDiet_bin_Pat$N_ref,
      metagen_obs.res_VegDiet_bin_Pat[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_bin_Pat[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_bin_Pat[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_bin_Pat[["I2.w"]],
      metagen_obs.res_VegDiet_bin_Pat[["tau2.w"]],
      b = metagen_obs.res_VegDiet_bin_Pat[["TE.fixed.w"]],
      se = metagen_obs.res_VegDiet_bin_Pat[["seTE.fixed.w"]],
      pval = metagen_obs.res_VegDiet_bin_Pat[["pval.fixed.w"]]
    )
  )

colnames(metagen_obs.tbl_VegDiet_bin_Pat) <-
  c(
    "Outcome",
    "Model",
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

metagen_obs.tbl_VegDiet_bin_Pat$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_Pat$Beta)
metagen_obs.tbl_VegDiet_bin_Pat$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_Pat$SE)
metagen_obs.tbl_VegDiet_bin_Pat$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_bin_Pat$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_bin_Pat$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_bin_Pat$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_bin_Pat$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_bin_Pat$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_bin_Pat$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_bin_Pat$`N of studies`)

metagen_obs.tbl_VegDiet_bin_Pat$OR <-
  format(round(exp(metagen_obs.tbl_VegDiet_bin_Pat$Beta), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_bin_Pat$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin_Pat$Beta - 1.96 * metagen_obs.tbl_VegDiet_bin_Pat$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_bin_Pat$Beta + 1.96 * metagen_obs.tbl_VegDiet_bin_Pat$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_bin_Pat <- metagen_obs.tbl_VegDiet_bin_Pat %>%
  select(
    Outcome,
    Model,
    `N of studies`,
    `Cases/total in exposed`,
    `Cases/total in reference`,
    OR,
    `95% CI`,
    `p-value`,
    `I²`,
    `tau²`,
    b,
    se,
    pval
  )

metagen_obs.tbl_VegDiet_bin_Pat
str(metagen_obs.tbl_VegDiet_bin_Pat)  # 18 obs. = 18 outcomes * 1 exposure * 1 model

write.xlsx(
  metagen_obs.tbl_VegDiet_bin_Pat,
  "ALL/IMP_PNC_meta.tbl_VegDiet_bin_Pat2.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot for ALL binary outcomes
metaforest_obs.res_VegDiet_bin_Pat <- forest(
  metagen_obs.res_VegDiet_bin_Pat,
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

metaforest_obs.res_VegDiet_bin_Pat <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat2.png",
  res = 300,
  height = 6700,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Pat)
dev.off()

################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for pregnancy outcomes
metaforest_obs.res_VegDiet_bin_Pat_Preg <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_Pat, Group == "Pregnancy outcome"),
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

metaforest_obs.res_VegDiet_bin_Pat_Preg <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat2_Preg.png",
  res = 300,
  height = 2000,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Pat_Preg)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for delivery outcomes
metaforest_obs.res_VegDiet_bin_Pat_Deli <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_Pat, Group == "Delivery outcome"),
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

metaforest_obs.res_VegDiet_bin_Pat_Deli <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat2_Deli.png",
  res = 300,
  height = 3800,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Pat_Deli)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for postnatal outcomes
metaforest_obs.res_VegDiet_bin_Pat_Post <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_VegDiet_bin_Pat, Group == "Postnatal outcome"),
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

metaforest_obs.res_VegDiet_bin_Pat_Post <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat2_Post.png",
  res = 300,
  height = 1300,
  width = 2600
)
print(metaforest_obs.res_VegDiet_bin_Pat_Post)
dev.off()
################################################################################

# Continuous outcomes

## Maternal Model

### Run meta-analysis
metagen_obs.res_VegDiet_con_Mat <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_con_Mat,
    sm = "",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_con_Mat

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_con_Mat <-
  as.data.frame(
    cbind(
      metagen_obs.res_VegDiet_con_Mat[["bylevs"]],
      "Model" = rep(
        unique(as.character(obs.res_VegDiet_con_Mat$Model)),
        length(metagen_obs.res_VegDiet_con_Mat[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_con_Mat[["k.w"]],
      totals_VegDiet_con_Mat$N_exp,
      totals_VegDiet_con_Mat$N_ref,
      metagen_obs.res_VegDiet_con_Mat[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_con_Mat[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_con_Mat[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_con_Mat[["I2.w"]],
      metagen_obs.res_VegDiet_con_Mat[["tau2.w"]],
      b = metagen_obs.res_VegDiet_con_Mat[["TE.fixed.w"]],
      se = metagen_obs.res_VegDiet_con_Mat[["seTE.fixed.w"]],
      pval = metagen_obs.res_VegDiet_con_Mat[["pval.fixed.w"]]
    )
  )

colnames(metagen_obs.tbl_VegDiet_con_Mat) <-
  c(
    "Outcome",
    "Model",
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

metagen_obs.tbl_VegDiet_con_Mat$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_con_Mat$Beta)
metagen_obs.tbl_VegDiet_con_Mat$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_con_Mat$SE)
metagen_obs.tbl_VegDiet_con_Mat$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_con_Mat$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_con_Mat$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_con_Mat$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_con_Mat$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_con_Mat$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_con_Mat$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_con_Mat$`N of studies`)

metagen_obs.tbl_VegDiet_con_Mat$`95% CI` <- paste0(format(
  round(
    metagen_obs.tbl_VegDiet_con_Mat$Beta - 1.96 * metagen_obs.tbl_VegDiet_con_Mat$SE,
    digits = 2
  ),
  nsmall = 2
), ", ", format(
  round(
    metagen_obs.tbl_VegDiet_con_Mat$Beta + 1.96 * metagen_obs.tbl_VegDiet_con_Mat$SE,
    digits = 2
  ),
  nsmall = 2
))
metagen_obs.tbl_VegDiet_con_Mat$Beta <-
  format(round(metagen_obs.tbl_VegDiet_con_Mat$Beta, digits = 2),
         nsmall = 2)

metagen_obs.tbl_VegDiet_con_Mat <- metagen_obs.tbl_VegDiet_con_Mat %>%
  select(
    Outcome,
    Model,
    `N of studies`,
    `N exposed`,
    `N reference`,
    Beta,
    `95% CI`,
    `p-value`,
    `I²`,
    `tau²`,
    b,
    se,
    pval
  )

metagen_obs.tbl_VegDiet_con_Mat
str(metagen_obs.tbl_VegDiet_con_Mat)  # 4 obs. = 4 outcomes * 1 exposure * 1 model

write.xlsx(
  metagen_obs.tbl_VegDiet_con_Mat,
  "ALL/IMP_PNC_meta.tbl_VegDiet_con_Mat2.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot
metaforest_obs.res_VegDiet_con_Mat <- forest(
  metagen_obs.res_VegDiet_con_Mat,
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

metaforest_obs.res_VegDiet_con_Mat <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_con_Mat2.png",
  res = 300,
  height = 1650,
  width = 2100
)
print(metaforest_obs.res_VegDiet_con_Mat)
dev.off()

################################################################################

## Paternal Model

### Run meta-analysis
metagen_obs.res_VegDiet_con_Pat <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_con_Pat,
    sm = "",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_con_Pat

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_con_Pat <-
  as.data.frame(
    cbind(
      metagen_obs.res_VegDiet_con_Pat[["bylevs"]],
      "Model" = rep(
        unique(as.character(obs.res_VegDiet_con_Pat$Model)),
        length(metagen_obs.res_VegDiet_con_Pat[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_con_Pat[["k.w"]],
      totals_VegDiet_con_Pat$N_exp,
      totals_VegDiet_con_Pat$N_ref,
      metagen_obs.res_VegDiet_con_Pat[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_con_Pat[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_con_Pat[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_con_Pat[["I2.w"]],
      metagen_obs.res_VegDiet_con_Pat[["tau2.w"]],
      b = metagen_obs.res_VegDiet_con_Pat[["TE.fixed.w"]],
      se = metagen_obs.res_VegDiet_con_Pat[["seTE.fixed.w"]],
      pval = metagen_obs.res_VegDiet_con_Pat[["pval.fixed.w"]]
    )
  )

colnames(metagen_obs.tbl_VegDiet_con_Pat) <-
  c(
    "Outcome",
    "Model",
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

metagen_obs.tbl_VegDiet_con_Pat$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_con_Pat$Beta)
metagen_obs.tbl_VegDiet_con_Pat$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_con_Pat$SE)
metagen_obs.tbl_VegDiet_con_Pat$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_con_Pat$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_con_Pat$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_con_Pat$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_con_Pat$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_con_Pat$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_con_Pat$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_con_Pat$`N of studies`)

metagen_obs.tbl_VegDiet_con_Pat$`95% CI` <- paste0(format(
  round(
    metagen_obs.tbl_VegDiet_con_Pat$Beta - 1.96 * metagen_obs.tbl_VegDiet_con_Pat$SE,
    digits = 2
  ),
  nsmall = 2
), ", ", format(
  round(
    metagen_obs.tbl_VegDiet_con_Pat$Beta + 1.96 * metagen_obs.tbl_VegDiet_con_Pat$SE,
    digits = 2
  ),
  nsmall = 2
))
metagen_obs.tbl_VegDiet_con_Pat$Beta <-
  format(round(metagen_obs.tbl_VegDiet_con_Pat$Beta, digits = 2),
         nsmall = 2)

metagen_obs.tbl_VegDiet_con_Pat <- metagen_obs.tbl_VegDiet_con_Pat %>%
  select(
    Outcome,
    Model,
    `N of studies`,
    `N exposed`,
    `N reference`,
    Beta,
    `95% CI`,
    `p-value`,
    `I²`,
    `tau²`,
    b,
    se,
    pval
  )

metagen_obs.tbl_VegDiet_con_Pat
str(metagen_obs.tbl_VegDiet_con_Pat)  # 4 obs. = 4 outcomes * 1 exposure * 1 model

write.xlsx(
  metagen_obs.tbl_VegDiet_con_Pat,
  "ALL/IMP_PNC_meta.tbl_VegDiet_con_Pat2.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot
metaforest_obs.res_VegDiet_con_Pat <- forest(
  metagen_obs.res_VegDiet_con_Pat,
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

metaforest_obs.res_VegDiet_con_Pat <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_con_Pat2.png",
  res = 300,
  height = 1650,
  width = 2100
)
print(metaforest_obs.res_VegDiet_con_Pat)
dev.off()

################################################################################

# Ordinal outcomes

## Maternal Model

### Run meta-analysis
metagen_obs.res_VegDiet_ord_Mat <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_ord_Mat,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_ord_Mat

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_ord_Mat <-
  as.data.frame(
    cbind(
      metagen_obs.res_VegDiet_ord_Mat[["bylevs"]],
      "Model" = rep(
        unique(as.character(obs.res_VegDiet_ord_Mat$Model)),
        length(metagen_obs.res_VegDiet_ord_Mat[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_ord_Mat[["k.w"]],
      totals_VegDiet_ord_Mat$N_exp,
      totals_VegDiet_ord_Mat$N_ref,
      metagen_obs.res_VegDiet_ord_Mat[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_ord_Mat[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_ord_Mat[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_ord_Mat[["I2.w"]],
      metagen_obs.res_VegDiet_ord_Mat[["tau2.w"]],
      b = metagen_obs.res_VegDiet_ord_Mat[["TE.fixed.w"]],
      se = metagen_obs.res_VegDiet_ord_Mat[["seTE.fixed.w"]],
      pval = metagen_obs.res_VegDiet_ord_Mat[["pval.fixed.w"]]
    )
  )

colnames(metagen_obs.tbl_VegDiet_ord_Mat) <-
  c(
    "Outcome",
    "Model",
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

metagen_obs.tbl_VegDiet_ord_Mat$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_Mat$Beta)
metagen_obs.tbl_VegDiet_ord_Mat$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_Mat$SE)
metagen_obs.tbl_VegDiet_ord_Mat$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_ord_Mat$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_ord_Mat$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_ord_Mat$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_ord_Mat$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_ord_Mat$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_ord_Mat$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_Mat$`N of studies`)

metagen_obs.tbl_VegDiet_ord_Mat$OR <-
  format(round(exp(metagen_obs.tbl_VegDiet_ord_Mat$Beta), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_ord_Mat$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord_Mat$Beta - 1.96 * metagen_obs.tbl_VegDiet_ord_Mat$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord_Mat$Beta + 1.96 * metagen_obs.tbl_VegDiet_ord_Mat$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_ord_Mat <- metagen_obs.tbl_VegDiet_ord_Mat %>%
  select(
    Outcome,
    Model,
    `N of studies`,
    `N exposed`,
    `N reference`,
    OR,
    `95% CI`,
    `p-value`,
    `I²`,
    `tau²`,
    b,
    se,
    pval
  )

metagen_obs.tbl_VegDiet_ord_Mat
str(metagen_obs.tbl_VegDiet_ord_Mat)  # 1 obs. = 1 outcome * 1 exposure * 1 model

write.xlsx(
  metagen_obs.tbl_VegDiet_ord_Mat,
  "ALL/IMP_PNC_meta.tbl_VegDiet_ord_Mat2.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot
metaforest_obs.res_VegDiet_ord_Mat <- forest(
  metagen_obs.res_VegDiet_ord_Mat,
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

metaforest_obs.res_VegDiet_ord_Mat <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_ord_Mat2.png",
  res = 300,
  height = 600,
  width = 2100
)
print(metaforest_obs.res_VegDiet_ord_Mat)
dev.off()

################################################################################

## Paternal Model

### Run meta-analysis
metagen_obs.res_VegDiet_ord_Pat <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_ord_Pat,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_ord_Pat

### Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_ord_Pat <-
  as.data.frame(
    cbind(
      metagen_obs.res_VegDiet_ord_Pat[["bylevs"]],
      "Model" = rep(
        unique(as.character(obs.res_VegDiet_ord_Pat$Model)),
        length(metagen_obs.res_VegDiet_ord_Pat[["bylevs"]])
      ),
      metagen_obs.res_VegDiet_ord_Pat[["k.w"]],
      totals_VegDiet_ord_Pat$N_exp,
      totals_VegDiet_ord_Pat$N_ref,
      metagen_obs.res_VegDiet_ord_Pat[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_ord_Pat[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_ord_Pat[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_ord_Pat[["I2.w"]],
      metagen_obs.res_VegDiet_ord_Pat[["tau2.w"]],
      b = metagen_obs.res_VegDiet_ord_Pat[["TE.fixed.w"]],
      se = metagen_obs.res_VegDiet_ord_Pat[["seTE.fixed.w"]],
      pval = metagen_obs.res_VegDiet_ord_Pat[["pval.fixed.w"]]
    )
  )

colnames(metagen_obs.tbl_VegDiet_ord_Pat) <-
  c(
    "Outcome",
    "Model",
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

metagen_obs.tbl_VegDiet_ord_Pat$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_Pat$Beta)
metagen_obs.tbl_VegDiet_ord_Pat$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_Pat$SE)
metagen_obs.tbl_VegDiet_ord_Pat$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_ord_Pat$`p-value`),
               digits = 3)
metagen_obs.tbl_VegDiet_ord_Pat$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_ord_Pat$`I²`) * 100,
    digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_ord_Pat$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_ord_Pat$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_ord_Pat$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_ord_Pat$`N of studies`)

metagen_obs.tbl_VegDiet_ord_Pat$OR <-
  format(round(exp(metagen_obs.tbl_VegDiet_ord_Pat$Beta), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_ord_Pat$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord_Pat$Beta - 1.96 * metagen_obs.tbl_VegDiet_ord_Pat$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord_Pat$Beta + 1.96 * metagen_obs.tbl_VegDiet_ord_Pat$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_ord_Pat <- metagen_obs.tbl_VegDiet_ord_Pat %>%
  select(
    Outcome,
    Model,
    `N of studies`,
    `N exposed`,
    `N reference`,
    OR,
    `95% CI`,
    `p-value`,
    `I²`,
    `tau²`,
    b,
    se,
    pval
  )

metagen_obs.tbl_VegDiet_ord_Pat
str(metagen_obs.tbl_VegDiet_ord_Pat)  # 1 obs. = 1 outcome * 1 exposure * 1 model

write.xlsx(
  metagen_obs.tbl_VegDiet_ord_Pat,
  "ALL/IMP_PNC_meta.tbl_VegDiet_ord_Pat2.xlsx",
  rowNames = F
)

################################################################################
################################################################################

### Meta-analysis forest plot
metaforest_obs.res_VegDiet_ord_Pat <- forest(
  metagen_obs.res_VegDiet_ord_Pat,
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

metaforest_obs.res_VegDiet_ord_Pat <- recordPlot()

png(
  "ALL/IMP_PNC_meta.forest_VegDiet_ord_Pat2.png",
  res = 300,
  height = 600,
  width = 2100
)
print(metaforest_obs.res_VegDiet_ord_Pat)
dev.off()

################################################################################
