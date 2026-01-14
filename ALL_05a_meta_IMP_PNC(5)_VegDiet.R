################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 20-Jan-2025
# This script is to perform meta-analysis on paternal negative control analysis (with imputed data) results for vegetarian diets.
## Part 5: Meta-analysis of ALSPAC and MoBa main results only (for interpretation)

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

obs.res_VegDiet_ord_ALSPAC <-
  read.xlsx("ALSPAC/IMP_BF_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_ALSPAC
str(obs.res_VegDiet_ord_ALSPAC)  # 3 results (1 outcome * 1 category * 3 models)

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

obs.res_VegDiet_ord_MoBa <-
  read.xlsx("MoBa/IMP_BF_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_MoBa
str(obs.res_VegDiet_ord_MoBa)  # 3 results (1 outcome * 1 category * 3 models)

################################################################################

## Combine

### Binary outcomes
obs.res_VegDiet_bin_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_bin_MoBa$Study <- "MoBa"

obs.res_VegDiet_bin <-
  rbind(obs.res_VegDiet_bin_ALSPAC, obs.res_VegDiet_bin_MoBa)
obs.res_VegDiet_bin
str(obs.res_VegDiet_bin)  # 129 results = 63 + 66

### Continuous outcomes
obs.res_VegDiet_con_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_con_MoBa$Study <- "MoBa"

obs.res_VegDiet_con <-
  rbind(obs.res_VegDiet_con_ALSPAC, obs.res_VegDiet_con_MoBa)
obs.res_VegDiet_con
str(obs.res_VegDiet_con)  # 24 results = 12 + 12

### Ordinal outcomes
obs.res_VegDiet_ord_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_ord_MoBa$Study <- "MoBa"

obs.res_VegDiet_ord <-
  rbind(obs.res_VegDiet_ord_ALSPAC, obs.res_VegDiet_ord_MoBa)
obs.res_VegDiet_ord
str(obs.res_VegDiet_ord)  # 6 results = 3 + 3

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
dim(obs.res_VegDiet_bin)  # 129 / 3 = 43 obs.

################################################################################
#### Exclude outcomes with only one study
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
dim(obs.res_VegDiet_bin)  # 43 -> 42 obs.

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
dim(obs.res_VegDiet_con)  # 8 obs.

################################################################################
################################################################################

### Ordinal outcomes
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
#### Exposure labels shown in the forest plot
obs.res_VegDiet_ord$Exposure[obs.res_VegDiet_ord$Exposure == "Pesco-/full vegetarian"] <- "Pesco-/full vs. non-vegetarian"
################################################################################

obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 2 obs.

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

### Ordinal outcomes
totals_VegDiet_ord <- obs.res_VegDiet_ord %>%
  group_by(Outcome) %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))
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
    "tau²",
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
metagen_obs.tbl_VegDiet_bin$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_bin$`tau²`))
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
    `tau²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_bin
str(metagen_obs.tbl_VegDiet_bin)  # 21 obs. = 21 outcomes * 1 exposure

write.xlsx(
  metagen_obs.tbl_VegDiet_bin,
  "ALL/IMP_MAIN_meta.tbl_VegDiet_bin_ALSPAC.MoBa.xlsx",
  rowNames = F
)

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
    "tau²",
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
metagen_obs.tbl_VegDiet_con$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_con$`tau²`))
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
    `tau²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_VegDiet_con
str(metagen_obs.tbl_VegDiet_con)  # 4 obs. = 4 outcomes * 1 exposure

write.xlsx(
  metagen_obs.tbl_VegDiet_con,
  "ALL/IMP_MAIN_meta.tbl_VegDiet_con_ALSPAC.MoBa.xlsx",
  rowNames = F
)

################################################################################

# Ordinal outcomes

## Run meta-analysis
metagen_obs.res_VegDiet_ord <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_VegDiet_ord,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_VegDiet_ord

## Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_ord <-
  as.data.frame(
    cbind(
      "Exposure" = rep(unique(
        as.character(obs.res_VegDiet_ord$Exposure)
      ), length(metagen_obs.res_VegDiet_ord[["bylevs"]])),
      metagen_obs.res_VegDiet_ord[["bylevs"]],
      metagen_obs.res_VegDiet_ord[["k.w"]],
      totals_VegDiet_ord$N_exp,
      totals_VegDiet_ord$N_ref,
      metagen_obs.res_VegDiet_ord[["TE.fixed.w"]],
      metagen_obs.res_VegDiet_ord[["seTE.fixed.w"]],
      metagen_obs.res_VegDiet_ord[["pval.fixed.w"]],
      metagen_obs.res_VegDiet_ord[["I2.w"]],
      metagen_obs.res_VegDiet_ord[["tau2.w"]],
      as.numeric(metagen_obs.res_VegDiet_ord[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_ord[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_VegDiet_ord[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_VegDiet_ord) <-
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

metagen_obs.tbl_VegDiet_ord$Beta <-
  as.numeric(metagen_obs.tbl_VegDiet_ord$Beta)
metagen_obs.tbl_VegDiet_ord$SE <-
  as.numeric(metagen_obs.tbl_VegDiet_ord$SE)
metagen_obs.tbl_VegDiet_ord$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_VegDiet_ord$`p-value`), digits = 3)
metagen_obs.tbl_VegDiet_ord$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_VegDiet_ord$`I²`) * 100, digits = 1
  ), nsmall = 1), "%")
################################################################################
style_tau2 <- function(my_tau2) {
  new_tau2 <- format(round(my_tau2, digits = 4), nsmall = 4)
  new_tau2[new_tau2 == "0.0000"] <- "<0.0001"
  new_tau2
}
metagen_obs.tbl_VegDiet_ord$`tau²` <-
  style_tau2(as.numeric(metagen_obs.tbl_VegDiet_ord$`tau²`))
################################################################################
metagen_obs.tbl_VegDiet_ord$`N of studies` <-
  as.numeric(metagen_obs.tbl_VegDiet_ord$`N of studies`)

metagen_obs.tbl_VegDiet_ord$OR <-
  format(round(exp(metagen_obs.tbl_VegDiet_ord$Beta), digits = 2), nsmall = 2)
metagen_obs.tbl_VegDiet_ord$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord$Beta - 1.96 * metagen_obs.tbl_VegDiet_ord$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_VegDiet_ord$Beta + 1.96 * metagen_obs.tbl_VegDiet_ord$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_VegDiet_ord <- metagen_obs.tbl_VegDiet_ord %>%
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

metagen_obs.tbl_VegDiet_ord
str(metagen_obs.tbl_VegDiet_ord)  # 1 obs. = 1 outcome * 1 exposure

write.xlsx(
  metagen_obs.tbl_VegDiet_ord,
  "ALL/IMP_BF_meta.tbl_VegDiet_ord_ALSPAC.MoBa.xlsx",
  rowNames = F
)
