################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 04-Aug-2025
# This script is to perform meta-analysis on main association analysis (with imputed data) results for plant-based diet indices (PDIs).
## (2) For the healthful plant-based diet index (hPDI)

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
obs.res_hPDI_bin_ALSPAC <-
  read.xlsx("ALSPAC/IMP_MAIN_obs.res_hPDI_bin.xlsx")
obs.res_hPDI_bin_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_bin_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_hPDI_bin_ALSPAC
str(obs.res_hPDI_bin_ALSPAC)  # 63 results (21 outcomes * 1 category * 3 models)

obs.res_hPDI_con_ALSPAC <-
  read.xlsx("ALSPAC/IMP_MAIN_obs.res_hPDI_con.xlsx")
obs.res_hPDI_con_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_con_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_hPDI_con_ALSPAC
str(obs.res_hPDI_con_ALSPAC)  # 12 results (4 outcomes * 1 category * 3 models)

obs.res_hPDI_ord_ALSPAC <-
  read.xlsx("ALSPAC/IMP_BF_obs.res_hPDI_ord.xlsx")
obs.res_hPDI_ord_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_ord_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_hPDI_ord_ALSPAC
str(obs.res_hPDI_ord_ALSPAC)  # 3 results (1 outcome * 1 category * 3 models)

## MoBa
obs.res_hPDI_bin_MoBa <-
  read.xlsx("MoBa/IMP_MAIN_obs.res_hPDI_bin.xlsx")
obs.res_hPDI_bin_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_bin_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_hPDI_bin_MoBa
str(obs.res_hPDI_bin_MoBa)  # 66 results (22 outcomes * 1 category * 3 models)

obs.res_hPDI_con_MoBa <-
  read.xlsx("MoBa/IMP_MAIN_obs.res_hPDI_con.xlsx")
obs.res_hPDI_con_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_con_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_hPDI_con_MoBa
str(obs.res_hPDI_con_MoBa)  # 12 results (4 outcomes * 1 category * 3 models)

obs.res_hPDI_ord_MoBa <-
  read.xlsx("MoBa/IMP_BF_obs.res_hPDI_ord.xlsx")
obs.res_hPDI_ord_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_ord_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_hPDI_ord_MoBa
str(obs.res_hPDI_ord_MoBa)  # 3 results (1 outcome * 1 category * 3 models)

## Project Viva
obs.res_hPDI_bin_Viva <-
  read.xlsx("Viva/IMP_MAIN_obs.res_hPDI_bin.xlsx")
obs.res_hPDI_bin_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_bin_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_hPDI_bin_Viva
str(obs.res_hPDI_bin_Viva)  # 51 results (17 outcomes * 1 category * 3 models)

obs.res_hPDI_con_Viva <-
  read.xlsx("Viva/IMP_MAIN_obs.res_hPDI_con.xlsx")
obs.res_hPDI_con_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_con_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_hPDI_con_Viva
str(obs.res_hPDI_con_Viva)  # 6 results (2 outcomes * 1 category * 3 models)

obs.res_hPDI_ord_Viva <-
  read.xlsx("Viva/IMP_BF_obs.res_hPDI_ord.xlsx")
obs.res_hPDI_ord_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_hPDI_ord_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_hPDI_ord_Viva
str(obs.res_hPDI_ord_Viva)  # 3 results (1 outcome * 1 category * 3 models)

################################################################################

## Combine

### Binary outcomes
obs.res_hPDI_bin_ALSPAC$Study <- "ALSPAC"
obs.res_hPDI_bin_MoBa$Study <- "MoBa"
obs.res_hPDI_bin_Viva$Study <- "Project Viva"

obs.res_hPDI_bin <-
  rbind(obs.res_hPDI_bin_ALSPAC,
        obs.res_hPDI_bin_MoBa,
        obs.res_hPDI_bin_Viva)
obs.res_hPDI_bin
str(obs.res_hPDI_bin)  # 180 results = 63 + 66 + 51

### Continuous outcomes
obs.res_hPDI_con_ALSPAC$Study <- "ALSPAC"
obs.res_hPDI_con_MoBa$Study <- "MoBa"
obs.res_hPDI_con_Viva$Study <- "Project Viva"

obs.res_hPDI_con <-
  rbind(obs.res_hPDI_con_ALSPAC,
        obs.res_hPDI_con_MoBa,
        obs.res_hPDI_con_Viva)
obs.res_hPDI_con
str(obs.res_hPDI_con)  # 30 results = 12 + 12 + 6

### Ordinal outcomes
obs.res_hPDI_ord_ALSPAC$Study <- "ALSPAC"
obs.res_hPDI_ord_MoBa$Study <- "MoBa"
obs.res_hPDI_ord_Viva$Study <- "Project Viva"

obs.res_hPDI_ord <-
  rbind(obs.res_hPDI_ord_ALSPAC,
        obs.res_hPDI_ord_MoBa,
        obs.res_hPDI_ord_Viva)
obs.res_hPDI_ord
str(obs.res_hPDI_ord)  # 9 results = 3 + 3 + 3

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
secondary_ord
dim(secondary_ord)  # 1 secondary categorical/ordinal outcome

### Identify available outcomes in the combined results
ALL_primary_bin <- subset(primary_bin, label %in% obs.res_hPDI_bin$Outcome)
ALL_primary_bin
dim(ALL_primary_bin)  # 13 primary (binary) outcomes available in the combined results

ALL_secondary_bin <- subset(
  secondary_bin,
  label %in% obs.res_hPDI_bin$Outcome &
    varname != "anaemia_preg_subsamp"
)  # Maternal anaemia (occurring during pregnancy) is only available for sensitivity analysis in MoBa
ALL_secondary_bin
dim(ALL_secondary_bin)  # 8 secondary binary outcomes available in the combined results

ALL_secondary_con <- subset(secondary_con, label %in% obs.res_hPDI_con$Outcome)
ALL_secondary_con
dim(ALL_secondary_con)  # 4 secondary continuous outcomes available in the combined results

ALL_secondary_ord <- subset(secondary_ord, label %in% obs.res_hPDI_ord$Outcome)
ALL_secondary_ord
dim(ALL_secondary_ord)  # 1 secondary categorical/ordinal outcome available in the combined results

################################################################################

## Select and prepare results for meta-analysis

### Binary outcomes
obs.res_hPDI_bin <-
  subset(obs.res_hPDI_bin, Model == "Model 3")

obs.res_hPDI_bin$Group <- NA
obs.res_hPDI_bin$Group[obs.res_hPDI_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_hPDI_bin$Group[obs.res_hPDI_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_hPDI_bin$Group[obs.res_hPDI_bin$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_hPDI_bin$Group <-
  factor(
    obs.res_hPDI_bin$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_hPDI_bin$Outcome <-
  factor(obs.res_hPDI_bin$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_hPDI_bin$Outcome])
obs.res_hPDI_bin <-
  obs.res_hPDI_bin %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_hPDI_bin$b <-
  as.numeric(obs.res_hPDI_bin$b)
obs.res_hPDI_bin$se <-
  as.numeric(obs.res_hPDI_bin$se)
obs.res_hPDI_bin$pval <-
  as.numeric(obs.res_hPDI_bin$pval)

obs.res_hPDI_bin
dim(obs.res_hPDI_bin)  # 180 / 3 = 60 obs.

################################################################################
#### Exclude outcomes with only one study - e.g., "anaemia_preg_subsamp"
obs.res_hPDI_bin <- obs.res_hPDI_bin %>%
  group_by(Outcome) %>%
  filter(n() >= 2) %>%
  ungroup()
################################################################################
#### Exposure labels shown in the forest plot
obs.res_hPDI_bin$Exposure <- "Healthful plant-based diet index (hPDI)"
################################################################################

obs.res_hPDI_bin
dim(obs.res_hPDI_bin)  # 60 -> 59 obs.

################################################################################
################################################################################

### Continuous outcomes
obs.res_hPDI_con <-
  subset(obs.res_hPDI_con, Model == "Model 3")

obs.res_hPDI_con$Group <- NA
obs.res_hPDI_con$Group[obs.res_hPDI_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_hPDI_con$Group[obs.res_hPDI_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_hPDI_con$Group[obs.res_hPDI_con$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_hPDI_con$Group <-
  factor(
    obs.res_hPDI_con$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_hPDI_con$Outcome <-
  factor(obs.res_hPDI_con$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_hPDI_con$Outcome])
obs.res_hPDI_con <-
  obs.res_hPDI_con %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_hPDI_con$b <-
  as.numeric(obs.res_hPDI_con$b)
obs.res_hPDI_con$se <-
  as.numeric(obs.res_hPDI_con$se)
obs.res_hPDI_con$pval <-
  as.numeric(obs.res_hPDI_con$pval)

################################################################################
#### Exposure labels shown in the forest plot
obs.res_hPDI_con$Exposure <- "Healthful plant-based diet index (hPDI)"
################################################################################

obs.res_hPDI_con
dim(obs.res_hPDI_con)  # 10 obs.

################################################################################
################################################################################

### Ordinal outcomes
obs.res_hPDI_ord <-
  subset(obs.res_hPDI_ord, Model == "Model 3")

obs.res_hPDI_ord$Group <- NA
obs.res_hPDI_ord$Group[obs.res_hPDI_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Pregnancy outcome")])] <-
  "Pregnancy outcome"
obs.res_hPDI_ord$Group[obs.res_hPDI_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Delivery outcome")])] <-
  "Delivery outcome"
obs.res_hPDI_ord$Group[obs.res_hPDI_ord$Outcome %in% unique(MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$grouping == "Postnatal outcome")])] <-
  "Postnatal outcome"
obs.res_hPDI_ord$Group <-
  factor(
    obs.res_hPDI_ord$Group,
    levels = c("Pregnancy outcome", "Delivery outcome", "Postnatal outcome")
  )

obs.res_hPDI_ord$Outcome <-
  factor(obs.res_hPDI_ord$Outcome,
         levels = unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_hPDI_ord$Outcome])
obs.res_hPDI_ord <-
  obs.res_hPDI_ord %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

obs.res_hPDI_ord$b <-
  as.numeric(obs.res_hPDI_ord$b)
obs.res_hPDI_ord$se <-
  as.numeric(obs.res_hPDI_ord$se)
obs.res_hPDI_ord$pval <-
  as.numeric(obs.res_hPDI_ord$pval)

################################################################################
#### Exposure labels shown in the forest plot
obs.res_hPDI_ord$Exposure <- "Healthful plant-based diet index (hPDI)"
################################################################################

obs.res_hPDI_ord
dim(obs.res_hPDI_ord)  # 3 obs.

################################################################################
## !!! Add total sample size columns for meta-analysed results (for final result tables) !!!

### Binary outcomes
obs.res_hPDI_bin <- obs.res_hPDI_bin %>%
  separate(N,
           into = c("case", "total"),
           sep = " / ",
           remove = F) %>%
  mutate(case = as.numeric(case), total = as.numeric(total))
totals_hPDI_bin <- obs.res_hPDI_bin %>%
  group_by(Outcome) %>%
  summarise(case_total = sum(case, na.rm = T),
            total_total = sum(total, na.rm = T)) %>%
  mutate(N = paste0(case_total, " / ", total_total)) %>%
  select(Outcome, N)

totals_hPDI_bin

### Continuous outcomes
totals_hPDI_con <- obs.res_hPDI_con %>%
  group_by(Outcome) %>%
  summarise(N = sum(N, na.rm = T))

totals_hPDI_con

### Ordinal outcomes
totals_hPDI_ord <- obs.res_hPDI_ord %>%
  group_by(Outcome) %>%
  summarise(N = sum(N, na.rm = T))

totals_hPDI_ord
################################################################################

#------------------------------------------------------------------------------#
#                                Meta-analysis                                 #----
#------------------------------------------------------------------------------#

# Binary outcomes

## Run meta-analysis
metagen_obs.res_hPDI_bin <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_hPDI_bin,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_hPDI_bin

## Extract and save meta-analysed results
metagen_obs.tbl_hPDI_bin <-
  as.data.frame(
    cbind(
      "Exposure" = rep(unique(
        as.character(obs.res_hPDI_bin$Exposure)
      ), length(metagen_obs.res_hPDI_bin[["bylevs"]])),
      metagen_obs.res_hPDI_bin[["bylevs"]],
      metagen_obs.res_hPDI_bin[["k.w"]],
      totals_hPDI_bin$N,
      metagen_obs.res_hPDI_bin[["TE.fixed.w"]],
      metagen_obs.res_hPDI_bin[["seTE.fixed.w"]],
      metagen_obs.res_hPDI_bin[["pval.fixed.w"]],
      metagen_obs.res_hPDI_bin[["I2.w"]],
      as.numeric(metagen_obs.res_hPDI_bin[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_hPDI_bin[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_hPDI_bin[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_hPDI_bin) <-
  c(
    "Exposure",
    "Outcome",
    "N of studies",
    "Cases/total",
    "Beta",
    "SE",
    "p-value",
    "I²",
    "b",
    "se",
    "pval"
  )

metagen_obs.tbl_hPDI_bin$Beta <-
  as.numeric(metagen_obs.tbl_hPDI_bin$Beta)
metagen_obs.tbl_hPDI_bin$SE <-
  as.numeric(metagen_obs.tbl_hPDI_bin$SE)
metagen_obs.tbl_hPDI_bin$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_hPDI_bin$`p-value`), digits = 3)
metagen_obs.tbl_hPDI_bin$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_hPDI_bin$`I²`) * 100, digits = 1
  ), nsmall = 1), "%")
metagen_obs.tbl_hPDI_bin$`N of studies` <-
  as.numeric(metagen_obs.tbl_hPDI_bin$`N of studies`)

metagen_obs.tbl_hPDI_bin$OR <-
  format(round(exp(metagen_obs.tbl_hPDI_bin$Beta), digits = 2), nsmall = 2)
metagen_obs.tbl_hPDI_bin$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_hPDI_bin$Beta - 1.96 * metagen_obs.tbl_hPDI_bin$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_hPDI_bin$Beta + 1.96 * metagen_obs.tbl_hPDI_bin$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_hPDI_bin <- metagen_obs.tbl_hPDI_bin %>%
  select(
    Outcome,
    Exposure,
    `N of studies`,
    `Cases/total`,
    OR,
    `95% CI`,
    `p-value`,
    `I²`,
    `b`,
    `se`,
    `pval`
  )

metagen_obs.tbl_hPDI_bin
str(metagen_obs.tbl_hPDI_bin)  # 21 obs. = 21 outcomes * 1 exposure

write.xlsx(metagen_obs.tbl_hPDI_bin,
           "ALL/IMP_MAIN_meta.tbl_hPDI_bin.xlsx",
           rowNames = F)

################################################################################
################################################################################

## Meta-analysis forest plot for ALL binary outcomes - Omitted

################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for pregnancy outcomes
metaforest_obs.res_hPDI_bin_Preg <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_hPDI_bin, Group == "Pregnancy outcome"),
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
  leftcols = c("studlab", "N", "w.common"),
  leftlabs = c("Study", "Cases/total", "Weight"),
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
metaforest_obs.res_hPDI_bin_Preg <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_hPDI_bin_Preg.png",
  res = 300,
  height = 2750,
  width = 2100
)
print(metaforest_obs.res_hPDI_bin_Preg)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for delivery outcomes
metaforest_obs.res_hPDI_bin_Deli <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_hPDI_bin, Group == "Delivery outcome"),
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
  leftcols = c("studlab", "N", "w.common"),
  leftlabs = c("Study", "Cases/total", "Weight"),
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
metaforest_obs.res_hPDI_bin_Deli <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_hPDI_bin_Deli.png",
  res = 300,
  height = 5200,
  width = 2100
)
print(metaforest_obs.res_hPDI_bin_Deli)
dev.off()
################################################################################
################################################################################
################################################################################
### Meta-analysis forest plot for postnatal outcomes
metaforest_obs.res_hPDI_bin_Post <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = subset(obs.res_hPDI_bin, Group == "Postnatal outcome"),
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
  leftcols = c("studlab", "N", "w.common"),
  leftlabs = c("Study", "Cases/total", "Weight"),
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
metaforest_obs.res_hPDI_bin_Post <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_hPDI_bin_Post.png",
  res = 300,
  height = 1300,
  width = 2100
)
print(metaforest_obs.res_hPDI_bin_Post)
dev.off()

################################################################################

# Continuous outcomes

## Run meta-analysis
metagen_obs.res_hPDI_con <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_hPDI_con,
    sm = "",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_hPDI_con

## Extract and save meta-analysed results
metagen_obs.tbl_hPDI_con <-
  as.data.frame(
    cbind(
      "Exposure" = rep(unique(
        as.character(obs.res_hPDI_con$Exposure)
      ), length(metagen_obs.res_hPDI_con[["bylevs"]])),
      metagen_obs.res_hPDI_con[["bylevs"]],
      metagen_obs.res_hPDI_con[["k.w"]],
      totals_hPDI_con$N,
      metagen_obs.res_hPDI_con[["TE.fixed.w"]],
      metagen_obs.res_hPDI_con[["seTE.fixed.w"]],
      metagen_obs.res_hPDI_con[["pval.fixed.w"]],
      metagen_obs.res_hPDI_con[["I2.w"]],
      as.numeric(metagen_obs.res_hPDI_con[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_hPDI_con[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_hPDI_con[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_hPDI_con) <-
  c(
    "Exposure",
    "Outcome",
    "N of studies",
    "N",
    "Beta",
    "SE",
    "p-value",
    "I²",
    "b",
    "se",
    "pval"
  )

metagen_obs.tbl_hPDI_con$Beta <-
  as.numeric(metagen_obs.tbl_hPDI_con$Beta)
metagen_obs.tbl_hPDI_con$SE <-
  as.numeric(metagen_obs.tbl_hPDI_con$SE)
metagen_obs.tbl_hPDI_con$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_hPDI_con$`p-value`), digits = 3)
metagen_obs.tbl_hPDI_con$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_hPDI_con$`I²`) * 100, digits = 1
  ), nsmall = 1), "%")
metagen_obs.tbl_hPDI_con$`N of studies` <-
  as.numeric(metagen_obs.tbl_hPDI_con$`N of studies`)

metagen_obs.tbl_hPDI_con$`95% CI` <- paste0(format(
  round(
    metagen_obs.tbl_hPDI_con$Beta - 1.96 * metagen_obs.tbl_hPDI_con$SE,
    digits = 2
  ),
  nsmall = 2
), ", ", format(
  round(
    metagen_obs.tbl_hPDI_con$Beta + 1.96 * metagen_obs.tbl_hPDI_con$SE,
    digits = 2
  ),
  nsmall = 2
))
metagen_obs.tbl_hPDI_con$Beta <-
  format(round(metagen_obs.tbl_hPDI_con$Beta, digits = 2), nsmall = 2)

metagen_obs.tbl_hPDI_con <- metagen_obs.tbl_hPDI_con %>%
  select(Outcome,
         Exposure,
         `N of studies`,
         `N`,
         Beta,
         `95% CI`,
         `p-value`,
         `I²`,
         `b`,
         `se`,
         `pval`)

metagen_obs.tbl_hPDI_con
str(metagen_obs.tbl_hPDI_con)  # 4 obs. = 4 outcomes * 1 exposure

write.xlsx(metagen_obs.tbl_hPDI_con,
           "ALL/IMP_MAIN_meta.tbl_hPDI_con.xlsx",
           rowNames = F)

################################################################################
################################################################################

## Meta-analysis forest plot
metaforest_obs.res_hPDI_con <- forest(
  metagen_obs.res_hPDI_con,
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
  leftcols = c("studlab", "N", "w.common"),
  leftlabs = c("Study", "N", "Weight"),
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

metaforest_obs.res_hPDI_con <- recordPlot()

png(
  "ALL/IMP_MAIN_meta.forest_hPDI_con.png",
  res = 300,
  height = 1800,
  width = 2100
)
print(metaforest_obs.res_hPDI_con)
dev.off()

################################################################################

# Ordinal outcomes

## Run meta-analysis
metagen_obs.res_hPDI_ord <-
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_hPDI_ord,
    sm = "OR",
    common = T,
    random = F,
    subgroup = Outcome,
    print.subgroup.name = F
  )

metagen_obs.res_hPDI_ord

## Extract and save meta-analysed results
metagen_obs.tbl_hPDI_ord <-
  as.data.frame(
    cbind(
      "Exposure" = rep(unique(
        as.character(obs.res_hPDI_ord$Exposure)
      ), length(metagen_obs.res_hPDI_ord[["bylevs"]])),
      metagen_obs.res_hPDI_ord[["bylevs"]],
      metagen_obs.res_hPDI_ord[["k.w"]],
      totals_hPDI_ord$N,
      metagen_obs.res_hPDI_ord[["TE.fixed.w"]],
      metagen_obs.res_hPDI_ord[["seTE.fixed.w"]],
      metagen_obs.res_hPDI_ord[["pval.fixed.w"]],
      metagen_obs.res_hPDI_ord[["I2.w"]],
      as.numeric(metagen_obs.res_hPDI_ord[["TE.fixed.w"]]),
      as.numeric(metagen_obs.res_hPDI_ord[["seTE.fixed.w"]]),
      as.numeric(metagen_obs.res_hPDI_ord[["pval.fixed.w"]])
    )
  )

colnames(metagen_obs.tbl_hPDI_ord) <-
  c(
    "Exposure",
    "Outcome",
    "N of studies",
    "N",
    "Beta",
    "SE",
    "p-value",
    "I²",
    "b",
    "se",
    "pval"
  )

metagen_obs.tbl_hPDI_ord$Beta <-
  as.numeric(metagen_obs.tbl_hPDI_ord$Beta)
metagen_obs.tbl_hPDI_ord$SE <-
  as.numeric(metagen_obs.tbl_hPDI_ord$SE)
metagen_obs.tbl_hPDI_ord$`p-value` <-
  style_pvalue(as.numeric(metagen_obs.tbl_hPDI_ord$`p-value`), digits = 3)
metagen_obs.tbl_hPDI_ord$`I²` <-
  paste0(format(round(
    as.numeric(metagen_obs.tbl_hPDI_ord$`I²`) * 100, digits = 1
  ), nsmall = 1), "%")
metagen_obs.tbl_hPDI_ord$`N of studies` <-
  as.numeric(metagen_obs.tbl_hPDI_ord$`N of studies`)

metagen_obs.tbl_hPDI_ord$OR <-
  format(round(exp(metagen_obs.tbl_hPDI_ord$Beta), digits = 2), nsmall = 2)
metagen_obs.tbl_hPDI_ord$`95% CI` <- paste0(format(round(
  exp(
    metagen_obs.tbl_hPDI_ord$Beta - 1.96 * metagen_obs.tbl_hPDI_ord$SE
  ),
  digits = 2
), nsmall = 2), ", ", format(round(
  exp(
    metagen_obs.tbl_hPDI_ord$Beta + 1.96 * metagen_obs.tbl_hPDI_ord$SE
  ),
  digits = 2
), nsmall = 2))

metagen_obs.tbl_hPDI_ord <- metagen_obs.tbl_hPDI_ord %>%
  select(Outcome,
         Exposure,
         `N of studies`,
         `N`,
         OR,
         `95% CI`,
         `p-value`,
         `I²`,
         `b`,
         `se`,
         `pval`)

metagen_obs.tbl_hPDI_ord
str(metagen_obs.tbl_hPDI_ord)  # 1 obs. = 1 outcome * 1 exposure

write.xlsx(metagen_obs.tbl_hPDI_ord,
           "ALL/IMP_MAIN_meta.tbl_hPDI_ord.xlsx",
           rowNames = F)

################################################################################
################################################################################

### Meta-analysis forest plot
metaforest_obs.res_hPDI_ord <- forest(
  metagen(
    TE = b,
    seTE = se,
    studlab = Study,
    data = obs.res_hPDI_ord,
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
  leftcols = c("studlab", "N", "w.common"),
  leftlabs = c("Study", "N", "Weight"),
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
metaforest_obs.res_hPDI_ord <- recordPlot()
png(
  "ALL/IMP_MAIN_meta.forest_hPDI_ord.png",
  res = 300,
  height = 700,
  width = 2100
)
print(metaforest_obs.res_hPDI_ord)
dev.off()

#------------------------------------------------------------------------------#
#                                Combine Plots                                 #----
#------------------------------------------------------------------------------#

# Finalise result presentation

## FDR correction for multiple testing
pval_bin <-
  as.data.frame(cbind(
    "Exposure" = rep(unique(
      as.character(obs.res_hPDI_bin$Exposure)
    ), length(metagen_obs.res_hPDI_bin[["bylevs"]])),
    "Outcome" = metagen_obs.res_hPDI_bin[["bylevs"]],
    "pval" = as.numeric(metagen_obs.res_hPDI_bin[["pval.fixed.w"]])
  ))

pval_con <-
  as.data.frame(cbind(
    "Exposure" = rep(unique(
      as.character(obs.res_hPDI_con$Exposure)
    ), length(metagen_obs.res_hPDI_con[["bylevs"]])),
    "Outcome" = metagen_obs.res_hPDI_con[["bylevs"]],
    "pval" = as.numeric(metagen_obs.res_hPDI_con[["pval.fixed.w"]])
  ))

pval_ord <-
  as.data.frame(cbind(
    "Exposure" = rep(unique(
      as.character(obs.res_hPDI_ord$Exposure)
    ), length(metagen_obs.res_hPDI_ord[["bylevs"]])),
    "Outcome" = metagen_obs.res_hPDI_ord[["bylevs"]],
    "pval" = as.numeric(metagen_obs.res_hPDI_ord[["pval.fixed.w"]])
  ))

pval_all <- rbind(pval_bin, pval_con, pval_ord)

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
pval_all

## Combine results with FDR-adjusted p-values

### Binary outcomes
metagen_obs.tbl_hPDI_bin <- left_join(metagen_obs.tbl_hPDI_bin,
                                      pval_all,
                                      by = c("Outcome", "Exposure"))
metagen_obs.tbl_hPDI_bin <- metagen_obs.tbl_hPDI_bin %>%
  relocate(c(`I²`, `b`, `se`, `pval`), .after = last_col())
metagen_obs.tbl_hPDI_bin
str(metagen_obs.tbl_hPDI_bin)
write.xlsx(metagen_obs.tbl_hPDI_bin,
           "ALL/IMP_MAIN_meta.tbl_hPDI_bin.xlsx",
           rowNames = F)

### Continuous outcomes
metagen_obs.tbl_hPDI_con <- left_join(metagen_obs.tbl_hPDI_con,
                                      pval_all,
                                      by = c("Outcome", "Exposure"))
metagen_obs.tbl_hPDI_con <- metagen_obs.tbl_hPDI_con %>%
  relocate(c(`I²`, `b`, `se`, `pval`), .after = last_col())
metagen_obs.tbl_hPDI_con
str(metagen_obs.tbl_hPDI_con)
write.xlsx(metagen_obs.tbl_hPDI_con,
           "ALL/IMP_MAIN_meta.tbl_hPDI_con.xlsx",
           rowNames = F)

### Ordinal outcomes
metagen_obs.tbl_hPDI_ord <- left_join(metagen_obs.tbl_hPDI_ord,
                                      pval_all,
                                      by = c("Outcome", "Exposure"))
metagen_obs.tbl_hPDI_ord <- metagen_obs.tbl_hPDI_ord %>%
  relocate(c(`I²`, `b`, `se`, `pval`), .after = last_col())
metagen_obs.tbl_hPDI_ord
str(metagen_obs.tbl_hPDI_ord)
write.xlsx(metagen_obs.tbl_hPDI_ord,
           "ALL/IMP_MAIN_meta.tbl_hPDI_ord.xlsx",
           rowNames = F)

################################################################################
