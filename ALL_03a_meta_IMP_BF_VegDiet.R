################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 19-Jan-2025
# This script is to perform meta-analysis on (negative control outcome ???) analysis on breastfeeding (with imputed data) results for vegetarian diets.

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
  read.xlsx("ALSPAC/IMP_BF_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_ALSPAC
str(obs.res_VegDiet_ord_ALSPAC)  # 3 results (1 outcome * 1 category * 3 models)

## BiB
obs.res_VegDiet_ord_BiB <-
  read.xlsx("BiB/IMP_BF_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_BiB[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_BiB[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_BiB
str(obs.res_VegDiet_ord_BiB)  # 3 results (1 outcome * 1 category * 3 models)

## MoBa
obs.res_VegDiet_ord_MoBa <-
  read.xlsx("MoBa/IMP_BF_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_MoBa
str(obs.res_VegDiet_ord_MoBa)  # 3 results (1 outcome * 1 category * 3 models)

## Project Viva
obs.res_VegDiet_ord_Viva <-
  read.xlsx("Viva/IMP_BF_obs.res_VegDiet_ord.xlsx")
obs.res_VegDiet_ord_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_ord_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_ord_Viva
str(obs.res_VegDiet_ord_Viva)  # 3 results (1 outcome * 1 category * 3 models)

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
str(obs.res_VegDiet_ord)  # 12 results = 3 + 3 + 3 + 3

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
obs.res_VegDiet_ord$Exposure <- "Pesco-/full vs. non-vegetarian"
################################################################################

obs.res_VegDiet_ord
dim(obs.res_VegDiet_ord)  # 4 obs.

################################################################################
## !!! Add total sample size columns for meta-analysed results (for final result tables) !!!
totals_VegDiet_ord <- obs.res_VegDiet_ord %>%
  summarise(N_exp = sum(N_exp, na.rm = T),
            N_ref = sum(N_ref, na.rm = T))

totals_VegDiet_ord
################################################################################

#------------------------------------------------------------------------------#
#                                Meta-analysis                                 #----
#------------------------------------------------------------------------------#

# Run meta-analysis
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

# Extract and save meta-analysed results
metagen_obs.tbl_VegDiet_ord <-
  as.data.frame(
    cbind(
      metagen_obs.res_VegDiet_ord[["bylevs"]],
      "Outcome" = rep(unique(
        as.character(obs.res_VegDiet_ord$Outcome)
      ), length(metagen_obs.res_VegDiet_ord[["bylevs"]])),
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

write.xlsx(metagen_obs.tbl_VegDiet_ord,
           "ALL/IMP_BF_meta.tbl_VegDiet_ord.xlsx",
           rowNames = F)

################################################################################

# Meta-analysis forest plot
metaforest_obs.res_VegDiet_ord <- forest(
  metagen_obs.res_VegDiet_ord,
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

metaforest_obs.res_VegDiet_ord <- recordPlot()

png(
  "ALL/IMP_BF_meta.forest_VegDiet_ord.png",
  res = 300,
  height = 800,
  width = 2200
)
print(metaforest_obs.res_VegDiet_ord)
dev.off()

#------------------------------------------------------------------------------#
#                              Follow-up Analyses                              #----
#------------------------------------------------------------------------------#

# Examine potential residual confounding by SEP (i.e., maternal education and IMD/household income -> vegetarianism and breastfeeding)

## Load cohort-specific results

### ALSPAC
obs.res_VegDiet_SEP_ALSPAC <- read.xlsx("ALSPAC/IMP_BF_obs.res_VegDiet_SEP.xlsx")
obs.res_VegDiet_SEP_ALSPAC$Study <- "ALSPAC"
obs.res_VegDiet_SEP_ALSPAC$Outcome[obs.res_VegDiet_SEP_ALSPAC$Outcome ==
                                     "Pesco-/full vegetarianism"] <- "Pesco-/full vs. non-vegetarian"
obs.res_VegDiet_SEP_ALSPAC$Outcome[obs.res_VegDiet_SEP_ALSPAC$Outcome ==
                                     "Breastfeeding duration"] <- "Longer vs. shorter breastfeeding"
obs.res_VegDiet_SEP_ALSPAC$Outcome <- factor(
  obs.res_VegDiet_SEP_ALSPAC$Outcome,
  levels = c(
    "Pesco-/full vs. non-vegetarian",
    "Longer vs. shorter breastfeeding"
  )
)
obs.res_VegDiet_SEP_ALSPAC$Exposure[obs.res_VegDiet_SEP_ALSPAC$Exposure ==
                                      "Maternal education"] <- "Maternal education (higher vs. lower)"
obs.res_VegDiet_SEP_ALSPAC

### BiB
obs.res_VegDiet_SEP_BiB <- read.xlsx("BiB/IMP_BF_obs.res_VegDiet_SEP.xlsx")
obs.res_VegDiet_SEP_BiB$Study <- "BiB"
obs.res_VegDiet_SEP_BiB$Outcome[obs.res_VegDiet_SEP_BiB$Outcome ==
                                  "Pesco-/full vegetarianism"] <- "Pesco-/full vs. non-vegetarian"
obs.res_VegDiet_SEP_BiB$Outcome[obs.res_VegDiet_SEP_BiB$Outcome ==
                                  "Breastfeeding duration"] <- "Longer vs. shorter breastfeeding"
obs.res_VegDiet_SEP_BiB$Outcome <- factor(
  obs.res_VegDiet_SEP_BiB$Outcome,
  levels = c(
    "Pesco-/full vs. non-vegetarian",
    "Longer vs. shorter breastfeeding"
  )
)
obs.res_VegDiet_SEP_BiB$Exposure[obs.res_VegDiet_SEP_BiB$Exposure ==
                                   "Maternal education"] <- "Maternal education (higher vs. lower)"
obs.res_VegDiet_SEP_BiB

### MoBa
obs.res_VegDiet_SEP_MoBa <- read.xlsx("MoBa/IMP_BF_obs.res_VegDiet_SEP.xlsx")
obs.res_VegDiet_SEP_MoBa$Study <- "MoBa"
obs.res_VegDiet_SEP_MoBa$Outcome[obs.res_VegDiet_SEP_MoBa$Outcome ==
                                   "Pesco-/full vegetarianism"] <- "Pesco-/full vs. non-vegetarian"
obs.res_VegDiet_SEP_MoBa$Outcome[obs.res_VegDiet_SEP_MoBa$Outcome ==
                                   "Breastfeeding duration"] <- "Longer vs. shorter breastfeeding"
obs.res_VegDiet_SEP_MoBa$Outcome <- factor(
  obs.res_VegDiet_SEP_MoBa$Outcome,
  levels = c(
    "Pesco-/full vs. non-vegetarian",
    "Longer vs. shorter breastfeeding"
  )
)
obs.res_VegDiet_SEP_MoBa$Exposure[obs.res_VegDiet_SEP_MoBa$Exposure ==
                                    "Maternal education"] <- "Maternal education (higher vs. lower)"
obs.res_VegDiet_SEP_MoBa$Exposure[obs.res_VegDiet_SEP_MoBa$Exposure ==
                                    "Household income"] <- "Household income (higher vs. lower)"
obs.res_VegDiet_SEP_MoBa

### Project Viva
obs.res_VegDiet_SEP_Viva <- read.xlsx("Viva/IMP_BF_obs.res_VegDiet_SEP.xlsx")
obs.res_VegDiet_SEP_Viva$Study <- "Project Viva"
obs.res_VegDiet_SEP_Viva$Outcome[obs.res_VegDiet_SEP_Viva$Outcome ==
                                   "Pesco-/full vegetarianism"] <- "Pesco-/full vs. non-vegetarian"
obs.res_VegDiet_SEP_Viva$Outcome[obs.res_VegDiet_SEP_Viva$Outcome ==
                                   "Breastfeeding duration"] <- "Longer vs. shorter breastfeeding"
obs.res_VegDiet_SEP_Viva$Outcome <- factor(
  obs.res_VegDiet_SEP_Viva$Outcome,
  levels = c(
    "Pesco-/full vs. non-vegetarian",
    "Longer vs. shorter breastfeeding"
  )
)
obs.res_VegDiet_SEP_Viva$Exposure[obs.res_VegDiet_SEP_Viva$Exposure ==
                                    "Maternal education"] <- "Maternal education (higher vs. lower)"
obs.res_VegDiet_SEP_Viva$Exposure[obs.res_VegDiet_SEP_Viva$Exposure ==
                                    "Household income"] <- "Household income (higher vs. lower)"
obs.res_VegDiet_SEP_Viva

### Combine
obs.res_VegDiet_SEP <-
  rbind(
    obs.res_VegDiet_SEP_ALSPAC,
    obs.res_VegDiet_SEP_BiB,
    obs.res_VegDiet_SEP_MoBa,
    obs.res_VegDiet_SEP_Viva
  )

obs.res_VegDiet_SEP
str(obs.res_VegDiet_SEP)

## Nightingale forest plots
colnames(obs.res_VegDiet_SEP)[colnames(obs.res_VegDiet_SEP) == "Exposure"] <- "SEP indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_SEP$`SEP indicator` <- factor(
  obs.res_VegDiet_SEP$`SEP indicator`,
  levels = c(
    "Household income (higher vs. lower)",
    "IMD (less vs. more deprived)",
    "Maternal education (higher vs. lower)"
  )
)

obs.forest_VegDiet_SEP <- ggforestplot::forestplot(
  df = obs.res_VegDiet_SEP,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `SEP indicator`,
  shape = `SEP indicator`,
  xlab = "OR and 95% CI",
  title = "Effect of socioeconimic position (SEP)",
  logodds = T
) +
  ggplot2::scale_colour_manual(
    values = c("darkred", "blueviolet", "darkorange"),
    breaks = c(
      "Maternal education (higher vs. lower)",
      "IMD (less vs. more deprived)",
      "Household income (higher vs. lower)"
    )
  ) +
  ggplot2::scale_shape_manual(
    values = c(21, 21, 21),
    breaks = c(
      "Maternal education (higher vs. lower)",
      "IMD (less vs. more deprived)",
      "Household income (higher vs. lower)"
    )
  ) +
  ggforce::facet_col(facets = ~ Study,
                     scales = "free_y",
                     space = "free")

obs.forest_VegDiet_SEP

ggsave(
  obs.forest_VegDiet_SEP,
  file = "ALL/IMP_BF_obs.forest_VegDiet_SEP_ALL.png",
  height = 6,
  width = 10
)

#------------------------------------------------------------------------------#
#                                Combine Plots                                 #----
#------------------------------------------------------------------------------#

# Combine meta-analysis and SEP forest plots
png("dummy.png")
replayPlot(metaforest_obs.res_VegDiet_ord)
grob1 <- grid.grab()

grid.newpage()

obs.forest_VegDiet_SEP <- obs.forest_VegDiet_SEP + ggplot2::theme(
  plot.title = ggplot2::element_blank(),
  legend.position = "bottom",
  legend.box.margin = ggplot2::margin(r = 200),
  plot.margin = ggplot2::margin(r = 100, l = 100)
) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = NULL),
    shape = ggplot2::guide_legend(title = NULL)
  )
print(obs.forest_VegDiet_SEP)
grob2 <- grid.grab()
dev.off()

title1 <- textGrob(
  "Association between maternal vegetarian diet adherence and breastfeeding duration",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title2 <- textGrob(
  "Effect of socioeconomic position (SEP) on maternal vegetarian diet adherence and breastfeeding duration",
  gp = gpar(fontsize = 12, fontface = "bold")
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.05, 0.95))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.05, 0.95))

png(
  "ALL/Comb_IMP_BF.meta.forest_SEP.obs.forest_VegDiet.png",
  res = 300,
  height = 2300,
  width = 2800
)
grid.arrange(
  grob1_with_title,
  grob2_with_title,
  ncol = 1,
  nrow = 2,
  heights = c(1, 1.8)
)
dev.off()
