################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 19-Jan-2025
# This script is to combine cohort-specific results for the effect of socioeconomic position (SEP) on both vegetarianism and perinatal outcomes (with imputed data).

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
               ggplot2,
               reshape2,
               gridExtra,
               patchwork)

# Set working directory
setwd("Z:/working/results/")

#------------------------------------------------------------------------------#
#                               Data Preparation                               #----
#------------------------------------------------------------------------------#

# Outcome grouping

## Load outcome lists and labels
MRPREG_outcome_labels <-
  read.xlsx("Z:/working/data/MRPREG_outcome_labels.xlsx", sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

################################################################################

# Load and prepare cohort-specific results

## ALSPAC
obs.res_VegDiet_bin_con_ALSPAC <-
  read.xlsx("ALSPAC/IMP_SEP_obs.res_VegDiet.subgroup_bin_con.xlsx")

################################################################################
### !!! Add in SEP effect on breastfeeding !!!
SEP_BF_ALSPAC <- read.xlsx("ALSPAC/IMP_BF_obs.res_VegDiet.subgroup_SEP.xlsx")
SEP_BF_ALSPAC$Exposure[SEP_BF_ALSPAC$Exposure ==
                         "Maternal education"] <- "Maternal education (higher vs. lower)"
SEP_BF_ALSPAC <- SEP_BF_ALSPAC[, c(2, 1, 3:ncol(SEP_BF_ALSPAC))]
colnames(SEP_BF_ALSPAC) <- colnames(obs.res_VegDiet_bin_con_ALSPAC)
SEP_BF_ALSPAC <- SEP_BF_ALSPAC[!(SEP_BF_ALSPAC$Outcome %in% c("Pesco-vegetarianism", "Full vegetarianism")), ]
obs.res_VegDiet_bin_con_ord_ALSPAC <- rbind(obs.res_VegDiet_bin_con_ALSPAC, SEP_BF_ALSPAC)
################################################################################

obs.res_VegDiet_bin_con_ord_ALSPAC[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_con_ord_ALSPAC[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_con_ord_ALSPAC$`Beta` <- as.numeric(obs.res_VegDiet_bin_con_ord_ALSPAC$b)
obs.res_VegDiet_bin_con_ord_ALSPAC$`Beta/SE` <- as.numeric(obs.res_VegDiet_bin_con_ord_ALSPAC$b / obs.res_VegDiet_bin_con_ord_ALSPAC$se)

obs.res_VegDiet_bin_con_ord_ALSPAC$Study <- "ALSPAC"

obs.res_VegDiet_bin_con_ord_ALSPAC$Group <- "Pregnancy & perinatal outcome (binary)"
obs.res_VegDiet_bin_con_ord_ALSPAC$Group[obs.res_VegDiet_bin_con_ord_ALSPAC$Outcome %in% c(
  "Pesco-/full vs. non-vegetarian",
  "Pesco- vs. non-vegetarian",
  "Full vs. non-vegetarian"
)] <- "Vegetarianism (binary)"
obs.res_VegDiet_bin_con_ord_ALSPAC$Group[obs.res_VegDiet_bin_con_ord_ALSPAC$Outcome %in% c(
  "Gestational age (weeks)",
  "Birth weight z-score",
  "Apgar score at 1 minute",
  "Apgar score at 5 minutes"
)] <- "Pregnancy & perinatal outcome (continuous)"
obs.res_VegDiet_bin_con_ord_ALSPAC$Group[obs.res_VegDiet_bin_con_ord_ALSPAC$Outcome %in% c("Breastfeeding duration")] <- "Pregnancy & perinatal outcome (ordinal)"
obs.res_VegDiet_bin_con_ord_ALSPAC$Group <-
  factor(
    obs.res_VegDiet_bin_con_ord_ALSPAC$Group,
    levels = c(
      "Vegetarianism (binary)",
      "Pregnancy & perinatal outcome (binary)",
      "Pregnancy & perinatal outcome (continuous)",
      "Pregnancy & perinatal outcome (ordinal)"
    )
  )

obs.res_VegDiet_bin_con_ord_ALSPAC$Outcome <-
  factor(obs.res_VegDiet_bin_con_ord_ALSPAC$Outcome, levels = rev(
    c(
      "Pesco-/full vs. non-vegetarian",
      "Pesco- vs. non-vegetarian",
      "Full vs. non-vegetarian",
      unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin_con_ord_ALSPAC$Outcome]
    )
  ))
obs.res_VegDiet_bin_con_ord_ALSPAC <-
  obs.res_VegDiet_bin_con_ord_ALSPAC %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

colnames(obs.res_VegDiet_bin_con_ord_ALSPAC)[colnames(obs.res_VegDiet_bin_con_ord_ALSPAC) == "SEP"] <- "SEP indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord_ALSPAC$`SEP indicator` <- gsub(" \\(",
                                                           "\n(",
                                                           obs.res_VegDiet_bin_con_ord_ALSPAC$`SEP indicator`)  # Split the SEP indicator in two lines
obs.res_VegDiet_bin_con_ord_ALSPAC$`SEP indicator` <- factor(
  obs.res_VegDiet_bin_con_ord_ALSPAC$`SEP indicator`,
  levels = c(
    "Maternal education\n(higher vs. lower)",
    "IMD\n(more vs. less affluent)"
  )
)

obs.res_VegDiet_bin_con_ord_ALSPAC
str(obs.res_VegDiet_bin_con_ord_ALSPAC)  # 29 outcomes * 2 models = 58 obs.

## BiB
obs.res_VegDiet_bin_con_BiB <-
  read.xlsx("BiB/IMP_SEP_obs.res_VegDiet.subgroup_bin_con.xlsx")

################################################################################
### !!! Add in SEP effect on breastfeeding !!!
SEP_BF_BiB <- read.xlsx("BiB/IMP_BF_obs.res_VegDiet.subgroup_SEP.xlsx")
SEP_BF_BiB$Exposure[SEP_BF_BiB$Exposure ==
                      "Maternal education"] <- "Maternal education (higher vs. lower)"
SEP_BF_BiB <- SEP_BF_BiB[, c(2, 1, 3:ncol(SEP_BF_BiB))]
colnames(SEP_BF_BiB) <- colnames(obs.res_VegDiet_bin_con_BiB)
SEP_BF_BiB <- SEP_BF_BiB[!(SEP_BF_BiB$Outcome %in% c("Pesco-vegetarianism", "Full vegetarianism")), ]
obs.res_VegDiet_bin_con_ord_BiB <- rbind(obs.res_VegDiet_bin_con_BiB, SEP_BF_BiB)
################################################################################

obs.res_VegDiet_bin_con_ord_BiB[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_con_ord_BiB[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_con_ord_BiB$`Beta` <- as.numeric(obs.res_VegDiet_bin_con_ord_BiB$b)
obs.res_VegDiet_bin_con_ord_BiB$`Beta/SE` <- as.numeric(obs.res_VegDiet_bin_con_ord_BiB$b / obs.res_VegDiet_bin_con_ord_BiB$se)

obs.res_VegDiet_bin_con_ord_BiB$Study <- "BiB"

obs.res_VegDiet_bin_con_ord_BiB$Group <- "Pregnancy & perinatal outcome (binary)"
obs.res_VegDiet_bin_con_ord_BiB$Group[obs.res_VegDiet_bin_con_ord_BiB$Outcome %in% c(
  "Pesco-/full vs. non-vegetarian",
  "Pesco- vs. non-vegetarian",
  "Full vs. non-vegetarian"
)] <- "Vegetarianism (binary)"
obs.res_VegDiet_bin_con_ord_BiB$Group[obs.res_VegDiet_bin_con_ord_BiB$Outcome %in% c(
  "Gestational age (weeks)",
  "Birth weight z-score",
  "Apgar score at 1 minute",
  "Apgar score at 5 minutes"
)] <- "Pregnancy & perinatal outcome (continuous)"
obs.res_VegDiet_bin_con_ord_BiB$Group[obs.res_VegDiet_bin_con_ord_BiB$Outcome %in% c("Breastfeeding duration")] <- "Pregnancy & perinatal outcome (ordinal)"
obs.res_VegDiet_bin_con_ord_BiB$Group <-
  factor(
    obs.res_VegDiet_bin_con_ord_BiB$Group,
    levels = c(
      "Vegetarianism (binary)",
      "Pregnancy & perinatal outcome (binary)",
      "Pregnancy & perinatal outcome (continuous)",
      "Pregnancy & perinatal outcome (ordinal)"
    )
  )

obs.res_VegDiet_bin_con_ord_BiB$Outcome <-
  factor(obs.res_VegDiet_bin_con_ord_BiB$Outcome, levels = rev(
    c(
      "Pesco-/full vs. non-vegetarian",
      "Pesco- vs. non-vegetarian",
      "Full vs. non-vegetarian",
      unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin_con_ord_BiB$Outcome]
    )
  ))
obs.res_VegDiet_bin_con_ord_BiB <-
  obs.res_VegDiet_bin_con_ord_BiB %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

colnames(obs.res_VegDiet_bin_con_ord_BiB)[colnames(obs.res_VegDiet_bin_con_ord_BiB) == "SEP"] <- "SEP indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord_BiB$`SEP indicator` <- gsub(" \\(", "\n(", obs.res_VegDiet_bin_con_ord_BiB$`SEP indicator`)  # Split the SEP indicator in two lines
obs.res_VegDiet_bin_con_ord_BiB$`SEP indicator` <- factor(
  obs.res_VegDiet_bin_con_ord_BiB$`SEP indicator`,
  levels = c(
    "Maternal education\n(higher vs. lower)",
    "IMD\n(more vs. less affluent)"
  )
)

obs.res_VegDiet_bin_con_ord_BiB
str(obs.res_VegDiet_bin_con_ord_BiB)  # 27 outcomes * 2 models = 54 obs.

## MoBa
obs.res_VegDiet_bin_con_MoBa <-
  read.xlsx("MoBa/IMP_SEP_obs.res_VegDiet.subgroup_bin_con.xlsx")

################################################################################
### !!! Add in SEP effect on breastfeeding !!!
SEP_BF_MoBa <- read.xlsx("MoBa/IMP_BF_obs.res_VegDiet.subgroup_SEP.xlsx")
SEP_BF_MoBa$Exposure[SEP_BF_MoBa$Exposure ==
                       "Maternal education"] <- "Maternal education (higher vs. lower)"
SEP_BF_MoBa$Exposure[SEP_BF_MoBa$Exposure ==
                       "Household income"] <- "Household income (higher vs. lower)"
SEP_BF_MoBa <- SEP_BF_MoBa[, c(2, 1, 3:ncol(SEP_BF_MoBa))]
colnames(SEP_BF_MoBa) <- colnames(obs.res_VegDiet_bin_con_MoBa)
SEP_BF_MoBa <- SEP_BF_MoBa[!(SEP_BF_MoBa$Outcome %in% c("Pesco-vegetarianism", "Full vegetarianism")), ]
obs.res_VegDiet_bin_con_ord_MoBa <- rbind(obs.res_VegDiet_bin_con_MoBa, SEP_BF_MoBa)
################################################################################

obs.res_VegDiet_bin_con_ord_MoBa[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_con_ord_MoBa[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_con_ord_MoBa$`Beta` <- as.numeric(obs.res_VegDiet_bin_con_ord_MoBa$b)
obs.res_VegDiet_bin_con_ord_MoBa$`Beta/SE` <- as.numeric(obs.res_VegDiet_bin_con_ord_MoBa$b / obs.res_VegDiet_bin_con_ord_MoBa$se)

obs.res_VegDiet_bin_con_ord_MoBa$Study <- "MoBa"

obs.res_VegDiet_bin_con_ord_MoBa$Group <- "Pregnancy & perinatal outcome (binary)"
obs.res_VegDiet_bin_con_ord_MoBa$Group[obs.res_VegDiet_bin_con_ord_MoBa$Outcome %in% c(
  "Pesco-/full vs. non-vegetarian",
  "Pesco- vs. non-vegetarian",
  "Full vs. non-vegetarian"
)] <- "Vegetarianism (binary)"
obs.res_VegDiet_bin_con_ord_MoBa$Group[obs.res_VegDiet_bin_con_ord_MoBa$Outcome %in% c(
  "Gestational age (weeks)",
  "Birth weight z-score",
  "Apgar score at 1 minute",
  "Apgar score at 5 minutes"
)] <- "Pregnancy & perinatal outcome (continuous)"
obs.res_VegDiet_bin_con_ord_MoBa$Group[obs.res_VegDiet_bin_con_ord_MoBa$Outcome %in% c("Breastfeeding duration")] <- "Pregnancy & perinatal outcome (ordinal)"
obs.res_VegDiet_bin_con_ord_MoBa$Group <-
  factor(
    obs.res_VegDiet_bin_con_ord_MoBa$Group,
    levels = c(
      "Vegetarianism (binary)",
      "Pregnancy & perinatal outcome (binary)",
      "Pregnancy & perinatal outcome (continuous)",
      "Pregnancy & perinatal outcome (ordinal)"
    )
  )

obs.res_VegDiet_bin_con_ord_MoBa$Outcome <-
  factor(obs.res_VegDiet_bin_con_ord_MoBa$Outcome, levels = rev(
    c(
      "Pesco-/full vs. non-vegetarian",
      "Pesco- vs. non-vegetarian",
      "Full vs. non-vegetarian",
      unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin_con_ord_MoBa$Outcome]
    )
  ))
obs.res_VegDiet_bin_con_ord_MoBa <-
  obs.res_VegDiet_bin_con_ord_MoBa %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

colnames(obs.res_VegDiet_bin_con_ord_MoBa)[colnames(obs.res_VegDiet_bin_con_ord_MoBa) == "SEP"] <- "SEP indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord_MoBa$`SEP indicator` <- gsub(" \\(",
                                                         "\n(",
                                                         obs.res_VegDiet_bin_con_ord_MoBa$`SEP indicator`)  # Split the SEP indicator in two lines
obs.res_VegDiet_bin_con_ord_MoBa$`SEP indicator` <- factor(
  obs.res_VegDiet_bin_con_ord_MoBa$`SEP indicator`,
  levels = c(
    "Maternal education\n(higher vs. lower)",
    "Household income\n(higher vs. lower)"
  )
)

obs.res_VegDiet_bin_con_ord_MoBa
str(obs.res_VegDiet_bin_con_ord_MoBa)  # 29 outcomes * 2 models = 58 obs.

## Project Viva
obs.res_VegDiet_bin_con_Viva <-
  read.xlsx("Viva/IMP_SEP_obs.res_VegDiet.subgroup_bin_con.xlsx")

################################################################################
### !!! Add in SEP effect on breastfeeding !!!
SEP_BF_Viva <- read.xlsx("Viva/IMP_BF_obs.res_VegDiet.subgroup_SEP.xlsx")
SEP_BF_Viva$Exposure[SEP_BF_Viva$Exposure ==
                       "Maternal education"] <- "Maternal education (higher vs. lower)"
SEP_BF_Viva$Exposure[SEP_BF_Viva$Exposure ==
                       "Household income"] <- "Household income (higher vs. lower)"
SEP_BF_Viva <- SEP_BF_Viva[, c(2, 1, 3:ncol(SEP_BF_Viva))]
colnames(SEP_BF_Viva) <- colnames(obs.res_VegDiet_bin_con_Viva)
SEP_BF_Viva <- SEP_BF_Viva[!(SEP_BF_Viva$Outcome %in% c("Pesco-vegetarianism", "Full vegetarianism")), ]
obs.res_VegDiet_bin_con_ord_Viva <- rbind(obs.res_VegDiet_bin_con_Viva, SEP_BF_Viva)
################################################################################

obs.res_VegDiet_bin_con_ord_Viva[, c("b", "se", "pval")] <-
  sapply(obs.res_VegDiet_bin_con_ord_Viva[, c("b", "se", "pval")], as.numeric)
obs.res_VegDiet_bin_con_ord_Viva$`Beta` <- as.numeric(obs.res_VegDiet_bin_con_ord_Viva$b)
obs.res_VegDiet_bin_con_ord_Viva$`Beta/SE` <- as.numeric(obs.res_VegDiet_bin_con_ord_Viva$b / obs.res_VegDiet_bin_con_ord_Viva$se)

obs.res_VegDiet_bin_con_ord_Viva$Study <- "Project Viva"

obs.res_VegDiet_bin_con_ord_Viva$Group <- "Pregnancy & perinatal outcome (binary)"
obs.res_VegDiet_bin_con_ord_Viva$Group[obs.res_VegDiet_bin_con_ord_Viva$Outcome %in% c(
  "Pesco-/full vs. non-vegetarian",
  "Pesco- vs. non-vegetarian",
  "Full vs. non-vegetarian"
)] <- "Vegetarianism (binary)"
obs.res_VegDiet_bin_con_ord_Viva$Group[obs.res_VegDiet_bin_con_ord_Viva$Outcome %in% c(
  "Gestational age (weeks)",
  "Birth weight z-score",
  "Apgar score at 1 minute",
  "Apgar score at 5 minutes"
)] <- "Pregnancy & perinatal outcome (continuous)"
obs.res_VegDiet_bin_con_ord_Viva$Group[obs.res_VegDiet_bin_con_ord_Viva$Outcome %in% c("Breastfeeding duration")] <- "Pregnancy & perinatal outcome (ordinal)"
obs.res_VegDiet_bin_con_ord_Viva$Group <-
  factor(
    obs.res_VegDiet_bin_con_ord_Viva$Group,
    levels = c(
      "Vegetarianism (binary)",
      "Pregnancy & perinatal outcome (binary)",
      "Pregnancy & perinatal outcome (continuous)",
      "Pregnancy & perinatal outcome (ordinal)"
    )
  )

obs.res_VegDiet_bin_con_ord_Viva$Outcome <-
  factor(obs.res_VegDiet_bin_con_ord_Viva$Outcome, levels = rev(
    c(
      "Pesco-/full vs. non-vegetarian",
      "Pesco- vs. non-vegetarian",
      "Full vs. non-vegetarian",
      unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin_con_ord_Viva$Outcome]
    )
  ))
obs.res_VegDiet_bin_con_ord_Viva <-
  obs.res_VegDiet_bin_con_ord_Viva %>% arrange(Outcome)  # Make sure the outcomes appear in the right order

colnames(obs.res_VegDiet_bin_con_ord_Viva)[colnames(obs.res_VegDiet_bin_con_ord_Viva) == "SEP"] <- "SEP indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord_Viva$`SEP indicator` <- gsub(" \\(",
                                                         "\n(",
                                                         obs.res_VegDiet_bin_con_ord_Viva$`SEP indicator`)  # Split the SEP indicator in two lines
obs.res_VegDiet_bin_con_ord_Viva$`SEP indicator` <- factor(
  obs.res_VegDiet_bin_con_ord_Viva$`SEP indicator`,
  levels = c(
    "Maternal education\n(higher vs. lower)",
    "Household income\n(higher vs. lower)"
  )
)

obs.res_VegDiet_bin_con_ord_Viva
str(obs.res_VegDiet_bin_con_ord_Viva)  # 23 outcomes * 2 models = 46 obs.

################################################################################

# Create templates (including all available outcomes and for two sets of SEP indicators)

template_edu_IMD <- subset(obs.res_VegDiet_bin_con_ord_ALSPAC,
                           select = c(Outcome, `SEP indicator`, Group))
template_edu_IMD

template_edu_income <- subset(obs.res_VegDiet_bin_con_ord_MoBa,
                              select = c(Outcome, `SEP indicator`, Group))
template_edu_income

mean(template_edu_IMD$Outcome %in% template_edu_income$Outcome)  # 100%
mean(template_edu_income$Outcome %in% template_edu_IMD$Outcome)  # 100%

## Combine BiB and Project Viva results with the templates
obs.res_VegDiet_bin_con_ord_BiB <- left_join(
  template_edu_IMD,
  obs.res_VegDiet_bin_con_ord_BiB,
  by = c("Outcome", "SEP indicator", "Group")
)
obs.res_VegDiet_bin_con_ord_BiB$Study <- "BiB"
obs.res_VegDiet_bin_con_ord_BiB

obs.res_VegDiet_bin_con_ord_Viva <- left_join(
  template_edu_income,
  obs.res_VegDiet_bin_con_ord_Viva,
  by = c("Outcome", "SEP indicator", "Group")
)
obs.res_VegDiet_bin_con_ord_Viva$Study <- "Project Viva"
obs.res_VegDiet_bin_con_ord_Viva

# Combine cohort-specific results
obs.res_VegDiet_bin_con_ord <- rbind(
  obs.res_VegDiet_bin_con_ord_ALSPAC,
  obs.res_VegDiet_bin_con_ord_BiB,
  obs.res_VegDiet_bin_con_ord_MoBa,
  obs.res_VegDiet_bin_con_ord_Viva
)

obs.res_VegDiet_bin_con_ord
dim(obs.res_VegDiet_bin_con_ord)  # 232 obs. = 58 + 58 + 58 + 58

################################################################################
## Check the upper and lower bound of heatmap legend
min(obs.res_VegDiet_bin_con_ord$`Beta`, na.rm = T)  # -0.7588282
max(obs.res_VegDiet_bin_con_ord$`Beta`, na.rm = T)  # 1.511007
################################################################################
# # Save results
# write.xlsx(obs.res_VegDiet_bin_con_ord_ALSPAC,
#            "ALSPAC/IMP_SEP_obs.res_VegDiet_bin_con_ord_temp.xlsx")
# write.xlsx(obs.res_VegDiet_bin_con_ord_BiB,
#            "BiB/IMP_SEP_obs.res_VegDiet_bin_con_ord_temp.xlsx")
# write.xlsx(obs.res_VegDiet_bin_con_ord_MoBa,
#            "MoBa/IMP_SEP_obs.res_VegDiet_bin_con_ord_temp.xlsx")
# write.xlsx(obs.res_VegDiet_bin_con_ord_Viva,
#            "Viva/IMP_SEP_obs.res_VegDiet_bin_con_ord_temp.xlsx")
# write.xlsx(obs.res_VegDiet_bin_con_ord,
#            "ALL/IMP_SEP_obs.res_VegDiet_bin_con_ord_temp.xlsx")
################################################################################

#------------------------------------------------------------------------------#
#                               Heatmap matrices                               #----
#------------------------------------------------------------------------------#

# Cohort-specific heatmaps

## ALSPAC

### Create a new column for significance levels
obs.res_VegDiet_bin_con_ord_ALSPAC$Sig <- cut(
  obs.res_VegDiet_bin_con_ord_ALSPAC$pval,
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
)

### Create heatmap
obs.heatmap_VegDiet_bin_con_ord_ALSPAC <- ggplot(obs.res_VegDiet_bin_con_ord_ALSPAC,
                                                 aes(x = `SEP indicator`, y = Outcome)) +
  geom_tile(aes(fill = `Beta`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-0.8, 1.6),
    name = "logOR or beta"
  ) +
  geom_text(
    aes(label = Sig),
    color = "black",
    size = 4,
    fontface = "bold",
    vjust = 0.5
  ) +
  ggforce::facet_col(
    facets = ~ Group,
    scales = "free_y",
    space = "free",
    strip.position = "top"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    legend.position = "none"
  ) +
  labs(x = "", y = "")

obs.heatmap_VegDiet_bin_con_ord_ALSPAC

### Save heatmap
ggsave(
  "ALSPAC/IMP_SEP_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord_ALSPAC,
  width = 5,
  height = 9
)

################################################################################
################################################################################

## BiB

### Create a new column for significance levels, including NAs
obs.res_VegDiet_bin_con_ord_BiB$Sig <- NA
obs.res_VegDiet_bin_con_ord_BiB$Sig[!is.na(obs.res_VegDiet_bin_con_ord_BiB$pval)] <- as.character(cut(
  obs.res_VegDiet_bin_con_ord_BiB$pval[!is.na(obs.res_VegDiet_bin_con_ord_BiB$pval)],
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
))
obs.res_VegDiet_bin_con_ord_BiB$Sig[is.na(obs.res_VegDiet_bin_con_ord_BiB$pval)] <- "NA"

### Create heatmap
obs.heatmap_VegDiet_bin_con_ord_BiB <- ggplot(obs.res_VegDiet_bin_con_ord_BiB,
                                              aes(x = `SEP indicator`, y = Outcome)) +
  geom_tile(aes(fill = `Beta`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-0.8, 1.6),
    name = "logOR or beta",
    na.value = "grey"
  ) +
  geom_text(
    aes(label = Sig),
    color = "black",
    size = ifelse(obs.res_VegDiet_bin_con_ord_BiB$Sig == "NA", 3, 4),
    fontface = ifelse(obs.res_VegDiet_bin_con_ord_BiB$Sig == "NA", "plain", "bold"),
    vjust = 0.5
  ) +
  ggforce::facet_col(
    facets = ~ Group,
    scales = "free_y",
    space = "free",
    strip.position = "top"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    legend.position = "bottom"
  ) +
  labs(x = "", y = "")

### Display heatmap
obs.heatmap_VegDiet_bin_con_ord_BiB

### Save heatmap
ggsave(
  "BiB/IMP_SEP_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord_BiB,
  width = 5,
  height = 9
)

################################################################################
################################################################################

## MoBa

### Create a new column for significance levels
obs.res_VegDiet_bin_con_ord_MoBa$Sig <- cut(
  obs.res_VegDiet_bin_con_ord_MoBa$pval,
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
)

### Create heatmap
obs.heatmap_VegDiet_bin_con_ord_MoBa <- ggplot(obs.res_VegDiet_bin_con_ord_MoBa,
                                               aes(x = `SEP indicator`, y = Outcome)) +
  geom_tile(aes(fill = `Beta`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-0.8, 1.6),
    name = "logOR or beta"
  ) +
  geom_text(
    aes(label = Sig),
    color = "black",
    size = 4,
    fontface = "bold",
    vjust = 0.5
  ) +
  ggforce::facet_col(
    facets = ~ Group,
    scales = "free_y",
    space = "free",
    strip.position = "top"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    legend.position = "none"
  ) +
  labs(x = "", y = "")

obs.heatmap_VegDiet_bin_con_ord_MoBa

### Save heatmap
ggsave(
  "MoBa/IMP_SEP_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord_MoBa,
  width = 5,
  height = 9
)

################################################################################
################################################################################

## Project Viva

# Create a new column for significance levels, including NAs
obs.res_VegDiet_bin_con_ord_Viva$Sig <- NA
obs.res_VegDiet_bin_con_ord_Viva$Sig[!is.na(obs.res_VegDiet_bin_con_ord_Viva$pval)] <- as.character(cut(
  obs.res_VegDiet_bin_con_ord_Viva$pval[!is.na(obs.res_VegDiet_bin_con_ord_Viva$pval)],
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
))
obs.res_VegDiet_bin_con_ord_Viva$Sig[is.na(obs.res_VegDiet_bin_con_ord_Viva$pval)] <- "NA"

# Create heatmap
obs.heatmap_VegDiet_bin_con_ord_Viva <- ggplot(obs.res_VegDiet_bin_con_ord_Viva,
                                               aes(x = `SEP indicator`, y = Outcome)) +
  geom_tile(aes(fill = `Beta`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-0.8, 1.6),
    name = "logOR or beta",
    na.value = "grey"
  ) +
  geom_text(
    aes(label = Sig),
    color = "black",
    size = ifelse(obs.res_VegDiet_bin_con_ord_Viva$Sig == "NA", 3, 4),
    fontface = ifelse(obs.res_VegDiet_bin_con_ord_Viva$Sig == "NA", "plain", "bold"),
    vjust = 0.5
  ) +
  ggforce::facet_col(
    facets = ~ Group,
    scales = "free_y",
    space = "free",
    strip.position = "top"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    legend.position = "none"
  ) +
  labs(x = "", y = "")

# Display heatmap
obs.heatmap_VegDiet_bin_con_ord_Viva

# Save heatmap
ggsave(
  "Viva/IMP_SEP_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord_Viva,
  width = 5,
  height = 9
)

################################################################################
################################################################################

# Combine
obs.heatmap_VegDiet_bin_con_ord_ALSPAC <- obs.heatmap_VegDiet_bin_con_ord_ALSPAC +
  labs(title = "ALSPAC (N up to 11693)") +
  theme(plot.title = element_text(size = 10))

obs.heatmap_VegDiet_bin_con_ord_BiB <- obs.heatmap_VegDiet_bin_con_ord_BiB +
  labs(title = "BiB (N up to 3647)") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    strip.text = element_text(hjust = 99),
    # Move facet labels outside of the plot (for combination)
  )

obs.heatmap_VegDiet_bin_con_ord_MoBa <- obs.heatmap_VegDiet_bin_con_ord_MoBa +
  labs(title = "MoBa (N up to 73868)") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    strip.text = element_text(hjust = 99),
    # Move facet labels outside of the plot (for combination)
  )

obs.heatmap_VegDiet_bin_con_ord_Viva <- obs.heatmap_VegDiet_bin_con_ord_Viva +
  labs(title = "Project Viva (N up to 1872)") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    strip.text = element_text(hjust = 99),
    # Move facet labels outside of the plot (for combination)
  )

obs.heatmap_VegDiet_bin_con_ord <- obs.heatmap_VegDiet_bin_con_ord_ALSPAC +
  obs.heatmap_VegDiet_bin_con_ord_BiB +
  obs.heatmap_VegDiet_bin_con_ord_MoBa +
  obs.heatmap_VegDiet_bin_con_ord_Viva +
  plot_layout(ncol = 4, widths = c(1, 1, 1, 1)) &
  theme(plot.margin = margin(5, 0, 0, 0))

obs.heatmap_VegDiet_bin_con_ord

ggsave(
  "ALL/Comb_IMP_SEP_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord,
  width = 13,
  height = 9
)
