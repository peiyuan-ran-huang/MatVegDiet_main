################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 17-Jun-2025
# This script is to combine cohort-specific results for the effect of health behaviours/consciousness on both vegetarianism and perinatal outcomes (with imputed data).

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
obs.res_VegDiet_bin_con_ord_ALSPAC <-
  read.xlsx("ALSPAC/IMP_conscious_obs.res_VegDiet.subgroup_bin_con_ord.xlsx")

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

colnames(obs.res_VegDiet_bin_con_ord_ALSPAC)[colnames(obs.res_VegDiet_bin_con_ord_ALSPAC) == "HC"] <- "HC indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord_ALSPAC$`HC indicator` <- gsub(" \\(",
                                                          "\n(",
                                                          obs.res_VegDiet_bin_con_ord_ALSPAC$`HC indicator`)  # Split the HC indicator in two lines
obs.res_VegDiet_bin_con_ord_ALSPAC$`HC indicator` <- factor(
  obs.res_VegDiet_bin_con_ord_ALSPAC$`HC indicator`,
  levels = c("Maternal adherence to healthy behaviours\n(higher vs. lower)")
)

obs.res_VegDiet_bin_con_ord_ALSPAC
str(obs.res_VegDiet_bin_con_ord_ALSPAC)  # 29 outcomes * 1 model = 29 obs.

## BiB
obs.res_VegDiet_bin_con_ord_BiB <-
  read.xlsx("BiB/IMP_conscious_obs.res_VegDiet.subgroup_bin_con_ord.xlsx")

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

colnames(obs.res_VegDiet_bin_con_ord_BiB)[colnames(obs.res_VegDiet_bin_con_ord_BiB) == "HC"] <- "HC indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord_BiB$`HC indicator` <- gsub(" \\(", "\n(", obs.res_VegDiet_bin_con_ord_BiB$`HC indicator`)  # Split the HC indicator in two lines
obs.res_VegDiet_bin_con_ord_BiB$`HC indicator` <- factor(
  obs.res_VegDiet_bin_con_ord_BiB$`HC indicator`,
  levels = c("Maternal adherence to healthy behaviours\n(higher vs. lower)")
)

obs.res_VegDiet_bin_con_ord_BiB
str(obs.res_VegDiet_bin_con_ord_BiB)  # 27 outcomes * 1 model = 27 obs.

## MoBa
obs.res_VegDiet_bin_con_ord_MoBa <-
  read.xlsx("MoBa/IMP_conscious_obs.res_VegDiet.subgroup_bin_con_ord.xlsx")

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

colnames(obs.res_VegDiet_bin_con_ord_MoBa)[colnames(obs.res_VegDiet_bin_con_ord_MoBa) == "HC"] <- "HC indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord_MoBa$`HC indicator` <- gsub(" \\(", "\n(", obs.res_VegDiet_bin_con_ord_MoBa$`HC indicator`)  # Split the HC indicator in two lines
obs.res_VegDiet_bin_con_ord_MoBa$`HC indicator` <- factor(
  obs.res_VegDiet_bin_con_ord_MoBa$`HC indicator`,
  levels = c("Maternal adherence to healthy behaviours\n(higher vs. lower)")
)

obs.res_VegDiet_bin_con_ord_MoBa
str(obs.res_VegDiet_bin_con_ord_MoBa)  # 29 outcomes * 1 model = 29 obs.

## Project Viva
obs.res_VegDiet_bin_con_ord_Viva <-
  read.xlsx("Viva/IMP_conscious_obs.res_VegDiet.subgroup_bin_con_ord.xlsx")

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

colnames(obs.res_VegDiet_bin_con_ord_Viva)[colnames(obs.res_VegDiet_bin_con_ord_Viva) == "HC"] <- "HC indicator"  # Change the column name to be shown in the forest plot
obs.res_VegDiet_bin_con_ord_Viva$`HC indicator` <- gsub(" \\(", "\n(", obs.res_VegDiet_bin_con_ord_Viva$`HC indicator`)  # Split the HC indicator in two lines
obs.res_VegDiet_bin_con_ord_Viva$`HC indicator` <- factor(
  obs.res_VegDiet_bin_con_ord_Viva$`HC indicator`,
  levels = c("Maternal adherence to healthy behaviours\n(higher vs. lower)")
)

obs.res_VegDiet_bin_con_ord_Viva
str(obs.res_VegDiet_bin_con_ord_Viva)  # 23 outcomes * 1 model = 23 obs.

################################################################################

# Create templates (including all available outcomes and for 1 set of HC indicators)

template_conscious <- subset(obs.res_VegDiet_bin_con_ord_ALSPAC,
                             select = c(Outcome, `HC indicator`, Group))
template_conscious

## Combine BiB, MoBa, and Project Viva results with the templates
obs.res_VegDiet_bin_con_ord_BiB <- left_join(
  template_conscious,
  obs.res_VegDiet_bin_con_ord_BiB,
  by = c("Outcome", "HC indicator", "Group")
)
obs.res_VegDiet_bin_con_ord_BiB$Study <- "BiB"
obs.res_VegDiet_bin_con_ord_BiB

obs.res_VegDiet_bin_con_ord_MoBa <- left_join(
  template_conscious,
  obs.res_VegDiet_bin_con_ord_MoBa,
  by = c("Outcome", "HC indicator", "Group")
)
obs.res_VegDiet_bin_con_ord_MoBa$Study <- "MoBa"
obs.res_VegDiet_bin_con_ord_MoBa

obs.res_VegDiet_bin_con_ord_Viva <- left_join(
  template_conscious,
  obs.res_VegDiet_bin_con_ord_Viva,
  by = c("Outcome", "HC indicator", "Group")
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
dim(obs.res_VegDiet_bin_con_ord)  # 116 obs. = 29 + 29 + 29 + 29

################################################################################
## Check the upper and lower bound of heatmap legend
min(obs.res_VegDiet_bin_con_ord$`Beta`, na.rm = T)  # -0.4258474
max(obs.res_VegDiet_bin_con_ord$`Beta`, na.rm = T)  # 0.6654766
################################################################################
# # Save results
# write.xlsx(obs.res_VegDiet_bin_con_ord_ALSPAC,
#            "ALSPAC/IMP_conscious_obs.res_VegDiet_bin_con_ord_temp.xlsx")
# write.xlsx(obs.res_VegDiet_bin_con_ord_BiB,
#            "BiB/IMP_conscious_obs.res_VegDiet_bin_con_ord_temp.xlsx")
# write.xlsx(obs.res_VegDiet_bin_con_ord_MoBa,
#            "MoBa/IMP_conscious_obs.res_VegDiet_bin_con_ord_temp.xlsx")
# write.xlsx(obs.res_VegDiet_bin_con_ord_Viva,
#            "Viva/IMP_conscious_obs.res_VegDiet_bin_con_ord_temp.xlsx")
# write.xlsx(obs.res_VegDiet_bin_con_ord,
#            "ALL/IMP_conscious_obs.res_VegDiet_bin_con_ord_temp.xlsx")
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
                                                 aes(x = `HC indicator`, y = Outcome)) +
  geom_tile(aes(fill = `Beta`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-0.6, 0.8),
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
  "ALSPAC/IMP_conscious_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord_ALSPAC,
  width = 3,
  height = 9
)

################################################################################
################################################################################

## BiB

# Create a new column for significance levels, including NAs
obs.res_VegDiet_bin_con_ord_BiB$Sig <- NA
obs.res_VegDiet_bin_con_ord_BiB$Sig[!is.na(obs.res_VegDiet_bin_con_ord_BiB$pval)] <- as.character(cut(
  obs.res_VegDiet_bin_con_ord_BiB$pval[!is.na(obs.res_VegDiet_bin_con_ord_BiB$pval)],
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
))
obs.res_VegDiet_bin_con_ord_BiB$Sig[is.na(obs.res_VegDiet_bin_con_ord_BiB$pval)] <- "NA"

# Create heatmap
obs.heatmap_VegDiet_bin_con_ord_BiB <- ggplot(obs.res_VegDiet_bin_con_ord_BiB,
                                              aes(x = `HC indicator`, y = Outcome)) +
  geom_tile(aes(fill = `Beta`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-0.6, 0.8),
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

# Display heatmap
obs.heatmap_VegDiet_bin_con_ord_BiB

# Save heatmap
ggsave(
  "BiB/IMP_conscious_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord_BiB,
  width = 3,
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
                                               aes(x = `HC indicator`, y = Outcome)) +
  geom_tile(aes(fill = `Beta`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-0.6, 0.8),
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
  "MoBa/IMP_conscious_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord_MoBa,
  width = 3,
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
                                               aes(x = `HC indicator`, y = Outcome)) +
  geom_tile(aes(fill = `Beta`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-0.6, 0.8),
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
  "Viva/IMP_conscious_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord_Viva,
  width = 3,
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
  "ALL/Comb_IMP_conscious_obs.heatmap_VegDiet_bin_con_ord.png",
  obs.heatmap_VegDiet_bin_con_ord,
  width = 13,
  height = 9
)
