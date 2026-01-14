################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 21-Jan-2025
# This script is to compare "diet-based" vs. "diet-based + self-defined" vegetarianism results for vegetarian diets.

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

# Load and combine analysis results

## Diet-based vegetarianism

### ALSPAC
main_bin_ALSPAC <- read.xlsx("ALSPAC/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
main_bin_ALSPAC <- main_bin_ALSPAC %>% subset(Model ==
                                                "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_bin_ALSPAC$`Definition of vegetarianism` <- "Diet-based (main results)"
main_bin_ALSPAC$Study <- "ALSPAC"
main_bin_ALSPAC

main_con_ALSPAC <- read.xlsx("ALSPAC/IMP_MAIN_obs.res_VegDiet_con.xlsx")
main_con_ALSPAC <- main_con_ALSPAC %>% subset(Model ==
                                                "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_con_ALSPAC$`Definition of vegetarianism` <- "Diet-based (main results)"
main_con_ALSPAC$Study <- "ALSPAC"
main_con_ALSPAC

main_ord_ALSPAC <- read.xlsx("ALSPAC/IMP_BF_obs.res_VegDiet_ord.xlsx")
main_ord_ALSPAC <- main_ord_ALSPAC %>% subset(Model ==
                                                "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_ord_ALSPAC$`Definition of vegetarianism` <- "Diet-based (main results)"
main_ord_ALSPAC$Study <- "ALSPAC"
main_ord_ALSPAC

### BiB
main_bin_BiB <- read.xlsx("BiB/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
main_bin_BiB <- main_bin_BiB %>% subset(Model ==
                                          "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_bin_BiB$`Definition of vegetarianism` <- "Diet-based (main results)"
main_bin_BiB$Study <- "BiB"
main_bin_BiB

main_con_BiB <- read.xlsx("BiB/IMP_MAIN_obs.res_VegDiet_con.xlsx")
main_con_BiB <- main_con_BiB %>% subset(Model ==
                                          "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_con_BiB$`Definition of vegetarianism` <- "Diet-based (main results)"
main_con_BiB$Study <- "BiB"
main_con_BiB

main_ord_BiB <- read.xlsx("BiB/IMP_BF_obs.res_VegDiet_ord.xlsx")
main_ord_BiB <- main_ord_BiB %>% subset(Model ==
                                          "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_ord_BiB$`Definition of vegetarianism` <- "Diet-based (main results)"
main_ord_BiB$Study <- "BiB"
main_ord_BiB

### MoBa
main_bin_MoBa <- read.xlsx("MoBa/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
main_bin_MoBa <- main_bin_MoBa %>% subset(Model ==
                                            "Model 3" &
                                            Outcome != "Maternal anaemia (occurring during pregnancy)") %>% select(Outcome, Exposure, b, se, pval)
main_bin_MoBa$`Definition of vegetarianism` <- "Diet-based (main results)"
main_bin_MoBa$Study <- "MoBa"
main_bin_MoBa

main_con_MoBa <- read.xlsx("MoBa/IMP_MAIN_obs.res_VegDiet_con.xlsx")
main_con_MoBa <- main_con_MoBa %>% subset(Model ==
                                            "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_con_MoBa$`Definition of vegetarianism` <- "Diet-based (main results)"
main_con_MoBa$Study <- "MoBa"
main_con_MoBa

main_ord_MoBa <- read.xlsx("MoBa/IMP_BF_obs.res_VegDiet_ord.xlsx")
main_ord_MoBa <- main_ord_MoBa %>% subset(Model ==
                                            "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_ord_MoBa$`Definition of vegetarianism` <- "Diet-based (main results)"
main_ord_MoBa$Study <- "MoBa"
main_ord_MoBa

### !!! MoBa (subgroups) !!!
main_bin_MoBa_subgroup <- read.xlsx("MoBa/IMP_MAIN_obs.res_VegDiet.subgroup_bin.xlsx")
main_bin_MoBa_subgroup <- main_bin_MoBa_subgroup %>% subset(Model ==
                                                              "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_bin_MoBa_subgroup$`Definition of vegetarianism` <- "Diet-based (main results)"
main_bin_MoBa_subgroup$Study <- "MoBa"
main_bin_MoBa_subgroup

main_con_MoBa_subgroup <- read.xlsx("MoBa/IMP_MAIN_obs.res_VegDiet.subgroup_con.xlsx")
main_con_MoBa_subgroup <- main_con_MoBa_subgroup %>% subset(Model ==
                                                              "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_con_MoBa_subgroup$`Definition of vegetarianism` <- "Diet-based (main results)"
main_con_MoBa_subgroup$Study <- "MoBa"
main_con_MoBa_subgroup

main_ord_MoBa_subgroup <- read.xlsx("MoBa/IMP_BF_obs.res_VegDiet.subgroup_ord.xlsx")
main_ord_MoBa_subgroup <- main_ord_MoBa_subgroup %>% subset(Model ==
                                                              "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_ord_MoBa_subgroup$`Definition of vegetarianism` <- "Diet-based (main results)"
main_ord_MoBa_subgroup$Study <- "MoBa"
main_ord_MoBa_subgroup

### Project Viva
main_bin_Viva <- read.xlsx("Viva/IMP_MAIN_obs.res_VegDiet_bin.xlsx")
main_bin_Viva <- main_bin_Viva  %>% subset(Model ==
                                             "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_bin_Viva$`Definition of vegetarianism` <- "Diet-based (main results)"
main_bin_Viva$Study <- "Project Viva"
main_bin_Viva

main_con_Viva <- read.xlsx("Viva/IMP_MAIN_obs.res_VegDiet_con.xlsx")
main_con_Viva <- main_con_Viva %>% subset(Model ==
                                            "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_con_Viva$`Definition of vegetarianism` <- "Diet-based (main results)"
main_con_Viva$Study <- "Project Viva"
main_con_Viva

main_ord_Viva <- read.xlsx("Viva/IMP_BF_obs.res_VegDiet_ord.xlsx")
main_ord_Viva <- main_ord_Viva %>% subset(Model ==
                                            "Model 3") %>% select(Outcome, Exposure, b, se, pval)
main_ord_Viva$`Definition of vegetarianism` <- "Diet-based (main results)"
main_ord_Viva$Study <- "Project Viva"
main_ord_Viva

## Diet-based + self-defined vegetarianism

### ALSPAC
FFQ.self_bin_ALSPAC <- read.xlsx("ALSPAC/IMP_SENS.FFQ.self_obs.res_VegDiet_bin.xlsx")
FFQ.self_bin_ALSPAC <- FFQ.self_bin_ALSPAC %>% subset(Model ==
                                                        "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_bin_ALSPAC$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_bin_ALSPAC$Study <- "ALSPAC"
FFQ.self_bin_ALSPAC

FFQ.self_con_ALSPAC <- read.xlsx("ALSPAC/IMP_SENS.FFQ.self_obs.res_VegDiet_con.xlsx")
FFQ.self_con_ALSPAC <- FFQ.self_con_ALSPAC %>% subset(Model ==
                                                        "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_con_ALSPAC$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_con_ALSPAC$Study <- "ALSPAC"
FFQ.self_con_ALSPAC

FFQ.self_ord_ALSPAC <- read.xlsx("ALSPAC/IMP_SENS.FFQ.self_obs.res_VegDiet_ord.xlsx")
FFQ.self_ord_ALSPAC <- FFQ.self_ord_ALSPAC %>% subset(Model ==
                                                        "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_ord_ALSPAC$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_ord_ALSPAC$Study <- "ALSPAC"
FFQ.self_ord_ALSPAC

### BiB - !!! Self-defined vegetarianism data NOT available !!!

### MoBa
FFQ.self_bin_MoBa <- read.xlsx("MoBa/IMP_SENS.FFQ.self_obs.res_VegDiet_bin.xlsx")
FFQ.self_bin_MoBa <- FFQ.self_bin_MoBa %>% subset(Model ==
                                                    "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_bin_MoBa$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_bin_MoBa$Study <- "MoBa"
FFQ.self_bin_MoBa

FFQ.self_con_MoBa <- read.xlsx("MoBa/IMP_SENS.FFQ.self_obs.res_VegDiet_con.xlsx")
FFQ.self_con_MoBa <- FFQ.self_con_MoBa %>% subset(Model ==
                                                    "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_con_MoBa$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_con_MoBa$Study <- "MoBa"
FFQ.self_con_MoBa

FFQ.self_ord_MoBa <- read.xlsx("MoBa/IMP_SENS.FFQ.self_obs.res_VegDiet_ord.xlsx")
FFQ.self_ord_MoBa <- FFQ.self_ord_MoBa %>% subset(Model ==
                                                    "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_ord_MoBa$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_ord_MoBa$Study <- "MoBa"
FFQ.self_ord_MoBa

### !!! MoBa (subgroups) !!!
FFQ.self_bin_MoBa_subgroup <- read.xlsx("MoBa/IMP_SENS.FFQ.self_obs.res_VegDiet.subgroup_bin.xlsx")
FFQ.self_bin_MoBa_subgroup <- FFQ.self_bin_MoBa_subgroup %>% subset(Model ==
                                                                      "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_bin_MoBa_subgroup$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_bin_MoBa_subgroup$Study <- "MoBa"
FFQ.self_bin_MoBa_subgroup

FFQ.self_con_MoBa_subgroup <- read.xlsx("MoBa/IMP_SENS.FFQ.self_obs.res_VegDiet.subgroup_con.xlsx")
FFQ.self_con_MoBa_subgroup <- FFQ.self_con_MoBa_subgroup %>% subset(Model ==
                                                                      "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_con_MoBa_subgroup$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_con_MoBa_subgroup$Study <- "MoBa"
FFQ.self_con_MoBa_subgroup

FFQ.self_ord_MoBa_subgroup <- read.xlsx("MoBa/IMP_SENS.FFQ.self_obs.res_VegDiet.subgroup_ord.xlsx")
FFQ.self_ord_MoBa_subgroup <- FFQ.self_ord_MoBa_subgroup %>% subset(Model ==
                                                                      "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_ord_MoBa_subgroup$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_ord_MoBa_subgroup$Study <- "MoBa"
FFQ.self_ord_MoBa_subgroup

### Project Viva
FFQ.self_bin_Viva <- read.xlsx("Viva/IMP_SENS.FFQ.self_obs.res_VegDiet_bin.xlsx")
FFQ.self_bin_Viva <- FFQ.self_bin_Viva %>% subset(Model ==
                                                    "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_bin_Viva$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_bin_Viva$Study <- "Project Viva"
FFQ.self_bin_Viva

FFQ.self_con_Viva <- read.xlsx("Viva/IMP_SENS.FFQ.self_obs.res_VegDiet_con.xlsx")
FFQ.self_con_Viva <- FFQ.self_con_Viva %>% subset(Model ==
                                                    "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_con_Viva$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_con_Viva$Study <- "Project Viva"
FFQ.self_con_Viva

FFQ.self_ord_Viva <- read.xlsx("Viva/IMP_SENS.FFQ.self_obs.res_VegDiet_ord.xlsx")
FFQ.self_ord_Viva <- FFQ.self_ord_Viva %>% subset(Model ==
                                                    "Model 3") %>% select(Outcome, Exposure, b, se, pval)
FFQ.self_ord_Viva$`Definition of vegetarianism` <- "Diet-based + self-defined"
FFQ.self_ord_Viva$Study <- "Project Viva"
FFQ.self_ord_Viva

################################################################################

## Combine
obs.res_VegDiet_bin_con <- rbind(
  main_bin_ALSPAC,
  main_bin_MoBa,
  main_bin_Viva,
  FFQ.self_bin_ALSPAC,
  FFQ.self_bin_MoBa,
  FFQ.self_bin_Viva,
  main_con_ALSPAC,
  main_con_MoBa,
  main_con_Viva,
  FFQ.self_con_ALSPAC,
  FFQ.self_con_MoBa,
  FFQ.self_con_Viva,
  main_ord_ALSPAC,
  main_ord_MoBa,
  main_ord_Viva,
  FFQ.self_ord_ALSPAC,
  FFQ.self_ord_MoBa,
  FFQ.self_ord_Viva
)
obs.res_VegDiet_bin_con
str(obs.res_VegDiet_bin_con)  # 144 obs.

obs.res_VegDiet_bin_con_MoBa_subgroup <- rbind(
  main_bin_MoBa_subgroup,
  FFQ.self_bin_MoBa_subgroup,
  main_con_MoBa_subgroup,
  FFQ.self_con_MoBa_subgroup,
  main_ord_MoBa_subgroup,
  FFQ.self_ord_MoBa_subgroup
)
obs.res_VegDiet_bin_con_MoBa_subgroup
str(obs.res_VegDiet_bin_con_MoBa_subgroup)  # 104 obs.

################################################################################

## Outcome grouping

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

################################################################################

# Prepare results for comparison

################################################################################
## Change character into numeric
obs.res_VegDiet_bin_con <- type.convert(obs.res_VegDiet_bin_con, as.is = T)
obs.res_VegDiet_bin_con_MoBa_subgroup <- type.convert(obs.res_VegDiet_bin_con_MoBa_subgroup, as.is = T)
################################################################################
## Remove some results with extremely huge 95% CIs
obs.res_VegDiet_bin_con <-
  obs.res_VegDiet_bin_con[which(obs.res_VegDiet_bin_con$se < 100), ]
head(obs.res_VegDiet_bin_con)
dim(obs.res_VegDiet_bin_con)  # 144 -> 136 obs.

obs.res_VegDiet_bin_con_MoBa_subgroup <-
  obs.res_VegDiet_bin_con_MoBa_subgroup[which(obs.res_VegDiet_bin_con_MoBa_subgroup$se < 100), ]
head(obs.res_VegDiet_bin_con_MoBa_subgroup)
dim(obs.res_VegDiet_bin_con_MoBa_subgroup)  # 104 -> 104 obs.
################################################################################

obs.res_VegDiet_bin_con$Outcome <-
  factor(obs.res_VegDiet_bin_con$Outcome,
         levels =
           unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin_con$Outcome])
obs.res_VegDiet_bin_con <-
  obs.res_VegDiet_bin_con %>% arrange(Outcome)  # Make sure the outcomes appear in the right order
obs.res_VegDiet_bin_con$`Definition of vegetarianism` <- factor(
  obs.res_VegDiet_bin_con$`Definition of vegetarianism`,
  levels = c("Diet-based + self-defined", "Diet-based (main results)")
)

obs.res_VegDiet_bin_con_MoBa_subgroup$Outcome <-
  factor(
    obs.res_VegDiet_bin_con_MoBa_subgroup$Outcome,
    levels =
      unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% obs.res_VegDiet_bin_con_MoBa_subgroup$Outcome]
  )
obs.res_VegDiet_bin_con_MoBa_subgroup <-
  obs.res_VegDiet_bin_con_MoBa_subgroup %>% arrange(Outcome)  # Make sure the outcomes appear in the right order
obs.res_VegDiet_bin_con_MoBa_subgroup$`Definition of vegetarianism` <- factor(
  obs.res_VegDiet_bin_con_MoBa_subgroup$`Definition of vegetarianism`,
  levels = c("Diet-based + self-defined", "Diet-based (main results)")
)

#------------------------------------------------------------------------------#
#                                 Forest Plots                                 #----
#------------------------------------------------------------------------------#

# Binary vegetarianism

## ALSPAC
obs.forest_VegDiet_bin_con_ALSPAC <- ggforestplot::forestplot(
  df = subset(obs.res_VegDiet_bin_con, Study == "ALSPAC"),
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `Definition of vegetarianism`,
  shape = `Definition of vegetarianism`,
  xlab = "LogOR or beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "ALSPAC",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkorange", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

obs.forest_VegDiet_bin_con_ALSPAC

## BiB - !!! NOT available !!!

## MoBa
obs.forest_VegDiet_bin_con_MoBa <- ggforestplot::forestplot(
  df = subset(obs.res_VegDiet_bin_con, Study == "MoBa"),
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `Definition of vegetarianism`,
  shape = `Definition of vegetarianism`,
  xlab = "LogOR or beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "MoBa",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkorange", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

obs.forest_VegDiet_bin_con_MoBa

## Project Viva
obs.forest_VegDiet_bin_con_Viva <- ggforestplot::forestplot(
  df = subset(obs.res_VegDiet_bin_con, Study == "Project Viva"),
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `Definition of vegetarianism`,
  shape = `Definition of vegetarianism`,
  xlab = "LogOR or beta and 95% CI\n(pesco-/full vegetarian vs. non-vegetarian)",
  title = "Project Viva",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkorange", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21))

obs.forest_VegDiet_bin_con_Viva

################################################################################

# Vegetarian subgroups - !!! In MoBa only !!!

## Pesco-vegetarian
obs.forest_VegDiet_bin_con_MoBa_pesco.V <- ggforestplot::forestplot(
  df = subset(
    obs.res_VegDiet_bin_con_MoBa_subgroup,
    Exposure == "Pesco-vegetarian"
  ),
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `Definition of vegetarianism`,
  shape = `Definition of vegetarianism`,
  xlab = "LogOR or beta and 95% CI (ref: non-vegetarian)",
  title = "Pesco-vegetarians in MoBa",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("mediumblue", "mediumblue")) +
  ggplot2::scale_shape_manual(values = c(25, 24))

obs.forest_VegDiet_bin_con_MoBa_pesco.V

## Full vegetarian
obs.forest_VegDiet_bin_con_MoBa_full.V <- ggforestplot::forestplot(
  df = subset(
    obs.res_VegDiet_bin_con_MoBa_subgroup,
    Exposure == "Full vegetarian"
  ),
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `Definition of vegetarianism`,
  shape = `Definition of vegetarianism`,
  xlab = "LogOR or beta and 95% CI (ref: non-vegetarian)",
  title = "Full vegetarians in MoBa",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("green4", "green4")) +
  ggplot2::scale_shape_manual(values = c(25, 24))

obs.forest_VegDiet_bin_con_MoBa_full.V

#------------------------------------------------------------------------------#
#                                Combine Plots                                 #----
#------------------------------------------------------------------------------#

# Binary vegetarianism
obs.forest_VegDiet_bin_con_ALSPAC <- obs.forest_VegDiet_bin_con_ALSPAC + ggplot2::theme(legend.position = "none")
print(obs.forest_VegDiet_bin_con_ALSPAC)
grob1 <- grid.grab()
dev.off()

obs.forest_VegDiet_bin_con_MoBa <- obs.forest_VegDiet_bin_con_MoBa + ggplot2::theme(legend.position = "none")
print(obs.forest_VegDiet_bin_con_MoBa)
grob2 <- grid.grab()
dev.off()

print(obs.forest_VegDiet_bin_con_Viva)
grob3 <- grid.grab()
dev.off()

png(
  "ALL/Comb_compare.FFQ.self_obs.forest_VegDiet.png",
  res = 300,
  height = 2800,
  width = 5000
)
grid.arrange(grob1,
             grob2,
             grob3,
             ncol = 3,
             widths = c(0.8, 0.8, 1.17))
dev.off()

# Vegetarian subgroups - !!! In MoBa only !!!
obs.forest_VegDiet_bin_con_MoBa_pesco.V <- obs.forest_VegDiet_bin_con_MoBa_pesco.V + ggplot2::theme(legend.position = "none")
print(obs.forest_VegDiet_bin_con_MoBa_pesco.V)
grob1 <- grid.grab()
dev.off()

print(obs.forest_VegDiet_bin_con_MoBa_full.V)
grob2 <- grid.grab()
dev.off()

png(
  "ALL/Comb_compare.FFQ.self_obs.forest_VegDiet.subgroup.png",
  res = 300,
  height = 2800,
  width = 4000
)
grid.arrange(grob1, grob2, ncol = 2, widths = c(0.7, 1))
dev.off()
