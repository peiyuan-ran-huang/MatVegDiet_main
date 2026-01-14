################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 08-Aug-2025
# This script is to perform meta-analysis on main association analysis (with imputed data) results for plant-based diet indices (PDIs).
## (4) Combine main analysis results

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

# Outcome grouping
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

# Load and combine meta-analysis results
metagen_obs.tbl_PDIs_bin <-
  rbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_bin.xlsx"),
    read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_bin.xlsx"),
    read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_bin.xlsx")
  )

metagen_obs.tbl_PDIs_ord <-
  rbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_ord.xlsx"),
    read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_ord.xlsx"),
    read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_ord.xlsx")
  )
colnames(metagen_obs.tbl_PDIs_ord)[colnames(metagen_obs.tbl_PDIs_ord) ==
                                     "N"] <- "Cases/total"

metagen_obs.tbl_PDIs_main <- rbind(metagen_obs.tbl_PDIs_bin, metagen_obs.tbl_PDIs_ord) %>% subset(.,
                                                                                                  Outcome %in% c(primary_bin$label, "Post-term birth", "Breastfeeding duration")) %>%
  group_by(Outcome)

metagen_obs.tbl_PDIs_main$Outcome <- factor(metagen_obs.tbl_PDIs_main$Outcome, levels = c(unique(MRPREG_outcome_labels$label)[unique(MRPREG_outcome_labels$label) %in% unique(metagen_obs.tbl_PDIs_main$Outcome)]))
metagen_obs.tbl_PDIs_main$Exposure[metagen_obs.tbl_PDIs_main$Exposure ==
                                     "Overall plant-based diet index (PDI)"] <- "PDI"
metagen_obs.tbl_PDIs_main$Exposure[metagen_obs.tbl_PDIs_main$Exposure ==
                                     "Healthful plant-based diet index (hPDI)"] <- "hPDI"
metagen_obs.tbl_PDIs_main$Exposure[metagen_obs.tbl_PDIs_main$Exposure ==
                                     "Unhealthful plant-based diet index (uPDI)"] <- "uPDI"
metagen_obs.tbl_PDIs_main <-
  metagen_obs.tbl_PDIs_main %>% arrange(Outcome, Exposure)

metagen_obs.tbl_PDIs_main$`Cases/total`[metagen_obs.tbl_PDIs_main$Outcome == "Breastfeeding duration"] <-
  "16345/4698/7927/53708"  # !!! Manually replace sample size for breastfeeding duration !!!

metagen_obs.tbl_PDIs_main

#------------------------------------------------------------------------------#
#                                Combine Plots                                 #----
#------------------------------------------------------------------------------#

# Make forest plot with texts

## Add beta and se values for the forest plot
metagen_obs.tbl_PDIs_main[, c("b", "se", "pval")] <-
  sapply(metagen_obs.tbl_PDIs_main[, c("b", "se", "pval")], as.numeric)

## Prepare data
outcomes <- metagen_obs.tbl_PDIs_main$Outcome
outcomes[(seq_along(outcomes) - 1) %% 3 != 0] <- " "
outcomes

ns <- metagen_obs.tbl_PDIs_main$`Cases/total`
ns[(seq_along(ns) - 1) %% 3 != 0] <- " "
ns

studies <- metagen_obs.tbl_PDIs_main$`N.of.studies`
studies[(seq_along(studies) - 1) %% 3 != 0] <- " "
studies

mydata <- data.frame(
  Outcome = outcomes,
  N_studies = studies,
  N = ns,
  Exposure = metagen_obs.tbl_PDIs_main$Exposure,
  OR = metagen_obs.tbl_PDIs_main$OR,
  CI = metagen_obs.tbl_PDIs_main$`95%.CI`,
  pval = metagen_obs.tbl_PDIs_main$`p-value`,
  a.pval = metagen_obs.tbl_PDIs_main$`Adjusted.p-value`,
  I2 = metagen_obs.tbl_PDIs_main$`I²`,
  b = metagen_obs.tbl_PDIs_main$b,
  se = metagen_obs.tbl_PDIs_main$se
)
mydata

tabletext <- rbind(
  c(
    "Outcome",
    "N studies",
    "N cases/total",
    "Exposure",
    "OR",
    "95% CI",
    "p-value",
    "Adjusted p-value",
    "I²"
  ),
  cbind(
    as.character(mydata[, "Outcome"]),
    as.character(mydata[, "N_studies"]),
    as.character(mydata[, "N"]),
    as.character(mydata[, "Exposure"]),
    as.character(mydata[, "OR"]),
    as.character(mydata[, "CI"]),
    as.character(mydata[, "pval"]),
    as.character(mydata[, "a.pval"]),
    as.character(mydata[, "I2"])
  )
)
tabletext

hrzl_lines <- list()
hrzl_lines[["2"]] <- gpar(lty = 1,
                          lwd = 3,
                          col = "black")
for (i in seq(3, 46)) {
  hrzl_lines[[as.character(i)]] <- gpar(lty = 1,
                                        lwd = 0.5,
                                        col = "grey")
}

for (i in seq(5, 44, 3)) {
  hrzl_lines[[as.character(i)]] <- gpar(lty = 1,
                                        lwd = 1,
                                        col = "black")
}

## Create forest plot with texts
png(
  "ALL/Comb_IMP_MAIN_meta.tbl.forest_PDIs_Prim.Out.png",
  height = 1800,
  width = 1700
)

forestplot(
  tabletext,
  graph.pos = 5,
  mean = rbind(NA, cbind(exp(mydata$b))),
  lower = rbind(NA, cbind(exp(
    mydata$b - 1.96 * mydata$se
  ))),
  upper = rbind(NA, cbind(exp(
    mydata$b + 1.96 * mydata$se
  ))),
  new_page = F,
  xlog = T,
  xticks = c(log(0.7), log(0.8), log(0.9), log(1.0), log(1.1), log(1.2)),
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
  is.summary = c(T, rep(F, nrow(mydata))),
  colgap = unit (10, "mm"),
  zero = 1.00,
  lwd.zero = 2,
  xlab = "OR and 95% CI (per 1 SD increase)"
)

dev.off()
