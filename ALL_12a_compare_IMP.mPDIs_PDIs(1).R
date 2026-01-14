################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 07-Aug-2025
# This script is to compare sensitivity analysis results for plant-based diet indices (PDIs).
## (1) For the overall plant-based diet index (PDI)

################################################################################

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

################################################################################

# Load meta-analysis results

## Binary outcomes
meta.res_PDI_bin <- rbind(
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_bin.xlsx"),
    data.frame(`Results from` = c(
      rep("Main analysis (with imputed data)", nrow(
        read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_bin.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/MAIN_meta.tbl_PDI_bin.xlsx"),
    data.frame(`Results from` = c(
      rep("Sensitivity analysis (with observed data)", nrow(
        read.xlsx("ALL/MAIN_meta.tbl_PDI_bin.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_PDIm_bin.xlsx"),
    data.frame(`Results from` = c(
      rep(
        "Sensitivity analysis\n(with modified PDI definition and imputed data)",
        nrow(read.xlsx("ALL/IMP_MAIN_meta.tbl_PDIm_bin.xlsx"))
      )
    ))
  )
)

meta.res_PDI_bin[, c("b", "se", "pval")] <-
  sapply(meta.res_PDI_bin[, c("b", "se", "pval")], as.numeric)

colnames(meta.res_PDI_bin)[which(colnames(meta.res_PDI_bin) == "Results.from")] <- "Results from"
meta.res_PDI_bin$`Results from` <- factor(
  meta.res_PDI_bin$`Results from`,
  levels = c(
    "Sensitivity analysis\n(with modified PDI definition and imputed data)",
    "Sensitivity analysis (with observed data)",
    "Main analysis (with imputed data)"
  ),
  ordered = T
)

meta.res_PDI_bin$Group <- "Binary outcome"

meta.res_PDI_bin
str(meta.res_PDI_bin)

################################################################################
################################################################################

## Continuous outcomes
meta.res_PDI_con <- rbind(
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_con.xlsx"),
    data.frame(`Results from` = c(
      rep("Main analysis (with imputed data)", nrow(
        read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_con.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/MAIN_meta.tbl_PDI_con.xlsx"),
    data.frame(`Results from` = c(
      rep("Sensitivity analysis (with observed data)", nrow(
        read.xlsx("ALL/MAIN_meta.tbl_PDI_con.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_PDIm_con.xlsx"),
    data.frame(`Results from` = c(
      rep(
        "Sensitivity analysis\n(with modified PDI definition and imputed data)",
        nrow(read.xlsx("ALL/IMP_MAIN_meta.tbl_PDIm_con.xlsx"))
      )
    ))
  )
)

meta.res_PDI_con[, c("b", "se", "pval")] <-
  sapply(meta.res_PDI_con[, c("b", "se", "pval")], as.numeric)

colnames(meta.res_PDI_con)[which(colnames(meta.res_PDI_con) == "Results.from")] <- "Results from"
meta.res_PDI_con$`Results from` <- factor(
  meta.res_PDI_con$`Results from`,
  levels = c(
    "Sensitivity analysis\n(with modified PDI definition and imputed data)",
    "Sensitivity analysis (with observed data)",
    "Main analysis (with imputed data)"
  ),
  ordered = T
)

meta.res_PDI_con$Group <- "Continuous outcome"

meta.res_PDI_con
str(meta.res_PDI_con)

################################################################################
################################################################################

## Ordinal outcome (i.e., breastfeeding duration)
meta.res_PDI_ord <- rbind(
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_ord.xlsx"),
    data.frame(`Results from` = c(
      rep("Main analysis (with imputed data)", nrow(
        read.xlsx("ALL/IMP_MAIN_meta.tbl_PDI_ord.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/MAIN_meta.tbl_PDI_ord.xlsx"),
    data.frame(`Results from` = c(
      rep("Sensitivity analysis (with observed data)", nrow(
        read.xlsx("ALL/MAIN_meta.tbl_PDI_ord.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_PDIm_ord.xlsx"),
    data.frame(`Results from` = c(
      rep(
        "Sensitivity analysis\n(with modified PDI definition and imputed data)",
        nrow(read.xlsx("ALL/IMP_MAIN_meta.tbl_PDIm_ord.xlsx"))
      )
    ))
  )
)

meta.res_PDI_ord[, c("b", "se", "pval")] <-
  sapply(meta.res_PDI_ord[, c("b", "se", "pval")], as.numeric)

colnames(meta.res_PDI_ord)[which(colnames(meta.res_PDI_ord) == "Results.from")] <- "Results from"
meta.res_PDI_ord$`Results from` <- factor(
  meta.res_PDI_ord$`Results from`,
  levels = c(
    "Sensitivity analysis\n(with modified PDI definition and imputed data)",
    "Sensitivity analysis (with observed data)",
    "Main analysis (with imputed data)"
  ),
  ordered = T
)

meta.res_PDI_ord$Group <- "Ordinal outcome"

meta.res_PDI_ord
str(meta.res_PDI_ord)

################################################################################

# Nightingale forestplots
meta.res_PDI <- rbind(
  subset(
    meta.res_PDI_bin,
    select = c("Outcome", "Exposure", "b", "se", "pval", "Results from", "Group")
  ),
  subset(
    meta.res_PDI_con,
    select = c("Outcome", "Exposure", "b", "se", "pval", "Results from", "Group")
  ),
  subset(
    meta.res_PDI_ord,
    select = c("Outcome", "Exposure", "b", "se", "pval", "Results from", "Group")
  )
)

meta.res_PDI$Group <- factor(
  meta.res_PDI$Group,
  levels = c("Binary outcome", "Continuous outcome", "Ordinal outcome"),
  ordered = T
)

meta.forest_PDI <- ggforestplot::forestplot(
  df = meta.res_PDI,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `Results from`,
  shape = `Results from`,
  xlab = "logOR or beta and 95% CI",
  title = "Overall plant-based diet index (PDI)",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("blueviolet", "darkred", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space  = "free")

meta.forest_PDI

ggsave(meta.forest_PDI,
       file = "ALL/compare.IMP.mPDIs_obs.forest_PDI.png",
       height = 12,
       width = 11)

################################################################################
