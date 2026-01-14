################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 07-Aug-2025
# This script is to compare sensitivity analysis results for plant-based diet indices (PDIs).
## (3) For the unhealthful plant-based diet index (uPDI)

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
meta.res_uPDI_bin <- rbind(
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_bin.xlsx"),
    data.frame(`Results from` = c(
      rep("Main analysis (with imputed data)", nrow(
        read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_bin.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/MAIN_meta.tbl_uPDI_bin.xlsx"),
    data.frame(`Results from` = c(
      rep("Sensitivity analysis (with observed data)", nrow(
        read.xlsx("ALL/MAIN_meta.tbl_uPDI_bin.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDIm_bin.xlsx"),
    data.frame(`Results from` = c(
      rep(
        "Sensitivity analysis\n(with modified uPDI definition and imputed data)",
        nrow(read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDIm_bin.xlsx"))
      )
    ))
  )
)

meta.res_uPDI_bin[, c("b", "se", "pval")] <-
  sapply(meta.res_uPDI_bin[, c("b", "se", "pval")], as.numeric)

colnames(meta.res_uPDI_bin)[which(colnames(meta.res_uPDI_bin) == "Results.from")] <- "Results from"
meta.res_uPDI_bin$`Results from` <- factor(
  meta.res_uPDI_bin$`Results from`,
  levels = c(
    "Sensitivity analysis\n(with modified uPDI definition and imputed data)",
    "Sensitivity analysis (with observed data)",
    "Main analysis (with imputed data)"
  ),
  ordered = T
)

meta.res_uPDI_bin$Group <- "Binary outcome"

meta.res_uPDI_bin
str(meta.res_uPDI_bin)

################################################################################
################################################################################

## Continuous outcomes
meta.res_uPDI_con <- rbind(
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_con.xlsx"),
    data.frame(`Results from` = c(
      rep("Main analysis (with imputed data)", nrow(
        read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_con.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/MAIN_meta.tbl_uPDI_con.xlsx"),
    data.frame(`Results from` = c(
      rep("Sensitivity analysis (with observed data)", nrow(
        read.xlsx("ALL/MAIN_meta.tbl_uPDI_con.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDIm_con.xlsx"),
    data.frame(`Results from` = c(
      rep(
        "Sensitivity analysis\n(with modified uPDI definition and imputed data)",
        nrow(read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDIm_con.xlsx"))
      )
    ))
  )
)

meta.res_uPDI_con[, c("b", "se", "pval")] <-
  sapply(meta.res_uPDI_con[, c("b", "se", "pval")], as.numeric)

colnames(meta.res_uPDI_con)[which(colnames(meta.res_uPDI_con) == "Results.from")] <- "Results from"
meta.res_uPDI_con$`Results from` <- factor(
  meta.res_uPDI_con$`Results from`,
  levels = c(
    "Sensitivity analysis\n(with modified uPDI definition and imputed data)",
    "Sensitivity analysis (with observed data)",
    "Main analysis (with imputed data)"
  ),
  ordered = T
)

meta.res_uPDI_con$Group <- "Continuous outcome"

meta.res_uPDI_con
str(meta.res_uPDI_con)

################################################################################
################################################################################

## Ordinal outcome (i.e., breastfeeding duration)
meta.res_uPDI_ord <- rbind(
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_ord.xlsx"),
    data.frame(`Results from` = c(
      rep("Main analysis (with imputed data)", nrow(
        read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDI_ord.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/MAIN_meta.tbl_uPDI_ord.xlsx"),
    data.frame(`Results from` = c(
      rep("Sensitivity analysis (with observed data)", nrow(
        read.xlsx("ALL/MAIN_meta.tbl_uPDI_ord.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDIm_ord.xlsx"),
    data.frame(`Results from` = c(
      rep(
        "Sensitivity analysis\n(with modified uPDI definition and imputed data)",
        nrow(read.xlsx("ALL/IMP_MAIN_meta.tbl_uPDIm_ord.xlsx"))
      )
    ))
  )
)

meta.res_uPDI_ord[, c("b", "se", "pval")] <-
  sapply(meta.res_uPDI_ord[, c("b", "se", "pval")], as.numeric)

colnames(meta.res_uPDI_ord)[which(colnames(meta.res_uPDI_ord) == "Results.from")] <- "Results from"
meta.res_uPDI_ord$`Results from` <- factor(
  meta.res_uPDI_ord$`Results from`,
  levels = c(
    "Sensitivity analysis\n(with modified uPDI definition and imputed data)",
    "Sensitivity analysis (with observed data)",
    "Main analysis (with imputed data)"
  ),
  ordered = T
)

meta.res_uPDI_ord$Group <- "Ordinal outcome"

meta.res_uPDI_ord
str(meta.res_uPDI_ord)

################################################################################

# Nightingale forestplots
meta.res_uPDI <- rbind(
  subset(
    meta.res_uPDI_bin,
    select = c("Outcome", "Exposure", "b", "se", "pval", "Results from", "Group")
  ),
  subset(
    meta.res_uPDI_con,
    select = c("Outcome", "Exposure", "b", "se", "pval", "Results from", "Group")
  ),
  subset(
    meta.res_uPDI_ord,
    select = c("Outcome", "Exposure", "b", "se", "pval", "Results from", "Group")
  )
)

meta.res_uPDI$Group <- factor(
  meta.res_uPDI$Group,
  levels = c("Binary outcome", "Continuous outcome", "Ordinal outcome"),
  ordered = T
)

meta.forest_uPDI <- ggforestplot::forestplot(
  df = meta.res_uPDI,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `Results from`,
  shape = `Results from`,
  xlab = "logOR or beta and 95% CI",
  title = "Unhealthful plant-based diet index (uPDI)",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("blueviolet", "darkred", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space  = "free")

meta.forest_uPDI

ggsave(meta.forest_uPDI,
       file = "ALL/compare.IMP.muPDIs_obs.forest_uPDI.png",
       height = 12,
       width = 11)

################################################################################
