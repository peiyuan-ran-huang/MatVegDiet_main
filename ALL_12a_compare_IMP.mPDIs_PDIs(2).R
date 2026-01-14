################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 07-Aug-2025
# This script is to compare sensitivity analysis results for plant-based diet indices (PDIs).
## (2) For the healthful plant-based diet index (hPDI)

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
meta.res_hPDI_bin <- rbind(
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_bin.xlsx"),
    data.frame(`Results from` = c(
      rep("Main analysis (with imputed data)", nrow(
        read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_bin.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/MAIN_meta.tbl_hPDI_bin.xlsx"),
    data.frame(`Results from` = c(
      rep("Sensitivity analysis (with observed data)", nrow(
        read.xlsx("ALL/MAIN_meta.tbl_hPDI_bin.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDIm_bin.xlsx"),
    data.frame(`Results from` = c(
      rep(
        "Sensitivity analysis\n(with modified hPDI definition and imputed data)",
        nrow(read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDIm_bin.xlsx"))
      )
    ))
  )
)

meta.res_hPDI_bin[, c("b", "se", "pval")] <-
  sapply(meta.res_hPDI_bin[, c("b", "se", "pval")], as.numeric)

colnames(meta.res_hPDI_bin)[which(colnames(meta.res_hPDI_bin) == "Results.from")] <- "Results from"
meta.res_hPDI_bin$`Results from` <- factor(
  meta.res_hPDI_bin$`Results from`,
  levels = c(
    "Sensitivity analysis\n(with modified hPDI definition and imputed data)",
    "Sensitivity analysis (with observed data)",
    "Main analysis (with imputed data)"
  ),
  ordered = T
)

meta.res_hPDI_bin$Group <- "Binary outcome"

meta.res_hPDI_bin
str(meta.res_hPDI_bin)

################################################################################
################################################################################

## Continuous outcomes
meta.res_hPDI_con <- rbind(
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_con.xlsx"),
    data.frame(`Results from` = c(
      rep("Main analysis (with imputed data)", nrow(
        read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_con.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/MAIN_meta.tbl_hPDI_con.xlsx"),
    data.frame(`Results from` = c(
      rep("Sensitivity analysis (with observed data)", nrow(
        read.xlsx("ALL/MAIN_meta.tbl_hPDI_con.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDIm_con.xlsx"),
    data.frame(`Results from` = c(
      rep(
        "Sensitivity analysis\n(with modified hPDI definition and imputed data)",
        nrow(read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDIm_con.xlsx"))
      )
    ))
  )
)

meta.res_hPDI_con[, c("b", "se", "pval")] <-
  sapply(meta.res_hPDI_con[, c("b", "se", "pval")], as.numeric)

colnames(meta.res_hPDI_con)[which(colnames(meta.res_hPDI_con) == "Results.from")] <- "Results from"
meta.res_hPDI_con$`Results from` <- factor(
  meta.res_hPDI_con$`Results from`,
  levels = c(
    "Sensitivity analysis\n(with modified hPDI definition and imputed data)",
    "Sensitivity analysis (with observed data)",
    "Main analysis (with imputed data)"
  ),
  ordered = T
)

meta.res_hPDI_con$Group <- "Continuous outcome"

meta.res_hPDI_con
str(meta.res_hPDI_con)

################################################################################
################################################################################

## Ordinal outcome (i.e., breastfeeding duration)
meta.res_hPDI_ord <- rbind(
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_ord.xlsx"),
    data.frame(`Results from` = c(
      rep("Main analysis (with imputed data)", nrow(
        read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDI_ord.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/MAIN_meta.tbl_hPDI_ord.xlsx"),
    data.frame(`Results from` = c(
      rep("Sensitivity analysis (with observed data)", nrow(
        read.xlsx("ALL/MAIN_meta.tbl_hPDI_ord.xlsx")
      ))
    ))
  ),
  cbind(
    read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDIm_ord.xlsx"),
    data.frame(`Results from` = c(
      rep(
        "Sensitivity analysis\n(with modified hPDI definition and imputed data)",
        nrow(read.xlsx("ALL/IMP_MAIN_meta.tbl_hPDIm_ord.xlsx"))
      )
    ))
  )
)

meta.res_hPDI_ord[, c("b", "se", "pval")] <-
  sapply(meta.res_hPDI_ord[, c("b", "se", "pval")], as.numeric)

colnames(meta.res_hPDI_ord)[which(colnames(meta.res_hPDI_ord) == "Results.from")] <- "Results from"
meta.res_hPDI_ord$`Results from` <- factor(
  meta.res_hPDI_ord$`Results from`,
  levels = c(
    "Sensitivity analysis\n(with modified hPDI definition and imputed data)",
    "Sensitivity analysis (with observed data)",
    "Main analysis (with imputed data)"
  ),
  ordered = T
)

meta.res_hPDI_ord$Group <- "Ordinal outcome"

meta.res_hPDI_ord
str(meta.res_hPDI_ord)

################################################################################

# Nightingale forestplots
meta.res_hPDI <- rbind(
  subset(
    meta.res_hPDI_bin,
    select = c("Outcome", "Exposure", "b", "se", "pval", "Results from", "Group")
  ),
  subset(
    meta.res_hPDI_con,
    select = c("Outcome", "Exposure", "b", "se", "pval", "Results from", "Group")
  ),
  subset(
    meta.res_hPDI_ord,
    select = c("Outcome", "Exposure", "b", "se", "pval", "Results from", "Group")
  )
)

meta.res_hPDI$Group <- factor(
  meta.res_hPDI$Group,
  levels = c("Binary outcome", "Continuous outcome", "Ordinal outcome"),
  ordered = T
)

meta.forest_hPDI <- ggforestplot::forestplot(
  df = meta.res_hPDI,
  name = Outcome,
  estimate = b,
  se = se,
  pvalue = pval,
  psignif = 0.05,
  colour = `Results from`,
  shape = `Results from`,
  xlab = "logOR or beta and 95% CI",
  title = "Healthful plant-based diet index (hPDI)",
  logodds = T
) +
  ggplot2::scale_colour_manual(values = c("blueviolet", "darkred", "darkcyan")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space  = "free")

meta.forest_hPDI

ggsave(meta.forest_hPDI,
       file = "ALL/compare.IMP.mhPDIs_obs.forest_hPDI.png",
       height = 12,
       width = 11)

################################################################################
