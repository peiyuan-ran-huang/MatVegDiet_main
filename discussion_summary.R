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
setwd("Z:/working/results/ALL")

################################################################################

# Outcome grouping

## Load outcome lists and labels
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
secondary_ord  # 1 (secondary) ordinal/categorical outcome (bf_dur_4c)

## Select primary outcomes
Prim_Out <- c(primary_bin$label, "Post-term birth", "Breastfeeding duration")  # Update on 16-Jun-2025: Added post-term birth and breastfeeding duration as primary outcomes.
Prim_Out  # 15 primary outcomes in total

## Create outcome label list
for (i in 1:nrow(MRPREG_outcome_labels)) {
  if (MRPREG_outcome_labels$label[i] %in% Prim_Out) {
    MRPREG_outcome_labels$label[i] <- paste0(MRPREG_outcome_labels$label[i], " †")
  }
}
MRPREG_outcome_labels$label

################################################################################

# Vegetarianism

## Pesco-/full vegetarianism
V_res_bin <- read.xlsx("IMP_MAIN_meta.tbl_VegDiet_bin.xlsx")
V_res_con <- read.xlsx("IMP_MAIN_meta.tbl_VegDiet_con.xlsx")
V_res_ord <- read.xlsx("IMP_MAIN_meta.tbl_VegDiet_ord_FINAL.xlsx")

V_res <- rbind(
  subset(V_res_bin, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(V_res_con, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(V_res_ord, select = c("Outcome", "Exposure", "b", "se", "pval"))
)

V_res$Exposure <- "Pesco-/full vegetarian\nvs. non-vegetarian"
V_res[, c("b", "se", "pval")] <- lapply(V_res[, c("b", "se", "pval")], as.numeric)
V_res$`logOR or beta / SE` <- V_res$b / V_res$se
for (i in 1:nrow(V_res)) {
  if (V_res$Outcome[i] %in% Prim_Out) {
    V_res$Outcome[i] <- paste0(V_res$Outcome[i], " †")
  }
}
V_res$Outcome <- factor(V_res$Outcome, levels = rev(unique(MRPREG_outcome_labels$label)))
V_res <- V_res %>% arrange(Outcome)

V_res
str(V_res)

## Pesco-vegetarianism
PV_res_bin <- read.xlsx("IMP_MAIN_meta.tbl_VegDiet.subgroup_bin_pesco.V.xlsx")
PV_res_con <- read.xlsx("IMP_MAIN_meta.tbl_VegDiet.subgroup_con_pesco.V.xlsx")
PV_res_ord <- read.xlsx("IMP_BF_meta.tbl_VegDiet.subgroup_ord_pesco.V.xlsx")

PV_res <- rbind(
  subset(PV_res_bin, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(PV_res_con, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(PV_res_ord, select = c("Outcome", "Exposure", "b", "se", "pval"))
)

PV_res$Exposure <- "Pesco-vegetarian\nvs. non-vegetarian"
PV_res[, c("b", "se", "pval")] <- lapply(PV_res[, c("b", "se", "pval")], as.numeric)
PV_res$`logOR or beta / SE` <- PV_res$b / PV_res$se
for (i in 1:nrow(PV_res)) {
  if (PV_res$Outcome[i] %in% Prim_Out) {
    PV_res$Outcome[i] <- paste0(PV_res$Outcome[i], " †")
  }
}
PV_res$Outcome <- factor(PV_res$Outcome, levels = rev(unique(MRPREG_outcome_labels$label)))
PV_res <- PV_res %>% arrange(Outcome)

PV_res
str(PV_res)

## Full vegetarianism
FV_res_bin <- read.xlsx("IMP_MAIN_meta.tbl_VegDiet.subgroup_bin_full.V.xlsx")
FV_res_con <- read.xlsx("IMP_MAIN_meta.tbl_VegDiet.subgroup_con_full.V.xlsx")
FV_res_ord <- read.xlsx("IMP_BF_meta.tbl_VegDiet.subgroup_ord_full.V.xlsx")

FV_res <- rbind(
  subset(FV_res_bin, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(FV_res_con, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(FV_res_ord, select = c("Outcome", "Exposure", "b", "se", "pval"))
)

FV_res$Exposure <- "Full vegetarian\nvs. non-vegetarian"
FV_res[, c("b", "se", "pval")] <- lapply(FV_res[, c("b", "se", "pval")], as.numeric)
FV_res$`logOR or beta / SE` <- FV_res$b / FV_res$se
for (i in 1:nrow(FV_res)) {
  if (FV_res$Outcome[i] %in% Prim_Out) {
    FV_res$Outcome[i] <- paste0(FV_res$Outcome[i], " †")
  }
}
FV_res$Outcome <- factor(FV_res$Outcome, levels = rev(unique(MRPREG_outcome_labels$label)))
FV_res <- FV_res %>% arrange(Outcome)

FV_res
str(FV_res)

###############################################################################

# Plant-based diet indices

## PDI
PDI_res_bin <- read.xlsx("IMP_MAIN_meta.tbl_PDI_bin.xlsx")
PDI_res_con <- read.xlsx("IMP_MAIN_meta.tbl_PDI_con.xlsx")
PDI_res_ord <- read.xlsx("IMP_MAIN_meta.tbl_PDI_ord.xlsx")

PDI_res <- rbind(
  subset(PDI_res_bin, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(PDI_res_con, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(PDI_res_ord, select = c("Outcome", "Exposure", "b", "se", "pval"))
)

PDI_res$Exposure <- "PDI\n(per SD increase)"
PDI_res[, c("b", "se", "pval")] <- lapply(PDI_res[, c("b", "se", "pval")], as.numeric)
PDI_res$`logOR or beta / SE` <- PDI_res$b / PDI_res$se
for (i in 1:nrow(PDI_res)) {
  if (PDI_res$Outcome[i] %in% Prim_Out) {
    PDI_res$Outcome[i] <- paste0(PDI_res$Outcome[i], " †")
  }
}
PDI_res$Outcome <- factor(PDI_res$Outcome, levels = rev(unique(MRPREG_outcome_labels$label)))
PDI_res <- PDI_res %>% arrange(Outcome)

PDI_res
str(PDI_res)

## hPDI
hPDI_res_bin <- read.xlsx("IMP_MAIN_meta.tbl_hPDI_bin.xlsx")
hPDI_res_con <- read.xlsx("IMP_MAIN_meta.tbl_hPDI_con.xlsx")
hPDI_res_ord <- read.xlsx("IMP_MAIN_meta.tbl_hPDI_ord.xlsx")

hPDI_res <- rbind(
  subset(hPDI_res_bin, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(hPDI_res_con, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(hPDI_res_ord, select = c("Outcome", "Exposure", "b", "se", "pval"))
)

hPDI_res$Exposure <- "hPDI\n(per SD increase)"
hPDI_res[, c("b", "se", "pval")] <- lapply(hPDI_res[, c("b", "se", "pval")], as.numeric)
hPDI_res$`logOR or beta / SE` <- hPDI_res$b / hPDI_res$se
for (i in 1:nrow(hPDI_res)) {
  if (hPDI_res$Outcome[i] %in% Prim_Out) {
    hPDI_res$Outcome[i] <- paste0(hPDI_res$Outcome[i], " †")
  }
}
hPDI_res$Outcome <- factor(hPDI_res$Outcome, levels = rev(unique(MRPREG_outcome_labels$label)))
hPDI_res <- hPDI_res %>% arrange(Outcome)

hPDI_res
str(hPDI_res)

## uPDI
uPDI_res_bin <- read.xlsx("IMP_MAIN_meta.tbl_uPDI_bin.xlsx")
uPDI_res_con <- read.xlsx("IMP_MAIN_meta.tbl_uPDI_con.xlsx")
uPDI_res_ord <- read.xlsx("IMP_MAIN_meta.tbl_uPDI_ord.xlsx")

uPDI_res <- rbind(
  subset(uPDI_res_bin, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(uPDI_res_con, select = c("Outcome", "Exposure", "b", "se", "pval")),
  subset(uPDI_res_ord, select = c("Outcome", "Exposure", "b", "se", "pval"))
)

uPDI_res$Exposure <- "uPDI\n(per SD increase)"
uPDI_res[, c("b", "se", "pval")] <- lapply(uPDI_res[, c("b", "se", "pval")], as.numeric)
uPDI_res$`logOR or beta / SE` <- uPDI_res$b / uPDI_res$se
for (i in 1:nrow(uPDI_res)) {
  if (uPDI_res$Outcome[i] %in% Prim_Out) {
    uPDI_res$Outcome[i] <- paste0(uPDI_res$Outcome[i], " †")
  }
}
uPDI_res$Outcome <- factor(uPDI_res$Outcome, levels = rev(unique(MRPREG_outcome_labels$label)))
uPDI_res <- uPDI_res %>% arrange(Outcome)

uPDI_res
str(uPDI_res)

###############################################################################

# Heatmap

## Prepare data

### Combine results
Veg_res <- rbind(V_res, PV_res, FV_res)
Veg_res$Exposure <- factor(
  Veg_res$Exposure,
  levels = c(
    "Pesco-/full vegetarian\nvs. non-vegetarian",
    "Pesco-vegetarian\nvs. non-vegetarian",
    "Full vegetarian\nvs. non-vegetarian"
  )
)
Veg_res$Group <- "Pregnancy & perinatal outcome (binary)"
Veg_res$Group[Veg_res$Outcome %in% c(
  "Gestational age (weeks)",
  "Birth weight z-score",
  "Apgar score at 1 minute",
  "Apgar score at 5 minutes"
)] <- "Pregnancy & perinatal outcome (continuous)"
Veg_res$Group[Veg_res$Outcome %in% c("Breastfeeding duration †")] <- "Pregnancy & perinatal outcome (ordinal)"
Veg_res$Group <-
  factor(
    Veg_res$Group,
    levels = c(
      "Pregnancy & perinatal outcome (binary)",
      "Pregnancy & perinatal outcome (continuous)",
      "Pregnancy & perinatal outcome (ordinal)"
    )
  )

PDIs_res <- rbind(PDI_res, hPDI_res, uPDI_res)
PDIs_res$Exposure <- factor(
  PDIs_res$Exposure,
  levels = c(
    "PDI\n(per SD increase)",
    "hPDI\n(per SD increase)",
    "uPDI\n(per SD increase)"
  )
)
PDIs_res$Group <- "Pregnancy & perinatal outcome (binary)"
PDIs_res$Group[PDIs_res$Outcome %in% c(
  "Gestational age (weeks)",
  "Birth weight z-score",
  "Apgar score at 1 minute",
  "Apgar score at 5 minutes"
)] <- "Pregnancy & perinatal outcome (continuous)"
PDIs_res$Group[PDIs_res$Outcome %in% c("Breastfeeding duration †")] <- "Pregnancy & perinatal outcome (ordinal)"
PDIs_res$Group <-
  factor(
    PDIs_res$Group,
    levels = c(
      "Pregnancy & perinatal outcome (binary)",
      "Pregnancy & perinatal outcome (continuous)",
      "Pregnancy & perinatal outcome (ordinal)"
    )
  )

all_res <- rbind(Veg_res, PDIs_res)

### Check the upper and lower bound of heatmap legend
min(all_res$`logOR or beta / SE`, na.rm = T)  # -6.424072
max(all_res$`logOR or beta / SE`, na.rm = T)  # 7.402909

## Heatmap for vegetarianism
Veg_res$Sig <- cut(
  Veg_res$pval,
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
)

Veg_heatmap <- ggplot(Veg_res, aes(x = Exposure, y = Outcome)) +
  geom_tile(aes(fill = `logOR or beta / SE`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-7, 8),
    name = "logOR or beta / SE"
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
    legend.position = "bottom",
    legend.justification = "right"
  ) +
  labs(x = "", y = "")

Veg_heatmap

ggsave(
  "Z:/working/results/Veg_heatmap.png",
  Veg_heatmap,
  width = 6,
  height = 8
)

## Heatmap for PDIs
PDIs_res$Sig <- cut(
  PDIs_res$pval,
  breaks = c(0, 0.001, 0.01, 0.05, 1),
  labels = c("***", "**", "*", ""),
  right = FALSE
)

PDIs_heatmap <- ggplot(PDIs_res, aes(x = Exposure, y = Outcome)) +
  geom_tile(aes(fill = `logOR or beta / SE`), color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    high = "firebrick",
    mid = "white",
    midpoint = 0,
    limit = c(-7, 8),
    name = "logOR or beta / SE"
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
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none") +
  labs(x = "", y = "")

PDIs_heatmap

ggsave(
  "Z:/working/results/PDIs_heatmap.png",
  PDIs_heatmap,
  width = 6,
  height = 8
)

# Combine
Veg_heatmap <- Veg_heatmap +
  labs(title = "Vegetarianism (Study 1)") +
  theme(plot.title = element_text(size = 10),
        strip.text = element_text(hjust = 0))

PDIs_heatmap <- PDIs_heatmap +
  labs(title = "Plant-based diet indices (Study 2)") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    strip.text = element_text(hjust = 99)
  )

Veg_PDIs_heatmap <- Veg_heatmap +
  PDIs_heatmap +
  plot_layout(ncol = 2, widths = c(1, 1)) &
  theme(plot.margin = margin(5, 0))

Veg_PDIs_heatmap

ggsave(
  "Z:/working/results/!!!Veg_PDIs_heatmap.png",
  Veg_PDIs_heatmap,
  width = 10,
  height = 8
)
