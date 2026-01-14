################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALL        #
################################################################################

# Last edited date: 14-Dec-2024
# This script is to perform meta-analysis on paternal negative control analysis (with imputed data) results for vegetarian diets.
## Part 3: Combine Maternal Model 1 + Maternal Model 2 + Paternal Model 1 + Paternal Model 2

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
               png,
               gridExtra,
               cowplot,
               grid)

# Set working directory
setwd("Z:/working/results/")

#------------------------------------------------------------------------------#
#                               Data Preparation                               #----
#------------------------------------------------------------------------------#

# Load meta-analysis forest plots

## ALL binary outcomes
metaforest_obs.res_VegDiet_bin_Mat1 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat1.png")
metaforest_obs.res_VegDiet_bin_Mat2 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat2.png")
metaforest_obs.res_VegDiet_bin_Pat1 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat1.png")
metaforest_obs.res_VegDiet_bin_Pat2 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat2.png")

### Binary pregnancy outcomes
metaforest_obs.res_VegDiet_bin_Mat1_Preg <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat1_Preg.png")
metaforest_obs.res_VegDiet_bin_Mat2_Preg <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat2_Preg.png")
metaforest_obs.res_VegDiet_bin_Pat1_Preg <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat1_Preg.png")
metaforest_obs.res_VegDiet_bin_Pat2_Preg <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat2_Preg.png")

### Binary delivery outcomes
metaforest_obs.res_VegDiet_bin_Mat1_Deli <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat1_Deli.png")
metaforest_obs.res_VegDiet_bin_Mat2_Deli <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat2_Deli.png")
metaforest_obs.res_VegDiet_bin_Pat1_Deli <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat1_Deli.png")
metaforest_obs.res_VegDiet_bin_Pat2_Deli <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat2_Deli.png")

### Binary postnatal outcomes
metaforest_obs.res_VegDiet_bin_Mat1_Post <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat1_Post.png")
metaforest_obs.res_VegDiet_bin_Mat2_Post <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Mat2_Post.png")
metaforest_obs.res_VegDiet_bin_Pat1_Post <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat1_Post.png")
metaforest_obs.res_VegDiet_bin_Pat2_Post <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_bin_Pat2_Post.png")

################################################################################

## Continuous outcomes
metaforest_obs.res_VegDiet_con_Mat1 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_con_Mat1.png")
metaforest_obs.res_VegDiet_con_Mat2 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_con_Mat2.png")
metaforest_obs.res_VegDiet_con_Pat1 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_con_Pat1.png")
metaforest_obs.res_VegDiet_con_Pat2 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_con_Pat2.png")

################################################################################

## Ordinal outcome (breastfeeding duration)
metaforest_obs.res_VegDiet_ord_Mat1 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_ord_Mat1.png")
metaforest_obs.res_VegDiet_ord_Mat2 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_ord_Mat2.png")
metaforest_obs.res_VegDiet_ord_Pat1 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_ord_Pat1.png")
metaforest_obs.res_VegDiet_ord_Pat2 <- readPNG("ALL/IMP_PNC_meta.forest_VegDiet_ord_Pat2.png")

#------------------------------------------------------------------------------#
#                                Combine Plots                                 #----
#------------------------------------------------------------------------------#

# ALL binary outcomes
grob1 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Mat1, interpolate = T)
grob2 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Mat2, interpolate = T)
grob3 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Pat1, interpolate = T)
grob4 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Pat2, interpolate = T)

title1 <- textGrob(
  "Pesco-/full vs. non-vegetarian for binary outcomes: Maternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title2 <- textGrob(
  "Maternal Model 2 (Maternal Model 1 additionally adjusted for paternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title3 <- textGrob(
  "Pesco-/full vs. non-vegetarian for binary outcomes: Paternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title4 <- textGrob(
  "Paternal Model 2 (Paternal Model 1 additionally adjusted for maternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.01, 0.99))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.01, 0.99))
grob3_with_title <- arrangeGrob(title3, grob3, ncol = 1, heights = c(0.01, 0.99))
grob4_with_title <- arrangeGrob(title4, grob4, ncol = 1, heights = c(0.01, 0.99))

png(
  "ALL/Comb_IMP_PNC_meta.forest_VegDiet_bin.png",
  res = 300,
  height = 7000,
  width = 10500
)
grid.arrange(grob1_with_title,
             grob2_with_title,
             grob3_with_title,
             grob4_with_title,
             ncol = 4)
dev.off()

################################################################################
################################################################################
################################################################################
## Binary pregnancy outcomes
grob1 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Mat1_Preg, interpolate = T)
grob2 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Mat2_Preg, interpolate = T)
grob3 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Pat1_Preg, interpolate = T)
grob4 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Pat2_Preg, interpolate = T)

title1 <- textGrob(
  "Pesco-/full vs. non-vegetarian for binary pregnancy outcomes: Maternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title2 <- textGrob(
  "Maternal Model 2 (Maternal Model 1 additionally adjusted for paternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title3 <- textGrob(
  "Pesco-/full vs. non-vegetarian for binary pregnancy outcomes: Paternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title4 <- textGrob(
  "Paternal Model 2 (Paternal Model 1 additionally adjusted for maternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.01, 0.99))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.01, 0.99))
grob3_with_title <- arrangeGrob(title3, grob3, ncol = 1, heights = c(0.01, 0.99))
grob4_with_title <- arrangeGrob(title4, grob4, ncol = 1, heights = c(0.01, 0.99))

png(
  "ALL/Comb_IMP_PNC_meta.forest_VegDiet_bin_Preg.png",
  res = 300,
  height = 2300,
  width = 10500
)
grid.arrange(grob1_with_title,
             grob2_with_title,
             grob3_with_title,
             grob4_with_title,
             ncol = 4)
dev.off()
################################################################################
################################################################################
################################################################################
## Binary delivery outcomes
grob1 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Mat1_Deli, interpolate = T)
grob2 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Mat2_Deli, interpolate = T)
grob3 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Pat1_Deli, interpolate = T)
grob4 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Pat2_Deli, interpolate = T)

title1 <- textGrob(
  "Pesco-/full vs. non-vegetarian for binary delivery outcomes: Maternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title2 <- textGrob(
  "Maternal Model 2 (Maternal Model 1 additionally adjusted for paternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title3 <- textGrob(
  "Pesco-/full vs. non-vegetarian for binary delivery outcomes: Paternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title4 <- textGrob(
  "Paternal Model 2 (Paternal Model 1 additionally adjusted for maternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.01, 0.99))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.01, 0.99))
grob3_with_title <- arrangeGrob(title3, grob3, ncol = 1, heights = c(0.01, 0.99))
grob4_with_title <- arrangeGrob(title4, grob4, ncol = 1, heights = c(0.01, 0.99))

png(
  "ALL/Comb_IMP_PNC_meta.forest_VegDiet_bin_Deli.png",
  res = 300,
  height = 4000,
  width = 10500
)
grid.arrange(grob1_with_title,
             grob2_with_title,
             grob3_with_title,
             grob4_with_title,
             ncol = 4)
dev.off()
################################################################################
################################################################################
################################################################################
## Binary postnatal outcomes
grob1 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Mat1_Post, interpolate = T)
grob2 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Mat2_Post, interpolate = T)
grob3 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Pat1_Post, interpolate = T)
grob4 <- rasterGrob(metaforest_obs.res_VegDiet_bin_Pat2_Post, interpolate = T)

title1 <- textGrob(
  "Pesco-/full vs. non-vegetarian for binary postnatal outcomes: Maternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title2 <- textGrob(
  "Maternal Model 2 (Maternal Model 1 additionally adjusted for paternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title3 <- textGrob(
  "Pesco-/full vs. non-vegetarian for binary postnatal outcomes: Paternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title4 <- textGrob(
  "Paternal Model 2 (Paternal Model 1 additionally adjusted for maternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.01, 0.99))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.01, 0.99))
grob3_with_title <- arrangeGrob(title3, grob3, ncol = 1, heights = c(0.01, 0.99))
grob4_with_title <- arrangeGrob(title4, grob4, ncol = 1, heights = c(0.01, 0.99))

png(
  "ALL/Comb_IMP_PNC_meta.forest_VegDiet_bin_Post.png",
  res = 300,
  height = 1500,
  width = 10500
)
grid.arrange(grob1_with_title,
             grob2_with_title,
             grob3_with_title,
             grob4_with_title,
             ncol = 4)
dev.off()

################################################################################

# Continuous outcomes
grob1 <- rasterGrob(metaforest_obs.res_VegDiet_con_Mat1, interpolate = T)
grob2 <- rasterGrob(metaforest_obs.res_VegDiet_con_Mat2, interpolate = T)
grob3 <- rasterGrob(metaforest_obs.res_VegDiet_con_Pat1, interpolate = T)
grob4 <- rasterGrob(metaforest_obs.res_VegDiet_con_Pat2, interpolate = T)

title1 <- textGrob(
  "Pesco-/full vs. non-vegetarian for continuous outcomes: Maternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title2 <- textGrob(
  "Maternal Model 2 (Maternal Model 1 additionally adjusted for paternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title3 <- textGrob(
  "Pesco-/full vs. non-vegetarian for continuous outcomes: Paternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title4 <- textGrob(
  "Paternal Model 2 (Paternal Model 1 additionally adjusted for maternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.01, 0.99))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.01, 0.99))
grob3_with_title <- arrangeGrob(title3, grob3, ncol = 1, heights = c(0.01, 0.99))
grob4_with_title <- arrangeGrob(title4, grob4, ncol = 1, heights = c(0.01, 0.99))

png(
  "ALL/Comb_IMP_PNC_meta.forest_VegDiet_con.png",
  res = 300,
  height = 1900,
  width = 8500
)
grid.arrange(grob1_with_title,
             grob2_with_title,
             grob3_with_title,
             grob4_with_title,
             ncol = 4)
dev.off()

################################################################################

# Ordinal outcome (breastfeeding duration)
grob1 <- rasterGrob(metaforest_obs.res_VegDiet_ord_Mat1, interpolate = T)
grob2 <- rasterGrob(metaforest_obs.res_VegDiet_ord_Mat2, interpolate = T)
grob3 <- rasterGrob(metaforest_obs.res_VegDiet_ord_Pat1, interpolate = T)
grob4 <- rasterGrob(metaforest_obs.res_VegDiet_ord_Pat2, interpolate = T)

title1 <- textGrob(
  "Pesco-/full vs. non-vegetarian for breastfeeding duration (ordinal): Maternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title2 <- textGrob(
  "Maternal Model 2 (Maternal Model 1 additionally adjusted for paternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title3 <- textGrob(
  "Pesco-/full vs. non-vegetarian for breastfeeding duration (ordinal): Paternal Model 1",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)
title4 <- textGrob(
  "Paternal Model 2 (Paternal Model 1 additionally adjusted for maternal exposure)",
  gp = gpar(fontsize = 12, fontface = "bold"),
  vjust = 2
)

grob1_with_title <- arrangeGrob(title1, grob1, ncol = 1, heights = c(0.01, 0.99))
grob2_with_title <- arrangeGrob(title2, grob2, ncol = 1, heights = c(0.01, 0.99))
grob3_with_title <- arrangeGrob(title3, grob3, ncol = 1, heights = c(0.01, 0.99))
grob4_with_title <- arrangeGrob(title4, grob4, ncol = 1, heights = c(0.01, 0.99))

png(
  "ALL/Comb_IMP_PNC_BF_meta.forest_VegDiet_ord.png",
  res = 300,
  height = 780,
  width = 8500
)
grid.arrange(grob1_with_title,
             grob2_with_title,
             grob3_with_title,
             grob4_with_title,
             ncol = 4)
dev.off()
