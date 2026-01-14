################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 18-May-2024
# This script is to produce descriptive tables for nutrient intakes in Project Viva.

################################################################################

#------------------------------------------------------------------------------#
#                                 Housekeeping                                 #----
#------------------------------------------------------------------------------#

# Clear environment
rm(list = ls())

# Collect information about the current R session
sessionInfo()

# Load packages
pacman::p_load(
  tidyverse,
  haven,
  descr,
  expss,
  gtsummary,
  kableExtra,
  flextable,
  readr,
  magrittr,
  openxlsx,
  corrplot,
  ggcorrplot
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/Viva/dat_exp_cov_out.rds")
dat <- zap_labels(dat)

head(dat)
dim(dat)  # 1872  606

################################################################################

theme_gtsummary_compact()  # Setting theme `Compact` for tbl_summary (reduces font size and cell padding)

#------------------------------------------------------------------------------#
#                      Prepare Nutrient Intake Variables                       #----
#------------------------------------------------------------------------------#

# Load label list
nutrient_labels <-
  read.xlsx("data/Viva/Viva_nutrient_varlab.xlsx", sheet = "Sheet1")
nutrient_labels
str(nutrient_labels)  # (60 nutrients * 2 timepoints) + 60 averaged = 180 obs.

# Compute mean intake of each nutrient from early to mid-pregnancy
cols <- nutrient_labels$varname[1:120]

EAR_cols <- grep("f1$", cols, value = T)

for (col in EAR_cols) {
  MID_col <- sub("f1", "f2", col)
  dat[[paste0(col, "_f2")]] <- rowMeans(dat[, c(col, MID_col)], na.rm = T)
}

# Assign label to each variable
for (var_name in nutrient_labels$varname) {
  var_lab(dat[, var_name]) <-
    nutrient_labels$label[which(nutrient_labels$varname == var_name)]
}

dim(dat)  # 1872  657

#------------------------------------------------------------------------------#
#                              Descriptive Tables                              #----
#------------------------------------------------------------------------------#

# Table 006 - Nutrient intakes by vegetarian subgroups
tab_006 <- tbl_summary(
  subset(dat, select = c(
    "VegDiet_3cat", nutrient_labels$varname[121:180]
  )),
  by = VegDiet_3cat,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  missing = "no"
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Daily nutrient intake**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data [presented as mean (standard deviation)] were energy-adjusted using the residual method."
  ) %>%
  modify_caption("Daily nutrient intakes by vegetarian subgroups") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_006

tab_006 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/Viva/006-nutrients_by_vegetarian_3cat.docx")

################################################################################

# Table 006a - Nutrient intakes by vegetarian subgroups (4 categories)
tab_006a <- tbl_summary(
  subset(dat, select = c(
    "VegDiet_subgroup", nutrient_labels$varname[121:180]
  )),
  by = VegDiet_subgroup,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  missing = "no"
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Daily nutrient intake**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data [presented as mean (standard deviation)] were energy-adjusted using the residual method."
  ) %>%
  modify_caption("Daily nutrient intakes by vegetarian subgroups (4 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_006a

tab_006a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/006a-nutrients_by_vegetarian_subgroup.docx")

#------------------------------------------------------------------------------#
#                             Descriptive Figures                              #----
#------------------------------------------------------------------------------#

# Correlation matrix between PDIs and nutrient intakes

## Rename variables as labels
nutrient_labels_DUR.p <- nutrient_labels[121:180, ]

corr_data <- subset(dat,
                    select = c("PDI", "hPDI", "uPDI", nutrient_labels_DUR.p$varname))

colnames(corr_data) <- c("PDI",
                         "hPDI",
                         "uPDI",
                         gsub(" during pregnancy", "", nutrient_labels_DUR.p$label))

corr_data

## Compute the correlation matrix
corr_matrix <-
  cor(corr_data, use = "pairwise.complete.obs", method = "pearson")

## Use ggcorrplot to generate half correlation matrix plot
correlogram_PDIs_nutrients_DUR.p <- ggcorrplot(
  corr_matrix,
  method = "square",
  type = "lower",
  # Only show the lower half
  lab = TRUE,
  lab_size = 4,
  lab_col = "black",
  show.legend = TRUE,
  tl.cex = 12,
  tl.srt = 45,
  # Rotate labels
  hc.order = FALSE
) + # Set to FALSE to keep the original order
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 14,
      color = "black"
    ),
    # Rotate x-axis labels
    axis.text.y = element_text(size = 14, color = "black"),
    # Adjust y-axis label size
    axis.title.x = element_blank(),
    # Hide x-axis title
    axis.title.y = element_blank(),
    # Hide y-axis title
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  # Reduce plot margins
  )

## Save the plot
ggsave(
  filename = "results/Viva/corr_PDIs_nutrients_DUR.p.png",
  plot = correlogram_PDIs_nutrients_DUR.p,
  height = 30,
  width = 49
)
