################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 07-May-2025
# This script is to produce main descriptive tables in Project Viva.

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
  Hmisc,
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
# Check maximum N (defined as those with data on at least one outcome)
my_out <- c(
  "hdp_subsamp",
  "gh_subsamp",
  "pe_subsamp",
  "gdm_subsamp",
  "anaemia_preg_all",
  "depr_subsamp",
  "induction",
  "cs",
  "el_cs",
  "em_cs",
  "ga_subsamp",
  "pretb_subsamp",
  "vpretb_subsamp",
  "posttb_subsamp",
  "zbw_subsamp",
  "sga",
  "lga",
  "lbw_subsamp",
  "hbw_subsamp",
  "bf_dur_4c"
)

sum(apply(dat[, my_out], 1, function(x)
  any(!is.na(x))))  # 1872
################################################################################

theme_gtsummary_compact()  # Setting theme `Compact` for tbl_summary (reduces font size and cell padding)

#------------------------------------------------------------------------------#
#                                 Main Tables                                  #----
#------------------------------------------------------------------------------#

# Table 01 - Key maternal characteristics (for vegetarian subgroups)
tab_01 <- tbl_summary(
  dat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    VegDiet_3cat ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_bin ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    PDIm ~ "continuous",
    hPDIm ~ "continuous",
    uPDIm ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "no",
  include = c(
    VegDiet_3cat,
    age_Mat_con,
    ethnic_Mat_bin,
    edu_Mat_3cat,
    income_Fam_3cat,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    PDIm,
    hPDIm,
    uPDIm,
    sex_Chi_bin
  )
) %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Key participant characteristics") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_01

tab_01 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                         "results/Viva/01-KEY-maternal_characteristics_for_vegetarian_3cat.docx")

################################################################################

# Table 02 - Key maternal characteristics (for PDI/hPDI/uPDI)
tab_02 <- tbl_summary(
  dat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDI ~ "continuous",
    hPDI ~ "continuous",
    uPDI ~ "continuous",
    age_Mat_con ~ "continuous",
    ethnic_Mat_bin ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "no",
  include = c(
    PDI,
    hPDI,
    uPDI,
    age_Mat_con,
    ethnic_Mat_bin,
    edu_Mat_3cat,
    income_Fam_3cat,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    hPDI,
    sex_Chi_bin
  )
) %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Key participant characteristics") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_02

tab_02 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                         "results/Viva/02-KEY-maternal_characteristics_for_PDIs.docx")

#------------------------------------------------------------------------------#
#                             Supplementary Tables                             #----
#------------------------------------------------------------------------------#

# Table 001 - All maternal characteristics by vegetarian subgroups (during pregnancy)
tab_001 <- tbl_summary(
  dat,
  by = VegDiet_3cat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDIm ~ "continuous",
    hPDIm ~ "continuous",
    uPDIm ~ "continuous",
    VegDiet_3cat_1 ~ "categorical",
    VegDiet_3cat_2 ~ "categorical",
    self.VegDiet_Mat_EAR.p_bin ~ "categorical",
    self.VegDiet_Mat_MID.p_bin ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    marital_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_PRE.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitA.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    vitB6.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    zinc.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDIm,
    hPDIm,
    uPDIm,
    VegDiet_3cat_1,
    VegDiet_3cat_2,
    self.VegDiet_Mat_EAR.p_bin,
    self.VegDiet_Mat_MID.p_bin,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    marital_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_PRE.p_bin,
    any.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitA.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    vitB6.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    zinc.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    sex_Chi_bin
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Detailed participant characteristics by vegetarian subgroups during pregnancy") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001

tab_001 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/Viva/001-ALL-maternal_characteristics_by_vegetarian_3cat.docx")

################################################################################
################################################################################

# Table 001a - All maternal characteristics by vegetarian subgroups (4 categories) during pregnancy
tab_001a <- tbl_summary(
  dat,
  by = VegDiet_subgroup,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDIm ~ "continuous",
    hPDIm ~ "continuous",
    uPDIm ~ "continuous",
    VegDiet_3cat_1 ~ "categorical",
    VegDiet_3cat_2 ~ "categorical",
    self.VegDiet_Mat_EAR.p_bin ~ "categorical",
    self.VegDiet_Mat_MID.p_bin ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    marital_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_PRE.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitA.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    vitB6.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    zinc.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDIm,
    hPDIm,
    uPDIm,
    VegDiet_3cat_1,
    VegDiet_3cat_2,
    self.VegDiet_Mat_EAR.p_bin,
    self.VegDiet_Mat_MID.p_bin,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    marital_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_PRE.p_bin,
    any.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitA.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    vitB6.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    zinc.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    sex_Chi_bin
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption(
    "Detailed participant characteristics by vegetarian subgroups (4 categories) during pregnancy"
  ) %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001a

tab_001a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/001a-ALL-maternal_characteristics_by_vegetarian_subgroup.docx")

################################################################################
################################################################################
# Table 001b - All maternal characteristics by vegetarian subgroups (in early pregnancy)
tab_001b <- tbl_summary(
  dat,
  by = VegDiet_3cat_1,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDIm ~ "continuous",
    hPDIm ~ "continuous",
    uPDIm ~ "continuous",
    # VegDiet_3cat_1 ~ "categorical",
    VegDiet_3cat_2 ~ "categorical",
    self.VegDiet_Mat_EAR.p_bin ~ "categorical",
    self.VegDiet_Mat_MID.p_bin ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    marital_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_PRE.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitA.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    vitB6.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    zinc.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDIm,
    hPDIm,
    uPDIm,
    # VegDiet_3cat_1,
    VegDiet_3cat_2,
    self.VegDiet_Mat_EAR.p_bin,
    self.VegDiet_Mat_MID.p_bin,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    marital_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_PRE.p_bin,
    any.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitA.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    vitB6.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    zinc.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    sex_Chi_bin
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Detailed participant characteristics by vegetarian subgroups in early pregnancy") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001b

tab_001b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/001b-ALL-maternal_characteristics_by_vegetarian_3cat_EAR.p.docx")
################################################################################
################################################################################
# Table 001c - All maternal characteristics by vegetarian subgroups (in mid-pregnancy)
tab_001c <- tbl_summary(
  dat,
  by = VegDiet_3cat_2,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDIm ~ "continuous",
    hPDIm ~ "continuous",
    uPDIm ~ "continuous",
    VegDiet_3cat_1 ~ "categorical",
    # VegDiet_3cat_2 ~ "categorical",
    self.VegDiet_Mat_EAR.p_bin ~ "categorical",
    self.VegDiet_Mat_MID.p_bin ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    marital_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_PRE.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitA.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    vitB6.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    zinc.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDIm,
    hPDIm,
    uPDIm,
    VegDiet_3cat_1,
    # VegDiet_3cat_2,
    self.VegDiet_Mat_EAR.p_bin,
    self.VegDiet_Mat_MID.p_bin,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    marital_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_PRE.p_bin,
    any.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitA.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    vitB6.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    zinc.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    sex_Chi_bin
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Detailed participant characteristics by vegetarian subgroups in mid-pregnancy") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001c

tab_001c %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/001c-ALL-maternal_characteristics_by_vegetarian_3cat_MID.p.docx")
################################################################################
################################################################################
# Table 001d - All maternal characteristics change in vegetarian subgroups from early to mid-pregnancy

## Generate variable for change in vegetarian subgroups
dat$VegDiet_3cat_change <- ifelse(
  dat$VegDiet_3cat_1 == "Non-vegetarian" &
    dat$VegDiet_3cat_2 == "Non-vegetarian",
  "Persistant Non-V",
  ifelse(
    dat$VegDiet_3cat_1 == "Non-vegetarian" &
      dat$VegDiet_3cat_2 == "Pesco-vegetarian",
    "Non-V -> Pesco-V",
    ifelse(
      dat$VegDiet_3cat_1 == "Non-vegetarian" &
        dat$VegDiet_3cat_2 == "Full vegetarian",
      "Non-V -> Full V",
      ifelse(
        dat$VegDiet_3cat_1 == "Pesco-vegetarian" &
          dat$VegDiet_3cat_2 == "Non-vegetarian",
        "Pesco-V -> Non-V",
        ifelse(
          dat$VegDiet_3cat_1 == "Pesco-vegetarian" &
            dat$VegDiet_3cat_2 == "Pesco-vegetarian",
          "Persistant Pesco-V",
          ifelse(
            dat$VegDiet_3cat_1 == "Pesco-vegetarian" &
              dat$VegDiet_3cat_2 == "Full vegetarian",
            "Pesco-V -> Full V",
            ifelse(
              dat$VegDiet_3cat_1 == "Full vegetarian" &
                dat$VegDiet_3cat_2 == "Non-vegetarian",
              "Full V -> Non-V",
              ifelse(
                dat$VegDiet_3cat_1 == "Full vegetarian" &
                  dat$VegDiet_3cat_2 == "Pesco-vegetarian",
                "Full V -> Pesco-V",
                ifelse(
                  dat$VegDiet_3cat_1 == "Full vegetarian" &
                    dat$VegDiet_3cat_2 == "Full vegetarian",
                  "Persistant Full V",
                  NA
                )
              )
            )
          )
        )
      )
    )
  )
)

dat$VegDiet_3cat_change <- factor(
  dat$VegDiet_3cat_change,
  levels = c(
    "Persistant Non-V",
    "Non-V -> Pesco-V",
    "Non-V -> Full V",
    "Pesco-V -> Non-V",
    "Persistant Pesco-V",
    "Pesco-V -> Full V",
    "Full V -> Non-V",
    "Full V -> Pesco-V",
    "Persistant Full V"
  )
)

table(dat$VegDiet_3cat_change, useNA = "always")

tab_001d <- tbl_summary(
  dat,
  by = VegDiet_3cat_change,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDIm ~ "continuous",
    hPDIm ~ "continuous",
    uPDIm ~ "continuous",
    VegDiet_3cat_1 ~ "categorical",
    VegDiet_3cat_2 ~ "categorical",
    self.VegDiet_Mat_EAR.p_bin ~ "categorical",
    self.VegDiet_Mat_MID.p_bin ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    marital_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_PRE.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitA.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    vitB6.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    zinc.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDIm,
    hPDIm,
    uPDIm,
    VegDiet_3cat_1,
    VegDiet_3cat_2,
    self.VegDiet_Mat_EAR.p_bin,
    self.VegDiet_Mat_MID.p_bin,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    marital_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_PRE.p_bin,
    any.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitA.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    vitB6.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    zinc.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    sex_Chi_bin
  )
) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption(
    "Detailed participant characteristics by change in vegetarian subgroups from early to mid-pregnancy"
  ) %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001d

tab_001d %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/001d-ALL-maternal_characteristics_by_vegetarian_3cat_CHANGE_EAR.p_MID.p.docx")
################################################################################

# Table 002a - All maternal characteristics by PDI quintiles
tab_002a <- tbl_summary(
  dat,
  by = PDI_5Q,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDI ~ "continuous",
    hPDI ~ "continuous",
    uPDI ~ "continuous",
    VegDiet_subgroup ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    marital_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_PRE.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitA.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    vitB6.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    zinc.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDI,
    hPDI,
    uPDI,
    VegDiet_subgroup,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    marital_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_PRE.p_bin,
    any.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitA.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    vitB6.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    zinc.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    sex_Chi_bin
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Detailed participant characteristics by PDI quintiles") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_002a

tab_002a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/002-ALL-maternal_characteristics_by_PDI.docx")

################################################################################
################################################################################
################################################################################

# Table 002b - All maternal characteristics by hPDI quintiles
tab_002b <- tbl_summary(
  dat,
  by = hPDI_5Q,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDI ~ "continuous",
    hPDI ~ "continuous",
    uPDI ~ "continuous",
    VegDiet_subgroup ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    marital_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_PRE.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitA.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    vitB6.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    zinc.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDI,
    hPDI,
    uPDI,
    VegDiet_subgroup,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    marital_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_PRE.p_bin,
    any.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitA.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    vitB6.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    zinc.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    sex_Chi_bin
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Detailed participant characteristics by hPDI quintiles") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_002b

tab_002b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/002-ALL-maternal_characteristics_by_hPDI.docx")

################################################################################
################################################################################
################################################################################

# Table 002c - All maternal characteristics by uPDI quintiles
tab_002c <- tbl_summary(
  dat,
  by = uPDI_5Q,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDI ~ "continuous",
    hPDI ~ "continuous",
    uPDI ~ "continuous",
    VegDiet_subgroup ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    marital_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_PRE.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitA.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    vitB6.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    zinc.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDI,
    hPDI,
    uPDI,
    VegDiet_subgroup,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    marital_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_PRE.p_bin,
    any.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitA.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    vitB6.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    zinc.supp_Mat_EAR.p_bin,
    energy_Mat_DUR.p_con,
    sex_Chi_bin
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Maternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Detailed participant characteristics by uPDI quintiles") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_002c

tab_002c %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/002-ALL-maternal_characteristics_by_uPDI.docx")

################################################################################

# Table 003 - Perinatal outcomes by vegetarian subgroups (during pregnancy)
tab_003 <- tbl_summary(
  dat,
  by = VegDiet_3cat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    hdp_subsamp ~ "categorical",
    gh_subsamp ~ "categorical",
    pe_subsamp ~ "categorical",
    gdm_subsamp ~ "categorical",
    anaemia_preg_all ~ "categorical",
    depr_subsamp ~ "categorical",
    induction ~ "categorical",
    cs ~ "categorical",
    el_cs ~ "categorical",
    em_cs ~ "categorical",
    ga_subsamp ~ "continuous",
    pretb_subsamp ~ "categorical",
    vpretb_subsamp ~ "categorical",
    posttb_subsamp ~ "categorical",
    zbw_subsamp ~ "continuous",
    sga ~ "categorical",
    lga ~ "categorical",
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    bf_dur_4c ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    hdp_subsamp,
    gh_subsamp,
    pe_subsamp,
    gdm_subsamp,
    anaemia_preg_all,
    depr_subsamp,
    induction,
    cs,
    el_cs,
    em_cs,
    ga_subsamp,
    pretb_subsamp,
    vpretb_subsamp,
    posttb_subsamp,
    zbw_subsamp,
    sga,
    lga,
    lbw_subsamp,
    hbw_subsamp,
    bf_dur_4c
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Perinatal outcome**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Perinatal outcomes by vegetarian subgroups during pregnancy") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003

tab_003 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/Viva/003-perinatal_outcomes_by_vegetarian_3cat.docx")

################################################################################
################################################################################

# Table 003a - Perinatal outcomes by vegetarian subgroups (4 categories) during pregnancy
tab_003a <- tbl_summary(
  dat,
  by = VegDiet_subgroup,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    hdp_subsamp ~ "categorical",
    gh_subsamp ~ "categorical",
    pe_subsamp ~ "categorical",
    gdm_subsamp ~ "categorical",
    anaemia_preg_all ~ "categorical",
    depr_subsamp ~ "categorical",
    induction ~ "categorical",
    cs ~ "categorical",
    el_cs ~ "categorical",
    em_cs ~ "categorical",
    ga_subsamp ~ "continuous",
    pretb_subsamp ~ "categorical",
    vpretb_subsamp ~ "categorical",
    posttb_subsamp ~ "categorical",
    zbw_subsamp ~ "continuous",
    sga ~ "categorical",
    lga ~ "categorical",
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    bf_dur_4c ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    hdp_subsamp,
    gh_subsamp,
    pe_subsamp,
    gdm_subsamp,
    anaemia_preg_all,
    depr_subsamp,
    induction,
    cs,
    el_cs,
    em_cs,
    ga_subsamp,
    pretb_subsamp,
    vpretb_subsamp,
    posttb_subsamp,
    zbw_subsamp,
    sga,
    lga,
    lbw_subsamp,
    hbw_subsamp,
    bf_dur_4c
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Perinatal outcome**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Perinatal outcomes by vegetarian subgroups (4 categories) during pregnancy") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003a

tab_003a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/003a-perinatal_outcomes_by_vegetarian_subgroup.docx")

################################################################################
################################################################################
# Table 003b - Perinatal outcomes by vegetarian subgroups (in early pregnancy)
tab_003b <- tbl_summary(
  dat,
  by = VegDiet_3cat_1,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    hdp_subsamp ~ "categorical",
    gh_subsamp ~ "categorical",
    pe_subsamp ~ "categorical",
    gdm_subsamp ~ "categorical",
    anaemia_preg_all ~ "categorical",
    depr_subsamp ~ "categorical",
    induction ~ "categorical",
    cs ~ "categorical",
    el_cs ~ "categorical",
    em_cs ~ "categorical",
    ga_subsamp ~ "continuous",
    pretb_subsamp ~ "categorical",
    vpretb_subsamp ~ "categorical",
    posttb_subsamp ~ "categorical",
    zbw_subsamp ~ "continuous",
    sga ~ "categorical",
    lga ~ "categorical",
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    bf_dur_4c ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    hdp_subsamp,
    gh_subsamp,
    pe_subsamp,
    gdm_subsamp,
    anaemia_preg_all,
    depr_subsamp,
    induction,
    cs,
    el_cs,
    em_cs,
    ga_subsamp,
    pretb_subsamp,
    vpretb_subsamp,
    posttb_subsamp,
    zbw_subsamp,
    sga,
    lga,
    lbw_subsamp,
    hbw_subsamp,
    bf_dur_4c
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Perinatal outcome**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Perinatal outcomes by vegetarian subgroups in early pregnancy") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003b

tab_003b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/003b-perinatal_outcomes_by_vegetarian_3cat_EAR.p.docx")
################################################################################
################################################################################
# Table 003c - Perinatal outcomes by vegetarian subgroups (in mid-pregnancy)
tab_003c <- tbl_summary(
  dat,
  by = VegDiet_3cat_2,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    hdp_subsamp ~ "categorical",
    gh_subsamp ~ "categorical",
    pe_subsamp ~ "categorical",
    gdm_subsamp ~ "categorical",
    anaemia_preg_all ~ "categorical",
    depr_subsamp ~ "categorical",
    induction ~ "categorical",
    cs ~ "categorical",
    el_cs ~ "categorical",
    em_cs ~ "categorical",
    ga_subsamp ~ "continuous",
    pretb_subsamp ~ "categorical",
    vpretb_subsamp ~ "categorical",
    posttb_subsamp ~ "categorical",
    zbw_subsamp ~ "continuous",
    sga ~ "categorical",
    lga ~ "categorical",
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    bf_dur_4c ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    hdp_subsamp,
    gh_subsamp,
    pe_subsamp,
    gdm_subsamp,
    anaemia_preg_all,
    depr_subsamp,
    induction,
    cs,
    el_cs,
    em_cs,
    ga_subsamp,
    pretb_subsamp,
    vpretb_subsamp,
    posttb_subsamp,
    zbw_subsamp,
    sga,
    lga,
    lbw_subsamp,
    hbw_subsamp,
    bf_dur_4c
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Perinatal outcome**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Perinatal outcomes by vegetarian subgroups in mid-pregnancy") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003c

tab_003c %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/003c-perinatal_outcomes_by_vegetarian_3cat_MID.p.docx")
################################################################################
################################################################################
# Table 003d - Perinatal outcomes by change in vegetarian subgroups from early to mid-pregnancy
tab_003d <- tbl_summary(
  dat,
  by = VegDiet_3cat_change,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    hdp_subsamp ~ "categorical",
    gh_subsamp ~ "categorical",
    pe_subsamp ~ "categorical",
    gdm_subsamp ~ "categorical",
    anaemia_preg_all ~ "categorical",
    depr_subsamp ~ "categorical",
    induction ~ "categorical",
    cs ~ "categorical",
    el_cs ~ "categorical",
    em_cs ~ "categorical",
    ga_subsamp ~ "continuous",
    pretb_subsamp ~ "categorical",
    vpretb_subsamp ~ "categorical",
    posttb_subsamp ~ "categorical",
    zbw_subsamp ~ "continuous",
    sga ~ "categorical",
    lga ~ "categorical",
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    bf_dur_4c ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    hdp_subsamp,
    gh_subsamp,
    pe_subsamp,
    gdm_subsamp,
    anaemia_preg_all,
    depr_subsamp,
    induction,
    cs,
    el_cs,
    em_cs,
    ga_subsamp,
    pretb_subsamp,
    vpretb_subsamp,
    posttb_subsamp,
    zbw_subsamp,
    sga,
    lga,
    lbw_subsamp,
    hbw_subsamp,
    bf_dur_4c
  )
) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Perinatal outcome**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Perinatal outcomes by change in vegetarian subgroups from early to mid-pregnancy") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003d

tab_003d %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/003d-perinatal_outcomes_by_vegetarian_3cat_CHANGE_EAR.p_MID.p.docx")
################################################################################
################################################################################
# Table 000 - Available outcome sample sizes for MATERNAL analyses
tab_000 <- tbl_summary(
  dat,
  by = VegDiet_3cat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    hdp_subsamp ~ "dichotomous",
    gh_subsamp ~ "dichotomous",
    pe_subsamp ~ "dichotomous",
    gdm_subsamp ~ "dichotomous",
    anaemia_preg_all ~ "dichotomous",
    depr_subsamp ~ "dichotomous",
    induction ~ "dichotomous",
    cs ~ "dichotomous",
    el_cs ~ "dichotomous",
    em_cs ~ "dichotomous",
    pretb_subsamp ~ "dichotomous",
    vpretb_subsamp ~ "dichotomous",
    posttb_subsamp ~ "dichotomous",
    ga_subsamp ~ "continuous",
    sga ~ "dichotomous",
    lga ~ "dichotomous",
    lbw_subsamp ~ "dichotomous",
    hbw_subsamp ~ "dichotomous",
    zbw_subsamp ~ "continuous",
    bf_dur_4c ~ "categorical"
  ),
  missing = "no",
  include = c(
    hdp_subsamp,
    gh_subsamp,
    pe_subsamp,
    gdm_subsamp,
    anaemia_preg_all,
    depr_subsamp,
    induction,
    cs,
    el_cs,
    em_cs,
    pretb_subsamp,
    vpretb_subsamp,
    posttb_subsamp,
    ga_subsamp,
    sga,
    lga,
    lbw_subsamp,
    hbw_subsamp,
    zbw_subsamp,
    bf_dur_4c
  )
) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Perinatal outcome**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Perinatal outcomes by vegetarian subgroups during pregnancy") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_000

tab_000 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/Viva/000-perinatal_outcomes_sample.size_Mat.docx")
################################################################################

# Table 004a - Perinatal outcomes by PDI quintiles
tab_004a <- tbl_summary(
  dat,
  by = PDI_5Q,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    hdp_subsamp ~ "categorical",
    gh_subsamp ~ "categorical",
    pe_subsamp ~ "categorical",
    gdm_subsamp ~ "categorical",
    anaemia_preg_all ~ "categorical",
    depr_subsamp ~ "categorical",
    induction ~ "categorical",
    cs ~ "categorical",
    el_cs ~ "categorical",
    em_cs ~ "categorical",
    ga_subsamp ~ "continuous",
    pretb_subsamp ~ "categorical",
    vpretb_subsamp ~ "categorical",
    posttb_subsamp ~ "categorical",
    zbw_subsamp ~ "continuous",
    sga ~ "categorical",
    lga ~ "categorical",
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    bf_dur_4c ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    hdp_subsamp,
    gh_subsamp,
    pe_subsamp,
    gdm_subsamp,
    anaemia_preg_all,
    depr_subsamp,
    induction,
    cs,
    el_cs,
    em_cs,
    ga_subsamp,
    pretb_subsamp,
    vpretb_subsamp,
    posttb_subsamp,
    zbw_subsamp,
    sga,
    lga,
    lbw_subsamp,
    hbw_subsamp,
    bf_dur_4c
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Perinatal outcome**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Perinatal outcomes by PDI quintiles") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_004a

tab_004a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/004-perinatal_outcomes_by_PDI.docx")

################################################################################

# Table 004b - Perinatal outcomes by hPDI quintiles
tab_004b <- tbl_summary(
  dat,
  by = hPDI_5Q,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    hdp_subsamp ~ "categorical",
    gh_subsamp ~ "categorical",
    pe_subsamp ~ "categorical",
    gdm_subsamp ~ "categorical",
    anaemia_preg_all ~ "categorical",
    depr_subsamp ~ "categorical",
    induction ~ "categorical",
    cs ~ "categorical",
    el_cs ~ "categorical",
    em_cs ~ "categorical",
    ga_subsamp ~ "continuous",
    pretb_subsamp ~ "categorical",
    vpretb_subsamp ~ "categorical",
    posttb_subsamp ~ "categorical",
    zbw_subsamp ~ "continuous",
    sga ~ "categorical",
    lga ~ "categorical",
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    bf_dur_4c ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    hdp_subsamp,
    gh_subsamp,
    pe_subsamp,
    gdm_subsamp,
    anaemia_preg_all,
    depr_subsamp,
    induction,
    cs,
    el_cs,
    em_cs,
    ga_subsamp,
    pretb_subsamp,
    vpretb_subsamp,
    posttb_subsamp,
    zbw_subsamp,
    sga,
    lga,
    lbw_subsamp,
    hbw_subsamp,
    bf_dur_4c
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Perinatal outcome**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Perinatal outcomes by hPDI quintiles") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_004b

tab_004b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/004-perinatal_outcomes_by_hPDI.docx")

################################################################################

# Table 004c - Perinatal outcomes by uPDI quintiles
tab_004c <- tbl_summary(
  dat,
  by = uPDI_5Q,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    hdp_subsamp ~ "categorical",
    gh_subsamp ~ "categorical",
    pe_subsamp ~ "categorical",
    gdm_subsamp ~ "categorical",
    anaemia_preg_all ~ "categorical",
    depr_subsamp ~ "categorical",
    induction ~ "categorical",
    cs ~ "categorical",
    el_cs ~ "categorical",
    em_cs ~ "categorical",
    ga_subsamp ~ "continuous",
    pretb_subsamp ~ "categorical",
    vpretb_subsamp ~ "categorical",
    posttb_subsamp ~ "categorical",
    zbw_subsamp ~ "continuous",
    sga ~ "categorical",
    lga ~ "categorical",
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    bf_dur_4c ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    hdp_subsamp,
    gh_subsamp,
    pe_subsamp,
    gdm_subsamp,
    anaemia_preg_all,
    depr_subsamp,
    induction,
    cs,
    el_cs,
    em_cs,
    ga_subsamp,
    pretb_subsamp,
    vpretb_subsamp,
    posttb_subsamp,
    zbw_subsamp,
    sga,
    lga,
    lbw_subsamp,
    hbw_subsamp,
    bf_dur_4c
  )
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Perinatal outcome**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Perinatal outcomes by uPDI quintiles") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_004c

tab_004c %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/004-perinatal_outcomes_by_uPDI.docx")

################################################################################

# 005 - Intakes of different food groups by vegetarian subgroups
tab_005 <- tbl_summary(
  dat,
  by = VegDiet_3cat,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  type = list(
    wholegrain ~ "continuous",
    fruit ~ "continuous",
    vegetable ~ "continuous",
    nut ~ "continuous",
    legume ~ "continuous",
    vegetableoil ~ "continuous",
    teacoffee ~ "continuous",
    fruitjuice ~ "continuous",
    refinedgrain ~ "continuous",
    potato ~ "continuous",
    sugarbeverage ~ "continuous",
    sweetdessert ~ "continuous",
    animalfat ~ "continuous",
    dairy ~ "continuous",
    egg ~ "continuous",
    fishseafood ~ "continuous",
    meat ~ "continuous",
    misc.animal ~ "continuous"
  ),
  missing = "no",
  include = c(
    wholegrain,
    fruit,
    vegetable,
    nut,
    legume,
    vegetableoil,
    teacoffee,
    fruitjuice,
    refinedgrain,
    potato,
    sugarbeverage,
    sweetdessert,
    animalfat,
    dairy,
    egg,
    fishseafood,
    meat,
    misc.animal
  )
) %>%
  add_p(
    test = all_continuous() ~ "aov",
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Food group intake**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as mean (standard deviation).") %>%
  modify_caption("Intake of different food groups by vegetarian subgroups (3 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_005

tab_005 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/Viva/005-food_intake_by_vegetarian_3cat.docx")

################################################################################
################################################################################

# 005a - Intakes of different food groups by vegetarian subgroups (4 categories)
tab_005a <- tbl_summary(
  dat,
  by = VegDiet_subgroup,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  type = list(
    wholegrain ~ "continuous",
    fruit ~ "continuous",
    vegetable ~ "continuous",
    nut ~ "continuous",
    legume ~ "continuous",
    vegetableoil ~ "continuous",
    teacoffee ~ "continuous",
    fruitjuice ~ "continuous",
    refinedgrain ~ "continuous",
    potato ~ "continuous",
    sugarbeverage ~ "continuous",
    sweetdessert ~ "continuous",
    animalfat ~ "continuous",
    dairy ~ "continuous",
    egg ~ "continuous",
    fishseafood ~ "continuous",
    meat ~ "continuous",
    misc.animal ~ "continuous"
  ),
  missing = "no",
  include = c(
    wholegrain,
    fruit,
    vegetable,
    nut,
    legume,
    vegetableoil,
    teacoffee,
    fruitjuice,
    refinedgrain,
    potato,
    sugarbeverage,
    sweetdessert,
    animalfat,
    dairy,
    egg,
    fishseafood,
    meat,
    misc.animal
  )
) %>%
  add_p(
    test = all_continuous() ~ "aov",
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Food group intake**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as mean (standard deviation).") %>%
  modify_caption("Intake of different food groups by vegetarian subgroups (4 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_005a

tab_005a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/Viva/005a-food_intake_by_vegetarian_subgroup.docx")

################################################################################

# Sanity check

## Correlation between maternal education and household income
dat$edu_Mat_3cat_num <- as.numeric(dat$edu_Mat_3cat)
dat$income_Fam_cat_num <- as.numeric(dat$income_Fam_3cat)

corr_matrix <- rcorr(as.matrix(dat[, c("edu_Mat_3cat_num", "income_Fam_cat_num")]), type = "spearman")

correlation <- corr_matrix$r
p_values <- corr_matrix$P

print(correlation)
print(p_values)

################################################################################
