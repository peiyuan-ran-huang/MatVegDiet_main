################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa       #
################################################################################

# Last edited date: 13-Jun-2025
# This script is to produce main descriptive tables in MoBa.

################################################################################

# Clear environment
rm(list = ls())

# Collect information about the current R session
sessionInfo()

# Load packages
pacman::p_load(
  tidyverse,
  openxlsx,
  haven,
  expss,
  gtsummary,
  kableExtra,
  flextable,
  readr,
  magrittr,
  ggplot2,
  hrbrthemes,
  Hmisc,
  corrplot
)

# Set working directory
setwd("N:/durable/projects/Ran_MoBa_var")

################################################################################

# Load data
dat <- readRDS("dat_exp_cov_out_pat.rds")
head(dat)
dim(dat)  # 73868  XXX

################################################################################
# Check maximum N (defined as those with data on at least one outcome)
my_out <- c(
  "hdp_subsamp",
  "gh_subsamp",
  "pe_subsamp",
  "gdm_subsamp",
  "anaemia_preg_all",
  "depr_subsamp",
  "rup_memb",
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
  "apgar1",
  "lowapgar1",
  "apgar5",
  "lowapgar5",
  "nicu",
  "bf_dur_4c"
)

sum(apply(dat[, my_out], 1, function(x)
  any(!is.na(x))))  # 73868
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
                                                         "results/01-KEY-maternal_characteristics_for_vegetarian_3cat.docx")

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
                                                         "results/02-KEY-maternal_characteristics_for_PDIs.docx")

#------------------------------------------------------------------------------#
#                             Supplementary Tables                             #----
#------------------------------------------------------------------------------#

# Table 001 - All maternal characteristics by vegetarian subgroups
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
    self.VegDiet_Mat_DUR.p_bin ~ "categorical",
    self.VegDiet_Mat_DUR.p_3cat ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    student_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    vitB12.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    iodine.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    omega3FA.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDIm,
    hPDIm,
    uPDIm,
    self.VegDiet_Mat_DUR.p_bin,
    self.VegDiet_Mat_DUR.p_3cat,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    student_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    vitB12.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    iodine.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    omega3FA.supp_Mat_EAR.p_bin,
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
  modify_caption("Detailed participant characteristics by vegetarian subgroups") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001

tab_001 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/001-ALL-maternal_characteristics_by_vegetarian_3cat.docx")

################################################################################
################################################################################
# Table 001a - All maternal characteristics by vegetarian subgroups - !!! 4 categories !!!
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
    self.VegDiet_Mat_DUR.p_bin ~ "categorical",
    self.VegDiet_Mat_DUR.p_3cat ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    student_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    vitB12.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    iodine.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    omega3FA.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDIm,
    hPDIm,
    uPDIm,
    self.VegDiet_Mat_DUR.p_bin,
    self.VegDiet_Mat_DUR.p_3cat,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    student_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    vitB12.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    iodine.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    omega3FA.supp_Mat_EAR.p_bin,
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
  modify_caption("Detailed participant characteristics by vegetarian subgroups (4 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001a

tab_001a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/001a-ALL-maternal_characteristics_by_vegetarian_subgroup.docx")
################################################################################
################################################################################
## Table 001b - All maternal characteristics by vegetarian subgroups - !!! Defined by BOTH FFQ AND self-definition !!!
dat <-
  dat %>% mutate(
    VegDiet_3cat_FFQ.self = case_when(
      VegDiet_3cat == "Non-vegetarian" &
        self.VegDiet_Mat_DUR.p_3cat == "Non-vegetarian" ~ 0,
      VegDiet_3cat == "Pesco-vegetarian" &
        self.VegDiet_Mat_DUR.p_3cat == "Pesco-vegetarian" ~ 1,
      VegDiet_3cat == "Full vegetarian" &
        self.VegDiet_Mat_DUR.p_3cat == "Full vegetarian" ~ 2,
      TRUE ~ NA_real_
    )
  )
val_lab(dat$VegDiet_3cat_FFQ.self) = c(
  "Non-vegetarian" = 0,
  "Pesco-vegetarian" = 1,
  "Full vegetarian" = 2
)
dat$VegDiet_3cat_FFQ.self <- as.factor(dat$VegDiet_3cat_FFQ.self)
var_lab(dat$VegDiet_3cat_FFQ.self) = "Diet-based + self-defined vegetarianism (3 categories) during pregnancy"

tab_001b <- tbl_summary(
  dat,
  by = VegDiet_3cat_FFQ.self,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDIm ~ "continuous",
    hPDIm ~ "continuous",
    uPDIm ~ "continuous",
    VegDiet_3cat ~ "categorical",
    self.VegDiet_Mat_DUR.p_bin ~ "categorical",
    self.VegDiet_Mat_DUR.p_3cat ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    student_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    vitB12.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    iodine.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    omega3FA.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDIm,
    hPDIm,
    uPDIm,
    VegDiet_3cat,
    self.VegDiet_Mat_DUR.p_bin,
    self.VegDiet_Mat_DUR.p_3cat,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    student_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    vitB12.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    iodine.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    omega3FA.supp_Mat_EAR.p_bin,
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
    "Detailed participant characteristics by diet-based & self-defined vegetarian subgroups"
  ) %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001b

tab_001b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/001b-ALL-maternal_characteristics_by_vegetarian_3cat_FFQ.self.docx")
################################################################################
################################################################################
# Table 001c - All maternal characteristics by vegetarian subgroups - !!! In those with paternal data !!!
tab_001c <- tbl_summary(
  dat[!is.na(dat$VegDiet_bin_Pat), ],
  by = VegDiet_3cat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    PDIm ~ "continuous",
    hPDIm ~ "continuous",
    uPDIm ~ "continuous",
    self.VegDiet_Mat_DUR.p_bin ~ "categorical",
    self.VegDiet_Mat_DUR.p_3cat ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    student_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    vitB12.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    iodine.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    omega3FA.supp_Mat_EAR.p_bin ~ "categorical",
    energy_Mat_DUR.p_con ~ "continuous",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    PDIm,
    hPDIm,
    uPDIm,
    self.VegDiet_Mat_DUR.p_bin,
    self.VegDiet_Mat_DUR.p_3cat,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    income_Fam_3cat,
    student_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    vitB12.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    iodine.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    omega3FA.supp_Mat_EAR.p_bin,
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
    "Detailed participant characteristics by vegetarian subgroups (3 categories) in couple subsamples"
  ) %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001c

tab_001c %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/001c-ALL-maternal_characteristics_by_vegetarian_3cat_subsamp.docx")

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
    student_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    vitB12.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    iodine.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    omega3FA.supp_Mat_EAR.p_bin ~ "categorical",
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
    student_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    vitB12.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    iodine.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    omega3FA.supp_Mat_EAR.p_bin,
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
# There was an error in 'add_p()/add_difference()' for variable 'VegDiet_subgroup', p-value omitted:
# Error in stats::fisher.test(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, : FEXACT error 501.
# The hash table key cannot be computed because the largest key
# is larger than the largest representable int.
# The algorithm cannot proceed.
# Reduce the workspace, consider using 'simulate.p.value=TRUE' or another algorithm.

tab_002a

tab_002a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/002-ALL-maternal_characteristics_by_PDI.docx")

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
    student_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    vitB12.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    iodine.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    omega3FA.supp_Mat_EAR.p_bin ~ "categorical",
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
    student_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    vitB12.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    iodine.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    omega3FA.supp_Mat_EAR.p_bin,
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
# There was an error in 'add_p()/add_difference()' for variable 'VegDiet_subgroup', p-value omitted:
# Error in stats::fisher.test(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, : FEXACT error 501.
# The hash table key cannot be computed because the largest key
# is larger than the largest representable int.
# The algorithm cannot proceed.
# Reduce the workspace, consider using 'simulate.p.value=TRUE' or another algorithm.

tab_002b

tab_002b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/002-ALL-maternal_characteristics_by_hPDI.docx")

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
    student_Mat_bin ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    phys.act_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    vitB12.supp_Mat_EAR.p_bin ~ "categorical",
    calcium.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    iodine.supp_Mat_EAR.p_bin ~ "categorical",
    folate.supp_Mat_EAR.p_bin ~ "categorical",
    omega3FA.supp_Mat_EAR.p_bin ~ "categorical",
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
    student_Mat_bin,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    phys.act_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    vitB12.supp_Mat_EAR.p_bin,
    calcium.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    iodine.supp_Mat_EAR.p_bin,
    folate.supp_Mat_EAR.p_bin,
    omega3FA.supp_Mat_EAR.p_bin,
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
# There was an error in 'add_p()/add_difference()' for variable 'VegDiet_subgroup', p-value omitted:
# Error in stats::fisher.test(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, : FEXACT error 501.
# The hash table key cannot be computed because the largest key
# is larger than the largest representable int.
# The algorithm cannot proceed.
# Reduce the workspace, consider using 'simulate.p.value=TRUE' or another algorithm.

tab_002c

tab_002c %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/002-ALL-maternal_characteristics_by_uPDI.docx")

################################################################################

# Table 003 - Perinatal outcomes by vegetarian subgroups (3 categories)
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
    rup_memb ~ "categorical",
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
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
    nicu ~ "categorical",
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
    rup_memb,
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
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
    nicu,
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
  modify_caption("Perinatal outcomes by vegetarian subgroups (3 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003

tab_003 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/003-perinatal_outcomes_by_vegetarian_3cat.docx")

################################################################################
################################################################################
# Table 003a - Perinatal outcomes by vegetarian subgroups - !!! 4 categories !!!
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
    rup_memb ~ "categorical",
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
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
    nicu ~ "categorical",
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
    rup_memb,
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
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
    nicu,
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
  modify_caption("Perinatal outcomes by vegetarian subgroups (4 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003a

tab_003a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/003a-perinatal_outcomes_by_vegetarian_subgroup.docx")
################################################################################
################################################################################
# Table 003b - Perinatal outcomes by vegetarian subgroups - !!! Defined by BOTH FFQ AND self-definition !!!
tab_003b <- tbl_summary(
  dat,
  by = VegDiet_3cat_FFQ.self,
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
    rup_memb ~ "categorical",
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
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
    nicu ~ "categorical",
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
    rup_memb,
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
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
    nicu,
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
  modify_caption("Perinatal outcomes by diet-based & self-defined vegetarian subgroups") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003b

tab_003b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/003b-perinatal_outcomes_by_vegetarian_3cat_FFQ.self.docx")
################################################################################
################################################################################
# Table 003c - Perinatal outcomes by vegetarian subgroups - !!! In those with paternal data !!!
tab_003c <- tbl_summary(
  dat[!is.na(dat$VegDiet_bin_Pat), ],
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
    rup_memb ~ "categorical",
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
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
    nicu ~ "categorical",
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
    rup_memb,
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
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
    nicu,
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
  modify_caption("Perinatal outcomes by vegetarian subgroups (3 categories) in couple subsamples") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003c

tab_003c %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/003c-perinatal_outcomes_by_vegetarian_3cat_subsamp.docx")
################################################################################
# Table 000a - Available outcome sample sizes for MATERNAL analyses
tab_000a <- tbl_summary(
  dat,
  by = VegDiet_3cat,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  type = list(
    hdp_subsamp ~ "dichotomous",
    gh_subsamp ~ "dichotomous",
    pe_subsamp ~ "dichotomous",
    gdm_subsamp ~ "dichotomous",
    anaemia_preg_all ~ "dichotomous",
    depr_subsamp ~ "dichotomous",
    rup_memb ~ "dichotomous",
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
    lowapgar1 ~ "dichotomous",
    apgar1 ~ "continuous",
    lowapgar5 ~ "dichotomous",
    apgar5 ~ "continuous",
    nicu ~ "dichotomous",
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
    rup_memb,
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
    lowapgar1,
    apgar1,
    lowapgar5,
    apgar5,
    nicu,
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
  modify_caption("Perinatal outcomes by vegetarian subgroups (3 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_000a

tab_000a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/000a-MAT-perinatal_outcomes_sample.size_Mat.docx")
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
    rup_memb ~ "categorical",
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
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
    nicu ~ "categorical",
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
    rup_memb,
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
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
    nicu,
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
                                                           "results/004-perinatal_outcomes_by_PDI.docx")

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
    rup_memb ~ "categorical",
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
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
    nicu ~ "categorical",
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
    rup_memb,
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
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
    nicu,
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
                                                           "results/004-perinatal_outcomes_by_hPDI.docx")

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
    rup_memb ~ "categorical",
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
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
    nicu ~ "categorical",
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
    rup_memb,
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
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
    nicu,
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
                                                           "results/004-perinatal_outcomes_by_uPDI.docx")

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
  modify_header(label ~ "**Food group intake (g/day)**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as mean (standard deviation).") %>%
  modify_caption("Intake of different food groups by vegetarian subgroups (3 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_005

tab_005 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/005-food_intake_by_vegetarian_3cat.docx")

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
  modify_header(label ~ "**Food group intake (g/day)**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as mean (standard deviation).") %>%
  modify_caption("Intake of different food groups by vegetarian subgroups (4 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_005a

tab_005a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/005a-food_intake_by_vegetarian_subgroup.docx")

################################################################################

# Table 006 - Nutrient intakes by vegetarian subgroups
nutrient_labels <-
  read.xlsx("nutrient_varlab.xlsx",
            sheet = "Sheet1")
nutrient_labels
str(nutrient_labels)  # 34 nutrients

tab_006 <- tbl_summary(
  dat[, c("VegDiet_3cat", nutrient_labels$varname)],
  by = VegDiet_3cat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  missing = "no"
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Nutrient intake**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as mean (standard deviation).") %>%
  modify_caption("Nutrient intake by vegetarian subgroups") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_006

tab_006 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/006-nutrients_by_vegetarian_3cat.docx")

################################################################################
################################################################################

# Table 006a - Nutrient intakes by vegetarian subgroups (4 categories)
nutrient_labels <-
  read.xlsx("nutrient_varlab.xlsx",
            sheet = "Sheet1")
nutrient_labels
str(nutrient_labels)  # 34 nutrients

tab_006a <- tbl_summary(
  dat[, c("VegDiet_subgroup", nutrient_labels$varname)],
  by = VegDiet_subgroup,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  missing = "no"
) %>%
  add_p(
    test = list(all_continuous() ~ "aov"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Nutrient intake**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as mean (standard deviation).") %>%
  modify_caption("Nutrient intake by vegetarian subgroups (4 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_006a

tab_006a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/006a-nutrients_by_vegetarian_subgroup.docx")

################################################################################

# Table 007a - Diet-based vegetarianism by self-defined vegetarianism
tab_007a <- tbl_summary(
  dat,
  by = self.VegDiet_Mat_DUR.p_4cat,
  type = list(VegDiet_3cat ~ "categorical"),
  missing = "no",
  include = c(VegDiet_3cat)
) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "** **") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Self-defined vegetarianism (4 categories)**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as frequency (percentage).") %>%
  modify_caption("Agreement between diet-based and self-defined vegetarianism") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_007a

tab_007a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/007a-diet-based_vs_self-defined_vegetarianism.docx")

################################################################################
################################################################################
################################################################################

# Table 007b - Diet-based vegetarianism by self-defined vegetarianism
tab_007b <- tbl_summary(
  dat,
  by = VegDiet_3cat,
  type = list(self.VegDiet_Mat_DUR.p_4cat ~ "categorical"),
  missing = "no",
  include = c(self.VegDiet_Mat_DUR.p_4cat)
) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "** **") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Diet-based vegetarianism (3 categories)**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as frequency (percentage).") %>%
  modify_caption("Agreement between self-defined and diet-based vegetarianism") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_007b

tab_007b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/007b-self-defined_vs_diet-based_vegetarianism.docx")

################################################################################

# Table 008 - Maternal & paternal characteristics by paternal vegetarian subgroups
tab_008 <- tbl_summary(
  dat,
  by = VegDiet_3cat_Pat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    VegDiet_3cat ~ "categorical",
    self.VegDiet_Pat_EAR.p_bin ~ "categorical",
    self.VegDiet_Pat_EAR.p_3cat ~ "categorical",
    age_Pat_con ~ "continuous",
    edu_Pat_3cat ~ "categorical",
    income_Fam_3cat ~ "categorical",
    parity_Pat_bin ~ "categorical",
    BMI_Pat_EAR.p_con ~ "continuous",
    smoking_Pat_EAR.p_bin ~ "categorical",
    alcohol_Pat_EAR.p_bin ~ "categorical",
    any.supp_Pat_EAR.p_bin ~ "categorical",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    VegDiet_3cat,
    self.VegDiet_Pat_EAR.p_bin,
    self.VegDiet_Pat_EAR.p_3cat,
    age_Pat_con,
    edu_Pat_3cat,
    income_Fam_3cat,
    parity_Pat_bin,
    BMI_Pat_EAR.p_con,
    smoking_Pat_EAR.p_bin,
    alcohol_Pat_EAR.p_bin,
    any.supp_Pat_EAR.p_bin,
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
  modify_header(label ~ "**Maternal & paternal characteristic**") %>%
  modify_footnote(
    all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
  ) %>%
  modify_caption("Maternal & paternal characteristics by paternal vegetarian subgroups") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))
# 51886 observations missing `VegDiet_3cat_Pat` have been removed. To include these observations, use `forcats::fct_explicit_na()` on `VegDiet_3cat_Pat` column before passing to `tbl_summary()`.
# There was an error in 'add_p()/add_difference()' for variable 'income_Fam_3cat', p-value omitted:
# Error in stats::fisher.test(structure(c(3L, 1L, 1L, 3L, 3L, 2L, 3L, 3L, : FEXACT error 501.
# The hash table key cannot be computed because the largest key
# is larger than the largest representable int.
# The algorithm cannot proceed.
# Reduce the workspace, consider using 'simulate.p.value=TRUE' or another algorithm.

tab_008

tab_008 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/008-PAT-characteristics_by_paternal_vegetarian_3cat.docx")

################################################################################

# Table 009 - Perinatal outcomes by paternal vegetarian subgroups
tab_009 <- tbl_summary(
  dat,
  by = VegDiet_3cat_Pat,
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
    rup_memb ~ "categorical",
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
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
    nicu ~ "categorical",
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
    rup_memb,
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
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
    nicu,
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
  modify_caption("Perinatal outcomes by paternal vegetarian subgroups (3 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_009

tab_009 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/009-PAT-perinatal_outcomes_by_paternal_vegetarian_3cat.docx")

################################################################################
# Table 000b - Available outcome sample sizes for PATERNAL analyses
tab_000b <- tbl_summary(
  dat,
  by = VegDiet_3cat_Pat,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  type = list(
    hdp_subsamp ~ "dichotomous",
    gh_subsamp ~ "dichotomous",
    pe_subsamp ~ "dichotomous",
    gdm_subsamp ~ "dichotomous",
    anaemia_preg_all ~ "dichotomous",
    depr_subsamp ~ "dichotomous",
    rup_memb ~ "dichotomous",
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
    lowapgar1 ~ "dichotomous",
    apgar1 ~ "continuous",
    lowapgar5 ~ "dichotomous",
    apgar5 ~ "continuous",
    nicu ~ "dichotomous",
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
    rup_memb,
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
    lowapgar1,
    apgar1,
    lowapgar5,
    apgar5,
    nicu,
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
  modify_caption("Perinatal outcomes by paternal vegetarian subgroups (3 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_000b

tab_000b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/000b-PAT-perinatal_outcomes_sample.size_Pat.docx")
################################################################################

# Sanity check

## Correlation between maternal education and household income
dat$edu_Mat_3cat_num <- as.numeric(dat$edu_Mat_3cat)
dat$income_Fam_cat_num <- as.numeric(dat$income_Fam_3cat)

corr_matrix <-
  rcorr(as.matrix(dat[, c("edu_Mat_3cat_num", "income_Fam_cat_num")]), type = "spearman")

correlation <- corr_matrix$r
p_values <- corr_matrix$P

print(correlation)
print(p_values)

################################################################################
