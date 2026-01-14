################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - BiB        #
################################################################################

# Last edited date: 07-May-2025
# This script is to produce main descriptive tables in BiB.

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
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/BiB/dat_exp_cov_out.rds")
head(dat)
dim(dat)  # 3647  XXX

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
  "apgar1",
  "lowapgar1",
  "apgar5",
  "lowapgar5",
  "bf_dur_4c"
)

sum(apply(dat[, my_out], 1, function(x)
  any(!is.na(x))))  # 3647
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
    IMD_Fam_cat ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "no",
  include = c(
    VegDiet_3cat,
    age_Mat_con,
    ethnic_Mat_bin,
    edu_Mat_3cat,
    IMD_Fam_cat,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
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
                                                         "results/BiB/01-KEY-maternal_characteristics_for_vegetarian_3cat.docx")

#------------------------------------------------------------------------------#
#                             Supplementary Tables                             #----
#------------------------------------------------------------------------------#

# Table 001 - All maternal characteristics by vegetarian subgroups in whole sample
tab_001 <- tbl_summary(
  dat,
  by = VegDiet_3cat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    FiveADay_Mat_DUR.p_3cat ~ "categorical",
    age_Mat_con ~ "continuous",
    ethnic_Mat_cat ~ "categorical",
    edu_Mat_3cat ~ "categorical",
    IMD_Fam_cat ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    other.supp_Mat_EAR.p_bin ~ "categorical",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    FiveADay_Mat_DUR.p_3cat,
    age_Mat_con,
    ethnic_Mat_cat,
    edu_Mat_3cat,
    IMD_Fam_cat,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    other.supp_Mat_EAR.p_bin,
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
  modify_caption("Detailed participant characteristics by vegetarian subgroups in whole sample") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001

tab_001 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/BiB/001-ALL-maternal_characteristics_by_vegetarian_3cat.docx")

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
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
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
  modify_caption("Perinatal outcomes by vegetarian subgroups (3 categories) in whole sample") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003

tab_003 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/BiB/003-perinatal_outcomes_by_vegetarian_3cat.docx")

################################################################################
################################################################################
# Table 003a - Available outcome sample sizes for MATERNAL analyses
tab_003a <- tbl_summary(
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
    lowapgar1 ~ "dichotomous",
    apgar1 ~ "continuous",
    lowapgar5 ~ "dichotomous",
    apgar5 ~ "continuous",
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
    lowapgar1,
    apgar1,
    lowapgar5,
    apgar5,
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
  modify_caption("Perinatal outcomes by vegetarian subgroups (3 categories) in whole sample") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003a

tab_003a %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/BiB/003a-perinatal_outcomes_sample.size_Mat.docx")
################################################################################
################################################################################
# Table 003b - Available outcome sample sizes for MATERNAL analyses
tab_003b <- tbl_summary(
  subset(dat, ethnic_Mat_cat %in% c("White British", "Pakistani")),
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
    lowapgar1 ~ "dichotomous",
    apgar1 ~ "continuous",
    lowapgar5 ~ "dichotomous",
    apgar5 ~ "continuous",
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
    lowapgar1,
    apgar1,
    lowapgar5,
    apgar5,
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
  modify_caption(
    "Perinatal outcomes by vegetarian subgroups (3 categories) in cross-context comparison sample"
  ) %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003b

tab_003b %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                           "results/BiB/003b-perinatal_outcomes_sample.size_CCC.docx")
################################################################################

# 005 - Intakes of different food groups by vegetarian subgroups
tab_005 <- tbl_summary(
  dat,
  by = VegDiet_3cat,
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  type = list(
    wholegrain ~ "continuous",
    teacoffee ~ "continuous",
    refinedgrain ~ "continuous",
    potato ~ "continuous",
    sugarbeverage ~ "continuous",
    sweetdessert ~ "continuous",
    fishseafood ~ "continuous",
    meat ~ "continuous",
    misc.animal ~ "continuous"
  ),
  missing = "no",
  include = c(
    wholegrain,
    teacoffee,
    refinedgrain,
    potato,
    sugarbeverage,
    sweetdessert,
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
  modify_header(label ~ "**Food group intake (times/day)**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as mean (standard deviation).") %>%
  modify_caption("Intake of different food groups by vegetarian subgroups (3 categories)") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_005

tab_005 %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                          "results/BiB/005-food_intake_by_vegetarian_3cat.docx")

#------------------------------------------------------------------------------#
#                         For Cross-context Comparison                         #----
#------------------------------------------------------------------------------#

# Table 001_WB - All maternal characteristics by vegetarian subgroups
tab_001_WB <- tbl_summary(
  subset(dat, ethnic_Mat_cat == "White British"),
  by = VegDiet_3cat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    FiveADay_Mat_DUR.p_3cat ~ "categorical",
    age_Mat_con ~ "continuous",
    edu_Mat_3cat ~ "categorical",
    IMD_Fam_cat ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    other.supp_Mat_EAR.p_bin ~ "categorical",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    FiveADay_Mat_DUR.p_3cat,
    age_Mat_con,
    edu_Mat_3cat,
    IMD_Fam_cat,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    other.supp_Mat_EAR.p_bin,
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
  modify_caption("Detailed participant characteristics by vegetarian subgroups in White British") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001_WB

tab_001_WB %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                             "results/BiB/001a_WB-ALL-maternal_characteristics_by_vegetarian_3cat.docx")

################################################################################
################################################################################

# Table 001_SA - All maternal characteristics by vegetarian subgroups
tab_001_SA <- tbl_summary(
  subset(dat, ethnic_Mat_cat == "Pakistani"),
  by = VegDiet_3cat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    FiveADay_Mat_DUR.p_3cat ~ "categorical",
    age_Mat_con ~ "continuous",
    edu_Mat_3cat ~ "categorical",
    IMD_Fam_cat ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    other.supp_Mat_EAR.p_bin ~ "categorical",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    FiveADay_Mat_DUR.p_3cat,
    age_Mat_con,
    edu_Mat_3cat,
    IMD_Fam_cat,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    other.supp_Mat_EAR.p_bin,
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
  modify_caption("Detailed participant characteristics by vegetarian subgroups in South Asians") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001_SA

tab_001_SA %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                             "results/BiB/001b_SA-ALL-maternal_characteristics_by_vegetarian_3cat.docx")


################################################################################
################################################################################

# Table 001_Other - All maternal characteristics by vegetarian subgroups
tab_001_Other <- tbl_summary(
  subset(dat, ethnic_Mat_cat == "Other"),
  by = VegDiet_3cat,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    FiveADay_Mat_DUR.p_3cat ~ "categorical",
    age_Mat_con ~ "continuous",
    edu_Mat_3cat ~ "categorical",
    IMD_Fam_cat ~ "categorical",
    parity_Mat_bin ~ "categorical",
    BMI_Mat_PRE.p_con ~ "continuous",
    smoking_Mat_EAR.p_bin ~ "categorical",
    alcohol_Mat_EAR.p_bin ~ "categorical",
    any.supp_Mat_EAR.p_bin ~ "categorical",
    iron.supp_Mat_EAR.p_bin ~ "categorical",
    multivit.supp_Mat_EAR.p_bin ~ "categorical",
    vitC.supp_Mat_EAR.p_bin ~ "categorical",
    vitD.supp_Mat_EAR.p_bin ~ "categorical",
    vitE.supp_Mat_EAR.p_bin ~ "categorical",
    other.supp_Mat_EAR.p_bin ~ "categorical",
    sex_Chi_bin ~ "categorical"
  ),
  missing = "always",
  missing_text = "Missing",
  include = c(
    FiveADay_Mat_DUR.p_3cat,
    age_Mat_con,
    edu_Mat_3cat,
    IMD_Fam_cat,
    parity_Mat_bin,
    BMI_Mat_PRE.p_con,
    smoking_Mat_EAR.p_bin,
    alcohol_Mat_EAR.p_bin,
    any.supp_Mat_EAR.p_bin,
    iron.supp_Mat_EAR.p_bin,
    multivit.supp_Mat_EAR.p_bin,
    vitC.supp_Mat_EAR.p_bin,
    vitD.supp_Mat_EAR.p_bin,
    vitE.supp_Mat_EAR.p_bin,
    other.supp_Mat_EAR.p_bin,
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
  modify_caption("Detailed participant characteristics by vegetarian subgroups in other ethnic groups") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_001_Other

tab_001_Other %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                                "results/BiB/001c_Other-ALL-maternal_characteristics_by_vegetarian_3cat.docx")

################################################################################

# Table 003_WB - Perinatal outcomes by vegetarian subgroups (3 categories)
tab_003_WB <- tbl_summary(
  subset(dat, ethnic_Mat_cat == "White British"),
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
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    sga ~ "categorical",
    lga ~ "categorical",
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
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
    lbw_subsamp,
    hbw_subsamp,
    sga,
    lga,
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
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
  modify_caption("Perinatal outcomes by vegetarian subgroups (3 categories) in White British") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003_WB

tab_003_WB %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                             "results/BiB/003a_WB-perinatal_outcomes_by_vegetarian_3cat.docx")

################################################################################
################################################################################

# Table 003_SA - Perinatal outcomes by vegetarian subgroups (3 categories)
tab_003_SA <- tbl_summary(
  subset(dat, ethnic_Mat_cat == "Pakistani"),
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
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    sga ~ "categorical",
    lga ~ "categorical",
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
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
    lbw_subsamp,
    hbw_subsamp,
    sga,
    lga,
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
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
  modify_caption("Perinatal outcomes by vegetarian subgroups (3 categories) in South Asians") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003_SA

tab_003_SA %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                             "results/BiB/003b_SA-perinatal_outcomes_by_vegetarian_3cat.docx")

################################################################################
################################################################################

# Table 003_Other - Perinatal outcomes by vegetarian subgroups (3 categories)
tab_003_Other <- tbl_summary(
  subset(dat, ethnic_Mat_cat == "Other"),
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
    lbw_subsamp ~ "categorical",
    hbw_subsamp ~ "categorical",
    sga ~ "categorical",
    lga ~ "categorical",
    apgar1 ~ "continuous",
    lowapgar1 ~ "categorical",
    apgar5 ~ "continuous",
    lowapgar5 ~ "categorical",
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
    lbw_subsamp,
    hbw_subsamp,
    sga,
    lga,
    apgar1,
    lowapgar1,
    apgar5,
    lowapgar5,
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
  modify_caption("Perinatal outcomes by vegetarian subgroups (3 categories) in other ethnic groups") %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

tab_003_Other

tab_003_Other %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                                "results/BiB/003c_Other-perinatal_outcomes_by_vegetarian_3cat.docx")

################################################################################

# [NOTE: Cross-tabulation between diet-based and self-defined vegetarianism NOT AVAILABLE as little overlap between FFQ1 and FFQ2!]

# # Table 007a - Diet-based vegetarianism by self-defined vegetarianism
# tab_007a <- tbl_summary(
#   dat,
#   by = self.VegDiet_Mat_DUR.p_3cat,
#   type = list(VegDiet_3cat ~ "categorical"),
#   missing = "no",
#   include = c(VegDiet_3cat)
# ) %>%
#   add_overall() %>%
#   add_n() %>%
#   modify_header(label ~ "** **") %>%
#   modify_spanning_header(c("stat_1", "stat_2") ~ "**Self-defined vegetarianism (3 categories)**") %>%
#   modify_footnote(all_stat_cols() ~ "Data are presented as frequency (percentage).") %>%
#   modify_caption("Agreement between diet-based and self-defined vegetarianism") %>%
#   bold_labels() %>%
#   modify_table_body(~ .x %>%
#                       dplyr::mutate(across(
#                         where(is.character), ~ gsub(",", "", ., fixed = T)
#                       )))
#
# tab_007a
#
# tab_007a %>% as_flex_table() %>% flextable::save_as_docx(., path =
#                                                            "results/BiB/007a-diet-based_vs_self-defined_vegetarianism.docx")
#
# ################################################################################
# ################################################################################
# ################################################################################
#
# # Table 007b - Self-defined vegetarianism by diet-based vegetarianism
# tab_007b <- tbl_summary(
#   dat,
#   by = VegDiet_3cat,
#   type = list(self.VegDiet_Mat_DUR.p_3cat ~ "categorical"),
#   missing = "no",
#   include = c(self.VegDiet_Mat_DUR.p_3cat)
# ) %>%
#   add_overall() %>%
#   add_n() %>%
#   modify_header(label ~ "** **") %>%
#   modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Diet-based vegetarianism (3 categories)**") %>%
#   modify_footnote(all_stat_cols() ~ "Data are presented as frequency (percentage).") %>%
#   modify_caption("Agreement between self-defined and diet-based vegetarianism") %>%
#   bold_labels() %>%
#   modify_table_body(~ .x %>%
#                       dplyr::mutate(across(
#                         where(is.character), ~ gsub(",", "", ., fixed = T)
#                       )))
#
# tab_007b
#
# tab_007b %>% as_flex_table() %>% flextable::save_as_docx(., path =
#                                                            "results/BiB/007b-self-defined_vs_diet-based_vegetarianism.docx")

################################################################################

################################################################################

# Sanity check

## Correlation between maternal education and IMD
dat$edu_Mat_3cat_num <- as.numeric(dat$edu_Mat_3cat)
dat$IMD_Fam_cat_num <- as.numeric(dat$IMD_Fam_cat)

corr_matrix <- rcorr(as.matrix(dat[, c("edu_Mat_3cat_num", "IMD_Fam_cat_num")]), type = "spearman")

correlation <- corr_matrix$r
p_values <- corr_matrix$P

print(correlation)
print(p_values)

################################################################################

################################################################################

# # Some checking
#
# ## Self-defined vegetarianism and socioeconomic position
# tab <- tbl_summary(
#   dat,
#   by = self.VegDiet_Mat_DUR.p_3cat,
#   statistic = list(
#     all_continuous() ~ "{mean} ({sd})",
#     all_categorical() ~ "{n} ({p}%)"
#   ),
#   type = list(
#     age_Mat_con ~ "continuous",
#     ethnic_Mat_bin ~ "categorical",
#     edu_Mat_3cat ~ "categorical",
#     IMD_Fam_cat ~ "categorical"
#   ),
#   missing = "no",
#   include = c(age_Mat_con, ethnic_Mat_bin, edu_Mat_3cat, IMD_Fam_cat)
# ) %>%
#   add_p(
#     test = list(all_continuous() ~ "aov"),
#     pvalue_fun = function(x)
#       style_pvalue(x, digits = 3)
#   ) %>%
#   add_overall() %>%
#   add_n() %>%
#   modify_header(label ~ "**Maternal socioeconomic position indicators**") %>%
#   modify_footnote(
#     all_stat_cols() ~ "Data are presented as mean (standard deviation) or frequency (percentage)."
#   ) %>%
#   modify_caption("Maternal socioeconomic position by self-defined vegetarian subgroups") %>%
#   bold_labels() %>%
#   modify_table_body(~ .x %>%
#                       dplyr::mutate(across(
#                         where(is.character), ~ gsub(",", "", ., fixed = T)
#                       )))
#
# tab
