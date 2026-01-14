################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - BiB        #
################################################################################

# Last edited date: 03-Oct-2024
# This script is to perform multiple imputation by chained equations (MICE) for main association analysis in BiB.

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
  ordinal,
  VGAM,
  mice,
  broom,
  VIM,
  caret,
  knitr
)

# Set working directory
setwd("Z:/working/")

################################################################################

# Load data
dat <- readRDS("data/BiB/dat_exp_cov_out.rds")
head(dat)
dim(dat)  # 3647  XXX

################################################################################

# Outcome grouping

## Load outcome lists and labels
MRPREG_outcome_labels <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

primary_bin <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Primary_bin")
primary_bin  # 13 primary (binary) outcomes

secondary_bin <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_bin")
secondary_bin  # 9 secondary binary outcomes

secondary_con <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_con")
secondary_con  # 4 secondary continuous outcomes

secondary_cat <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_cat")
secondary_cat  # 1 (secondary) ordinal/categorical outcome (bf_dur_4c as negative outcome)

## Identify available outcomes in the cohort
BiB_primary_bin <-
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat))]
BiB_primary_bin  # 11 primary (binary) outcomes available in BiB

BiB_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat))]
BiB_secondary_bin  # 8 secondary binary outcomes available in BiB

BiB_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat))]
BiB_secondary_con  # 4 secondary continuous outcomes available in BiB

BiB_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat))]
BiB_secondary_cat  # 1 (primary) ordinal/categorical outcome available in BiB

## Group outcome variables
BiB_out_bin <- c(BiB_primary_bin, BiB_secondary_bin)
BiB_out_con <- BiB_secondary_con
BiB_out_cat <- BiB_secondary_cat

################################################################################

# Multiple imputation by chained equations (MICE)

## Load selected variables
substantive_var <-
  read.xlsx("data/BiB/select_var.xlsx", sheet = "Substantive")
substantive_var  # Substantive variables (e.g., covariates)
mean(substantive_var$var_name %in% colnames(dat)) == 1  # Check if all variables are available in the dataset

auxiliary_var <-
  read.xlsx("data/BiB/select_var.xlsx", sheet = "Auxiliary")
auxiliary_var  # Auxiliary variables (e.g., IDs, exposures, covariates at different time points)
mean(auxiliary_var$var_name %in% colnames(dat)) == 1  # Check if all variables are available in the dataset

################################################################################
other_imp <-
  c("NON.iron.multivit.supp_Mat_EAR.p_bin")  # Other variables to be imputed (e.g., used for adjustment)
other_N_imp <-
  c("iron.multivit.supp_Mat_EAR.p_bin",
    "vit0vitipr",
    "ethnic_Mat_cat",
    "VegDiet_bin")  # Other variables NOT to be imputed (e.g., used for stratification or as alternative exposure)
################################################################################

dat <- subset(dat, select = colnames(dat)[which(
  colnames(dat) %in% c(
    substantive_var$var_name,
    auxiliary_var$var_name,
    BiB_out_bin,
    BiB_out_con,
    BiB_out_cat,
    other_imp,
    other_N_imp
  )
)])

head(dat)
dim(dat)  # 3647   40

## Check missing data
## [NOTE: If there are variables with missingness not to be imputed but used to impute other variables,
##        the imputed variables will still have some missingness (but less than in the original dataset).]
sum(is.na(dat$BiBMotherID))  # 0
sum(is.na(dat$VegDiet_3cat))  # 0
sum(is.na(dat$VegDiet_bin))  # 0

## Prepare data for imputation
convert_binary_factors <- function(dat) {
  for (var in names(dat)) {
    if (is.factor(dat[[var]]) && nlevels(dat[[var]]) == 2) {
      levels <- levels(dat[[var]])
      dat[[var]] <-
        factor(as.numeric(dat[[var]]) - 1, levels = c(0, 1))
    }
  }
  return(dat)
}
dat <-
  convert_binary_factors(dat)  # Convert binary factors (1/2) to 0/1

################################################################################
# Relabel variables (for outcome variables only)
for (var_name in c(BiB_primary_bin,
                   BiB_secondary_bin,
                   BiB_secondary_con,
                   BiB_secondary_cat))
{
  var_lab(dat[, var_name]) <-
    MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$varname == var_name)]
}
################################################################################

## Set method for imputation
method <- sapply(dat, function(x) {
  if (is.numeric(x)) {
    return("pmm")  # Predictive mean matching for all continuous variables
    # } else if (is.ordered(x)) {
    #   return("polr")  # Proportional odds logistic regression for all ordinal variables
  } else if (is.factor(x) && nlevels(x) == 2) {
    return("logreg")  # Logistic regression for all binary variables
  } else if (is.factor(x) && nlevels(x) > 2) {
    return("polyreg")  # Polynomial regression for all categorical factor variables
  } else if (is.character(x)) {
    return("polyreg")  # Polynomial regression for all categorical character variables
  } else {
    return("pmm")  # Predictive mean matching for all other variables
  }
})

cov <- substantive_var$var_name  # Specify covariates (i.e., key variables to be imputed)
cov
# [1] "age_Mat_con"            "ethnic_Mat_bin"         "edu_Mat_3cat"           "IMD_Fam_cat"
# [5] "parity_Mat_bin"         "BMI_Mat_PRE.p_con"      "smoking_Mat_EAR.p_bin"  "alcohol_Mat_EAR.p_bin"
# [9] "any.supp_Mat_EAR.p_bin" "sex_Chi_bin"

method[c("BiBMotherID",
         "VegDiet_3cat",
         BiB_out_bin,
         BiB_out_con,
         BiB_out_cat,
         other_N_imp)] <-
  ""  # IDs, exposures, outcomes, other non-imputed variables - NOT being imputed
method

## Check correlation matrix for continuous variables
dat_num <- dat %>% select_if(is.numeric)
corr_matrix <- cor(dat_num, use = "complete.obs")
corr_matrix

high_corr_vars <-
  findCorrelation(corr_matrix, cutoff = 0.9, names = T)
cat("Highly correlated variables (cutoff = 0.9):",
    high_corr_vars,
    "\n")  # No highly correlated variables identified

## Set predictor matrix for imputation
predictorMatrix <- make.predictorMatrix(dat)
predictorMatrix[, c("BiBMotherID",
                    BiB_out_bin,
                    BiB_out_con,
                    BiB_out_cat,
                    other_N_imp)] <-
  0  # NOT being used in imputation for other variables: IDs, outcomes, other non-imputed variables
predictorMatrix[c("BiBMotherID",
                  "VegDiet_3cat",
                  BiB_out_bin,
                  BiB_out_con,
                  BiB_out_cat,
                  other_N_imp), ] <-
  0  # NOT being imputed: IDs, exposures, outcomes, other non-imputed variables
predictorMatrix

# ## Dry run for imputation
# dryrun <- mice(
#   dat,
#   m = 1,
#   maxit = 5,
#   predictorMatrix = predictorMatrix,
#   method = method,
#   seed = 19705,
#   printFlag = T
# )
# dryrun$method
# dryrun$predictorMatrix
# dryrun$loggedEvents
# dryrun_dat <- complete(dryrun, 1)
# head(dryrun_dat)
# for (varname in cov) {
#   print(varname)
#   print(sum(is.na(dryrun_dat[[varname]])))
# }

## !!! Check percentages of incomplete cases (in covariates) !!!
dat_cov <- dat[, colnames(dat) %in% cov]  # Subset to covariates
head(dat_cov)
dim(dat_cov)  # 3647   10

dat_cov_md.pattern <-
  md.pattern(dat_cov)  # Check missing data patterns
summary(dat_cov_md.pattern)

num_incomplete_cases <- sum(!complete.cases(dat_cov))
num_total_cases <- nrow(dat_cov)
(num_incomplete_cases / num_total_cases) * 100  # Percentage of incomplete cases: 27.30003%

## Impute data with MICE
dat_imp <- mice(
  dat,
  m = 30,
  maxit = 20,
  predictorMatrix = predictorMatrix,
  method = method,
  seed = 19705,
  printFlag = T
)

## Save imputed data
save(dat_imp, file = "data/BiB/dat_exp_cov_out_IMP.RData")
load("data/BiB/dat_exp_cov_out_IMP.RData")

################################################################################

# Check imputed data
summary(dat_imp)

plot(dat_imp)  # Trace plots

dat_com <- list()
for (i in 1:dat_imp$m) {
  complete_dat <- complete(dat_imp, i)
  dat_com[[i]] <- complete_dat
}  # Save complete datasets

dat_com_1 <- dat_com[[1]]
for (varname in cov) {
  print(varname)
  print(sum(is.na(dat_com_1[[varname]])))
}

dat_com_2 <- dat_com[[2]]
for (varname in cov) {
  print(varname)
  print(sum(is.na(dat_com_2[[varname]])))
}

dat_com_3 <- dat_com[[3]]
for (varname in cov) {
  print(varname)
  print(sum(is.na(dat_com_3[[varname]])))
}

################################################################################

# Summary of observed and imputed data

## Combine imputed data
dat_imp_long <- complete(dat_imp, "long", include = TRUE)
dat_imp_long <- dat_imp_long[dat_imp_long$.imp != 0, ]
dat_imp_long <-
  dat_imp_long[, !grepl("^.imp|^\\.id", colnames(dat_imp_long))]
head(dat_imp_long)
dim(dat_imp_long)  # 109410     41

## Add marker for observed/imputed data
dat$marker <- "Observed"
dat_imp_long$marker <- "Imputed"

dim(dat)  # 3647   41
dim(dat_imp_long)  # 109410     42

## Combine observed and imputed data
dat_mix <- rbind(dat, dat_imp_long)
dat_mix$marker <-
  factor(dat_mix$marker, levels = c("Observed", "Imputed"))
head(dat_mix)
dim(dat_mix)  # 113057     42

## Summary of observed/imputed data
theme_gtsummary_compact()

tab_ori_imp <- tbl_summary(
  dat_mix[, c("marker", cov)],
  by = marker,
  statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(all_categorical() ~ 1)
) %>%
  modify_header(label ~ "**Covariate**") %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as mean (standard deviation) or percentage.") %>%
  modify_caption(
    "Distribution of covariates (for main analysis) in the observed and imputed datasets in BiB"
  ) %>%
  bold_labels() %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(across(
                        where(is.character), ~ gsub(",", "", ., fixed = T)
                      )))

## Calculate percentage of imputed data
perc_imputed <- sapply(dat[, cov], function(x) {
  paste0(sprintf("%.1f", sum(is.na(x)) / length(x) * 100), "%")
})
perc_imputed

## Incorporate percentage of imputed data into the summary table
perc_imputed_df <- data.frame(
  variable = names(perc_imputed),
  perc_imputed = perc_imputed,
  stringsAsFactors = F
)
perc_imputed_df$variable_row_type <-
  paste0(perc_imputed_df$variable, "_label")
perc_imputed_df <- perc_imputed_df %>% select(-variable)
perc_imputed_df

tab_ori_imp_body <- as_tibble(tab_ori_imp$table_body)
tab_ori_imp_body$variable_row_type <-
  paste0(tab_ori_imp_body$variable, "_", tab_ori_imp_body$row_type)
tab_ori_imp_body <-
  tab_ori_imp_body %>% left_join(perc_imputed_df, by = "variable_row_type")
tab_ori_imp_body <- tab_ori_imp_body %>% select(-variable_row_type)
tab_ori_imp_body

## Recreate the tbl_summary object with the new column
tab_ori_imp <- tab_ori_imp %>%
  modify_table_body(~ tab_ori_imp_body) %>%
  modify_header(update = list(perc_imputed ~ "**% data imputed**"))

tab_ori_imp

tab_ori_imp %>% as_flex_table() %>% flextable::save_as_docx(., path =
                                                              "results/BiB/00a-maternal_covariates_OBS.vs.IMP.docx")
