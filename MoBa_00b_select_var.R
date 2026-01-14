################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa       #
################################################################################

# Last edited date: 18-May-2024
# This script is to select key variables for subsequent variable derivation and analyses in MoBa.

################################################################################

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
  openxlsx
)

# Set working directory
setwd("N:/durable/projects/Ran_MoBa_var")

################################################################################

# Load data
dat <- readRDS("dat_pheno_raw.rds")

head(dat)
dim(dat)  # 87271  1118

################################################################################
# Additionally add in other variables (requested later)
dat_Q1_add <-
  read.table(
    "2024-06-06_peiyuanh_PDB2552_Q1_v12_id-27.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
head(dat_Q1_add)
dim(dat_Q1_add)
colnames(dat_Q1_add)[colnames(dat_Q1_add) %in% colnames(dat)]

dat <-
  left_join(dat,
            dat_Q1_add,
            by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
################################################################################
################################################################################
################################################################################
dat_QF_add <-
  read.table(
    "2024-06-06_peiyuanh_PDB2552_QF_v12_id-28.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
head(dat_QF_add)
dim(dat_QF_add)
colnames(dat_QF_add)[colnames(dat_QF_add) %in% colnames(dat)]

dat <-
  left_join(dat,
            dat_QF_add,
            by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))

dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

################################################################################
################################################################################
################################################################################
head(dat)
dim(dat)  # 87271   1146
################################################################################

# Select key variables (to release memory usage)

## Load key variable lists

### IDs
ID_var <-
  read.xlsx("select_var.xlsx",
            sheet = "IDs")
ID_var

### Exposures
exp_var <-
  read.xlsx("select_var.xlsx",
            sheet = "Exposures")
exp_var

### Covariates
cov_var <-
  read.xlsx("select_var.xlsx",
            sheet = "Covariates")
cov_var

### Outcomes
out_var <-
  read.xlsx("select_var.xlsx",
            sheet = "Outcomes")
out_var

### Paternal variables
pat_var <-
  read.xlsx("select_var.xlsx",
            sheet = "Paternal")
pat_var

## Select variables in the lists
dat_select <-
  dat[, colnames(dat) %in% rbind(ID_var, exp_var, cov_var, out_var, pat_var)$var_name]

head(dat_select)
dim(dat_select)  # 1146 -> XXX variables

################################################################################

# Save dataset
saveRDS(dat_select, "dat_pheno_select.rds")
write_dta(dat_select, "dat_pheno_select.dta")
