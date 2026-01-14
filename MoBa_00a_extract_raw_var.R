################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - MoBa       #
################################################################################

# Last edited date: 18-May-2024
# This script is to combine and extract raw variables in MoBa.

################################################################################

# Clear environment
rm(list = ls())

# Collect information about the current R session
sessionInfo()

# Load packages
library(tidyverse)
library(tidyverse)
library(haven)

# Set working directory
setwd("N:/durable/projects/Ran_MoBa_var")

################################################################################

# Load MoBa raw data (each questionnaire may have more than one datasets as they were requested separately on different dates)
dat_Q1_1 <-
  read.table(
    "2023-03-29_peiyuanh_PDB2552_Q1_v12_id-4.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_Q1_2 <-
  read.table(
    "2023-11-24_peiyuanh_PDB2552_Q1_v12_id-22.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_Q2 <-
  read.table(
    "2023-03-29_peiyuanh_PDB2552_Q2CDW_v12_id-3.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_Q2_cal <-
  read.table(
    "2023-04-17_peiyuanh_PDB2552_Q2_calculation_CDW_v12_id-10.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_Q2_255 <-
  read.table(
    "2023-04-17_peiyuanh_PDB2552_Skjema2_beregning_CDW_foodgroups255_v12_id-9.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_Q3_1 <-
  read.table(
    "2023-03-29_peiyuanh_PDB2552_Q3_v12_id-5.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_Q3_2 <-
  read.table(
    "2023-11-24_peiyuanh_PDB2552_Q3_v12_id-23.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_DODKAT <-
  read.table(
    "2023-11-24_peiyuanh_PDB2552_DODKAT_MFR_541_v12_id-20.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_MBRN_1 <-
  read.table(
    "2023-03-29_peiyuanh_PDB2552_MBRN_541_v12_id-7.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_MBRN_2 <-
  read.table(
    "2023-11-24_peiyuanh_PDB2552_MBRN_541_v12_id-21.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_Q4 <-
  read.table(
    "2023-11-24_peiyuanh_PDB2552_Q4_6months_v12_id-24.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )
dat_QF <-
  read.table(
    "2023-03-29_peiyuanh_PDB2552_QF_v12_id-8.txt",
    header = T,
    sep = "\t",
    na.strings = " "
  )

head(dat_Q1_1)
head(dat_Q1_2)
head(dat_Q2)
head(dat_Q2_cal)
head(dat_Q2_255)
head(dat_Q3_1)
head(dat_Q3_2)
head(dat_DODKAT)
head(dat_MBRN_1)
head(dat_MBRN_2)
head(dat_Q4)
head(dat_QF)

dim(dat_Q1_1)  # 103508    352
dim(dat_Q1_2)  # 103508     33
dim(dat_Q2)  # 88930   861
dat_Q2 <-
  subset(dat_Q2,
         select = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552", "BB15"))  # Trim to release system storage
dim(dat_Q2_cal)  # 88889    43
dim(dat_Q2_255)  # 88890   259
dim(dat_Q3_1)  # 95352   288
dim(dat_Q3_2)  # 95352    15
dim(dat_DODKAT)  # 117516      4
dim(dat_MBRN_1)  # 117516     10
dim(dat_MBRN_2)  # 117516     22
dim(dat_Q4)  # 92276    19
dim(dat_QF)  # 78230   102

################################################################################

# Merge using dat_Q2_255 (those with FFQ data) as the "base" (not using pipeline, in order to see merging details)
dat <-
  left_join(dat_Q2_255,
            dat_Q2_cal,
            by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_Q2_255, dat_Q2_cal)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_Q1_1, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_Q1_1)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_Q1_2, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_Q1_2)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_Q2, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_Q2)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_Q3_1, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_Q3_1)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_Q3_2, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_Q3_2)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_DODKAT, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_DODKAT)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_MBRN_1, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_MBRN_1)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_MBRN_2, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_MBRN_2)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_Q4, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_Q4)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

dat <-
  left_join(dat, dat_QF, by = c("PREG_ID_2552", "M_ID_2552", "F_ID_2552"))
rm(dat_QF)
dat <-
  dat[!duplicated(dat[, c("PREG_ID_2552", "M_ID_2552", "F_ID_2552")]), ]

################################################################################

# Convert data format
dat <- as.tibble(dat)
head(dat)
dim(dat)  # 87271  1118

sum(duplicated(dat$PREG_ID_2552))  # No duplicated pregnancy IDs

dat[5:ncol(dat)] <-
  dat[5:ncol(dat)] %>% mutate(across(everything(),
                                     ~ as.numeric(as.character(.))))  # Convert character into numeric (skip ID and version columns)

head(dat)
head(dat[, 500:510])

################################################################################

# Save data
saveRDS(dat, "dat_pheno_raw.rds")

# dat <- readRDS("dat_pheno_raw.rds")
