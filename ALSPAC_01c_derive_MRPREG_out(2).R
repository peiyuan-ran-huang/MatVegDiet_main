################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 26-Jun-2024
# This script is to derive MR-PREG outcome variables in ALSPAC (Part 2).

# ADAPTED FROM: "2.alspac_dataset_mrpreg_20240610.R" written by the MR-PREG team

################################################################################

################################################################################################################################
################################################################################################################################
# MR-PREG outcomes in ALSPAC
################################################################################################################################
#Script developed by AF-S after AS 1st revision and CS changes in exclusion criteria and case/control definitions. Finished on 10.05.2021
#Updated on 04.06.2021
#Updated on 17.06.2021
#Updated on 24.07.2023 by AT - adding in the hyperemesis and NVP variables
#Updated on 08.08.2023 by AS - updated code to reflect changes in definition of unique woman (ALSPAC syntax)
#Updated on 05.09.2023 by AS - corrected breastfeeding variable
#Updated on 10.06.2024 by AS - added date stamp on file name and revised code regarding missing data

################################################################################################################################
################################################################################################################################
## Delete this part for GitHub
rm(list=ls())

data_dir <- "Z:/working/data/ALSPAC/"

################################################################################################################################
################################################################################################################################
#     1. WD
################################################################################################################################
################################################################################################################################


today = Sys.Date()
today = gsub("-","_",today)


# Load libraries
library(haven)
library(dplyr)
library(foreign)


setwd(data_dir)

alspac <- read.table("pheno_all_Rvars.csv", sep=";", header = T)

# ################################################################################################################################
# ### 7.1 Birth weight and gestational age-related variables - VARIABLES DERIVED IN STATA
# ################################################################################################################################




# - Data generated with STATA code where zanthro considering reference values for weight-for-age

pheno_stata <- read.table(paste0(data_dir, "pheno_all_vars", ".csv"), sep=",", header = T)
names(pheno_stata)
dim(pheno_stata)



# 7.1.1 Birthweight <10th percentile for gestational age
################################################################################################################################

# 7.1.1.a Cases <10 percentile, controls at least in the 10th percentile

# Merge Stata and R datafiles
pheno <- merge(alspac, pheno_stata, by = c("aln","qlet"))
dim(pheno)

# Using UK WHO Preterm Growth Charts, version UKWHOpreterm
table(pheno$sga_UKWHO, useNA = "always")

# Using British 1990 Growth Charts, version UK
table(pheno$sga_UK, useNA = "always")

# Compare UK WHO Preterm vs British 1990 Growth Charts
table(pheno$sga_UKWHO, pheno$sga_UK, useNA = "always")

# Derive sga_all using UKWHOpreterm version
pheno$sga_all <- case_when(pheno$sga_UKWHO == 1 ~ 1,
                            pheno$sga_UKWHO == 0 ~ 0,
                            TRUE ~ NA_real_
                            )
                            
pheno$sga_all <- as.factor(pheno$sga_all)
summary(pheno$sga_all)

#even if I did not restrict in stata to singletons and live births, there are none classified as cases or controls
# table(pheno$sga_all,pheno$singleton)
#       0     1
# 0     0 12411
# 1     0  1073
# table(pheno$sga_all,pheno$kz010)
#       0     1     2     3     4     7
# 0     0     0    26     4    17 12364
# 1     0     0     5     2     4  1062



# 7.1.2 Birthweight >90th percentile for gestational age
################################################################################################################################

# 7.1.2.a Cases >90 percentile, controls at least in the 90th percentile

pheno$lga_all <- case_when(pheno$lga_UKWHO == 1 ~ 1,
                            pheno$lga_UKWHO == 0 ~ 0,
                            TRUE ~ NA_real_
)
pheno$lga_all <- as.factor(pheno$lga_all)
summary(pheno$lga_all)

#even if I did not restrict in stata to singletons and live births, there are none classified as cases or controls
# table(pheno$lga_all,pheno$singleton)
#       0     1
# 0     0 12005
# 1     0  1479
# table(pheno$lga_all,pheno$kz010)
#       0     1     2     3     4     7
# 0     0     0    28     5    21 11951
# 1     0     0     3     1     0  1475





################################################################################################################################
################################################################################################################################
#     8. Save final dataset
################################################################################################################################
################################################################################################################################



#I add new identifiers
pheno$aln1 <- paste(pheno$aln, "M", sep="")
pheno$aln2 <- paste(pheno$aln, pheno$qlet, "M", sep="")



#i select the identifiers and MR-PREG vars
# pheno <- pheno[, c("aln", "qlet", "aln1", "aln2",
#                    "hdp_all", "hdp_subsamp", "gh_all", "gh_subsamp", "pe_all", "pe_subsamp", 
#                    "gdm_all", "gdm_subsamp","anaemia_preg_all", "anaemia_post_all",  
#                    "depr_all", "depr_subsamp", "ante_depr_all", "ante_depr_subsamp",
#                    "induction", "rup_memb", "cs", "el_cs", "em_cs", 
#                    "misc_all", "r_misc_all", "s_misc_all", "misc_subsamp", "r_misc_subsamp", "s_misc_subsamp","sb_all", "sb_subsamp", "pregloss",
#                    "misc_index_all", "misc_i_subsamp", "sb_index_all", "sb_i_subsamp",
#                    "bw", "zbw_all", "zbw_subsamp", "lbw_all", "lbw_subsamp","hbw_all", "hbw_subsamp","ga_all", "ga_subsamp",
#                    "vpretb_all", "vpretb_subsamp", "pretb_all", "pretb_subsamp", "posttb_all", "posttb_subsamp",
#                    "sga_all", "lga_all",
#                    "apgar1", "apgar5", "lowapgar1", "lowapgar5", "nicu",
#                    "bf_ini", "bf_dur", "bf_dur_inv", "bf_dur_4c","bf_est", "bf_sus", "hyp", "nvp_sev_all", "nvp_sev_subsamp"
# )]

# write.table(pheno, file=paste0(data_dir, "pheno_mr-preg", ".txt"), sep=" ", col.names=T, quote=F)
# 
# pheno <- read.table(paste0(data_dir, "pheno_mr-preg", ".txt"), 
#                     header=T, 
#                     stringsAsFactors=F, 
#                     sep=" ")
head(pheno)
dim(pheno)

saveRDS(pheno, "dat_out.rds")
