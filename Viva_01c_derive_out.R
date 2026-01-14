################################################################################
#   Maternal Vegetarian/Plant-based Diets & Perinatal Health - Project Viva    #
################################################################################

# Last edited date: 18-May-2024
# This script is to derive MR-PREG outcome variables in Project Viva.

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
dat <- readRDS("data/Viva/dat_exp_cov.rds")
dat <- zap_labels(dat)

head(dat)
dim(dat)  # 1872  583

#------------------------------------------------------------------------------#
#                               Derive Outcomes                                #----
#------------------------------------------------------------------------------#

# [NOTE: _subsamp: pre-existing conditions excluded; _all: pre-existing conditions NOT excluded.
#       For this analysis, we will only derive and use _subsamp outcomes (except for maternal anaemia).]

################################################################################

# preE_vitals_hosp_d (values: PE, GH, CH, normal)
dat$preE_vitals_hosp_d
dat$preE_vitals_hosp_d[dat$highbp_mom_epi_epia_d == 1] <-
  NA  # Exclude: pre-existing hypertension

## Hypertensive disorders of pregnancy
dat$hdp_subsamp <- NA
dat$hdp_subsamp[dat$preE_vitals_hosp_d == "No"] <- 0
dat$hdp_subsamp[dat$preE_vitals_hosp_d == "GH" |
                  dat$preE_vitals_hosp_d == "PE"] <- 1

## Gestational hypertension
dat$gh_subsamp <- NA
dat$gh_subsamp[dat$preE_vitals_hosp_d == "No"] <- 0
dat$gh_subsamp[dat$preE_vitals_hosp_d == "GH"] <- 1

## Pre-eclampsia
dat$pe_subsamp <- NA
dat$pe_subsamp[dat$preE_vitals_hosp_d == "No"] <- 0
dat$pe_subsamp[dat$preE_vitals_hosp_d == "PE"] <- 1

################################################################################

# gdmstat_4cat_d - Values: GDM, IGT, TH, normal
dat$gdmstat_4cat_d
dat$gdmstat_4cat_d[dat$t1diab_mom_epi_epia_d == 1 |
                     dat$t2diab_mom_epi_epia_d == 1] <-
  NA  # Exclude: pre-existing type 1 and type 2 diabetes

## Gestational diabetes ("transient hyperglycemia" and "igt" recoded as controls) - !!! NEED DOUBLE CHECK !!!
dat$gdm_subsamp <- NA
dat$gdm_subsamp[dat$gdmstat_4cat_d == "nor"] <- 0
dat$gdm_subsamp[dat$gdmstat_4cat_d == "gdm"] <- 1

################################################################################

# anemia_preg (any Hb <110 g/l in the first trimester or Hb <105 g/l in the second or third trimesters)
dat$anemia_preg

## Maternal anaemia during pregnancy (no exclusion as data on pre-existing anaemia not available)
dat$anaemia_preg_all <- dat$anemia_preg

################################################################################

# edinsum_mpq_d (EPDS score) edinsum_mpq_ge13 (≥13)

## In mid-pregnancy
dat$edinsum_mpq_d
dat$edinsum_mpq_ge13

## At 6 months postpartum
dat$edinsum_smq_d
dat$edinsum_smq_ge13

## At 12 months postpartum
dat$edinsum_1y
dat$edinsum_1y_ge13

## Perinatal depression - Use MR-PREG definition: average score across three time points ≥13
x <- rowMeans(dat[, c("edinsum_mpq_d", "edinsum_smq_d", "edinsum_1y")], na.rm = T)

dat$depr_subsamp <- NA
dat$depr_subsamp[x >= 13] <- 1
dat$depr_subsamp[x < 13] <- 0

rm(x)

dat$depr_subsamp[dat$dephx == 1] <-
  NA  # Exclude: pre-existing depression

################################################################################

# Prelabour rupture of membrane - NOT AVAILABLE

################################################################################

# csection (0/1)
# delivery_type_3cat_d
#   - planned cesarean birth
#   - unplanned cesarean birth
#   - vaginal birth
# delivery_type_5cat_d
#   - induction of labor, cesarean birth
#   - induction of labor, vaginal birth
#   - no labor, cesarean birth
#   - spontaneous labor, cesarean birth
#   - spontaneous labor, vaginal birth
dat$csection
dat$delivery_type_3cat_d
dat$delivery_type_5cat_d

## Induction of labour (control: no induction)
dat$induction <- NA
dat$induction[dat$delivery_type_5cat_d == "spontaneous labor, vaginal birth" |
                dat$delivery_type_5cat_d == "spontaneous labor, cesarean birth" |
                dat$delivery_type_5cat_d == "no labor, cesarean birth"] <-
  0
dat$induction[dat$delivery_type_5cat_d == "induction of labor, vaginal birth" |
                dat$delivery_type_5cat_d == "induction of labor, cesarean birth"] <-
  1

## Caesarean section
dat$cs <- dat$csection

## Elective Caesarean section (control: no Caesarean section)
dat$el_cs <- NA
dat$el_cs[dat$delivery_type_3cat_d == "vaginal birth"] <- 0
dat$el_cs[dat$delivery_type_3cat_d == "planned cesarean birth"] <- 1

## Emergency Caesarean section (control: no Caesarean section)
dat$em_cs <- NA
dat$em_cs[dat$delivery_type_3cat_d == "vaginal birth"] <- 0
dat$em_cs[dat$delivery_type_3cat_d == "unplanned cesarean birth"] <-
  1

################################################################################

# From Sheryl: I gave you gestage_wks_deliv_d (continuous, and you can derive binary variables).
summary(dat$gestage_wks_deliv_d)

## Gestational age
dat$ga_subsamp <- dat$gestage_wks_deliv_d
dat$ga_subsamp[dat$el_cs == 1 |
                 dat$induction == 1] <-
  NA  # Exclude: elective Caesarean section and physician-initiated delivery (induction)

## Preterm birth
dat$pretb_subsamp <- NA
dat$pretb_subsamp[dat$ga_subsamp >= 37 & dat$ga_subsamp < 42] <- 0
dat$pretb_subsamp[dat$ga_subsamp < 37] <- 1

## Very preterm birth
dat$vpretb_subsamp <- NA
dat$vpretb_subsamp[dat$ga_subsamp >= 37 & dat$ga_subsamp < 42] <- 0
dat$vpretb_subsamp[dat$ga_subsamp < 34] <- 1

## Post-term birth
dat$posttb_subsamp <- NA
dat$posttb_subsamp[dat$ga_subsamp >= 37 & dat$ga_subsamp < 42] <- 0
dat$posttb_subsamp[dat$ga_subsamp >= 42] <- 1

################################################################################

# From Sheryl: I gave you birthweight_d (continuous birthweight in grams, and you can derive binary variables).
summary(dat$birthweight_d)  # Birth weight in grams
summary(dat$zvalue_bwt_gage_sex_d)  # Birth weight z-score based on the US reference (Oken et al., 2003, BMC Pediatr)

## Birth weight z-score - !!! Use sample mean and SD, NOT the US reference (different from SGA and LGA below) !!!
bw_sex <- dat %>%
  group_by(sex_Chi_bin) %>%
  dplyr::summarise(
    bw_mean = mean(birthweight_d, na.rm = T),
    bw_sd = sd(birthweight_d, na.rm = T)
  )  # Calculate sex-specific study mean and SD

dat <- dat %>%
  left_join(bw_sex, by = "sex_Chi_bin") %>%
  mutate(zbw_subsamp = (birthweight_d - bw_mean) / bw_sd) %>% select(-c("bw_mean", "bw_sd"))  # Calculate raw z-score

dat[abs(dat$zbw_subsamp) > 5, c("familyid",
                                "aid",
                                "sex_Chi_bin",
                                "parity_d",
                                "zbw_subsamp",
                                "gestage_wks_deliv_d")]  # 2 with extreme birth weight (>5 SD from the sex-specific study mean)

dat$zbw_subsamp[dat$gestage_wks_deliv_d < 37 |
                  abs(dat$zbw_subsamp) > 5] <-
  NA  # Exclude: preterm births (gestational age <37 weeks) and extreme birth weight (>5 SD from the sex-specific study mean)

cor(dat$zbw_subsamp,
    dat$zvalue_bwt_gage_sex_d,
    method = "pearson",
    use = "complete.obs")  # 0.9666966 - High correlation between study mean/SD and the US reference

## Low birth weight
dat$lbw_subsamp <- NA
dat$lbw_subsamp[dat$birthweight_d >= 2500 &
                  dat$birthweight_d <= 4500] <-
  0
dat$lbw_subsamp[dat$birthweight_d < 2500] <- 1

dat$lbw_subsamp[dat$gestage_wks_deliv_d < 37] <-
  NA  # Exclude: preterm births (gestational age <37 weeks) - !!! No need to exclude extreme birth weight !!!

## High birth weight
dat$hbw_subsamp <- NA
dat$hbw_subsamp[dat$birthweight_d >= 2500 &
                  dat$birthweight_d <= 4500] <-
  0
dat$hbw_subsamp[dat$birthweight_d > 4500] <- 1

dat$hbw_subsamp[dat$gestage_wks_deliv_d < 37] <-
  NA  # Exclude: preterm births (gestational age <37 weeks) - !!! No need to exclude extreme birth weight !!!

## Small for gestational age - !!! No special exclusion criteria !!!
dat$sga <- NA
dat$sga[dat$zvalue_bwt_gage_sex_d >= -1.3105791] <-
  0  # !!! Use the US reference (different from ZBW above) !!!
dat$sga[dat$zvalue_bwt_gage_sex_d < -1.3105791] <- 1

## Large for gestational age - !!! No special exclusion criteria !!!
dat$lga <- NA
dat$lga[dat$zvalue_bwt_gage_sex_d <= 1.3105791] <-
  0  # !!! Use the US reference (different from ZBW above) !!!
dat$lga[dat$zvalue_bwt_gage_sex_d > 1.3105791] <- 1

################################################################################

# Apgar score at 1 minute - NOT AVAILABLE

# Low Apgar score at 1 minute (<7) - NOT AVAILABLE

# Apgar score at 5 minutes - NOT AVAILABLE

# Low Apgar score at 5 minutes (<7) - NOT AVAILABLE

################################################################################

# NICU admission - NOT AVAILABLE

################################################################################

# bfdur_mos_oyq_d (duration at 12m)
# if bfdur_mos_oyq_d >0 then bfdur_mos_oyq_any = 1;
# else if bfdur_mos_oyq_d = 0 then bfdur_mos_oyq_any = 0;
# if bfdur_mos_oyq_d ≥2 then bfdur_mos_oyq_ge2m = 1;
# else if bfdur_mos_oyq_d ≥0 then bfdur_mos_oyq_ge2m = 0;
# if bfdur_mos_oyq_d ≥6 then bfdur_mos_oyq_ge6m = 1;
# else if bfdur_mos_oyq_d ≥0 then bfdur_mos_oyq_ge6m = 0;
table(dat$bfdur_mos_oyq_d, useNA = "always")
table(dat$bfdur_mos_oyq_any, useNA = "always")
table(dat$bfdur_mos_oyq_ge2m, useNA = "always")
table(dat$bfdur_mos_oyq_ge6m, useNA = "always")

## Breastfeeding NOT initiated
dat$bf_N_ini <- dat$bfdur_mos_oyq_any
dat$bf_N_ini <- car::recode(dat$bf_N_ini,
                            "1=0;0=1",
                            as.factor = F,
                            as.numeric = T)  # 0 (No) = Initiated; 1 (Yes) = NOT initiated

## Breastfeeding NOT established ("established" defined as breastfeeding duration ≥2 months)
dat$bf_N_est <- NA

dat$bf_N_est[dat$bfdur_mos_oyq_d >= 2] <- 0  # 0 (No) = Established
dat$bf_N_est[dat$bfdur_mos_oyq_d < 2] <-
  1  # 1 (Yes) = NOT established

# dat$bf_N_est[dat$bfdur_mos_oyq_d == 0] <-
#   NA  # Exclude: never breastfed
# dat$bf_N_est[dat$pretb_subsamp == 1] <-
#   NA  # Exclude: GA < 37

## Breastfeeding NOT sustained ("sustained" defined as breastfeeding duration ≥6 months)
dat$bf_N_sus <- NA
dat$bf_N_sus[dat$bfdur_mos_oyq_d >= 6] <- 0  # 0 (No) = Sustained
dat$bf_N_sus[dat$bfdur_mos_oyq_d < 6] <-
  1  # 1 (Yes) = NOT sustained

# dat$bf_N_sus[dat$bfdur_mos_oyq_d == 0] <-
#   NA  # Exclude: never breastfed
# dat$bf_N_sus[dat$pretb_subsamp == 1] <-
#   NA  # Exclude: GA < 37

## Breastfeeding duration - Ordinal/4 categories: 0 to <1 month, 1 to <3 months, 3 to <6 months, ≥6 months
dat$bf_dur_4c <- NA
dat$bf_dur_4c[dat$bfdur_mos_oyq_d >= 0 &
                dat$bfdur_mos_oyq_d < 1] <- 0
dat$bf_dur_4c[dat$bfdur_mos_oyq_d >= 1 &
                dat$bfdur_mos_oyq_d < 3] <- 1
dat$bf_dur_4c[dat$bfdur_mos_oyq_d >= 3 &
                dat$bfdur_mos_oyq_d < 6] <- 2
dat$bf_dur_4c[dat$bfdur_mos_oyq_d >= 6] <- 3

################################################################################

# Label variables and values

## Load outcome lists and labels
MRPREG_outcome_labels <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Label")
MRPREG_outcome_labels
str(MRPREG_outcome_labels)  # 60 MR-PREG outcomes in total

primary_bin <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Primary_bin")
primary_bin  # 14 primary (binary) outcomes

secondary_bin <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_bin")
secondary_bin  # 8 secondary binary outcomes

secondary_con <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_con")
secondary_con  # 4 secondary continuous outcomes

secondary_cat <-
  read.xlsx("data/MRPREG_outcome_labels.xlsx", sheet = "Secondary_cat")
secondary_cat  # 1 (secondary) ordinal/categorical outcome (bf_dur_4c as negative outcome)

## Identify available outcomes in the cohort
Viva_primary_bin <-
  primary_bin$varname[which(primary_bin$varname %in% colnames(dat))]
Viva_primary_bin  # 9 primary (binary) outcomes available in Viva

Viva_secondary_bin <-
  secondary_bin$varname[which(secondary_bin$varname %in% colnames(dat))]
Viva_secondary_bin  # 7 secondary binary outcomes available in Viva

Viva_secondary_con <-
  secondary_con$varname[which(secondary_con$varname %in% colnames(dat))]
Viva_secondary_con  # 2 secondary continuous outcomes available in Viva

Viva_secondary_cat <-
  secondary_cat$varname[which(secondary_cat$varname %in% colnames(dat))]
Viva_secondary_cat  # 1 (primary) ordinal/categorical outcome available in Viva

## Label values

### Binary
for (var_name in colnames(dat[, which(colnames(dat) %in% c(Viva_primary_bin, Viva_secondary_bin))])) {
  val_lab(dat[, var_name]) = c("No" = 0, "Yes" = 1)
  
  dat[[var_name]] <- factor(dat[[var_name]], label = c("No", "Yes"))
}

### Ordinal/categorical
for (var_name in colnames(dat[, which(colnames(dat) %in% Viva_secondary_cat)])) {
  val_lab(dat[, var_name]) = c(
    "0 to <1 month" = 0,
    "1 to <3 months" = 1,
    "3 to <6 months" = 2,
    "≥6 months" = 3
  )  # From Brion et al., 2011, IJE
  
  dat[[var_name]] <- factor(
    dat[[var_name]],
    label = c("0 to <1 month", "1 to <3 months", "3 to <6 months", "≥6 months"),
    ordered = T
  )
}

## Label outcome variables
for (var_name in c(Viva_primary_bin,
                   Viva_secondary_bin,
                   Viva_secondary_con,
                   Viva_secondary_cat))
{
  var_lab(dat[, var_name]) <-
    MRPREG_outcome_labels$label[which(MRPREG_outcome_labels$varname == var_name)]
}

## Sanity check
dat_check <- dat[, which(
  colnames(dat) %in% c(
    Viva_primary_bin,
    Viva_secondary_bin,
    Viva_secondary_con,
    Viva_secondary_cat
  )
)]

# View(dat_check)
str(dat_check)
str(dat$bf_dur_4c)

#------------------------------------------------------------------------------#
#                                 Check & Save                                 #----
#------------------------------------------------------------------------------#

head(dat)
dim(dat)  # 1872  609

saveRDS(dat, "data/Viva/dat_exp_cov_out.rds")
