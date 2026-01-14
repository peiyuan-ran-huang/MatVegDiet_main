################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - BiB        #
################################################################################

# Last edited date: 31-Jul-2024
# This script is to derive MR-PREG covariates in BiB.

# ADAPTED FROM: "BiB_covs.R" written by the MR-PREG team

################################################################################

###BiB covs###
## NM beta version ##
## To be checked and confirmed ## 

## Vars chosen according to
## 'covariables_definition' in MRPREG/phenotypes/
# Characteristics::
# Maternal age (years) ✓
# Education ✓
# BMI (kg/m2) ✓
# Parity ✓
# Ethnicity ✓
# Smoking status ✓
# Alcohol status ✓
# Sex ✓

## Update from GC 290623
## Cov names 
# mat_age_years
# mat_edu_cat3
# mat_edu_b_uni
# mat_bmi_kgm2
# parity_cat5
# parity_b_multi
# smoke_b_yes
# alcohol_b
# sex_b_male

## Update from AT 150923
# matage_yrs
# matedu_cat3
# matedu_b_uni
# matbmi_kgm2
# parity_cat5
# parity_births
# parity_b_multi
# matethnic_b_other
# smoke_b_any
# smoke_cat3
# alcohol_b_any
# alcohol_cat3
# sex_b_male

rm(list=ls())
setwd("Z:/working/data/BiB")
# load("env_master.RData")
#Loading all packages using pacman
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(plyr, readstata13, dplyr,haven,foreign,tidyverse,vtable,writexl,tictoc)

## Issue with AT 020523 - loading pkgs - manual load

library(dplyr)
library(readstata13)
library(haven)
library(foreign)
library(tidyverse)
library(vtable)
library(writexl)
library(tictoc)

#load paths.R file where all paths are defined
source("paths.R")

#data path 
Sys.setenv(datadir=BiB_latest_dir)

#Bib cohort info person info
persinfo <- as_tibble(read_dta(paste0(Sys.getenv('datadir'),personinfo_dir)))
persinfo$BiBPregNumber<-as.double(persinfo$BiBPregNumber)
#Distribution
persinfo%>%
  group_by(ParticipantType)%>%
  dplyr::summarise(n=n())

# ParticipantType  freq
# 1           Child 13858
# 2          Father  3353
# 3          Mother 12453

persinfo <- persinfo %>% select(BiBPersonID,
                                BiBPregNumber,
                                ParticipantType,
                                Gender,
                                BiBMotherID)

#pregnancy info
preginfo_all <- as_tibble(read_dta(paste0(Sys.getenv('datadir'),preginfo_dir)))
#select needed variables
preginfo_all <- preginfo_all %>% select(BiBPersonID,
                                        BiBPregNumber,
                                        PregnancyID,
                                        adminpnbirths)

#maternal baseline questionnaire all pregnancies 
mbqall<-as_tibble(read_dta(paste0(Sys.getenv("datadir"),mbqall_dir)))
#restrict to covs
mbqall <- mbqall %>% select(BiBPersonID,
                            BiBPregNumber,
                            smk0smkprg,
                            smk0regsmk, 
                            agemy_mbqall,
                            edu0mumede,
                            edu0mumeuk,
                            mms0mbkbmi,
                            alc0drpreg,
                            alc0dr4thm,
                            alc0drfr3m,
                            imd_2007_quintile_nat)

#nrow=11395
#eclipse pregnancy record 
#parity
eclpreg<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),eclpreg_dir)))
eclpreg<- eclpreg%>% select(BiBPersonID,
                            BiBPregNumber,
                            eclregpart,
                            eclnregbrt
                            )

#eclipse neonatal record
# #baby sex
eclbaby<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),eclbaby_dir)))
eclbaby<-eclbaby %>%
  select(BiBPersonID,
         ChildID,
         eclbabysex)

#ethnicity
ethnic<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),ethnicity)))
ethnic <- ethnic %>% select(BiBPersonID,
                            deminfeth0eth3gp)
ethnic$BiBMotherID<-ethnic$BiBPersonID
ethnic$BiBPersonID<-NULL

#maternity records infant (birth outcomes, route of birth)
matrecinfant<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),matrecinfant_dir )))
matrecinfant<-matrecinfant %>%
  select(BiBPersonID,
         ChildID)
                            
#####Merge data tables ######

personKeys <- c("BiBPersonID","BiBPregNumber")
motherKeys <- c("BiBMotherID","BiBPregNumber")
bibdata<- merge(
          merge(
          merge(
          merge(
          merge(
          merge(eclbaby, matrecinfant, by=c("BiBPersonID","ChildID"), all =T),
          persinfo, by="BiBPersonID", all.x = T),
          ethnic, by="BiBMotherID", all.x = T),
          preginfo_all, by.y = personKeys, by.x = motherKeys, all=T), 
          eclpreg, by.x = motherKeys, by.y = personKeys, all=T), 
          mbqall, by.x = motherKeys, by.y = personKeys, all= T)

# ##bibdata_use is the tibble when the derived outcomes will be stored
# #keep just singletons(no twins) & just 1 random pregnancy is retained
set.seed(275)
bibdata_use<-bibdata %>%
  filter(adminpnbirths==1|eclnregbrt==1) %>%
  group_by(BiBMotherID) %>%
  slice_sample(., n=1)
#12103 of 17

rm(list=setdiff(ls(), "bibdata_use"))

###########Derive outcomes###################

bibdata_use <- bibdata_use %>% rename(parity_preg = eclregpart)
table(bibdata_use$parity_preg)

bibdata_use <- bibdata_use %>% mutate(parity_cat5 = ifelse(parity_preg >= 4, 4, parity_preg))
table(bibdata_use$parity_cat5)

bibdata_use <- bibdata_use %>%
  #create the variable
  mutate(
    parity_b_multi = case_when(
      parity_preg == 0 ~ 0, 
      parity_preg >= 1 ~ 1,
      TRUE ~ NA_real_
    ))

table(bibdata_use$parity_b_multi)
##0    1
##4715 6760 

table(bibdata_use$deminfeth0eth3gp)
# 1    2    3 
# 4018 4482 1555 

## check categorisations here
bibdata_use <- bibdata_use %>%
  mutate(
    mateth_b_other = case_when(
      deminfeth0eth3gp == 1 ~ 0,
      deminfeth0eth3gp == 2 ~ 1,
      deminfeth0eth3gp == 3 ~ 1
    ))

table(bibdata_use$mateth_b_other)
# 0    1 
# 4018 6037 

bibdata_use <- bibdata_use %>%
  #create the variable
  mutate(
    sex_b_male = case_when(
      eclbabysex == 1 ~ 1, 
      eclbabysex >= 2 ~ 0,
      TRUE ~ NA_real_
    ))

table(bibdata_use$sex_b_male)
# 0    1 
# 5735 6179

table(bibdata_use$smk0smkprg)
## 1    2
## 1693 8330

bibdata_use <- bibdata_use %>%
  #create the variable
  mutate(
    matsmoke_b_dur = case_when(
      smk0smkprg == 2 ~ 0, 
      smk0smkprg == 1 ~ 1,
      TRUE ~ NA_real_
    ))

table(bibdata_use$matsmoke_b_dur)
# 0    1 
# 8330 1693 

table(bibdata_use$alc0drpreg)
## 1    2     3
## 3155 6859  7
## Drank alcohol in preg or 3 months before
## 1 yes 2 no 3 refers to 'don't remember' - should we NA? Check
## The only variable that could be 'before' preg - OK to use?
## If so...

# bibdata_use <- bibdata_use %>%
#   #create the variable
#   mutate(
#     matalcohol_b_prior = case_when(
#       alc0drpreg == 1 ~ 1, 
#       alc0drpreg >= 2 ~ 2,
#       TRUE ~ NA_real_
#     ))

## Amy suggests using  alc0dr3mb4 alc0dr4thm & alc0drfr3m
## We dont want alc0dr3mb4 because that's the months prior preg, but perhaps can use the other 2
## Mother drank alcohol since 4th month of pregnancy
## Mother drank alcohol in the first 3 months of pregnancy

table(bibdata_use$alc0dr4thm)
# 1    2    3    4    5 
# 90  901  196 1931    3 
# Yes, once a week|Yes, occasionally|Yes, not specified|No|Don't remember

table(bibdata_use$alc0drfr3m)
# 1    2    3    4    5 
# 331  975  280 1559    7 
# Yes, once a week|Yes, occasionally|Yes, not specified|No|Don't remember

#bibdata_use <- bibdata_use %>%
  #create the variable
 # mutate(
  #  matalcohol_b_dur = case_when(
#      alc0drfr3m == 4 | alc0dr4thm == 4 ~ 0,
#      alc0drfr3m == 1 | alc0dr4thm == 1 ~ 1,
#      alc0drfr3m == 2 | alc0dr4thm == 2 ~ 1,
#      alc0drfr3m == 3 | alc0dr4thm == 3 ~ 1,
#      TRUE ~ NA_real_
#    ))

bibdata_use <- bibdata_use %>%
  #create the variable
  mutate(
    matalcohol_b_dur = case_when(
      alc0drpreg == 2 ~ 0,
      alc0drfr3m == 1 | alc0dr4thm == 1 ~ 1,
      alc0drfr3m == 2 | alc0dr4thm == 2 ~ 1,
      alc0drfr3m == 3 | alc0dr4thm == 3 ~ 1,
      TRUE ~ NA_real_
    ))


# 0    1 
# 6859  2047 

bibdata_use$matbmi_kgm2<-as.numeric(bibdata_use$mms0mbkbmi)
summary(bibdata_use$matbmi_kgm2)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  12.89   21.93   24.98   26.03   29.08   57.00    2719 

bibdata_use$matage_yrs<-as.numeric(bibdata_use$agemy_mbqall)
summary(bibdata_use$matage_yrs)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  14.00   23.00   27.00   27.36   31.00   49.00    2060 

## We want a categorical 3 variable for education low/med/high
## And a binary var for if uni or not 

## After update, we are going to leave edu as a categorical variable and then make a uni binary variable 

table(bibdata_use$edu0mumede)

## Have named as cat 7 not cat 3 - researchers can derive this variable differently if it suits them 

bibdata_use$mumedu_cat7<-as.numeric(bibdata_use$edu0mumede)

## What do we do with foreign quals in this var?

# table(bibdata_use$edu0mumeuk)
# 
# bibdata_use <- bibdata_use %>% mutate(edu0mumeuk = as.numeric(edu0mumeuk))

bibdata_use$matedu_b_uni<-ifelse(bibdata_use$edu0mumeuk==9|bibdata_use$edu0mumeuk==10,'1','0')

## IMD - can use for SES 

table(bibdata_use$imd_2007_quintile_nat)
bibdata_use$imd_cat<-as.numeric(bibdata_use$imd_2007_quintile_nat)

# select final vars 
bib_covs <- bibdata_use %>% select(BiBMotherID,BiBPregNumber,BiBPersonID,ChildID,sex_b_male,matsmoke_b_dur,matalcohol_b_dur,imd_cat,matedu_b_uni,parity_cat5,parity_preg,parity_b_multi,mateth_b_other,matbmi_kgm2,matage_yrs)
## 12103 of 13 
# save.image("covs.env.RData")
# write.table(bib_covs,file="BiB_covs_180923.txt",sep="\t",col.names=T,row.names=F,quote=F)

saveRDS(bib_covs, "dat_MRPREG_cov.rds")

## Summary table

#install.packages("gtsummary")
library(gtsummary)
#bib_covs %>% tbl_summary(sex_b_male,smoke_b_yes,alcohol_b,imd_cat,mat_b_edu_uni,parity_cat4,parity_cat10,parity_b_multi,ethnicity,mat_bmi_kgm2,mat_age_years)
library(ggplot2)
# hist
bib_covs %>% 
  pivot_longer(cols = c(8,10,11,14,15)) %>% 
  ggplot(aes(value)) + 
  geom_histogram() + 
  facet_wrap(~name, scales = "free")

## Edit 220424 restrict covs file to those in outcomes file

# load("env_master.RData")

# bib_covs <- data.table::fread("BiB_covs_180923.txt")

## Agreed in meeting 22 and 240424 to use final linked file for GWAS
## So using orig covs file 

summary(bib_covs$matage_yrs)
table(bib_covs$mateth_b_other, useNA = "always")
table(bib_covs$matedu_b_uni, useNA = "always")
table(bib_covs$imd_cat, useNA = "always")
table(bib_covs$parity_b_multi, useNA = "always")
table(bib_covs$matsmoke_b_dur, useNA = "always")
table(bib_covs$matalcohol_b_dur, useNA = "always")
summary(bib_covs$matbmi_kgm2)
table(bib_covs$sex_b_male, useNA = "always")
sd(bib_covs$matage_yrs, na.rm=TRUE)
sd(bib_covs$matbmi_kgm2, na.rm=TRUE)
