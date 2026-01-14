################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 26-Jun-2024
# This script is to derive MR-PREG outcome variables in ALSPAC (Part 1).

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


################################################################################################################################
################################################################################################################################
#     2. Load Dataset created from STATA by AS excluding mothers and children with consent withdrawals
################################################################################################################################
################################################################################################################################


alspac_raw <- read_dta("raw_exp_cov_out.dta")
alspac <- alspac_raw


################################################################################################################################
################################################################################################################################
#     3. New variables
################################################################################################################################
################################################################################################################################



################################################################################################################################
# 3.1 Mother variables
################################################################################################################################



#restrict to 1 mother entry: 
### unique=1 Restricted to mothers with data for only one pregnancy if contributing with more than one 
table(alspac$unique)
#     1      
# 15204   

alspac <- alspac[which(alspac$unique==1),]  # 15645 -> 15204
alspac <- alspac[which(alspac$singleton==1),]   # 15204 -> 14744

# most variables also require to restrict each pregnancy to only one baby (singleton=1) and live births (kz010>=2)

################################################################################################################################
### 3.1.1 Variables related to Miscarriages, stillbirth, pregnancy loss
################################################################################################################################

# Variables in DB previous/index pregnancy: mz011a, kz010, b007, b008, b009, b010, b011, b012

###B quest - 18gw

# b024 Outcome of last PREG
table(alspac$b024, useNA = "always")
#   value             label
# -7         HaB short
# -1           Missing
# 0  No previous PREG
# 1       Miscarriage
# 2       Termination
# 3        Stillbirth
# 4  Liveborn CH died
# 5    CH still alive
# 6             Other

alspac$b024 <- ifelse(alspac$b024<0,
                      NA,
                      alspac$b024)
alspac$b024mc <- ifelse(alspac$b024==1,
                        1,
                        0)
alspac$b024mc <- as.factor(alspac$b024mc)

alspac$b024sb <- ifelse(alspac$b024==3,
                        1,
                        0)
alspac$b024sb <- as.factor(alspac$b024sb)

alspac$b024ab <- ifelse(alspac$b024==2,
                        1,
                        0)
alspac$b024ab <- as.factor(alspac$b024ab)

# b007 Miscarriages
table(alspac$b007, useNA = "always")
#   value             label
# -7         HaB short
# -1           Missing
# 1                 Y
# 2                 N

alspac$b007 <- ifelse(alspac$b007<0,
                      NA,
                      ifelse(alspac$b007==1,
                             1,
                             0))
alspac$b007 <- as.factor(alspac$b007)

#b008 Nb of miscarriages - refer to whether the mum experienced any miscarriage
table(alspac$b008, useNA = "always")
#   value             label
# -7         HaB short
# -1           Missing
# 0              none

alspac$b008 <- ifelse(alspac$b008<0,
                      NA,
                      alspac$b008)
alspac$b008 <- as.character(alspac$b008)

# b011  Stillbirths, b012 NO of stillbirths
table(alspac$b011, useNA = "always")
#   value             label
# -7         HaB short
# -1           Missing
# 1                 Y
# 2                 N

alspac$b011 <- ifelse(alspac$b011<0,
                      NA,
                      ifelse(alspac$b011==1,
                             1,
                             0))
alspac$b011 <- as.factor(alspac$b011)

table(alspac$b012, useNA = "always")
#   value             label
# -7         HaB short
# -1           Missing
# 0              none

alspac$b012 <- ifelse(alspac$b012<0,
                      NA,
                      alspac$b012)
alspac$b012 <- as.factor(alspac$b012)

# b009 Abortions or terminations, b010 NO of abortions or terminations
table(alspac$b009, useNA = "always")
#   value             label
# -7         HaB short
# -1           Missing
# 1                 Y
# 2                 N

alspac$b009 <- ifelse(alspac$b009<0,
                      NA,
                      ifelse(alspac$b009==1,
                             1,
                             0))
alspac$b009 <- as.factor(alspac$b009)

table(alspac$b010, useNA = "always")
#   value             label
# -7         HaB short
# -1           Missing
# 0              none

alspac$b010 <- ifelse(alspac$b010<0,
                      NA,
                      alspac$b010)
alspac$b010 <- as.character(alspac$b010) #to be able to use conditions of >=1


###index pregnancy

# mz011a Miscarriage < 20 wks - refers to the index pregnancy
table(alspac$mz011a, useNA = "always")
# 1                 yes
# 2                  no

alspac$mz011a <- ifelse(alspac$mz011a<0,
                        NA,
                        ifelse(alspac$mz011a==1,
                               1,
                               0))
alspac$mz011a <- as.factor(alspac$mz011a)

# kz010 Outcome A - refers to the index pregnancy
table(alspac$kz010, useNA = "always")
# value                  label
# 0     fetal loss <20 wks
# 1 fetal death/sb 20+ wks
# 2       nn death <7 days
# 3     nn death 7-27 days
# 4  post nn death 28-1 yr
# 7               survivor

alspac$kz010mc <- ifelse(alspac$kz010==0,
                         1,
                         0)
alspac$kz010mc <- as.factor(alspac$kz010mc)

alspac$kz010sb <- ifelse(alspac$kz010==1,
                         1,
                         0)
alspac$kz010sb <- as.factor(alspac$kz010sb)



###Menarche

#d010 Age when periods started , D010A Age when periods started(recoded)
table(alspac$d010, useNA = "always")
#   value             label
# -1         Missing
# 77         Never had

table(alspac$d010a, useNA = "always")
# value                label
# -1            Missing
# 1             8-11 YRS
# 2            12-14 YRS
# 3 15 YRS+ or never had



# Whole sample - a #############################################################################################################
################################################################################################################################



# 3.1.1.1 Miscarriages
################################################################################################################################

#According to the outcome definition file re. ALSPAC, base these variables on the questionnaires sent at 18-20w gestation
#mz refers to the index pregnancy and we cannot differentiate between types of miscarriages if we only include this one
#The miscarriage variable used by Ana considers any miscarriage the mother could have had also years after the pregnancy
#In the last meeting in 2021 we said to use history of miscarriage prior to index and the index pregnancy outcome

# Exclude multiple births (only alspac$mz010a==1 or alspac$singleton==1)

#3.1.1.1.1.a Cases miscarriages (< 22 completed weeks of gestation), controls none - update exclusion of multiple

#index pregnancy
alspac$misc_index_all <- case_when(alspac$unique==1 & #unique mothers
                                     alspac$singleton==1 & #non multiple births
                                   (alspac$mz011a==1 | #Miscarriage < 20 wks==1
                                      alspac$kz010==0) ~1, #fetal loss <20 wks
                                   
                                   alspac$unique==1 & #unique mothers
                                     alspac$singleton==1 & #non multiple births
                                     (alspac$mz011a==0 & #non Miscarriage < 20 wks==1
                                        alspac$kz010!=0) ~0, #non fetal loss <20 wks
                                   
                                   TRUE ~ NA_real_)
alspac$misc_index_all <- as.factor(alspac$misc_index_all)
summary(alspac$misc_index_all)
#check the distribution is the expected 
length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$mz011a==1 | alspac$kz010==0)))
length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$mz011a==0 & alspac$kz010!=0 )))
dim(alspac)[1]-
  length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$mz011a==1 | alspac$kz010==0))) -
  length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$mz011a==0 & alspac$kz010!=0 )))

table(alspac$unique,alspac$misc_index_all, useNA = "always")
table(alspac$singleton,alspac$misc_index_all, useNA = "always")
table(alspac$mz011a,alspac$kz010, useNA = "always")

#b quest - prior to index pregnancy
alspac$miscb <- case_when(alspac$unique==1 & #unique mothers
                                     alspac$singleton==1 & #non multiple births
                                     (alspac$b024==1 | #Outcome of PREG before index was mc
                                        alspac$b007==1) ~1, #some mc before index 
                                   
                                   alspac$unique==1 & #unique mothers
                                     alspac$singleton==1 & #non multiple births
                            (alspac$b024!=1 & #no Outcome of PREG before index was mc
                               alspac$b007==0) ~ 0, #no mc before index 
                                   
                                   TRUE ~ NA_real_)
alspac$miscb <- as.factor(alspac$miscb)
summary(alspac$miscb)
#check the distribution is the expected 
length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$b024==1 | alspac$b007==1)))
length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$b024!=1 &alspac$b007==0 )))
dim(alspac)[1]-
  length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$b024==1 | alspac$b007==1))) -
  length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$b024!=1 & alspac$b007==0 )))

table(alspac$unique,alspac$miscb, useNA = "always")
table(alspac$singleton,alspac$miscb, useNA = "always")
table(alspac$b024,alspac$b007, useNA = "always")

#combine miscarriages from the pregnancies before and index preg 
alspac$misc_all <- ifelse(alspac$misc_index_all==1 | 
                              alspac$miscb==1,
                          1,
                          0)
alspac$misc_all <- as.factor(alspac$misc_all)
summary(alspac$misc_all)
#check the distribution is the expected 
length(which(alspac$misc_index_all==1 | alspac$miscb==1))
length(which(alspac$misc_index_all==0 & alspac$miscb==0))
dim(alspac)[1]-
  length(which(alspac$misc_index_all==1 | alspac$miscb==1)) -
  length(which(alspac$misc_index_all==0 & alspac$miscb==0))

table(alspac$misc_index_all,alspac$miscb, useNA = "always")



#3.1.1.1.2.a Cases sporadic miscarriages (<3), controls none

alspac$s_misc_all <- ifelse(alspac$misc_all==0,
                            0,
                            ifelse((alspac$b008>0 &
                                      alspac$b008<3) |
                                     (alspac$misc_index_all==1 & 
                                        alspac$b008>=0 &
                                        alspac$b008<2) |
                                     (alspac$misc_index_all==1 & 
                                        is.na(alspac$b008)),
                                   1,
                                   NA))

alspac$s_misc_all <- as.factor(alspac$s_misc_all)
summary(alspac$s_misc_all)
#check the distribution is the expected 
length(which(alspac$misc_all==0))
length(which(alspac$misc_all!=0 & ((alspac$b008>0 & alspac$b008<3) |
               (alspac$misc_index_all==1 &  alspac$b008>=0 & alspac$b008<2) |
               (alspac$misc_index_all==1 & is.na(alspac$b008)))))
dim(alspac)[1]-
  length(which(alspac$misc_all==0)) -
  length(which(alspac$misc_all!=0 & ((alspac$b008>0 & alspac$b008<3) |
                                       (alspac$misc_index_all==1 &  alspac$b008>=0 & alspac$b008<2) |
                                       (alspac$misc_index_all==1 & is.na(alspac$b008)))))



#3.1.1.1.3.a Cases recurrent (>2) miscarriages, controls none

alspac$r_misc_all <- ifelse(alspac$misc_all==0,
                            0,
                            ifelse(alspac$b008>2 |
                                     (alspac$misc_index_all==1 & 
                                        alspac$b008>=2),
                                   1,
                                   NA))
alspac$r_misc_all <- as.factor(alspac$r_misc_all)
summary(alspac$r_misc_all)
#check the distribution is the expected 
length(which(alspac$misc_all==0))
length(which(alspac$misc_all!=0 & (alspac$b008>2 | (alspac$misc_index_all==1 &  alspac$b008>=2))))
dim(alspac)[1]-
  length(which(alspac$misc_all==0)) -
  length(which(alspac$misc_all!=0 & (alspac$b008>2 | (alspac$misc_index_all==1 &  alspac$b008>=2))))



#3.1.1.2 Stillbirths
################################################################################################################################



#3.1.1.2.a Cases stillbirths (>= 22 completed weeks of gestation), controls none 

#SB for index pregnancy
alspac$sb_index_all <- case_when(alspac$unique==1 & #unique mothers
                                     alspac$singleton==1 & #non multiple births
                                   alspac$kz010==1 ~1, #fetal death/sb 20+ wks
                                   
                                   alspac$unique==1 & #unique mothers
                                     alspac$singleton==1 & #non multiple births
                                        alspac$kz010!=1 ~0, #non fetal death/sb 20+ wks
                                   
                                   TRUE ~ NA_real_)
alspac$sb_index_all <- as.factor(alspac$sb_index_all)
summary(alspac$sb_index_all)
#check the distribution is the expected 
length(which(alspac$unique==1 & alspac$singleton==1 & alspac$kz010==1))
length(which(alspac$unique==1 & alspac$singleton==1 & alspac$kz010!=1))
dim(alspac)[1]-
  length(which(alspac$unique==1 & alspac$singleton==1 & alspac$kz010==1)) -
  length(which(alspac$unique==1 & alspac$singleton==1 & alspac$kz010!=1))

table(alspac$unique,alspac$kz010, useNA = "always")
table(alspac$singleton,alspac$kz010, useNA = "always")

#b quest - prior to index pregnancy
alspac$sbb <- case_when(alspac$unique==1 & #unique mothers
                            alspac$singleton==1 & #non multiple births
                            (alspac$b024==3 | #Outcome of PREG before index was sb
                               alspac$b011==1) ~1, #some sb before index 
                          
                          alspac$unique==1 & #unique mothers
                            alspac$singleton==1 & #non multiple births
                            (alspac$b024!=3 & #no Outcome of PREG before index was sb
                               alspac$b011==0) ~ 0, #no sb before index 
                          
                          TRUE ~ NA_real_)
alspac$sbb <- as.factor(alspac$sbb)
summary(alspac$sbb)
#check the distribution is the expected 
length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$b024==3 | alspac$b011==1)))
length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$b024!=3 &alspac$b011==0 )))
dim(alspac)[1]-
  length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$b024==3 | alspac$b011==1))) -
  length(which(alspac$unique==1 & alspac$singleton==1 & (alspac$b024!=3 & alspac$b011==0 )))

table(alspac$unique,alspac$sbb, useNA = "always")
table(alspac$singleton,alspac$sbb, useNA = "always")
table(alspac$b024,alspac$b011, useNA = "always")

#combine miscarriages from the pregnancies before and index preg 
alspac$sb_all <- ifelse(alspac$sb_index_all==1 | 
                            alspac$sbb==1,
                          1,
                          0)
alspac$sb_all <- as.factor(alspac$sb_all)
summary(alspac$sb_all)
#check the distribution is the expected 
length(which(alspac$sb_index_all==1 | alspac$sbb==1))
length(which(alspac$sb_index_all==0 & alspac$sbb==0))
dim(alspac)[1]-
  length(which(alspac$sb_index_all==1 | alspac$sbb==1)) -
  length(which(alspac$sb_index_all==0 & alspac$sbb==0))

table(alspac$sb_index_all,alspac$sbb, useNA = "always")



#3.1.1.3 pregnancy loss
################################################################################################################################

alspac$pregloss <- case_when(alspac$misc_all==1 |
                               alspac$sb_all==1 ~ 1,
                             alspac$misc_all==0 &
                               alspac$sb_all==0 ~ 0,
                             TRUE ~ NA_real_)
alspac$pregloss <- as.factor(alspac$pregloss)
summary(alspac$pregloss)
#check the distribution is the expected 
length(which(alspac$misc_all==1 | alspac$sb_all==1))
length(which(alspac$misc_all==0 & alspac$sb_all==0))
dim(alspac)[1]-
  length(which(alspac$misc_all==1 | alspac$sb_all==1)) -
  length(which(alspac$misc_all==0 & alspac$sb_all==0))

table(alspac$misc_all,alspac$sb_all, useNA = "always")



# Subsamples excluding other pregnancy losses - b #############################################################################################################
################################################################################################################################



# 3.1.1.1 Miscarriages
################################################################################################################################

# 3.1.1.1.1.b Cases miscarriages, controls no pregnancy loss

alspac$misc_subsamp <-  case_when(alspac$misc_all==1 ~1,
                                  alspac$misc_all==0 &
                                    alspac$sb_all==0 ~0,
                                  TRUE~ NA_real_)
alspac$misc_subsamp <- as.factor(alspac$misc_subsamp)
summary(alspac$misc_subsamp)
#check the distribution is the expected 
length(which(alspac$misc_all==1))
length(which(alspac$misc_all==0 & alspac$sb_all==0))
dim(alspac)[1]-
  length(which(alspac$misc_all==1 | alspac$sb_all==1)) -
  length(which(alspac$misc_all==0 & alspac$sb_all==0))


#Only considering index pregnancy for sensitivity analyses in projects such as Ran's on coffee consumption
alspac$misc_i_subsamp <-  case_when(alspac$misc_index_all==1 ~1,
                                  alspac$misc_index_all==0 &
                                    alspac$sb_index_all==0 ~0,
                                  TRUE~ NA_real_)
alspac$misc_i_subsamp <- as.factor(alspac$misc_i_subsamp)
summary(alspac$misc_i_subsamp)
#check the distribution is the expected 
length(which(alspac$misc_index_all==1))
length(which(alspac$misc_index_all==0 & alspac$sb_index_all==0))
dim(alspac)[1]-
  length(which(alspac$misc_index_all==1)) -
  length(which(alspac$misc_index_all==0 & alspac$sb_index_all==0))



# 3.1.1.1.2.b Cases sporadic miscarriages, controls no pregnancy loss, also excluding age of menarche <9 and >17, appendix ICD???

alspac$s_misc_subsamp <-  case_when(alspac$s_misc_all==1 &
                                      alspac$d010>=9 & # exclude menarche <9yo
                                      alspac$d010<=17 ~1, # exclude menarche >17yo
                                  alspac$misc_all==0 &
                                    alspac$sb_all==0 ~0,
                                  TRUE~ NA_real_)
alspac$s_misc_subsamp <- as.factor(alspac$s_misc_subsamp)
summary(alspac$s_misc_subsamp)
#check the distribution is the expected 
length(which(alspac$s_misc_all==1 & alspac$d010>=9 &alspac$d010<=17))
length(which(alspac$misc_all==0 & alspac$sb_all==0))
dim(alspac)[1]-
  length(which(alspac$s_misc_all==1 & alspac$d010>=9 &alspac$d010<=17)) -
length(which(alspac$misc_all==0 & alspac$sb_all==0))



# 3.1.1.1.3.b Cases recurrent miscarriages, controls no pregnancy loss, also excluding age of menarche <9 and >17, appendix ICD???

alspac$r_misc_subsamp <-  case_when(alspac$r_misc_all==1 &
                                      alspac$d010>=9 & # exclude menarche <9yo
                                      alspac$d010<=17 ~1, # exclude menarche >17yo
                                    alspac$misc_all==0 &
                                      alspac$sb_all==0 ~0,
                                    TRUE~ NA_real_)
alspac$r_misc_subsamp <- as.factor(alspac$r_misc_subsamp)
summary(alspac$r_misc_subsamp)
#check the distribution is the expected 
length(which(alspac$r_misc_all==1 & alspac$d010>=9 &alspac$d010<=17))
length(which(alspac$misc_all==0 & alspac$sb_all==0))
dim(alspac)[1]-
  length(which(alspac$r_misc_all==1 & alspac$d010>=9 &alspac$d010<=17)) -
  length(which(alspac$misc_all==0 & alspac$sb_all==0))



#3.1.1.2.b Cases stillbirths, controls no pregnancy loss 

alspac$sb_subsamp <-  case_when(alspac$sb_all==1 ~1,
                                  alspac$misc_all==0 &
                                    alspac$sb_all==0 ~0,
                                  TRUE~ NA_real_)
alspac$sb_subsamp <- as.factor(alspac$sb_subsamp)
summary(alspac$sb_subsamp)
#check the distribution is the expected 
length(which(alspac$sb_all==1))
length(which(alspac$misc_all==0 & alspac$sb_all==0))
dim(alspac)[1]-
  length(which(alspac$sb_all==1)) -
  length(which(alspac$misc_all==0 & alspac$sb_all==0))



#Only considering index pregnancy for sensitivity analyses in projects such as Ran's on coffee consumption
alspac$sb_i_subsamp <-  case_when(alspac$sb_index_all==1 ~1,
                                    alspac$misc_index_all==0 &
                                      alspac$sb_index_all==0 ~0,
                                    TRUE~ NA_real_)
alspac$sb_i_subsamp <- as.factor(alspac$sb_i_subsamp)
summary(alspac$sb_i_subsamp)
#check the distribution is the expected 
length(which(alspac$sb_index_all==1))
length(which(alspac$sb_index_all==0 & alspac$misc_index_all==0))
dim(alspac)[1]-
  length(which(alspac$sb_index_all==1)) -
  length(which(alspac$sb_index_all==0 & alspac$misc_index_all==0))





################################################################################################################################
### 3.1.2 Blood Pressure-related variables
################################################################################################################################

# Variables in DB: HDP, preeclampsia, gesthyp, prev_hyp, proturi
# HDP - High blood pressure after 20 weeks without known hypertension prior to pregnancy
table(alspac$HDP, useNA="always")

# preeclampsia - without known hypertension prior to pregnancy
table(alspac$preeclampsia, useNA="always")

# gesthyp - Gestational hypertension
table(alspac$gesthyp, useNA="always")

# prev_hyp - Previously been diagnosed with hypertension outside of pregnancy
table(alspac$prev_hyp, useNA="always")

# proturi - Proteinuria (1+ or more) after 20 weeks gestation
table(alspac$proturi, useNA="always")



# 3.1.2.1 Hypertensive disorders of pregnancy (combines preeclampsia and gestational hypertension)
################################################################################################################################

# 3.1.2.1.a Cases HDP (preeclampsia + gesthyp), controls no HDP

alspac$hdp_all <- case_when(alspac$unique==1 & #unique mothers
                              alspac$singleton==1 & #non multiple births
                              alspac$kz010>=2 & #live births
                              alspac$HDP==1 ~1,
                    
                            alspac$unique==1 & #unique mothers
                              alspac$singleton==1 & #non multiple births
                              alspac$kz010>=2 & #live births
                              alspac$HDP==0 ~ 0, 
                            
                            TRUE ~ NA_real_)

alspac$hdp_all <- as.factor(alspac$hdp_all)
summary(alspac$hdp_all)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$HDP==1))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$HDP==0))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$HDP==1)) -
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$HDP==0))



# 3.1.2.1.b Subsample excluding women with pre-existing hypertension (i.e. essential/chronic hypertension); controls no HDP or pre-existing HTN 

alspac$hdp_subsamp <- case_when(alspac$hdp_all==1 &
                               (alspac$prev_hyp==0 |
                                  is.na(alspac$prev_hyp)) ~ 1,
                               alspac$hdp_all==0 &
                                 (alspac$prev_hyp==0 |
                                    is.na(alspac$prev_hyp)) ~ 0,
                             TRUE ~ NA_real_)
alspac$hdp_subsamp <- as.factor(alspac$hdp_subsamp)
summary(alspac$hdp_subsamp)
#check the distribution is the expected 
length(which(alspac$hdp_all==1 & (alspac$prev_hyp==0 |
                                    is.na(alspac$prev_hyp))))
length(which(alspac$hdp_all==0 & (alspac$prev_hyp==0 |
                                    is.na(alspac$prev_hyp))))
dim(alspac)[1]-
  length(which(alspac$hdp_all==1 & (alspac$prev_hyp==0 |
                                      is.na(alspac$prev_hyp))))-
length(which(alspac$hdp_all==0 & (alspac$prev_hyp==0 |
                                    is.na(alspac$prev_hyp))))



# 3.1.2.2 Gestational HTN
################################################################################################################################

# 3.1.2.2.a Cases gestational hypertension (Hypertension diagnosed after 20 weeks' gestation in the absence of proteinuria), controls no HDP, exclude women with preeclampsia

alspac$gh_all <- case_when(alspac$unique==1 & #unique mothers
                             alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$gesthyp==1 ~1,
                           
                           alspac$unique==1 & #unique mothers
                             alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$hdp_all==0 ~ 0, 
                           
                           TRUE ~ NA_real_)
alspac$gh_all <- as.factor(alspac$gh_all)
summary(alspac$gh_all)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$gesthyp==1))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$hdp_all==0))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$gesthyp==1)) -
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$hdp_all==0))



# 3.1.2.2.b Subsample excluding women with preeclampsia or pre-existing hypertension (i.e. essential/chronic hypertension); controls no HDP or pre-existing HTN 

alspac$gh_subsamp <- case_when(alspac$gh_all==1 &
                                 (alspac$prev_hyp==0 |
                                    is.na(alspac$prev_hyp)) ~ 1,
                               alspac$gh_all==0 &
                                 (alspac$prev_hyp==0 |
                                    is.na(alspac$prev_hyp)) ~ 0,
                               TRUE ~ NA_real_)

alspac$gh_subsamp <- as.factor(alspac$gh_subsamp)
summary(alspac$gh_subsamp) 
#check the distribution is the expected 
length(which(alspac$gh_all==1 & (alspac$prev_hyp==0 |
                                   is.na(alspac$prev_hyp))))
length(which(alspac$gh_all==0 & (alspac$prev_hyp==0 |
                                   is.na(alspac$prev_hyp))))
dim(alspac)[1]-
  length(which(alspac$gh_all==1 & (alspac$prev_hyp==0 |
                                     is.na(alspac$prev_hyp))))-
  length(which(alspac$gh_all==0 & (alspac$prev_hyp==0 |
                                     is.na(alspac$prev_hyp))))



### 3.1.2.3 Preeclampsia
################################################################################################################################

# 3.1.2.3.a Cases preeclampsia (Hypertension diagnosed after 20 weeks' gestation in the presence of proteinuria), controls no preeclampsia

alspac$pe_all <- case_when(alspac$unique==1 & #unique mothers
                             alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$preeclampsia==1 ~1,
                           
                           alspac$unique==1 & #unique mothers
                             alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$hdp_all==0 ~ 0, 
                           
                           TRUE ~ NA_real_)

alspac$pe_all <- as.factor(alspac$pe_all)
summary(alspac$pe_all)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$preeclampsia==1))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$hdp_all==0))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$preeclampsia==1)) -
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$hdp_all==0))



# 3.1.2.3.b Subsample excluding women with HDP or pre-existing hypertension (i.e. essential/chronic hypertension); controls no HDP or pre-existing HTN 

alspac$pe_subsamp <- case_when(alspac$pe_all==1 &
                                 (alspac$prev_hyp==0 |
                                    is.na(alspac$prev_hyp)) ~ 1,
                               alspac$pe_all==0 &
                                 (alspac$prev_hyp==0 |
                                    is.na(alspac$prev_hyp)) ~ 0,
                               TRUE ~ NA_real_)

alspac$pe_subsamp <- as.factor(alspac$pe_subsamp)
summary(alspac$pe_subsamp)
#check the distribution is the expected 
length(which(alspac$pe_all==1 & (alspac$prev_hyp==0 |
                                   is.na(alspac$prev_hyp))))
length(which(alspac$pe_all==0 & (alspac$prev_hyp==0 |
                                   is.na(alspac$prev_hyp))))
dim(alspac)[1]-
  length(which(alspac$pe_all==1 & (alspac$prev_hyp==0 |
                                     is.na(alspac$prev_hyp))))-
  length(which(alspac$pe_all==0 & (alspac$prev_hyp==0 |
                                     is.na(alspac$prev_hyp))))





################################################################################################################################
### 3.1.3 Gestational diabetes
################################################################################################################################

#Variables in DB: pregnancy_diabetes

#pregnancy_diabetes - Diabetes in pregnancy
table(alspac$pregnancy_diabetes, useNA="always")
#              0                   1                      2            3    
#"No glycosuria or diabetes" "Existing diabetes" "Gestational diabetes" "Glycosuria"
# There is no information on the types of diabetes (e.g. gestational diabetes) other than whether or not she had diabetes during pregnancy. Further information on the types and treatment for diabetes can be found in the detailed questionnaires of the mothers. Measures of glycosuria are also available in the antenatal files of Lawlor.



# 3.1.3.a Cases gestational diabetes, controls no gestational diabetes
#If I consider codes 0, 1 and 3 as no gestational diabetes (controls), deriving the variable in the subsample so that controls are those with no gestational diabetes or existing diabetes, we end up with the same variable (55 cases, 11802 controls)
#So I define the controls for the variable in the whole sample as those with no diabetes during pregnancy
alspac$gdm_all <- case_when(alspac$unique==1 & #unique mothers
                           alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$pregnancy_diabetes==2 ~1,
                           
                           alspac$unique==1 & #unique mothers
                             alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$pregnancy_diabetes!=2 ~ 0, 
                           
                           TRUE ~ NA_real_)

alspac$gdm_all <- as.factor(alspac$gdm_all)
summary(alspac$gdm_all)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$pregnancy_diabetes==2))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$pregnancy_diabetes!=2))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$pregnancy_diabetes==2)) -
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$pregnancy_diabetes!=2))



# 3.1.3.b Subsample excluding women with pre-existing diabetes from controls

alspac$gdm_subsamp <- case_when(alspac$unique==1 & #unique mothers
                              alspac$singleton==1 & #non multiple births
                              alspac$kz010>=2 & #live births
                              alspac$pregnancy_diabetes==2 ~1,
                            
                            alspac$unique==1 & #unique mothers
                              alspac$singleton==1 & #non multiple births
                              alspac$kz010>=2 & #live births
                              (alspac$pregnancy_diabetes!=2 &
                                 alspac$pregnancy_diabetes!=1) ~ 0, 
                            
                            TRUE ~ NA_real_)

alspac$gdm_subsamp <- as.factor(alspac$gdm_subsamp)
summary(alspac$gdm_subsamp)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$pregnancy_diabetes==2))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  (alspac$pregnancy_diabetes!=2&alspac$pregnancy_diabetes!=1)))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$pregnancy_diabetes==2)) -
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  (alspac$pregnancy_diabetes!=2&alspac$pregnancy_diabetes!=1)))





################################################################################################################################
### 3.1.4 Anaemia-related variables 
################################################################################################################################

##Variables in DB: DEL_P1060, DEL_P1427, DEL_P1428, v1dad1a1_anaemia, v1dad1a2_result_anaemia, ?DEL_P1047?

#DEL_P1060 B6c: Anaemia noted during pregnancy before onset of labour #Hb<10g/dl 
table(alspac$DEL_P1060, useNA="always")
#   value             label
# -10     Not completed
# 1               Yes
# 2                No
# Here it was recorded whether anaemia was diagnosed (but the DAs were told to only code 'yes' if there was evidence that Hb<10g/dl)

#DEL_P1427 D1a Anaemia postpartum #Hb<10g/dl 
table(alspac$DEL_P1427, useNA="always")
# value             label
# -1      Not Answered
# 1               Yes
# 2                No

# DEL_P1428 D1arslt: Hb level postpartum of anaemic women
# value             label
# -10     Not completed
# 1                Yes
# 2                No

# v1dad1a1_anaemia Anaemia noted to have occurred during the first 14 days postpartum
table(alspac$v1dad1a1_anaemia, useNA="always")
# value                         label
# 1                           Yes
# 2                            No

# v1dad1a2_result_anaemia Anaemia result #when anaemia postpartum noted (This variable only has data in women where a diagnosis of anaemia - post partum
# was noted in the notes and it is the corresponding haemoglobin value written in the notes at the time that that diagnosis was made in the notes.
table(alspac$v1dad1a2_result_anaemia, useNA="always")

# DEL_P1047 First haemoglobin measurement (if this was before 18 weeks) - next measurements in appendix F according to OB file



# 3.1.4.1 Anaemia pre-pregnancy
################################################################################################################################

# Cases with maternal anaemia before pregnancy, Controls no maternal anaemia before pregnancy - No variable found in ALSPAC



# 3.1.4.2 Anaemia during pregnancy
################################################################################################################################

# 3.1.4.2.a Cases anaemia during pregnancy, controls no anaemia during pregnancy

alspac$anaemia_preg_all <- case_when(alspac$unique==1 & #unique mothers
                                       alspac$singleton==1 & #non multiple births
                                       alspac$kz010>=2 & #live births
                                       alspac$DEL_P1060==1 ~ 1,
                                     
                                     alspac$unique==1 & #unique mothers
                                       alspac$singleton==1 & #non multiple births
                                       alspac$kz010>=2 & #live births
                                       alspac$DEL_P1060==2 ~0,
                                     
                                     TRUE ~ NA_real_) 

alspac$anaemia_preg_all <- as.factor(alspac$anaemia_preg_all)
summary(alspac$anaemia_preg_all)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1060==1))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1060==2))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1060==1)) -
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1060==2))



# 3.1.4.2.b Subsample excluding women with pre-existing anaemia - cannot be derived due to lack of pre-existing anaemia



# 3.1.4.3 Anaemia post-pregnancy
################################################################################################################################

# 3.1.4.3.a Cases anaemia post-pregnancy, controls no anaemia post-pregnancy

alspac$anaemia_post_all <- case_when(alspac$unique==1 & #unique mothers
                                       alspac$singleton==1 & #non multiple births
                                       alspac$kz010>=2 & #live births
                                       (alspac$DEL_P1427==1 |
                                          alspac$v1dad1a1_anaemia==1)~ 1,
                                     
                                     alspac$unique==1 & #unique mothers
                                       alspac$singleton==1 & #non multiple births
                                       alspac$kz010>=2 & #live births
                                       alspac$DEL_P1427==2 |
                                       alspac$v1dad1a1_anaemia==2 ~0,
                                     
                                     TRUE ~ NA_real_)
  
alspac$anaemia_post_all <- as.factor(alspac$anaemia_post_all)
summary(alspac$anaemia_post_all)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  (alspac$DEL_P1427==1|alspac$v1dad1a1_anaemia==1)))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1427==2&alspac$v1dad1a1_anaemia==2 ))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  (alspac$DEL_P1427==1|alspac$v1dad1a1_anaemia==1))) -
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1427==2&alspac$v1dad1a1_anaemia==2 ))

# Note 1: I could have used also the continuous variables DEL_P1428 and 1dad1a2_result_anaemia but the categorical were already defined by DAs using a threshold of 10g/dl
# Note 2: I have not excluded those cases that already had anaemia during pregnancy:



# 3.1.4.3.b Subsample excluding women with anaemia before delivery (defined as Hb <110 g/l in the first trimester or Hb <105 g/l in the second or third trimesters)
# we could only exclude those with anaemia during pregnancy but not those with previous anaemia 





################################################################################################################################
### 3.1.5 Depression-related variables
################################################################################################################################

#Variables in DB: b370, c600, d171, e390, f200

# b_4f	(having a baby 18gw, HAB): b370 Edinburgh Post-natal Depression Score
                                # d24 + d25 + d26 + d27 + d28 + d29 + d30 + d31 + d32 + d33. All with a missing value on any of the 10 variables were put to ???1.
                                table(alspac$b370, useNA="always")
                                # value             label
                                # -7               YHL Your Home and Lifestyle (in the A file)
                                # -1           Missing
alspac$b370 <- ifelse(alspac$b370<0, 
                      NA,
                      alspac$b370)

#                             b371 EPDS in HAB - as B370 but if all 10 values were missing then B371 put to ???1, if only one, this was the mode.

#                             b122	Medication for depression this PREG		
                                  # value               label
                                  # -1             Missing
                                  # 1       Y in 1-3 MTHS
                                  # 2     Y 4 MTHS to now
                                  # 3 Y both time periods
                                  # 4          not at all

#                             b123	Medication for depression in 1st 3 months

# c_8a (your pregnancy, 32gw, YP):c101	MEDTN for depression in last 3MTHS
                                   # value             label
                                  # -7               FTG
                                  # -1           Missing
                                  # 0             Other
                                  # 1                 Y
                                  # 2                 N
                                  # 9                DK

#                             c600 EPDS in YP
                              # This score was put to missing if any of the 10 component items were missing.
                                table(alspac$c600, useNA="always")
                                # value             label
                                # -7               FTG Filling the Gaps (sent at 12m baby to those who did not receive the YP-c file)
                                # -1           Missing
                                # 0     not depressed
                                # 29    very depressed
  
alspac$c600 <- ifelse(alspac$c600<0, 
                      NA,
                      alspac$c600)
                                #                             C601 EPDS with missing values - As for C600 but with missing variables put to the mode unless all 10 values missing - in which case C601 put to missing.

# d_4b(about yourself 12gw, AY): # d171 Had severe depression
                              table(alspac$d171, useNA="always")
                              # value             label
                              # 0             Other
                              # 1      Yes recently
                              # 2       Yes in past
                              # 3          No never
                           # d171a Had severe depression, Y/N
                              table(alspac$d171a, useNA="always")
                              # value             label
                              # 1               Yes
                              # 2                No

# e_4f (me and my baby, 8w): e390 Edinburgh Post-natal Depression Score
                              # Any with a missing value on an item put to -1
                              table(alspac$e390, useNA="always")
                              # value             label
                              # -1           Missing
                              # 0     not depressed
                              # 30    very depressed

#                             e391 EPDS - Any with missing value put to mode unless all items missing
alspac$e390 <- ifelse(alspac$e390<0, 
                      NA,
                      alspac$e390)

# f_2b (looking after the baby, 8m): f200 Edinburgh Post-natal Depression Score
                                      # Any with a missing value on an item put to -1
                                        table(alspac$f200, useNA="always")
                                        # value             label
                                        # -1           Missing
                                        # 0     not depressed
                                        # 29    very depressed
alspac$f200 <- ifelse(alspac$f200<0, 
                      NA,
                      alspac$f200)

#                                     F201 EPDS with missing values - Any with missing value put to mode unless all items missing

# n_3a (mother and family 8y 1m): n1061	A3v: Mother has ever had severe depression

# The Edinburgh Postnatal Depression Scale (EPDS)
# The 10-item Edinburgh Postnatal Depression Scale (EPDS) assesses depressive symptoms during the prior week. Respondents rate the 
# frequency of each symptom on a Likert-type scale with four response options, the exact wording of which varies depending on the item. 
# See Table 1 for a complete item list with corresponding response options. Responses from all ten questions are coded 0-3 and three 
# items (1, 2, and 4) are reverse coded. All ten items are then summed to give a score ranging between 0 and 30, where higher scores 
# indicate more depressive symptoms. The EPDS does not assess or provide information on the duration or intensity of depressive symptoms.
# https://wellcomeopenresearch.org/articles/5-108
# Mean of all available EPDS total scores >12

                                        
                                        
# 3.1.5.1 Perinatal - Depression in pregnancy or 1 year postnatally, self-reported, from medical records or both, whichever is most appropriate
################################################################################################################################

# 3.1.5.1.a Cases perinatal depression (Mean of all available EPDS total scores >12), controls no perinatal depression

# I coded as NA the negative values of the depression vars first because we were missing controls and cases when doing it in the code below
alspac$depr_mean <- ifelse(alspac$unique==1 & #unique mothers
                             alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2, #live births
                           rowMeans(alspac[,c("b370","c600","e390","f200")], na.rm=T), 
                           NA)
alspac$depr_all <- case_when(alspac$depr_mean>12 ~ 1, 
                            alspac$depr_mean<=12 ~ 0, 
                                TRUE ~ NA_real_)

alspac$depr_all <- as.factor(alspac$depr_all)
summary(alspac$depr_all)
#check the distribution is the expected 
length(which(alspac$depr_mean>12))
length(which(alspac$depr_mean>=0 &
               alspac$depr_mean<=12))
dim(alspac)[1]-
  length(which(alspac$depr_mean>12)) -
  length(which(alspac$depr_mean>=0 &
                 alspac$depr_mean<=12))



# 3.1.5.1.b Subsample excluding women with pre-existing depression - controls never depressed
# Concern: not sure if d171 "yes in the past" (2) is the unique variable to get any preexistance depression, and "no never" (3) to get the history of depression

alspac$depr_subsamp <- case_when(alspac$depr_all==1  &
                                alspac$d171==3 ~ 1,
                                alspac$depr_all==0 &
                                alspac$d171==3 ~ 0,
                                    TRUE ~ NA_real_)
alspac$depr_subsamp <- as.factor(alspac$depr_subsamp)
summary(alspac$depr_subsamp)
#check the distribution is the expected 
length(which(alspac$depr_all==1  & alspac$d171==3))
length(which(alspac$depr_all==0  & alspac$d171==3))
dim(alspac)[1]-
  length(which(alspac$depr_all==1  & alspac$d171==3))-
length(which(alspac$depr_all==0  & alspac$d171==3))



# 3.1.5.2 Antenatal depression - Depression in pregnancy, self-reported, from medical records or both, whichever is most appropriate
################################################################################################################################

# 3.1.5.2.a Cases depression during pregnancy only, controls no depression during pregnancy
# Rebecca Pearson told Qian to define it either as the average of EPDS at 18w and at 32w 
# (https://jamanetwork.com/journals/jamapsychiatry/article-abstract/1748838) or go for the late pregnancy

alspac$ante_depr_mean <- ifelse(alspac$unique==1 & #unique mothers
                                  alspac$singleton==1 & #non multiple births
                                  alspac$kz010>=2, #live births
                             #     & (alspac$b370>=0 |
                             # alspac$c600>=0) ,
                             rowMeans(alspac[,c("b370","c600")],na.rm=T),
                           NA)
alspac$ante_depr_all <- case_when(alspac$ante_depr_mean>12 ~1,
                               alspac$ante_depr_mean>=0&
                                 alspac$ante_depr_mean<=12 ~ 0,
                                     TRUE ~ NA_real_)
alspac$ante_depr_all <- as.factor(alspac$ante_depr_all)
summary(alspac$ante_depr_all)
#check the distribution is the expected 
length(which(alspac$ante_depr_mean>12))
length(which(alspac$ante_depr_mean>=0 &
               alspac$ante_depr_mean<=12))
dim(alspac)[1]-
  length(which(alspac$ante_depr_mean>12)) -
  length(which(alspac$ante_depr_mean>=0 &
                 alspac$ante_depr_mean<=12))

# Concern: not sure if those time points are the good way to define antenatal depression



# 3.1.5.2.b Subsample excluding women with pre-existing depression

# If those never depressed (3) and with no perinatal depression are controls:
alspac$ante_depr_subsamp <- case_when(alspac$ante_depr_all==0 &
                                     alspac$d171==3 ~ 0,
                                   alspac$ante_depr_all==1&
                                     alspac$d171==3 ~ 1,
                                          TRUE ~ NA_real_)
alspac$ante_depr_subsamp <- as.factor(alspac$ante_depr_subsamp)
summary(alspac$ante_depr_subsamp)
#check the distribution is the expected 
length(which(alspac$ante_depr_all==1  & alspac$d171==3))
length(which(alspac$ante_depr_all==0  & alspac$d171==3))
dim(alspac)[1]-
  length(which(alspac$ante_depr_all==1  & alspac$d171==3))-
  length(which(alspac$ante_depr_all==0  & alspac$d171==3))



################################################################################################################################
### 3.1.6 Labour-related variables
################################################################################################################################

# Variables in DB: DEL_P1160, DEL_P1154

#DEL_P1160 C4i How labour started
table(alspac$DEL_P1160, useNA="always")
#   value                               label
# -10                           Not completed
# -1                            Not Answered
# 1                           Spontaneously
# 2 After induction (incl failed induction)
# 3                No labour (e.g elective)
# 4                            In other way

# DEL_P1154 C3c Membranes ruptured before or after onset of regular contractions
table(alspac$DEL_P1154)
#   value                              label
# -10                      Not completed
# -9                       Missing
# -1                       Not Answered
# 1                             Before
# 2                              After
# 3 With onset of regular contractions
# 4                            Unclear
# 7                    No contractions
# 9                                 NK

#-10 not completed and -1 not answered / -10 -1 -9 4 will be coded as NA
alspac$DEL_P1160 <- ifelse(alspac$DEL_P1160==-10 |
                             alspac$DEL_P1160==-1,
                           NA,
                           alspac$DEL_P1160)

alspac$DEL_P1154 <- ifelse(alspac$DEL_P1154==-10 |
                                   alspac$DEL_P1154==-1 |
                                   alspac$DEL_P1154==-9 |
                                   alspac$DEL_P1154==4,
                                 NA,
                           alspac$DEL_P1154)



#3.1.3.1 Induction of labour
################################################################################################################################

# Cases labour needed induction, controls no induction needed

alspac$induction <- case_when(alspac$unique==1 & #unique mothers
                                alspac$singleton==1 & #non multiple births
                                alspac$kz010>=2 & #live births
                                alspac$DEL_P1160==2 ~1,
                           
                              alspac$unique==1 & #unique mothers
                                alspac$singleton==1 & #non multiple births
                                alspac$kz010>=2 & #live births
                                alspac$DEL_P1160!=2 ~0,
                              
                              TRUE ~NA_real_)

alspac$induction <- as.factor(alspac$induction)
summary(alspac$induction)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1160==2))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1160!=2))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1160==2)) -
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1160!=2))



#3.1.3.2 Membrane rupture
################################################################################################################################

# Cases premature membrane rupture, controls no premature rupture

alspac$rup_memb <- case_when(alspac$unique==1 & #unique mothers
                               alspac$singleton==1 & #non multiple births
                               alspac$kz010>=2 & #live births
                               alspac$DEL_P1154==1 ~1,
                             
                             alspac$unique==1 & #unique mothers
                               alspac$singleton==1 & #non multiple births
                               alspac$kz010>=2 & #live births
                               alspac$DEL_P1154!=1 ~0,
                             
                             TRUE ~NA_real_)

alspac$rup_memb <- as.factor(alspac$rup_memb)
# Concerns:
# should we also consider DEL_P1153 C3b: Length of interval between membrane rupture and delivery
summary(alspac$rup_memb)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1154==1))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1154!=1))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1154==1)) -
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1154!=1))





################################################################################################################################
### 3.1.7 C-section and types of c-section
################################################################################################################################
# Pregnancies resulting in caesarean sections were identified from the mothers' questionnaires [at 8 weeks] augmented with cases from the computer system STORK which covered the two major hospitals BMH and Southmead.

# Variables in DB: DEL_P1210, DEL_P1212

#DEL_P1210 C6a Method of delivery
table(alspac$DEL_P1210, useNA="always")
#   value              label
# -10      Not completed
# -1       Not Answered
# 0        Spontaneous
# 1    Assisted breech
# 2 Breech, extraction
# 3  Caesarean section
# 4            Forceps
# 5  Vacuum extraction
# 6              Other
# The instructions for P1210 was that if a failed, in combination with a successful, method of delivery, code 6 should be used.

#DEL_P1212 C6c Delivery by caesarean section
table(alspac$DEL_P1212, useNA="always")
#   value             label
# -10     Not completed
# -1      Not Answered
# 1      Yes elective
# 2     Yes emergency
# 3                No 
# The instructions for classification of CS were as follows: Elective CS must have been pre-planned during the antenatal period; if elective is booked but woman goes into labour and has CS, code as emergency.



# 3.1.7.1 Cases c-section, controls no c-section

alspac$cs <- case_when(alspac$unique==1 & #unique mothers
                         alspac$singleton==1 & #non multiple births
                         alspac$kz010>=2 & #live births
                         (alspac$DEL_P1212==1 |
                      alspac$DEL_P1212==2)  ~ 1,
                      alspac$unique==1 & #unique mothers
                        alspac$singleton==1 & #non multiple births
                        alspac$kz010>=2 & #live births
                        alspac$DEL_P1212==3 ~ 0,
                           TRUE ~ NA_real_)
alspac$cs <- as.factor(alspac$cs)
summary(alspac$cs)
#check the distribution is the expected 
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  (alspac$DEL_P1212==1 | alspac$DEL_P1212==2)))
length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1212==3))
dim(alspac)[1]-
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  (alspac$DEL_P1212==1 | alspac$DEL_P1212==2))) -
  length(which(alspac$unique==1 &  alspac$singleton==1 &  alspac$kz010>=2 &  alspac$DEL_P1212==3))



# 3.1.7.2 Cases emergency c-section, controls no c-section

alspac$em_cs <- case_when(alspac$unique==1 & #unique mothers
                            alspac$singleton==1 & #non multiple births
                            alspac$kz010>=2 & #live births
                            alspac$DEL_P1212==2 ~ 1,
                          alspac$unique==1 & #unique mothers
                            alspac$singleton==1 & #non multiple births
                            alspac$kz010>=2 & #live births
                            alspac$DEL_P1212==3 ~ 0,
                              TRUE ~ NA_real_)
alspac$em_cs <- as.factor(alspac$em_cs)
summary(alspac$em_cs)



# 3.1.7.3 Cases elective c-section, controls no c-section

alspac$el_cs <- case_when(alspac$unique==1 & #unique mothers
                            alspac$singleton==1 & #non multiple births
                            alspac$kz010>=2 & #live births
                            alspac$DEL_P1212==1 ~ 1,
                          alspac$unique==1 & #unique mothers
                            alspac$singleton==1 & #non multiple births
                            alspac$kz010>=2 & #live births
                            alspac$DEL_P1212==3 ~ 0,
                          TRUE ~ NA_real_)
alspac$el_cs <- as.factor(alspac$el_cs)
summary(alspac$el_cs)



#Concern: If I coded the variables related to c-section considering P1210, we would miss some cs cases in comparison with excluding P1210


#3.1.8 - Hyperemesis and Nausea and Vomiting during pregnancy

alspac$hyp <- case_when(alspac$unique==1 &  #unique mothers
                        alspac$singleton==1 & #non multiple births
                        alspac$kz010>=2 & #live births
                          alspac$DEL_P1061==1 ~ 1,
                        alspac$unique==1 & #unique mothers
                        alspac$singleton==1 & #non multiple births
                        alspac$kz010>=2 & #live births
                            alspac$DEL_P1061==2 ~ 0,
                          TRUE ~ NA_real_)
alspac$hyp <- as.factor(alspac$hyp)
summary(alspac$hyp)

alspac <- mutate(alspac, any_nausea = case_when((b043==1 | b043==2 | b043==3) | b044==1 | (b100==1 | b100==2 | b100==3) | b101==1 | c090==1 | e100==1 | c052==1 ~ 1,
                            b043==4 | b044==2 | c052==2 ~ 0,  
                            TRUE ~ NA_real_ ))

alspac$any_nausea <- as.factor(alspac$any_nausea)
summary(alspac$any_nausea)

alspac <- mutate(alspac, any_vomit = case_when((b045==1 | b045==2 | b045==3) | b046==1 | (b104==1 | b104==2 | b104==3) | b105==1 | c053==1 | c092==1 | e101==1 ~ 1,
                                                b045==4 | b046==2 | c053==2 ~ 0,  
                                                TRUE ~ NA_real_ ))

alspac$any_vomit <- as.factor(alspac$any_vomit)
summary(alspac$any_vomit)

alspac <- mutate(alspac, medication = case_when((b100==1 | b100==2 | b100==3) | b101==1 | (b104==1 | b104==2 | b104==3) | b105==1 | c090==1 | c092==1 ~ 1,

                                                TRUE ~ NA_real_ ))

 

alspac <- mutate(alspac, nvp_sev_all = case_when(alspac$unique==1 & #unique mothers
                                             alspac$singleton==1 & #non multiple births
                                             alspac$kz010>=2 & #live births
                                               (medication==1 | hyp==1) ~3, 
                                           alspac$singleton==1 & #non multiple births
                                           alspac$kz010>=2 & #live births
                                             alspac$unique==1 & #unique mothers
                                               any_vomit==1 ~2, 
                                           alspac$singleton==1 & #non multiple births
                                             alspac$kz010>=2 & #live births
                                             alspac$unique==1 & #unique mothers
                                               any_nausea==1 ~1,
                                              alspac$singleton==1 & #non multiple births
                                             alspac$kz010>=2 & #live births 
                                             alspac$unique==1 & #unique mothers 
                                               (any_nausea==0 & any_vomit==0) ~0, 
                                             TRUE ~ NA_real_ ))

table(alspac$nvp_sev_all)

alspac <- mutate(alspac, allq = case_when(((b044==1 | b044==2) & (c053==1 | c053==2) & (e100==1 | e100==2)) ~1,
                                           TRUE ~ NA_real_ ))

alspac$nvp_sev_subsamp <- ifelse(alspac$allq==1, alspac$nvp_sev_all, TRUE ~ NA_real_)




################################################################################################################################
# 3.2 Children
################################################################################################################################


#most variables require to restrict each pregnancy to only one baby (singleton=1) and live births (kz010>=2)

################################################################################################################################
### 3.2.1 Birth weight-related variables 
################################################################################################################################

# Variables in db: kz030, kz021

#kz030 Preferred birthweight
range(alspac$kz030, na.rm=T)
# [1]  -10 5640
#   value                label
# -10   Not in core sample
alspac$kz030 <- ifelse(alspac$kz030<0, NA, alspac$kz030)

# kz021 - Participant sex
table(alspac$kz021, useNA="always")
# -2    1    2 
# 325 7363 7056
# -1         Not known
# 1              Male
# 2            Female



# #Whole sample - a
# ################################################################################################################################

# #3.2.1.1a Continuous variable - z using the ALSPAC population, zanthro calculated with STATA (next scripts)
 
# calculated separately in males and females, Exclude extreme BW (>5 SD from the sex-specific study mean)
boy <- subset(alspac, singleton==1 & #non multiple births
                   kz010>=2 & #live births
                kz021==1)
girl <- subset(alspac, singleton==1 & #non multiple births
                    kz010>=2 & #live births
                 kz021==2)
boy$bw <- (boy$kz030 - mean(boy$kz030, na.rm=T))/(sd(boy$kz030, na.rm=T))
girl$bw <- (girl$kz030 - mean(girl$kz030, na.rm=T))/(sd(girl$kz030, na.rm=T))
sexbw <- rbind(boy, girl)
# range(girl$bw, na.rm=T)
# -5.248380  4.343841  
# range(boy$bw, na.rm=T)
# -5.092005  3.804511
sexbw <- sexbw[,c(1,dim(sexbw)[2])]
alspac_sex <- alspac[which(alspac$aln%in%sexbw$aln),]
alspac_sex <- merge(alspac_sex, sexbw, by="aln")
alspac_nosex <- alspac[-which(alspac$aln%in%sexbw$aln),]
alspac_nosex$bw <- NA
alspac <- rbind(alspac_sex, alspac_nosex)
alspac$zbw_all <- alspac$bw
alspac$zbw_all <- ifelse(alspac$zbw_all>=-5, 
                         alspac$zbw_all,
                         NA)
summary(alspac$zbw_all)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -4.9386 -0.5476  0.0462  0.0015  0.6270  4.3438    1421 
alspac$bw <- case_when(alspac$singleton==1 &
                             alspac$kz010>=2 ~ alspac$bw,
                           TRUE ~ NA_real_)
alspac$bw <- ifelse(alspac$kz030<0, 
                    NA,
                    alspac$kz030)

#3.2.1.2a Low-bw - cases < 2500 g, Controls: 2500 - 4500 g

alspac$lbw_all <- case_when(alspac$singleton==1 & #non multiple births
                              alspac$kz010>=2 & #live births
                              alspac$kz030 > 0 &
                           alspac$kz030 < 2500 ~ 1,
                           
                           alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$kz030 >= 2500 &
                                  alspac$kz030 <= 4500 ~ 0,
                         
                               TRUE ~ NA_real_)

alspac$lbw_all <- as.factor(alspac$lbw_all)
summary(alspac$lbw_all)



#3.2.1.3a High-bw - cases > 4500 g, Controls: 2500 - 4500 g

alspac$hbw_all <- case_when(alspac$singleton==1 & #non multiple births
                              alspac$kz010>=2 & #live births
                              alspac$kz030 > 0 &
                              alspac$kz030 > 4500 ~ 1,
                            
                            alspac$singleton==1 & #non multiple births
                              alspac$kz010>=2 & #live births
                              alspac$kz030 >= 2500 &
                              alspac$kz030 <= 4500 ~ 0,
                            
                            TRUE ~ NA_real_)
alspac$hbw_all <- as.factor(alspac$hbw_all)
summary(alspac$hbw_all)



## Subsamples excluding multiple births and preterm - b
################################################################################################################################

# Variables in db: mz010a==2 multiple pregnancy, DEL_P50,  DEL_P1010==1 preterm delivery, bestgest gestational age

# DEL_P50==1 multiple birth
# table(alspac$DEL_P50, useNA="always")
# value             label
# -10     Not completed
# -1      Not Answered
# 1               Yes
# 2                No
alspac$DEL_P50 <- ifelse(alspac$DEL_P50<0,
                         NA,
                         ifelse(alspac$DEL_P50==1,
                                1,
                                0))
alspac$DEL_P50 <- as.factor(alspac$DEL_P50)

# DEL_P1010==1 preterm delivery
# table(alspac$DEL_P1010, useNA="always")
# -10   -7   -5   -1    1    2    NA
# 6662    3    6   54  652 7337   30
# value               label
# -10       Not completed
# -7          Unresolved
# -5 Exact EDD not known
# -1        Not answered
# 1                 Yes
# 2                  No
alspac$DEL_P1010 <- ifelse(alspac$DEL_P1010<0,
                         NA,
                         ifelse(alspac$DEL_P1010==1,
                                1,
                                0))
alspac$DEL_P1010 <- as.factor(alspac$DEL_P1010)





#3.2.1.1b Continuous variable

boy <- subset(alspac, singleton==1 & #non multiple births
                kz010>=2 & #live births
                bestgest>=37 &
                kz021==1)
girl <- subset(alspac, singleton==1 & #non multiple births
                 kz010>=2 & #live births
                 bestgest>=37 &
                 kz021==2)
boy$bw2 <- (boy$kz030 - mean(boy$kz030, na.rm=T))/(sd(boy$kz030, na.rm=T))
girl$bw2 <- (girl$kz030 - mean(girl$kz030, na.rm=T))/(sd(girl$kz030, na.rm=T))
sexbw <- rbind(boy, girl)
# range(girl$bw2, na.rm=T)
# -5.320766  4.798497 
# range(boy$bw2, na.rm=T)
# -4.906742  4.297964
sexbw <- sexbw[,c(1,dim(sexbw)[2])]
alspac_sex <- alspac[which(alspac$aln%in%sexbw$aln),]
alspac_sex <- merge(alspac_sex, sexbw, by="aln")
alspac_nosex <- alspac[-which(alspac$aln%in%sexbw$aln),]
alspac_nosex$bw2 <- NA
alspac <- rbind(alspac_sex, alspac_nosex)
alspac$zbw_subsamp <- alspac$bw2
alspac$zbw_subsamp <- ifelse(alspac$zbw_subsamp>=-5, 
                         alspac$zbw_subsamp,
                         NA)
summary(alspac$zbw_subsamp)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -4.9067 -0.6655 -0.0098  0.0004  0.6459  4.7985    2098 



#3.2.1.2b Low-bw - cases < 2500 g, Controls: 2500 - 4500 g

alspac$lbw_subsamp <- case_when(alspac$singleton==1&
                                  alspac$kz010>=2 & #live births
                                  alspac$bestgest>=37 &
                                  alspac$lbw_all==1 ~ 1,
                                
                                alspac$singleton==1&
                                  alspac$kz010>=2 & #live births
                                  alspac$bestgest>=37 &
                                  alspac$lbw_all==0 ~ 0,
                                
                                TRUE ~ NA_real_)
alspac$lbw_subsamp <- as.factor(alspac$lbw_subsamp)
summary(alspac$lbw_subsamp )



#3.2.1.3b High-bw - cases > 4500 g, Controls: 2500 - 4500 g

alspac$hbw_subsamp <- case_when(alspac$singleton==1&
                                  alspac$kz010>=2 & #live births
                                  alspac$bestgest>=37 &
                                   alspac$hbw_all==1 ~ 1,
                                 
                                 alspac$singleton==1&
                                  alspac$kz010>=2 & #live births
                                  alspac$bestgest>=37 &
                                   alspac$hbw_all==0 ~ 0,
                                 
                                 TRUE ~ NA_real_)
alspac$hbw_subsamp <- as.factor(alspac$hbw_subsamp)
summary(alspac$hbw_subsamp )


# - 2nd R file using data generated with STATA code where zanthro can be used to consider the reference values (go to end of the script)


################################################################################################################################
### 3.2.2 Gestational age
################################################################################################################################
# The date at which each measurement was undertaken together with the recorded (in the medical records) 
# expected date of delivery was used to derive the gestational age at each measurement and repeat measurements 
# are stored with gestational age (in days and week).
# Gestational age, at the time of each measurement, was calculated using the expected date of delivery (EDD) 
# as reported in the medical record (for most, given the dates of ALSPAC recruitment (when few women had dating 
# scans), this will have been based on the mothers LMP).
# Gestational age in days was calculated as:
#   280 - (EDD- date of measure)
# Once that was calculated we generated a second variable of gestational age in completed weeks by taking the 
# rounded down integer of gestational age in days divided by 7.
# bestgest in useful data is the best gestation we can get - Length of pregnancy (weeks)

# Variable in db: 

#bestgest
range(alspac$bestgest, na.rm=T)

alspac$bestgest <- ifelse(alspac$bestgest<0,
                      NA,
                      alspac$bestgest)
range(alspac$bestgest, na.rm=T)
#  4 47

## Whole sample - a
################################################################################################################################

#3.2.2.1a Continuous variable

alspac$ga_all <- case_when(alspac$singleton==1&
                             alspac$kz010>=2 ~ alspac$bestgest,
                           TRUE ~ NA_real_)
summary(alspac$ga_all)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  22.00   39.00   40.00   39.46   41.00   47.00    1248 



#3.2.2.2a Very pre-term - Cases: GA<34 weeks , controls: GA>=37 to < 42 weeks

alspac$vpretb_all <- case_when(alspac$singleton==1&
                                 alspac$kz010>=2 &
                                 alspac$ga_all<34 ~ 1,
                               
                               alspac$singleton==1&
                                 alspac$kz010>=2 &
                                 alspac$ga_all>=37 & 
                                 alspac$ga_all<42 ~ 0,
                               
                                 TRUE ~ NA_real_)
alspac$vpretb_all <- as.factor(alspac$vpretb_all)
summary(alspac$vpretb_all)



#3.2.2.3a Pre-term - Cases: GA<37 weeks , controls: GA>=37 to < 42 weeks

alspac$pretb_all <- case_when(alspac$singleton==1&
                                alspac$kz010>=2 &
                                alspac$ga_all<37 ~ 1,
                              
                              alspac$singleton==1&
                                alspac$kz010>=2 &
                                alspac$ga_all>=37 & 
                                alspac$ga_all<42 ~ 0,
                              
                               TRUE ~ NA_real_)
alspac$pretb_all <- as.factor(alspac$pretb_all)
summary(alspac$pretb_all)



#3.2.2.4a Post-term - Cases: GA at least 42 weeks , controls: GA>=37 to < 42 weeks

alspac$posttb_all <- case_when(alspac$singleton==1&
                                 alspac$kz010>=2 &
                                 alspac$ga_all>=42 ~ 1,
                               
                               alspac$singleton==1&
                                 alspac$kz010>=2 &
                                 alspac$ga_all>=37 & 
                                 alspac$ga_all<42 ~ 0,
                               
                              TRUE ~ NA_real_)

alspac$posttb_all <- as.factor(alspac$posttb_all)
summary(alspac$posttb_all)



## Subsamples excluding multiple births, stillbirths, elective cs and induction of labour - b
################################################################################################################################

#3.2.2.1b Continuous variable

alspac$ga_subsamp <- case_when(alspac$singleton==1&
                                 alspac$kz010>=2 &
                               alspac$el_cs==0 & 
                               alspac$induction==0 ~ alspac$ga_all,
                               TRUE ~ NA_real_) 
summary(alspac$ga_subsamp)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  22.00   39.00   40.00   39.42   41.00   46.00    9463  



#3.2.2.2b Very pre-term - Cases: GA<34 weeks, controls: GA >= 37 to < 42 weeks

alspac$vpretb_subsamp <- case_when(alspac$singleton==1&
                                     alspac$kz010>=2 &
                                     alspac$el_cs==0 & 
                                     alspac$induction==0 &
                            alspac$vpretb_all==1 ~ 1,
                            
                            alspac$singleton==1&
                              alspac$kz010>=2 &
                              alspac$el_cs==0 & 
                              alspac$induction==0 &
                              alspac$vpretb_all==0  ~ 0,
                            
                                   TRUE ~ NA_real_)

alspac$vpretb_subsamp <- as.factor(alspac$vpretb_subsamp)
summary(alspac$vpretb_subsamp)



#3.2.2.3b Pre-term - Cases: GA<37 weeks , controls: GA >= 37 to < 42 weeks

alspac$pretb_subsamp <- case_when(alspac$singleton==1&
                                    alspac$kz010>=2 &
                                    alspac$el_cs==0 & 
                                    alspac$induction==0 &
                                    alspac$pretb_all==1 ~ 1,
                                  
                                  alspac$singleton==1&
                                    alspac$kz010>=2 &
                                    alspac$el_cs==0 & 
                                    alspac$induction==0 &
                                    alspac$pretb_all==0  ~ 0,
                                  
                                  TRUE ~ NA_real_) 
alspac$pretb_subsamp <- as.factor(alspac$pretb_subsamp)
summary(alspac$pretb_subsamp)



#3.2.2.4b Post-term - Cases: GA at least 42 weeks , controls: GA >= 37 to < 42 weeks

alspac$posttb_subsamp <- case_when(alspac$singleton==1&
                                     alspac$kz010>=2 &
                                     alspac$el_cs==0 & 
                                     alspac$induction==0 &
                                     alspac$posttb_all==1 ~ 1,
                                   
                                   alspac$singleton==1&
                                     alspac$kz010>=2 &
                                     alspac$el_cs==0 & 
                                     alspac$induction==0 &
                                     alspac$posttb_all==0  ~ 0,
                                   
                                   TRUE ~ NA_real_) 
alspac$posttb_subsamp <- as.factor(alspac$posttb_subsamp)
summary(alspac$posttb_subsamp)





################################################################################################################################
### 3.2.3 Apgar
################################################################################################################################

# Variables in DB: DEL_B4003, DEL_B4004

#DEL_B4003 F1dap1: Apgar at 1 minute
table(alspac$DEL_B4003, useNA="always")
# value             label
# -10     Not completed
# -1      Not Answered

# DEL_B4004 F1dap5: Apgar at 5 minutes
table(alspac$DEL_B4004, useNA="always")
#  value             label
# -10     Not completed
# -1      Not Answered



#3.2.3.1 Apgar 1 min continuous
################################################################################################################################

alspac$apgar1 <- case_when(alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$DEL_B4003 >= 0 ~ alspac$DEL_B4003,
                         TRUE ~ NA_real_)  
summary(alspac$apgar1)



#3.2.3.2 Apgar 5 min continuous
################################################################################################################################

alspac$apgar5 <- case_when(alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$DEL_B4004 >= 0 ~ alspac$DEL_B4004,
                           TRUE ~ NA_real_)  
summary(alspac$apgar5)



#3.2.3.3 Cases Apgar at 1min <7, controls >=7
################################################################################################################################

alspac$lowapgar1 <- case_when(alspac$apgar1 < 7 ~ 1,
                           alspac$apgar1 >= 7 ~ 0,
                                  TRUE ~ NA_real_)
alspac$lowapgar1 <- as.factor(alspac$lowapgar1)
summary(alspac$lowapgar1)



#3.2.3.4 Cases Apgar at 5min <7, controls >=7
################################################################################################################################

alspac$lowapgar5 <- ifelse(alspac$apgar5 < 7,
                           1,
                           ifelse(alspac$apgar5 >= 7,
                                  0,
                                  NA))
alspac$lowapgar5 <- as.factor(alspac$lowapgar5)
summary(alspac$lowapgar5)





################################################################################################################################
### 3.2.4 NICU
################################################################################################################################

# Variables in DB: 

#DEL_B4005 F1e: Baby was resuscitated
table(alspac$DEL_B4005, useNA="always")
#  value             label
#    -10     Not completed
#     -1      Not Answered
#      1               Yes
#      2                No

# ka014 Admission to SCBU etc
table(alspac$ka014, useNA="always")
# value             label
# -1           Missing
# 1               Yes
# 2                No
# 3            Unsure

#Cases admitted to NICU, controls not admitted
alspac$nicu <- case_when(alspac$singleton==1 & #non multiple births
                           alspac$kz010>=2 & #live births
                           alspac$ka014 == 1 ~ 1,
                         alspac$singleton==1 & #non multiple births
                           alspac$kz010>=2 & #live births
                           alspac$ka014 == 2 ~ 0,
                             TRUE ~ NA_real_)  
alspac$nicu <- as.factor(alspac$nicu)
summary(alspac$nicu)





################################################################################################################################
### 3.2.5 Breastfeeding
################################################################################################################################

# Variables in DB: ka035, kb279, kb280, kc403, kc404

#ka035 Any breast feeding
table(alspac$ka035, useNA="always")
#  value             label
#      1               Yes
#      2                No

# kb279 Age in weeks when breast feeding stopped, quest 6m
#  value             label
#     -2       Still using
#     -1           Missing
length(which(is.na(alspac$kb279)))
# 3680

# kb280 Duration of breast feeding, quest 6m
table(alspac$kb280, useNA="always")
#  value             label
#     -1           Missing
#      1             Never
#      2          <1 month
#      3       1-<3 months
#      4       3-<6 months
#      5         6 or more

# kc403 Age stopped breast feeding, quest 15m
range(alspac$kc403, na.rm=T)
#  value             label
#  -9999 Consent withdrawn
#     -2      Never br'fed
#     -1           Missing
#     77             Still
length(which(is.na(alspac$kc403)))
# 4064

# kc404 Duration of breast feeding, quest 15m
table(alspac$kc404, useNA="always")
#  value             label
#     -1           Missing
#      0             Never
#      1            <3mths
#      2           3-5mths
#      3            6mths+



#3.2.5.1 Cases breastfeding, controls no breastfeeding

alspac$bf_ini <- case_when(alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$ka035 == 1 ~ 1,
                           alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live births
                             alspac$ka035 == 2 ~ 0,
                               TRUE ~ NA_real_) 
alspac$bf_ini <- as.factor(alspac$bf_ini)
summary(alspac$bf_ini)
#    0    1 NA's 
# 2707 9189 2848 


#3.2.5.2 Breastfeeding duration continuous

# alspac$bf_dur_w_6m <- case_when(alspac$kb279==-2 ~ 6*4, #babies who were still breast fed at the 6m visit considered as 6m
#                                     alspac$kb279>0 ~ as.numeric(alspac$kb279),
#                              TRUE ~ NA_real_)
# 
# summary(alspac$bf_dur_w_6m, na.rm=T)
# 
# alspac$bf_dur_w_15m <- case_when(alspac$kc403==-2 ~ 0,
#                                 alspac$kc403==77 ~ 15*4, #babies who were still breast fed at the 15m visit considered as 15m
#                                 alspac$kc403>=0 & alspac$kc403<77 ~ alspac$kc403*4,
#                                 TRUE ~ NA_real_)
# summary(alspac$bf_dur_w_15m, na.rm=T)
# hist(alspac$bf_dur_w_15m)
# 
# alspac$bf_dur_m_15m <- case_when(alspac$kc403==-2 ~ 0,
#                                  alspac$kc403==77 ~ 15, #babies who were still breast fed at the 15m visit considered as 15m
#                                  alspac$kc403>=0 & alspac$kc403<77 ~ as.numeric(alspac$kc403),
#                                  TRUE ~ NA_real_)
# 
# summary(alspac$bf_dur_m_15m, na.rm=T)
# hist(alspac$bf_dur_m_15m)


#Note: change made here - instead of recoding kc403, I have created a new variable
alspac$bf_dur_m <- case_when(alspac$singleton==1 & #non multiple births
                            alspac$kz010>=2 & 
                            alspac$kc403==-2 ~ 0,
                          
                          alspac$singleton==1 & #non multiple births
                            alspac$kz010>=2 & 
                            alspac$kc403==77 ~ 15,
                            alspac$kc403==16 ~ 15, #only 6 with 16 or 19 months, so truncated at 15 months
                            alspac$kc403==19 ~ 15,
                          
                          alspac$singleton==1 & #non multiple births
                            alspac$kz010>=2 &
                            (alspac$kc403>=0 &
                               alspac$kc403<77) ~ as.numeric(alspac$kc403),
                          
                          TRUE ~ NA_real_)



#SD of months
alspac$bf_dur <- (alspac$kc403 - mean(alspac$kc403, na.rm=T))/sd(alspac$kc403, na.rm=T)
summary(alspac$bf_dur)
sd(alspac$bf_dur,na.rm=T)
hist(alspac$bf_dur)


# Inverse normal transformed residuals
### Function to apply rank-based inverse normal transformation (INT) 
int <- function(x) { 
  y <- qnorm((rank(x, na.last="keep") - 0.375) / (sum(!is.na(x))+0.25))
}
#Ana's suggested formula is y<-qnorm((rank(x,na.last="keep")-0.5)/sum(!is.na(x)))

alspac$bf_dur_inv <- qnorm((rank(alspac$kc403, na.last="keep") - 0.375) / (sum(!is.na(alspac$kc403))+0.25))

summary(alspac$bf_dur_inv)
hist(alspac$bf_dur_inv)



# # 4 categories. cut off points: 0, 1-3, 4-6, >6
# alspac$bf_dur_4c <- case_when(alspac$kc403==0 ~ 0,
#                               (alspac$kc403>=1 &
#                                  alspac$kc403<=3) ~ 1,
#                               (alspac$kc403>=4 &
#                                  alspac$kc403<=6) ~ 2,
#                               alspac$kc403>6 ~ 3,
#                               TRUE ~ NA_real_)

# *** 4 categories. cut off points: 0 to <1, 1 to <3, 3 to <6, >=6
alspac$bf_dur_4c <- case_when(alspac$kc403>=0 &
                                alspac$kc403<1 ~ 0,
                           (alspac$kc403>=1 &
                                    alspac$kc403<3) ~ 1,
                                  (alspac$kc403>=3 &
                                           alspac$kc403<6) ~ 2,
                           alspac$kc403>=6 ~ 3,
                                                TRUE ~ NA_real_)
alspac$bf_dur_4c <- as.factor(alspac$bf_dur_4c)
summary(alspac$bf_dur_4c)



# BF_est
#Note: for this variable, never breastfeed, pre-term and multiple births should be excluded
#Note2: in the previous code version, kc403 has been previously recoded to restrict to singleton and live birth; never breastfed was recoded as zero

alspac$bf_est <- case_when((alspac$singleton==1 & #non multiple births
                             alspac$kz010>=2 & #live birth
                             alspac$ga_all>=37 & #term-only babies
                             alspac$kc403>=2) ~ 1,
                           
                              (alspac$singleton==1 & #non multiple births
                                 alspac$kz010>=2 & #live birth
                                 alspac$ga_all>=37 & #term-only babies
                                alspac$kc403<2 &
                                 alspac$kc403>=0) ~ 0,
                              TRUE ~ NA_real_)
alspac$bf_est <- as.factor(alspac$bf_est)
summary(alspac$bf_est)


# BF_sus
alspac$bf_sus <- case_when((alspac$singleton==1 & #non multiple births
                              alspac$kz010>=2 & #live birth
                              alspac$ga_all>=37 & #term-only babies
                              alspac$kc403>=6) ~ 1,
                           
                           (alspac$singleton==1 & #non multiple births
                              alspac$kz010>=2 & #live birth
                              alspac$ga_all>=37 & #term-only babies
                              alspac$kc403<6 &
                              alspac$kc403>=0) ~ 0,
                           TRUE ~ NA_real_)
alspac$bf_sus <- as.factor(alspac$bf_sus)
summary(alspac$bf_sus)




################################################################################################################################
################################################################################################################################
#     7. Prepare data to generate SGA and LGA variables in STATA
################################################################################################################################
################################################################################################################################

write.table(alspac, file=paste0(data_dir, "pheno_all_Rvars", ".csv"), sep=";", col.names=T, quote=F)
write.dta(alspac, file=paste0(data_dir, "pheno_all_Rvars", ".dta"), 
          convert.factors = "string") # if i did not add this, most codes of binary outcomes changed to 1 and 2 (instead of 0 and 1)

# Run DO-file in Stata to get LGA and SGA against reference population. I did not restrict zanthro function to singletons and live births 
# (zanthro does not seem to work with gen var replace if, but with egen only, so I do not know how to do that in stata)
