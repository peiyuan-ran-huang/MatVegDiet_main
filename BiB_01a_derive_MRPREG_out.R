################################################################################
#        Maternal Vegetarian/Plant-based Diets & Perinatal Health - BiB        #
################################################################################

# Last edited date: 31-Jul-2024
# This script is to extract raw data and derive MR-PREG outcome variables in BiB.

# ADAPTED FROM: "BiB_outcomes_master.R" written by the MR-PREG team

################################################################################

## MRPREG PHENOTYPE DERIVATION ##
## MASTER COPY ##
## DO NOT EDIT ##

# .libPaths("C:/Program Files/R")
setwd("Z:/working/data/BiB")

source("paths.R")

library(readstata13)
library(dplyr)
library(haven)
library(foreign)
library(tidyverse)
library(vtable)
library(writexl)
library(tictoc)

#data path 
Sys.setenv(datadir=BiB_latest_dir)

#Bib cohort info - person info
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

#pregnancy info
preginfo_all <- as_tibble(read_dta(paste0(Sys.getenv('datadir'),preginfo_dir)))
#select needed variables
preginfo_all <- preginfo_all %>% select(BiBPersonID,BiBPregNumber,PregnancyID,adminpnbirths)

#maternal baseline questionnaire all pregnancies 
mbqall<-as_tibble(read_dta(paste0(Sys.getenv("datadir"),mbqall_dir)))
#ghq variables 
mbqall <- mbqall %>% select(BiBPersonID,
                            BiBPregNumber,
                            rep0firper,
                            qad0gestwd,
                            ghq0ques22,
                            ghq0ques23,
                            ghq0ques24,
                            ghq0ques25,
                            ghq0ques26,
                            ghq0ques27,
                            ghq0ques28)
#nrow=11395
#eclipse pregnancy record 
eclpreg<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),eclpreg_dir)))
eclpreg<- eclpreg%>% select(BiBPersonID,
                            BiBPregNumber,
                            eclnregbrt,
                            bkfdiabete,
                            eclgestwks,
                            bkfhyperex,
                            drvgesdiab,
                            eclonstlab)

#eclipse neonatal record
eclbaby<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),eclbaby_dir)))
eclbaby<-eclbaby %>% 
  select(BiBPersonID,
         ChildID,
         eclapgar1m,
         eclapgar5m,
         eclbirthwt,
         eclbrthocm,
         ecllgaukwho,
         eclrtbirth,
         eclsgaukwho,
         eclrupbrth)

#maternity records mother info (age at birth of first child)
#maternity records pre-bib pregnancy (prebib gdm)
matrecprebibpreg<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),matrecprebibpreg_dir)))
matrecprebibpreg<- matrecprebibpreg %>%
  select(all_of(c("BiBPersonID","prnprebprggdm","prnprebprgpregid")))
#nrow=10642 pregnancy Key=prnprebinfpregid
#prnprebprgpregid Pre BiB preg ID - links to pre-BiB infant data

#maternity records pre-bib infant (bith outcomes prebib, infant death)
matrecprebibinfant<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),matrecprebibinfant_dir)))
#prnprebinfboutcome prebib birth outcome
matrecprebibinfant<-matrecprebibinfant %>% select(all_of(c("BiBPersonID",
                                                           "prnprebinfpregid",
                                                           "prnprebinfboutcome",
                                                           "prnprebinfbroute"
                                                           
)))
# Key=prnprebinfpregid
#remove duplicated id (multiple births) 
#matrecprebibinfant<-matrecprebibinfant[!duplicated(matrecprebibinfant$prnprebinfpregid),]
matrecprebibinfant <- matrecprebibinfant%>%
  group_by(prnprebinfpregid)%>%
  filter(n()==1)
#maternity records bib pregnancy
matrecpreg<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),matrecpreg_dir)))
matrecpreg<- matrecpreg %>% 
  select(BiBPersonID,
         BiBPregNumber,
         prnbpanaemianoted,
         prnbphdp,
         prnbphyperemesis,
         prnprexconthryroid)
#nrow=10939

#maternity records infant (birth outcomes, route of birth)
matrecinfant<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),matrecinfant_dir )))
matrecinfant<-matrecinfant %>%
  select(BiBPersonID,
         ChildID,
         prnbibirthoutc,
         prnbirouteofbrt,
         prnbispecialcare)
#nrow=11010
#prnbibirthoutc birth outcome, prnbirouteofbrt Route of birth, prnbispecialcare admitted to special care

####breastfeeding####
bib1000_6m<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),bib1000_6m_dir)))
bib1000_6m <- bib1000_6m %>%
  select(BiBPersonID,
         agecm_b6mtab,
         bib6c01,
         bib6c02,
         bib6c03days)
bib1000_12m<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),bib1000_12m_dir)))
bib1000_12m<- bib1000_12m %>%
  select(BiBPersonID, 
         agecm_b12tab,
         bib12e01,
         bib12e02,
         bib12e03days)
#18months questionnairee is ambiguous doesn't contain similar info as other questionnaires(was ever breastfead,age when stopped...)
#bib1000_18m<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),bib1000_18m_dir))) 
bib1000_24m<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),bib1000_24m_dir)))
bib1000_24m<-bib1000_24m%>%
  select(BiBPersonID,
         agecm_b24tab,
         bib24f01,
         bib24f02,
         bib24f03days)
bib1000_36m<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),bib1000_36m_dir)))
bib1000_36m<-bib1000_36m%>%
  select(BiBPersonID,
         agecm_b36tab,
         bib36c01,
         bib36c02)
medall<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),medall_dir)))
medall<- medall %>%
  select(BiBPersonID,
         agecm_medqst,
         mede08,
         mede08dy,
         mede08mn,
         mede08still,
         mede08wk,
         agecm_medqst,
         agecy_medqst)

allin_12<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),allin_12_dir)))
allin_24<-as_tibble(read_dta(paste0(Sys.getenv('datadir'),allin_24_dir)))
####merge all breastfeeding data#####
bf<-merge(
  merge(
    merge(
      merge(
        merge(
          merge(bib1000_6m,bib1000_12m,by="BiBPersonID",all=T),
          bib1000_24m, by="BiBPersonID",all=T),
        bib1000_36m, by="BiBPersonID",all=T),
      medall, by="BiBPersonID", all=T),
    allin_12, by="BiBPersonID", all=T),
  allin_24, by="BiBPersonID", all=T)

#####Merge data tables ######
personKeys <- c("BiBPersonID","BiBPregNumber")
motherKeys <- c("BiBMotherID","BiBPregNumber")
bibdata<- merge(
  merge(
    merge(
      merge(
        merge(
          merge(
            merge(eclbaby,matrecinfant, by =c("BiBPersonID","ChildID"), all =T),
            persinfo, by="BiBPersonID", all.x = T),
          preginfo_all,by.y=personKeys,by.x=motherKeys,all=T), 
        matrecpreg,by.x=motherKeys,by.y=personKeys,all=T),
      eclpreg,by.x=motherKeys,by.y=personKeys,all=T), 
    mbqall, by.x = motherKeys, by.y = personKeys, all= T),
  bf,by="BiBPersonID",all=T)

##bibdata_use is the tibble when the derived outcomes will be stored
#keep just singletons(no twins)  & just 1 random pregnancy is retained
set.seed(275)
bibdata_use<-bibdata %>% 
  filter(adminpnbirths==1|eclnregbrt==1) %>% 
  group_by(BiBMotherID) %>%
  slice_sample(., n=1)

###Miscarriage stillbirth and pregloss
###number of miscarriages in prebib data
prebibMiscID<-matrecprebibinfant[which(matrecprebibinfant[,"prnprebinfboutcome"]==2),"BiBPersonID"]
prebibMiscID<-prebibMiscID %>%
  group_by(BiBPersonID )%>%
  dplyr::summarise(prebib_misc_n = n())
#62 mothers had misc
#nm edit - i get 59
#calculate number of miscarriages prior to BiB 
bibdata_use<-merge(bibdata_use,prebibMiscID,by.x="BiBMotherID",by.y="BiBPersonID",all.x=T)

#number of stillbirths in prebib data
prebibSbID<-matrecprebibinfant[which(matrecprebibinfant[,"prnprebinfboutcome"]==3),"BiBPersonID"]
prebibSbID<-prebibSbID %>% 
  group_by(BiBPersonID)%>%
  dplyr::summarise(prebib_sb_n = n())
bibdata_use<-merge(bibdata_use,prebibSbID,by.x="BiBMotherID",by.y="BiBPersonID",all.x=T)
nrow(bibdata_use)
#120 had prebib sb
#nm edit - i get 115

#calculate number of miscarriages from non chosen pregnancies 
#removed pregnancies (twins and multiple pregs)
multipregremoved<-anti_join(bibdata,bibdata_use,by=c("BiBMotherID","BiBPregNumber"))

multipregmiscID<- multipregremoved %>%
  group_by(BiBMotherID) %>%
  #remove twins
  filter(adminpnbirths==1|eclnregbrt==1)%>%
  dplyr::summarise(multipreg_misc_n=sum(prnbibirthoutc==2))%>%
  select(BiBMotherID,multipreg_misc_n)
##No miscarriages and 10 stillbirths
#calculate number of stillbirths from non index pregnancies
multipregsbID<- multipregremoved%>%group_by(BiBMotherID) %>%
  filter(adminpnbirths==1|eclnregbrt==1) %>%
  dplyr::summarise(multipreg_sb_n=sum(prnbibirthoutc==3)) %>%
  select(BiBMotherID,multipreg_sb_n)

bibdata_use<-merge(bibdata_use,multipregmiscID,by="BiBMotherID",all.x=T)#just all.x 
bibdata_use<-merge(bibdata_use,multipregsbID,by="BiBMotherID",all.x=T)

bibdata_use<- bibdata_use %>%
  rowwise()%>%
  dplyr::mutate(total_prev_sb = sum(prebib_sb_n,multipreg_sb_n,na.rm = T),
                total_prev_misc = sum(prebib_misc_n,prebib_misc_n,na.rm = T))

###########Derive outcomes###################

#rm(list=setdiff(ls(), "bibdata_use"))

####Antenatal depression####
#1-ante_depr_all
#average of sum of all questions scores -1 to start by 0 
#sum of all scores -number on non missing answers divided by number of non missing answers 
#count non na values 
bibdata_use$ghqnona<- apply(bibdata_use[c("ghq0ques22","ghq0ques23","ghq0ques24","ghq0ques25","ghq0ques26","ghq0ques27","ghq0ques28")],
                            MARGIN= 1,function(x) sum(!is.na(x)))
bibdata_use <- bibdata_use %>%
  rowwise() %>%
  dplyr::mutate(ante_depr_cont = (sum(c_across(ghq0ques22:ghq0ques28),na.rm=T)-ghqnona)*7/ghqnona)#start from 0 m

#threshold:85 percentile of continuous depression
depthr<-quantile(bibdata_use$ante_depr_cont,.85,na.rm=T)
#create the variable
bibdata_use <- bibdata_use %>%
  mutate(
    ante_depr_all=case_when(ante_depr_cont<=depthr & eclbrthocm ==1 ~0,
                            ante_depr_cont>depthr& eclbrthocm ==1 ~1,
                            TRUE ~ NA_real_
    ),)

###count rows with NAs
bibdata_use%>%
  filter(ghqnona>0,ghqnona<7)%>%
  nrow()
#122 cases have at least 1 na in the questions 

#2-gdm_all: 
bibdata_use <- bibdata_use %>%
  #create the variable
  mutate(
    gdm_all = case_when(
      drvgesdiab == 1 & eclbrthocm == 1  ~ 0,#only include live birth 
      drvgesdiab == 2 & eclbrthocm == 1 ~ 1,
      TRUE ~ NA_real_
    ), 
    #3-gdm_subsamp
    gdm_subsamp = case_when( 
      drvgesdiab == 1 &  bkfdiabete == 1 & eclbrthocm == 1 ~ 0,
      drvgesdiab == 2 & eclbrthocm == 1 ~ 1,
      TRUE ~ NA_real_
    ), 
    #4-anaemia_preg_all
    anaemia_preg_all = case_when( 
      prnbpanaemianoted == 1 & eclbrthocm == 1 ~ 0,
      prnbpanaemianoted == 2 & eclbrthocm == 1 ~ 1,
      TRUE ~ NA_real_
    ))
tic()
bibdata_use <- bibdata_use %>%
  #create the variable
  mutate(
    ######??For all hdps I didn't use proteinurea variable because we have prnbphdp already...  
    #5-Gestational hypertension gh_all
    gh_all = case_when(
      prnbphdp == 1 & eclbrthocm == 1 ~ 0 ,
      prnbphdp == 2  & eclbrthocm == 1 ~ 1, 
      TRUE ~ NA_real_
    ), 
    #6-gh_subsamp
    gh_subsamp = case_when(
      prnbphdp == 1 & eclbrthocm == 1  & bkfhyperex == 1 ~ 0 ,
      prnbphdp == 2 & bkfhyperex == 1 & eclbrthocm == 1 ~ 1, 
      TRUE ~ NA_real_
      
    ), 
    #7-preeclampsia all pe_all
    pe_all = case_when(
      prnbphdp == 1 & eclbrthocm == 1 ~ 0,
      prnbphdp == 3 & eclbrthocm == 1 ~ 1,
      TRUE ~ NA_real_
      
    ), 
    # 8- preeclampsia subsample pe_subsamp
    pe_subsamp = case_when(
      prnbphdp == 1 & eclbrthocm == 1  & bkfhyperex == 1 ~ 0 ,
      prnbphdp == 3 & eclbrthocm == 1  & bkfhyperex == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    #9- hypertensive disorders of pregnancy hdp_all
    hdp_all = case_when(
      prnbphdp == 1 & eclbrthocm == 1 ~ 0,
      (prnbphdp == 3 | prnbphdp == 2 ) & eclbrthocm == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    #10- hdp_subsamp 
    hdp_subsamp = case_when(
      prnbphdp == 1 & eclbrthocm == 1  & bkfhyperex == 1 ~ 0 ,
      (prnbphdp == 3 | prnbphdp == 2 ) & eclbrthocm == 1  & bkfhyperex == 1 ~ 1,
      TRUE ~ NA_real_
    ))
toc()#
####?? For c-section I didn't chose 1st pregnancy because already we've chosen a random one! 
####???eclipse baby has cs non cs with eclrtbirth # matrecbibinf prnbirouteofbrt 7 outcomes 
#11-caesarean section cs
tic()
bibdata_use <- bibdata_use %>%
  #create the variable
  mutate(
    cs = case_when(
      eclrtbirth == 1    & eclbrthocm == 1 ~ 0 , 
      eclrtbirth == 2  & eclbrthocm == 1 ~ 1 , 
      TRUE ~ NA_real_
    ),
    ##
    #12-Emergency c-section em_cs
    em_cs = case_when(
      eclrtbirth == 1 |(prnbirouteofbrt<=3 & prnbirouteofbrt>=1)    & eclbrthocm == 1 ~ 0 ,
      prnbirouteofbrt == 6 & eclbrthocm == 1 ~ 1,
      TRUE ~ NA_real_
      
    ),
    #13-elective c-section 
    ####??include semi-elective c-section
    el_cs = case_when(
      eclrtbirth == 1 | (prnbirouteofbrt<=3)  & eclbrthocm == 1 ~ 0 ,
      prnbirouteofbrt == 4 & eclbrthocm == 1 ~ 1,
      TRUE ~ NA_real_
    ), 
    #14-induction 
    ###? medical and surirgical 
    induction = case_when (
      eclonstlab <=3 & eclbrthocm == 1  ~ 0,
      eclonstlab >3 & eclbrthocm == 1  ~ 1,
      TRUE ~ NA_real_
      
    ),
    
    #15- gestational age all ga_all
    ###eclgestwds####is the right one
    ga_all = case_when(eclbrthocm == 1 ~ eclgestwks,
                       TRUE ~ NA_real_
    ),
    #16- gestational age subsample
    ga_subsamp = case_when(eclbrthocm == 1  & el_cs==0 & induction==0 ~ eclgestwks, 
                           TRUE ~ NA_real_
    ), 
    #17-Very preterm birth all 
    vpretb_all = case_when(
      ga_all>=37 & ga_all<42 & eclbrthocm==1 ~ 0 ,
      ga_all<34 & eclbrthocm==1  ~ 1,
      TRUE ~ NA_real_
    ),
    
    #18-very preterm birth subsamp
    
    vpretb_subsamp = case_when(
      ga_subsamp>=37 & ga_subsamp<42 & eclbrthocm==1   ~ 0 ,
      ga_subsamp<34 & eclbrthocm==1  ~ 1,
      TRUE ~ NA_real_
    ),
    #19-preterm birth pretb_all
    pretb_all = case_when(
      ga_all>=37 & ga_all<42 & eclbrthocm==1  ~ 0 ,
      ga_all<37 & eclbrthocm==1 ~ 1,
      TRUE ~ NA_real_
    ), 
    #20-preterm birth subsample 
    
    pretb_subsamp = case_when(
      ga_subsamp>=37 & ga_subsamp<42 & eclbrthocm==1 ~ 0 ,
      ga_subsamp<37 & eclbrthocm==1 & eclbrthocm==1 ~ 1,
      TRUE ~ NA_real_
    ),
    
    #21- post-term birth posttb_all 
    posttb_all = case_when(
      ga_all>=37 & ga_all<42 & eclbrthocm==1~ 0 ,
      ga_all>=42 & eclbrthocm==1 ~ 1,
      TRUE ~ NA_real_
    ), 
    
    #22- post-term birth subsample posttb_subsamp
    posttb_subsamp = case_when(
      ga_subsamp>=37 & ga_subsamp<42    ~ 0 ,
      ga_subsamp>=42  ~ 1,
      TRUE ~ NA_real_
    ),
    
    #23- Low Birth weight all lbw_all 
    lbw_all = case_when(
      eclbirthwt>=2500 & eclbirthwt<=4500 & eclbrthocm == 1 ~ 0,
      eclbirthwt<2500  & eclbrthocm == 1 ~ 1, 
      TRUE ~ NA_real_
    ),
    #24- lbw_subsamp
    ###? when we say no preterm birth ga<37 use preterm derived variable variable or just eclgestwds>=37
    lbw_subsamp= case_when(
      eclbirthwt>=2500 & eclbirthwt<=4500 & eclbrthocm == 1 & ga_all>=37 ~ 0,
      eclbirthwt<2500  & eclbrthocm == 1 & ga_all>=37 ~ 1, 
      TRUE ~ NA_real_
      
    ), 
    #25- hbw_all
    hbw_all = case_when(
      eclbirthwt>=2500 & eclbirthwt<=4500 & eclbrthocm == 1 ~ 0,
      eclbirthwt>4500  & eclbrthocm == 1 ~ 1, 
      TRUE ~ NA_real_
    ),
    #26-hbw_subsamp
    hbw_subsamp =  case_when(
      eclbirthwt>=2500 & eclbirthwt<=4500 & eclbrthocm == 1 & ga_all>=37 ~ 0,
      eclbirthwt>4500  & eclbrthocm == 1 & ga_all>=37 ~ 1, 
      TRUE ~ NA_real_
    ),
    #27-small for gestational age 
    
    sga = case_when(eclsgaukwho == 1 & eclbrthocm == 1 ~ 0,
                    eclsgaukwho == 2 & eclbrthocm == 1 ~ 1,
                    TRUE ~ NA_real_
    ),
    
    #28- large for gestational age lga
    lga = case_when(ecllgaukwho == 1 & eclbrthocm == 1 ~ 0,
                    ecllgaukwho == 2 & eclbrthocm == 1 ~ 1,
                    TRUE ~ NA_real_
    ),
  )
toc()  
tic()
bibdata_use <- bibdata_use %>%
  #create the variable
  mutate( 
    #29-Miscarriage all
    misc_all= case_when(prnbibirthoutc !=2 &  total_prev_misc==0 ~ 0,
                        prnbibirthoutc==2 | total_prev_misc > 0 ~ 1 ,
                        TRUE ~ NA_real_
                        
    ),
    
    #30-Miscarriage subsample
    misc_subsamp= case_when(misc_all==0  & total_prev_sb==0 & prnbibirthoutc ==1~ 0,
                            misc_all==1  ~ 1 ,
                            TRUE ~ NA_real_
    ),
    
    #31-Stillbirth all sb_all
    
    sb_all = case_when(prnbibirthoutc !=3 & total_prev_sb==0 ~ 0,
                       prnbibirthoutc==3 | total_prev_sb > 0 ~ 1 ,
                       TRUE ~ NA_real_
                       
    ),
    #32-Stillbirth subsample sb_sumbsamp
    sb_subsamp= case_when(misc_subsamp == 0  ~ 0,
                          sb_all==1 ~ 1 ,
                          TRUE ~ NA_real_
    ),
    
    #33-pregnancy loss pregloss
    pregloss = case_when (misc_subsamp == 0  ~ 0,
                          misc_all==1 | sb_all == 1 ~ 1,
                          TRUE ~ NA_real_
    ),
    
    #34- sporadic miscarriage s_misc_all
    ###TODO add also bib miscarriages from multiple pregnancies
    s_misc_all=  case_when(misc_all ==0  ~ 0,
                           misc_all==1 & (total_prev_misc ==1) | (misc_all==0 & (total_prev_misc==2 ))   ~ 1 ,
                           TRUE ~ NA_real_
    ),
    
    #35- Sporadic miscarriage subsample
    
    s_misc_subsamp =case_when( misc_all==0 & sb_all==0 & rep0firper>=9 & rep0firper<=17  ~ 0,
                               s_misc_all==1 & rep0firper>=9 & rep0firper<=17  ~ 1 ,
                               TRUE ~ NA_real_
    ),
    #36-recurrent miscarriage all 
    r_misc_all=  case_when( misc_all == 0 ~ 0, 
                            (prnbibirthoutc==2 & total_prev_misc>=2) | (prnbibirthoutc!=2 & total_prev_misc>=3)   ~ 1 ,
                            TRUE ~ NA_real_
    ),
    #37-recurrent miscarriage subsample 
    r_misc_subsamp =case_when( misc_all==0 & sb_all==0 & rep0firper>=9 & rep0firper<=17 & rep0firper>=9 & rep0firper<=17 ~ 0,
                               r_misc_all==1  & rep0firper>=9 & rep0firper<=17  ~ 1 ,
                               TRUE ~ NA_real_
    ),
    
    
    #38- apgar score 1min 
    apgar1 = case_when(eclbrthocm==1 ~ eclapgar1m, 
                       TRUE ~ NA_real_
    ),
    #46- apgar score at 5 min
    apgar5 = case_when(eclbrthocm==1 ~ eclapgar5m,
                       TRUE ~ NA_real_
    ),
    #39- lowapgar at 1 min
    lowapgar1 = case_when(apgar1>=7  ~0,
                          apgar1<7  ~1, 
                          TRUE ~ NA_real_
    ), 
    #40- low apgar at 5 min
    lowapgar5 = case_when(apgar5>=7  ~0,
                          apgar5<7  ~1, 
                          TRUE ~ NA_real_
    )
  )

toc()

##edit 090823 add hyperemesis
bibdata_use <- bibdata_use %>%  mutate(hg_all = case_when(
  prnbphyperemesis == 1 & eclbrthocm == 1 ~ 0,#only include live birth 
  prnbphyperemesis == 2 & eclbrthocm == 1 ~ 1,
  TRUE ~ NA_real_
))

###############################################################################3
# Save temporary datasets to avoid starting over
saveRDS(bibdata_use, "bibdata_use_temp.rds")
bibdata_use <- readRDS("bibdata_use_temp.rds")
###############################################################################3

##edit 090823 need to add CA - from kurts script - check with AT the vars?

zbw_female <- bibdata_use %>%
  filter(Gender == 'Female') %>%
  select(BiBPersonID, eclbirthwt)
zbw_female$zscore <- scale(zbw_female$eclbirthwt)
zbw_female <- zbw_female %>% select(BiBPersonID, zscore)

zbw_male <- bibdata_use %>%
  filter(Gender == 'Male') %>%
  select(BiBPersonID, eclbirthwt)
zbw_male$zscore <- scale(zbw_male$eclbirthwt)
zbw_male <- zbw_male %>% select(BiBPersonID, zscore)

## edit 150923 there is an error in the below script where it assigns all the scaled weights nan? cant figure out why
#zbw_female <- scale(bibdata_use$eclbirthwt)    
#standarised birthweight zbw_all zbw_subsamp
#calculate sex specific zbw 
# zbw_female<-bibdata_use %>%
#   filter(Gender=="Female") %>%
#   mutate(zscore=scale(eclbirthwt)) %>%
#   select(BiBPersonID,zscore)
# zbw_male<-bibdata_use %>%
#   filter(Gender =="Male") %>%
#   mutate (zscore=scale(eclbirthwt)) %>%
#   select(BiBPersonID,zscore)
#bind them together then merge with bibdata_use
zbw <-rbind(zbw_female,zbw_male)
bibdata_use<-merge(bibdata_use,zbw,by=c("BiBPersonID"),all.x=T)
#41-zbw_all
bibdata_use<-bibdata_use %>%
  mutate(zbw_all = case_when(zscore<5 & zscore>-5 & eclbrthocm==1 ~ zscore,
                             TRUE ~ NA_real_
  ),
  #42-zbw_subsamp        
  zbw_subsamp = case_when(zscore<5 & zscore>-5 &eclbrthocm==1 & ga_all>=37 ~zscore,
                          TRUE ~NA_real_
  )
  )

########breastfeeding##################
## critical edit 060624
## removing ga condition after discussion
## in co working on 050624

#43-bf_ini cases:2585, control: 1098
bibdata_use<-bibdata_use %>%
  #breast feeding initiation
  mutate(bf_ini= case_when((bib6c01!=1 | bib12e01!=1 | bib24f01!=1 | bib36c01!=1 |mede08!=1 |all12c1!=1) & eclbrthocm==1 ~0,
                           (bib6c01==1 | bib12e01==1 | bib24f01==1 | bib36c01==1 |mede08==1 |all12c1==1) & eclbrthocm==1 ~1,
                           TRUE ~NA_real_
  )
  )

##all12c3 very wrong variable, it is mean to be age but coded 1,2,3 as categorical
#1-handle differences in bib1000 3 questionaires(6,12,24 mo)
#if dur<6mo in all take the 6mo value-->max<6mo
#if dur<12 mo in all take the 12 mo value 6<max<12
#if dur<24 mo in all take the 24 mo value 12<max<24
#if in one <6 and in other >6 take the higher

bibdata_use<-bibdata_use%>%
  dplyr::mutate(maxdurmo_1000=pmax(bib6c03days%/%30, bib12e03days%/%30,bib24f03days%/%30,na.rm=T),
                maxdurmo_allin=pmax(all24c2dy,all24c2wk*7,all12c3wk*7,na.rm=T),
                bf_dur_raw=case_when( 
                  maxdurmo_1000<=agecm_b6mtab & eclbrthocm==1 & bf_ini==1  ~ bib6c03days,
                  maxdurmo_1000<=agecm_b12tab & maxdurmo_1000>agecm_b6mtab & eclbrthocm==1 & bf_ini==1 ~ bib12e03days,
                  maxdurmo_1000<=agecm_b24tab & maxdurmo_1000>agecm_b12tab & eclbrthocm==1 & bf_ini==1 ~ bib24f03days,
                  TRUE ~NA_real_
                )
                
  )
#2-prioritise bib1000 entries
#if only one form has the duration take that value
#if 2 forms have it and they are equal take one of them
#if at 12 mo and 24 mo the values are different take th    is.na(bf_dur_raw) & eclbrthocm==1 & !is.na(all24c2wk)   & is.na(all24c2dy) & is.na(all12c3wk)   ~ all24c2wk*7,e value which is <age of baby at questionnaire
#3-prioritize all24 and all12 if form filled with days and not wks keep days and viceversa
#if form filled with both set to NA because they are dodgey and ambiguous (6 wk , 8 month)
bibdata_use<-bibdata_use%>%mutate(bf_dur_raw=case_when(!is.na(bf_dur_raw)~ bf_dur_raw,
                                                       
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(bib6c03days)  & is.na(bib12e03days)  & !is.na(bib24f03days) ~bib24f03days,  
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(bib6c03days)  & !is.na(bib12e03days) & is.na(bib24f03days) ~bib12e03days,                    
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(bib6c03days)  & is.na(bib12e03days)  & !is.na(bib24f03days) ~bib6c03days,                    
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(bib6c03days)  & bib12e03days==bib24f03days ~ bib12e03days,                    
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(bib6c03days)  & bib12e03days!=bib24f03days & bib12e03days<=agecm_b12tab*30 ~ bib12e03days,                    
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(bib6c03days)  & bib12e03days!=bib24f03days & bib12e03days>agecm_b12tab*30 ~ bib24f03days,                    
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(all24c2wk)    & !is.na(all24c2dy)& is.na(all12c3wk)  ~ all24c2dy,
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(all24c2wk)    & is.na(all24c2dy) & !is.na(all12c3wk) & is.na(all24c2wk)   & is.na(all24c2dy) ~ all12c3wk*7,
                                                       #in case we have values at 24 and 12 we check if max <12month then we chose value at 12 month if >12 month then we chose value at 24 mo
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & !is.na(all24c2wk)   & !is.na(all12c3wk) & maxdurmo_allin<=365   ~ all12c3wk*7,
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & !is.na(all24c2wk)   & !is.na(all12c3wk) & maxdurmo_allin>365   ~ all24c2wk*7,
                                                       #medall study
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(all24c2wk)    & is.na(all24c2dy)&is.na(all12c3wk) & !is.na(mede08dy) & is.na(mede08wk) & is.na(mede08mn) ~ mede08dy,
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(all24c2wk)    & is.na(all24c2dy)&is.na(all12c3wk) & is.na(mede08dy)  & !is.na(mede08wk)& is.na(mede08mn) ~ mede08wk*7,
                                                       is.na(bf_dur_raw) & eclbrthocm==1 & is.na(all24c2wk)    & is.na(all24c2dy)&is.na(all12c3wk) & is.na(mede08dy)  & is.na(mede08wk) & !is.na(mede08mn) ~ mede08mn*30,
                                                       
                                                       TRUE ~NA_real_
))  
##44-breastfeeding established
bibdata_use<-bibdata_use %>%
  
  mutate(bf_est= case_when(bf_dur_raw<2*30 & eclbrthocm==1 & bf_ini==1 ~0,
                           bf_dur_raw>=2*30 & eclbrthocm==1 & bf_ini==1 ~1,
                           TRUE ~NA_real_
  ),
  #45-breastfeeding sustained
  bf_sus = case_when(bf_dur_raw<6*30 & eclbrthocm==1 & bf_ini==1 ~0,
                     bf_dur_raw>=6*30 & eclbrthocm==1 & bf_ini==1 ~1,
                     TRUE ~NA_real_
  ),
  #46-breastfeeding duration
  bf_dur= case_when(eclbrthocm==1 ~ (bf_dur_raw - mean(bf_dur_raw, na.rm=T))/sd(bf_dur_raw,na.rm=T),
                    TRUE ~ NA_real_
  ),
  #47-bf_dur_inv
  bf_dur_inv=case_when(eclbrthocm==1 ~ qnorm((rank(bf_dur_raw,na.last="keep")-0.5)/sum(!is.na(bf_dur_raw))) ,
                       TRUE~NA_real_),
  #48-bf_dur_4c
  bf_dur_4c=case_when(eclbrthocm==1 & bf_ini==0 ~0, 
                      eclbrthocm==1 & bf_dur_raw>=1 & bf_dur_raw<=3 ~ 1,
                      eclbrthocm==1 & bf_dur_raw>=4 & bf_dur_raw<=6 ~ 2,
                      eclbrthocm==1 & bf_dur_raw>6 ~3,
                      TRUE ~ NA_real_
  )
  )

## MAA script is at the level of the child. Need to do a separate child script.
## AT solution to the above edit 310823
## Looks like I renamed personid to childId and motherId to personID and then linked

dim(bibdata_use)

unique_rows <- bibdata_use %>%
  group_by(ChildID) %>%
  filter(n() == 1) %>%
  ungroup()

head(unique_rows)
dim(unique_rows)  # Exclude empty BiBChildID: 12103 -> 11928
# colnames(unique_rows)[1]<-"BiBChildID"
# colnames(unique_rows)[2]<-"BiBPersonID"

## EDIT 230524 NM - after consultation we need to inc geno chip as a technical covariate 

link <- as_tibble(read_dta(paste0(Sys.getenv('datadir'),linkage)))
link <-link %>%
  select(BiBPersonID,
         lab_id,
         gt_chip,
         passed_qc)
link <- link %>% filter(passed_qc==1)

#merge chip on to phenofile
bibdata_use_link <- left_join(unique_rows,link,by="BiBPersonID")  # NOT using inner_join
length(unique(bibdata_use_link$lab_id))
table(is.na(bibdata_use_link$lab_id))
length(unique(bibdata_use_link$BiBPersonID))
length(unique(bibdata_use$BiBPersonID))

# add on TB chip here 
geno4 <- read_dta("X:/scripts/GWAS/BIB/geno_batch_variable/bib_geno_chip_4cat.dta")
bibdata_use_link <- left_join(bibdata_use_link,geno4,by="lab_id")  # NOT using inner_join
length(unique(bibdata_use_link$lab_id))
table(is.na(bibdata_use_link$lab_id))
length(unique(bibdata_use_link$BiBPersonID))
length(unique(bibdata_use$BiBPersonID))

# save.image("env_master.RData")

bibdata_use_link <- subset(bibdata_use_link, select=c("BiBPersonID","BiBMotherID","BiBPregNumber","ChildID","lab_id","gt_chip","gt_chip_4cat", 
                                                      "eclbrthocm","adminpnbirths","eclnregbrt",
                                                      "misc_all","misc_subsamp", "s_misc_all", "s_misc_subsamp", 
                                                      "r_misc_all", "r_misc_subsamp", "sb_all", "sb_subsamp", 
                                                      "pregloss", "hdp_all", "hdp_subsamp", "gh_all", "gh_subsamp", 
                                                      "pe_all", "pe_subsamp", "gdm_all", "gdm_subsamp","ante_depr_all", 
                                                      "induction","cs", "em_cs", "el_cs", 
                                                      "zbw_all", "zbw_subsamp", "lbw_all", "lbw_subsamp", 
                                                      "hbw_all", "hbw_subsamp", "ga_all", "ga_subsamp", 
                                                      "vpretb_all", "vpretb_subsamp", "pretb_all", "pretb_subsamp", 
                                                      "posttb_all", "posttb_subsamp", "sga", 
                                                      "lga", "apgar1", "apgar5", 
                                                      "lowapgar1", "lowapgar5", "bf_ini", "bf_dur", 
                                                      "bf_dur_inv", "bf_dur_4c", "bf_est", "bf_sus", 
                                                      "anaemia_preg_all", "hg_all"))

# write.table(bibdata_use_link, "bib_outcomes_master_0824.txt", quote=F, row.names=F)

saveRDS(bibdata_use_link, "dat_MRPREG_out.rds")

# ## And for children
# ## Same outcomes, same data, just using the childs bib person id to link to the lab id so its at their genetic level
# 
# unique_rows <- bibdata_use %>%
#   group_by(ChildID) %>%
#   filter(n() == 1) %>%
#   ungroup()
# 
# #merge chip on to phenofile
# bibdata_use_link_child <- inner_join(unique_rows,link,by="BiBPersonID")
# length(unique(bibdata_use_link_child$lab_id))
# table(is.na(bibdata_use_link_child$lab_id))
# length(unique(bibdata_use_link_child$BiBPersonID))
# length(unique(bibdata_use_link_child$BiBPersonID))
# 
# # add on TB chip here 
# geno4 <- read_dta("X:/scripts/GWAS/BIB/geno_batch_variable/bib_geno_chip_4cat.dta")
# bibdata_use_link_child <- inner_join(bibdata_use_link_child,geno4,by="lab_id")
# length(unique(bibdata_use_link$lab_id))
# table(is.na(bibdata_use_link$lab_id))
# length(unique(bibdata_use_link$BiBPersonID))
# length(unique(bibdata_use$BiBPersonID))
# 
# bibdata_use_link_child <- subset(bibdata_use_link_child, select=c("BiBPersonID","BiBPregNumber","lab_id","gt_chip","gt_chip_4cat","misc_all", 
#                                                                   "misc_subsamp", "s_misc_all", "s_misc_subsamp", 
#                                                                   "r_misc_all", "r_misc_subsamp", "sb_all", "sb_subsamp", 
#                                                                   "pregloss", "hdp_all", "hdp_subsamp", "gh_all", "gh_subsamp", 
#                                                                   "pe_all", "pe_subsamp", "gdm_all", "gdm_subsamp","ante_depr_all", 
#                                                                   "induction","cs", "em_cs", "el_cs", 
#                                                                   "zbw_all", "zbw_subsamp", "lbw_all", "lbw_subsamp", 
#                                                                   "hbw_all", "hbw_subsamp", "ga_all", "ga_subsamp", 
#                                                                   "vpretb_all", "vpretb_subsamp", "pretb_all", "pretb_subsamp", 
#                                                                   "posttb_all", "posttb_subsamp", "sga", 
#                                                                   "lga", "apgar1", "apgar5", 
#                                                                   "lowapgar1", "lowapgar5", "bf_ini", "bf_dur", 
#                                                                   "bf_dur_inv", "bf_dur_4c", "bf_est", "bf_sus", 
#                                                                   "anaemia_preg_all", "hg_all"))
# 
# write.table(bibdata_use_link_child, "bib_outcomes_master_child_110624.txt", quote=F, row.names=F)
