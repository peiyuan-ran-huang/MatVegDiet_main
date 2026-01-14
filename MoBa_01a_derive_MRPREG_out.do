/*******************************************************************************
ADAPTED FROM: "1_generating_data_pheno_from_raw_data.do" written by the MR-PREG team
*******************************************************************************/

/************************************************************************

All derivations based on the 'vars_definition' tab in the outcomes defintion excel spreadsheet of MR preg phenotypes. Then the Moba tab has mostly been updated based on this.

Then separate moba documentation is here:
N:\durable\Documentation

**************************************************************
*/
clear matrix
clear mata
set maxvar 10000

**This datatset includes MBRN and Q1-4
*use "N:\durable\RAW\MoBa_Phenotype\STATA\PDB2552_MoBa_MBRN.dta", clear

**merge with livebirth variable
*merge 1:1 PREG_ID_2552 BARN_NR using"M:\p582-gemmalc\phenotype\livebirth_dataset"
*drop _merge 

use "N:\durable\projects\Ran_MoBa_var\dat_pheno_select.dta", clear

tab DODKAT, m
gen livebirth=1 if DODKAT_G==1 | DODKAT_G==.
replace livebirth=0 if DODKAT_G==2 | DODKAT_G==3

drop if livebirth == 0
tab livebirth, m
* All samples are livebirths now

**************************************************************************
**RESTRICT TO CORRECT POPULATION
**************************************************************************

** multiple pregnancy ids from twins/trips
tab FLERFODSEL, m

** multiple mat ids from multiple pregnancies over time
tab PLURAL

** We exclude multiple pregnancies
gen from_multiplebirth=1 if FLERFODSEL==1
replace from_multiplebirth=0 if FLERFODSEL==.

preserve
keep if from_multiplebirth==1
keep PREG_ID_2552 M_ID_2552 F_ID_2552
cd "N:\durable\projects\Ran_MoBa_var\"
export excel using multiplebirthids, firstrow(var) replace 
restore


drop if FLERFODSEL==1



**RANDOMLY SAMPLE TO INCLUDE 1 MOTHER ID
codebook M_ID_2552, compact
set seed 4629602
sample 1, count by(M_ID_2552)


**Variable needed to indicate live births of the index preganncy (meeded for all except previous miscarraiges and stillbirths)
**MARIA REQ DODKAT 10/06/21 (STILL NO UPDATE 17/01/21)

count if PREG_ID_2552!=.
count if M_ID_2552!=""
count if F_ID_2552!=""

*offspring sex
tab KJONN
tab DD12

tab DODKAT_G 


tab KJONN DD12, m
* 1=male, 2=female
*1 boy, 2=girl
gen offspring_sex=KJONN if KJONN==1 | KJONN==2
replace offspring_sex=1 if DD12==1 & offspring_sex==.
replace offspring_sex=2 if DD12==2 & offspring_sex==.

tab offspring_sex, m
label define offspring_sex 1 "Male" 2 "Female"
label values offspring_sex offspring_sex

*Define age at menarche (needed for sporadic miscarraige and recuurent miscarraige)

gen age_menarche=AA12
sum age_menarche, d
*top and bottom 1% look too extreme but we dont use those anyway



**************************************************************************
**OUTCOMES COLLECTED RETROSPECTIVELY i.e. on previous pregnancies
**************************************************************************

**miscarriage
cap drop abort*
gen abort1=1 if  AA95==2 & (AA96>=2 & AA96<=22)
gen abort2=1 if  AA101==2 & (AA102>=2 & AA102<=22)
gen abort3=1 if  AA107==2 & (AA108>=2 & AA108<=22)
gen abort4=1 if  AA113==2 & (AA114>=2 & AA114<=22)
gen abort5=1 if  AA119==2 & (AA120>=2 & AA120<=22)
gen abort6=1 if  AA125==2 & (AA126>=2 & AA126<=22)
gen abort7=1 if  AA131==2 & (AA132>=2 & AA132<=22)
gen abort8=1 if  AA137==2 & (AA138>=2 & AA138<=22)
gen abort9=1 if  AA143==2 & (AA144>=2 & AA144<=22)
gen abort10=1 if  AA149==2 & (AA150>=2 & AA150<=22)
tab1 abort1 abort2 abort3 abort4 abort5 abort6 abort7 abort8 abort9 abort10
cap drop number_misc
egen number_misc=rowtotal(abort1 abort2 abort3 abort4 abort5 abort6 abort7 abort8 abort9 abort10)
tab number_misc
cap drop misc_all
gen misc_all=0 if number_misc==0
replace misc_all=1 if number_misc>=1 & number_misc<.
tab misc_all




**stillbirth
cap drop stillbirth*
gen stillbirth1=1 if  AA95==2 & (AA96>22 & AA96<=43)
gen stillbirth2=1 if  AA101==2 & (AA102>22 & AA102<=43)
gen stillbirth3=1 if  AA107==2 & (AA108>22 & AA108<=43)
gen stillbirth4=1 if  AA113==2 & (AA114>22 & AA114<=43)
gen stillbirth5=1 if  AA119==2 & (AA120>22 & AA120<=43)
gen stillbirth6=1 if  AA125==2 & (AA126>22 & AA126<=43)
gen stillbirth7=1 if  AA131==2 & (AA132>22 & AA132<=43)
gen stillbirth8=1 if  AA137==2 & (AA138>22 & AA138<=43)
gen stillbirth9=1 if  AA143==2 & (AA144>22 & AA144<=43)
gen stillbirth10=1 if  AA149==2 & (AA150>22 & AA150<=43)
tab1 stillbirth1 stillbirth2 stillbirth3 stillbirth4 stillbirth5 stillbirth6 stillbirth7 stillbirth8 stillbirth9 stillbirth10
cap drop number_stillbirths
egen number_stillbirths=rowtotal(stillbirth1 stillbirth2  stillbirth3 stillbirth4 stillbirth5 stillbirth6 stillbirth7 stillbirth8 stillbirth9 stillbirth10)
tab number_stillbirths
cap drop sb_all
gen sb_all=0 if number_stillbirths==0
replace sb_all=1 if number_stillbirths>=1 & number_stillbirths<.
tab sb_all
tab misc_all sb_all


**misc_subsamp - Controls: no miscarriages or stillbirths
tab misc_all
gen misc_subsamp=misc_all
replace misc_subsamp=. if sb_all==1


*Sporadic miscarriage (1 or 2 vs 0)
tab number_misc
gen s_misc_all=1 if (number_misc==1 | number_misc==2) & number_misc!=.
replace s_misc_all=0 if number_misc==0

*Sporadic miscarraige (excludes SB)
gen s_misc_subsamp=s_misc_all
replace s_misc_subsamp=. if sb_all==1
replace s_misc_subsamp=. if (age_menarche<9 | age_menarche>17) & age_menarche!=.

*Recurrent miscarraige (Cases: 3 or more miscarriages; Controls: no miscarriages)
gen r_misc_all=1 if (number_misc>=3 & number_misc!=.)
replace r_misc_all=0 if number_misc==0

*Recurrent miscarraige (excludes SB)
gen r_misc_subsamp=r_misc_all
replace r_misc_subsamp=. if sb_all==1
replace r_misc_subsamp=. if (age_menarche<9 | age_menarche>17) & age_menarche!=.

**SB subsamp
*Controls: no miscarriages or stillbirths
tab sb_all
gen sb_subsamp=sb_all
replace sb_subsamp=. if misc_all==1

tab sb_subsamp, m
tab misc_subsamp, m

**Pregnancy loss pregloss: Pregnancy loss (ie miscarriage or stillbirth) Cases: one or more pregnancy losses Controls: no pregnancy loss
gen pregloss=1 if misc_all==1 | sb_all==1
replace pregloss=0 if misc_all==0 & sb_all==0

**CHECK
tab pregloss if misc_subsamp==0
tab misc_subsamp if pregloss==1
tab misc_subsamp if pregloss==0

tab pregloss misc_subsamp, m

tab sb_subsamp if pregloss==1
tab sb_subsamp if pregloss==0
tab pregloss sb_subsamp, m

tab sb_subsamp, m
tab misc_subsamp, m

**************************************************************************
**OUTCOMES RELATED TO THE INDEX PREGNANCY (but some use previous info)
**************************************************************************
**STILL NEED TO EXCLUDE THOSE THAT WERE NOT A LIVEBIRTH**
tab DODKAT_G, m
sum SVLEN if DODKAT_G==1, d
sum SVLEN if DODKAT_G==2, d
sum SVLEN if DODKAT_G==3, d
sum SVLEN if DODKAT_G==., d

tab SVLEN if DODKAT_G==2
tab SVLEN if DODKAT_G==2 & SVLEN>=22
gen sb_index_all=1 if DODKAT_G==2 & SVLEN>=22
replace sb_index_all=0 if sb_index_all==.

tab sb_index_all

gen sb_index_subsamp=sb_index_all
replace sb_index_subsamp=. if DODKAT_G!=1 & sb_index_all==0
gen sb_index_subsamp2=sb_index_subsamp
replace sb_index_subsamp2=. if (DODKAT_G!=1 | pregloss==1) & sb_index_all==0


tab sb_index_subsamp

gen misc_index_all=1 if DODKAT_G==2 & SVLEN<22
replace misc_index_all=0 if misc_index_all==.

gen misc_index_subsamp=misc_index_all

replace misc_index_subsamp=. if DODKAT_G!=1 & misc_index_all==0

gen misc_index_subsamp2=misc_index_subsamp
replace misc_index_subsamp2=. if (DODKAT_G!=1 | pregloss==1) & misc_index_all==0

tab sb_index_all
tab sb_index_subsamp
tab sb_index_subsamp2

tab misc_index_all
tab misc_index_subsamp
tab misc_index_subsamp2





tab SVLEN if DODKAT_G==2 & SVLEN<22

tab misc_all if DODKAT_G==.
tab pregloss if DODKAT_G==.

*gen livebirth_temp=1 if DODKAT_G==1 | DODKAT_G==2 | DODKAT_G==3
*replace livebirth_temp=0 if livebirth_temp==.
/*
gen livebirth_temp=0 if SVLEN<22
replace livebirth_temp=1 if livebirth_temp==.
replace livebirth_temp=0 if KJONN==0 | KJONN==3

tab DODKAT_G livebirth_temp, m
sum SVLEN if livebirth_temp==0
sum SVLEN if livebirth_temp==1, d
*/
**need preterm birth and livebirth as exclusion criteria
cap drop pretb_all
gen pretb_all=.
replace pretb_all=1 if SVLEN <37
replace pretb_all=0 if SVLEN >=37 & SVLEN <42
replace pretb_all=. if livebirth==0

*BW
sum VEKT
gen bw=VEKT

**STILL NEED TO EXCLUDE THOSE THAT WERE NOT A LIVEBIRTH**
replace bw=. if livebirth==0

* BW in sex-specific z-score
tab KJONN, mis
egen zbw_boys = std(bw) if KJONN==1
sum zbw_boys
egen zbw_girls = std(bw) if KJONN==2
sum zbw_girls
egen zbw_all = rowfirst(zbw_boys zbw_girls)

list bw if zbw_all<-5
replace zbw_all=. if zbw_all<-5 | zbw_all>5
replace bw=. if zbw_all<-5 | zbw_all>5
sum bw

sum SVLEN if livebirth==0, d
sum SVLEN if livebirth==1, d
tab livebirth, m

*TEMP UNTIL WE GET ACTUAL LIVEBIRTH VAR, BUT ASSUME THOSE BORN LESS THAN 24 WEEKS ARE NOT LIVEBIRTHS AND THEREFORE NOT INC IN BW


*standardise after subsamp of bw
gen bw_subsamp=bw
replace bw_subsamp=. if pretb_all==1
*then standardise
egen zbw_subsamp_boys = std(bw_subsamp) if KJONN==1
sum zbw_subsamp_boys
egen zbw_subsamp_girls = std(bw_subsamp) if KJONN==2
sum zbw_subsamp_girls
egen zbw_subsamp = rowfirst(zbw_subsamp_boys zbw_subsamp_girls)
replace zbw_subsamp=. if zbw_subsamp<-5 | zbw_subsamp>5

sum LENGDE
tab LENGDE
gen birthlength=LENGDE

replace birthlength=. if livebirth==0
label var birthlength "offspring birth length (cm)"

egen birthlength_zscore = std(birthlength)

label var birthlength_zscore "Z-score: offspring birth length"


*Ponderal index (PI) was calculated as birth weight in kilograms divided by the cube of birth length in metres (kg/m3).
*Ponderal index – birthweight (kg) divided by the birth length (m) cubed



gen blm = birthlength/100
sum bw
gen lcub = blm ^ 3
gen birthweight_kg=bw/1000
gen ponderal = birthweight_kg / lcub

label var ponderal "offspring ponderal index" 

drop lcub blm birthweight_kg



egen ponderal_zscore = std(ponderal)

label var ponderal_zscore "Z-score: Ponderal index"




**WILL NEED TO REPLACE WITH MISSING FOR ALL IN THIS SECTION**

**HDP
tab HYPERTENSJON_KRONISK
cap drop HDP
gen HDP=0 if MORS_ALDER!=. /*& HYPERTENSJON_KRONISK!=1*/
replace HDP=1 if HYPERTENSJON_ALENE==1 | (CC114>=140 & CC114<. ) | (CC115>=90 & CC115<.) | CC616==1 | CC617==1 | CC618==1 | CC619==1 | CC620==1 | DD434==1
replace HDP=2 if PREEKL==1 | PREEKL==2 | PREEKL==3 | EKLAMPSI==1 |  HELLP==1 | PREEKLTIDL==1
tab HDP, mis


*control includes pre existing hypertension 
gen hdp_all=1 if HDP==1 | HDP==2
replace hdp_all=0 if HDP==0
replace hdp_all=. if livebirth==0

*control excludes pre existing hypertension 
gen hdp_subsamp=1 if HDP==1 | HDP==2
replace hdp_subsamp=0 if HDP==0 & HYPERTENSJON_KRONISK!=1
replace hdp_subsamp=. if livebirth==0

tab hdp_subsamp
tab hdp_all


tab HYPERTENSJON_ALENE
tab HYPERTENSJON_KRONISK
tab MORS_ALDER
tab EKLAMPSI
tab PREEKLTIDL
tab HDP
tab PREEKL


**GH
*control includes pre existing hypertension 
gen gh_all=1 if HDP==1
replace gh_all=0 if HDP==0
replace gh_all=. if livebirth==0

*control excludes pre existing hypertension 
gen gh_subsamp=1 if HDP==1
replace gh_subsamp=0 if HDP==0 & HYPERTENSJON_KRONISK!=1
replace gh_subsamp=. if livebirth==0

tab gh_subsamp, mis


**PE
*control includes pre existing hypertension 
gen pe_all=1 if HDP==2
replace pe_all=0 if HDP==0
replace pe_all=. if livebirth==0


*control excludes pre existing hypertension 
gen pe_subsamp=1 if HDP==2
replace pe_subsamp=0 if HDP==0 & HYPERTENSJON_KRONISK!=1
replace pe_subsamp=. if livebirth==0

tab pe_subsamp, mis



**GDM
tab FAAR, mis
tab DIABETES_MELLITUS, mis
cap drop gdm_all
gen gdm_all=0 if FAAR!=.
replace gdm_all=1 if DIABETES_MELLITUS==4
replace gdm_all=. if livebirth==0

tab DIABETES_MELLITUS gdm_all, mis

tab gdm_all, mis
gen gdm_subsamp=gdm_all
replace gdm_subsamp=. if DIABETES_MELLITUS==1 | DIABETES_MELLITUS==2 | DIABETES_MELLITUS==3 | DIABETES_MELLITUS==5
replace gdm_subsamp=. if livebirth==0

tab gdm_subsamp, mis

**maternal depression

tab AA1548

*only want values 1-4
foreach x of varlist AA1548 AA1549 AA1550 AA1551 AA1552 ///
CC1202 CC1203 CC1204 CC1205 CC1206 ///
DD837 DD838 DD839 DD840 DD841 {
recode `x' (0=.) (12=.) (13=.) (14=.) (23=.) (24=.) (34=.)
}

egen dep_15g=rowmean(AA1548 AA1549 AA1550 AA1551 AA1552)

tab dep_15g if AA1548==., m

egen dep_30g=rowmean(CC1202 CC1203 CC1204 CC1205 CC1206 )
egen dep_6m=rowmean(DD837 DD838 DD839 DD840 DD841)
tab dep_15g, m
*tab ante_dep, m

**this antenatal dep is a mean of depression scores at 15 weeks gestation and 30 weeks gestation
egen ante_dep=rowmean(dep_15g dep_30g)
gen ante_depr_all=1 if ante_dep>=2 & ante_dep<.
replace ante_depr_all=0 if ante_dep<2
replace ante_depr_all=. if livebirth==0

tab ante_depr_all, mis


**this perinatal dep is a mean of depression scores at 15 weeks gestation, 30 weeks gestation and 6 months post partum
egen peri_dep=rowmean(dep_15g dep_30g dep_6m)
gen depr_all=1 if peri_dep>=2 & peri_dep<.
replace depr_all=0 if peri_dep<2
replace depr_all=. if livebirth==0

tab depr_all, mis

**depr_subsamp: Control: never depressed
gen depr_subsamp=1 if depr_all==1
replace depr_subsamp=0 if peri_dep<2
replace depr_subsamp=. if livebirth==0


**ante_depr_subsamp: Control: never depressed
gen ante_depr_subsamp=1 if ante_depr_all==1
replace ante_depr_subsamp=0 if peri_dep<2
replace ante_depr_subsamp=. if livebirth==0


**Premature rupture of membranes
tab VANNAVGANG
gen rup_memb=1 if VANNAVGANG==1 | VANNAVGANG==2 | VANNAVGANG==3
replace rup_memb=0 if VANNAVGANG==.
replace rup_memb=. if livebirth==0

tab rup_memb, mis

**Induction of labour
tab FSTART
tab KSNITT

tab KSNITT FSTART

gen induction=1 if FSTART==2
replace induction=0 if FSTART==1 | FSTART==3
replace induction=. if livebirth==0

tab induction, mis

**Caesarian section
gen cs=0 if FAAR!=.
replace cs=1 if KSNITT!=.
replace cs=. if livebirth==0

tab cs, mis

tab DD20
tab DD20 cs, m

corr cs DD20
**Emergency c-section
tab KSNITT, mis
gen em_cs=1 if KSNITT==2
replace em_cs=0 if cs==0 // no c-section
replace em_cs=. if livebirth==0
ta em_cs, mis

**Elective c-section
gen el_cs=1 if KSNITT==1
replace el_cs=0 if cs==0 // no c-section
replace el_cs=. if livebirth==0
ta el_cs, mis

**************************************************************************
**OUTCOMES MORE RELATED TO OFFSPRING (THE INDEX PREGNANCY)
**************************************************************************
**STILL NEED TO EXCLUDE THOSE THAT WERE NOT A LIVEBIRTH**
**WILL NEED TO REPLACE WITH MISSING FOR ALL IN THIS SECTION**

**gestational age
tab SVLEN
gen ga_all=SVLEN
**STILL NEED TO EXCLUDE THOSE THAT WERE NOT A LIVEBIRTH**
sum ga_all
replace ga_all=. if livebirth==0


**gestational age with the following exclusions
/*
- Non-live birth
- Multiple births
- Elective caesarian section
- Physician initiated delivery (induction)
*/
gen ga_subsamp=ga_all
replace ga_subsamp=. if el_cs==1
replace ga_subsamp=. if induction==1
replace ga_subsamp=. if from_multiplebirth==1
replace ga_subsamp=. if livebirth==0


**very preterm
gen vpretb_all=.
replace vpretb_all=1 if SVLEN <34
replace vpretb_all=0 if SVLEN >=37 & SVLEN <42
**STILL NEED TO EXCLUDE THOSE THAT WERE NOT A LIVEBIRTH**
replace vpretb_all=. if livebirth==0
tab vpretb_all, mis


**very preterm with the following exclusions
/*
- Non-live birth
- Multiple births
- Elective caesarian section
- Physician initiated delivery (induction)
*/
gen vpretb_subsamp=vpretb_all
replace vpretb_subsamp=. if el_cs==1
replace vpretb_subsamp=. if induction==1
replace vpretb_subsamp=. if from_multiplebirth==1
replace vpretb_subsamp=. if livebirth==0


**preterm birth

**STILL NEED TO EXCLUDE THOSE THAT WERE NOT A LIVEBIRTH**
tab pretb_all, mis

**preterm birth with the following exclusions
/*
- Non-live birth
- Multiple births
- Elective caesarian section
- Physician initiated delivery (induction)
*/
gen pretb_subsamp=pretb_all
replace pretb_subsamp=. if el_cs==1
replace pretb_subsamp=. if induction==1
replace pretb_subsamp=. if from_multiplebirth==1
replace pretb_subsamp=. if livebirth==0

*postterm
gen posttb_all=1 if SVLEN >=42 & SVLEN <.
replace posttb_all=0 if SVLEN >=37 & SVLEN <42
**STILL NEED TO EXCLUDE THOSE THAT WERE NOT A LIVEBIRTH**
replace posttb_all=. if livebirth==0


tab posttb_all, mis

**post birth with the following exclusions
/*
- Non-live birth
- Multiple births
- Elective caesarian section
- Physician initiated delivery (induction)
*/
gen posttb_subsamp=posttb_all
replace posttb_subsamp=. if el_cs==1
replace posttb_subsamp=. if induction==1
replace posttb_subsamp=. if from_multiplebirth==1
replace posttb_subsamp=. if livebirth==0

/*
*BW
sum VEKT
gen bw=VEKT
**STILL NEED TO EXCLUDE THOSE THAT WERE NOT A LIVEBIRTH**

* BW in sex-specific z-score
tab KJONN, mis
egen zbw_boys = std(bw) if KJONN==1
sum zbw_boys
egen zbw_girls = std(bw) if KJONN==2
sum zbw_girls
egen zbw_all = rowfirst(zbw_boys zbw_girls)

*Check this is right
replace zbw_all=. if zbw_all<-5 | zbw_all>5
sum zbw_all

gen zbw_subsamp=zbw_all
replace zbw_subsamp=. if pretb_all==1
*/


*LBW
*note the control in Marias definitio includes participants with a bw>=4500 but I've currently stayed with our own mrpreg definitions
gen lbw_all=1 if bw<2500 & bw!=.
replace lbw_all=0 if bw>=2500 & bw<=4500
tab lbw_all, mis
replace lbw_all=. if from_multiplebirth==1
replace lbw_all=. if livebirth==0


gen lbw_subsamp=lbw_all
replace lbw_subsamp=. if pretb_all==1


*HBW
*note the control in Marias definitio includes participants with a bw<=2500 but I've currently stayed with our own mrpreg definitions
gen hbw_all=1 if bw>4500 & bw!=.
replace hbw_all=0 if bw>=2500 & bw<=4500
tab hbw_all, mis
replace hbw_all=. if from_multiplebirth==1
replace hbw_all=. if livebirth==0

gen hbw_subsamp=hbw_all
replace hbw_subsamp=. if pretb_all==1


**FOR SGA AND LGA we use a separate excel sheet with the mean and sd of bw by gestational week and sex from babies born in Normway during the moba period
merge m:1 ga_all offspring_sex using "N:\durable\file-import\p582-member-group\referencebw.dta"

list offspring_sex ga_all BW_MEAN BW_SD _merge in 1/10

order offspring_sex ga_all BW_MEAN BW_SD bw _merge

gen zbw_ref=(bw-BW_MEAN)/BW_SD
replace zbw_ref=. if bw==. | BW_MEAN==. | ga_all==.

sum zbw_ref

hist zbw_ref

gen sga=1 if zbw_ref<-1.282 & zbw_ref!=.
replace sga=0 if zbw_ref>=-1.282 & zbw_ref!=.

tab sga, m

gen lga=1 if zbw_ref>1.282 & zbw_ref!=.
replace lga=0 if zbw_ref<=1.282 & zbw_ref!=.

tab lga, m

sum bw if lga==1, d

list bw offspring_sex ga_all ga_subsamp if lga==1 & bw<2000

replace sga=. if from_multiplebirth==1
replace sga=. if livebirth==0

replace lga=. if from_multiplebirth==1
replace lga=. if livebirth==0


**Apgar at 1 min
tab APGAR1
gen apgar1=APGAR1
replace apgar1=. if from_multiplebirth==1
replace apgar1=. if livebirth==0

sum apgar1

**LOW APGAR SCORE at 1 (<7)
gen lowapgar1=1 if apgar1<7 & apgar1!=.
replace lowapgar1=0 if apgar1>=7 & apgar1!=.
replace lowapgar1=. if from_multiplebirth==1
replace lowapgar1=. if livebirth==0

tab lowapgar1, mis

**Apgar at 5 min
tab APGAR5
gen apgar5=APGAR5
sum apgar5
replace apgar5=. if from_multiplebirth==1
replace apgar5=. if livebirth==0

**LOW APGAR SCORE at 5 (<7)
gen lowapgar5=1 if apgar5<7 & apgar5!=.
replace lowapgar5=0 if apgar5>=7 & apgar5!=.
replace lowapgar5=. if from_multiplebirth==1
replace lowapgar5=. if livebirth==0

tab lowapgar5, mis

**nicu
gen nicu=1 if  OVERFLYTTET==1
replace nicu=0 if OVERFLYTTET==2
replace nicu=. if from_multiplebirth==1
replace nicu=. if livebirth==0

tab nicu, m

**Breastfeed initiation
**THIS INFORMATION IS OBTAINED FROM THE QUESTIONNAIRE ADMINISTERED WHEN THE CHILD IS 6 MONTHS
*tab VERSJON_SKJEMA4_TBL1

tab DD42
*case initiated breastfeeding/did not
*also chosen DD49 as thats 0-<1 months
gen bf_ini=1 if DD42==1 | DD49==1
replace bf_ini=0 if DD42==. & DD49==.


tab bf_ini, mis


**BF DURING
/*
What has your child been given to drink during the first 6 months of his/her life?
0 months DD49 1 month=DD50 month2=DD51, month3=DD52, month4=DD53, month5=DD54
month6==DD55

*/
*0=0 months, 1=1-3, 2=4-6, 3=6+
tab DD49
tab DD50
tab DD51
tab DD52

*gen bf_dur_4c=0 if (DD49==1 | DD42==1 | DD42==.)
*replace bf_dur_4c=1 if (DD50==1 | DD51==1 | DD52==1) & (DD53!=1 | DD54!=1 | DD55!=1)
*replace bf_dur_4c=2 if (DD53==1 | DD54==1 | DD55==1)
********************************************************************************
*** MODIFIED BY PEIYUAN HUANG
* 0 = 0 to <1
gen bf_dur_4c=0 if (DD49==1 | DD42==1 | DD42==.)
* 1 = 1 to <3
replace bf_dur_4c=1 if (DD50==1 | DD51==1) & (DD52!=1 & DD53!=1 & DD54!=1 & DD55!=1)
* 2 = 3 to <6
replace bf_dur_4c=2 if (DD52==1 | DD53==1 | DD54==1) & DD55!=1
* 3 = >=6
replace bf_dur_4c=3 if (DD55==1)
********************************************************************************

tab bf_dur_4c, mis
**breast feeding established, cases>=2 months, controls<2 momths
gen bf_est=1 if DD51==1 | DD52==1 | DD53==1 | DD54==1 | DD55==1
replace bf_est=0 if DD51!=1 & DD52!=1 & DD53!=1 & DD54!=1 & DD55!=1

**breast feeding sustained cases>=6 months, controls <6 months
*controls include those who didnt breastfeed at all
gen bf_sus=1 if DD55==1
replace bf_sus=0 if DD55!=1 
tab bf_sus

count if (DD53==1 | DD54==1) & DD55!=1
count if bf_sus==1 & bf_dur_4c==2
tab bf_ini, m
tab bf_dur_4c, m
tab bf_est, m
tab bf_sus, m

list DD42 DD49 DD50 DD51 DD52 DD52 bf_est bf_ini bf_dur_4c in 1/20
**AS the qu only goes up to 6 months I've not derived the continuosu versions of the bf variables as they'd have a weird distribution from being truncated at 6m (I think).


**ANAEMIA prev preg
tab AA572
gen anaemia_prev=1 if AA572==1
replace anaemia_prev=0 if AA572!=1
tab anaemia_prev, mis
replace anaemia_prev=. if from_multiplebirth==1
replace anaemia_prev=. if livebirth==0



**ANAEMIA duringpreg
tab AA573
gen anaemia_preg_all=1 if AA573==1
replace anaemia_preg_all=0 if AA573!=1
tab anaemia_preg_all, mis
replace anaemia_preg_all=. if from_multiplebirth==1
replace anaemia_preg_all=. if livebirth==0




gen anaemia_preg_subsamp=1 if AA573==1 & anaemia_prev!=1
replace anaemia_preg_subsamp=0 if AA573!=1 & anaemia_prev!=1
tab anaemia_preg_subsamp, mis
replace anaemia_preg_subsamp=. if from_multiplebirth==1
replace anaemia_preg_subsamp=. if livebirth==0

/*******************************************************************************
ADAPTED FROM: "4_upd_ga.do" by Dr Gemma Clayton
*******************************************************************************/
sum ga_all, d

count if ga_all<22
list bw if ga_all<22

*keep if ga_all<22

*order ga_all bw ga_subsamp pretb_all pretb_subsamp vpretb_all vpretb_subsamp sga lga

replace ga_all=. if ga_all<22

replace ga_subsamp=. if ga_all==.

tab pretb_subsamp, m
tab pretb_all, m
replace pretb_subsamp=. if ga_all==.
replace pretb_all=. if ga_all==.

tab vpretb_subsamp, m
tab vpretb_all, m
replace vpretb_subsamp=. if ga_all==.
replace vpretb_all=. if ga_all==.

sum sga if ga_all==.
sum lga if ga_all==.

********************************************************************************

order M_ID_2552 PREG_ID_2552 F_ID_2552 offspring_sex livebirth from_multiplebirth pregloss misc_all misc_subsamp  s_misc_all s_misc_subsamp r_misc_all r_misc_subsamp sb_all sb_subsamp  sb_index_all sb_index_subsamp misc_index_all misc_index_subsamp hdp_all hdp_subsamp gh_all gh_subsamp pe_all pe_subsamp  gdm_all gdm_subsamp depr_all depr_subsamp ante_depr_all ante_depr_subsamp rup_memb induction cs em_cs el_cs  bw zbw_all zbw_subsamp lbw_all lbw_subsamp hbw_all hbw_subsamp ga_all ga_subsamp vpretb_all vpretb_subsamp  pretb_all pretb_subsamp  posttb_all posttb_subsamp sga lga apgar1 lowapgar1 apgar5 lowapgar5 nicu bf_ini bf_est bf_sus bf_dur_4c anaemia_preg_all anaemia_preg_subsamp anaemia_prev 

*keep M_ID_2552 PREG_ID_2552 F_ID_2552 offspring_sex livebirth from_multiplebirth pregloss misc_all misc_subsamp  s_misc_all s_misc_subsamp r_misc_all r_misc_subsamp sb_all sb_subsamp  sb_index_all sb_index_subsamp misc_index_all misc_index_subsamp hdp_all hdp_subsamp gh_all gh_subsamp pe_all pe_subsamp  gdm_all gdm_subsamp depr_all depr_subsamp ante_depr_all ante_depr_subsamp rup_memb induction cs em_cs el_cs  bw zbw_all zbw_subsamp lbw_all lbw_subsamp hbw_all hbw_subsamp ga_all ga_subsamp vpretb_all vpretb_subsamp  pretb_all pretb_subsamp  posttb_all posttb_subsamp sga lga apgar1 lowapgar1 apgar5 lowapgar5 nicu bf_ini bf_est bf_sus bf_dur_4c anaemia_preg_all anaemia_preg_subsamp anaemia_prev

save "N:\durable\projects\Ran_MoBa_var\dat_MRPREG_out.dta", replace

