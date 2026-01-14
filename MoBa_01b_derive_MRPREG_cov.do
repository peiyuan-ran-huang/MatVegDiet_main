/*******************************************************************************
ADAPTED FROM: "cp_0_gen_covars.do" written by the MR-PREG team
*******************************************************************************/

** creates covariables that match the moba tab of covariables_defintion (stored in MR PREG onedrive)

/*******************************************************************************
** FILE 1: MBRN, creates matage_yrs, parity_cat5, parity_b_multi, mateth_b_other, offspring_sex (to be merged with D to create sex_b_male)

*******************************************************************************/

*import delimited "N:\durable\projects\covariables_cp\2023-05-09_gemmalc_PDB2552_MBRN_541_v12_id-11.txt", clear case(preserve)

use "N:\durable\projects\Ran_MoBa_var\dat_MRPREG_out.dta", clear

** maternal age at delivery
gen matage_yrs=MORS_ALDER
sum matage_yrs, d

** parity
gen parity_cat5=PARITET_5

** partiy
gen parity_b_multi=1 if parity_cat5>=1 & parity_cat5!=.
replace parity_b_multi=0 if parity_cat==0

** ethnicity

tab FODELAND_KAT_NOR_GBD
/*
0201 = Norway
0202 = High Income
0203 = Central Europe, Eastern Europe and Central Asia
0204 = Sub-Saharan Africa
0205 = North Africa and Middle East
0206 = South Asia
0207 = South East Asia, East Asia and Oceania
0208 = Latin America and Caribbean
*/

gen mateth_b_other=0 if FODELAND_KAT_NOR_GBD==201 | FODELAND_KAT_NOR_GBD==202 | FODELAND_KAT_NOR_GBD==203
replace mateth_b_other=1 if FODELAND_KAT_NOR_GBD==204 | FODELAND_KAT_NOR_GBD==205 | FODELAND_KAT_NOR_GBD==206 | FODELAND_KAT_NOR_GBD==207 | FODELAND_KAT_NOR_GBD==208


label define mateth_b_other 0 "Norway, High Income or Central Europe, Eastern Europe and Central Asia" 1 "Sub-Saharan Africa, North Africa and Middle East, South Asia, South East Asia, East Asia and Oceania, Latin America and Caribbean"

label val mateth_b_other mateth_b_other

** offspring sex
tab KJONN
gen offspring_sex_mbrn=KJONN if KJONN==1 | KJONN==2
gen sex_b_male=1 if KJONN==1
replace sex_b_male=0 if KJONN==2

*keep PREG_ID_2552 matage_yrs parity_cat5 parity_b_multi mateth_b_other offspring_sex_mbrn sex_b_male

*cd "N:\durable\projects\Gemmapregoutcomes\GWAS PIPELINE\data"

*save "mbrn_covar_f", replace

/*******************************************************************************
** FILE 2: QU 1 0-3 months, creates matbmi_kgm2, matedu_cat3, matedu_b_uni, matsmoke_b_early (to be merged with C qu to create matsmoke_b_yes)
*******************************************************************************/


*import delimited "N:\durable\projects\covariables_cp\2023-05-09_gemmalc_PDB2552_Q1_v12_id-12.txt", clear case(preserve)

** maternal bmi
*what did you weigh when you became pregnant 
tab AA85
*replace AA85="" if AA85=="NA"
destring AA85, replace
sum AA85 //in kg
gen mat_weight=AA85
replace mat_weight=. if AA85<30 | AA85>200

*height in cm
tab AA87
*replace AA87="" if AA87=="NA"
destring AA87, replace

sum AA87, d

gen mat_height_cm=AA87 
replace mat_height_cm=. if mat_height_cm<135

gen mat_height_m= mat_height_cm/100
gen matbmi_kgm2=mat_weight/(mat_height_m^2)

sum matbmi_kgm2, d

* maternal education 0=less than university degree/1=completed a university degree

gen matedu_b_uni=1 if AA1124==5 | AA1124==6
replace matedu_b_uni=0 if AA1124==1 | AA1124==2 | AA1124==3 | AA1124==4
replace matedu_b_uni=0 if matedu_b_uni!=. & AA1125!=.
replace matedu_b_uni=0 if matedu_b_uni!=. & AA1125!=.

label var matedu_b_uni "Degree or higher"

* maternal smoking (from A qu)
tab AA1356

gen matsmoke_b_early=1 if (AA1356==2 | AA1356==3)
replace matsmoke_b_early=0 if AA1356==1


*keep PREG_ID_2552 mat_weight mat_height_cm mat_height_m matbmi_kgm2 matedu_b_uni matsmoke_b_early

*cd "N:\durable\projects\Gemmapregoutcomes\GWAS PIPELINE\data"

*save "A_covar_f", replace



/*******************************************************************************
** FILE 3: QU 3, creates matsmoke_b_during (to be merged with A qu to create matsmoke_b_yes), matalcohol_b_yes
*******************************************************************************/

*import delimited "N:\durable\projects\covariables_cp\2023-05-09_gemmalc_PDB2552_Q3_v12_id-13.txt", case(preserve) clear

destring CC1037, replace force
destring CC1157, replace force
destring CC1158, replace force
destring CC1159, replace force

** matsmoke_b_during
tab CC1037

gen matsmoke_b_during= 1 if CC1037==2 | CC1037==3 |  CC1037==4 | CC1037==5 | CC1037==6 | CC1037==7
replace matsmoke_b_during=0 if CC1037==1


** maternal alcohol

gen mat_alcohol_1=1 if CC1157==1 | CC1157==2 | CC1157==3 | CC1157==4
replace mat_alcohol_1=0 if CC1157==5 | CC1157==6 | CC1157==7
replace mat_alcohol_1=. if CC1157==0 

tab mat_alcohol_1, m

gen mat_alcohol_2=1 if CC1158==1 | CC1158==2 | CC1158==3 | CC1158==4
replace mat_alcohol_2=0 if CC1158==5 | CC1158==6 | CC1158==7
replace mat_alcohol_2=. if CC1158==0 

tab mat_alcohol_2, m

gen mat_alcohol_3=1 if CC1159==1 | CC1159==2 | CC1159==3 | CC1159==4
replace mat_alcohol_3=0 if CC1159==5 | CC1159==6 | CC1159==7
replace mat_alcohol_3=. if CC1159==0 

tab mat_alcohol_3, m

gen mat_alcohol_any=1 if mat_alcohol_1==1 | mat_alcohol_2==1 | mat_alcohol_3==1
replace mat_alcohol_any=0 if mat_alcohol_1==0 & mat_alcohol_2==0 & mat_alcohol_3==0


tab mat_alcohol_any, m
label var mat_alcohol_any "any alcohol during pregnancy"



*keep PREG_ID_2552 matsmoke_b_during mat_alcohol_any

*cd "N:\durable\projects\Gemmapregoutcomes\GWAS PIPELINE\data"

*save "C_covar_f", replace




/*******************************************************************************
** FILE 4: QU 4, updates offspring_sex to make sex_b_male
*******************************************************************************/

*import delimited "N:\durable\projects\covariables_cp\2023-05-09_gemmalc_PDB2552_Q4_6months_v12_id-14.txt" , case(preserve) clear 

tab DD12
*1=BOY, 2=GIRL

gen offspring_sex_d=1 if DD12==1
replace offspring_sex_d=0 if DD12==2

*keep PREG_ID_2552 offspring_sex_d

*cd "N:\durable\projects\Gemmapregoutcomes\GWAS PIPELINE\data"

*save "D_covar_f", replace



/*******************************************************************************
** FILE 4: Combine these
*******************************************************************************/

***COMBINED VARS
*cd "N:\durable\projects\Gemmapregoutcomes\GWAS PIPELINE\data"

*use "mbrn_covar_f", clear
*merge m:m PREG_ID_2552 using "A_covar_f"
*drop _merge
*merge m:m PREG_ID_2552 using "C_covar_f"
*drop _merge
*merge m:m PREG_ID_2552 using "D_covar_f"
*drop _merge

*drop offspring_sex_d

save "N:\durable\projects\Ran_MoBa_var\dat_MRPREG_out_cov.dta", replace
