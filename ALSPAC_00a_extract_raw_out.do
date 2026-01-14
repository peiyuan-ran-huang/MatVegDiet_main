/*******************************************************************************
ADAPTED FROM: "1.syntax_template_20240610.do" written by the MR-PREG team
*******************************************************************************/

********************************************************************************
********** ALSPAC SYNTAX - MR-PREG **********
*********************************************

cd "Z:\working\data\ALSPAC"

* Script updatad by AS - updated to latest ALSPAC syntax

*** Syntax template for direct users preparing datasets using child and parent based datasets.

* Updated 16th May 2024 - updated withdrawal of consent frequencies (child c)


****************************************************************************************************************************************************************************************************************************
* This template is based on that used by the data buddy team and they include a number of variables by default.
* To ensure the file works we suggest you keep those in and just add any relevant variables that you need for your project.
* To add data other than that included by default you will need to add the relvant files and pathnames in each of the match commands below.
* There is a separate command for mothers questionnaires, mothers clinics, partner, mothers providing data on the child and data provided by the child themselves.
* Each has different withdrawal of consent issues so they must be considered separately.
* You will need to replace 'YOUR PATHNAME' in each section with your working directory pathname.

*****************************************************************************************************************************************************************************************************************************.

* G0 Mother (pregnancy) based files - include here all files related to the pregnancy and/or mother

* If no mother variables are required, KEEP this section and remove the instruction below to run it..

clear
set maxvar 32767	
use "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\other\cohort profile\G0\mother\mz_6a.dta", clear
sort aln
gen in_mz=1
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\a_3e.dta", keepus(a006 a525) nogen 
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\b_4f.dta", keepus(b003 b007 - b012 b023 b024 b032 b122 b123 b370 b650 b663 - b667 b923 b925  b043-b046 b100 b101 b104-b105) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\c_8a.dta", keepus(c101 c600 c645a c755 c765 c800 - c804 c994 c052 c053 c090-c092) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\d_4b.dta", keepus(dw042 d010 d010a d171 d171a) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\e_4f.dta", keepus(e390 e691 e695 e100 e101) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\f_2b.dta", keepus(f085 f088 f200 f253 f253a) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\g_5c.dta", keepus(g145 g146 g153 g154 g155 g333 g333a) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\h_6d.dta", keepus(h100 h101 h104a h104b h104c h104d h104e h115f h243 h243a) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\j_5b.dta", keepus(j130 j131b j132b j133b j134b j135b j136b j137 j333 j333a) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\k_r1b.dta", keepus(k1220 k1221 k1231 k1241 k1251 k1261 k1271 k1281 k1306 k4033) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\l_r1b.dta", keepus(l3280 l4033) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\m_2a.dta", keepus(m4060 m4061 m4062 m4063 m4065 m4100 m4101 m4110 m4120 m4130 m4140 m4150 m4160 m4206) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\n_3a.dta", keepus(n1120 n5000 n5002 n5008) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\p_r1b.dta", keepus(p2030 p2033) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\r_r1b.dta", keepus(r2080 r5030 r5033 r6010 r6016 r6018) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\t_2a.dta", keepus(t3329 t3332 t3333 t4510 t4540 t4541 t4542 t4543 t4545 t4560 t4561 t4562 t4906) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\v_2a.dta", keepus(V4515 V4520 V4522 V4524 V4526 V4528 V4530 V4532 V4534 V4536 V4538 V4906) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\current\other\obstetric\G0\mother\OB_2a.dta", keepus(DEL_P1160 DEL_P1154 DEL_P1210 DEL_P1212 DEL_P1010 DEL_P50 DEL_P1427 DEL_P1428 DEL_P1060 DEL_P1061)  nogen

order aln mz010a, first
order bestgest, last


*keep only those pregnancies enrolled in ALSPAC
keep if preg_enrol_status < 3

* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before bestgest, so replace the *** line above with additional variables. 
* If none are required remember to delete the *** line.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file. Note that mother based WoCs are set to .a

order aln mz010a, first
order bestgest, last

do "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\syntax\Withdrawal of consent\mother_WoC.do"

* Check withdrawal of consent frequencies=30 and baseline number is 15447
tab1 mz010a, mis

save "mother.dta", replace



*****************************************************************************************************************************************************************************************************************************.
/* G0 PARTNER - ***UNBLOCK SECTION WHEN REQUIRED***
* G0 Partner files - include here all files related to the G0 partner/father


use "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\other\cohort profile\G0\mother\pz_1a.dta", clear
sort aln
/*merge in any additional datasets if required*/

keep aln partner_in_alspac partner_data partner_enrolled partner_in_core pz_mult pz_multid partner_changed partner_changed_when ///
/* add your variable list here; remove this line if you are not adding any extra*/
partner_age second_partner_age


*keep in just those enrolled partners
keep if partner_in_alspac >= 1


* Removing withdrawl of consent cases *** FOR LARGE DATASETS THIS CAN TAKE A FEW MINUTES
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file. Note that partner based WoCs are set to .c

order aln partner_in_alspac, first
order partner_age second_partner_age, last

do "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\syntax\Withdrawal of consent\partner_WoC.do"

* Check there is a total of n=12113 G0 partners.
* Check withdrawal of consent frequencies partner=5  - currently none of these are in the formal cohort
tab1 partner_in_alspac, mis

save "YOUR PATHNAME\partner.dta", replace 
*/





*****************************************************************************************************************************************************************************************************************************.
* G1 Child BASED files - in this section the following file types need to be placed:
* Mother completed Qs about YP
* Obstetrics file OA

* ALWAYS KEEP THIS SECTION EVEN IF ONLY CHILD COMPLETED REQUESTED, although you will need to remove the *****

use "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\other\cohort profile\G1\cp_3a.dta", clear
sort aln qlet
gen in_kz=1
/* remove this line and end of commenting on the next line to add in any other file names if relevant files are needed in this section
merge 1:1 aln qlet using "", nogen   */

merge 1:1 aln qlet using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\other\obstetric\G0\mother\OA_1c.dta", keepus(HDP preeclampsia prev_hyp gesthyp proturi pregnancy_diabetes v1dad1a2_result_anaemia v1dad1a1_anaemia) nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\current\other\obstetric\G1\OC_2a.dta", keepus(DEL_B4003 DEL_B4004 DEL_B4005) nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G1\proxy\ka_5a.dta", keepus(ka035 ka014) nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G1\proxy\kb_7b.dta", keepus(kb279 kb280) nogen
merge 1:1 aln qlet using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G1\proxy\kc_6a.dta", keepus(kc403 kc404) nogen


* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before in_core, so replace the ***** line with additional variables.
* If none are required remember to delete the ***** line.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file

order aln qlet kz021, first
order in_alsp tripquad, last

do "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\syntax\Withdrawal of consent\child_based_WoC.do"

* Check withdrawal of consent frequencies child based=32 (two mums of twins have withdrawn consent)
tab1 kz021, mis

save "childB.dta", replace

*****************************************************************************************************************************************************************************************************************************.
* G1 Child COMPLETED files - in this section the following file types need to be placed:
* YP completed Qs
* Puberty Qs
* Child clinic data
* Child biosamples data
* School Qs
* Obstetrics file OC
* G1 IMD for years goes in this section e.g. jan1999imd2010_crimeq5_YP
* Child longitudinal data

* If there are no child completed files, this section can be starred out.
* NOTE: having to keep kz021 tripquad just to make the withdrawal of consent work - these are dropped for this file as the ones in the child BASED file are the important ones and should take priority

use "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\other\cohort profile\G1\cp_3a.dta", clear
sort aln qlet
gen in_kz=1


keep aln qlet kz021 ///
tripquad

* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before tripquad, so replace the ***** line with additional variables.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file.  Note that mother based WoCs are set to .b

order aln qlet kz021, first
order tripquad, last

do "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\syntax\Withdrawal of consent\child_completed_WoC.do"

* Check withdrawal of consent frequencies child completed=32
tab1 kz021, mis

drop kz021 tripquad
save "childC.dta", replace

*****************************************************************************************************************************************************************************************************************************.
** Matching all data together and saving out the final file*.
* NOTE: any linkage data should be added here*.

use "childB.dta", clear
merge 1:1 aln qlet using "childC.dta", nogen
merge m:1 aln using "mother.dta", nogen
* IF partner data is required please unstar the following line
/* merge m:1 aln using "YOUR PATHWAY\partner.dta", nogen */


* Remove non-alspac children.
drop if in_alsp!=1.

* Remove trips and quads.
drop if tripquad==1
* Comments by PH: 15645 (count) + 13 (tripquad) = 15658

drop in_alsp tripquad
save "dataset_for_casewhen.dta", replace

*****************************************************************************************************************************************************************************************************************************.
* QC checks*
use "dataset_for_casewhen.dta", clear

* Check that there are 15645 records.
count



******************************************************************************************************************************************************************************************************************************.
** Keep variables needed - those current in "temp_dataset_for_casewhen.csv" at the time script was first created
keep aln qlet mz010a mz005a mz011a mz028b b007 b008 b009 b010 b011 b012 b024 b122 b123 b370 b043-b046 b100 b101 b104-b105 c052 c053 c090-c092 c101 c600 c994 d010 d010a dw042 d171 d171a e390 e100 e101 f088 f200 f253 f253a ///
g153 g154 g155 g333 g333a ///
h243 h243a h104a h104b h104c h104d h104e h115f j131b j132b j133b j134b j135b j136b j137 j333a k1231 k1241 k1251 k1261 k1271 k1281 k1306 k4033 l4033 m4061 m4063 m4065 m4110 m4120 m4130 m4140 m4150 m4160 m4206 ///
p2033 r5033 t3332 t3333 t4541 t4542 t4543 t4545 t4560 t4561 t4562 t4906 V4520 V4522 V4524 V4526 V4528 V4530 V4532 V4534 V4536 V4538 V4906 bestgest ///
DEL_P50 DEL_P1010 DEL_P1060 DEL_P1154 DEL_P1160 DEL_P1210 DEL_P1212 DEL_P1427 DEL_P1428 DEL_P1061 HDP preeclampsia prev_hyp gesthyp proturi pregnancy_diabetes ///
v1dad1a2_result_anaemia v1dad1a1_anaemia DEL_B4003 DEL_B4004 DEL_B4005 kz030 kz010 ka035 ka014 kb279 kb280 kc403 kc404 kz021 mz005k


** Recode missing coded as .a or .b
foreach x of varlist kz021 - bestgest {
	replace `x'=. if `x'==.a | `x'==.b
}
*[Note: this dataset countains missing values coded as negative numbers, as well as .]

gen unique = 1 if mz005k==-2 | mz005k==2 | mz005k==3
*[Note: when analysing only mothers, restrict to unique mothers based on definition of mz005k - more information on this variable in the ALSPAC documentation]


gen singleton = 0 if mz010==0 | DEL_P50==1
replace singleton=1 if mz010==1 | DEL_P50==2
lab var singleton "singleton"
*[Note: when need to restrict to singleton, this can be used]

save "raw_out.dta", replace


**** Remove datasets 
rm childB.dta
rm childC.dta
rm mother.dta