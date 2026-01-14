/*******************************************************************************
ADAPTED FROM: "4.syntax_template_covars.do" written by the MR-PREG team
Added in more raw variables for project-specific analysis
*******************************************************************************/

********************************************************************************
********** ALSPAC SYNTAX - MR-PREG **********
*********************************************

cd "Z:\working\data\ALSPAC"

*** Syntax template for direct users preparing datasets using child and parent based datasets.
* Last update:
* Updated 25th July 2023 - updated withdrawal of consent frequencies (mother and child based)



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
keep aln mz010a preg_in_alsp preg_in_core preg_enrol_status mz005l mz005m mz013 mz014 mz028b mz010a mz005k bestgest
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\a_3e.dta", keepus(a222-a234 a420-a424 a525) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\b_4f.dta", keepus(b032 b043-b046 b100-b105 b140-b149 b500-b516 b634 b635 b636 b650-b671 b683-b691 b720-b723 b730 b742-b763) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\c_8a.dta", keepus(c052 c053 c090-c092 c110-c373 c481a c482 c490-c497 c503 c504 c645a c666a c755 c765 c800-c804 c3800-c3844) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\d_4b.dta", keepus(dw002 dw021 dw042) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\mother\e_4f.dta", keepus(e151-e157 e178 e220 e221) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\other\geo\G0\mother\G0_IMD_1b.dta", nogen keepus(aTownsendq5 bTownsendq5 cTownsendq5 aimd2000q5 bimd2000q5 cimd2000q5)
merge 1:1 aln using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\current\other\obstetric\G0\mother\OB_2a.dta", keepus(DEL_P50) nogen

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

* Check withdrawal of consent frequencies=29 and baseline number is 15447
tab1 mz010a, mis

save "mother.dta", replace

*****************************************************************************************************************************************************************************************************************************.

* G0 PARTNER
* G0 Partner files - include here all files related to the G0 partner/father


use "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\other\cohort profile\G0\partner\pz_1a.dta", clear
sort aln
/*merge in any additional datasets if required*/

keep aln partner_in_alspac partner_data partner_enrolled partner_in_core pz_mult pz_multid partner_changed partner_changed_when partner_age second_partner_age
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\partner\pa_4a.dta", keepus(pa065 pa070 pa071 pa910 paw002 paw010) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\partner\pb_4b.dta", keepus(pb013 pb014 pb020-pb070 pb071-pb079 pb099-pb101 pb910) nogen
merge 1:1 aln using "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\current\quest\G0\partner\pc_3a.dta", keepus(pc260 pc280 pc281) nogen

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

save "partner.dta", replace 

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

* Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before in_core, so replace the ***** line with additional variables.
* If none are required remember to delete the ***** line.
* An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file

order aln qlet kz021, first
order in_alsp tripquad, last

do "\\ads.bris.ac.uk\filestore\SSCM ALSPAC\Data\syntax\Withdrawal of consent\child_based_WoC.do"

* Check withdrawal of consent frequencies child based=31 (two mums of twins have withdrawn consent)
tab1 kz021, mis

save "childB.dta", replace

*****************************************************************************************************************************************************************************************************************************.
** Matching all data together and saving out the final file*.
* NOTE: any linkage data should be added here*.

use "childB.dta", clear
merge m:1 aln using "mother.dta", nogen
merge m:1 aln using "partner.dta", nogen
merge m:1 aln qlet using "raw_out.dta", nogen

* Remove non-alspac children.
drop if in_alsp!=1.

* Remove trips and quads.
drop if tripquad==1

drop in_alsp tripquad

*****************************************************************************************************************************************************************************************************************************.
* Check that there are 15645 records.
count

capture drop unique
gen unique = 1 if mz005k==-2 | mz005k==2 | mz005k==3
*[Note: when analysing only mothers, restrict to unique mothers based on definition of mz005k - more information on this variable in the ALSPAC documentation]

capture drop singleton
gen singleton = 0 if mz010==0 | DEL_P50==1
replace singleton=1 if mz010==1 | DEL_P50==2
lab var singleton "singleton"
*[Note: when need to restrict to singleton, this can be used]

save "raw_exp_cov_out.dta", replace


**** Remove datasets 
rm childB.dta
rm mother.dta
rm partner.dta