/*******************************************************************************
ADAPTED FROM: "3.alspac_dataset_mrpreg_20240611_zanthro.do" written by the MR-PREG team
*******************************************************************************/

	*install zanthro
*sysdir set PLUS C:\Users\rt20627\Desktop\Stata_packages
*ssc install zanthro
*ssc install: "zanthro" not found at SSC, type search zanthro
*(To find all packages at SSC that start with z, type ssc describe z)
*search zanthro
*package name:  dm0004_1.pkg
*from:  http://www.stata-journal.com/software/sj13-2/
*  checking dm0004_1 consistency and verifying not already installed...
*installing into C:\Users\rt20627\Desktop\Stata_packages\...
*installation complete.

cd "Z:\working\data\ALSPAC"

clear

*load the file from R
use pheno_all_Rvars, clear

*code as NA (.) those negative values
replace kz021 = . if kz021 == -1 | kz021==-2

*create var with age in years and BW in kg 
gen age_baby=0
gen bwt_kg=kz030/1000

*zanthro with UK-WHO charts
*gen zbw_UKWHO=.
*replace zbw_UKWHO=zanthro(bwt_kg,wa,UKWHOpreterm), xvar(age_baby) gender(kz021) gencode(male=1, female=2) gestage(ga_all) if singleton==1 & kz010>=2
*(zanthro does not seem to work with gen var replace if, but with egen only, so I do not know how to do restrict zanthro function to singletons and live births in STATA)
egen zbw_UKWHO=zanthro(bwt_kg,wa,UKWHOpreterm), xvar(age_baby) gender(kz021) gencode(male=1, female=2) gestage(ga_all)
*WARNING: Maximum value in your gestational age variable is 47 weeks.
*(Z values generated for 13484  cases) 
*(gender was assumed to be coded male=1, female=2)
*(age was assumed to be in years)
*(Z values can be missing because age is nonpositive or otherwise
 * out of range for the chart code, the gender variable is missing,
 * gestation age is missing or places corrected age out of range
 * for the chart code, or the Z value has an absolute value >=5)
*(2,161 missing values generated)

*zanthro with UK charts
*gen zbw_UK=.
*replace zbw_UK=zanthro(bwt_kg,wa,UK), xvar(age_baby) gender(kz021) gencode(male=1, female=2) gestage(ga_all)  if singleton==1 & kz010>=2
*(zanthro does not seem to work with gen var replace if, but with egen only, so I do not know how to do restrict zanthro function to singletons and live births in STATA)
egen zbw_UK=zanthro(bwt_kg,wa,UK), xvar(age_baby) gender(kz021) gencode(male=1, female=2) gestage(ga_all)
*WARNING: Maximum value in your gestational age variable is 47 weeks.
*(Z values generated for 13483 cases) 
*(gender was assumed to be coded male=1, female=2)
*(age was assumed to be in years)
*(Z values can be missing because age is nonpositive or otherwise
 * out of range for the chart code, the gender variable is missing,
 * gestation age is missing or places corrected age out of range
 * for the chart code, or the Z value has an absolute value >=5)
*(2,162 missing values generated)


*check distributions of the z-scores
hist(zbw_UK)
*(bin=41, start=-4.3719521, width=.22346626)

hist(zbw_UKWHO)
*(bin=41, start=-4.8573427, width=.23117385)


* SGA
gen sga_UKWHO=.
replace sga_UKWHO=1 if zbw_UKWHO <-1.282
replace sga_UKWHO=0 if zbw_UKWHO>=-1.282 & zbw_UKWHO!=.

gen sga_UK=.
replace sga_UK=1 if zbw_UK <-1.282
replace sga_UK=0 if zbw_UK>=-1.282 & zbw_UK!=.

* LGA
gen lga_UKWHO=.
replace lga_UKWHO=1 if zbw_UKWHO >1.282 & zbw_UKWHO!=.
replace lga_UKWHO=0 if zbw_UKWHO <=1.282

gen lga_UK=.
replace lga_UK=1 if zbw_UK >1.282 & zbw_UK!=.
replace lga_UK=0 if zbw_UK <=1.282


tab1 sga_UKWHO
tab1 sga_UK
tab1 lga_UKWHO
tab1 lga_UK

keep aln qlet sga_UKWHO sga_UK lga_UKWHO lga_UK

save pheno_all_vars.dta, replace
export delimited pheno_all_vars.csv, replace
