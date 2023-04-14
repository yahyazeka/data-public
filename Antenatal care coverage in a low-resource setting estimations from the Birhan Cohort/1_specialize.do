// Specialized datasets for ANC coverage manuscript //

/* Last updated on April 14, 2023 by CPD */

//// Specialized dataset - ANC visits attended at different timepoints ////
c gdrive
clear
clear mata
set maxvar 32767

use "24.1. De-identified Birhan Data\5. Data sharing\clara_anc_04132023\MCH/all_data_combined", clear

*keeping only women enrolled during pregnancy
keep if enroll == "p1"

*removing duplicated women due to twin pregnancies
duplicates drop uuid mch_preg intdt_enroll, force

*merging with gestational age dataset
merge 1:1 uuid intdt_enroll using "24.1. De-identified Birhan Data\5. Data sharing\clara_anc_04132023\MCH\specialize_mch_ga_unique_uuid"

*removing duplicated women with more than 1 pregnancy during study period
duplicates list uuid
bysort uuid (intdt_enroll): gen rep_ent = cond(_N==1,0,_n)
drop if rep_ent==2

*converting date variables
local dates "dodel conc_date intdt_enroll ga_date intdt_pf2_1 intdt_pf2_2 intdt_pf2_3 intdt_pf3_1 intdt_pf3_2 intdt_pf3_3 intdt_pf3_4 intdt_pf4_1 intdt_pf4_2 intdt_pf4_3 intdt_pf5_1 intdt_pf5_2 intdt_pf5_3 vdate_pf2_1 vdate_pf2_2 vdate_pf2_3 vdate_pf3_1 vdate_pf3_2 vdate_pf3_3 vdate_pf3_4 vdate_pf4_1 vdate_pf4_2 vdate_pf4_3 vdate_pf5_1 vdate_pf5_2 vdate_pf5_3 intdt_pc2_1 intdt_pc2_10 intdt_pc2_2 intdt_pc2_3 intdt_pc2_4 intdt_pc2_5 intdt_pc2_6 intdt_pc2_7 intdt_pc2_8 intdt_pc2_9 intdt_pc2b_1 intdt_pc2b_2 intdt_pc3_1 intdt_pc3_2 intdt_pc3_3 intdt_pc3_4 intdt_pc3_5 intdt_pc3_6 intdt_pc3_7 intdt_pf6_1 intdt_pf6_2 intdt_pf6_3 intdt_pf6_4 intdt_pf7_1 intdt_pf7_2 intdt_pf8_1 intdt_pf8_2"
foreach var in `dates'{
	gen `var'_new = date(`var', "DM20Y")
	format `var'_new %td
	drop `var'
	rename `var'_new `var'
}

*keeping only deliveries before covid
keep if dodel<=date("9Apr2020", "DM20Y") | missing(dodel)

*cleaning: dropping implausible gestational ages and women lost to follow-up with no gestational age at delivery
drop if (ga_dodel>=46 | ga_dodel<28) & ga_dodel!=. 
gen ga_enroll= (ga*7 + (intdt_enroll-ga_date))/7
drop if ga_enroll<=0 | ga_enroll==.

*keeping relevant variables
keep uuid mch_preg intdt_enroll enroll intdt* vdate_p* nancvis* ga_enroll conc_date dodel ga_dodel ga_date ga

*replacing interview date with visit date for retrospectively recorded ANC visits
local values "_pf2_1 _pf2_2 _pf2_3 _pf3_1 _pf3_2 _pf3_3 _pf3_4 _pf4_1 _pf4_2 _pf4_3 _pf5_1 _pf5_2 _pf5_3" 
foreach val of local values {
replace intdt`val' = vdate`val' if vdate`val' < intdt`val'
}

*removing retrospective ANC visits (i.e. those recorded before enrollment) to avoid double counting these with visits reported at enrollment
foreach var2 of varlist intdt_pf* {
replace `var2' = . if `var2' < intdt_enroll
}

*Counting number of ANC visits attended at different timepoints
foreach val2 of local values {
gen ga_intdt`val2' = (intdt`val2'-conc_date)/7
gen intdt_2ANC`val2' = intdt`val2'
replace intdt_2ANC`val2' = . if (ga_intdt`val2' >=29 | ga_intdt`val2'<17) & !missing(ga_intdt`val2')
gen intdt_3ANC`val2' = intdt`val2'
replace intdt_3ANC`val2' = . if (ga_intdt`val2' < 29 | ga_intdt`val2' >= 36) & !missing(ga_intdt`val2')
gen intdt_4ANC`val2' = intdt`val2'
replace intdt_4ANC`val2' = . if ga_intdt`val2' <36  & !missing(ga_intdt`val2')
}

replace nancvis_enroll = 0 if missing(nancvis_enroll)

egen anc = rownonmiss(intdt_pf2_1 intdt_pf2_2 intdt_pf2_3 intdt_pf3_1 intdt_pf3_2 intdt_pf3_3 intdt_pf3_4 intdt_pf4_1 intdt_pf4_2 intdt_pf4_3 intdt_pf5_1 intdt_pf5_2 intdt_pf5_3)
egen anc_rec1 = rownonmiss(intdt_2ANC_pf*)
egen anc_rec2 = rownonmiss(intdt_3ANC_pf*)
egen anc_rec3 = rownonmiss(intdt_4ANC_pf*)

replace anc_rec1 = 0 if missing(anc)
replace anc_rec2 = 0 if missing(anc)
replace anc_rec3 = 0 if missing(anc)

*Generating variable to classify women based on their GA at enrollment
gen enrollment = 1 if ga_enroll<5 & ga_enroll>0
replace enrollment = 2 if ga_enroll>=5 & ga_enroll<9
replace enrollment = 3 if ga_enroll>=9 & ga_enroll<13
replace enrollment = 4 if ga_enroll >=13 & ga_enroll<17
replace enrollment = 5 if ga_enroll >=17 & ga_enroll<21
replace enrollment = 6 if ga_enroll >=21 & ga_enroll<25
replace enrollment = 7 if ga_enroll >=25 & ga_enroll<29
replace enrollment = 8 if ga_enroll >=29 & ga_enroll<33
replace enrollment = 9 if ga_enroll >=33 & ga_enroll<37
replace enrollment = 9 if ga_enroll >=37

*last time we saw a participant in the cohort
egen intdt_last_visit = rowmax(intdt_pc2_1 intdt_pc2_10 intdt_pc2_2 intdt_pc2_3 intdt_pc2_4 intdt_pc2_5 intdt_pc2_6 intdt_pc2_7 intdt_pc2_8 intdt_pc2_9 intdt_pc2b_1 intdt_pc2b_2 intdt_pc3_1 intdt_pc3_2 intdt_pc3_3 intdt_pc3_4 intdt_pc3_5 intdt_pc3_6 intdt_pc3_7 intdt_pf2_1 intdt_pf2_2 intdt_pf2_3 intdt_pf3_1 intdt_pf3_2 intdt_pf3_3 intdt_pf3_4 intdt_pf4_1 intdt_pf4_2 intdt_pf4_3 intdt_pf5_1 intdt_pf5_2 intdt_pf5_3 intdt_pf6_1 intdt_pf6_2 intdt_pf6_3 intdt_pf6_4 intdt_pf7_1 intdt_pf7_2 intdt_pf8_1 intdt_pf8_2) // date of last time a woman was visited as part of the cohort
replace intdt_last_visit = intdt_enroll if intdt_last_visit==.
format intdt_last_visit %td
gen ga_last_visit = (((intdt_last_visit - ga_date) + (ga*7))/7) if missing(dodel)

*reshape long
reshape long anc_rec, i(uuid) j(value)

*creating variable for at least 1 ANC visit in each timepoint/window
gen anc_rec_1 = 1 if anc_rec>=1 & !missing(anc_rec)
replace anc_rec_1 = 0 if anc_rec==0

*keeping relevant variables
keep uuid mch_preg intdt_enroll enroll ga_enroll nancvis_enroll anc_rec* dodel ga_dodel enrollment value ga_last_visit

*export to csv file
compress
label drop _all
outsheet using "24. Birhan Data/4. Specialize/Manuscripts/ANC_tests/anc_timepoints.csv", comma  replace

*---------------------------------------------------------------------------------------------------
//// Specialized dataset - ANC visits attended before enrollment, retrospectively collected ////
c gdrive
clear
clear mata
set maxvar 32767

use "24.1. De-identified Birhan Data\5. Data sharing\clara_anc_04132023\MCH/all_data_combined", clear

*keeping only women enrolled during pregnancy
keep if enroll == "p1"

*removing duplicated women due to twin pregnancies
duplicates drop uuid mch_preg intdt_enroll, force

*merging with gestational age dataset
merge 1:1 uuid intdt_enroll using "24.1. De-identified Birhan Data\5. Data sharing\clara_anc_04132023\MCH\specialize_mch_ga_unique_uuid"

*converting date variables
local dates "conc_date intdt_enroll ga_date intdt_pf2_1 intdt_pf2_2 intdt_pf2_3 intdt_pf3_1 intdt_pf3_2 intdt_pf3_3 intdt_pf3_4 intdt_pf4_1 intdt_pf4_2 intdt_pf4_3 intdt_pf5_1 intdt_pf5_2 intdt_pf5_3 vdate_pf2_1 vdate_pf2_2 vdate_pf2_3 vdate_pf3_1 vdate_pf3_2 vdate_pf3_3 vdate_pf3_4 vdate_pf4_1 vdate_pf4_2 vdate_pf4_3 vdate_pf5_1 vdate_pf5_2 vdate_pf5_3 intdt_pc2_1 intdt_pc2_10 intdt_pc2_2 intdt_pc2_3 intdt_pc2_4 intdt_pc2_5 intdt_pc2_6 intdt_pc2_7 intdt_pc2_8 intdt_pc2_9 intdt_pc2b_1 intdt_pc2b_2 intdt_pc3_1 intdt_pc3_2 intdt_pc3_3 intdt_pc3_4 intdt_pc3_5 intdt_pc3_6 intdt_pc3_7 intdt_pf6_1 intdt_pf6_2 intdt_pf6_3 intdt_pf6_4 intdt_pf7_1 intdt_pf7_2 intdt_pf8_1 intdt_pf8_2"
foreach var in `dates'{
	gen `var'_new = date(`var', "DM20Y")
	format `var'_new %td
	drop `var'
	rename `var'_new `var'
}

*generating gestational age at enrollment
gen ga_enroll = (intdt_enroll-conc_date)/7

keep uuid mch_preg intdt_enroll enroll intdt_pf2* intdt_pf3* intdt_pf4* intdt_pf5* vdate_p* nancvis* ga ga_date ga_enroll conc_date

*Creating variables for retrospective visits (i.e. those recorded before enrollment)
local values "_pf2_1 _pf2_2 _pf2_3 _pf3_1 _pf3_2 _pf3_3 _pf3_4 _pf4_1 _pf4_2 _pf4_3 _pf5_1 _pf5_2 _pf5_3" 
foreach var of varlist vdate_pf* {
gen `var'_re = `var' if `var' < intdt_enroll
}

*Counting number of retrospective ANC visits
gen anc_retro=0
foreach val2 of local values {
replace anc_retro = anc_retro + 1 if vdate`val2'_re!=. 
}

*gestational age at first retrospective visits
egen first_retro = rowmin(vdate_pf2_1_re vdate_pf2_2_re vdate_pf2_3_re vdate_pf3_1_re vdate_pf3_2_re vdate_pf3_3_re vdate_pf3_4_re vdate_pf4_1_re vdate_pf4_2_re vdate_pf4_3_re vdate_pf5_1_re vdate_pf5_2_re vdate_pf5_3_re)

local dates "first_retro vdate_pf2_1_re vdate_pf2_2_re vdate_pf2_3_re vdate_pf3_1_re vdate_pf3_2_re vdate_pf3_3_re vdate_pf3_4_re vdate_pf4_1_re vdate_pf4_2_re vdate_pf4_3_re vdate_pf5_1_re vdate_pf5_2_re vdate_pf5_3_re"
foreach var in `dates'{
	format `var' %td
}

gen ga_first_retro = (first_retro - conc_date)/7 if first_retro!=.

*exporting csv file
compress
label drop _all
keep uuid intdt_enroll enroll mch_preg nancvis_enroll anc_retro first_retro ga_first_retro
outsheet using "24. Birhan Data/4. Specialize/Manuscripts/ANC_tests/anc_retro.csv", comma  replace

*---------------------------------------------------------------------------------------------------
//// Specialized long datasets for alluvial plot ////
c gdrive
clear
clear mata
set maxvar 32767

use "24.1. De-identified Birhan Data\5. Data sharing\clara_anc_04132023\MCH/all_data_combined", clear

*keeping only women enrolled during pregnancy
keep if enroll == "p1"

*removing duplicated women due to twin pregnancies
duplicates drop uuid mch_preg intdt_enroll, force

*merging with gestational age dataset
merge 1:1 uuid intdt_enroll using "24.1. De-identified Birhan Data\5. Data sharing\clara_anc_04132023\MCH\specialize_mch_ga_unique_uuid"

*removing duplicated women with more than 1 pregnancy during study period
duplicates list uuid
bysort uuid (intdt_enroll): gen rep_ent = cond(_N==1,0,_n)
drop if rep_ent==2

*converting date variables
local dates "dodel conc_date intdt_enroll ga_date intdt_pf2_1 intdt_pf2_2 intdt_pf2_3 intdt_pf3_1 intdt_pf3_2 intdt_pf3_3 intdt_pf3_4 intdt_pf4_1 intdt_pf4_2 intdt_pf4_3 intdt_pf5_1 intdt_pf5_2 intdt_pf5_3 vdate_pf2_1 vdate_pf2_2 vdate_pf2_3 vdate_pf3_1 vdate_pf3_2 vdate_pf3_3 vdate_pf3_4 vdate_pf4_1 vdate_pf4_2 vdate_pf4_3 vdate_pf5_1 vdate_pf5_2 vdate_pf5_3 intdt_pc2_1 intdt_pc2_10 intdt_pc2_2 intdt_pc2_3 intdt_pc2_4 intdt_pc2_5 intdt_pc2_6 intdt_pc2_7 intdt_pc2_8 intdt_pc2_9 intdt_pc2b_1 intdt_pc2b_2 intdt_pc3_1 intdt_pc3_2 intdt_pc3_3 intdt_pc3_4 intdt_pc3_5 intdt_pc3_6 intdt_pc3_7 intdt_pf6_1 intdt_pf6_2 intdt_pf6_3 intdt_pf6_4 intdt_pf7_1 intdt_pf7_2 intdt_pf8_1 intdt_pf8_2"
foreach var in `dates'{
	gen `var'_new = date(`var', "DM20Y")
	format `var'_new %td
	drop `var'
	rename `var'_new `var'
}

*cleaning: dropping implausible gestational ages, women lost to follow-up and those who delivered after pandemic onset
drop if (ga_dodel>=46 | ga_dodel<28) & ga_dodel!=. 
gen ga_enroll= (ga*7 + (intdt_enroll-ga_date))/7
su ga_enroll
drop if ga_enroll<=0 | ga_enroll==.
drop if dodel==.
keep if dodel<=date("9Apr2020", "DM20Y")

*Replacing interview date with visit date for retrospectively recorded ANC visits
local values "_pf2_1 _pf2_2 _pf2_3 _pf3_1 _pf3_2 _pf3_3 _pf3_4 _pf4_1 _pf4_2 _pf4_3 _pf5_1 _pf5_2 _pf5_3" 
foreach val of local values {
replace intdt`val' = vdate`val' if vdate`val' < intdt`val'
}

*Removing retrospective ANC visits (i.e. those recorded before enrollment) to avoid double counting these with visits reported at enrollment
foreach var of varlist intdt_pf* {
replace `var' = . if `var' < intdt_enroll
}

*gestational age at each ANC visit
foreach var in `values'{
	gen ga`var'=((intdt`var'-ga_date) + (ga*7))/7
}

*new variables for ANC attendance in each time window
gen valueANC1=2 // 2 = No ANC in that window time
foreach var in `values'{
	replace valueANC1=3 if !missing(intdt`var') & ga`var'<16 // 3 = ANC in that window time
	replace valueANC1=1 if ga_enroll>=16 & !missing(ga_enroll) // 1 = enrolled after that window time
}

gen valueANC2=2
foreach var in `values'{
	replace valueANC2=3 if !missing(intdt`var') & ga`var'>=16 & ga`var'<28
	replace valueANC2=1 if ga_enroll>=28 & !missing(ga_enroll)
}

gen valueANC3=2
foreach var in `values'{
	replace valueANC3=3 if !missing(intdt`var') & ga`var'>=28 & ga`var'<36
	replace valueANC3=1 if ga_enroll>=36  & !missing(ga_enroll)
}

gen valueANC4=2
foreach var in `values'{
	replace valueANC4=3 if !missing(intdt`var') & ga`var'>=36
	replace valueANC4=0 if ga_dodel<36 // 0 = delivered before that window time

}

*reshape
reshape long value, i(uuid) j(visit) string

*replace to ANC1 visit == yes if woman reported prior visits at enrollment and she was enrolled in the first trimester
replace value=3 if visit=="ANC1" & value==2 & nancvis_enroll!=0 & !missing(nancvis_enroll) & ga_enroll<13

*keeping relevant variables
keep uuid value visit ga_enroll

*exporting csv file
export delimited using "24. Birhan Data\4. Specialize\Manuscripts\ANC_tests\anc_alluvial.csv", replace