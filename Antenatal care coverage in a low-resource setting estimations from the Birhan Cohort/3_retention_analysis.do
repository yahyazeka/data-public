// Retention analysis for ANC manuscript //

/* Last updated on April 14, 2023 by CPD */

//// Retention in care results (table 3 of the manuscript) ////
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
merge 1:1 uuid intdt_enroll using "24. Birhan Data/4. Specialize/MCH Phase 1/specialize_mch_ga_unique_uuid"

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

*new variables for ANC attendance in each time window
foreach var in `values'{
	gen ga`var'=((intdt`var'-ga_date) + (ga*7))/7
}

gen valueANC1=2
foreach var in `values'{
	replace valueANC1=3 if !missing(intdt`var') & ga`var'<16
	replace valueANC1=1 if ga_enroll>=16 & !missing(ga_enroll)
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
	replace valueANC4=0 if ga_dodel<36

}

*reshape
reshape long value, i(uuid) j(visit) string

*replace to ANC1 visit == yes if woman reported prior visits at enrollment and she was enrolled in the first trimester
replace value=3 if visit=="ANC1" & value==2 & nancvis_enroll!=0 & !missing(nancvis_enroll) & ga_enroll<13

drop _merge

*reshape back
reshape wide value, i(uuid) j(visit) string

*keep only women enrolled <13 weeks
keep if ga_enroll<13

*********************************************************************************************
** RETENTION IN CARE ESTIMATES - ANC DROP-OFF **

*Lost after ANC 1 (<16 weeks)
count if valueANC1==3 & valueANC2==2 & valueANC3==2 & (valueANC4==2 | valueANC4==0)
scalar numerator1=r(N)
count if valueANC1==3
scalar denominator1=r(N)
di "n " numerator1 " N " denominator1 " % "  (numerator1/denominator1)*100

*Lost after ANC 2 (16 weeks to <28 weeks)
count if valueANC2==3 & valueANC3==2 & (valueANC4==2 | valueANC4==0)
scalar numerator2=r(N)
count if valueANC2==3
scalar denominator2=r(N)
di "n " numerator2 " N " denominator2 " % "  (numerator2/denominator2)*100

*Lost after ANC 3 (28 to <36 weeks)
count if valueANC3==3 & valueANC4==2
scalar numerator3=r(N)
count if valueANC3==3 & valueANC4!=0
scalar denominator3=r(N)
di "n " numerator3 " N " denominator3 " % "  (numerator3/denominator3)*100