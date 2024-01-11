* Study title: Estimates and determinants of health facility delivery in the Birhan maternal and child health cohort study, 2018–2020, Amhara region, Ethiopia
* Description: This code was written to define and re-categorize/ re-label variables and perform descriptive, univariate and multivariate analysis the dataset 
* for this study. This code has been annotated to explain what different sections of code accomplish and how.  

* Clearing the global environment
clear
clear matrix
clear mata
set maxvar 20000

* 1. Importing specialized community births dataset
import excel "H:\Shared drives\20. Birhan\11. Analysis\1. Manuscript analysis\Community births\x. Analysis output\delloc_dat.xlsx", sheet("Sheet1") firstrow
cd "H:\Shared drives\20. Birhan\11. Analysis\1. Manuscript analysis\Community births\x. Analysis output\Secondary Analysis"
numlabel, add

* 2. Applying wealth indices code to generate wealth index quintiles to study population
gen hhid=hhid_enroll
merge m:1 hhid using "H:\Shared drives\24. Birhan Data\4. Specialize\Census\specialized_census_wealthindex"
drop if _merge==2
drop _merge

* Adding labels
label define wealthind_5cat 1"Poorest" 2 "Poorer" 3 "Middle" 4 "Richer" 5 "Richest"
label values wealthindex_5cat wealthind_5cat
tab wealthindex_5cat

* 3. Adding  variables religion and ethnicity from census, recategorize and relabel
merge 1:1 uuid using "H:\Shared drives\24. Birhan Data\1. Census\1. Data\3. Indexed\census_ind.dta", keepusing(rel ethn)
drop if _merge==2
drop _merge
tab rel, missing
tab ethn, missing

* Categorizing and Labelling
gen rel_cat = 0 if rel ==  1 | rel == 3
replace rel_cat = 1 if rel ==2
label define religion_cat 0 "Christian" 1 "Muslim"
label values rel_cat religion_cat

gen ethn_cat = 0 if ethn == 1
replace ethn_cat = 1 if ethn == 2
replace ethn_cat =2 if ethn >2  & !missing(ethn)
label define ethnicity_cat 0 "Amhara" 1 "Oromo" 2 "Other"
label values ethn_cat ethnicity_cat

*4. Location of birth
*Fix error on location of birth for uuid 879b03cfc33c4dfa8953808b15a3f3d6 from community to facility birth based on data entered 
*in variable delloc_b and dellocos_b
replace location_b="facility" if uuid=="879b03cfc33c4dfa8953808b15a3f3d6"

*4a. Converting location of birth values from string to numerical and relabel
replace location_b = "1" if location_b == "facility"
replace location_b = "0" if location_b == "community"
destring location_b, replace
label variable location_b "birth location"
label define locationbirth 0 "Community" 1 "Facility"
label values location_b locationbirth
*N.B. Six births categorized as community are actually facility births from responses to variables wnwishdelhfos_b' and 'limdelhfos_b'. 
*1) uuid: 3f205473cc744b7c9a76fe05d28d5e40
*2) uuid: 95060094f4124a25881cd1acfbc518ba
*3) uuid: d5526b8be4084962a88fa5e79db7365b
*4) uuid: 9bb95218-49f2-4999-9cbb-9115bed82fd3
*5) uuid: a808b197385f471faa431b89973871f5
*6) uuid: d4c07663066a4ba490594e82316eaeba

*4b. Stratifying community birth by 'home' or 'on the way to facility'
gen birth_location = 0 if location_b == 0
replace birth_location = 1 if facility_b == 1 | facility_b == 2 | facility_b == 66
replace birth_location = 2 if delloc_b == 2
label define exact_loc 0 "Home" 1 "Facility" 2 "On the way to facility"
label values birth_location exact_loc
*correct birth location for IDs noted above in 4 and 4a
replace birth_location = 1 if uuid=="879b03cfc33c4dfa8953808b15a3f3d6" | uuid=="a808b197385f471faa431b89973871f5" | uuid=="d4c07663066a4ba490594e82316eaeba"

*4c. Delivery attendant / Skilled delivery attendance
gen skilled_att = 0 if pdelatt == 1 | pdelatt == 2 | pdelatt == 3 
replace skilled_att = 1 if pdelatt == 4 | delvatd >0 & delvatd <7
replace skilled_att = 2 if pdelatt == 66 | pdelatt == 88| delvatd == 66 | delvatd == 88
replace skilled_att = 1 if uuid=="9092ad8ac19442beb98969ed50ab962c" | uuid=="97e8684eefd7476795270c5dcdb08b01" | uuid=="a181f4d499ed46e0b156285222feed81"
label define skilldel_att 0 "Non-skilled" 1 "Skilled" 2 " Other/Unknown"
label values skilled_att skilldel_att
*Three ids come up with unknown skilled_att for facility delivery. Looking into the variable pdelattos_b, these were all delivered by skilled professional (doctor/midwife)
*1) uuid: 9092ad8ac19442beb98969ed50ab962c
*2) uuid: 97e8684eefd7476795270c5dcdb08b01
*3) uuid: a181f4d499ed46e0b156285222feed81
*Two mothers were delivered by a HEW in a facility (uuid: 5e8f702bd0a844eeb786e376bb413216 and a7fb139c4a0c460b9174fb034dbaa4c3


*4d. Type of delivery attendant in community (home; on the way)
merge 1:m uuid enroll intdt_enroll dodel intdt_b dcid_b delloc_b facility_b nstillbirth_enroll using "H:\Shared drives\24. Birhan Data\3.MCH\1. Data\3. Indexed\all_data_combined.dta", keepusing(pdelattos)
drop if _merge==2
drop _merge

*To look at duplicates 'duplicates list uuid', 'sort uuid'
duplicates drop uuid, force

* Type of delivery attendant: recoding responses for other specify (community birth attendant)
replace  pdelattos = "3" if pdelattos == "Amati weym yebaluwa enat" | pdelattos == "Gorebatocha nachew" | pdelattos == "Gorebet" | pdelattos == "ጎረቤት" 
replace  pdelattos = "4" if pdelattos == "Doctor" | pdelattos == "Midwifery or doctor" | pdelattos == "Gorebet" | pdelattos == "ጎረቤት" 
replace pdelattos = "0" if pdelattos != "3" & pdelattos != "4" & !missing(pdelattos) 
destring pdelattos, replace 
label define att_os 0 "No attendant/alone" 3 "Family member" 4 " Health Professional"
label values pdelattos att_os

* Generate variable with all community delivery attendant responses (including other type of attendant specify recoded)
gen delivatt_comm = .
replace delivatt_comm = 0 if pdelattos == 0 & birth_location == 0 | pdelattos == 0 & birth_location ==2 | pdelatt_b == 0 & birth_location == 0 | pdelatt_b ==0 & birth_location ==2
replace delivatt_comm = 1 if pdelattos == 1 & birth_location == 0 | pdelattos == 1 & birth_location ==2 | pdelatt_b== 1 & birth_location == 0 | pdelatt_b == 1 & birth_location == 2 
replace delivatt_comm = 2 if pdelattos == 2 & birth_location == 0 | pdelattos == 2 & birth_location ==2 | pdelatt_b== 2 & birth_location == 0 | pdelatt_b == 2 & birth_location == 2 
replace delivatt_comm = 3 if pdelattos == 3 & birth_location == 0 | pdelattos == 3 & birth_location ==2 | pdelatt_b== 3 & birth_location == 0 | pdelatt_b == 3 & birth_location == 2  
replace delivatt_comm = 4 if pdelattos == 4 & birth_location == 0 | pdelattos == 4 & birth_location ==2 | pdelatt_b== 4 & birth_location == 0 | pdelatt_b == 4 & birth_location == 2 
replace delivatt_comm = 88 if pdelattos == 88 & birth_location == 0 | pdelattos == 88 & birth_location ==2 | pdelatt_b== 88 & birth_location == 0 | pdelatt_b == 88 & birth_location == 2  
label define comm_typeattendant 0 "No attendant/alone" 1 "Traditional Birth Attendant (TBA)" 2 "Health Extension Worker (HEW)" 3 "Family member" 4 "Health Professional" 66 "Other" 88 "Unknown"
label values delivatt_comm comm_typeattendant
*These home births were not attended by health professionals but by family members (IDs below:)
*1) uuid:4bd58e3f669a475c8c333cd39e80dcc4
*2) uuid: 76adbde76dd545159f24905c5e8f3fe8
*When these are accounted for, there are no home births that were attended by health professionals.


*Skilled and non skilled for community delivery (home + on the way)
gen skilledatt_comm = 0 if delivatt_comm == 0 | delivatt_comm == 1 | delivatt_comm == 2 | delivatt_comm == 3 
replace skilledatt_comm = 1 if delivatt_comm == 4
replace skilledatt_comm = 2 if delivatt_comm == 88
label define comm_skilldelatt 0 "Non-skilled" 1 "Skilled" 2 " Unknown"
label values skilledatt_comm comm_skilldelatt

* Overall Delivery attendant / Skilled delivery attendance

gen allskill_att = 0 if skilled_att == 0 | skilledatt_comm == 0
replace allskill_att = 1 if skilled_att == 1 | skilledatt_comm == 1
replace allskill_att = 2 if skilledatt_comm == 2
label define allskilldel_att 0 "Non-skilled" 1 "Skilled" 2 " Other/Unknown"
label values allskill_att allskilldel_att


* Type of delivery attendant by location: 'home' only
gen delivatt_home = .
replace delivatt_home = 0 if birth_location == 0 & delivatt_comm == 0
replace delivatt_home = 1 if birth_location == 0 & delivatt_comm == 1
replace delivatt_home = 2 if birth_location == 0 & delivatt_comm == 2
replace delivatt_home = 3 if birth_location == 0 & delivatt_comm == 3
replace delivatt_home = 4 if birth_location == 0 & delivatt_comm == 4
replace delivatt_home = 88 if birth_location == 0 & delivatt_comm == 88
label define home_typeattendant 0 "No attendant/alone" 1 "Traditional Birth Attendant (TBA)" 2 "Health Extension Worker (HEW)" 3 "Family member" 4 "Health Professional" 66 "Other" 88 "Unknown"
label values delivatt_home home_typeattendant

*Skilled and non skilled for home delivery: 'home' only
gen skilledatt_home = 0 if delivatt_home == 0 | delivatt_home == 1 | delivatt_home == 2 | delivatt_home == 3 
replace skilledatt_home = 1 if delivatt_home == 4
replace skilledatt_home = 2 if delivatt_home == 88 | delivatt_home == 66
label define home_skilldelatt 0 "Non-skilled" 1 "Skilled" 2 " Other/Unknown"
label values skilledatt_home home_skilldelatt

* Type of delivery attendant by location: 'on the way to facility' only
gen delivatt_ontheway = .
replace delivatt_ontheway = 0 if birth_location == 2 & delivatt_comm == 0
replace delivatt_ontheway = 1 if birth_location == 2 & delivatt_comm == 1
replace delivatt_ontheway = 2 if birth_location == 2 & delivatt_comm == 2
replace delivatt_ontheway = 3 if birth_location == 2 & delivatt_comm == 3
replace delivatt_ontheway = 4 if birth_location == 2 & delivatt_comm == 4
replace delivatt_ontheway = 88 if birth_location == 2 & delivatt_comm == 88
label define ontheway_typeattendant 0 "No attendant/alone" 1 "Traditional Birth Attendant (TBA)" 2 "Health Extension Worker (HEW)" 3 "Family member" 4 "Health Professional" 66 "Other" 88 "Unknown"
label values delivatt_ontheway ontheway_typeattendant

*Skilled and non skilled attendance for deliveries 'on the way'
gen skilledatt_ontheway = 0 if delivatt_ontheway == 0 | delivatt_ontheway == 1 | delivatt_ontheway == 2 | delivatt_ontheway == 3 
replace skilledatt_ontheway = 1 if delivatt_ontheway == 4
replace skilledatt_ontheway = 2 if delivatt_ontheway == 88 | delivatt_ontheway == 66
label define ontheway_skilldelatt 0 "Non-skilled" 1 "Skilled" 2 " Other/Unknown"
label values skilledatt_ontheway ontheway_skilldelatt

* 5. create CI for location of birth
cii proportions 2482 1824, exact

* 6. Create and label binary variable for parity, primiparous/multiparous
tab nlivebirth_enroll, missing
tab nstillbirth_enroll, missing
gen parity = 1 if (nlivebirth_enroll == 0 & nstillbirth_enroll == 0) | nlivebirth_enroll == .
replace parity = 0 if nlivebirth_enroll >0 & !missing(nlivebirth_enroll)
replace parity = 0 if nstillbirth_enroll >0 & !missing(nstillbirth_enroll)
browse if parity == 0

label define primi_multi 0 "multiparous" 1 "primiparous"
label values parity primi_multi

* 7. Label variable edu (education levels)
label define edu_level 0 "None" 1 "Primary" 2"Secondary or above"
label values edu edu_level

* 8. Convert maternal age to categorical
gen age_cat = 0 if age_pmap >14 & age_pmap <25
replace age_cat =1 if age_pmap >= 25 & age_pmap < 35
replace age_cat =2 if  age_pmap >= 35 & age_pmap < 50
label define agecat_label 0 "15-24 years" 1 " 25-34 years" 2 "35-49 years"
label values age_cat agecat_label

* 9a. Recategorize and label responses to number of ANC visits
gen anc_1to4 = anc_vis
replace anc_1to4 = 4 if  anc_vis >= 4
label define anc_num 0 "0" 1 "1" 2 "2" 3 "3" 4 "4+"
label values anc_1to4 anc_num

* 9b. Create binary variable for ANC attendance
gen ancyesno = 1 if anc_vis >0
replace ancyesno = 0 if anc_vis == 0

* 10. Generate ruralurban variable to reassign 0 response to rural; Label variable location of residence urban/rural
gen ruralurban =0 if rural == 1
replace ruralurban=1 if rural == 0
label variable ruralurban "Rural or Urban"
label define urb_rur 0 "rural" 1 "urban"
label values ruralurban urb_rur

* 11. Recategorize hftime into three categories (<0.5, 0.5 to 1 hr, >1hr)
gen hftime_3cat = 0 if hftime <30 & hftime >0
replace hftime_3cat = 1 if hftime >= 30 & hftime <=60
replace hftime_3cat = 2 if hftime >60 & !missing(hftime)
label define hftime_cat 0 "< 0.5 hr" 1 "0.5 to 1 hr" 2">1 hr"
label values hftime_3cat hftime_cat

* 12. Recategorize hfdist as binary (<2km, >= 2km)
gen hfdist_bin = 1 if hfdist < 2 & !missing(hfdist)
replace hfdist_bin = 0 if hfdist >= 2 & !missing(hfdist)

* 13. Assign 0 and 1 values to variable weekday (not 1 and 2)
replace weekday = 0 if weekday == 2
label define weekday_weekend 0 "Weekend" 1 "Weekday"
label values weekday weekday_weekend

* 14. Generating unadjusted relative risk from multivariate poisson regression with robust variance
   
* Regression model that includes the coefficients
logit location_b i.ruralurban i.edu i.age_cat i.hftime_3cat i.parity i.anc_1to4 i.rel_cat i.wealthindex_5cat

* Calculating Relative Risk for bivariate analysis using poisson regression with robust error variance
glm location_b i.ruralurban, fam(poisson) link(log) nolog vce(robust) eform
glm location_b i.edu, fam(poisson) link(log) nolog vce(robust) eform
glm location_b i.age_cat, fam(poisson) link(log) nolog vce(robust) eform
glm location_b i.hftime_3cat, fam(poisson) link(log) nolog vce(robust) eform
glm location_b i.parity, fam(poisson) link(log) nolog vce(robust) eform
glm location_b i.weekday, fam(poisson) link(log) nolog vce(robust) eform
glm location_b i.anc_1to4, fam(poisson) link(log) nolog vce(robust) eform
glm location_b i.rel_cat, fam(poisson) link(log) nolog vce(robust) eform
glm location_b i.wealthindex_5cat, fam(poisson) link(log) nolog vce(robust) eform
*Without robust error, variances will result in a confidence interval that is too wide

* 15. Generating multivariate model using poisson regression with robust error variance with all co variates in model
glm location_b i.ruralurban i.edu i.age_cat i.hftime_3cat i.parity i.weekday i.anc_1to4 i.rel_cat i.wealthindex_5cat, fam(poisson) link(log) nolog vce(robust) eform

*Remove weekday (not significant in bivariate)
glm location_b i.ruralurban i.edu i.age_cat i.hftime_3cat i.parity i.anc_1to4 i.rel_cat i.wealthindex_5cat, fam(poisson) link(log) nolog vce(robust) eform

*Remove urban/rural, age (not significant in multivariate) and then add one at a time
glm location_b i.edu i.hftime_3cat i.parity i.anc_1to4 i.rel_cat i.wealthindex_5cat, fam(poisson) link(log) nolog vce(robust) eform

*Add urban/rural (no age)
glm location_b i.ruralurban i.edu i.hftime_3cat i.parity i.anc_1to4 i.rel_cat i.wealthindex_5cat, fam(poisson) link(log) nolog vce(robust) eform

*Add age (no urban/rural)
glm location_b i.edu i.age_cat i.hftime_3cat i.parity i.anc_1to4 i.rel_cat i.wealthindex_5cat, fam(poisson) link(log) nolog vce(robust) eform

*Final model keeping those at p<0.05 after the above steps
glm location_b i.edu i.hftime_3cat i.parity i.anc_1to4 i.rel_cat i.wealthindex_5cat, fam(poisson) link(log) nolog vce(robust) eform

* 16. Check for collinearity, checking for vif (variance inflation factor) >=4 or command 'corr' and list of independent variables for pairwise correlation. if correlation coefficient >= 0.5, shows red flag or vce corr
regress location_b ruralurban tv_radio mobile_telephone edu age_cat hftime_3cat parity weekday anc_1to4 ethn_cat rel_cat
vif

*17. Generating cluster level variables (Kebele and Woreda) for random effects analysis
* Labelling and Converting kebele values from string to numerical
gen kebele_id = kebele
replace kebele_id = "1" if kebele_id == "A01"
replace kebele_id = "2" if kebele_id == "A02"
replace kebele_id = "3" if kebele_id == "A03"
replace kebele_id = "4" if kebele_id == "A04"
replace kebele_id = "5" if kebele_id == "A05"
replace kebele_id = "6" if kebele_id == "A06"
replace kebele_id = "7" if kebele_id == "A07"
replace kebele_id = "8" if kebele_id == "A08"
replace kebele_id = "9" if kebele_id == "K01"
replace kebele_id = "10" if kebele_id == "K02"
replace kebele_id = "11" if kebele_id == "K03"
replace kebele_id = "12" if kebele_id == "K04"
replace kebele_id = "13" if kebele_id == "K05"
replace kebele_id = "14" if kebele_id == "K06"
replace kebele_id = "15" if kebele_id == "K07"
replace kebele_id = "16" if kebele_id == "K08"
destring kebele_id, replace

*Categorizing 16 Kebeles to two Woredas
gen woreda_id = kebele_id
replace woreda_id = 1 if woreda_id == 1 | woreda_id == 2 | woreda_id == 3 | woreda_id == 4| woreda_id ==5 | woreda_id == 6 | woreda_id == 7 | woreda_id == 8
replace woreda_id = 2 if woreda_id == 9 | woreda_id == 10 | woreda_id == 11| woreda_id == 12 | woreda_id == 13 | woreda_id == 14 | woreda_id == 15 | woreda_id == 16

* 18a. Look for clustering using Random effects model (violation of assumption of independence of observations and equal variance across kebeles)

*setting panel data with kebele ID
xtset kebele_id
*Random effects panel regression using xtreg command
xtreg location_b ruralurban edu age_cat hftime_3cat parity weekday anc_1to4 ethn_cat rel_cat wealthindex_5cat, mle
xtreg location_b ruralurban edu age_cat hftime_3cat parity weekday anc_1to4 ethn_cat rel_cat wealthindex_5cat, re 

*Post estimation command for the model, Akaike's information criterion and Bayesian information criterion
estat ic
*Checking residual intraclass correlation
estat icc

*xtsum (to summarize panel data)
xtsum location_b ruralurban edu age_cat hftime_3cat parity weekday anc_1to4 ethn_cat rel_cat wealthindex_5cat

*18b. Alternatively, using Mixed effects regression using the mixed command to look for clustering by kebele
mixed location_b ruralurban tv_radio mobile_telephone edu age_cat hftime_3cat parity weekday anc_1to4 ethn_cat rel_cat || kebele_id:

*Checking residual intraclass correlation
estat icc
*Testing nested mixed models, null and full; comparing model with best fit using LR test
mixed location_b || kebele_id:, mle
estimates store m1
estat icc
estat ic
mixed location_b ruralurban edu age_cat hftime_3cat parity weekday anc_1to4 rel_cat wealthindex_5cat || kebele_id:, mle
estat icc
estat ic
estimates store m2
*Check model of best fit using LR test
lrtest (m1) (m2), force

mixed location_b edu hftime_3cat parity anc_1to4 rel_cat wealthindex_5cat || kebele_id:, mle
estat icc
estat ic

* Saving
save regression_stata, replace
**************************************************************************
