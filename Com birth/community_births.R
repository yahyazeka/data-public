###########PREAMBLE###################
# Study title: Estimates and determinants of health facility delivery in the Birhan maternal and child health cohort study, 2018-2020, Amhara region, Ethiopia

# Description: See analysis plan (https://drive.google.com/file/d/1KUjEXzkrfVDHgOufiHUf6A9k-dppCDxY/view?usp=sharing)

# Notes: This code was written with 2 goals in mind: 1. Answering the research questions outlined in the above analysis 
# plan; 2) Provide a template for the HaSET team for how to write code in R that a) tailors our large longitudinal 
# datasets to specific research questions, b) analyzes data in accordance with those questions and c) produces 
# publication-quality tables and figures. This code has been annotated to explain what different sections of code 
# accomplish and how. Annotation was limited to the first time a type of code was written and was not repeated further
# down when the same code was reused for different purposes. 

###########SETUP###################
#Clearing the global environment and console
rm(list=ls())
cat("\014")  
#Calling necessary packages
library(tidyverse) #collection of packages for data manipulation and visualization (including ggplot)
library(kableExtra) #package to create tables in R
library(writexl) #package to read and export excel files
options(knitr.kable.NA = '') #Adding an option that NAs are displayed as empty cells in tables

#setting working directory (i.e. the folder where the results will be exported to)
setwd(paste0(Sys.getenv("gdrive"), "20. Birhan/11. Analysis/1. Manuscript analysis/Community births/x. Analysis output"))

#Importing dataset and keeping only necessary variables
mch <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/3.MCH/1. Data/3. Indexed/all_data_combined.csv"), guess_max = 5000) %>%  #All MCH data. very large dataset so can take a while to import.  
  select_at(vars(c("uuid", "enroll", "intdt_enroll", "dcid_b", "hhid_enroll", "enroll", "location_b","delloc_b","facility_b", "lifehrs_b",
                   "intdt_b", "dodel","nlivebirth_enroll", "nstillbirth_enroll", "pdelatt_b", "delvatd_b", "wishdelhf_b",	"wnwishdelhf_b",
                   "wnwishdelhfos_b",	"limdelhf_b",	"limdelhfos_b"))) %>% #keep only relevant variables
  mutate(intdt_b = as.Date(intdt_b, "%d%b%Y"), #converting dates from string to date format
         intdt_enroll = as.Date(intdt_enroll, "%d%b%Y"),
         dodel = as.Date(dodel, "%d%b%Y"))
census <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/1. Census/1. Data/3. Indexed/census_ind.csv"), guess_max = 100000) %>%  
  select_at(vars(c("uuid","monthly_income","mobile_telephone", "tv", "radio", "edutype", "edulevel", "hftime", "hfdist"))) %>% 
  mutate(dob = as.Date(dob, "%d%b%Y"))
ga <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/4. Specialize/MCH Phase 1/specialize_mch_ga_unique_uuid.csv")) %>% 
  select_at(vars(c("uuid", "intdt_enroll", "enroll", "ga", "ga_date", "ga_dodel"))) %>% 
  mutate(ga_date = as.Date(ga_date, "%d%b%Y"),
         intdt_enroll = as.Date(intdt_enroll, "%d%b%Y"))
anc_dat <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/4. Specialize/MCH Phase 1/specialize_mch_anc.csv")) %>% 
  select_at(vars(c("uuid", "intdt_enroll", "enroll", "anc_vis"))) %>% 
  mutate(intdt_enroll = as.Date(intdt_enroll, "%d%b%Y"))
bo <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/4. Specialize/MCH Phase 1/specialized_mch_birthoutcomes.csv")) %>% 
  select_at(vars(c("uuid", "intdt_enroll", "enroll", "preg_outcome"))) %>% 
  mutate(intdt_enroll = as.Date(intdt_enroll, "%d%b%Y"))
phase1_participants <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/4. Specialize/Census/phase1_participants.csv"), guess_max = 10000) %>% 
  mutate(dob_pmap = as.Date(dob, "%d%b%Y")) %>% 
  select(uuid, dob_pmap)
  

###########TABLE 1: LOCATION OF DELIVERY###################
delloc_dat =  mch %>% #Create a small specialize delivery location dataset from the big MCH data set. %>% designates a 'pipe', which communicates to the code to execute piped code together
  left_join(bo, by = c("uuid" = "uuid", "intdt_enroll" = "intdt_enroll", "enroll" = "enroll")) %>% #left_join merges two datasets together. In this case it merge birth outcome data (bo) to delivery location data (delloc_dat)
  filter(enroll=="p1") %>% #keep only  women in the pregnancy cohort
  distinct() %>%   #remove duplicates from twin births
  left_join(census, by = c("uuid" = "uuid")) %>%  
  left_join(phase1_participants, by = c("uuid" = "uuid")) %>%  
  left_join(ga, by = c("uuid" = "uuid", "intdt_enroll" = "intdt_enroll", "enroll" = "enroll")) %>% 
  left_join(anc_dat, by = c("uuid" = "uuid", "intdt_enroll" = "intdt_enroll", "enroll" = "enroll")) %>% 
  arrange(uuid, intdt_enroll) %>% 
  group_by(uuid) %>% 
  mutate(mch_preg = row_number()) %>% 
  ungroup() %>% 
  filter(mch_preg == 1 & ga_dodel >= 28 & ga_dodel <46  & preg_outcome<4)  %>% #Keeping only the first enrolled pregnancies AND those that delivered between >=28 and <46 weeks GA AND those with a birth outcome to be consistent with birth outcomes paper
  mutate(rural = ifelse(substr(hhid_enroll,1,3)!="A01" & substr(hhid_enroll,1,3)!="K06",1,0), #Creating new variables with binary outcomes (1/0) for analysis
         primi = ifelse(nlivebirth_enroll + nstillbirth_enroll == 0 | is.na(nlivebirth_enroll),1,0),
         primi = ifelse(!is.na(nlivebirth_enroll) & nlivebirth_enroll > 0,0,primi),
         age = (as.numeric(ga_date) - as.numeric(dob) - (ga*7))/365,
         age_pmap = (as.numeric(ga_date) - as.numeric(dob_pmap) - (ga*7))/365,
         edu = ifelse(edulevel == 1 & edutype == 1, 1, 0),
         edu = ifelse(edulevel> 1 & edulevel < 6 & edutype == 1, 2, edu),
         edu = ifelse(edulevel==6 | edutype > 2 , 0, edu),
         weekday = ifelse(weekdays(dodel) == "Saturday" | weekdays(dodel) == "Sunday",2,1),
         time = ifelse(hftime>120,4,ifelse(hftime>60,3,ifelse(hftime>30,2,1))),
         dist = ifelse(hfdist>10,4,ifelse(hfdist>5,3,ifelse(hfdist>2,2,1))),
         tv_radio = ifelse(tv==1 | radio == 1,1,0),
         atten_c = ifelse(pdelatt_b != 4,0,1),
         atten_f = ifelse(delvatd_b > 6,0,1),
         atten = ifelse(!is.na(atten_c),atten_c,atten_f),
         wordea =  substr(hhid_enroll,1,1),
         kebele = substr(hhid_enroll,1,3)) 

delloc_dat %>% write_xlsx("delloc_dat.xlsx")

#Creating an R data frame as a raw form of a table of results 
delloc_tab = as.data.frame(cbind(
  c("Pregnancies with birth outcomes", #First column: Descriptive
    "Facility births",
    "Health center",
    "Hospital",
    "Home births",
    "On the way to facility",
    "Other/undefined"),
  c(nrow(delloc_dat), #Second column: results in n
    sum(with(delloc_dat,location_b=="facility"&facility_b!=66)),
    sum(with(delloc_dat,location_b=="facility"&facility_b==2)),
    sum(with(delloc_dat,location_b=="facility"&facility_b==1)),
    sum(with(delloc_dat,location_b=="community"&delloc_b==1)),
    sum(with(delloc_dat,location_b=="community"&delloc_b==2)),
    sum(with(delloc_dat,(location_b=="facility"&facility_b==66) | (location_b=="community"&delloc_b==66))) )))

#Formatting the above raw table to create a publication/presentation-quality table
delloc_tab[,2] <- as.numeric(delloc_tab[,2]) #Converting the second column to being in numeric format (so that we can calculate percentages in the next row)
delloc_tab = cbind(delloc_tab[,c(1:2)],c(NA,as.numeric(format(round(delloc_tab[c(2:7),2]/delloc_tab[1,2]*100,1),nsmall=1))) ) #Creating a third column of percentages
colnames(delloc_tab) <- NULL #removing column names (manually adding column names later)
delloc_tab[,c(3)][delloc_tab[,3]<1] <- "<1" #Designating percentages that are <1 as <1 (instead of having very small numbers)
delloc_tab[,c(3)][(delloc_tab[,c(2)]==0)] <- "0" #Designating percentages that are 0 as 0

kable(delloc_tab, align = "lcc", booktabs = T, caption = "Table 1: Delivery locations") %>% #using kable to create a formatted HTML table
  kable_classic(full_width=F,html_font = "Arial") %>% 
  add_indent(c(2,5:7), level_of_indent = 1) %>%
  add_indent(c(3:4), level_of_indent = 2) %>%
  add_header_above(c("Location of delivery" = 1, "n" = 1, "%" = 1), bold=T) %>%
  save_kable("table1.png",zoom=2)

###########TABLE 1a: DELIVERY LOCATION BY KEBELE###################
by_kebele = as.data.frame(cbind(table(subset(delloc_dat,substr(kebele,1,1)=="A")$kebele),table(subset(delloc_dat,substr(kebele,1,1)=="A" & location_b == "community")$kebele),
                                table(subset(delloc_dat,substr(kebele,1,1)=="K")$kebele),table(subset(delloc_dat,substr(kebele,1,1)=="K" & location_b == "community")$kebele)))

by_kebele = rbind(colSums(by_kebele),by_kebele)
by_kebele = cbind(by_kebele[,c(1,2)],format(round(100*(by_kebele[,2]/by_kebele[,1]),1),nsmall=1),by_kebele[,c(3,4)],format(round(100*(by_kebele[,4]/by_kebele[,3]),1),nsmall=1))
by_kebele = cbind(c("Overall", "Chacha", "Seriti", "Adadi", "Chefanen", "Tsigereda", "Addis Amba", "Gelila", "Cheki"),
                  by_kebele[,c(1:3)],
                  c("Overall", "Abayatir", "Sefi Beret", "Medina", "Debir ena Jegol", "Meriye", "Wurba", "Yelen", "Tere"),
                  by_kebele[,c(4:6)])

rownames(by_kebele) <- NULL 
colnames(by_kebele) <- NULL 

kable(by_kebele, align = "lccclccc", booktabs = T, caption = "Table 1a: Community births by kebele") %>% #using kable to create a formatted HTML table
  kable_classic(full_width=F,html_font = "Arial") %>% 
  add_header_above(c("Kebele" = 1, "N" = 1, "n" = 1, "%" = 1, "Kebele" = 1, "N" = 1, "n" = 1, "%" = 1), bold=T) %>%
  add_header_above(c("Angolela Tera" = 4, "Kewet/Shewa Robit " = 4), bold=T) %>%
  row_spec(1,extra_css = "border-bottom: 1px dashed") %>% 
  save_kable("table1a.png",zoom=2)

###########TABLE 2a: SOCIODEMOGRAPHIC DIFFERNCES BY LOCATION OF DELIVERY###################
delloc_dat_fac = subset(delloc_dat, location_b == "facility") 
delloc_dat_comm = subset(delloc_dat, location_b == "community")

rural.test_loc <- chisq.test(table(delloc_dat$rural,delloc_dat$location_b))
primi.test_loc <- chisq.test(table(delloc_dat$primi,delloc_dat$location_b))
phone.test_loc <- chisq.test(table(delloc_dat$mobile_telephone,delloc_dat$location_b))
tv_radio.test_loc <- chisq.test(table(delloc_dat$tv_radio,delloc_dat$location_b))
edu.test_loc <- chisq.test(table(delloc_dat$edu,delloc_dat$location_b))
age.test_loc <- t.test(age ~ location_b, data = delloc_dat)
income.test_loc <- t.test(monthly_income ~ location_b, data = delloc_dat)

delloc_tab2 <-  as.data.frame( 
  c(" ", 
    "Place of residence",
    "Rural",
    "Urban",
    "Media access",
    "TV or radio",
    "No TV or radio",
    "Phone ownership",
    "Yes",
    "No",
    "Formal education",
    "None",
    "Any primary",
    "Secondary or above",
    ".",
    " ",
    "Age at conception, in years",
    "Monthly income, in ETB") )

for(i in c("_fac", "_comm")) {
delloc_tab2 <- cbind(delloc_tab2, 
                     c(NA, 
                       sum(with(get(paste0("delloc_dat",i)),!is.na(rural)),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),rural==1),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),rural==0),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),!is.na(tv_radio)),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),tv_radio==1),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),tv_radio==0),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),!is.na(mobile_telephone)),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),mobile_telephone==1),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),mobile_telephone==0),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),!is.na(edu)),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),edu==0),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),edu == 1),na.rm=T),
                       sum(with(get(paste0("delloc_dat",i)),edu == 2),na.rm=T),
                       NA,
                       NA,
                       round(mean(get(paste0("delloc_dat",i))$age,na.rm=T),0),
                       round(mean(get(paste0("delloc_dat",i))$monthly_income,na.rm=T),0)) )
}

delloc_tab2[c(3:4,6:7,9:10,12:14,17,18),2] = c(paste0(delloc_tab2[c(3:4),2], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2[c(3:4),2])/as.numeric(delloc_tab2[c(2),2]),2))), c(")",")")),
                    paste0(delloc_tab2[c(6:7),2], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2[c(6:7),2])/as.numeric(delloc_tab2[c(5),2]),2))), c(")",")")),
                    paste0(delloc_tab2[c(9:10),2], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2[c(9:10),2])/as.numeric(delloc_tab2[c(8),2]),2))), c(")",")")),
                    paste0(delloc_tab2[c(12:14),2], c(" ("," ("," ("), c(as.character(100*round(as.numeric(delloc_tab2[c(12:14),2])/as.numeric(delloc_tab2[c(11),2]),2))), c(")",")",")")),
                    paste0(delloc_tab2[17,2], c(" ("), c(round(sd(delloc_dat_fac$age,na.rm=T),0)), c(")")), 
                    paste0(delloc_tab2[18,2], c(" ("), c(round(sd(delloc_dat_fac$monthly_income,na.rm=T),0)), c(")")))

delloc_tab2[c(3:4,6:7,9:10,12:14,17,18),3] = c(paste0(delloc_tab2[c(3:4),3], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2[c(3:4),3])/as.numeric(delloc_tab2[c(2),3]),2))), c(")",")")),
                    paste0(delloc_tab2[c(6:7),3], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2[c(6:7),3])/as.numeric(delloc_tab2[c(5),3]),2))), c(")",")")),
                    paste0(delloc_tab2[c(9:10),3], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2[c(9:10),3])/as.numeric(delloc_tab2[c(8),3]),2))), c(")",")")),
                    paste0(delloc_tab2[c(12:14),3], c(" ("," ("," ("), c(as.character(100*round(as.numeric(delloc_tab2[c(12:14),3])/as.numeric(delloc_tab2[c(11),3]),2))), c(")",")",")")),
                    paste0(delloc_tab2[17,3], c(" ("), c(round(sd(delloc_dat_comm$age,na.rm=T),0)), c(")")), 
                    paste0(delloc_tab2[18,3], c(" ("), c(round(sd(delloc_dat_comm$monthly_income,na.rm=T),0)), c(")")))

delloc_tab2 = cbind(delloc_tab2,
                    c(NA,
                      NA,
                      NA,
                      round(rural.test_loc$statistic,1),
                      NA,
                      NA,
                      round(tv_radio.test_loc$statistic,1),
                      NA,
                      NA,
                      round(phone.test_loc$statistic,1),
                      NA,
                      NA,
                      NA,
                      round(edu.test_loc$statistic,1),
                      NA,
                      NA,
                      round(as.numeric(age.test_loc$estimate[1]) - as.numeric(age.test_loc$estimate[2]),0),
                      round(as.numeric(income.test_loc$estimate[1]) - as.numeric(income.test_loc$estimate[2]),0)),
                    c(NA,
                      NA,
                      NA,
                      rural.test_loc$p.value,
                      NA,
                      NA,
                      tv_radio.test_loc$p.value,
                      NA,
                      NA,
                      phone.test_loc$p.value,
                      NA,
                      NA,
                      NA,
                      edu.test_loc$p.value,
                      NA,
                      NA,
                      age.test_loc$p.value,
                      income.test_loc$p.value) )

colnames(delloc_tab2) <- NULL 
delloc_tab2[,5] <- as.numeric(delloc_tab2[,5]) 
delloc_tab2[,5] <- round(delloc_tab2[,5],3)
delloc_tab2[,5][(delloc_tab2[,5]==0.000)] <- "<0.001" 

delloc_tab2[1,c(2:5)] = c("n (%)","n (%)", "Chi-square statistic", "p-value")
delloc_tab2[16,c(2:5)] = c("mean (sd)", "mean (sd)", "Difference", "p-value")

kable(delloc_tab2, align = "lllll", booktabs = T, caption = "Table 2: Sociodemographic characteristics by location of delivery") %>% 
  kable_classic(full_width=F,html_font = "Arial") %>% 
  add_indent(c(3:4,6:7,9:10,12:14), level_of_indent = 1) %>%
  add_header_above(c("Characteristics" = 1, "Facility" = 1, "Community" = 1, " " = 1, " " = 1),bold=T) %>%
  row_spec(1, bold = T, extra_css = "border-top: thick double") %>% 
  row_spec(2, extra_css = "border-top: 1px solid") %>% 
  row_spec(4, extra_css = "border-bottom: 1px dashed") %>% 
  row_spec(7, extra_css = "border-bottom: 1px dashed") %>% 
  row_spec(10, extra_css = "border-bottom: 1px dashed") %>% 
  row_spec(14, extra_css = "border-bottom: 1px dashed") %>% 
  row_spec(15, color = "white") %>% 
  row_spec(16, bold = T, extra_css = "border-bottom: 1px solid") %>% 
  save_kable("table2a.png", zoom = 2)

###########TABLE 2b: OBSTETRIC DIFFERNCES BY LOCATION OF DELIVERY###################
dist.test_loc <- chisq.test(table(delloc_dat$dist,delloc_dat$location_b))
primi.test_loc <- chisq.test(table(delloc_dat$primi,delloc_dat$location_b))
time.test_loc <- chisq.test(table(delloc_dat$time,delloc_dat$location_b))
weekday.test_loc <- chisq.test(table(delloc_dat$weekday,delloc_dat$location_b))
atten.test_loc <- chisq.test(table(delloc_dat$atten,delloc_dat$location_b))
anc.test_loc <- chisq.test(table(delloc_dat$anc_vis,delloc_dat$location_b))


delloc_tab2b <-  as.data.frame( 
  c(" ", 
    "Distance to health facility (walking time)",
    "1/2 hr or less",
    "> ½ to 1hr",
    ">1 to 2hrs",
    ">2hrs",
    "Distance to health facility (km)",
    "2km or less",
    ">2 to 5km",
    ">5 to 10km",
    ">10km",
    "Parity",
    "Primiparous",
    "Multiparous",
    "Day of birth",
    "Weekday",
    "Weekend",
    "Delivery attendant",
    "Health professional",
    "Other/unknown",
    "Antenatal care attendance",
    "0 visits",
    "1 visit",
    "2 visits",
    "3 visits",
    "4+ visits") )

for(i in c("_fac", "_comm")) {
  delloc_tab2b <- cbind(delloc_tab2b, 
                       c(NA, 
                         sum(with(get(paste0("delloc_dat",i)),!is.na(time)),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),time==1),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),time==2),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),time==3),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),time==4),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),!is.na(dist)),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),dist==1),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),dist==2),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),dist==3),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),dist==4),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),!is.na(primi)),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),primi==1),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),primi==0),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),!is.na(weekday)),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),weekday==1),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),weekday==2),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),!is.na(atten)),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),atten==1),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),atten==0),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),!is.na(anc_vis)),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),anc_vis==0),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),anc_vis==1),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),anc_vis==2),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),anc_vis==3),na.rm=T),
                         sum(with(get(paste0("delloc_dat",i)),anc_vis>3),na.rm=T)) )
}


delloc_tab2b[c(3:6,8:11,13:14,16:17,19:20,22:26),2] = c(paste0(delloc_tab2b[c(3:6),2], c(" ("," ("," ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(3:6),2])/as.numeric(delloc_tab2b[2,2]),2))), c(")",")",")",")")),
                                                        paste0(delloc_tab2b[c(8:11),2], c(" ("," ("," ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(8:11),2])/as.numeric(delloc_tab2b[7,2]),2))), c(")",")",")",")")),
                                                        paste0(delloc_tab2b[c(13:14),2], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(13:14),2])/as.numeric(delloc_tab2b[12,2]),2))), c(")",")")),
                                                        paste0(delloc_tab2b[c(16:17),2], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(16:17),2])/as.numeric(delloc_tab2b[15,2]),2))), c(")",")")),
                                                        paste0(delloc_tab2b[c(19:20),2], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(19:20),2])/as.numeric(delloc_tab2b[18,2]),2))), c(")",")")),
                                                        paste0(delloc_tab2b[c(22:26),2], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(22:26),2])/as.numeric(delloc_tab2b[21,2]),2))), c(")",")")))

delloc_tab2b[c(3:6,8:11,13:14,16:17,19:20,22:26),3] = c(paste0(delloc_tab2b[c(3:6),3], c(" ("," ("," ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(3:6),3])/as.numeric(delloc_tab2b[2,3]),2))), c(")",")",")",")")),
                                                        paste0(delloc_tab2b[c(8:11),3], c(" ("," ("," ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(8:11),3])/as.numeric(delloc_tab2b[7,3]),2))), c(")",")",")",")")),
                                                        paste0(delloc_tab2b[c(13:14),3], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(13:14),3])/as.numeric(delloc_tab2b[12,3]),2))), c(")",")")),
                                                        paste0(delloc_tab2b[c(16:17),3], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(16:17),3])/as.numeric(delloc_tab2b[15,3]),2))), c(")",")")),
                                                        paste0(delloc_tab2b[c(19:20),3], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(19:20),3])/as.numeric(delloc_tab2b[18,3]),2))), c(")",")")),
                                                        paste0(delloc_tab2b[c(22:26),3], c(" ("," ("), c(as.character(100*round(as.numeric(delloc_tab2b[c(22:26),3])/as.numeric(delloc_tab2b[21,3]),2))), c(")",")")))



delloc_tab2b = cbind(delloc_tab2b,
                    c(NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      round(time.test_loc$statistic,1),
                      NA,
                      NA,
                      NA,
                      NA,
                      round(dist.test_loc$statistic,1),
                      NA,
                      NA,
                      round(primi.test_loc$statistic,1),
                      NA,
                      NA,
                      round(weekday.test_loc$statistic,1),
                      NA,
                      NA,
                      round(atten.test_loc$statistic,1),
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      round(anc.test_loc$statistic,1)),
                    c(NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      round(time.test_loc$p.value,1),
                      NA,
                      NA,
                      NA,
                      NA,
                      round(dist.test_loc$p.value,1),
                      NA,
                      NA,
                      round(primi.test_loc$p.value,1),
                      NA,
                      NA,
                      round(weekday.test_loc$p.value,1),
                      NA,
                      NA,
                      round(atten.test_loc$p.value,1),
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      round(anc.test_loc$p.value,1)))
                    
colnames(delloc_tab2b) <- NULL 
delloc_tab2b[,5] <- as.numeric(delloc_tab2b[,5]) 
delloc_tab2b[,5] <- round(delloc_tab2b[,5],3)
delloc_tab2b[,5][(delloc_tab2b[,5]==0.000)] <- "<0.001" 

delloc_tab2b[1,c(2:5)] = c("n (%)","n (%)", "Chi-square statistic", "p-value")

kable(delloc_tab2b, align = "lllll", booktabs = T, caption = "Table 2b: Obsetric characteristics by location of delivery") %>% 
  kable_classic(full_width=F,html_font = "Arial") %>% 
  add_indent(c(3:6,8:11,13:14,16:17,19:20,22:26), level_of_indent = 1) %>%
  add_header_above(c("Characteristics" = 1, "Facility" = 1, "Community" = 1, " " = 1, " " = 1),bold=T) %>%
  row_spec(1, bold = T, extra_css = "border-top: thick double") %>% 
  row_spec(2, extra_css = "border-top: 1px solid") %>% 
  row_spec(6, extra_css = "border-bottom: 1px dashed") %>% 
  row_spec(11, extra_css = "border-bottom: 1px dashed") %>% 
  row_spec(14, extra_css = "border-bottom: 1px dashed") %>% 
  row_spec(17, extra_css = "border-bottom: 1px dashed") %>% 
  row_spec(20, extra_css = "border-bottom: 1px dashed") %>% 
  save_kable("table2b.png", zoom = 2)

###########REASONS FOR HOME DELIVERY############
reasons = delloc_dat %>% 
  filter(location_b == "community") %>% 
  select(uuid, intdt_enroll, enroll, wishdelhf_b, wnwishdelhf_b, wnwishdelhfos_b, limdelhf_b, limdelhfos_b) %>% 
  arrange(uuid) %>% 
  write_xlsx("homedelivery_reasons_update.xlsx")

###########FIGURE 1: COMMUNITY BIRTH BY MONTH############
delloc_dat$MY = format(delloc_dat$dodel,"%Y-%m")
count_tot = data.frame(table(delloc_dat$MY))
count_comm = delloc_dat %>% group_by(MY) %>% summarise(sum = sum(location_b=="community"))

colnames(count_tot) = c("MY", "total")
colnames(count_comm) = c("MY", "comm")
count =  left_join(count_tot, count_comm, by = c("MY" = "MY")) 

count$perc = 100*(count$comm/count$total)

ggplot(count, aes(x = MY, y = perc, group=1)) +
  geom_line(alpha=0.3) +
  geom_point() + 
  geom_text(aes(label=paste0(comm,"/",total)),hjust=0.3, vjust=-1,size=2.5,position=position_dodge(width=0.5)) +
  theme_bw()+
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("Year and month of births") +  ylab("Percentage of community births") +
  labs(title = "Figure 1: Percentage of community births over time") 
  ggsave("figure1.png", width = 10, height = 6)

  

  ###########SUPPLEMENTAL MATERIALS###################
  
  ###########TABLE S1: TIMING OF COMMUNITY BIRTH INTERVIEW###################
  deltime_dat =  delloc_dat  %>% 
    filter(location_b == "community") %>% 
    mutate(day_diff = as.numeric(intdt_b)-as.numeric(dodel),
           lifehrs_b = ifelse(day_diff > 1 & lifehrs_b <=24,NA,lifehrs_b),
           lifehrs_b = ifelse(day_diff > 2 & lifehrs_b <=48,NA,lifehrs_b),
           lifehrs_b = ifelse(day_diff > 3 & lifehrs_b <=72,NA,lifehrs_b),
           lifehrs_b = ifelse(day_diff > 4 & lifehrs_b <=96,NA,lifehrs_b),
           timing = ifelse(day_diff == 0 | lifehrs_b < 24,"<24 hours",NA),
           timing = ifelse((day_diff != 0 & lifehrs_b >= 24 & lifehrs_b <72) | (day_diff == 2) ,"24 to <72 hours",timing),
           timing = ifelse((lifehrs_b >= 72 & day_diff > 2 & day_diff <6) | (day_diff == 3 & is.na(lifehrs_b)) | day_diff == 4 | day_diff == 5 ,"72 hours to <6 days",timing),
           timing = ifelse(day_diff >= 6, "6 days or later",timing),
           lifehrs_b = ifelse(timing == "<24 hours" & lifehrs_b >=24, NA, lifehrs_b),
           lifehrs_b = ifelse(timing == "24 to <72 hours" & lifehrs_b >=72, NA, lifehrs_b),
           dela = ifelse(pdelatt_b == 3 | pdelatt_b == 66 | pdelatt_b == 88,1,ifelse(pdelatt_b == 1,2,ifelse(pdelatt_b == 2 | pdelatt_b == 4,3,NA))),
           anydela = ifelse(pdelatt_b == 1 | pdelatt_b == 2 | pdelatt_b == 4,1,0),
           timely = ifelse(timing == "<24 hours", 1, 0),
           comm_inf = ifelse(dodel>as.Date('2019-02-28'),1,2),
           rainy = ifelse(dodel>=as.Date('2019-06-08') & dodel<=as.Date('2019-09-11'),1,2),
           holiday = ifelse(dodel == as.Date('2019-01-07') | dodel == as.Date('2019-01-19') | dodel == as.Date('2019-03-02') | dodel == as.Date('2019-04-26') | dodel == as.Date('2019-04-28') | dodel == as.Date('2019-05-01') | dodel == as.Date('2019-05-05') | dodel == as.Date('2019-05-28') | dodel == as.Date('2019-06-03') | dodel == as.Date('2019-06-04') | dodel == as.Date('2019-08-11') | dodel == as.Date('2019-08-12') | dodel == as.Date('2019-09-12') | dodel == as.Date('2019-09-27') | dodel == as.Date('2019-11-09') | dodel == as.Date('2019-11-10') | dodel == as.Date('2020-01-07') | dodel == as.Date('2020-01-20') | dodel == as.Date('2020-03-02') | dodel == as.Date('2020-04-17') | dodel == as.Date('2020-04-19'),1,2),
           female_dc = ifelse(dcid_b == "SGB" | dcid_b == "STA" | dcid_b == "GHG" | dcid_b == "SGK" | dcid_b == "SMK" | dcid_b == "SZW" | dcid_b == "TYK" | dcid_b == "ZNT" | dcid_b == "HGK" | dcid_b == "HAD" | dcid_b == "ASA" | dcid_b == "YZM" | dcid_b == "BZH" | dcid_b == "AAB" | dcid_b == "DTM",2,1)) %>% 
    distinct()
  
  deltime_tab = as.data.frame(cbind(
    c("Community births", 
      "<24 hours",
      "24 to <72 hours",
      "72 hours to <6 days",
      "6 days or later"),
    c(nrow(deltime_dat),
      sum(with(deltime_dat,day_diff == 0 | lifehrs_b < 24),na.rm=T),
      sum(with(deltime_dat,(day_diff != 0 & lifehrs_b >= 24 & lifehrs_b <72) | (day_diff == 2) ),na.rm=T),
      sum(with(deltime_dat,(lifehrs_b >= 72 & day_diff > 2 & day_diff <6) | (day_diff == 3 & is.na(lifehrs_b)) | day_diff == 4 | day_diff == 5),na.rm=T),
      sum(with(deltime_dat,day_diff >= 6),na.rm=T)) ))
  
  deltime_tab[,2] <- as.numeric(deltime_tab[,2]) 
  deltime_tab = cbind(deltime_tab[,c(1:2)],c(NA,round(deltime_tab[c(2:5),2]/deltime_tab[1,2]*100,0) )) 
  colnames(deltime_tab) <- NULL 
  deltime_tab[,c(3)][(deltime_tab[,c(3)]<1)] <- "<1" 
  
  kable(deltime_tab, align = "lcc", booktabs = T, caption = "Table S1: Timing of community birth interviews") %>% 
    kable_classic(full_width=F,html_font = "Arial") %>% 
    add_indent(c(2:5), level_of_indent = 1) %>%
    add_header_above(c("Timing of birth interview" = 1, "n" = 1, "%" = 1), bold=T) %>%
    save_kable("Supplemental/tableS1.png",zoom=2)
  
  
  
  #################TABLE S2: SOCIODEMOGRAPHIC CHARACTERISTICS ASSOCIATED WITH TIMELLY BIRTH DATA COLLECTION###############
  deltime_dat_timely = subset(deltime_dat, timely == 1)
  deltime_dat_late = subset(deltime_dat, timely == 0)
  
  rural.test_time <- chisq.test(table(deltime_dat$rural,deltime_dat$timely))
  primi.test_time <- chisq.test(table(deltime_dat$primi,deltime_dat$timely))
  phone.test_time <- chisq.test(table(deltime_dat$mobile_telephone,deltime_dat$timely))
  edu.test_time <- chisq.test(table(deltime_dat$edu,deltime_dat$timely))
  edu.fishertest_time <- fisher.test(table(deltime_dat$edu,deltime_dat$timely))
  dela.test_time <- chisq.test(table(deltime_dat$dela,deltime_dat$timely))
  age.test_time <- t.test(age ~ timely, data = deltime_dat)
  income.test_time <- t.test(monthly_income ~ timely, data = deltime_dat)
  
  deltime_tab2 <-  as.data.frame( 
    c(" ", 
      "Place of residence",
      "Rural",
      "Urban",
      "Parity",
      "Primiparous",
      "Multiparous",
      "Phone ownership",
      "Yes",
      "No",
      "Formal education*",
      "None",
      "Any primary",
      "Secondary or above",
      "Delivery attendant",
      "Family member/Other",
      "Traditional birth attendant",
      "Health extension worker/professional",
      ".",
      " ",
      "Age at conception, in years",
      "Monthly income, in ETB"))
  
  for(i in c("","_timely", "_late")) {
    deltime_tab2 <- cbind(deltime_tab2, 
                          c(NA, 
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),rural==1),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),rural==0),na.rm=T),
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),primi==1),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),primi==0),na.rm=T),
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),mobile_telephone==1),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),mobile_telephone==0),na.rm=T),
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),edu==0),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),edu == 1),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),edu == 2),na.rm=T),
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),dela==1),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),dela == 2),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),dela == 3),na.rm=T),
                            NA,
                            NA,
                            round(mean(get(paste0("deltime_dat",i))$age,na.rm=T),0),
                            round(mean(get(paste0("deltime_dat",i))$monthly_income,na.rm=T),0)) )
  }
  
  deltime_tab2[c(21:22),2] = c(sum(with(deltime_dat,!is.na(age)),na.rm=T),
                               sum(with(deltime_dat,!is.na(monthly_income)),na.rm=T))
  
  deltime_tab2[,3] = c(NA,
                       NA,
                       paste0(deltime_tab2[c(3:4),3], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(3:4),3])/as.numeric(deltime_tab2[c(3:4),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab2[c(6:7),3], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(6:7),3])/as.numeric(deltime_tab2[c(6:7),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab2[c(9:10),3], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(9:10),3])/as.numeric(deltime_tab2[c(9:10),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab2[c(12:14),3], c(" ("," ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(12:14),3])/as.numeric(deltime_tab2[c(12:14),2]),2))), c(")",")",")")),
                       NA,
                       paste0(deltime_tab2[c(16:18),3], c(" ("," ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(16:18),3])/as.numeric(deltime_tab2[c(16:18),2]),2))), c(")",")",")")),
                       NA,
                       NA,
                       paste0(deltime_tab2[21,3], c(" ("), c(round(sd(deltime_dat_timely$age,na.rm=T),0)), c(")")), 
                       paste0(deltime_tab2[22,3], c(" ("), c(round(sd(deltime_dat_timely$monthly_income,na.rm=T),0)), c(")")))
  
  deltime_tab2[,4] = c(NA,
                       NA,
                       paste0(deltime_tab2[c(3:4),4], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(3:4),4])/as.numeric(deltime_tab2[c(3:4),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab2[c(6:7),4], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(6:7),4])/as.numeric(deltime_tab2[c(6:7),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab2[c(9:10),4], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(9:10),4])/as.numeric(deltime_tab2[c(9:10),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab2[c(12:14),4], c(" ("," ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(12:14),4])/as.numeric(deltime_tab2[c(12:14),2]),2))), c(")",")",")")),
                       NA,
                       paste0(deltime_tab2[c(16:18),4], c(" ("," ("," ("), c(as.character(100*round(as.numeric(deltime_tab2[c(16:18),4])/as.numeric(deltime_tab2[c(16:18),2]),2))), c(")",")",")")),
                       NA,
                       NA,
                       paste0(deltime_tab2[21,4], c(" ("), c(round(sd(deltime_dat_late$age,na.rm=T),0)), c(")")), 
                       paste0(deltime_tab2[22,4], c(" ("), c(round(sd(deltime_dat_late$monthly_income,na.rm=T),0)), c(")")))
  
  
  deltime_tab2 = cbind(deltime_tab2,
                       c(NA,
                         NA,
                         NA,
                         round(rural.test_time$statistic,1),
                         NA,
                         NA,
                         round(primi.test_time$statistic,1),
                         NA,
                         NA,
                         round(phone.test_time$statistic,1),
                         NA,
                         NA,
                         NA,
                         round(edu.test_time$statistic,1),
                         NA,
                         NA,
                         NA,
                         round(dela.test_time$statistic,1),
                         NA,
                         NA,
                         round(as.numeric(age.test_time$estimate[1]) - as.numeric(age.test_time$estimate[2]),0),
                         round(as.numeric(income.test_time$estimate[1]) - as.numeric(income.test_time$estimate[2]),0)),
                       c(NA,
                         NA,
                         NA,
                         rural.test_time$p.value,
                         NA,
                         NA,
                         primi.test_time$p.value,
                         NA,
                         NA,
                         phone.test_time$p.value,
                         NA,
                         NA,
                         NA,
                         edu.test_time$p.value,
                         NA,
                         NA,
                         NA,
                         dela.test_time$p.value,
                         NA,
                         NA,
                         age.test_time$p.value,
                         income.test_time$p.value) )
  
  colnames(deltime_tab2) <- NULL 
  deltime_tab2[,6] <- as.numeric(deltime_tab2[,6]) 
  deltime_tab2[,6] <- round(deltime_tab2[,6],3)
  deltime_tab2[,6][(deltime_tab2[,6]==0.000)] <- "<0.001" 
  
  deltime_tab2[1,c(2:6)] = c("N" ,"n (%)","n (%)", "Chi-square statistic", "p-value")
  deltime_tab2[20,c(2:6)] = c("N" ,"mean (sd)", "mean (sd)", "Difference", "p-value")
  
  kable(deltime_tab2, align = "lccccc", booktabs = T, caption = "Table S2: Sociodemographic characteristics among community births with timely versus late interviews") %>% 
    kable_classic(full_width=F,html_font = "Arial") %>% 
    add_indent(c(3:4,6:7,9:10,12:14,16:18), level_of_indent = 1) %>%
    add_header_above(c("Characteristics" = 1, " " = 1, "Timely interview (<24 hours)" = 1, "Late interview (>24 hours)" = 1, " " = 1, " " = 1),bold=T) %>%
    row_spec(1, bold = T, extra_css = "border-top: thick double") %>% 
    row_spec(2, extra_css = "border-top: 1px solid") %>% 
    row_spec(4, extra_css = "border-bottom: 1px dashed") %>% 
    row_spec(7, extra_css = "border-bottom: 1px dashed") %>% 
    row_spec(10, extra_css = "border-bottom: 1px dashed") %>% 
    row_spec(14, extra_css = "border-bottom: 1px dashed") %>% 
    row_spec(18, extra_css = "border-bottom: 1px dashed") %>% 
    row_spec(19, color = "white") %>% 
    row_spec(20, bold = T, extra_css = "border-bottom: 1px solid") %>% 
    add_footnote("*Fisher's exact test also completed due to small single-cell sample size: p = 0.182", notation="none") %>% 
    save_kable("Supplemental/tableS2.png", zoom = 2)
  
  
  #################TABLE S3: OPERATIONAL CHARACTERISTICS ASSOCIATED WITH TIMELLY BIRTH DATA COLLECTION###############
  weekday.test_time <- chisq.test(table(deltime_dat$weekday,deltime_dat$timely))
  female_dc.test_time <- chisq.test(table(deltime_dat$female_dc,deltime_dat$timely))
  comm_inf.test_time <- chisq.test(table(deltime_dat$comm_inf,deltime_dat$timely))
  rainy.test_time <- chisq.test(table(deltime_dat$rainy,deltime_dat$timely))
  holiday.test_time <- chisq.test(table(deltime_dat$holiday,deltime_dat$timely))
  holiday.fishertest_time <- fisher.test(table(deltime_dat$holiday,deltime_dat$timely))
  
  deltime_tab3 <-  as.data.frame( 
    c(" ",
      "Weekday of birth", 
      "Monday to Friday",
      "Saturday and Sunday",
      "Sex of data collector",
      "Female",
      "Male",
      "Community informants*",
      "Recruited",
      "Not recruited",    
      "Season",
      "Dry",
      "Rainy**",    
      "Holiday***",
      "No",
      "Yes"))
  
  for(i in c("" ,"_timely", "_late")) {
    deltime_tab3 <- cbind(deltime_tab3, 
                          c(NA, 
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),weekday==1),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),weekday==2),na.rm=T),
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),female_dc==1),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),female_dc==2),na.rm=T),
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),comm_inf==1),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),comm_inf==2),na.rm=T),
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),rainy==2),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),rainy==1),na.rm=T),
                            NA,
                            sum(with(get(paste0("deltime_dat",i)),holiday==2),na.rm=T),
                            sum(with(get(paste0("deltime_dat",i)),holiday==1),na.rm=T)) )
  }
  
  deltime_tab3[,3] = c(NA,
                       NA,
                       paste0(deltime_tab3[c(3:4),3], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(3:4),3])/as.numeric(deltime_tab3[c(3:4),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab3[c(6:7),3], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(6:7),3])/as.numeric(deltime_tab3[c(6:7),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab3[c(9:10),3], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(9:10),3])/as.numeric(deltime_tab3[c(9:10),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab3[c(12:13),3], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(12:13),3])/as.numeric(deltime_tab3[c(12:13),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab3[c(15:16),3], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(15:16),3])/as.numeric(deltime_tab3[c(15:16),2]),2))), c(")",")")) )
  
  deltime_tab3[,4] = c(NA,
                       NA,
                       paste0(deltime_tab3[c(3:4),4], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(3:4),4])/as.numeric(deltime_tab3[c(3:4),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab3[c(6:7),4], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(6:7),4])/as.numeric(deltime_tab3[c(6:7),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab3[c(9:10),4], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(9:10),4])/as.numeric(deltime_tab3[c(9:10),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab3[c(12:13),4], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(12:13),4])/as.numeric(deltime_tab3[c(12:13),2]),2))), c(")",")")),
                       NA,
                       paste0(deltime_tab3[c(15:16),4], c(" ("," ("), c(as.character(100*round(as.numeric(deltime_tab3[c(15:16),4])/as.numeric(deltime_tab3[c(15:16),2]),2))), c(")",")")) )
  
  deltime_tab3 = cbind(deltime_tab3,
                       c(NA,
                         NA,
                         NA,
                         round(weekday.test_time$statistic,1),
                         NA,
                         NA,
                         round(female_dc.test_time$statistic,1),
                         NA,
                         NA,
                         round(comm_inf.test_time$statistic,1),
                         NA,
                         NA,
                         round(rainy.test_time$statistic,1),
                         NA,
                         NA,
                         round(holiday.test_time$statistic,1)),
                       c(NA,
                         NA,
                         NA,
                         weekday.test_time$p.value,
                         NA,
                         NA,
                         female_dc.test_time$p.value,
                         NA,
                         NA,
                         comm_inf.test_time$p.value,
                         NA,
                         NA,
                         rainy.test_time$p.value,
                         NA,
                         NA,
                         holiday.test_time$p.value) )
  
  colnames(deltime_tab3) <- NULL 
  deltime_tab3[,6] <- as.numeric(deltime_tab3[,6]) 
  deltime_tab3[,6] <- round(deltime_tab3[,6],3)
  deltime_tab3[,6][(deltime_tab3[,6]==0.000)] <- "<0.001" 
  
  deltime_tab3[1,c(2:6)] = c("N" ,"n (%)", "n (%)", "Chi-square statistic", "p-value")
  
  kable(deltime_tab3, align = "lcccc", booktabs = T,   caption = "Table S3: Operational characteristics among community births with timely versus late interviews") %>% 
    kable_classic(full_width = F, html_font = "Arial") %>% 
    add_indent(c(3:4,6:7,9:10,12:13, 15:16), level_of_indent = 1) %>%
    add_header_above(c("Characteristics" = 1, " " = 1, "Timely interview (<24 hours)" = 1, "Late interview (>24 hours)" = 1, " " = 1, " " = 1), bold=T) %>%
    row_spec(1, bold = T, extra_css = "border-top: thick double") %>% 
    row_spec(2, extra_css = "border-top: 1px solid") %>% 
    row_spec(4, extra_css = "border-bottom: 1px dashed") %>% 
    row_spec(7, extra_css = "border-bottom: 1px dashed") %>% 
    row_spec(10, extra_css = "border-bottom: 1px dashed") %>% 
    row_spec(13, extra_css = "border-bottom: 1px dashed") %>% 
    add_footnote(c("*Recruited February 2019", "**June 8 - September 11", "***Fisher's exact test also completed due to small single-cell sample size: p = 0.019"), notation="none") %>% 
    save_kable("Supplemental/tableS3.png", zoom = 2)
  
  
  ###########FIGURE S1: TIMING OF COMMUNITY BIRTH INTERVIEW <72hrs###################
  ggplot(subset(deltime_dat, timing == "<24 hours" | timing == "24 to <72 hours") %>% 
           arrange(lifehrs_b) %>% 
           mutate(rn = row_number())) +
    geom_step(aes(x=lifehrs_b, y=rn)) +
    theme_bw()+
    labs(y = "Cumulative count", x = "Timing of community birth interviews completed <72 hours (by hour)")+
    scale_x_continuous(breaks=c(0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72), limits=c(0,72))+
    labs(title = "Figure S1:  Cumulative count of community birth interviews completed within 72 hours by hour") 
    ggsave("Supplemental/figureS1.png", width = 10, height = 6)
  
  
  ###########FIGURE S2: TIMELY BIRTH INTERVIEW BY MONTH############
  deltime_dat$MY = format(deltime_dat$intdt_b,"%Y-%m")
  count_tot = data.frame(table(deltime_dat$MY))
  count_timely = deltime_dat %>% group_by(MY) %>% summarise(sum = sum(timing=="<24 hours"))
  
  colnames(count_tot) = c("MY", "total")
  colnames(count_timely) = c("MY", "timely")
  count =  left_join(count_tot, count_timely, by = c("MY" = "MY")) 
  
  count$perc = 100*(count$timely/count$total)
  
  ggplot(count, aes(x = MY, y = perc, group=1)) +
    geom_line() +
    geom_point() + 
    theme_bw()+
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
    xlab("Year and month birth interviews were completed") +  ylab("Percentage of timely birth interviews") +
    labs(title = "Figure S2: Percentage of timely (<24 hours) community birth interviews over time") 
    ggsave("Supplemental/figureS2.png", width = 10, height = 6)
    
    