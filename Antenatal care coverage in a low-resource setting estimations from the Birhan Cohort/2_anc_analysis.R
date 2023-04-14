########### PREAMBLE ###################
# ANC coverage manuscript
# Study lead: Clara Pons Duran
# Last updated: April 14, 2023 by CPD

########### SETUP ###################
#Clearing the global environment and console
rm(list=ls())
cat("\014") 

#Calling necessary packages
library(tidyverse) 
library(kableExtra) #package to create tables in R
library(data.table)
library(gridExtra)
library(binom)
library(epitools)
library(haven)
library(magick)
library(ggalluvial)
library(ggpubr)
library(logistf)
library(MASS)
library(lme4)
library(brglm2)
library(stats)
library(nnet)
library(irr)
library(ggpattern)
options(knitr.kable.NA = '') #Adding an option that NAs are displayed as empty cells in tables

#Setting working directory (i.e. the folder where the results will be exported to)
setwd(paste0(Sys.getenv("gdrive"), "20. Birhan/11. Analysis/1. Manuscript analysis/Prediction of ANC attendance/x. Analysis output"))

########### PREPARING DATA ###################
#Importing datasets
all_data <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24.1. De-identified Birhan Data/5. Data sharing/clara_anc_04132023/MCH/all_data_combined.csv"), guess_max = 5000) %>% 
  mutate(dodel = as.Date(dodel, "%d%b%Y"))
ga <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24.1. De-identified Birhan Data/5. Data sharing/clara_anc_04132023/MCH/specialize_mch_ga_unique_uuid.csv")) %>% 
  mutate(conc_date = as.Date(conc_date, "%d%b%Y"),
         ga_date = as.Date(ga_date, "%d%b%Y"))
bo <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24.1. De-identified Birhan Data/5. Data sharing/clara_anc_04132023/MCH/specialized_mch_birthoutcomes.csv")) %>% 
  select_at(vars(c("uuid", "intdt_enroll", "enroll", "birth_outcome", "preg_outcome"))) 
anc <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24.1. De-identified Birhan Data/5. Data sharing/clara_anc_04132023/MCH/specialize_mch_anc.csv"))
phase1_participants <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24.1. De-identified Birhan Data/5. Data sharing/clara_anc_04132023/Census/phase1_participants.csv"), guess_max = 10000) %>% 
  select_at(vars(c("uuid", "dob", "hhid"))) %>% 
  mutate(dob = as.Date(dob, "%d-%b-%y"))
wealth <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24.1. De-identified Birhan Data/5. Data sharing/clara_anc_04132023/Census/specialized_census_wealthindex.csv"), guess_max = 10000)
census <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24.1. De-identified Birhan Data/5. Data sharing/clara_anc_04132023/Census/census_ind.csv"), guess_max = 100000) %>%  
  select_at(vars(c("uuid","eduliteracy", "edutype", "woreda", "hftime", "rel", "ethn")))
alluvial <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/4. Specialize/Manuscripts/ANC_tests/anc_alluvial.csv"), guess_max = 100000)
timepoints <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/4. Specialize/Manuscripts/ANC_tests/anc_timepoints.csv"), guess_max = 100000) %>% 
  mutate(dodel = as.Date(dodel, "%d%b%Y"))
anc_retro <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/4. Specialize/Manuscripts/ANC_tests/anc_retro.csv")) %>% 
   select_at(vars(c("uuid", "intdt_enroll", "anc_retro", "ga_first_retro")))

#Merging datasets
all_data_anc <- all_data %>% 
  select_at(vars(c("uuid", "intdt_enroll", "enroll", "grav_enroll", "nlivebirth_enroll", "nstillbirth_enroll", "preg1_enroll", "dodel", "nancvis_enroll", "hxpremb_enroll"))) %>% 
  left_join(ga, by = c("uuid" = "uuid", "intdt_enroll" = "intdt_enroll", "enroll" = "enroll")) %>% 
  left_join(bo, by = c("uuid" = "uuid", "intdt_enroll" = "intdt_enroll", "enroll" = "enroll")) %>% 
    filter(enroll == "p1") %>% 
  left_join(anc, by = c("uuid" = "uuid", "intdt_enroll" = "intdt_enroll")) %>%
  left_join(anc_retro, by = c("uuid" = "uuid", "intdt_enroll" = "intdt_enroll")) %>% 
  distinct() %>% 
  left_join(phase1_participants, by = c("uuid" = "uuid")) %>% 
  left_join(census, by = c("uuid" = "uuid")) %>% 
  left_join(wealth, by = c("hhid" = "hhid")) %>% # creating new variables
    mutate(age_conc = (as.numeric(conc_date)-as.numeric(dob))/365.25,
         intdt_enroll = as.Date(intdt_enroll, "%d%b%Y"),
         ga_enroll = (ga*7 + (as.numeric(intdt_enroll) - as.numeric(ga_date)))/7,
         anc_yn = ifelse(anc_rec == 0, 0,
                         ifelse(anc_rec >=1 & !is.na(anc_rec), 1, NA)),
         enroll_group = ifelse(ga_enroll<13 & !is.na(ga_enroll), 1,
                               ifelse(ga_enroll>=13 & !is.na(ga_enroll), 0, NA)),
         parity = ifelse((nlivebirth_enroll == 0 & nstillbirth_enroll == 0) | preg1_enroll == 1, 0,
                         ifelse((nlivebirth_enroll != 0 & !is.na(nlivebirth_enroll)) | (nstillbirth_enroll != 0 & !is.na(nstillbirth_enroll)), 1, NA)),
         hxstillbirth = ifelse(!is.na(nstillbirth_enroll) & nstillbirth_enroll!= 0, 1, 0),
         hxpremb_enroll = ifelse(preg1_enroll == 1, 0, hxpremb_enroll),
         literacy = ifelse(((eduliteracy != 1 & !is.na(eduliteracy)) | (edutype == 1 & is.na(eduliteracy))), 1,
                           ifelse(eduliteracy == 1, 0, NA))) %>%
  arrange(uuid, intdt_enroll) %>% 
  group_by(uuid) %>% 
  mutate(mch_preg = row_number()) %>% 
  filter(mch_preg == 1) #dropping duplicates women, enrolled more than once

#Dropping women with illogical gestational age at delivery, who were LTFU, or had <=0 gestational weeks at enrollment
data_cleaned <- all_data_anc %>%
  filter(ga_dodel>=28 & ga_dodel<46 & !is.na(ga_dodel) & ga_enroll>0 & !is.na(ga_enroll)) %>% 
  filter(dodel<=as.Date("2020-04-09"))

########### ANALYSIS ###################
## MANUSCRIPT TABLE 1. Background characteristics of individuals enrolled in different trimesters ##

#Dataframe for table5
data_cleaned_early <- subset(data_cleaned, ga_enroll<13 & !is.na(ga_enroll))
data_cleaned_late <- subset(data_cleaned, ga_enroll>=13 & !is.na(ga_enroll))

age_test = t.test(age_conc ~ enroll_group, data = data_cleaned)
time_test = t.test(hftime ~ enroll_group, data = data_cleaned)
literacy_test = chisq.test(table(data_cleaned$literacy, data_cleaned$enroll_group))
woreda_test = chisq.test(table(data_cleaned$woreda, data_cleaned$enroll_group))
parity_test = chisq.test(table(data_cleaned$parity, data_cleaned$enroll_group))
wealth_test = chisq.test(table(data_cleaned$wealthindex_5cat, data_cleaned$enroll_group))
ethn_test = fisher.test(table(data_cleaned$ethn, data_cleaned$enroll_group))
stillbirth_test = chisq.test(table(data_cleaned$hxstillbirth, data_cleaned$enroll_group))
preterm_test = fisher.test(table(data_cleaned$hxpremb_enroll, data_cleaned$enroll_group))

table1 <-   as.data.frame(cbind(
  c(" ", # Column 1: Variables names
    "Age at conception (years)",
    "Walking time to nearest HF",
    " ",
    "Woreda: Angolela Tera",
    "Angolela Tera",
    "Kewet",
    "Cannot read and write",
    "Wealth index",
    "1st (the wealthiest)",
    "2nd",
    "3rd",
    "4th",
    "5th (the least wealthy)",
    "Ethnicity",
    "Amhara",
    "Oromo", 
    "Other",
    "Primiparous",
    "Hx of stillbirth",
    "Hx of preterm birth"),
  c("N", # Column 2: Total (N)
    sum(with(data_cleaned, !is.na(age_conc)),na.rm=T),
    sum(with(data_cleaned, !is.na(hftime)),na.rm=T),
    "N",
    sum(with(data_cleaned, !is.na(woreda)),na.rm=T),
    " ",
    " ",
    sum(with(data_cleaned, !is.na(literacy)),na.rm=T),
    sum(with(data_cleaned, !is.na(wealthindex_5cat)),na.rm=T),
    " ",
    " ",
    " ",
    " ",
    " ",
    sum(with(data_cleaned, !is.na(ethn)),na.rm=T),
    " ",
    " ",
    " ",
    sum(with(data_cleaned, !is.na(parity)),na.rm=T),
    sum(with(data_cleaned, !is.na(hxstillbirth)),na.rm=T),
    sum(with(data_cleaned, !is.na(hxpremb_enroll)),na.rm=T)),
  c("mean", # Column 3: Total (mean/n)
    round(mean(data_cleaned$age_conc, na.rm=T),1),
    round(mean(data_cleaned$hftime, na.rm=T),1),
    "n",
    " ",
    sum(with(data_cleaned, woreda=="A"), na.rm=T),
    sum(with(data_cleaned, woreda=="K"), na.rm=T),
    sum(with(data_cleaned, literacy==0), na.rm=T),
    " ",
    sum(with(data_cleaned, wealthindex_5cat==1), na.rm=T),
    sum(with(data_cleaned, wealthindex_5cat==2), na.rm=T),
    sum(with(data_cleaned, wealthindex_5cat==3), na.rm=T),
    sum(with(data_cleaned, wealthindex_5cat==4), na.rm=T),
    sum(with(data_cleaned, wealthindex_5cat==5), na.rm=T),
    " ",
    sum(with(data_cleaned, ethn==1), na.rm=T),
    sum(with(data_cleaned, ethn==2), na.rm=T),
    sum(with(data_cleaned, ethn==66), na.rm=T),
    sum(with(data_cleaned, parity==0), na.rm=T),
    sum(with(data_cleaned, hxstillbirth==1), na.rm=T),
    sum(with(data_cleaned, hxpremb_enroll==1), na.rm=T)),
  c("SD", # Column 4: Total (SD/%)
    round(sd(data_cleaned$age_conc, na.rm=T),1),
    round(sd(data_cleaned$hftime, na.rm=T),1),
    "%",
    " ",
    100*round(sum(with(data_cleaned, woreda=="A"), na.rm=T)/sum(with(data_cleaned, !is.na(woreda)),na.rm=T),3),
    100*round(sum(with(data_cleaned, woreda=="K"), na.rm=T)/sum(with(data_cleaned, !is.na(woreda)),na.rm=T),3),
    100*round(sum(with(data_cleaned, literacy==0), na.rm=T)/sum(with(data_cleaned, !is.na(literacy)),na.rm=T),3),
    " ",
    100*round(sum(with(data_cleaned, wealthindex_5cat==1), na.rm=T)/sum(with(data_cleaned, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned, wealthindex_5cat==2), na.rm=T)/sum(with(data_cleaned, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned, wealthindex_5cat==3), na.rm=T)/sum(with(data_cleaned, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned, wealthindex_5cat==4), na.rm=T)/sum(with(data_cleaned, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned, wealthindex_5cat==5), na.rm=T)/sum(with(data_cleaned, !is.na(wealthindex_5cat)),na.rm=T),3),
    " ",
    100*round(sum(with(data_cleaned, ethn==1), na.rm=T)/sum(with(data_cleaned, !is.na(ethn)),na.rm=T),3),
    100*round(sum(with(data_cleaned, ethn==2), na.rm=T)/sum(with(data_cleaned, !is.na(ethn)),na.rm=T),3),
    100*round(sum(with(data_cleaned, ethn==66), na.rm=T)/sum(with(data_cleaned, !is.na(ethn)),na.rm=T),3),
    100*round(sum(with(data_cleaned, parity==0), na.rm=T)/sum(with(data_cleaned, !is.na(parity)),na.rm=T),3),
    100*round(sum(with(data_cleaned, hxstillbirth==1), na.rm=T)/sum(with(data_cleaned, !is.na(hxstillbirth)),na.rm=T),3),
    100*round(sum(with(data_cleaned, hxpremb_enroll==1), na.rm=T)/sum(with(data_cleaned, !is.na(hxpremb_enroll)),na.rm=T),3)),
  c("N", # Column 5: ANC (N)
    sum(with(data_cleaned_early, !is.na(age_conc)),na.rm=T),
    sum(with(data_cleaned_early, !is.na(hftime)),na.rm=T),
    "N",
    sum(with(data_cleaned_early, !is.na(woreda)),na.rm=T),
    " ",
    " ",
    sum(with(data_cleaned_early, !is.na(literacy)),na.rm=T),
    sum(with(data_cleaned_early, !is.na(wealthindex_5cat)),na.rm=T),
    " ",
    " ",
    " ",
    " ",
    " ",
    sum(with(data_cleaned_early, !is.na(ethn)),na.rm=T),
    " ",
    " ",
    " ",
    sum(with(data_cleaned_early, !is.na(parity)),na.rm=T),
    sum(with(data_cleaned_early, !is.na(hxstillbirth)),na.rm=T),
    sum(with(data_cleaned_early, !is.na(hxpremb_enroll)),na.rm=T)),
  c("mean", # Column 6: ANC (mean/n)
    round(mean(data_cleaned_early$age_conc, na.rm=T),1),
    round(mean(data_cleaned_early$hftime, na.rm=T),1),
    "n",
    " ",
    sum(with(data_cleaned_early, woreda=="A"), na.rm=T),
    sum(with(data_cleaned_early, woreda=="K"), na.rm=T),
    sum(with(data_cleaned_early, literacy==0), na.rm=T),
    " ",
    sum(with(data_cleaned_early, wealthindex_5cat==1), na.rm=T),
    sum(with(data_cleaned_early, wealthindex_5cat==2), na.rm=T),
    sum(with(data_cleaned_early, wealthindex_5cat==3), na.rm=T),
    sum(with(data_cleaned_early, wealthindex_5cat==4), na.rm=T),
    sum(with(data_cleaned_early, wealthindex_5cat==5), na.rm=T),
    " ",
    sum(with(data_cleaned_early, ethn==1), na.rm=T),
    sum(with(data_cleaned_early, ethn==2), na.rm=T),
    sum(with(data_cleaned_early, ethn==66), na.rm=T),
    sum(with(data_cleaned_early, parity==0), na.rm=T),
    sum(with(data_cleaned_early, hxstillbirth==1), na.rm=T),
    sum(with(data_cleaned_early, hxpremb_enroll==1), na.rm=T)),
  c("SD", # Column 7: ANC (SD/%)
    round(sd(data_cleaned_early$age_conc, na.rm=T),1),
    round(sd(data_cleaned_early$hftime, na.rm=T),1),
    "%",
    " ",
    100*round(sum(with(data_cleaned_early, woreda=="A"), na.rm=T)/sum(with(data_cleaned_early, !is.na(woreda)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, woreda=="K"), na.rm=T)/sum(with(data_cleaned_early, !is.na(woreda)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, literacy==0), na.rm=T)/sum(with(data_cleaned_early, !is.na(literacy)),na.rm=T),3),
    " ",
    100*round(sum(with(data_cleaned_early, wealthindex_5cat==1), na.rm=T)/sum(with(data_cleaned_early, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, wealthindex_5cat==2), na.rm=T)/sum(with(data_cleaned_early, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, wealthindex_5cat==3), na.rm=T)/sum(with(data_cleaned_early, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, wealthindex_5cat==4), na.rm=T)/sum(with(data_cleaned_early, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, wealthindex_5cat==5), na.rm=T)/sum(with(data_cleaned_early, !is.na(wealthindex_5cat)),na.rm=T),3),
    " ",
    100*round(sum(with(data_cleaned_early, ethn==1), na.rm=T)/sum(with(data_cleaned_early, !is.na(ethn)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, ethn==2), na.rm=T)/sum(with(data_cleaned_early, !is.na(ethn)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, ethn==66), na.rm=T)/sum(with(data_cleaned_early, !is.na(ethn)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, parity==0), na.rm=T)/sum(with(data_cleaned_early, !is.na(parity)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, hxstillbirth==1), na.rm=T)/sum(with(data_cleaned_early, !is.na(hxstillbirth)),na.rm=T),3),
    100*round(sum(with(data_cleaned_early, hxpremb_enroll==1), na.rm=T)/sum(with(data_cleaned_early, !is.na(hxpremb_enroll)),na.rm=T),3)),
  c("N", # Column 8: no ANC (N)
    sum(with(data_cleaned_late, !is.na(age_conc)),na.rm=T),
    sum(with(data_cleaned_late, !is.na(hftime)),na.rm=T),
    "N",
    sum(with(data_cleaned_late, !is.na(woreda)),na.rm=T),
    " ",
    " ",
    sum(with(data_cleaned_late, !is.na(literacy)),na.rm=T),
    sum(with(data_cleaned_late, !is.na(wealthindex_5cat)),na.rm=T),
    " ",
    " ",
    " ",
    " ",
    " ",
    sum(with(data_cleaned_late, !is.na(ethn)),na.rm=T),
    " ",
    " ",
    " ",
    sum(with(data_cleaned_late, !is.na(parity)),na.rm=T),
    sum(with(data_cleaned_late, !is.na(hxstillbirth)),na.rm=T),
    sum(with(data_cleaned_late, !is.na(hxpremb_enroll)),na.rm=T)),
  c("mean", # Column 9: no ANC (mean/n)
    round(mean(data_cleaned_late$age_conc, na.rm=T),1),
    round(mean(data_cleaned_late$hftime, na.rm=T),1),
    "n",
    " ",
    sum(with(data_cleaned_late, woreda=="A"), na.rm=T),
    sum(with(data_cleaned_late, woreda=="K"), na.rm=T),
    sum(with(data_cleaned_late, literacy==0), na.rm=T),
    " ",
    sum(with(data_cleaned_late, wealthindex_5cat==1), na.rm=T),
    sum(with(data_cleaned_late, wealthindex_5cat==2), na.rm=T),
    sum(with(data_cleaned_late, wealthindex_5cat==3), na.rm=T),
    sum(with(data_cleaned_late, wealthindex_5cat==4), na.rm=T),
    sum(with(data_cleaned_late, wealthindex_5cat==5), na.rm=T),
    " ",
    sum(with(data_cleaned_late, ethn==1), na.rm=T),
    sum(with(data_cleaned_late, ethn==2), na.rm=T),
    sum(with(data_cleaned_late, ethn==66), na.rm=T),
    sum(with(data_cleaned_late, parity==0), na.rm=T),
    sum(with(data_cleaned_late, hxstillbirth==1), na.rm=T),
    sum(with(data_cleaned_late, hxpremb_enroll==1), na.rm=T)),
  c("SD", # Column 10: no ANC (SD/%)
    round(sd(data_cleaned_late$age_conc, na.rm=T),1),
    round(sd(data_cleaned_late$hftime, na.rm=T),1),
    "%",
    " ",
    100*round(sum(with(data_cleaned_late, woreda=="A"), na.rm=T)/sum(with(data_cleaned_late, !is.na(woreda)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, woreda=="K"), na.rm=T)/sum(with(data_cleaned_late, !is.na(woreda)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, literacy==0), na.rm=T)/sum(with(data_cleaned_late, !is.na(literacy)),na.rm=T),3),
    " ",
    100*round(sum(with(data_cleaned_late, wealthindex_5cat==1), na.rm=T)/sum(with(data_cleaned_late, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, wealthindex_5cat==2), na.rm=T)/sum(with(data_cleaned_late, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, wealthindex_5cat==3), na.rm=T)/sum(with(data_cleaned_late, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, wealthindex_5cat==4), na.rm=T)/sum(with(data_cleaned_late, !is.na(wealthindex_5cat)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, wealthindex_5cat==5), na.rm=T)/sum(with(data_cleaned_late, !is.na(wealthindex_5cat)),na.rm=T),3),
    " ",
    100*round(sum(with(data_cleaned_late, ethn==1), na.rm=T)/sum(with(data_cleaned_late, !is.na(ethn)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, ethn==2), na.rm=T)/sum(with(data_cleaned_late, !is.na(ethn)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, ethn==66), na.rm=T)/sum(with(data_cleaned_late, !is.na(ethn)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, parity==0), na.rm=T)/sum(with(data_cleaned_late, !is.na(parity)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, hxstillbirth==1), na.rm=T)/sum(with(data_cleaned_late, !is.na(hxstillbirth)),na.rm=T),3),
    100*round(sum(with(data_cleaned_late, hxpremb_enroll==1), na.rm=T)/sum(with(data_cleaned_late, !is.na(hxpremb_enroll)),na.rm=T),3)),
  c("p-value", #Column 11: p-value
    round(age_test$p.value, 2), 
    round(time_test$p.value, 2),
    "p-value",
    round(woreda_test$p.value, 2),
    " ",
    " ",
    round(literacy_test$p.value, 2),
    round(wealth_test$p.value, 2),
    " ",
    " ",
    " ",
    " ",
    " ",
    round(ethn_test$p.value, 2),
    " ",
    " ",
    " ",
    round(parity_test$p.value, 2),
    round(stillbirth_test$p.value, 2),
    round(preterm_test$p.value, 2))))

table1[,11][(table1[,11]==0.00)] <- "<0.01"

colnames(table1) <- NULL #removing column names (manually adding column names later)

#Table 1
kable(table1, align = "lcccccccccc", booktabs = T, caption = "Characteristics of the study sample by GA group at enrollment") %>%
  kable_classic(full_width=F,html_font = "Arial") %>%
  add_header_above(c("Variables" = 1, "Total" = 3, "Enrolled <13w" = 3, "Enrolled ≥13w" = 3, " " = 1), bold=T) %>%
  row_spec(1, bold = T, extra_css = "border-bottom: 1px solid;") %>%
  row_spec(4, bold = T, extra_css = "border-bottom: 1px solid;") %>%
  row_spec(21, extra_css = "border-bottom: 1px solid;") %>%
  add_indent(c(6, 7, 10, 11, 12, 13, 14, 16, 17, 18), level_of_indent = 1) %>% 
  column_spec(4, extra_css = "border-right: 1px dashed;") %>%
  add_footnote(c("N = 2069 women who were followed-up until delivery before the onset of COVID-19 pandemic"), notation = "symbol")

## SUPPLEMENTAL FILE 1. ANC ATTENDANCE AT DIFFERENT TIMEPOINTS BY GA AT ENROLLMENT ##
data_timepoints_cleaned <- subset(timepoints, dodel<=as.Date("2020-04-09"))

group1 <- subset(data_timepoints_cleaned, enrollment==1) #GROUP 1: women who were enrolled 0-4w
group2 <- subset(data_timepoints_cleaned, enrollment==2) #GROUP 2: women who were enrolled 5-8w
group3 <- subset(data_timepoints_cleaned, enrollment==3) #GROUP 3: women who were enrolled 9-12w
group4 <- subset(data_timepoints_cleaned, enrollment==4) #GROUP 4: women who were enrolled 13-16w
group5 <- subset(data_timepoints_cleaned, enrollment==5) #GROUP 5: women who were enrolled 17-20w
group6 <- subset(data_timepoints_cleaned, enrollment==6) #GROUP 6: women who were enrolled 21-24w
group7 <- subset(data_timepoints_cleaned, enrollment==7) #GROUP 7: women who were enrolled 25-28w
group8 <- subset(data_timepoints_cleaned, enrollment==8) #GROUP 8: women who were enrolled 29-32w
group9 <- subset(data_timepoints_cleaned, enrollment==9) #GROUP 9: women who were enrolled 33-36w

group1_1 <- subset(data_timepoints_cleaned, enrollment==1 & value==1) #GROUP 1: women who were enrolled 0-4w + ANC2
group1_2 <- subset(data_timepoints_cleaned, enrollment==1 & value==2) #GROUP 1: women who were enrolled 0-4w + ANC3
group1_3 <- subset(data_timepoints_cleaned, enrollment==1 & value==3) #GROUP 1: women who were enrolled 0-4w + ANC4

group2_1 <- subset(data_timepoints_cleaned, enrollment==2 & value==1) #GROUP 2: women who were enrolled 5-8w + ANC2
group2_2 <- subset(data_timepoints_cleaned, enrollment==2 & value==2) #GROUP 2: women who were enrolled 5-8w + ANC3
group2_3 <- subset(data_timepoints_cleaned, enrollment==2 & value==3) #GROUP 2: women who were enrolled 5-8w + ANC4

group3_1 <- subset(data_timepoints_cleaned, enrollment==3 & value==1) #GROUP 3: women who were enrolled 9-12w + ANC2
group3_2 <- subset(data_timepoints_cleaned, enrollment==3 & value==2) #GROUP 3: women who were enrolled 9-12w + ANC3
group3_3 <- subset(data_timepoints_cleaned, enrollment==3 & value==3) #GROUP 3: women who were enrolled 9-12w + ANC4

group4_1 <- subset(data_timepoints_cleaned, enrollment==4 & value==1) #GROUP 4: women who were enrolled 13-16w + ANC2
group4_2 <- subset(data_timepoints_cleaned, enrollment==4 & value==2) #GROUP 4: women who were enrolled 13-16w + ANC3
group4_3 <- subset(data_timepoints_cleaned, enrollment==4 & value==3) #GROUP 4: women who were enrolled 13-16w + ANC4

group5_1 <- subset(data_timepoints_cleaned, enrollment==5 & value==1) #GROUP 5: women who were enrolled 17-20w + ANC2
group5_2 <- subset(data_timepoints_cleaned, enrollment==5 & value==2) #GROUP 5: women who were enrolled 17-20w + ANC3
group5_3 <- subset(data_timepoints_cleaned, enrollment==5 & value==3) #GROUP 5: women who were enrolled 17-20w + ANC4

group6_1 <- subset(data_timepoints_cleaned, enrollment==6 & value==1) #GROUP 6: women who were enrolled 21-24w + ANC2
group6_2 <- subset(data_timepoints_cleaned, enrollment==6 & value==2) #GROUP 6: women who were enrolled 21-24w + ANC3
group6_3 <- subset(data_timepoints_cleaned, enrollment==6 & value==3) #GROUP 6: women who were enrolled 21-24w + ANC4

group7_1 <- subset(data_timepoints_cleaned, enrollment==7 & value==1) #GROUP 7: women who were enrolled 25-28w + ANC2
group7_2 <- subset(data_timepoints_cleaned, enrollment==7 & value==2) #GROUP 7: women who were enrolled 25-28w + ANC3
group7_3 <- subset(data_timepoints_cleaned, enrollment==7 & value==3) #GROUP 7: women who were enrolled 25-28w + ANC4

group8_2 <- subset(data_timepoints_cleaned, enrollment==8 & value==2) #GROUP 8: women who were enrolled 29-32w + ANC3
group8_3 <- subset(data_timepoints_cleaned, enrollment==8 & value==3) #GROUP 8: women who were enrolled 29-32w + ANC4

group9_2 <- subset(data_timepoints_cleaned, enrollment==9 & value==2) #GROUP 9: women who were enrolled 33-36w + ANC3
group9_3 <- subset(data_timepoints_cleaned, enrollment==9 & value==3) #GROUP 9: women who were enrolled 33-36w + ANC4

group10_1 <- subset(data_timepoints_cleaned, (enrollment==1 | enrollment==2 | enrollment==3) & value==1) #GROUP 10: women who were enrolled 0-12w + ANC2
group10_2 <- subset(data_timepoints_cleaned, (enrollment==1 | enrollment==2 | enrollment==3) & value==2) #GROUP 10: women who were enrolled 0-12w + ANC3
group10_3 <- subset(data_timepoints_cleaned, (enrollment==1 | enrollment==2 | enrollment==3) & value==3) #GROUP 10: women who were enrolled 0-12w + ANC4

#Dataframe
table2 <- as.data.frame(cbind(
  c(paste("0-12w, N=", sum(with(group10_1, !is.na(anc_rec)),na.rm=T)), # Column 1: Groups
    paste("13-16w, N=", sum(with(group4_2, !is.na(anc_rec)),na.rm=T)),
    paste("17-20w, N=", sum(with(group5_2, !is.na(anc_rec)),na.rm=T)),
    paste("21-24w, N=", sum(with(group6_2, !is.na(anc_rec)),na.rm=T)),
    paste("25-28w, N=", sum(with(group7_2, !is.na(anc_rec)),na.rm=T)),
    paste("29-32w, N=", sum(with(group8_3, !is.na(anc_rec)),na.rm=T)),
    paste("33-36w, N=", sum(with(group9_3, !is.na(anc_rec)),na.rm=T))),
  c(paste0(sum(with(group10_2, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group10_2, anc_rec_1==1),na.rm=T)/sum(with(group10_2, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group4_2, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group4_2, anc_rec_1==1),na.rm=T)/sum(with(group4_2, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group5_2, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group5_2, anc_rec_1==1),na.rm=T)/sum(with(group5_2, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group6_2, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group6_2, anc_rec_1==1),na.rm=T)/sum(with(group6_2, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group7_2, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group7_2, anc_rec_1==1),na.rm=T)/sum(with(group7_2, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    "-",
    "-"),
  c(paste0(sum(with(group10_3, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group10_3, anc_rec_1==1),na.rm=T)/sum(with(group10_3, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group4_3, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group4_3, anc_rec_1==1),na.rm=T)/sum(with(group4_3, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group5_3, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group5_3, anc_rec_1==1),na.rm=T)/sum(with(group5_3, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group6_3, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group6_3, anc_rec_1==1),na.rm=T)/sum(with(group6_3, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group7_3, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group7_3, anc_rec_1==1),na.rm=T)/sum(with(group7_3, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group8_3, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group8_3, anc_rec_1==1),na.rm=T)/sum(with(group8_3, !is.na(anc_rec_1)),na.rm=T),3)), ")"),
    paste0(sum(with(group9_3, anc_rec_1==1),na.rm=T), " (", 100*(round(sum(with(group9_3, anc_rec_1==1),na.rm=T)/sum(with(group9_3, !is.na(anc_rec_1)),na.rm=T),3)), ")"))))

colnames(table2) <- NULL

#Table 2
kable(table2, align = "lll", booktabs = T, caption = "Attendance to at least 1 ANC visit in different time windows by GA at enrollment") %>%
  kable_classic(full_width=F, html_font = "Arial") %>%
  add_header_above(c("\nGA at enrollment" = 1, "ANC visits ≥29 - <37w\n n (%)" = 1, "ANC visits ≥37w\n n (%)" = 1), bold=T) %>%
  add_footnote(c("N = 2069 women who were followed-up until delivery before the onset of COVID-19 pandemic"), notation = "symbol") %>%
  row_spec(7, extra_css = "border-bottom: 1px solid;") %>% 
  row_spec(1,background="lightblue")

## SUPPLEMENTAL FILE 4. ANC COVERAGE ## all women in the cohort
  
#Dataframe
table3 <- as.data.frame(cbind(
  c(" ", # Column 1: Variables names 
    "At least 1 ANC visit",
    "At least 4 ANC visits",
    "At least 8 ANC visits",
    " ",
    "Number of ANC visits"),
  c("n", # Column 2: n
    sum(with(data_cleaned, anc_vis>=1),na.rm=T),
    sum(with(data_cleaned, anc_vis>=4),na.rm=T),
    sum(with(data_cleaned, anc_vis>=8),na.rm=T),
    "median",
    round(median(data_cleaned$anc_vis, na.rm=T),1)),
  c("%", # Column 3: %
    100*round(sum(with(data_cleaned, anc_vis>=1),na.rm=T)/sum(with(data_cleaned, !is.na(anc_vis)), na.rm=T),3),
    100*round(sum(with(data_cleaned, anc_vis>=4),na.rm=T)/sum(with(data_cleaned, !is.na(anc_vis)), na.rm=T),3),
    100*round(sum(with(data_cleaned, anc_vis>=8),na.rm=T)/sum(with(data_cleaned, !is.na(anc_vis)), na.rm=T),3),
    "IQR",
    paste(round(quantile(data_cleaned$anc_vis,0.25, na.rm=T),1),"-",round(quantile(data_cleaned$anc_vis,0.75, na.rm=T),1)))))

table3.1 <- as.data.frame(cbind(
  c(" ", # Column 1: Variables names 
    "At least 1 ANC visit",
    "At least 4 ANC visits",
    "At least 8 ANC visits",
    " ",
    "Number of ANC visits"),
  c("n", # Column 2: n
    sum(with(data_cleaned, anc_vis>=1),na.rm=T),
    sum(with(data_cleaned, anc_vis>=4),na.rm=T),
    sum(with(data_cleaned, anc_vis>=8),na.rm=T),
    "median",
    round(median(data_cleaned$anc_vis, na.rm=T),1)),
  c("%", # Column 3: %
    100*round(sum(with(data_cleaned, anc_vis>=1),na.rm=T)/sum(with(data_cleaned, !is.na(anc_vis)), na.rm=T),3),
    100*round(sum(with(data_cleaned, anc_vis>=4),na.rm=T)/sum(with(data_cleaned, !is.na(anc_vis)), na.rm=T),3),
    100*round(sum(with(data_cleaned, anc_vis>=8),na.rm=T)/sum(with(data_cleaned, !is.na(anc_vis)), na.rm=T),3),
    "IQR",
    paste(round(quantile(data_cleaned$anc_vis,0.25, na.rm=T),1),"-",round(quantile(data_cleaned$anc_vis,0.75, na.rm=T),1))),
  c("95% CI", # Column 4
    paste0("(", round(binom.confint(as.numeric(table3[2,2]), 2069, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(as.numeric(table3[2,2]), 2069, method = "agresti-coull")[,6]*100,1)), ")"),
    paste0("(", round(binom.confint(as.numeric(table3[3,2]), 2069, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(as.numeric(table3[3,2]), 2069, method = "agresti-coull")[,6]*100,1)), ")"),
    paste0("(", round(binom.confint(as.numeric(table3[4,2]), 2069, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(as.numeric(table3[4,2]), 2069, method = "agresti-coull")[,6]*100,1)), ")"),
    " ",
    " ")))

colnames(table3.1) <- NULL #removing column names (manually adding column names later)

#Table 3
kable(table3.1, align = "lcccc", booktabs = T, caption = "ANC coverage, entire cohort") %>%
  kable_classic(full_width=F, html_font = "Arial") %>% 
  add_header_above(c("Indicator" = 1, "ANC coverage" = 3), bold=T) %>%
  row_spec(1, bold = T, extra_css = "border-bottom: 1px dashed;") %>% 
  row_spec(5, bold = T, extra_css = "border-bottom: 1px dashed;") %>% 
  row_spec(6, extra_css = "border-bottom: 1px solid;") %>% 
  add_footnote(c("N = 2069 women who were followed-up until delivery before\n the onset of COVID-19 pandemic"), notation = "symbol")

# SUPPLEMENTAL FILE 4. Distribution of visits #
data_cleaned %>% 
  ggplot(aes(x=anc_vis)) +
  theme_classic()+
  theme(text=element_text(size = 13))+
  geom_histogram(binwidth=1, color="black", fill="grey") +
  xlab("# ANC visits attended during pregnancy") +
  ylab("# sample participants") +
  scale_x_continuous(breaks = seq(from = 0, to = 8, by = 1), limits=c(-0.5,8.5))+
  geom_text(stat="count" , aes(label=paste0(after_stat(count), " (", 100*round(after_stat(prop),3), "%)")), vjust = -1)+
  annotate("text", x = 8, y = 1, label = "0", size = 4, color = "black")

## MANUSCRIPT TABLE 2. ANC COVERAGE ## women enrolled <13 weeks gestation

#Subsetting women who were enrolled <13w of gestation
data_cleaned_w13 <- subset(data_cleaned, ga_enroll<13 & !is.na(ga_enroll))

#Dataframe for table 2
table4 <- as.data.frame(cbind(
  c(# Column 1: Variables names 
    "At least 1 ANC visit",
    "At least 4 ANC visits",
    "At least 8 ANC visits",
    " ",
    "Number of ANC visits"),
  c(# Column 2: n 
    sum(with(data_cleaned_w13, anc_vis>=1),na.rm=T),
    sum(with(data_cleaned_w13, anc_vis>=4),na.rm=T),
    sum(with(data_cleaned_w13, anc_vis>=8),na.rm=T),
    "median",
    round(median(data_cleaned_w13$anc_vis, na.rm=T),1)),
  c(# Column 3: % 
    100*round(sum(with(data_cleaned_w13, anc_vis>=1),na.rm=T)/sum(with(data_cleaned_w13, !is.na(anc_vis)), na.rm=T),3),
    100*round(sum(with(data_cleaned_w13, anc_vis>=4),na.rm=T)/sum(with(data_cleaned_w13, !is.na(anc_vis)), na.rm=T),3),
    100*round(sum(with(data_cleaned_w13, anc_vis>=8),na.rm=T)/sum(with(data_cleaned_w13, !is.na(anc_vis)), na.rm=T),3),
    "IQR",
    paste0(round(quantile(data_cleaned_w13$anc_vis,0.25, na.rm=T),1),"-",round(quantile(data_cleaned_w13$anc_vis,0.75, na.rm=T),1)))))

table4.1 <- as.data.frame(cbind(
  c(# Column 1: Variables names 
    "At least 1 ANC visit",
    "At least 4 ANC visits",
    "At least 8 ANC visits",
    " ",
    "Number of ANC visits"),
  c(# Column 2: n 
    sum(with(data_cleaned_w13, anc_vis>=1),na.rm=T),
    sum(with(data_cleaned_w13, anc_vis>=4),na.rm=T),
    sum(with(data_cleaned_w13, anc_vis>=8),na.rm=T),
    "median",
    round(median(data_cleaned_w13$anc_vis, na.rm=T),1)),
  c(# Column 3: % 
    100*round(sum(with(data_cleaned_w13, anc_vis>=1),na.rm=T)/sum(with(data_cleaned_w13, !is.na(anc_vis)), na.rm=T),3),
    100*round(sum(with(data_cleaned_w13, anc_vis>=4),na.rm=T)/sum(with(data_cleaned_w13, !is.na(anc_vis)), na.rm=T),3),
    100*round(sum(with(data_cleaned_w13, anc_vis>=8),na.rm=T)/sum(with(data_cleaned_w13, !is.na(anc_vis)), na.rm=T),3),
    "IQR",
    paste(round(quantile(data_cleaned_w13$anc_vis,0.25, na.rm=T),1),"-",round(quantile(data_cleaned_w13$anc_vis,0.75, na.rm=T),1))),
  c(# Column 4: 95%CI
    paste0("(", round(binom.confint(as.numeric(table4[1,2]), 150, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(as.numeric(table4[1,2]), 150, method = "agresti-coull")[,6]*100,1)), ")"),
    paste0("(", round(binom.confint(as.numeric(table4[2,2]), 150, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(as.numeric(table4[2,2]), 150, method = "agresti-coull")[,6]*100,1)), ")"),
    "-",
    " ",
    " ")))

colnames(table4.1) <- NULL #removing column names (manually adding column names later)

#Table 4
kable(table4.1, align = "lccc", booktabs = T, caption = "ANC coverage, women enrolled <13 weeks") %>%
  kable_classic(full_width=F, html_font = "Arial") %>% 
  add_header_above(c("Indicator" = 1, "n" = 1, "%" =1, "95% CI" =1), bold=T) %>%
  row_spec(4, bold = T, extra_css = "border-bottom: 2px solid;") %>% 
  row_spec(5, extra_css = "border-bottom:2px solid;") %>% 
  add_footnote(c("N = 150 women who were followed-up until delivery before\n the onset of COVID-19 pandemic and were enrolled in the first trimester"), notation = "symbol")

# MANUSCRIPT FIGURE 1. Distribution of visits #
data_cleaned_w13 %>% 
ggplot(aes(x=anc_vis)) +
  theme_classic()+
  theme(text=element_text(size = 13))+
  geom_histogram(binwidth=1, color="black", fill="grey") +
  xlab("# ANC visits attended during pregnancy") +
  ylab("# sample participants") +
  scale_x_continuous(breaks = seq(from = 0, to = 8, by = 1), limits=c(-0.5,8.5))+
  geom_text(stat="count" , aes(label=paste0(after_stat(count), " (", 100*round(after_stat(prop),3), "%)")), vjust = -1)+
  annotate("text", x = 8, y = 1, label = "0", size = 4, color = "black")

## SUPPLEMENTAL FILE 2. SENSITIVITY ANALYSIS ##
data_sens <- all_data_anc %>%
  filter(((ga_dodel>=28 & ga_dodel<46) | is.na(ga_dodel)) & ga_enroll>0 & !is.na(ga_enroll)) %>% 
  subset(ga_enroll<13 & !is.na(ga_enroll)) %>% 
  subset(birth_outcome==1 | birth_outcome==2 | is.na(birth_outcome)) %>% 
  mutate(del = conc_date + 40*7) %>% 
  mutate(del = if_else(!is.na(dodel), dodel, del)) %>% 
  filter(del<=as.Date("2020-04-09"))

#dataset with only lost to follow-up participants among those enrolled <13 weeks
data_ltfu <- data_sens %>% 
  subset(is.na(dodel))

#1 ANC visit
sum(with(data_sens, anc_vis>=1 & !is.na(anc_vis))) # 161, numerator of the sensitivity analysis for 1 ANC visit
paste0(round((sum(with(data_sens, anc_vis>=1 & !is.na(anc_vis)))/167)*100, 1),"(", round(binom.confint(161, 167, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(161, 167, method = "agresti-coull")[,6]*100,1)), ")")

#4 ANC visits
scenario1 <- (as.numeric(table4[2,3])*0.5/100) * sum(with(data_ltfu, anc_vis==3)) + (as.numeric(table4[2,3])*0.25/100) * sum(with(data_ltfu, anc_vis==2)) + (as.numeric(table4[2,3])*0.10/100) * sum(with(data_ltfu, anc_vis==1))
sum(with(data_cleaned_w13, anc_vis>=4 & !is.na(anc_vis))) + as.numeric(scenario1) #52.496, numerator of scenario 1
paste0(round(((sum(with(data_cleaned_w13, anc_vis>=4 & !is.na(anc_vis))) + as.numeric(scenario1))/167)*100, 1),"(", round(binom.confint(52.496, 167, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(52.496, 167, method = "agresti-coull")[,6]*100,1)), ")")

scenario2 <- (as.numeric(table4[2,3])*0.75/100) * sum(with(data_ltfu, anc_vis==3)) + (as.numeric(table4[2,3])*0.5/100) * sum(with(data_ltfu, anc_vis==2)) + (as.numeric(table4[2,3])*0.25/100) * sum(with(data_ltfu, anc_vis==1)) + (as.numeric(table4[2,3])*0.1/100) * sum(with(data_ltfu, anc_vis==0))
sum(with(data_cleaned_w13, anc_vis>=4 & !is.na(anc_vis))) + as.numeric(scenario2) #53.703, numerator of scenario 2
paste0(round(((sum(with(data_cleaned_w13, anc_vis>=4 & !is.na(anc_vis))) + as.numeric(scenario2))/167)*100, 1),"(", round(binom.confint(53.703, 167, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(53.703, 167, method = "agresti-coull")[,6]*100,1)), ")")

scenario3 <- (as.numeric(table4[2,3])*0.9/100) * sum(with(data_ltfu, anc_vis==3)) + (as.numeric(table4[2,3])*0.75/100) * sum(with(data_ltfu, anc_vis==2)) + (as.numeric(table4[2,3])*0.50/100) * sum(with(data_ltfu, anc_vis==1)) + (as.numeric(table4[2,3])*0.25/100) * sum(with(data_ltfu, anc_vis==0))
sum(with(data_cleaned_w13, anc_vis>=4 & !is.na(anc_vis))) + as.numeric(scenario3) #54.91, numerator of scenario 2
paste0(round(((sum(with(data_cleaned_w13, anc_vis>=4 & !is.na(anc_vis))) + as.numeric(scenario3))/167)*100, 1),"(", round(binom.confint(54.91, 167, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(54.91, 167, method = "agresti-coull")[,6]*100,1)), ")")

## Retention in care ##
# MANUSCRIPT FIGURE 2. Alluvial plot #

alluvial2 <- subset(alluvial, ga_enroll<13) # only prospectively recorded visits

ggplot(alluvial2, aes(x=visit, stratum=value, alluvium=uuid, fill=factor(value)))+ #Create alluvial plot
  geom_flow(stat="alluvium",lode.guidance="frontback", alpha=0.5)+ #Assign specifications for the flow between the columns
  geom_stratum(width=1/4, alpha=0.5)+ #Specify details for the columns
  theme(legend.title=element_blank())+ #Remove legend title
  geom_text(stat="stratum", aes(label=paste0(after_stat(count), "\n", "(", 100*round(after_stat(prop),3), "%)")))+ #Add values to each section of each column
  scale_x_discrete (labels=c("ANC1"="ANC 1 (<16 wks)", "ANC2"="ANC 2 (16-<28 wks)", "ANC3"="ANC 3 (28-<36 wks)", "ANC4"="ANC 4 (>=36 wks)"))+ 
  scale_fill_manual(values=c("Gray", "#F8766D", "#00BA38"), labels = c("Already delivered", "No ANC", "ANC"))+
  labs(
    x = "",
    y = "Study participants")

## MANUSCRIPT TABLE 3. Confidence intervals of the estimates generated in the Stata do file 3_retention_analysis ##
paste0("(", round(binom.confint(16, 121, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(16, 121, method = "agresti-coull")[,6]*100,1)), ")")
paste0("(", round(binom.confint(33, 88, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(33, 88, method = "agresti-coull")[,6]*100,1)), ")")
paste0("(", round(binom.confint(52, 74, methods = "agresti-coull")[,5]*100,1), " - ", format(round(binom.confint(52, 74, method = "agresti-coull")[,6]*100,1)), ")")

## SUPPLEMENTAL FILE 3. COMPARING RETROSPECTIVE VISITS WITH SELF-REPORTS ##
# Loading dataset with counts of retrospective visits
retro_self <- all_data_anc %>% 
  filter(((ga_dodel>=28 & ga_dodel<46) | is.na(ga_dodel)) & ga_enroll>0 & !is.na(ga_enroll)) %>% 
  subset(anc_retro!=0) %>% 
  select_at(vars(c("nancvis_enroll", "anc_retro")))

#Agreement between counts of self-reports and retrospective visits
retro_self %>% 
  ggplot(aes(x=nancvis_enroll, y=anc_retro)) +
  geom_count() +
  xlim(0, 7) +
  ylim(0, 4) +
  geom_abline(slope=1, intercept=0)+
  xlab("Self-reported visits at enrollment")+
  ylab("Retrospective visits")

retro_self <- subset(retro_self, select = -c(1))
retro_self <- as.matrix(retro_self)
retro_self <- t(retro_self)

kripp.alpha(retro_self, method="ordinal")
# alpha = 0.401 - poor agreement
