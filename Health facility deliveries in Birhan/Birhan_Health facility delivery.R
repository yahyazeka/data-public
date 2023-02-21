###########PREAMBLE###################
# Study title: Estimates and determinants of health facility delivery in the Birhan maternal and child health cohort study, 2018-2020, Amhara region, Ethiopia

# Notes: This code was written to create the dataset for this study. Further analysis and creation of summary figures 
# are available in the stata code. This code has been annotated to explain what different sections of code 
# accomplish and how.  

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
  

    
    