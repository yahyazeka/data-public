###########PREAMBLE###################
# BIRHAN stunting only analysis 
# Study lead: Frederick Goddard (fgoddard@hsph.harvard.edu) and Bezawit Hunegnaw <bhunegnaw@hsph.harvard.edu>
# Code by Frederick Goddard and Jonathan Luu 

# Notes: 

###########SETUP###################
#Clearing the global environment and console
rm(list=ls())
cat("\014")  
#Calling necessary packages
library(tidyverse) #collection of packages for data manipulation and visualization (inlcuding ggplot)
library(kableExtra) #package to create tables in R
library(epiR) #package that has functions in it to calculate prevalence, incidence etc. 
library(readxl) #package to read excel files
library(writexl) #package to export excel files
library(binom) #package to create agresti-coull confidence intervals
library(lme4) #package for linear mixed effects models

options(knitr.kable.NA = '') #Adding an option that NAs are displayed as empty cells in tables

#setting working directory (i.e. the folder where the results will be exported to)
setwd(paste0(Sys.getenv("gdrive"), "20. Birhan/11. Analysis/1. Manuscript analysis/Child Anthropometry/x. Analysis output"))

#Importing datasets
anthro = readr::read_csv(paste0(Sys.getenv("gdrive"),"24. Birhan Data/4. Specialize/MCH Phase 1/specialize_mch_anthro_long.csv"), guess_max = 100000) %>% 
  mutate(laz = ifelse(flen ==1,NA,laz),
         laz_inter = ifelse(flen_inter ==1,NA,laz_inter),
         length = ifelse((flen ==1  & !is.na(flen)),NA,length),
         intdt = as.Date(intdt, "%d%b%Y"),
         cdob = as.Date(cdob, "%d%b%Y")) %>%  #converting dates from string to date format
 filter(location=="community" | visit=="birth") # Remove facility visits after birth 

mch <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/3.MCH/1. Data/3. Indexed/all_data_combined.csv"), guess_max = 5000)  %>%  
  rowwise() %>% 
  mutate(pretrm1 = max(c_across(starts_with("preterm"))),
         pretrm2 = max(c_across(starts_with("preemie"))),
         pretrm = max(c_across(starts_with("pretrm"))),
         lbw = max(c_across(starts_with("lbw"))),
         cdob = as.Date(cdob, "%d%b%Y")) %>% 
   select(uuid, enroll, intdt_enroll, cuuid, hhid_enroll, cdob, pretrm, lbw) %>% 
   filter(!is.na(cuuid))

ga <- readr::read_csv(paste0(Sys.getenv("gdrive"), "24. Birhan Data/4. Specialize/MCH Phase 1/specialize_mch_ga_unique_uuid.csv")) %>% 
  select(cuuid, ga_dodel) %>% 
  filter(!is.na(cuuid))

bweight = anthro %>% 
  filter(age < 3  & !is.na(weight)) %>% 
  rename(bweight=weight) %>% 
  group_by(cuuid) %>% 
  arrange(cuuid,age) %>% 
  mutate(meas = row_number()) %>% 
  filter(meas==1) %>% 
  select(cuuid, bweight)

who_lt_boys <- read_excel("lhfa-boys-zscore-expanded-tables.xlsx") 
who_lt_girls <- read_excel("lhfa-girls-zscore-expanded-tables.xlsx") 
who_lt = left_join(who_lt_boys,who_lt_girls, by="Day") %>% 
  mutate(age = Day/30.417,
         SD0 = (SD0.x+SD0.y)/2,
         SD2neg = (SD2neg.x+SD2neg.y)/2) %>% 
  select(age, SD0, SD2neg) %>% 
  gather("who", "standard", -age) %>% 
  mutate(who = ifelse(who == "SD0", "Median", "-2 SD"))

##########DRAFT MANUSCRIPT ANALYSES#########
#Table 1: Summary characteristics
anthro_tab1 = anthro %>% 
  left_join(mch %>% select(cuuid, hhid_enroll, lbw, pretrm), by = "cuuid") %>% 
  left_join(ga %>% select(cuuid, ga_dodel), by = "cuuid") %>% 
  left_join(bweight %>% select(cuuid, bweight), by = "cuuid") %>% 
  mutate(pretrm = ifelse(ga_dodel<37 & !is.na(ga_dodel),1,pretrm),
         pretrm = ifelse(ga_dodel>=37 & !is.na(ga_dodel),0,pretrm),
         lbw= ifelse(bweight<2.5 & !is.na(bweight),1,lbw),
         lbw= ifelse(bweight>=2.5 & !is.na(bweight),0,lbw)) %>% 
  filter(!is.na(length))
  

summ_tab = as.data.frame(cbind(
  c(n_distinct(mch %>% select(cuuid)),
    n_distinct(mch %>% select(cuuid)),
    n_distinct(anthro_tab1 %>% select(cuuid)),
    n_distinct(anthro_tab1 %>% filter(!is.na(gender)) %>% select(cuuid)),
    n_distinct(anthro_tab1 %>% filter(!is.na(pretrm)) %>% select(cuuid)),
    n_distinct(anthro_tab1 %>% filter(!is.na(lbw)) %>% select(cuuid)),
    n_distinct(anthro_tab1 %>% select(cuuid))),
    c(NA,
    n_distinct(anthro_tab1 %>% select(cuuid)),
    n_distinct(anthro_tab1 %>% filter(substr(hhid_enroll,1,3)!="A01" & substr(hhid_enroll,1,3)!="K06") %>% select(cuuid)),
    n_distinct(anthro_tab1 %>% filter(gender==2) %>% select(cuuid)),
    n_distinct(anthro_tab1 %>% filter(pretrm==1) %>% select(cuuid)),
    n_distinct(anthro_tab1 %>% filter(lbw==1) %>% select(cuuid)),
    n_distinct(mch %>% group_by(uuid, enroll, intdt_enroll) %>% add_count(uuid, enroll, intdt_enroll) %>% ungroup() %>% 
                 right_join(anthro_tab1 %>% select(cuuid) %>% distinct(), by = "cuuid") %>% filter(n==1) %>% select(cuuid)))))

summ_tab = cbind(summ_tab,format(100*round(summ_tab[,2]/summ_tab[,1],3),nsmall=1))


anthro_tab1 %>% filter(!is.na(length)) %>% select(cuuid) %>% n_distinct()
mean((anthro_tab1 %>% filter(!is.na(length)) %>% group_by(cuuid) %>% mutate(meas = row_number()))$meas)  
sd((anthro_tab1 %>% filter(!is.na(length)) %>% group_by(cuuid) %>% mutate(meas = row_number()))$meas)  
min((anthro_tab1 %>% filter(!is.na(length)) %>% group_by(cuuid) %>% mutate(meas = row_number()))$meas)  
max((anthro_tab1 %>% filter(!is.na(length)) %>% group_by(cuuid) %>% mutate(meas = row_number()))$meas)  

#Figure 1: Data summary plot
anthro_fig1 = anthro %>% 
  mutate(age = age/30.417)

ggplot(data = subset(who_lt, age<26), aes(x=age, y = standard)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="top") +
  geom_ribbon(data = subset(who_lt, age<26 & who == "-2 SD"), aes(x=age, ymax=standard, ymin=38), alpha=0.15, fill="darkorange") +
  geom_point(data = subset(anthro_fig1,!is.na(length) & age<=26), aes(x = age, y = length),size=0.01, color = "gray45") +
  geom_line(aes(group=who,linetype = who), size = 0.7) +
  scale_linetype_manual(name = "WHO Length-for-Age Standards", values = c("Median" = "solid", "-2 SD" = "dashed")) +
  coord_cartesian(ylim = c(38,101), clip = "off", expand = c(0)) +
  scale_x_continuous(breaks = c(0,1,6,12,24), expand = 0) +
  xlab("Age (months)") + ylab("Length (cm)")

#Table 2: Comparing to WHO standards
lt_dat = anthro %>% 
  mutate(visit = ifelse(age <= 7, 1,
                        ifelse(age >= 21 & age <= 35, 2,
                               ifelse(age >= 36 & age <= 49, 3,
                                      ifelse(age >= 155 & age <= 211, 4,
                                             ifelse(age >= 337 & age <= 393, 5,
                                                 ifelse(age >= 702 & age <= 758, 6, NA))))))) %>% 
  filter(!is.na(visit) & !is.na(length)) %>% 
  group_by(cuuid,visit, gender) %>% 
  mutate(laz = ifelse(!is.na(laz_inter),laz_inter, laz)) %>% 
  summarize(laz_mean = mean(laz),
            length_mean = mean(length),
            age_mean = mean(age)) %>% 
  mutate(stunt = ifelse(laz_mean < -2,1,0))


quantile(lt_dat$length_mean, probs = c(.25, .5, .75))
lt_dat %>% filter(gender == 2) %>% group_by(visit) %>%
  summarize(n=length(length_mean),
            laz_low=quantile(laz_mean, probs=0.25),
            laz_med=quantile(laz_mean, probs=0.5),
            laz_up=quantile(laz_mean, probs=0.75),
            ln_low=quantile(length_mean,probs=0.25),
            ln_med=quantile(length_mean,probs=0.5),
            ln_up=quantile(length_mean,probs=0.75))

lt_dat %>% filter(gender == 1) %>% group_by(visit) %>%
  summarize(n=length(length_mean),
            laz_low=quantile(laz_mean, probs=0.25),
            laz_med=quantile(laz_mean, probs=0.5),
            laz_up=quantile(laz_mean, probs=0.75),
            low=quantile(length_mean,probs=0.25),
            med=quantile(length_mean,probs=0.5),
            up=quantile(length_mean,probs=0.75))


##### Modeling #####
# Prepare data for modeling work
dat = anthro %>% 
  select(c("cuuid", "visit", "location", "length", "gender", "age", "laz")) %>% 
  subset(age >= 0 & age < 1000 & !is.na(length)) %>% filter(!is.na(laz)) %>%  # Remove invalid age and missing lengths
  mutate(visit = case_when(age <= 7 ~ "Birth", 
                           age > 20 & age < 36 ~ "Day 28",
                           age > 34 & age < 50 ~ "Day 42",
                           age > 154 & age < 212 ~ "6 months",
                           age > 336 & age < 394 ~ "12 months",
                           age > 701 & age < 759 ~ "24 months"),
         visit = factor(visit,levels = c("Birth","Day 28","Day 42","6 months","12 months","24 months")),
         age=age/30.437, # Convert age to months
         age_s = scale(age),
         spline1 = case_when(age > 0 & age <= 1 ~ age, age==0 ~ 0, TRUE ~ 1), # Append splines to dataset
         spline2 = case_when(age > 1 & age <= 6 ~ age-1, age <= 1 ~ 0, TRUE ~ 5),
         spline3 = case_when(age > 6 & age <= 12 ~ age-6, age <= 6 ~ 0, TRUE ~ 6),
         spline4 = case_when(age > 12 & age <= 29 ~ age-12, age <= 12 ~ 0, TRUE ~ 12)) %>% # Create standardized age variable
  filter(location=="community" | visit=="Birth") %>%  # Remove facility visits after birth - 13410 to 11175 observations
  group_by(cuuid) %>% 
  mutate(n_obs = n(), cuuid=factor(cur_group_id())) %>%  # Number of observations per child + relabel ID
  ungroup()  
# Add stunted column
isStunted <- Vectorize(function(laz){
  return(laz <= -2)
})
dat %>% mutate(stunt=isStunted(laz))

# LME with random intercept
mod1 <- lmer(length ~ age_s + (1|cuuid), data=dat, REML=F) 

# LME with random intercept+slope
mod2 <- lmer(length ~ age_s + (1+age_s|cuuid), data=dat, REML=F) 

# LME with random intercept and quadratic age
mod3 <- lmer(length ~ age_s + I(age_s^2) + (1|cuuid), data=dat, REML=F)

# LME with random intercept+slope and quadratic age
mod4 <- lmer(length ~ age_s + I(age_s^2) + (1 + age_s|cuuid), data=dat, REML=F)

# LME with random intercept and piecewise-linear age
mod5 <- lmer(length ~ spline1 + spline2 + spline3 + spline4 + (1|cuuid), data=dat, REML=F)

# LME with random intercept+slope and piecewise-linear age
mod6 <- lmer(length ~ spline1 + spline2 + spline3 + spline4 + (1 + age_s|cuuid), data=dat, REML=F)


##### Removing unreliable points #####
# Function that removes points not within 1.5sd, given the model
removesd <- function(mod.in, dat.in){
  sd_val <- summary(mod.in)$sigma
  dat.sd <- dat.in %>% select(cuuid,age,age_s,length,visit,n_obs,laz) %>% 
    mutate(p_length = predict(mod.in)) %>%
    filter(length >= (p_length-1.5*sd_val) & length <= (p_length+1.5*sd_val))
  return(dat.sd)
}

# Function that gets median length if an individual has >1 observations in a given time window
averageMulti <- function(dat.in){
  if (nrow(dat.in) > 1){
    median <- dat.in %>% select(age,length,laz) %>% apply(2,median)
    dat.out <- data.frame(t(median))
    return(dat.out)
  }else{
    return(dat.in)
  }
}


##### Calculate prevalence, incidence, and reversal #####
# Function that calculates prevalence for the given data
prevCalc <- function(dat.in, modelname=""){
  # Average out multiple instances per window
  dat.in <- dat.in %>% group_by(cuuid,visit) %>% group_modify(~ averageMulti(.)) %>% ungroup()
  pred.stunt <- dat.in %>% select(age,length,visit,laz) %>% mutate(stunt=isStunted(laz))
  
  # Calculate proportion of stunted
  prop <- table(pred.stunt$visit, pred.stunt$stunt) %>% prop.table(1) %>% .[,2]
  out <- tibble(c("Birth","Day 28","Day 42","6 months","12 months","24 months"), prop, modelname)
  colnames(out) <- c("Category", "Proportion", "Model")
  return(list(out, table(pred.stunt$visit, pred.stunt$stunt)))
}

# Function to calculate both incidence and reversal
# Baseline determines if we are calculating incidence/reversal based on shifting time points (6 days vs. 6 months, 6 months vs 12 months) or a baseline time point (6 days vs 6 months, 6 days vs 12 months)
# To select a baseline, pass in one of the categories: "Birth","Day 28","Day 42","6 months","12 months","24 months"
inc_and_rev_Calc <- function(dat.in, modelname="", baseline=NULL){
  # Average out multiple instances per window
  dat.in <- dat.in %>% group_by(cuuid,visit) %>% group_modify(~ averageMulti(.)) %>% ungroup()
  pred.stunt <- dat.in %>% select(cuuid,age,length,visit,laz) %>% mutate(stunt=isStunted(laz))
  
  # Setup storage output
  selectID.inc <- selectID.rev <- pred.stunt %>% pull(cuuid)
  # Choose the categories we are interested in
  categories <- c("Birth","Day 28","Day 42","6 months","12 months","24 months")
  
  out.inc <- data.frame(matrix(ncol = length(categories), nrow = 0))
  out.rev <- data.frame(matrix(ncol = length(categories), nrow = 0))
  colnames(out.inc) <- categories
  colnames(out.rev) <- categories
  first <- T
  
  # Calculate incidence and reversal by category
  for (i in categories){
    # Obtain number of individuals stunted/not stunted
    stuntCount.inc <- pred.stunt %>% filter(visit==i & stunt==TRUE & cuuid %in% selectID.inc) %>% nrow
    stuntCount.rev <- pred.stunt %>% filter(visit==i & stunt==FALSE & cuuid %in% selectID.rev) %>% nrow
    
    # Ignore first column as we need two time points to calculate incidence/reversal
    if (first==T){
      out.inc[1:3,i] <- NA
      out.rev[1:3,i] <- NA
      first <- F
    }
    else{ # Obtain denominator and calculate incidence/reversal
      prevN.inc <- pred.stunt %>% filter(visit==i & cuuid %in% selectID.inc) %>% nrow()
      out.inc[1,i] <- stuntCount.inc/prevN.inc # Estimate
      out.inc[2,i] <- stuntCount.inc # Numerator
      out.inc[3,i] <- prevN.inc # Denominator
      
      prevN.rev <- pred.stunt %>% filter(visit==i & cuuid %in% selectID.rev) %>% nrow()
      out.rev[1,i] <- stuntCount.rev/prevN.rev
      out.rev[2,i] <- stuntCount.rev
      out.rev[3,i] <- prevN.rev
    }
    
    # If baseline is null, keep individuals from previous time point 
    if (is.null(baseline)){
      selectID.inc <- pred.stunt %>% filter(visit==i & stunt==FALSE) %>% pull(cuuid)
      selectID.rev <- pred.stunt %>% filter(visit==i & stunt==TRUE) %>% pull(cuuid)
    }else{ # If baseline is specified, keep individuals from baseline
      selectID.inc <- pred.stunt %>% filter(visit==baseline & stunt==FALSE) %>% pull(cuuid)
      selectID.rev <- pred.stunt %>% filter(visit==baseline & stunt==TRUE) %>% pull(cuuid)
    }
  }
  return(cbind(tibble(Model=modelname,round(out.inc,3)), tibble(Model=modelname,round(out.rev,3))))
}


##### Generate tables #####
# Prevalence table
smallDat <- removesd(mod6,dat)
allPrev.2sd <- rbind(prevCalc(smallDat,"Mod6"),
                     prevCalc(dat,"Observed"))

prev <- as_tibble(rbind(allPrev.2sd[[1]]$Proportion, allPrev.2sd[[2]]$Proportion))
prev_c <- as_tibble(rbind(rowSums(allPrev.2sd[[3]]), rowSums(allPrev.2sd[[4]])))
prev_counts <- rbind(prev[1,], allPrev.2sd[[3]][,2], rowSums(allPrev.2sd[[3]]), prev[2,], allPrev.2sd[[4]][,2], rowSums(allPrev.2sd[[4]]))
prev_counts <- cbind(Number=rep(c("Est.", "Num.", "Denom."),2),
                     Model=c(rep("Mod6",3),rep("Observed",3)),prev_counts)

# Incidence and reversal tables
allIR.2sd <- rbind(inc_and_rev_Calc(smallDat,"Mod6"),
                   inc_and_rev_Calc(dat,"Observed"))

IR_n <- nrow(allIR.2sd)/3
IR_length <- length(allIR.2sd)/2
inc <- allIR.2sd[rep(c(T,F,F),IR_n),1:IR_length] # Get incidence
inc_c <- allIR.2sd[rep(c(F,F,T),IR_n),1:IR_length] # Get incidence counts
rev <- allIR.2sd[rep(c(T,F,F),IR_n),(IR_length+1):(2*IR_length)] # Get reversal
rev_c <- allIR.2sd[rep(c(F,F,T),IR_n),(IR_length+1):(2*IR_length)] # Get reversal counts

# Incidence and reversal tables w/ common baseline of Birth
allIR.2sd.baseline <- rbind(inc_and_rev_Calc(smallDat,"Mod6", "Birth"),
                            inc_and_rev_Calc(dat,"Observed", "Birth"))

IR_n.cb <- nrow(allIR.2sd.baseline)/3
IR_length.cb <- length(allIR.2sd.baseline)/2
inc.cb <- allIR.2sd.baseline[rep(c(T,F,F),IR_n.cb),1:IR_length.cb] # Get incidence w/ common baseline
inc_c.cb <- allIR.2sd.baseline[rep(c(F,F,T),IR_n.cb),1:IR_length.cb] # Get incidence counts w/ common baseline
rev.cb <- allIR.2sd.baseline[rep(c(T,F,F),IR_n.cb),(IR_length.cb+1):(2*IR_length.cb)] # Get reversal w/ common baseline
rev_c.cb <- allIR.2sd.baseline[rep(c(F,F,T),IR_n.cb),(IR_length.cb+1):(2*IR_length.cb)] # Get reversal counts w/ common baseline

# Get numerator and denominators
seq_inc <- cbind(Number=rep(c("Est.", "Num.", "Denom."),2),allIR.2sd[,c(1,3:7)])
seq_rev <- cbind(Number=rep(c("Est.", "Num.", "Denom."),2),allIR.2sd[,c(1,10:14)])
cb_inc <- cbind(Number=rep(c("Est.", "Num.", "Denom."),2),allIR.2sd.baseline[,c(1,3:7)])
cb_rev <- cbind(Number=rep(c("Est.", "Num.", "Denom."),2),allIR.2sd.baseline[,c(1,10:14)])


##### Model diagnostics #####
AIC(mod1,mod2,mod3,mod4,mod5,mod6)
BIC(mod1,mod2,mod3,mod4,mod5,mod6)
sapply(c(mod1,mod2,mod3,mod4,mod5,mod6),deviance)


##### Proportion confidence intervals #####
# Returns Agresti-coull CI, given data frame with # of successes and # of trials
getCI <- function(data.in){
  colnames(data.in) <- c("Success","N")
  result <- round(binom.confint(data.in[,"Success"],data.in[,"N"],methods="agresti-coull")[4:6],3)*100
}

# Returns formatted dataset for plotting
createPlotCI <- function(n, N, categories){
  model <- cbind(type="Model (excluding outliers)",Age=categories, getCI(cbind(t(n[1,]), t(N[1,]))))
  obs <- cbind(type="Observed (all measurements)",Age=categories, getCI(cbind(t(n[2,]), t(N[2,]))))
  out <- rbind(model, obs)
  
  labels <-  c("Model", "Age", "Estimate", "Lower", "Upper")
  colnames(out) <- labels
  return(out)
}

# Confidence intervals for prevalence
categories <- factor(c("Birth","Day 28","Day 42","6 months","12 months","24 months"), 
                     levels = c("Birth","Day 28","Day 42","6 months","12 months","24 months"))
prev_dat <- createPlotCI(prev*prev_c, prev_c, categories)

# Confidence intervals for sequential incidence 
inc_dat <- createPlotCI(inc[,3:IR_length]*inc_c[,3:IR_length], inc_c[,3:IR_length], categories[2:6])

# Confidence intervals for common-baseline incidence 
inc_dat.cb <- createPlotCI(inc.cb[,3:IR_length.cb]*inc_c.cb[,3:IR_length.cb], inc_c.cb[,3:IR_length.cb], categories[2:6])

# Confidence intervals for sequential reversal 
rev_dat <- createPlotCI(rev[,3:IR_length]*rev_c[,3:IR_length], rev_c[,3:IR_length], categories[2:6])

# Confidence intervals for common-baseline reversal 
rev_dat.cb <- createPlotCI(rev.cb[,3:IR_length.cb]*rev_c.cb[,3:IR_length.cb], rev_c.cb[,3:IR_length.cb], categories[2:6])


##### Plotting #####
# Scatter plot of age*length with age windows
dat %>% mutate(age=age*30.437) %>% ggplot(aes(x=age, y=length)) + geom_point(shape=1, color="orange") +
  scale_x_continuous(name="Age (days)", breaks=c(0, 28, 180, 365, 730)) +  theme(legend.position = "none") + ylab("Length") + geom_vline(xintercept = 165, linetype="dotted", color="Blue") +
  geom_vline(xintercept = 200, linetype="dotted", color="Blue") + geom_vline(xintercept = 350, linetype="dotted", color="Red") +
  geom_vline(xintercept = 380, linetype="dotted", color="Red") + geom_vline(xintercept = 715, linetype="dotted", color="green") +
  geom_vline(xintercept = 745, linetype="dotted", color="green") + geom_vline(xintercept = 40, linetype="dotted", color="yellow") +
  geom_vline(xintercept = 60, linetype="dotted", color="yellow") +geom_vline(xintercept = 20, linetype="dotted", color="black") +
  geom_vline(xintercept = 40, linetype="dotted", color="black") + geom_vline(xintercept = 12, linetype="dotted", color="cyan") +
  geom_vline(xintercept = 5, linetype="dotted", color="cyan") + geom_vline(xintercept = 0, linetype="dotted", color="pink") +
  geom_vline(xintercept = 4, linetype="dotted", color="pink") + ggtitle("Scatter plot of Age*Length with age windows ")

# # Scatter plot function of predicted vs. observed length
plotOP <- function(dat_in, modelText){
  dat_in %>% ggplot(aes(x=age, y=value, color=name)) + geom_point() +
    ggtitle(paste("Predicted vs. observed length",modelText)) +
    scale_color_manual(labels = c("Observed", "Predicted"), values = c("orange", "darkturquoise")) +
    labs(y="Length", x="Age",color="")
}

# Plot observed vs predicted length for model 6
# Create dataset with both observed and predicted values
pred.v.obs <- dat %>% select(age,age_s,length,visit,cuuid,n_obs) %>% mutate(p_length6=predict(mod6))
predicted_plot <- pred.v.obs %>% pivot_longer(c("length", "p_length6")) %>% plotOP("Model 6")

# Plot removed points for model 6
removed_pts <- anti_join(dat, removesd(mod6,dat)) %>% mutate(removed=T) %>% select(age,length,removed, gender)
plot_pts <- removesd(mod6,dat) %>% mutate(removed=F) %>% select(age,length,removed) %>% rbind(removed_pts)
plot_pts <- plot_pts %>% rename("Removed"="removed")
ggplot(data=plot_pts, aes(x=age,y=length)) + 
  geom_point(aes(color=Removed, shape=Removed), alpha = 0.5, size = 0.1) + 
  scale_shape_manual(values=c(16, 4)) + xlab("Age (months)") + 
  ylab("Length (cm)") + ggtitle("Removed dataset observations") + 
  xlim(c(0,30)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.9,0.13))+ 
  scale_color_manual(values = c("#E7B800", "#2E9FDF"))

# Removed points table
removed_pts_tab <- removed_pts %>% 
  mutate(age = age*30.417,
         visit = case_when(age <= 7 ~ "Birth", 
                           age > 20 & age < 36 ~ "Day 28",
                           age > 34 & age < 50 ~ "Day 42",
                           age > 154 & age < 212 ~ "6 months",
                           age > 336 & age < 394 ~ "12 months",
                           age > 701 & age < 759 ~ "24 months"),
         visit = factor(visit,levels = c("Birth","Day 28","Day 42","6 months","12 months","24 months"))) %>% 
  filter(!is.na(visit))
nrow(removed_pts_tab)

#Nr of observations
table(as.data.frame(dat)$gender, as.data.frame(dat)$visit)
#Nr of removed points 
table(removed_pts_tab$gender, removed_pts_tab$visit)
#Nr of observations after removal
table(as.data.frame(dat)$gender,as.data.frame(dat)$visit) - table(removed_pts_tab$gender,removed_pts_tab$visit) 


# Plot proportions with confidence intervals
plotCI <- function(data.in, ylab, title){
  ggplot(data = data.in, aes(x = Age, y = Estimate, fill = Model, color = Model), size = 3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.title = element_blank(), legend.key = element_rect(fill = "white")) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1,position=position_dodge(width=0.7)) +
    geom_point(position=position_dodge(width=0.7)) +
    geom_text(aes(label=format(round(Estimate,1),nsmall=1)),hjust=-0.2, size=3,show.legend = FALSE,position=position_dodge(width=0.7)) +
    scale_color_manual(values = c("#E7B800", "#2E9FDF")) + 
    scale_x_discrete(breaks = data.in$Age, labels = data.in$Age) +
    scale_y_continuous(limits = c(0,80), name = ylab) + ggtitle(title)
}

# Plot prevalence
plotCI(prev_dat, ylab="Prevalence with 95% CI", title="Estimated prevalence of stunting")
ggsave("/Users/fgoddard/Downloads/Figure 2a - original format.eps", width = 8, height = 4)

# Plot sequential incidence
plotCI(inc_dat, ylab="Incidence with 95% CI", title="Estimated incidence of stunting (Sequential)")
ggsave("/Users/fgoddard/Downloads/Figure 2b - original format.eps", width = 8, height = 4)

# Plot CB incidence
plotCI(inc_dat.cb, ylab="Incidence with 95% CI", title="Estimated incidence of stunting (Common baseline - Birth)")
ggsave("/Users/fgoddard/Downloads/Figure S1 - original format.eps", width = 8, height = 4)

# Plot sequential reversal
plotCI(rev_dat, ylab="Reversal with 95% CI", title="Estimated incidence of reversal (Sequential)")
ggsave("/Users/fgoddard/Downloads/Figure 2c - original format.eps", width = 8, height = 4)

# Plot CB reversal
plotCI(rev_dat.cb, ylab="Reversal with 95% CI", title="Estimated incidence of reversal (Common baseline - Birth)")
ggsave("/Users/fgoddard/Downloads/Figure S2 - original format.eps", width = 8, height = 4)

