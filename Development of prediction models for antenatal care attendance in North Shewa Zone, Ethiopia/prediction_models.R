library(tidyverse)
library(pROC)
library(survival)
library(groupdata2)
library(LTRCtrees)
library(glmnet)
library(xgboost)
library(gtools)
library(fastDummies)
library(boot)
library(kableExtra)
library(e1071)
library(randomForest)
library(mice)

#################################################
#Load and merge datasets, extract features
################################################

#load general datasets for the cohort
mch <- readr::read_csv("mch_data.csv", guess_max = 5000)  #All MCH data. Very large dataset so can take a while to import.  
mch <- mch %>% mutate(intdt_enroll_date = as.Date(intdt_enroll, "%d%b%Y")) %>%
  mutate(across(contains("intdt"), ~ as.Date(., "%d%b%Y"))) %>%
  mutate(across(contains("vdate"), ~ as.Date(., "%d%b%Y"))) %>%
  mutate(dodel =  as.Date(dodel, "%d%b%Y")) %>%
  distinct()

core_wra_load <- readr::read_csv(paste0("core_women_wide.csv"), guess_max = 100000) %>% 
  mutate(across(contains("intdt"), ~ as.Date(., "%d%b%Y"))) %>%
  distinct()

#load files with specific outcomes and covariates related to ANC attendance
data <- readr::read_csv("specialized_anc_prediction.csv", guess_max = 5000)   
data_anc <- readr::read_csv("specialize_mch_anc.csv", guess_max = 5000)   


data <- data %>% left_join(data_anc, by=c("uuid", "intdt_enroll"))
data <- data %>% mutate(anc_vis = anc_vis.y) 
data_load <- data


data <- data %>% mutate(across(contains("intdt"), ~ as.Date(., "%d%b%Y")))


core_wra <- data %>% left_join(core_wra_load, by="uuid")

core_vars = c("radiotv", "mobphone", "prevredtype", "durlive", "prevreg", "khat", 
              "feedchild", "feedself", "dchild", "ndsum", "contr", "bstfeed_nbleed", 
              "abstain", "relcontr", "hcg")
dummies <- c("prevredtype", "radiotv", "prevreg", "feedchild", "feedself", "ldelloc_enroll")
#most recent value prior to enrollment date in mch
for(varname in core_vars){
  core_wra[[varname]] = as.double(NA)
  for(i in 5:1){
    if(paste(varname, "_H1_", i, sep="") %in% colnames(core_wra)){
      val <- sym(paste(varname, "_H1_", i, sep=""))
      intdt <- sym(paste("intdt", "_H1_", i, sep=""))
      
      core_wra <- core_wra %>% mutate_at(varname, ~if_else((!!intdt) < intdt_enroll & !is.na(!!val) & is.na(.), !!val, ., .))
    }
  }
}

core_subset <- core_wra %>% select(append(c("uuid", "intdt_enroll"), core_vars))
data <- data %>% left_join(core_subset, by = c("uuid", "intdt_enroll"))



mch <- mch_load %>% mutate(pc_delay = difftime(intdt_pc2_1, intdt_enroll_date, unit='week'))
mch <- mch %>% inner_join(data %>% select(uuid, ga_enroll, intdt_enroll), by=c("uuid", "intdt_enroll"))  %>%
  mutate(pc_13 = if_else(pc_delay + ga_enroll <= 13, 1, 0)) %>% 
  mutate(pc_24 = if_else(pc_delay + ga_enroll <= 24, 1, 0)) 

forms <- c("enroll", "pc2_1", "pc2_2", "pc2_3", "pc2_4", "pc2_5", "pc2_6", "pc2_7", "pc2_8",
           "pc3_1", "pc3_2", "pc3_3", "pc3_4")
pf_forms <- c("pf2_1", "pf2_2", "pf2_3", "pf3_1",
          "pf3_2", "pf3_3", "pf3_4", "pf4_1", "pf4_2", "pf4_3", "pf5_1", "pf5_2",
          "pf5_3")

#extract information available from MCH as of each week of gestation
for(timepoint in c(13, 24, 50)){
  for(varname in c("iron", "folicac", "ironfa", "uripain", "uriurg")){
    combined_name <- paste(varname, "_combined_", timepoint, sep="")
    mch[[combined_name]] = as.double(NA)
    for(form in forms){
      val <- sym(paste(varname, "_", form, sep=""))
      intdt <- sym(paste("intdt", "_", form, sep=""))
      mch <- mch %>% mutate_at(combined_name, ~if_else((difftime(!!intdt, intdt_enroll_date, unit='week') + ga_enroll <= timepoint) & !is.na(!!val) & (!!val == 1), 1, ., .))
    }
  }
  
  for(form in pf_forms){
    intdt <- paste0("intdt", "_", form)
    vdt <- paste0("vdate", "_", form)
    
    combined_name <- paste0(form, "_before_", timepoint)
    mch[[combined_name]] = as.integer(!is.na(mch[[intdt]]) & (difftime(mch[[vdt]], mch$intdt_enroll_date, unit='week') + mch$ga_enroll <= timepoint))
  }
  
  mch <- mch %>% mutate(!!paste0("n_anc_rec_", timepoint) := rowSums(across(contains(paste0("before_", timepoint)))))
}

#stitch together all information from MCH forms
mch_vars <- mch %>% select(uuid, intdt_enroll, iron_enroll,folicac_enroll, pc_delay, 
                           multbirth_enroll, ldelloc_enroll, lcalive_enroll, preg1age_enroll,
                           preghosp_enroll, gapyrs_enroll, lpregplan_enroll, pregfast_enroll, iron_pc2_1, 
                           iron_combined_13, iron_combined_24, folicac_combined_13, 
                           folicac_combined_24, folicac_pc2_1, pc_13, pc_24,
                           ironfa_combined_13, ironfa_combined_24, n_anc_rec_13, n_anc_rec_24, n_anc_rec_50,
                           pmhdiab_enroll, uripain_combined_13, uripain_combined_24, uriurg_combined_13,
                           uriurg_combined_24, dodel) %>%
  distinct()

#remove duplicates and merge with main dataset, matching on participant ID and enrollment date in MCH
mch_vars_deduped <- mch_vars %>% group_by(uuid, intdt_enroll) %>% filter(n() > 1) %>% ungroup() %>% filter(!is.na(iron_enroll)) 
mch_vars_nodups <- mch_vars %>% group_by(uuid, intdt_enroll) %>% filter(n() == 1) %>% ungroup() 

data <- data %>% left_join(rbind(mch_vars_deduped, mch_vars_nodups), by=c("uuid", "intdt_enroll"))

data$anc_vis <- data$n_anc_rec_50


#filter out observations with missing gestational age or LTFU before observation
#of any outcome
data <- data %>% mutate(dodel = dodel.y) %>%
  filter(dodel <= as.Date("2020-04-09")) %>%
  filter(!(outcome1 == 0 && anc_vis == 0)) %>%
  filter(!is.na(ga_enroll)) %>%
  select(-dodel, -dodel.x, -dodel.y)


#versions of data available at each time point

data_core_mnhenroll <- data %>% select(age_conc, oromo, other_ethn, muslim, protestant, edu_none,
                                   edu_higher, literate, farmer, merchant, petty_trade, housewife, 
                                  unemployed, daily_laborer, gov_employee, student, self_employed, married, 
                                  woreda, own_income, wealthindex_cat2, wealthindex_cat3, wealthindex_cat4, wealthindex_cat5, income_gov, income_farming, 
                                  income_ngo, income_merchant, income_private, income_dailylabor, income_pettytrader,
                                  hftime, hfdist, famsize, weight, height, bmi, alcohol_2, 
                                  eatdairy_2, eatbean_2, eatfruit_2, eatveg_2, eatvita_2, eatofruveg_2, eatroot_2, 
                                  eategg_2, eatfish_2, eatmeat_2, eatteff_2, eatcer_2, eatsug_2, eatoil_2, 
                                  eatfort_2, eatorg_2, eatcond_2, eatcoff_2, radiotv, mobphone, prevredtype, durlive, 
                                  prevreg, khat, feedchild, feedself, dchild, ndsum, contr, 
                                  muac_core, prev_still, 
                                  prev_misc, prev_preterm, prev_mg, prev_csection, prev_lbw, prev_bdefects, 
                                  gravidity_5, parity_5, ip_interval, pmhdiab_enroll, std,
                                  multbirth_enroll, ldelloc_enroll, lcalive_enroll, preg1age_enroll,
                                  gapyrs_enroll, lpregplan_enroll,
                                  anc_vis)

data_core_anc13 <- data %>% select(age_conc, oromo, other_ethn, muslim, protestant, edu_none,
                                         edu_higher, literate, farmer, merchant, petty_trade, housewife, 
                                         unemployed, daily_laborer, gov_employee, student, self_employed, married, 
                                         woreda, own_income, wealthindex_cat2, wealthindex_cat3, wealthindex_cat4, wealthindex_cat5, income_gov, income_farming, 
                                         income_ngo, income_merchant, income_private, income_dailylabor, income_pettytrader,
                                         hftime, hfdist, famsize, weight, height, bmi, alcohol_2, 
                                         eatdairy_2, eatbean_2, eatfruit_2, eatveg_2, eatvita_2, eatofruveg_2, eatroot_2, 
                                         eategg_2, eatfish_2, eatmeat_2, eatteff_2, eatcer_2, eatsug_2, eatoil_2, 
                                         eatfort_2, eatorg_2, eatcond_2, eatcoff_2, radiotv, mobphone, prevredtype, durlive, 
                                         prevreg, khat, feedchild, feedself, dchild, ndsum, contr, 
                                         muac_enroll, prev_still, vom_w13, hdache_w13, blurvis_w13, ruqabd_w13, urifreq_w13, fetmvmt_w13, vagbl_w13,
                                         prev_misc, prev_preterm, prev_mg, prev_csection, prev_lbw, prev_bdefects, 
                                         gravidity_5, parity_5, ip_interval, pmhdiab_enroll, std, 
                                         uripain_combined_13, uriurg_combined_13, pc_13,
                                         multbirth_enroll, ldelloc_enroll, lcalive_enroll, preg1age_enroll,
                                         gapyrs_enroll, iron_combined_13, folicac_combined_13, 
                                         ironfa_combined_13, n_anc_rec_13, anc_vis) %>%
  filter(n_anc_rec_13 == 0)


data_core_mnhpc_anc24 <- data %>% select(age_conc, oromo, other_ethn, muslim, protestant, edu_none,
                                         edu_higher, literate, farmer, merchant, petty_trade, housewife, 
                                         unemployed, daily_laborer, gov_employee, student, self_employed, married, 
                                         woreda, own_income, wealthindex_cat2, wealthindex_cat3, wealthindex_cat4, wealthindex_cat5, income_gov, income_farming, 
                                         income_ngo, income_merchant, income_private, income_dailylabor, income_pettytrader,
                                         hftime, hfdist, famsize, weight, height, bmi, alcohol_2, 
                                         eatdairy_2, eatbean_2, eatfruit_2, eatveg_2, eatvita_2, eatofruveg_2, eatroot_2, 
                                         eategg_2, eatfish_2, eatmeat_2, eatteff_2, eatcer_2, eatsug_2, eatoil_2, 
                                         eatfort_2, eatorg_2, eatcond_2, eatcoff_2, radiotv, mobphone, prevredtype, durlive, 
                                         prevreg, khat, feedchild, feedself, dchild, ndsum, contr, muac_enroll, prev_still, 
                                         prev_misc, prev_preterm, prev_mg, prev_csection, prev_lbw, prev_bdefects, 
                                         gravidity_5, parity_5, ip_interval, pmhdiab_enroll, std, vom_w24, hdache_w24, blurvis_w24, ruqabd_w24, urifreq_w24, fetmvmt_w24, vagbl_w24,
                                         uripain_combined_24, uriurg_combined_24, multbirth_enroll, ldelloc_enroll, 
                                         lcalive_enroll, preg1age_enroll, gapyrs_enroll, iron_combined_24, 
                                         folicac_combined_24,  pc_24,
                                         n_anc_rec_24, ironfa_combined_24,
                                         anc_vis) %>%
  filter(n_anc_rec_24 == 0)

data_baselines <- data %>% select(n_anc_rec_13, n_anc_rec_24)
data_baselines[is.na(data_baselines)] <- 0
dflist <- list(data_core_mnhenroll, data_core_anc13, data_core_mnhpc_anc24)

data_names = c("Conception", "Week 13", "Week 24")
threshold_n_visits = 1

#collect summary results and record of all predictions in these dataframes
results <- data.frame(data = character(), method=character(), fold=integer(), mape = double(), cor = double(), mse = double(), auc=double())
results_raw <- data.frame(data = character(), method = character(), fold=integer(), pred = double(), truth_binary = double(), truth_count=integer())

quantized_data <- F

#collect the models themselves from each CV fold here
lasso_models <- list()
tree_models <- list()


####################################################################
#Run models for each dataset, CV fold, with and without imputation
#####################################################################


for(df_idx in 1:length(dflist)){
  for(do_impute in c(T, F)){
    data_withoutna <- data.frame(dflist[[df_idx]])
    data_name <- data_names[df_idx]
    
    set.seed(1) 
    
    
    #add fold labels
    data_withoutna <- fold(data_withoutna, k = 5) %>% ungroup()
    
    #remove the label from the set of predictors
    data_predictors <- data_withoutna %>% select(-anc_vis)
    #columns from core/mch that we need dummies for
    dummies_to_make <- dummies[dummies %in% colnames(data_predictors)]
    
    
    if(do_impute){
      data_predictors <- data_predictors %>%
        mutate_each_(~factor(.),dummies_to_make)
      
      #add na indicators
      na_indicators <- 1*is.na(data_predictors)
      colnames(na_indicators) <- paste(colnames(na_indicators),"NA",sep="_")
  
      mr <- mice(data_predictors, m = 1, seed=500)
      
      data_predictors <- complete(mr, 1)
      
      data_predictors <- cbind(data_predictors, na_indicators)
      # if("lpregplan_enroll" %in% colnames(data_predictors)){
      #   data_predictors$lpregplan_enroll[is.na(data_predictors$lpregplan_enroll)] <- 0
      # }
      # 
      # if("multbirth_enroll" %in% colnames(data_predictors)){
      #   data_predictors$multbirth_enroll[is.na(data_predictors$multbirth_enroll)] <- 0
      # }
      data_predictors[is.na(data_predictors)] <- 0
    }
    
    
    data_predictors <- dummy_cols(data_predictors, select_columns = dummies_to_make, remove_selected_columns = T) 
    
    
    if(quantized_data){
      #quantize variables with more than 5 values
      data_predictors <- data_predictors %>% mutate(across(c(where(~length(unique(.)) > 5), -.folds), ~cut(., labels=F, breaks=unique(quantile(., probs=seq(0, 1, 0.2), na.rm=T)))))
      
      #make dummies for quantized variables
      cols <- colnames(data_predictors)[which(sapply(data_predictors, is.factor))]
      cols <- colnames(data_predictors)[which(sapply(data_predictors, function(x) length(unique(x)) > 4))]
      cols <- cols[cols != ".folds"]
      data_predictors <- dummy_cols(data_predictors, remove_selected_columns=T, select_columns = cols)
      
      #fill NAs with 0 (since we should have NA indicators)
      data_predictors[is.na(data_predictors)] <- 0 #note, this is currently overwriting NAs in the binary columns like eat_
      
    }
    
    
    data_response <- data_withoutna %>% select(anc_vis, .folds)
    
    data_all <- cbind(data_predictors, data_response)
    
    # data_baselines <- cbind(data_baselines, data_withoutna %>% select(.folds))
    
  
    target_sensitivity = 0.9
    trees <- c()
    alpha = 0
    gamma = 0
    for(i in 1:5){
      #train/test split for this fold
      train_predictors <- data_predictors[data_predictors$.folds != i,] %>% ungroup() %>% select(-.folds)

      train_all <- data_all[data_all$.folds != i,] %>% ungroup() %>% select(-.folds)
      test_all  <- data_all[data_all$.folds == i,] %>% ungroup() %>% select(-.folds)  
      
    
      test_predictors <- test_all %>% select(-anc_vis)
        
      train_response = data_response[data_response$.folds != i,]
      test_response = data_response[data_response$.folds == i,]  
      

    
      train_count_response <- train_response$anc_vis
      test_count_response <- test_response$anc_vis 
      train_binary_response <- as.integer(train_response$anc_vis < threshold_n_visits)
      test_binary_response <- as.integer(test_response$anc_vis < threshold_n_visits) 
      
      #xgboost -- only run this on data that is not quantized/imputed 
      if(!quantized_data && ! do_impute){
        tree <- xgboost(data = data.matrix(train_predictors), label = train_count_response,max.depth = 2, eta = 1, alpha = alpha, gamma=gamma, nthread = 2, nrounds = 20, objective = "count:poisson")
        preds <- predict(tree, data.matrix(test_predictors))
        test_mse <- mean(preds - test_count_response)**2
        test_cor <- cor(preds, test_count_response)
        test_auc <- as.double(roc(as.integer(test_binary_response), preds)$auc)
        
        results <- results %>% add_row(data = data_name, method='xgb_poisson', fold=i, mse=test_mse, cor=test_cor, auc=test_auc)
        tree_xgb_poisson <- tree
        new_raw_results <- data.frame(data = data_name, method='xgb_poisson', fold=i, pred = preds, truth_binary = test_binary_response, truth_count = test_count_response)
        results_raw <- results_raw %>% bind_rows(new_raw_results)
        if(i == 5){
          tree_models[[data_name]] <- tree_xgb_poisson
        }
        
      }
      # # #lasso logistic model -- only do this with quantization or imputing to handle missingness
      if(quantized_data | do_impute){
        cv_lasso_logistic <- cv.glmnet(data.matrix(train_predictors),train_binary_response,alpha=1,family="binomial")
        preds <- predict(cv_lasso_logistic, newx = data.matrix(test_predictors), s=cv_lasso_logistic$lambda.min, type="response")[,1]
        test_mse <- mean(preds - test_count_response)**2
        test_cor <- cor(preds, test_count_response)
        test_auc <- as.double(roc(as.integer(test_binary_response), preds)$auc)
        results <- results %>% add_row(data = data_name, method='lasso_logistic', fold=i, mse=test_mse, cor=test_cor, auc=test_auc)
        new_raw_results <- data.frame(data = data_name, method='lasso_logistic', fold=i, pred = preds, truth_binary = test_binary_response, truth_count = test_count_response)
        results_raw <- results_raw %>% bind_rows(new_raw_results)
        if(i == 5){
          lasso_models[[data_name]] <- cv_lasso_logistic
        }
      }
    
      }
  }
}

####################################################################
#Make plots and tables
#####################################################################

results$data[results$data == "Core + MNH enrollment"] <- "MNH enrollment"
results$method[results$method == "lasso_logistic"] <- "LASSO"
results$method[results$method == "xgb_poisson"] <- "XGBoost"

#make table and plot with AUC values

results_raw$data[results_raw$data == "Core + MNH enrollment"] <- "Preconception"
results_raw$data[results_raw$data == "Preconception"] <- "Conception"

results_raw$method[results_raw$method == "lasso_logistic"] <- "LASSO"
results_raw$method[results_raw$method == "xgb_poisson"] <- "XGBoost"
results_raw <- results_raw %>% filter(data != 'Core')

summary <- data.frame(method = character(), data = character(), auc = double(), auc_ci_lower = double(), auc_ci_upper = double())

for(method_name in unique(results_raw$method)){
  for(data_name in unique(results_raw$data)){
    subset <- results_raw %>% filter(method==method_name) %>% filter(data==data_name)
    roc_obj <- roc(as.integer(subset$truth_binary), subset$pred, ci=TRUE, ci.alpha=0.95)
    summary <- summary %>% add_row(method=method_name, data=data_name, auc=roc_obj$ci[2], 
                                   auc_ci_lower = roc_obj$ci[1],  auc_ci_upper = roc_obj$ci[3])
    
  }
}


summary <- summary %>% mutate(auc_combined = sprintf("%.2f (%.2f, %.2f)", auc, auc_ci_lower, auc_ci_upper))


summary$method[summary$method == "XGBoost (decision tree)"] <- "XGBoost"

summary_results <- spread(summary %>% select(-auc, -auc_ci_lower, -auc_ci_upper) %>% rename(Time=data), method, auc_combined)

summary_results %>% kable(booktabs=T, caption="Table 3: AUC of predictive models.") %>%
  kable_classic(full_width=F,html_font = "Arial") 


plots = list()
title = list()
title[["LASSO"]] = "Logistic regression with LASSO"
title[["XGBoost"]] = "Decision tree ensemble"


for(method_name in unique(results_raw$method)){
  roclist = list()
  for(data_name in unique(results_raw$data)){
    subset <- results_raw %>% filter(method==method_name) %>% filter(data==data_name)
    roclist[[data_name]] = roc(as.integer(subset$truth_binary), subset$pred)
  }
  
  g <- ggroc(roclist, aes = "color", legacy.axes = TRUE, size=1.5) +
    geom_abline() +
    theme_bw() +
    labs(x = "False Positive Rate",
         y = "True Positive Rate",
         color = "Model") +
    scale_x_continuous(breaks = seq(0,1,0.25), lim=c(0,1.0), expand = expand_scale()) +
    scale_y_continuous(breaks = seq(0,1,0.25), lim=c(0,1.0), expand = expand_scale())+ 
    theme(axis.text = element_text(size = 17), axis.title = element_text(size = 20), 
          legend.text = element_text(size = 20), legend.title = element_text(size = 20), 
          plot.title = element_text(size = 20)) +
    ggtitle(title[[method_name]])
  # if(method_name == "LASSO"){
  #   g <- g + theme(legend.position="none")
  # }
  g <- g + theme(legend.position="none")
  
  # g
  # ggsave(paste0("roccurve_", method_name, ".png"))
  plots[[method_name]] <- g
  
}

ggarrange(plots$LASSO, plots$XGBoost, ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave(paste0("roccurve_combined", ".jpg"), dpi=700, width = 15, height = 8)




#pull out the coefficients for the LASSO models and make a table with the ORs for the most important
all_coefs = data.frame(Name = character(), Coefficient = double(), OR = double(), data=character())
for(data_name in unique(results_raw$data)){
  cv_lasso_logistic <- lasso_models[[data_name]]
  
  coefs <- coef(cv_lasso_logistic, s = cv_lasso_logistic$lambda.min)
  coefs <- data.frame(Name = dimnames(coefs)[[1]], Coefficient = matrix(coefs), OR = exp(matrix(coefs)))
  coefs$data <- data_name
  all_coefs <- all_coefs %>% bind_rows(coefs)  
}

all_coefs$Name[all_coefs$Name == "pc_13_NA"] <- "No community visit"
all_coefs$Name[all_coefs$Name == "pc_24_NA"] <- "No community visit"
all_coefs$Name[all_coefs$Name == "uriurg_combined_13"] <- "uriurg"
all_coefs$Name[all_coefs$Name == "uriurg_combined_24"] <- "uriurg"
all_coefs$data[all_coefs$data == "Core + MNH enrollment"] <- "Preconception"
all_coefs$data[all_coefs$data == "Preconception"] <- "Conception"


total_coefs  <- all_coefs %>% group_by(Name) %>% select(Name, Coefficient) %>% mutate(totalcoef = sum(abs(Coefficient)))%>%
  select(-Coefficient) %>%
  unique() %>% 
  arrange(desc(abs(totalcoef))) %>%
  head(15) %>%
  filter(Name != "(Intercept)")

#convert to odds ratios
all_coefs$Coefficient <- exp(all_coefs$Coefficient)
all_coefs$coef_format <- as.character(all_coefs$Coefficient)
all_coefs$coef_format[all_coefs$coef_format == '1'] <- '-'
all_coefs$coef_format <- substr(all_coefs$coef_format, 1, 4)
  
all_coefs %>% filter(Name %in% total_coefs$Name) %>%
  filter(data != 'Core') %>%
  select(Name, coef_format, data) %>%
  spread(data, coef_format) %>%
  kable( booktabs = T, caption="Table 4: LASSO model coefficients for top predictors") %>%
  kable_classic(full_width=F,html_font = "Arial")


#make a table for the tree ensemble with feature importances

all_coefs = data.frame(Name = character(), Coefficient = double(), data=character())
for(data_name in unique(results_raw$data)){
  model <- tree_models[[data_name]]
  
  # coefs <- coef(cv_lasso_logistic, s = cv_lasso_logistic$lambda.min)
  importances <- xgb.importance(model=model)
  coefs <- data.frame(Name = importances$Feature, Coefficient = importances$Gain)
  coefs$data <- data_name
  all_coefs <- all_coefs %>% bind_rows(coefs)  
}

all_coefs$Name[all_coefs$Name == "pc_13_NA"] <- "No community visit"
all_coefs$Name[all_coefs$Name == "pc_24_NA"] <- "No community visit"
all_coefs$Name[all_coefs$Name == "uriurg_combined_13"] <- "uriurg"
all_coefs$Name[all_coefs$Name == "uriurg_combined_24"] <- "uriurg"
all_coefs$data[all_coefs$data == "Core + MNH enrollment"] <- "Preconception"
all_coefs$data[all_coefs$data == "Preconception"] <- "Conception"


total_coefs  <- all_coefs %>% group_by(Name) %>% select(Name, Coefficient) %>% mutate(totalcoef = sum(abs(Coefficient)))%>%
  select(-Coefficient) %>%
  unique() %>% 
  arrange(desc(abs(totalcoef))) %>%
  head(15) %>%
  filter(Name != "(Intercept)")

all_coefs$coef_format <- as.character(all_coefs$Coefficient)
all_coefs$coef_format[all_coefs$coef_format == '1'] <- '-'
all_coefs$coef_format <- substr(all_coefs$coef_format, 1, 4)

all_coefs %>% filter(Name %in% total_coefs$Name) %>%
  filter(data != 'Core') %>%
  select(Name, coef_format, data) %>%
  spread(data, coef_format) %>%
  kable( booktabs = T, caption="Table 4: Decision tree ensemble model coefficients for top predictors") %>%
  kable_classic(full_width=F,html_font = "Arial")
