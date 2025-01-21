# NON RESPONSE ANALYSIS
# I.K. Schuurmans, 2021-08-30

# open library
library(haven)
library(xlsx)

# read in data
tmp_study <- tmp
tmp_full_1 <- readRDS("O:/medewerkers/042647 Schuurmans, I/Project_1_ELS_CM/tmp.rds")

# add cbcl and IQ
iq <- read_sav("O:/medewerkers/042647 Schuurmans, I/DATA/CHILDSONIQ5_06042012.sav")
tmp_full_2 <- merge(tmp_full_1, iq, by.x = 'idc', by.y = 'IDC', all = T)
tmp_full <- merge(tmp_full_2, cbclvar5, by.x = 'idc', by.y = 'CBCL_idc', all = T)

#---------------------------------------------------------------
# NON RESPONSE
#---------------------------------------------------------------

# function
nonresponse <- function(ids_study, ids_full, dataset, variables){
  
  # convert dataset to dataframe
  dataset <- as.data.frame(dataset)[]
  
  # make indicator variable for included and excluded participants
  dataset$indicator <- ids_full %in% ids_study
  
  # get empty table
  table_stats <- matrix(NA, nrow = (length(variables)), ncol = 5)
  
  # compare on the variables
  
  for (i in 1:length(variables)){
    
    # t-test continuous outcomes
    if (is.numeric(dataset[,variables[i]])){
      
      table_stats[i,1] <- paste(round(mean(dataset[dataset$indicator == T,variables[i]], na.rm = T),2), round(sd(dataset[dataset$indicator == T,variables[i]], na.rm = T),2), sep = ' ± ')
      table_stats[i,2] <- paste(round(mean(dataset[dataset$indicator == F,variables[i]], na.rm = T),2), round(sd(dataset[dataset$indicator == F,variables[i]], na.rm = T),2), sep = ' ± ')
      table_stats[i,3] <- round(t.test(dataset[dataset$indicator == T,variables[i]], dataset[dataset$indicator == F,variables[i]])$statistic, 2)
      table_stats[i,4] <- round(t.test(dataset[dataset$indicator == T,variables[i]], dataset[dataset$indicator == F,variables[i]])$parameter, 2)
      table_stats[i,5] <- round(t.test(dataset[dataset$indicator == T,variables[i]], dataset[dataset$indicator == F,variables[i]])$p.value, 3)
      
    } 
    
    # chi-square for categorical outcomes 
    else {
      
      table_stats[i,1] <- paste(table(dataset[dataset$indicator == T,variables[i]])[1], ' (', round(prop.table(table(dataset[dataset$indicator == T,variables[i]]))[1]*100,1), ')', sep = '')
      table_stats[i,2] <- paste(table(dataset[dataset$indicator == F,variables[i]])[1], ' (', round(prop.table(table(dataset[dataset$indicator == F,variables[i]]))[1]*100,1), ')', sep = '') 
      table_stats[i,3] <- round(chisq.test(dataset[,variables[i]], dataset$indicator)$statistic, 2)
      table_stats[i,4] <- round(chisq.test(dataset[,variables[i]], dataset$indicator)$parameter, 2)
      table_stats[i,5] <- round(chisq.test(dataset[,variables[i]], dataset$indicator)$p.value, 3)
      
    }
    
  }
  
  # clean up stats
  colnames(table_stats) <- c('mean study', 'mean remaining','t/X', 'df_t', 'p_t')
  rownames(table_stats) <- variables
  
  return(table_stats)
  
}

## we will check non-response with auxiliary variables and gender

# convert to correct data types GENDER
tmp_full$gender <- tmp_full$gender - 1
tmp_full$smoking <- ifelse(tmp_full$smoking == 3, 2, tmp_full$smoking) - 1
tmp_full[,c('ethnicity', 'smoking', 'gender', 'edum','edup','mari','edu',"mardich.y")] <- 
  apply(tmp_full[,c('ethnicity', 'smoking', 'gender', 'edum','edup','mari','edu',"mardich.y")], 2, as.factor)

# run non-response
tab1 <- nonresponse(ids_study = tmp_study$idc, ids_full = tmp_full$idc, dataset = tmp_full, 
                    variables = c('gender','ethnicity', 'gestbir', 'weight','F0300178',
                                  'age_m_v2','edu','smoking','bmi_1', 'dep'))

tab2 <- nonresponse(ids_study = tmp_study$idc, ids_full = tmp_full$idc[which(tmp_full$visit13==1)], dataset = tmp_full[which(tmp_full$visit13==1),], 
                    variables = c('gender','ethnicity', 'gestbir', 'weight','F0300178',
                                  'age_m_v2','edu','smoking','bmi_1', 'dep'))

write.xlsx(tab1, 'nonresponse_genr_20220107.xlsx')
write.xlsx(tab2, 'nonresponse_f13_20220107.xlsx')
getwd()

#---------------------------------------------------------------
# ATTRITION ANALYSES
#---------------------------------------------------------------

# function
attrition <- function(ids_study, ids_full, dataset, variables){
  
  # convert dataset to dataframe
  dataset <- as.data.frame(dataset)[]
  
  # make indicator variable for included and excluded participants
  indicator <- ids_full %in% ids_study
  
  # get data set will all variables
  dataset_vars <- dataset[,variables]
  
  # get empty table
  table_stats <- matrix(NA, nrow = (length(variables)), ncol = 6)
  
  # individual models
  
  for (i in 1:length(variables)){
    
    # logistic regression for each seperate variable
    mod <- summary(glm(indicator ~ dataset_vars[,i], family = binomial))
    
    table_stats[i,1] <- round(exp(mod$coefficients[2]),2)
    table_stats[i,2] <- paste0(round(exp(mod$coefficients[2]-1.96*mod$coefficients[2,2]),2), ', ',
                               round(exp(mod$coefficients[2]+1.96*mod$coefficients[2,2]),2), sep = '')
    table_stats[i,3] <- round(mod$coefficients[2,4], 3)

  }
  
  # simulaneous model
  dataset_vars$indicator <- ids_full %in% ids_study
  mod <- summary(glm(indicator ~ ., data = dataset_vars, family = binomial))
  
  table_stats[,4] <- round(exp(mod$coefficients[2:ncol(dataset_vars)]),2)
  table_stats[,5] <- paste0(round(exp(mod$coefficients[2:ncol(dataset_vars)]-1.96*mod$coefficients[2:ncol(dataset_vars),2]),2), ', ',
                             round(exp(mod$coefficients[2:ncol(dataset_vars)]+1.96*mod$coefficients[2:ncol(dataset_vars),2]),2), sep = '')
  table_stats[,6] <- round(mod$coefficients[2:ncol(dataset_vars),4], 3)
  
  # clean up stats
  colnames(table_stats) <- c('odds ind', 'ci ind','p ind', 'odds sim', 'ci sim','p sim')
  rownames(table_stats) <- variables
  
  return(table_stats)
  
}

## we will check attrition with auxiliary variables and gender

#get idc
cbcl <- cbclvar5$CBCL_idc[!is.na(cbclvar5$CBCL_total_problems_tscore_5)]
trf <- trf$TRF_idc[!is.na(trf$TRF_total_problems_tscore)]
wisc <- cogn$idc[!is.na(cogn$IQ)]
cito <- cito$idc[!is.na(cito$cito)]

# run non-response
tab_att_cbcl <- attrition(ids_study = cbcl, ids_full = tmp_full$idc, dataset = tmp_full, 
                      variables = c('gender','ethnicity', 'weight','F0300178',
                                    'age_m_v2','edu','smoking', 'bmi_1','dep'))
tab_att_trf <- attrition(ids_study = trf, ids_full = tmp_full$idc, dataset = tmp_full, 
                      variables = c('gender','ethnicity', 'weight','CBCL_total_problems_tscore_5','F0300178',
                                    'age_m_v2','edu','smoking', 'bmi_1','dep'))
tab_att_cito <- attrition(ids_study = cito, ids_full = tmp_full$idc, dataset = tmp_full, 
                      variables = c('gender','ethnicity', 'weight','CBCL_total_problems_tscore_5','F0300178',
                                    'age_m_v2','edu','smoking', 'bmi_1','dep'))
tab_att_wisc <- attrition(ids_study = wisc, ids_full = tmp_full$idc, dataset = tmp_full, 
                      variables = c('gender','ethnicity', 'weight','CBCL_total_problems_tscore_5','F0300178',
                                    'age_m_v2','edu','smoking', 'bmi_1','dep'))

tab_att <- rbind(tab_att_cbcl, tab_att_trf, tab_att_cito, tab_att_wisc)

write.xlsx(tab_att, 'attrition_20220107.xlsx')



