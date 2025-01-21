# ANALYSIS CBCL TRAJECTORIES AND THE IQ-ACHIEVEMENT DISCREPANCY

setwd("o:/medewerkers/042647 schuurmans, i/Project_2_BP_CM")
neff = 19.12 # number of corrections to do
options(scipen=0) # scientific notation allowed

# library
library(ggplot2)
library(xlsx)
library(magrittr)
library(mice)
library(car)

# functions
source("R code/Functions_20201230.R")

#----------------------------------------------------------
# PREPARE VARIABLES
#----------------------------------------------------------

# standardise IQ and COGNITION
tmp$cito <- as.numeric(scale(tmp$cito))
IQ <- tmp$IQ
tmp$IQ <- as.numeric(scale(tmp$IQ))

# construct IQ ACHIEVEMENT DISCREPANCY
tmp$cm <- as.numeric(scale(resid(lm(cito ~ IQ, data = tmp))))

# tmp$gender (1 = girl)
tmp$gender <- tmp$gender - 1

# select order confounders # gender should be first
confound <- tmp[,c('gender', 'age', 'income', 'ethni')]

# read in the implist - no need to run again
implist <- readRDS("imp_data_BP_20212604.rds")

# remove low IQs
table(IQ<75)
tmp <- tmp[-which(IQ<75),]
implist_c <- complete(implist, action  = 'long', include = T)
implist_c <- implist_c[implist_c$idc %in% tmp$idc,]
implist <- as.mids(implist_c)

# select order predictors
predictors_sep <- tmp[,c('TRF_total_problems_tscore',"TRF_internalizing_problems_tscore","TRF_externalizing_problems_tscore",  
                         "TRF_anxious__depressed_tscore","TRF_withdrawn__depressed_tscore",                            
                         "TRF_somatic_complaints_tscore","TRF_aggressive_behavior_tscore","TRF_attention_problems_tscore", 
                         "TRF_social_problems_tscore","TRF_thought_problems_tscore",                        
                         "TRF_rule_breaking_behavior_tscore",
                         'CBCL_total_problems_tscore_5',"CBCL_internalizing_problems_tscore_5","CBCL_externalizing_problems_tscore_5",
                         "CBCL_anxious__depressed_tscore_5",                      
                         "CBCL_withdrawn_tscore_5","CBCL_somatic_complaints_tscore_5",
                         "CBCL_aggressive_behavior_tscore_5","CBCL_attention_problems_tscore_5",                           
                         "CBCL_emotionally_reactive_tscore_5", "CBCL_sleep_problems_tscore_5")]


#----------------------------------------------------------
# VARIABLE LEVEL SEPERATE MODELS
#----------------------------------------------------------

# make seperate file with confounders
confound <- tmp[,c('age', 'gender', 'income', 'ethni')]

# analysis
tableS3a <- coefficienttable(tmp$cm, predictors_sep, confound, analysis = 'seperate regression', MI = T, implist=implist, neff = neff)
tableS3b <- coefficienttable(tmp$IQ, predictors_sep, confound, analysis = 'seperate regression', MI = T, implist=implist, neff = neff)
tableS3c <- coefficienttable(tmp$cito, predictors_sep, confound, analysis = 'seperate regression', MI = T, implist=implist, neff = neff)

# combine and write out
tableS3 <- cbind(tableS3a, tableS3b, tableS3c)
write.xlsx(tableS3, 'tableS3_cbcl.xlsx')


