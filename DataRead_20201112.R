# CBCL trajectories and the IQ-achievement discrepancy

# run this code first!!

######################
#### READ IN DATA ####
######################

# Open library
library(foreign)

# functions

## replace na 
replacenas <- function(dat, miss_spec = c(777,888,999), col = c(2:length(dat))){
  for (j in 1:length(miss_spec)){
    for (i in 1:length(col)){
      dat[,col[i]] <- ifelse(dat[,col[i]] == miss_spec[j], NA, dat[,col[i]])
    }
  }
  return (dat)
}

## read in data quickly
readquick <- function(path){ # only works for spss
  dataframe <- read.spss(path, use.value.labels = F, to.data.frame = T)
  names(dataframe) <- tolower(names(dataframe))
  dataframe <- replacenas(dataframe)
  return(dataframe)
}

### Once a Risk, Always a Risk
oncerisk <- function(variables, cutoffs, method = 'both'){#always lower than...
  variables <- data.frame(variables)
  
  if(length(cutoffs)==1){
    print('cut off is used for all variables')
    cutoffs <- rep(cutoffs, ncol(variables))
  }  
  
  if (length(cutoffs) != ncol(variables)){
    stop('specify cut off for every variable')
  }
  
  for (i in 1:length(variables)){
    variables[i] <-  ifelse(variables[i] < cutoffs[i], 1, 0)
    final <- data.frame(variables[1:i])
    fin <- rowSums(final)}
  
  once <- ifelse(fin > 0, 1, 0)
  always <- ifelse(fin == ncol(final), 1, 0)
  oncealways <- data.frame(once, always)
  
  if (method == 'both'){
    return(oncealways)
  }
  
  if (method == 'once'){
    return(once)
  }
  
  if (method == 'always'){
    return(always)
  }
  
} 

#--------

#----------------------------------------------------------
# BASE: GENERAL INFO
#----------------------------------------------------------

# GENERAL
general <- readquick("O:/medewerkers/042647 Schuurmans, I/DATA/CHILD-ALLGENERALDATA_11052020.sav")

# FINAL RECODING: COVARIATES

## ETHNICITY (0 = western, 1 = non-western)

### create new empty variable
general$ethni <- rep(NA, nrow(general))

### recode ethnicities  
for (i in 1:nrow(general)){
  if (is.na(general$ethninfv2[i])){
    general$ethni[i] <- NA}
  else{
    if (general$ethninfv2[i] == 2|general$ethninfv2[i] == 3|general$ethninfv2[i] == 4|
        general$ethninfv2[i] == 5|general$ethninfv2[i] == 6|general$ethninfv2[i] == 7|
        general$ethninfv2[i] == 200|general$ethninfv2[i] == 400|general$ethninfv2[i] == 600){
      general$ethni[i] <- 2}
    else {
      if (general$ethninfv2[i] == 300| general$ethninfv2[i] == 500|
          general$ethninfv2[i] == 700|general$ethninfv2[i] == 800){
        general$ethni[i] <- 1}
      else {
        if (general$ethninfv2[i] == 1){
          general$ethni[i] <- 0}}}}}

#----------------------------------------------------------

## INCOME (0 = never been pover during age 3, 5, and 9, 1 = have been pover once) # Ageneral PREGNANCY PERIOD

### recode 
general$income <- ifelse(general$income5 < 4, 1, 0)

### recode 
general$edu <- rep(NA, nrow(general))
general$edu[which(general$educm5<3)] <- 1
general$edu[(which(general$educm5==3 | general$educm5==4))] <- 2
general$edu[which(general$educm5>4)] <- 3
general$edu <- as.factor(general$edu)

#----------------------------------------------------------
# PREDICTOR: CBCL TRAJECTORIES
#----------------------------------------------------------

trf <- readquick("o:/medewerkers/042647 schuurmans, i/data/cbcl/TRF_incl_Tscores_20201111.sav")
cbclvar5 <- readquick("o:/medewerkers/042647 schuurmans, i/data/cbcl/CBCL_6_incl_Tscores_20201111.sav")

# change names in trf to avoid doubles
for (i in 1:ncol(trf)){
  names(trf)[i] <- paste0('TRF_', names(trf[i]))
}

for (i in 1:ncol(cbclvar5)){
  names(cbclvar5)[i] <- paste0('CBCL_', names(cbclvar5[i]))
}

#----------------------------------------------------------
# OUTCOME: IQ AND WISC
#----------------------------------------------------------

## Outcome data Cito

# read in data cito school report
cito1 <- readquick("o:/medewerkers/042647 schuurmans, i/data/childcito_15122017.sav")

# read in data cito mother report
cito2 <- readquick("o:/medewerkers/042647 schuurmans, i/data/GR1093-F_School_13052020.sav")

# merge both
cito <- merge(cito1, cito2, by = 'idc', all = T)

# make new cito variable
cito$cito <- ifelse(is.na(cito$citotest), yes = cito$f0200293_cleaned_citoscore, no = cito$citotest)
cito$reportcito <- ifelse(is.na(cito$citotest), yes = 'mother', no = 'teacher')
cito$age <- ifelse(is.na(cito$citotest), cito$agechildcito_gr1093, cito$age_cito)

rm(cito1, cito2)

#--------

## Outcome data Wisc

# read in cognition data -> use son as we have no access to wisc yet
# read in data cito school report
cogn <- readquick("o:/medewerkers/042647 schuurmans, i/data/WISC/20201206/19112020_WISC_PRELIMINARYDATA.sav")

# make an IQ-score (orthogonalize on age + scale, exclude, weight)
cogn$IQ <- cogn$wisc13_fsiq

#--------

# COMBINE ALL
df2 <- merge(general, cogn, all = T, by = 'idc')
df3 <- merge(df2, cito, all = T, by = 'idc')
df4 <- merge(df3, cbclvar5, all = T, by.x = 'idc', by.y = 'CBCL_idc')
df <- merge(df4, trf, all = T, by.x = 'idc', by.y = 'TRF_idc')

#----------------------------------------------------------
# FLOWCHART
#----------------------------------------------------------

# We start with the kids who participated and the focus at 13 wave
data <- df[which(!is.na(df$visit13)),]   
block1 <- nrow(data)

# Exclude kids with no TRF or CBCL
data1 <- data[which(!is.na(data$CBCL_externalizing_problems_tscore_5)),]
data2 <- data1[which(!is.na(data1$CBCL_internalizing_problems_tscore_5)),]
block2_1 <- nrow(data2)
suba_1<- block2_1-block1
data3 <- data2[which(!is.na(data2$TRF_externalizing_problems_tscore)),]
data4 <- data3[which(!is.na(data3$TRF_internalizing_problems_tscore)),]
block2_2 <- nrow(data4)
suba_2 <- block2_2-block2_1

# Exclude kids with no cito data
data5 <- data4[which(!is.na(data4$cito)),]
block3 <- nrow(data5)
subb <- block3-block2_2

# Exclude kids with missing cognition data
data6 <- data5[which(!is.na(data5$IQ)),]
block4 <- nrow(data6)
subc <- block4-block3

# Flowchart
(figureS1 <- list(block1, suba_1, block2_1, suba_2, block2_2, subb, block3, subc, block4))

#----------------------------------------------------------
# SELECT VARIABLES
#----------------------------------------------------------

tmp <- data6[,c('idc','cito','IQ',"TRF_age_trf","CBCL_agechild_gr1075", "agechild13" ,
                'age', 'ethni', 'income', 'edu', 'gender', 
                "TRF_anxious__depressed_tscore","TRF_withdrawn__depressed_tscore",                            
                "TRF_somatic_complaints_tscore","TRF_social_problems_tscore",                                 
                "TRF_thought_problems_tscore","TRF_attention_problems_tscore",                              
                "TRF_rule_breaking_behavior_tscore","TRF_aggressive_behavior_tscore",
                "CBCL_emotionally_reactive_tscore_5","CBCL_anxious__depressed_tscore_5","CBCL_somatic_complaints_tscore_5",                      
                "CBCL_withdrawn_tscore_5","CBCL_sleep_problems_tscore_5",                           
                "CBCL_attention_problems_tscore_5", "CBCL_aggressive_behavior_tscore_5", "TRF_school_cluster", 'TRF_teacher_cluster',"ethninfv2", 'reportcito',
                'coding_assessment',
                'TRF_total_problems_tscore','CBCL_total_problems_tscore_5', "CBCL_gr1075rbm",
                "TRF_internalizing_problems_tscore","TRF_externalizing_problems_tscore",
                "CBCL_internalizing_problems_tscore_5","CBCL_externalizing_problems_tscore_5")]

# check for stimulants
library(haven)
stim <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_2_BP_CM/CHILDADHDMEDICATION9_17112016.sav")
test <- merge(tmp, stim, by.x = 'idc', by.y = 'IDC')

test$ADHDmed[test$ADHDmed==999] <- NA
table(test$ADHDmed,exclude = F)
prop.table(table(test$ADHDmed,exclude = F))
