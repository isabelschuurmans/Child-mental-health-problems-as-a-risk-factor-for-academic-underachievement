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
# DESCRIPTIVES
#----------------------------------------------------------

# TABLE DESCRIPTIVES

# child characteristics 
row1_1 <- c(nrow(tmp[tmp$gender == 1,]), 
            round(nrow(tmp[tmp$gender == 1,])/nrow(tmp),2))
row1_2 <- c(nrow(tmp[tmp$gender == 2,]), 
            round(nrow(tmp[tmp$gender == 2,])/nrow(tmp),2))

row1_3 <- meansd(tmp$TRF_age_trf/12)
row1_4 <- meansd(tmp$CBCL_agechild_gr1075/12)

row1_5 <- meansd(tmp$agechild13)
row1_6 <- meansd(tmp$age)

row1_7 <- c(nrow(tmp[which(tmp$ethni == 0),]), 
            round(nrow(tmp[which(tmp$ethni == 0),])/nrow(tmp),3))
row1_8 <- c(nrow(tmp[which(tmp$ethni == 1),]), 
            round(nrow(tmp[which(tmp$ethni == 1),])/nrow(tmp),3))
row1_9 <- c(nrow(tmp[which(tmp$ethni == 2),]), 
            round(nrow(tmp[which(tmp$ethni == 2),])/nrow(tmp),3))

row1_10 <- meansd(tmp$IQ)
row1_11 <- meansd(tmp$cito)
row1_12 <- c(0,1)
# stimulant usage at the end of the data read script!

# family characteristics 
row1_13 <- c(nrow(tmp[which(tmp$income == 1),]), 
            round(nrow(tmp[which(tmp$income == 1),])/nrow(tmp),3))
row1_14 <- c(nrow(tmp[which(tmp$income == 0),]), 
             round(nrow(tmp[which(tmp$income == 0),])/nrow(tmp),3))

row1_15 <- c(nrow(tmp[which(tmp$edu == 1),]), 
             round(nrow(tmp[which(tmp$edu == 1),])/nrow(tmp),3))
row1_16 <- c(nrow(tmp[which(tmp$edu == 2),]), 
             round(nrow(tmp[which(tmp$edu == 2),])/nrow(tmp),3))
row1_17 <- c(nrow(tmp[which(tmp$edu == 3),]), 
             round(nrow(tmp[which(tmp$edu == 3),])/nrow(tmp),3))

table1 <- rbind(row1_2, row1_3, row1_4, row1_5, row1_6, row1_7, row1_8, row1_9, row1_10, row1_11, row1_12, row1_13, row1_14, row1_15, row1_16, row1_17)
write.xlsx(table1, 'table1_cbcl.xlsx')

#----------------------------------------------------------
# PREPARE VARIABLES
#----------------------------------------------------------

# standardise IQ and COGNITION
tmp$cito <- as.numeric(scale(tmp$cito))
tmp$IQ <- as.numeric(scale(tmp$IQ))

# construct IQ ACHIEVEMENT DISCREPANCY
tmp$cm <- as.numeric(scale(resid(lm(cito ~ IQ, data = tmp))))
tmp$status_cm <- as.factor(recode(tmp$cm, 'lo:-1=2; 1:hi=3; else=1'))

# tmp$gender (1 = girl)
tmp$gender <- tmp$gender - 1

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

# select order confounders # gender should be first
confound <- tmp[,c('gender', 'age', 'income', 'ethni')]

# select order and construct interaction with gender
predictors_int <- data.frame(matrix(NA, nrow(predictors_sep), 2*ncol(predictors_sep)))
predictors_toadd <- data.frame(matrix(NA, nrow(predictors_sep), ncol(predictors_sep)))
for (i in 1:ncol(predictors_sep)){
  j <- i*2
  k <- j-1
  predictors_int[,k] <- as.numeric(scale(predictors_sep[,i]))
  predictors_int[,j] <- as.numeric(scale(predictors_sep[,i])*tmp$gender)
  predictors_toadd[,i] <- as.numeric(scale(predictors_sep[,i])*tmp$gender)
  names(predictors_int)[k] <- names(predictors_sep)[i]
  names(predictors_int)[j] <- paste0('int_', names(predictors_sep)[i])
  names(predictors_toadd)[i] <- paste0('int_', names(predictors_sep)[i])
}
tmp <- cbind(tmp, predictors_toadd)

#----------------------------------------------------------
# MULTIPLE IMPUTATION
#----------------------------------------------------------

# set up run
imp0 <- mice(tmp, maxit = 0, defaultMethod = c('pmm', 'pmm', 'pmm', 'pmm'))

# consider the methods for the variables with missings
meth <- imp0$method
# variables with a imp method -> age, ethni, income, edu, ethninfv2

# change predictor matrix
predictormatrix <- imp0$predictorMatrix
# COL - we don't want to use: idc, trf_age_trf, CBCL_age_child, agechild13, trf_school, trf_teacher, ethniv, reportcito, cito, interactions
# cito out because of multicolinearity
predictormatrix[,c('idc', 'TRF_age_trf', 'CBCL_agechild_gr1075', 'agechild13', 'TRF_school_cluster', 'TRF_teacher_cluster', 'ethninfv2', 'reportcito', 'cito', 'coding_assessment','CBCL_gr1075rbm',names(predictors_toadd))] <- 0
# ROW - we don't want to impute: ethnivf2
predictormatrix[c('ethninfv2','coding_assessment', 'CBCL_gr1075rbm', 'reportcito'),]<-0

# visit the secquence
VisSeq <- imp0$visitSequence

# imputations
#implist <- mice(tmp, m = 30, maxit = 60, seed = 08121996, method = meth, visitSequence = VisSeq, predictorMatrix = predictormatrix)
 
# save it
#saveRDS(implist, "imp_data_BP_20212604.rds")

# read in the implist - no need to run again
implist <- readRDS("imp_data_BP_20212604.rds")

#----------------------------------------------------------
# VARIABLE LEVEL SEPERATE MODELS
#----------------------------------------------------------

# make seperate file with confounders
confound <- tmp[,c('age', 'gender', 'income', 'ethni')]

# analysis
table2 <- coefficienttable(tmp$cm, predictors_sep, confound, analysis = 'seperate regression', MI = T, implist=implist)
tableS1a <- coefficienttable(tmp$IQ, predictors_sep, confound, analysis = 'seperate regression', MI = T, implist=implist)
tableS1b <- coefficienttable(tmp$cito, predictors_sep, confound, analysis = 'seperate regression', MI = T, implist=implist)

# combine and write out
write.xlsx(table2, 'table2_cbcl.xlsx')
tableS1 <- cbind(tableS1a, tableS1b)
write.xlsx(tableS1, 'tableS1_cbcl.xlsx')

#----------------------------------------
# MULTINOMIAL
#----------------------------------------

# analysis
figure2 <- coefficienttable(tmp$status_cm, predictors_sep, confound, analysis = "multinomial")

# combine
write.xlsx(figure2, 'figure2_cbcl.xlsx')

#-------------------------------------------
# GENDER INTERACTION
#-------------------------------------------

# analysis
figureS1 <- coefficienttable(tmp$cm, predictors_int, confound, analysis = 'simultaneous regression', MI = T, implist=implist)

# write out
write.xlsx(figureS1, 'figureS1_cbcl.xlsx')

# PLOTS
tmp$Gender <- as.factor(tmp$gender)

# make a facet grid plot - first make a long df with only the necessary info
plotdata_trf_tot <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_total_problems_tscore, rep('Total problems', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata_trf_tot) <- c('cm','Gender','problem','type', 'reporter')
plotdata_trf_int <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_internalizing_problems_tscore, rep('Internalizing problems', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata_trf_int) <- c('cm','Gender','problem','type', 'reporter')
plotdata_trf_ext <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_externalizing_problems_tscore, rep('Externalizing problems', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata_trf_ext) <- c('cm','Gender','problem','type', 'reporter')
plotdata1 <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_withdrawn__depressed_tscore, rep('Withdrawn/depressed', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata1) <- c('cm','Gender','problem','type', 'reporter')
plotdata2 <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_anxious__depressed_tscore, rep('Anxious/depressed', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata2) <- c('cm','Gender','problem','type', 'reporter')
plotdata3 <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_somatic_complaints_tscore, rep('Somatic complaints', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata3) <- c('cm','Gender','problem','type', 'reporter')
plotdata4 <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_social_problems_tscore, rep('Social problems', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata4) <- c('cm','Gender','problem','type', 'reporter')
plotdata5 <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_thought_problems_tscore, rep('Thought problems', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata5) <- c('cm','Gender','problem','type', 'reporter')
plotdata6 <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_attention_problems_tscore, rep('Attention problems', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata6) <- c('cm','Gender','problem','type', 'reporter')
plotdata7 <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_rule_breaking_behavior_tscore, rep('Rule-breaking behavior', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata7) <- c('cm','Gender','problem','type', 'reporter')
plotdata8 <- data.frame(tmp$cm, tmp$Gender, tmp$TRF_aggressive_behavior_tscore, rep('Agressive behavior', nrow(tmp)),rep('teacher', nrow(tmp)))
names(plotdata8) <- c('cm','Gender','problem','type', 'reporter')

plotdata_cbcl_tot <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_total_problems_tscore, rep('Total problems', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata_cbcl_tot) <- c('cm','Gender','problem','type', 'reporter')
plotdata_cbcl_int <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_internalizing_problems_tscore, rep('Internalizing problems', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata_cbcl_int) <- c('cm','Gender','problem','type', 'reporter')
plotdata_cbcl_ext <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_externalizing_problems_tscore, rep('Externalizing problems', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata_cbcl_ext) <- c('cm','Gender','problem','type', 'reporter')
plotdata9 <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_anxious__depressed_tscore_5, rep('Anxious/depressed', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata9) <- c('cm','Gender','problem','type', 'reporter')
plotdata10 <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_withdrawn_tscore_5, rep('Withdrawn', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata10) <- c('cm','Gender','problem','type', 'reporter')
plotdata11 <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_somatic_complaints_tscore_5, rep('Somatic complaints', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata11) <- c('cm','Gender','problem','type', 'reporter')
plotdata12 <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_attention_problems_tscore_5, rep('Attention problems', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata12) <- c('cm','Gender','problem','type', 'reporter')
plotdata13 <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_sleep_problems_tscore_5, rep('Sleep problems', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata13) <- c('cm','Gender','problem','type', 'reporter')
plotdata14 <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_aggressive_behavior_tscore_5, rep('Agressive behavior', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata14) <- c('cm','Gender','problem','type', 'reporter')
plotdata15 <- data.frame(tmp$cm, tmp$Gender, tmp$CBCL_emotionally_reactive_tscore_5, rep('Emotionally reactive', nrow(tmp)),rep('mother', nrow(tmp)))
names(plotdata15) <- c('cm','Gender','problem','type', 'reporter')

# combine the dataset to make it long
plotdata <- rbind(plotdata_trf_tot, plotdata_cbcl_tot, 
                  plotdata_trf_int, plotdata_cbcl_int, 
                  plotdata_trf_ext, plotdata_cbcl_ext,
                  plotdata1, plotdata2, plotdata3, plotdata4, plotdata5,
                  plotdata6, plotdata7, plotdata8, 
                  plotdata9, plotdata10,
                  plotdata11, plotdata12, plotdata13, plotdata14, plotdata15)

# get information for the facet grids
tp <- unique(plotdata[,c('reporter', 'type')])
tp$reporter <- factor(tp$reporter, levels = c('teacher', 'mother'))
tp$type <- factor(tp$type, levels = c("Total problems","Internalizing problems","Externalizing problems","Withdrawn/depressed", 'Withdrawn','Anxious/depressed', 'Somatic complaints', 'Agressive behavior','Attention problems', 
                                      'Social problems', 'Thought problems', 'Rule-breaking behavior', 'Sleep problems', 'Emotionally reactive'))
tp$cm <- 1

# the plotS
ggplot() + 
  theme(legend.justification = "top", legend.title = element_text(face="bold")) +
  ggtitle("Emotional and behavior problems: the interaction with Gender") +
  xlab("Problem behavior") + ylab("IQ-achievement disceprancy") +
  scale_color_discrete(name = "Gender", labels = c("girls", 'boys')) +
  theme(legend.justification = "top", legend.title = element_text(face="bold")) +
  facet_wrap(~ type + reporter, nrow = 6, ncol = 4)  +
  theme(strip.text.y = element_text(angle = 0)) +
  geom_rect(data = tp, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.3, aes(fill = reporter)) +
  scale_fill_manual(name = 'Reporter', values=c('lightblue', 'white')) +
  labs(fill = 'reporter') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  stat_smooth(method='lm', formula = y~x, data=plotdata, aes(y=cm, x=problem, group = Gender, color = Gender)) 

#-------------------------------------------
# CORRELATION ACROSS DIFFERENT SYNDROME SCALES
#-------------------------------------------

# correlation matrix -> send to personal pc
cortmpord <- tmp[,c('CBCL_total_problems_tscore_5',"CBCL_internalizing_problems_tscore_5","CBCL_externalizing_problems_tscore_5", 
                    
                    "CBCL_anxious__depressed_tscore_5","CBCL_withdrawn_tscore_5", 
                    "CBCL_somatic_complaints_tscore_5","CBCL_aggressive_behavior_tscore_5",
                    "CBCL_attention_problems_tscore_5","CBCL_emotionally_reactive_tscore_5",
                    "CBCL_sleep_problems_tscore_5",
                    
                    'TRF_total_problems_tscore',"TRF_internalizing_problems_tscore","TRF_externalizing_problems_tscore",  
                    
                    "TRF_anxious__depressed_tscore","TRF_withdrawn__depressed_tscore",
                    "TRF_somatic_complaints_tscore","TRF_aggressive_behavior_tscore",
                    "TRF_attention_problems_tscore", "TRF_social_problems_tscore",
                    "TRF_thought_problems_tscore","TRF_rule_breaking_behavior_tscore")]
cormatorder <- cor(cortmpord)
write.xlsx(cormatorder, 'figure1_cbcl.xlsx')

# get the p-values
figure1_cbcl_p <- round(pvalue_cor(tmp, cormatorder),4)

# change names
figure1_cbcl_p_df <- as.data.frame(figure1_cbcl_p)
colnames(figure1_cbcl_p_df) <- names(cortmpord)
row.names(figure1_cbcl_p_df) <- names(cortmpord)

write.xlsx(figure1_cbcl_p_df, 'figure1_cbcl_p.xlsx')

#-------------------------------------------
# EXTRA ANALYSIS FOR RESULTS SECTIONS 
#-------------------------------------------

# only significant syndrome scales
mod <- with(implist, (lm(cm ~ gender + age + income + ethni + scale(CBCL_attention_problems_tscore_5) + scale(CBCL_somatic_complaints_tscore_5))))
summary(pool(mod))
# get the p-corrected p-values
round(2*pt(summary(pool(mod))[,4], summary(pool(mod))[,5])*neff,3)
# get CI
round(summary(pool(mod))[6,2]-1.96*summary(pool(mod))[6,3],2); round(summary(pool(mod))[6,2]+1.96*summary(pool(mod))[6,3],2)

# only significant syndrome scales
mod1 <- with(implist, (lm(cm ~ gender + age + income + ethni + scale(TRF_attention_problems_tscore) + scale(TRF_aggressive_behavior_tscore) + scale(TRF_rule_breaking_behavior_tscore))))
summary(pool(mod1))
# get the p-corrected p-values
round(2*pt(summary(pool(mod1))[,4], summary(pool(mod1))[,5])*neff,3)
# get CI
round(summary(pool(mod1))[6,2]-1.96*summary(pool(mod1))[6,3],2); round(summary(pool(mod1))[6,2]+1.96*summary(pool(mod1))[6,3],2)

# simultaeous association total problems CBCL and TRF
mod2 <- with(implist, lm(cm ~ gender + age + income + ethni + scale(TRF_total_problems_tscore) + scale(CBCL_total_problems_tscore_5), data = tmp))
# get the p-corrected p-values
summary(pool(mod2))
round(2*pt(summary(pool(mod2))[,4], summary(pool(mod2))[,5])*neff,3)
# get CI
round(summary(pool(mod2))[7,2]-1.96*summary(pool(mod2))[7,3],2); round(summary(pool(mod2))[7,2]+1.96*summary(pool(mod2))[7,3],2) #mom
round(summary(pool(mod2))[6,2]-1.96*summary(pool(mod2))[6,3],2); round(summary(pool(mod2))[6,2]+1.96*summary(pool(mod2))[6,3],2) #teach
# shared variance 
summary(lm(scale(tmp$TRF_total_problems_tscore) ~ scale(tmp$CBCL_total_problems_tscore_5)))
# no need to use mice as these variables are complete

# simultaeous association externalizing problems CBCL and TRF
mod3 <- with(implist, lm(cm ~ gender + age + income + ethni + scale(TRF_externalizing_problems_tscore) + scale(CBCL_externalizing_problems_tscore_5), data = tmp))
# get the p-corrected p-values
summary(pool(mod3))
round(2*pt(summary(pool(mod3))[,4], summary(pool(mod3))[,5])*neff,3)
# get CI
round(summary(pool(mod3))[7,2]-1.96*summary(pool(mod3))[7,3],2); round(summary(pool(mod3))[7,2]+1.96*summary(pool(mod3))[7,3],2) #mom
round(summary(pool(mod3))[6,2]-1.96*summary(pool(mod3))[6,3],2); round(summary(pool(mod3))[6,2]+1.96*summary(pool(mod3))[6,3],2) #teach
# calculate r-squared 
summary(lm(tmp$TRF_externalizing_problems_tscore ~ tmp$CBCL_externalizing_problems_tscore_5))

# simultaeous association attention problems CBCL and TRF
mod4 <- with(implist, lm(cm ~ gender + age + income + ethni + scale(TRF_attention_problems_tscore) + scale(CBCL_attention_problems_tscore_5), data = tmp))
# get the p-corrected p-values
summary(pool(mod4))
round(2*pt(summary(pool(mod4))[,4], summary(pool(mod4))[,5])*neff,3)
# get CI
round(summary(pool(mod4))[7,2]-1.96*summary(pool(mod4))[7,3],2); round(summary(pool(mod4))[7,2]+1.96*summary(pool(mod4))[7,3],2) #mom
round(summary(pool(mod4))[6,2]-1.96*summary(pool(mod4))[6,3],2); round(summary(pool(mod4))[6,2]+1.96*summary(pool(mod4))[6,3],2) #teach
# explained variance teacher-mother reported attention problems
summary(lm(tmp$TRF_attention_problems_tscore~tmp$CBCL_attention_problems_tscore_5))

# different teachers and schools
length(unique(tmp$TRF_teacher_cluster))
length(unique(tmp$TRF_school_cluster))

# mean age difference trf and cbcl
meansd(tmp$TRF_age_trf/12-tmp$CBCL_agechild_gr1075/12)

# number teacher report and self report
table(tmp$reportcito)

# number of paper-pencil version (2) coding task
table(tmp$coding_assessment)

# number biological mother filling in cbcl
table(tmp$CBCL_gr1075rbm)
round(table(tmp$CBCL_gr1075rbm)/nrow(tmp)*100,1)

# missing value frequency
sort(apply(tmp,2,function(x){
  table(is.na(x))[2]/length(x)*100}))


