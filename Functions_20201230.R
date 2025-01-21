# manually adjust confounders (+confounders[,4]) to number of confounders you have

# functions

# function for quick mean and sd
meansd <- function(var){
  x <- c(round(mean(var, na.rm=T),2), 
         round(sd(var, na.rm=T),2))
  return(x)
}

# same but double
meansd_d <- function(var1, var2){
  x <- c(meansd(var1), meansd(var2))
  return(x)
}

# get p-values in correlation
pvalue_cor <- function(dataset, cortable){
  varsx <- varsy <- rownames(cortable)
  pmatrix <- matrix(NA, length(varsx), length(varsx))
  for (i in 1:length(varsx)){
    for (j in 1:length(varsy)){
      pmatrix[i,j] <- cor.test(dataset[,varsx[i]], dataset[,varsy[j]])$p.value
    }
  }
  return(pmatrix)
}

# get summary of data
data_summary <- function(x){
  m <- mean(x, na.rm=T)
  se <- sd(x, na.rm=T)/sqrt(length(x))
  ymin <- m-1.96*se
  ymax <- m+1.96*se
  return(c(y=m, ymin=ymin, ymax=ymax))
}

# get ci_interval
ci_interval <- function(table){
  
  for (i in 1:nrow(table)){
    table[i,2] <- (paste(table[i,2], table[i,3], sep = ', '))
  }
  
  table <- table[,c(1:2,4)]
  return(table)
  
}

# get coefficienttable fast
coefficienttable <- function(outcome, preds, confounders, analysis = 'seperate regression', interactor, MI = FALSE, implist, neff){
  
  if (analysis == 'seperate regression'){
    
    # make an empty table 
    table_1 <- data.frame(matrix(NA, length(preds), 4))
    
   
    if (MI){
      
      # regessions
      for (i in 1:ncol(preds)){
        
        # run regression
        mod <- with(implist, lm(outcome~ scale(preds[,i]) + age + gender + income + ethni))
        
        # save in table
        table_1[i,] <- data.frame(round(summary(pool(mod))[2,2],2), 
                                  round(summary(pool(mod))[2,2]-1.96*summary(pool(mod))[2,3],2),
                                  round(summary(pool(mod))[2,2]+1.96*summary(pool(mod))[2,3],2),
                                  summary(pool(mod))[2,6])
      }
    } 
    
    else{
      
      # regessions
      for (i in 1:ncol(preds)){
        
        # make predictor sections
        totalpred <- cbind(scale(preds[,i]), confounders)
        
        # run regression
        x <- lm(outcome ~ ., data = totalpred) %>% summary()
        
        # save in table
        table_1[i,] <- data.frame(round(x$coefficients[2],2),
                                  round(x$coefficients[2]-1.96*x$coefficients[2,2],2),
                                  round(x$coefficients[2]+1.96*x$coefficients[2,2],2),
                                  x$coefficients[2,4])
      }
    }
    
    # columns and row names
    names(table_1) <- c('beta', 'CI', 'CI ul', 'p')
    row.names(table_1) <- names(preds)
  
    # add confidence interval
    table_2 <- ci_interval(table_1)
  
  }
  
  if (analysis == 'simultaneous regression'){
    
    # make an empty table 
    table_1 <- data.frame(matrix(NA, length(preds), 4))
    
    if (MI){
      
      for (i in 1:ncol(preds)){
        
        if (i%%2) {j <- i+1
      
        # run regression
        mod <- with(implist, lm(outcome~ scale(preds[,i]) +scale(preds[,j]) + age + gender + income + ethni))
      
        # save in table
        table_1[i:j,] <- data.frame(round(summary(pool(mod))[2:3,2],2), 
                                  round(summary(pool(mod))[2:3,2]-1.96*summary(pool(mod))[2:3,3],2),
                                  round(summary(pool(mod))[2:3,2]+1.96*summary(pool(mod))[2:3,3],2),
                                  summary(pool(mod))[2:3,6])
        

        }
      }  
    }
    
    else { 
      
      # regessions
      for (i in 1:ncol(preds)){
        
        if (i%%2) {j <- i+1
        
        # make predictor sections
        totalpred <- cbind(scale(preds[,i:j]), confounders)
      
        # run regression
        x <- lm(outcome ~ ., data = totalpred) %>% summary()
      
        # save in table
        table_1[i:j,] <- data.frame(round(x$coefficients[2:3],2),
                                    round(x$coefficients[2:3]-1.96*x$coefficients[2:3,2],2),
                                    round(x$coefficients[2:3]+1.96*x$coefficients[2:3,2],2),
                                    x$coefficients[2:3,4])
        }
      }
    }  
    
    # add row and column names
    names(table_1) <- c('beta', 'CI', 'CI ul', 'p')
    row.names(table_1) <- names(preds)
    
    # add confidence interval
    table_2 <- ci_interval(table_1)
  }
  
  if (analysis == 'interaction'){
    
    # make an empty table 
    table_1 <- data.frame(matrix(NA, 3*length(preds), 4))
    
    # regessions
    
    for (i in 1:ncol(preds)){
      
      interactor <- as.numeric(interactor)
        
      # make predictor sections
      totalpred <- cbind(scale(preds[,i]), interactor, preds[,i]*interactor, confounders)
      
      # run regression
      x <- lm(outcome ~ ., data = totalpred) %>% summary()
       
      # save in table
      j <- i*3
      table_1[(j-2):j,] <- data.frame(round(x$coefficients[2:4],2),
                                      round(x$coefficients[2:4]-1.96*x$coefficients[2:4,2],2),
                                      round(x$coefficients[2:4]+1.96*x$coefficients[2:4,2],2),
                                      x$coefficients[2:4,4])
      }
    
    # make table ready
    names(table_1) <- c('beta', 'CI', 'CI ul', 'p')
    
    # row names
    names_multi <- c(rep(NA, 3*length(preds)))
    for (i in 1:ncol(preds)){
      names_multi[((i*3)-2)] <- paste0(names(preds[i]))
      names_multi[((i*3)-1)] <- paste0('sex (model: ',names(preds[i]), ')')
      names_multi[((i*3))] <- paste0('interaction (model: ',names(preds[i]), ')')
    }
    
    row.names(table_1) <- names_multi
    
    table_2 <- ci_interval(table_1)
    
    
  }
  
  if (analysis == "non-linear f-test"){
    
    # make an empty table 
    table <- data.frame(matrix(NA, length(preds), 3))
    
    # regessions
    for (i in 1:ncol(preds)){
      
      # make predictor sections
      totalpred <- cbind(scale(preds[,i]), confounders)
      totalpred2 <- cbind(scale(preds[,i]), scale(preds[,i])^2, confounders)
      
      # run regression
      x <- lm(outcome ~ ., data = totalpred) 
      x2 <- lm(outcome ~ ., data = totalpred2) 
      
      # compare fit with anova (F test)
      anv <- anova(x, x2)
      
      # make table
      table[i,] <- data.frame(round(anv[2,5], 2),
                              round(anv[2,3], 2),
                              round(anv[2,6], 3))
    }
    
    # names table
    names(table) <- c('F', 'Df', 'p')
    row.names(table) <- names(preds)
    
    table_2 <- table
    
  }
  
  if (analysis == "multinomial"){
    
    library(nnet)
    
    # make an empty table 
    table_1 <- data.frame(matrix(NA, 2*length(preds), 5))
    
    # regessions
    for (i in 1:ncol(preds)){
      
      # make predictor sections
      totalpred <- cbind(scale(preds[,i]), confounders)
      
      # run regression
      x <- multinom(outcome ~ ., data = totalpred) %>% summary()
      
      # get p-values
      z <- x$coefficients / x$standard.errors
      pmulpost <- (1-pnorm(abs(z), 0,1)) * 2
      
      # save in table
      table_1[((i*2)-1):(i*2),] <- data.frame(round(t(t(x$coefficients[,2])), 2),
                                              round(t(t(exp(x$coefficients[,2]))), 2),
                                              round(t(t(exp(x$coefficients[,2]-1.96*t(t(x$standard.errors[,2]))))), 2),
                                              round(t(t(exp(x$coefficients[,2]+1.96*t(t(x$standard.errors[,2]))))), 2),
                                              t(t(pmulpost[,2])))
    }
    
    
      
    # make table ready
    colnames(table_1) <- c('beta', 'odd ratio','LL', 'UL', 'p')
    
    # row names
    names_multi <- c(rep(NA, 2*length(preds)))
    for (i in 1:ncol(preds)){
      names_multi[((i*2)-1)] <- paste0('low compared to norm ', names(preds[i]))
      names_multi[((i*2))] <- paste0('high compared to norm ', names(preds[i]))
    }
    
    row.names(table_1) <- names_multi
    
    table_2 <- table_1
    
  }
  
  # add corrected p
  table_2$p_corrected <- round(table_2$p * neff, 10)
  table_2$p_corrected <- ifelse(table_2$p_corrected > 1, 1, table_2$p_corrected)
  
  # round p > .001
  table_2[table_2$p>.001,"p"] <- round(table_2[table_2$p>.001,"p"],3)
  table_2[table_2$p_corrected>.001,"p_corrected"] <- round(table_2[table_2$p_corrected>.001,"p_corrected"],3)
  
  # disable scientific notation for p > .001
  table_2[table_2$p<.001,] <- format(table_2[table_2$p<.001,], scientific = F)
  table_2[table_2$p<.001,] <- format(table_2[table_2$p<.001,], scientific = F)
  return(table_2)
  
}


