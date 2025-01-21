# 2020-05-02

# Sander Lamballais
# Isabel Schuurmans

# reproducibility
set.seed(08121996)

#############
# FUNCTIONS #
#############

#------------
# GALWEY
#------------

galwey <- function(X){
  cc <- cor(X)
  eigens <- eigen(cc)
  eval <- eigens$values
  galwey <- ((sum(sqrt(eval)))^2)/sum(eval)
  return(galwey)}

#------------
# SHUFFLE 
#------------

shuffy <- function(y, p){
  Y <- matrix(0, nrow=length(y), ncol=p)
  for (i in 1:p) {
    Y[,i] <- sample(y)
  }
  return(Y)
}

#------------
# PERMUTATION
#------------

perm <- function(X, Y){
  X <- cbind(rep.int(1, nrow(X)), X)
  nn <- ncol(X)
  df <- nrow(X) - nn
  XTX <- chol2inv(chol(crossprod(X)))
  XTXX <- tcrossprod(XTX, X)
  bhat <- XTXX %*% Y
  res <- Y - X %*% bhat
  s2 <- colSums(res^2 / df)
  se <- do.call("cbind", lapply(s2, function(q) sqrt(diag(q * XTX))))
  tval <- bhat / se
  pval <- 2 * stats::pt(-abs(tval), df = df)
  return(t(pval))
}

#------------
# ANALYSIS
#------------

DF <- tmp[,c('cm', "TRF_anxious__depressed_tscore","TRF_withdrawn__depressed_tscore",                            
             "TRF_somatic_complaints_tscore","TRF_aggressive_behavior_tscore","TRF_attention_problems_tscore", 
             "TRF_social_problems_tscore","TRF_thought_problems_tscore",                        
             "TRF_rule_breaking_behavior_tscore",
             "CBCL_anxious__depressed_tscore_5",                      
             "CBCL_withdrawn_tscore_5","CBCL_somatic_complaints_tscore_5",
             "CBCL_aggressive_behavior_tscore_5","CBCL_attention_problems_tscore_5",                           
             "CBCL_emotionally_reactive_tscore_5", "CBCL_sleep_problems_tscore_5",
             'TRF_total_problems_tscore','CBCL_total_problems_tscore_5', 
             "TRF_internalizing_problems_tscore","TRF_externalizing_problems_tscore",
             "CBCL_internalizing_problems_tscore_5","CBCL_externalizing_problems_tscore_5")]

p = 10000
# as it is repeated 10 times, actually 100 000 permutations
results <- data.frame(1:p, rep(NA, p),rep(NA, p),rep(NA, p),rep(NA, p),rep(NA, p), rep(NA, p),rep(NA, p),rep(NA, p),rep(NA, p),rep(NA, p))

set.seed(08122996)
for (i in 1:10){
  # regression
  Y <- shuffy(DF[,1], p)
  P <- perm(as.matrix(DF[,-1]), Y)
  Pmin <- apply(P[, -1, drop = FALSE], 1, min)
  results[,i+1] <- Pmin
  
}

# original regression
Pmin_new <- c(results[,2],results[,3] ,results[,4] ,results[,5] ,results[,6] ,results[,7] ,results[,8] ,results[,9] ,results[,10],results[,11])
hist(Pmin_new, breaks = 100)
0.05 / quantile(Pmin_new, 0.05)
