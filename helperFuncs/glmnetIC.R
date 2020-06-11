glmnetIC = function(glmMod) {
  
  tLL = glmMod$nulldev - deviance(glmMod) #get loglikelihood
  k = glmMod$df #get degrees of freedom
  n = glmMod$nobs #get number of independent observations
  AICc = -tLL+2*k+2*k*(k+1)/(n-k-1) #calculate AICc

  BIC = log(n)*k - tLL #calculate BIC
  
  return(list(AICc, BIC))
  
}