#detrend beta-series via OLS regression
bs_preproc = function(dat) {
  
  outMat = matrix(NaN, dim(dat)[1], dim(dat)[2], dimnames = list(c(), colnames(dat))) #create output matrix
  
  for (c in 1:length(dat)) { #loop over columns
    
    outMat[,c] = lm(dat[,c] ~ c(1:length(dat[,c])))$residuals #regress beta series values on time, take residuals as detrended vals
    
    
  }
  
  return(as.data.frame(outMat))
  
}