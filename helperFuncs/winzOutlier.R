winzOutlier = function(dat) {
  
  for (c in 1:dim(dat)[2]) {
    
    #take IQR
    q3 = as.numeric(summary(dat[,c])['3rd Qu.']); q1 = as.numeric(summary(dat[,c])['1st Qu.'])
    iqr = q3 - q1
    outLowMask = !(sort(dat[,c]) < (q1 - (1.5*iqr)))
    newMin = min(sort(dat[,c])[outLowMask])
    outHiMask = !(sort(dat[,c]) > (q3 + (1.5*iqr)))
    newMax = max(sort(dat[,c])[outHiMask])
    dat[,c][dat[,c] < (q1 - (1.5*iqr))] = newMin
    dat[,c][dat[,c] > (q3 + (1.5*iqr))] = newMax
    
  }
  
  return(dat)
  
}
