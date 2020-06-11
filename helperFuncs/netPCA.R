netPCA = function(dat) {
  
  netPrinComp = princomp(dat, cor = FALSE, scores = TRUE) #run principle components analysis
  return(netPrinComp$scores[,1]) #return the PC scores for the first component
  
  
}