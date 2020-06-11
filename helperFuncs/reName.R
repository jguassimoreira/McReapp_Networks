##Re-Naming function to clean-up/reformat network labels
reName = function(nameVec, parc) {
  
  namesNew = c() #output vector for new names
  
  prc = ifelse(parc == "schaefer7", 1,
        ifelse(parc == "schaefer17", 1, 0))
  
  if (prc == 1) {
    
    #loop over each name and fix
    for (n in nameVec){
      
      #split up the labels
      subLabels = strsplit(n, "_")[[1]][c(3:5,2)]
      #reformat them by ditching the '7Networks' tag and putting hemisphere tag at the end
      namesNew=c(namesNew, paste(subLabels, collapse = '_'))
      
    }
    
    return(namesNew)
    
  } else {
    
    return(nameVec)
    
  }
  
  
}
