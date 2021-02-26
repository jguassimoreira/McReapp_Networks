## Function used to get the capacity and age scores from the SB data
## If the data aren't already compiled in a local file, compile them

readSBDat = function(subList) {
  
  #get path to behavioral file containing capacity and age scores
  behav = Sys.glob("C:/Users/jguas/Desktop/McReapp/sbBehavDat*.csv")
  
  #if the path exists, get the data
  if (length(behav) > 0 ) {
    
    return(read_csv(behav))
    
    #if the path doesn't exist, compile the data
  } else {
    
    #first get age, since it'll be easier
    age = read_csv("P:/Social_Behavior_(SB_study)/Data/Study_logs/SB_group_list.csv")
    age = age[age$ID %in% sbSubs,c("ID", "Age")]
    
    #now we gotta get the reapp scores
    #start by getting the paths to each subject's capacity score
    reappFiles = lapply(1:length(sbSubs),
                        function(i) sprintf("P:/Social_Behavior_(SB_study)/Data/Behavioral_data/%s/wave1/Scan_session/Clean/%s_People.csv", sbSubs[i], sbSubs[i]))
    reappFiles = unlist(reappFiles)
    
    #now load in the data
    capacityScores = lapply(1:length(sbSubs),  
                            function(i) as.numeric(read.csv(reappFiles[i])[dim(read.csv(reappFiles[i]))[1],][!is.na(read.csv(reappFiles[i])[dim(read.csv(reappFiles[i]))[1],])][2]))
    capacityScores = unlist(capacityScores)
   
    age$Cap = capacityScores
    
    write.csv(age, "C:/Users/jguas/Desktop/McReapp/sbBehavDat.csv", row.names = FALSE)
     
  }
}
