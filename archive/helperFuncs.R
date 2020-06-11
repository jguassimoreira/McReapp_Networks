##Re-Naming function to clean-up/reformat Schaefer labels
reName = function(nameVec) {
  
  namesNew = c() #output vector for new names
  
  #loop over each name and fix
  for (n in nameVec){
    
    #split up the labels
    subLabels = strsplit(n, "_")[[1]][c(3:5,2)]
    #reformat them by ditching the '7Networks' tag and putting hemisphere tag at the end
    namesNew=c(namesNew, paste(subLabels, collapse = '_'))
    
  }
  
  return(namesNew)
  
}

##Grouping function to group beta-series into network specific object
groupNets = function(nameVec, dat) {
  
  #split up the network names
  nameVecSplit = strsplit(nameVec, "_")
  #get a vector containing all the network tags (i.e., the label used by Schaefer to denote each network in the ROI name)
  netNames = unique(unlist(lapply(nameVecSplit, function(x) x[1])))
  #create output list
  outList = list()
  #counter
  n = 1
  
  #loop over networks
  for (net in netNames) {
    
    #get the first part of each column's (ROI) label (i.e,. get what network the ROI belongs to)
    #compare to the current network, using it to subset beta-series belonging only to the current network
    #append to the output
    outList[[n]] = dat[,unlist(lapply(nameVecSplit, function(x) x[1])) == net]
    n = n + 1
    
  }
  
  #returns a list where each element is a tibble containing the data from only a given network
  return(outList)
  
}


getNetMetrics = function(farNets, lookNets) {
  
  #create output matrix
  #Columns correspond to metrics (sim, mod, com)
  #Rows correspond to one of the eight networks
  outMat = matrix(NA, length(farNets), 3)
  
  for (n in 1:length(farNets)) {
    
    #Get the appropriate network from a list whose objects are individual tibbles containing network-specific data
    far = as.data.frame(farNets[[n]]); look = as.data.frame(lookNets[[n]])
    
    #Create similarity matrices for each task state (Far, Look)
    farSim = corr.test(far, method = "spearman")[[1]]; lookSim = corr.test(look, method = "spearman")[[1]] 
    
    #Lop off redundant parts of said matrices, and vectorize them
    farSimVec = farSim[lower.tri(farSim)]; lookSimVec = lookSim[lower.tri(lookSim)]
    
    #Variance stabilization via Fisher Z transform
    farSimVecZ = fisherz(farSimVec); lookSimVecZ = fisherz(lookSimVec)
    
    #correlate similarity vectors, then apply fisher transform
    outMat[n,1] = fisherz(cor(farSimVecZ, lookSimVecZ, method="spearman"))
    
    ##Now for modularity & community; start with Far and then do Look. Steps for each task state follow.
    
    #Fisher transform the similarity matrix from above (but not vectorize this time)
    #Turn into adjacency matrix; criterion for an edge is a Z value > 0.5
    #Use the adjacency matrix to create  agraph
    #Calculate modularity (using walk trap algorithm)
    #Calculate number of communities (using walk trap algorithm)
    
    farSimZ = fisherz(farSim)
    farAdjm = farSimZ; farAdjm[ farSimZ == Inf] = 0; farAdjm[ farSimZ < .5000 ] = 0
    farGraph = graph_from_adjacency_matrix(farAdjm, mode = "undirected", weighted=TRUE)
    farMod = modularity(cluster_walktrap(farGraph))
    farComm = length(cluster_walktrap(farGraph))
    
    lookSimZ = fisherz(lookSim)
    lookAdjm = lookSimZ; lookAdjm[ lookSimZ == Inf] = 0; lookAdjm[ lookSimZ < .5000 ] = 0
    lookGraph = graph_from_adjacency_matrix(lookAdjm, mode = "undirected", weighted=TRUE)
    lookMod = modularity(cluster_walktrap(lookGraph))
    lookComm = length(cluster_walktrap(lookGraph))
    
    #take differences for modularity and community
    outMat[n,2] = farMod - lookMod; outMat[n,3] = farComm - lookComm
    
  }
  
  #return the output matrix as a column vector
  #first eight rows are the sim metrics, one for each network
  #next eight are the mod metrics, one for each network
  #final eight are the com metrics, one for each network
  return(Reshape(outMat, 24,1))
}


#Custom function used to winsorize outliers in our network data, expects a matrix
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


#Custom function used to gather CRN beta series data for each subject
getCRNDat = function(subj) { 
  
  roiList_CRN = c("ldlPFC_A_ERN", "ldlPFC_B_ERN", "ldlPFC_C_ERN", "ldlPFC_D_ERN", "ldlPFC_E_ERN", 
                  "ldlPFC_F_ERN", "ldlPFC_G_ERN", "rdlPFC_A_ERN", "rdlPFC_B_ERN", "rdlPFC_C_ERN",
                  "rdlPFC_D_ERN", "rvlPFC_A_ERN", "rvlPFC_B_ERN", "rvlPFC_C_ERN", "rdmPFC_A_ERN", 
                  "rdmPFC_B_ERN", "rdmPFC_C_ERN", "rdmPFC_D_ERN", "rdmPFC_E_ERN", "rdmPFC_F_ERN", 
                  "rdmPFC_G_ERN", "lMTG_A_ERN", "lMTG_B_ERN", "lSPL_A_ERN", "lSPL_B_ERN", 
                  "lSPL_C_ERN", "lSPL_D_ERN", "lSPL_E_ERN", "rSPL_A_ERN", "rSPL_B_ERN", "rSPL_C_ERN", 
                  "rSPL_D_ERN")
  
  farCRN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/%s_expanded_farNeg_lss_ts.txt", subj))
  names(farCRN) <- roiList_CRN
  
  lookCRN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/%s_expanded_lookNeg_lss_ts.txt", subj))
  names(lookCRN) <- roiList_CRN
  
  return(list(farCRN, lookCRN))
  
}


#Custom KNN function using the class package
runKNN = function(dat, labs, kvec) {
  
  outList = list()
  for (k in 1:length(kvec)) {
    
    mod = knn.cv(dat, labs, k = kvec[k], prob=TRUE)
    outList[[k]] = CrossTable(x = labs, y = mod, prop.chisq=FALSE)
    
  }
  
  return(outList)
  
}

tableF1 = function(modSums) {
  
  outMat = matrix(NaN, length(modSums), 7)
  
  for (m in 1:length(modSums)) {
    
    mod = modSums[m][[1]]$t
    
    for (c in 1:2) {
      
      #set replacement vectors for each category
      
      if (c == 1) {
        
        #category 1 (child, high ability)
        repVec = c(1:3)
        
      } else {
        
        #category 2 (child, high ability)
        repVec = c(4:6)
        
      }
      
      #precision 
      p = mod[c,c]/sum(mod[c,c], mod[3-c,c])
      #recall
      r = mod[c,c]/sum(mod[c,c], mod[c,3-c])
      #f1 
      f = 2 * ((p*r)/(p+r))
      
      outMat[m,repVec] = c(p,r,f)
      
    }
    
    outMat[m,7] = 2 * ((outMat[m,3]*outMat[m,6])/(outMat[m,3]+outMat[m,6]))  
    
  }
  
  return(outMat)
  
  }

