## Function get thats the three network activity metrics of interest
## network similarity (via RSA), network community structure, and network modularity

getNetMetrics = function(farNets, lookNets, detrend, net_thresh) {
  
  #create output matrix
  #Columns correspond to metrics (sim, mod, com)
  #Rows correspond to one of the eight networks
  outMat = matrix(NA, length(farNets), 3)
  
  for (n in 1:length(farNets)) {
    
    #Get the appropriate network from a list whose objects are individual tibbles containing network-specific data
    far = as.data.frame(farNets[[n]]); look = as.data.frame(lookNets[[n]])
    
    #detrend the networks if necessary
    if (detrend == 1) {
      
      far = bs_preproc(far); look = bs_preproc(look)
      
    }
    
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
    farAdjm = farSimZ; farAdjm[ farSimZ == Inf] = 0; farAdjm[ farSimZ < net_thresh ] = 0
    farGraph = graph_from_adjacency_matrix(farAdjm, mode = "undirected", weighted=TRUE)
    farMod = modularity(cluster_walktrap(farGraph))
    farComm = length(cluster_walktrap(farGraph))
    
    lookSimZ = fisherz(lookSim)
    lookAdjm = lookSimZ; lookAdjm[ lookSimZ == Inf] = 0; lookAdjm[ lookSimZ < net_thresh ] = 0
    lookGraph = graph_from_adjacency_matrix(lookAdjm, mode = "undirected", weighted=TRUE)
    lookMod = modularity(cluster_walktrap(lookGraph))
    lookComm = length(cluster_walktrap(lookGraph))
    
    #take differences for modularity and community
    outMat[n,2] = farMod - lookMod; outMat[n,3] = farComm - lookComm
    
  }
  
  #return the output matrix as a column vector
  #first n (n = number of networks) rows are the sim metrics, one for each network
  #next n are the mod metrics, one for each network
  #final n are the com metrics, one for each network
  return(Reshape(outMat, 3*length(farNets),1))
}