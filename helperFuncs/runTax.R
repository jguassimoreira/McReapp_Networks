runTax = function(dat) {
  
  netNamesFull = str_sub(colnames(dat), start = 4) #get names of each network data variable (network name + metric name)
  netNames = unique(netNamesFull) #get the names of each network
  
  taxDat = matrix(NaN, dim(dat)[1], dim(dat)[2]/3) #create matrix that we'll use in the taxonomy analysis
  colnames(taxDat) = netNames #assign names to the dataset
  
  #loop over each network, extract first principle component across the three metrics
  for (name in netNames) {
    
    #Get the current network's data
    subDat = dat[,unlist(lapply(netNamesFull, function(x) x[1])) == name]
    #Pull out the PC scores for the current network and put in taxDat
    taxDat[,name] = netPCA(subDat)
    
  }
  
  nd = as.dist((1-cor(scale(taxDat)))/2) #create the distance matrix
  ndclust = hclust(nd) #submit the distance matrix to the agglomerative clustering algorithm
  
  ind = c(1:70) #create list of indices for bootstrapping
  ndclust_boot = c(rep(NaN, 5000)) #create vector that will hold bootstrapped cophenetic correlation coefs
  set.seed(300) #set a seed for reproducibility
  
  for (b in 1:5000) {
    
    samp = sample(ind, size = 70, replace=TRUE) #draw bootstrapped sample of participants
    dthisi = taxDat[samp,] #use sample of indices to select relevant data
    clustthisi = hclust(as.dist((1-cor(scale(dthisi)))/2)) #cluster the bootstrapped data
    ndclust_boot[b] = cor_cophenetic(as.dendrogram(ndclust), as.dendrogram(clustthisi)) #take cophenetic correlation between bootstrapped dend and observed dend
    
  }
  
  #put everything in an output list
  outList = list(mean(ndclust_boot),
                 as.vector(quantile(ndclust_boot,c(.025, .975))),
                 (sum(ndclust_boot > 0)/5000),
                 ndclust,
                 nd)
  
  return(outList) #return the output
  
}
