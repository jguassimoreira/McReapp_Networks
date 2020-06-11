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