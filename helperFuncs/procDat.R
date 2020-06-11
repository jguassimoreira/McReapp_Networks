## Function used to process the network data. Parses the data into 1 of 4 networks
## Further processes it in different ways
procDat = function(subList, parc, preproc_bs, net_thresh) {
  
  #first decide what kind of parcellation we're using
  
  if (parc == "schaefer7") {
    
    netDat = matrix(NaN,70,24) #70 subjects, 3 net metrics x 8 networks = 24 net values
    colnames(netDat) = c("simCoN", "simDMN", "simDAN", "simLiN", "simSaVAN", "simSMN", "simViN", "simCRN",
                         "modCoN", "modDMN", "modDAN", "modLiN", "modSaVAN", "modSMN", "modViN", "modCRN",
                         "comCoN", "comDMN", "comDAN", "comLiN", "comSaVAN", "comSMN", "comViN", "comCRN") 
    
    
  } else if (parc == "schaefer17") {
    
    netDat = matrix(NaN,70,54) #70 subjects, 3 net metrics x 18 neteworks = 24 net values
    colnames(netDat) = c("simCoNA", "simCoNB", "simCoNC", "simDMNA", "simDMNB", "simDMNC", "simDANA", "simDANB",
                        "simLiNA", "simLiNB", "simSaVaNA", "simSaVaNB", "simSMNA", "simSMNB", "simTPN", "simVCN",
                        "simVPN", "simCRN",
                        "modCoNA", "modCoNB", "modCoNC", "modDMNA", "modDMNB", "modDMNC", "modDANA", "modDANB",
                        "modLiNA", "modLiNB", "modSaVaNA", "modSaVaNB", "modSMNA", "modSMNB", "modTPN", "modVCN",
                        "modVPN", "modCRN",
                        "comCoNA", "comCoNB", "comCoNC", "comDMNA", "comDMNB", "comDMNC", "comDANA", "comDANB",
                        "comLiNA", "comLiNB", "comSaVaNA", "comSaVaNB", "comSMNA", "comSMNB", "comTPN", "comVCN",
                        "comVPN", "comCRN") 
    
    
  } else if (parc == "ns1") { #our original neurosynth network definition
    
    netDat = matrix(NaN,70,18) #70 subjects, 3 net metrics x 6 networks = 18 net values
    colnames(netDat) = c("simFPN", "simDMN", "simDAN", "simVAN", "simSaN", "simCRN",
                         "modFPN", "modDMN", "modDAN", "modVAN", "modSaN", "modCRN",
                         "comFPN", "comDMN", "comDAN", "comVAN", "comSaN", "comCRN")
    
    
  } else { 
    
    netDat = matrix(NaN,70,27) #70 subjects, 3 net metrics x 9 networks = 27 net values
    colnames(netDat) = c("simFPN", "simDMN", "simSMN", "simVAN", "simSaN", "simAuN", "simViN", "simLiN", "simCRN",
                         "modFPN", "modDMN", "modSMN", "modVAN", "modSaN", "modAuN", "modViN", "modLiN", "modCRN",
                         "comFPN", "comDMN", "comSMN", "comVAN", "comSaN", "comAuN", "comViN", "comLiN", "comCRN")
    
    }
  
  #loop over subjects
  for (s in 1:length(subList)) {
    
    #read in data
    datFar = readNetDat(subList[s], parc)[[1]]
    datLook = readNetDat(subList[s], parc)[[2]]
    
    #get roi labels and clean them up
    hdr = names(datFar)
    hdrNew = reName(hdr, parc = parc)
    
    #pull out dataframes for each network
    groupedByNetFar = groupNets(hdrNew, datFar)
    groupedByNetLook = groupNets(hdrNew, datLook)
    
    if (parc == "schaefer7") {
      
      groupedByNetFar[8] = getCRNDat(subs[s])[1]
      groupedByNetLook[8] = getCRNDat(subs[s])[2]
      
    } else if (parc == "schaefer17") {
      
      groupedByNetFar[18] = getCRNDat(subs[s])[1]
      groupedByNetLook[18] = getCRNDat(subs[s])[2]
      
    }
    
    if (preproc_bs == 'detrend') {
      
      
      netDat[s,] = t(getNetMetrics(groupedByNetFar, groupedByNetLook, 1, net_thresh))
      
    } else {
      
      netDat[s,] = t(getNetMetrics(groupedByNetFar, groupedByNetLook, 0, net_thresh))
      
    }
    
    
    
  }
  
  return(netDat)
  
}