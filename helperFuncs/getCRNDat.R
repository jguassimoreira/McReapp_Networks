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

