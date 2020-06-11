## Function to read in network data, according to which parcellation/network definition was specified

readNetDat = function(sub, parc) {
  
  #pretty simple to read in data for the schaefer parcellation
  if (parc == "schaefer7") {
    
    far = read_csv(file.path(sprintf("%s", datPath), 'schaefer', 'bs_dat', sprintf("%s_far.csv", sub)))
    look = read_csv(file.path(sprintf("%s", datPath), 'schaefer', 'bs_dat', sprintf("%s_look.csv", sub)))
    out = list(far, look)
    
  } else if (parc == "schaefer17") {
    
    far = read_csv(file.path(sprintf("%s", datPath), 'schaefer', 'bs_dat17', sprintf("%s_far.csv", sub)))
    look = read_csv(file.path(sprintf("%s", datPath), 'schaefer', 'bs_dat17', sprintf("%s_look.csv", sub)))
    out = list(far, look)
    
  #more of a pain in the ass for the original neurosynth definition (because I had baaaad foresight when setting up the data infrastructure)  
  } else if (parc == "ns1") {
    
    namesFPN = c('FPN_ACC_A_sphere', 'FPN_ACC_B_sphere', 'FPN_ACC_C_sphere',
                 'FPN_lAI_A_sphere',
                 'FPN_rAI_A_sphere', 
                 'FPN_ldlPFC_A_sphere', 'FPN_ldlPFC_B_sphere',
                 'FPN_rdlPFC_A_sphere', 'FPN_rdlPFC_B_sphere', 'FPN_rdlPFC_C_sphere',
                 'FPN_lSPC_A_sphere', 'FPN_lSPC_B_sphere', 'FPN_lSPC_C_sphere', 
                 'FPN_rSPC_A_sphere', 'FPN_rSPC_B_sphere', 'FPN_rSPC_C_sphere', 'FPN_rSPC_D_sphere', 'FPN_rSPC_E_sphere', 'FPN_rSPC_F_sphere', 'FPN_rSPC_G_sphere', 'FPN_rSPC_H_sphere',
                 'FPN_Tha_A_sphere', 
                 'FPN_Tha_B_sphere',
                 'FPN_lPMC_A_sphere',
                 'FPN_rPMC_A_sphere')
    
    namesDMN = c('DMN_ACC_A_sphere', 'DMN_ACC_B_sphere', 'DMN_ACC_C_sphere', 'DMN_ACC_D_sphere', 'DMN_ACC_E_sphere',
                 'DMN_lAI_A_sphere',
                 'DMN_rAI_A_sphere', 
                 'DMN_ldlPFC_A_sphere', 'DMN_ldlPFC_B_sphere',
                 'DMN_rdlPFC_A_sphere',
                 'DMN_lHip_A_sphere', 'DMN_lHip_B_sphere', 'DMN_lHip_C_sphere', 
                 'DMN_rHip_A_sphere',
                 'DMN_lTPJ_A_sphere', 'DMN_lTPJ_B_sphere',
                 'DMN_rTPJ_A_sphere', 'DMN_rTPJ_B_sphere',
                 'DMN_mPFC_A_sphere', 'DMN_mPFC_B_sphere', 'DMN_mPFC_C_sphere', 
                 'DMN_PCC_A_sphere', 'DMN_PCC_B_sphere', 'DMN_PCC_C_sphere',
                 'DMN_Tha_A_sphere', 
                 'DMN_lTS_A_sphere')  
    
    namesDAN = c('DAN_lLOC_A_sphere', 'DAN_lLOC_B_sphere', 'DAN_lLOC_C_sphere', 'DAN_lLOC_D_sphere', 'DAN_lLOC_E_sphere', 
                 'DAN_rLOC_A_sphere', 'DAN_rLOC_B_sphere',
                 'DAN_lSPC_A_sphere', 'DAN_lSPC_B_sphere', 'DAN_lSPC_C_sphere', 'DAN_lSPC_D_sphere', 'DAN_lSPC_E_sphere',
                 'DAN_rSPC_A_sphere', 'DAN_rSPC_B_sphere',
                 'DAN_lPCG_A_sphere',
                 'DAN_rPCG_A_sphere', 'DAN_rPCG_B_sphere', 'DAN_rPCG_C_sphere', 'DAN_rPCG_D_sphere', 
                 'DAN_ACC_A_sphere', 
                 'DAN_PCC_A_sphere',
                 'DAN_rAI_A_sphere', 
                 'DAN_lMFG_A_sphere', 'DAN_lMFG_B_sphere',
                 'DAN_mPFC_A_sphere', 'DAN_mPFC_B_sphere', 'DAN_mPFC_C_sphere')
    
    namesVAN = c('VAN_lLOC_A_sphere', 'VAN_lLOC_B_sphere', 'VAN_lLOC_C_sphere', 'VAN_lLOC_D_sphere', 
                 'VAN_rLOC_A_sphere', 'VAN_rLOC_B_sphere', 'VAN_rLOC_C_sphere',
                 'VAN_lFFG_A_sphere', 'VAN_lFFG_B_sphere', 'VAN_lFFG_C_sphere',
                 'VAN_rFFG_A_sphere', 'VAN_rFFG_B_sphere', 'VAN_rFFG_C_sphere', 'VAN_rFFG_D_sphere', 'VAN_rFFG_E_sphere',
                 'VAN_ACC_A_sphere',
                 'VAN_lAI_A_sphere',
                 'VAN_rAI_A_sphere', 
                 'VAN_lOP_A_sphere',
                 'VAN_rOP_A_sphere',
                 'VAN_lIFG_A_sphere')
    
    namesSaN = c('SaN_ACC_A_sphere', 'SaN_ACC_B_sphere',
                 'SaN_PCC_A_sphere', 'SaN_PCC_B_sphere', 'SaN_PCC_C_sphere',
                 'SaN_lAI_A_sphere', 'SaN_lAI_B_sphere', 'SaN_lAI_C_sphere', 
                 'SaN_rAI_A_sphere', 'SaN_rAI_B_sphere', 'SaN_rAI_C_sphere', 
                 'SaN_lTPJ_A_sphere', 'SaN_lTPJ_B_sphere', 'SaN_lTPJ_C_sphere')
      
    namesCRN = c("CRN_ldlPFC_A", "CRN_ldlPFC_B", "CRN_ldlPFC_C", "CRN_ldlPFC_D", "CRN_ldlPFC_E", 
                 "CRN_ldlPFC_F", "CRN_ldlPFC_G", "CRN_rdlPFC_A", "CRN_rdlPFC_B", "CRN_rdlPFC_C",
                 "CRN_rdlPFC_D", "CRN_rvlPFC_A", "CRN_rvlPFC_B", "CRN_rvlPFC_C", "CRN_rdmPFC_A", 
                 "CRN_rdmPFC_B", "CRN_rdmPFC_C", "CRN_rdmPFC_D", "CRN_rdmPFC_E", "CRN_rdmPFC_F", 
                 "CRN_rdmPFC_G", "CRN_lMTG_A", "CRN_lMTG_B", "CRN_lSPL_A", "CRN_lSPL_B", 
                 "CRN_lSPL_C", "CRN_lSPL_D", "CRN_lSPL_E", "CRN_rSPL_A", "CRN_rSPL_B", "CRN_rSPL_C", 
                 "CRN_rSPL_D")
    
    namesList = list(namesFPN, namesDMN, namesDAN, namesVAN, namesSaN, namesCRN)
    netList = c('FPN', 'DMN', 'DAN', 'VAN', 'SaN', 'CRN')
    
    far = data.frame(x = c(1:20)); look = data.frame(x = c(1:20))
    
    for (n in 1:5) {
      
      far = cbind(far, read.table(file.path(sprintf("%s", datPath), "lss_trials", sprintf("%s", netList[n]), sprintf("%s_expanded_farNeg_lss_%s_ts.txt", sub, netList[n])),
                                  col.names=namesList[[n]]))
      look = cbind(look, read.table(file.path(sprintf("%s", datPath), "lss_trials", sprintf("%s", netList[n]), sprintf("%s_expanded_lookNeg_lss_%s_ts.txt", sub, netList[n])),
                                  col.names=namesList[[n]]))
    }
    
    far = cbind(far, read.table(file.path(sprintf("%s", datPath), "lss_trials", sprintf("%s_expanded_farNeg_lss_ts.txt", sub)),
                                col.names=namesList[[6]]))
    look = cbind(look, read.table(file.path(sprintf("%s", datPath), "lss_trials", sprintf("%s_expanded_lookNeg_lss_ts.txt", sub)),
                                col.names=namesList[[6]]))
   
    out = list(far[,-1], look[,-1]) 
    
  } else {
    
    namesFPN = c('FPN_ACC_A_sphere', 'FPN_ACC_B_sphere', 'FPN_ACC_C_sphere',
                 'FPN_lAI_A_sphere',
                 'FPN_rAI_A_sphere', 
                 'FPN_ldlPFC_A_sphere', 'FPN_ldlPFC_B_sphere',
                 'FPN_rdlPFC_A_sphere', 'FPN_rdlPFC_B_sphere', 'FPN_rdlPFC_C_sphere',
                 'FPN_lSPC_A_sphere', 'FPN_lSPC_B_sphere', 'FPN_lSPC_C_sphere', 
                 'FPN_rSPC_A_sphere', 'FPN_rSPC_B_sphere', 'FPN_rSPC_C_sphere', 'FPN_rSPC_D_sphere', 'FPN_rSPC_E_sphere', 'FPN_rSPC_F_sphere', 'FPN_rSPC_G_sphere', 'FPN_rSPC_H_sphere',
                 'FPN_Tha_A_sphere', 
                 'FPN_Tha_B_sphere',
                 'FPN_lPMC_A_sphere',
                 'FPN_rPMC_A_sphere')
    
    namesDMN = c('DMN_ACC_A_sphere', 'DMN_ACC_B_sphere', 'DMN_ACC_C_sphere', 'DMN_ACC_D_sphere', 'DMN_ACC_E_sphere',
                 'DMN_lAI_A_sphere',
                 'DMN_rAI_A_sphere', 
                 'DMN_ldlPFC_A_sphere', 'DMN_ldlPFC_B_sphere',
                 'DMN_rdlPFC_A_sphere',
                 'DMN_lHip_A_sphere', 'DMN_lHip_B_sphere', 'DMN_lHip_C_sphere', 
                 'DMN_rHip_A_sphere',
                 'DMN_lTPJ_A_sphere', 'DMN_lTPJ_B_sphere',
                 'DMN_rTPJ_A_sphere', 'DMN_rTPJ_B_sphere',
                 'DMN_mPFC_A_sphere', 'DMN_mPFC_B_sphere', 'DMN_mPFC_C_sphere', 
                 'DMN_PCC_A_sphere', 'DMN_PCC_B_sphere', 'DMN_PCC_C_sphere',
                 'DMN_Tha_A_sphere', 
                 'DMN_lTS_A_sphere')  
    
    namesSMN = c('SMN_rPCG_A_sphere','SMN_rPCG_B_sphere','SMN_rPCG_C_sphere','SMN_rPCG_D_sphere','SMN_rPCG_E_sphere', 'SMN_rPCG_F_sphere',
                 'SMN_lPCG_A_sphere','SMN_lPCG_B_sphere','SMN_lPCG_C_sphere',
                 'SMN_rCau_A_sphere','SMN_rCau_B_sphere','SMN_rCau_C_sphere','SMN_rCau_D_sphere',
                 'SMN_lCau_A_sphere',
                 'SMN_rLOC_A_sphere','SMN_rLOC_A_sphere',
                 'SMN_lLOC_B_sphere',
                 'SMN_dACC_A_sphere',
                 'SMN_rTha_A_sphere',
                 'SMN_lTha_A_sphere',
                 'SMN_lIFG_A_sphere',
                 'SMN_rPOC_A_sphere',
                 'SMN_lCOC_A_sphere',
                 'SMN_rAmy_A_sphere',
                 'SMN_lAmy_A_sphere',
                 'SMN_rMFG_A_sphere',
                 'SMN_rSFG_A_sphere')

    namesVAN = c('VAN_lLOC_A_sphere', 'VAN_lLOC_B_sphere', 'VAN_lLOC_C_sphere', 'VAN_lLOC_D_sphere', 
                 'VAN_rLOC_A_sphere', 'VAN_rLOC_B_sphere', 'VAN_rLOC_C_sphere',
                 'VAN_lFFG_A_sphere', 'VAN_lFFG_B_sphere', 'VAN_lFFG_C_sphere',
                 'VAN_rFFG_A_sphere', 'VAN_rFFG_B_sphere', 'VAN_rFFG_C_sphere', 'VAN_rFFG_D_sphere', 'VAN_rFFG_E_sphere',
                 'VAN_ACC_A_sphere',
                 'VAN_lAI_A_sphere',
                 'VAN_rAI_A_sphere', 
                 'VAN_lOP_A_sphere',
                 'VAN_rOP_A_sphere',
                 'VAN_lIFG_A_sphere')
    
    namesSaN = c('SaN_ACC_A_sphere', 'SaN_ACC_B_sphere',
                 'SaN_PCC_A_sphere', 'SaN_PCC_B_sphere', 'SaN_PCC_C_sphere',
                 'SaN_lAI_A_sphere', 'SaN_lAI_B_sphere', 'SaN_lAI_C_sphere', 
                 'SaN_rAI_A_sphere', 'SaN_rAI_B_sphere', 'SaN_rAI_C_sphere', 
                 'SaN_lTPJ_A_sphere', 'SaN_lTPJ_B_sphere', 'SaN_lTPJ_C_sphere')
    
    namesAuN = c('AuN_dmPFC_A_sphere',
                 'AuN_lAI_A_sphere', 'AuN_lAI_B_sphere',
                 'AuN_lCau_A_sphere',
                 'AuN_lLOC_A_sphere',
                 'AuN_lMFG_A_sphere',
                 'AuN_lOP_A_sphere',
                 'AuN_lOP_B_sphere',
                 'AuN_lPCG_A_sphere',
                 'AuN_lSPL_A_sphere',
                 'AuN_lSTG_A_sphere',
                 'AuN_lTha_A_sphere','AuN_lTha_B_sphere',
                 'AuN_rAmy_A_sphere',
                 'AuN_rCau_A_sphere','AuN_rCau_B_sphere',
                 'AuN_rIFG_A_sphere','AuN_rIFG_B_sphere','AuN_rIFG_C_sphere','AuN_rIFG_D_sphere',
                 'AuN_lLOC_B_sphere','AuN_rLOC_B_sphere',
                 'AuN_rMFG_A_sphere','AuN_rMFG_B_sphere',
                 'AuN_rSTG_A_sphere',
                 'AuN_rTha_A_sphere','AuN_rTha_B_sphere')
    
    namesViN = c('ViN_dACC_A_sphere', 
                 'ViN_lFFG_A_sphere','ViN_lFFG_B_sphere','ViN_lFFG_C_sphere','ViN_lFFG_D_sphere',
                 'ViN_lLOC_A_sphere','ViN_lLOC_B_sphere','ViN_lLOC_C_sphere',
                 'ViN_lMFG_A_sphere',
                 'ViN_lOP_B_sphere',
                 'ViN_lPCG_A_sphere',
                 'ViN_lSMG_A_sphere','ViN_lSMG_B_sphere',
                 'ViN_lSPL_A_sphere',
                 'ViN_rAI_A_sphere',
                 'ViN_rFFG_A_sphere','ViN_rFFG_B_sphere','ViN_rFFG_C_sphere','ViN_rFFG_D_sphere',
                 'ViN_rIFG_A_sphere',
                 'ViN_rLOC_A_sphere',
                 'ViN_rMFG_A_sphere',
                 'ViN_rMTG_A_sphere',
                 'ViN_rOP_A_sphere', 'ViN_rOP_B_sphere',
                 'ViN_rSPL_A_sphere')
    
    namesLiN = c('LiN_dACC_A_sphere', 'LiN_dACC_B_sphere', 
                 'LiN_lAI_A_sphere', 'LiN_lAI_B_sphere',
                 'LiN_lAmy_A_sphere',
                 'LiN_lCau_A_sphere',
                 'LiN_lHip_A_sphere',
                 'LiN_lIFG_A_sphere',
                 'LiN_lTha_A_sphere',
                 'LiN_mPFC_A_sphere',
                 'LiN_rAI_A_sphere',
                 'LiN_rAmy_A_sphere',
                 'LiN_rCau_A_sphere', 'LiN_rCau_B_sphere',
                 'LiN_rHip_A_sphere',
                 'LiN_rIFG_A_sphere',
                 'LiN_rTha_A_sphere',
                 'LiN_rTha_B_sphere')
    
    namesCRN = c("CRN_ldlPFC_A", "CRN_ldlPFC_B", "CRN_ldlPFC_C", "CRN_ldlPFC_D", "CRN_ldlPFC_E", 
                 "CRN_ldlPFC_F", "CRN_ldlPFC_G", "CRN_rdlPFC_A", "CRN_rdlPFC_B", "CRN_rdlPFC_C",
                 "CRN_rdlPFC_D", "CRN_rvlPFC_A", "CRN_rvlPFC_B", "CRN_rvlPFC_C", "CRN_rdmPFC_A", 
                 "CRN_rdmPFC_B", "CRN_rdmPFC_C", "CRN_rdmPFC_D", "CRN_rdmPFC_E", "CRN_rdmPFC_F", 
                 "CRN_rdmPFC_G", "CRN_lMTG_A", "CRN_lMTG_B", "CRN_lSPL_A", "CRN_lSPL_B", 
                 "CRN_lSPL_C", "CRN_lSPL_D", "CRN_lSPL_E", "CRN_rSPL_A", "CRN_rSPL_B", "CRN_rSPL_C", 
                 "CRN_rSPL_D")
    
    namesList = list(namesFPN, namesDMN, namesSMN, namesVAN, namesSaN, namesAuN, namesViN, namesLiN, namesCRN)
    netList = c('FPN', 'DMN', 'SMN', 'VAN', 'SaN', 'AuN', 'ViN', 'LiN', 'CRN')
    
    far = data.frame(x = c(1:20)); look = data.frame(x = c(1:20))
    
    for (n in 1:8) {
      
      far = cbind(far, read.table(file.path(sprintf("%s", datPath), "lss_trials", sprintf("%s", netList[n]), sprintf("%s_expanded_farNeg_lss_%s_ts.txt", sub, netList[n])),
                                  col.names=namesList[[n]]))
      look = cbind(look, read.table(file.path(sprintf("%s", datPath), "lss_trials", sprintf("%s", netList[n]), sprintf("%s_expanded_lookNeg_lss_%s_ts.txt", sub, netList[n])),
                                    col.names=namesList[[n]]))
    }
    
    far = cbind(far, read.table(file.path(sprintf("%s", datPath), "lss_trials", sprintf("%s_expanded_farNeg_lss_ts.txt", sub)),
                                col.names=namesList[[9]]))
    look = cbind(look, read.table(file.path(sprintf("%s", datPath), "lss_trials", sprintf("%s_expanded_lookNeg_lss_ts.txt", sub)),
                                  col.names=namesList[[9]]))
    
    out = list(far[,-1], look[,-1]) 
    
    
  }
  
}