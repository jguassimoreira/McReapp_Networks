library(psych)
library(readxl)
library(igraph)

subs = c(1001, 1002, 1005, 1006, 1007, 1008, 1031, 1032, 1034, 1036,   ##input subject IDs
         1072, 1077, 1078, 1081, 1089, 1090, 1091, 1098, 1105, 1106,
         1111, 1115, 1116, 1117, 1118, 1119, 1125, 1126, 1127, 1128,
         1132, 1133, 1134, 1136, 1142, 1143, 1145, 1146, 1147, 1152,
         1153, 1154, 1157, 1166, 1167, 1173, 1176, 1185, 1195, 1207,
         1208, 1218, 1226, 1227, 1228, 1233, 1237, 1239, 1240, 1244,
         1245, 1246, 1248, 1249, 1250, 1252, 1253, 1256, 1262, 1263)

#Create & name Far/Look matrices for all networks

ERFarMat = as.data.frame(matrix(NaN, 70, 32))  
names(ERFarMat) <- c("ldlPFC_A", "ldlPFC_B", "ldlPFC_C", "ldlPFC_D", "ldlPFC_E", 
                     "ldlPFC_F", "ldlPFC_G", "rdlPFC_A", "rdlPFC_B", "rdlPFC_C",
                     "rdlPFC_D", "rvlPFC_A", "rvlPFC_B", "rvlPFC_C", "rdmPFC_A", 
                     "rdmPFC_B", "rdmPFC_C", "rdmPFC_D", "rdmPFC_E", "rdmPFC_F", 
                     "rdmPFC_G", "lMTG_A", "lMTG_B", "lSPL_A", "lSPL_B", 
                     "lSPL_C", "lSPL_D", "lSPL_E", "rSPL_A", "rSPL_B", "rSPL_C", 
                     "rSPL_D")
ERLookMat = as.data.frame(matrix(NaN, 70, 32))
names(ERLookMat) <- c("ldlPFC_A", "ldlPFC_B", "ldlPFC_C", "ldlPFC_D", "ldlPFC_E", 
                      "ldlPFC_F", "ldlPFC_G", "rdlPFC_A", "rdlPFC_B", "rdlPFC_C",
                      "rdlPFC_D", "rvlPFC_A", "rvlPFC_B", "rvlPFC_C", "rdmPFC_A", 
                      "rdmPFC_B", "rdmPFC_C", "rdmPFC_D", "rdmPFC_E", "rdmPFC_F", 
                      "rdmPFC_G", "lMTG_A", "lMTG_B", "lSPL_A", "lSPL_B", 
                      "lSPL_C", "lSPL_D", "lSPL_E", "rSPL_A", "rSPL_B", "rSPL_C", 
                      "rSPL_D")

DMNFarMat = as.data.frame(matrix(NaN, 70, 26))
names(DMNFarMat) <- c('ACC_A_sphere', 'ACC_B_sphere', 'ACC_C_sphere', 'ACC_D_sphere', 'ACC_E_sphere',
                     'lAI_A_sphere',
                     'rAI_A_sphere', 
                     'ldlPFC_A_sphere', 'ldlPFC_B_sphere',
                     'rdlPFC_A_sphere',
                     'lHip_A_sphere', 'lHip_B_sphere', 'lHip_C_sphere', 
                     'rHip_A_sphere',
                     'lTPJ_A_sphere', 'lTPJ_B_sphere',
                     'rTPJ_A_sphere', 'rTPJ_B_sphere',
                     'mPFC_A_sphere', 'mPFC_B_sphere', 'mPFC_C_sphere', 
                     'PCC_A_sphere', 'PCC_B_sphere', 'PCC_C_sphere',
                     'Tha_A_sphere', 
                     'lTS_A_sphere')
DMNLookMat = as.data.frame(matrix(NaN, 70, 26))
names(DMNLookMat) <- c('ACC_A_sphere', 'ACC_B_sphere', 'ACC_C_sphere', 'ACC_D_sphere', 'ACC_E_sphere',
                      'lAI_A_sphere',
                      'rAI_A_sphere', 
                      'ldlPFC_A_sphere', 'ldlPFC_B_sphere',
                      'rdlPFC_A_sphere',
                      'lHip_A_sphere', 'lHip_B_sphere', 'lHip_C_sphere', 
                      'rHip_A_sphere',
                      'lTPJ_A_sphere', 'lTPJ_B_sphere',
                      'rTPJ_A_sphere', 'rTPJ_B_sphere',
                      'mPFC_A_sphere', 'mPFC_B_sphere', 'mPFC_C_sphere', 
                      'PCC_A_sphere', 'PCC_B_sphere', 'PCC_C_sphere',
                      'Tha_A_sphere', 
                      'lTS_A_sphere')

FPNFarMat = as.data.frame(matrix(NaN, 70, 25))
names(FPNFarMat) <- c('ACC_A_sphere', 'ACC_B_sphere', 'ACC_C_sphere',
                     'lAI_A_sphere',
                     'rAI_A_sphere', 
                     'ldlPFC_A_sphere', 'ldlPFC_B_sphere',
                     'rdlPFC_A_sphere', 'rdlPFC_B_sphere', 'rdlPFC_C_sphere',
                     'lSPC_A_sphere', 'lSPC_B_sphere', 'lSPC_C_sphere', 
                     'rSPC_A_sphere', 'rSPC_B_sphere', 'rSPC_C_sphere', 'rSPC_D_sphere', 'rSPC_E_sphere', 'rSPC_F_sphere', 'rSPC_G_sphere', 'rSPC_H_sphere',
                     'Tha_A_sphere', 
                     'Tha_B_sphere',
                     'lPMC_A_sphere',
                     'rPMC_A_sphere')
FPNLookMat = as.data.frame(matrix(NaN, 70, 25))
names(FPNLookMat) <- c('ACC_A_sphere', 'ACC_B_sphere', 'ACC_C_sphere',
                      'lAI_A_sphere',
                      'rAI_A_sphere', 
                      'ldlPFC_A_sphere', 'ldlPFC_B_sphere',
                      'rdlPFC_A_sphere', 'rdlPFC_B_sphere', 'rdlPFC_C_sphere',
                      'lSPC_A_sphere', 'lSPC_B_sphere', 'lSPC_C_sphere', 
                      'rSPC_A_sphere', 'rSPC_B_sphere', 'rSPC_C_sphere', 'rSPC_D_sphere', 'rSPC_E_sphere', 'rSPC_F_sphere', 'rSPC_G_sphere', 'rSPC_H_sphere',
                      'Tha_A_sphere', 
                      'Tha_B_sphere',
                      'lPMC_A_sphere',
                      'rPMC_A_sphere')

SaNFarMat = as.data.frame(matrix(NaN, 70, 14))
names(SaNFarMat) <- c('ACC_A_sphere', 'ACC_B_sphere',
                     'PCC_A_sphere', 'PCC_B_sphere', 'PCC_C_sphere',
                     'lAI_A_sphere', 'lAI_B_sphere', 'lAI_C_sphere', 
                     'rAI_A_sphere', 'rAI_B_sphere', 'rAI_C_sphere', 
                     'lTPJ_A_sphere', 'lTPJ_B_sphere', 'lTPJ_C_sphere')
SaNLookMat = as.data.frame(matrix(NaN, 70, 14))
names(SaNLookMat) <- c('ACC_A_sphere', 'ACC_B_sphere',
                      'PCC_A_sphere', 'PCC_B_sphere', 'PCC_C_sphere',
                      'lAI_A_sphere', 'lAI_B_sphere', 'lAI_C_sphere', 
                      'rAI_A_sphere', 'rAI_B_sphere', 'rAI_C_sphere', 
                      'lTPJ_A_sphere', 'lTPJ_B_sphere', 'lTPJ_C_sphere')


DANFarMat = as.data.frame(matrix(NaN, 70, 27))
names(DANFarMat) <- c('lLOC_A_sphere', 'lLOC_B_sphere', 'lLOC_C_sphere', 'lLOC_D_sphere', 'lLOC_E_sphere', 
                     'rLOC_A_sphere', 'rLOC_B_sphere',
                     'lSPC_A_sphere', 'lSPC_B_sphere', 'lSPC_C_sphere', 'lSPC_D_sphere', 'lSPC_E_sphere',
                     'rSPC_A_sphere', 'rSPC_B_sphere',
                     'lPCG_A_sphere',
                     'rPCG_A_sphere', 'rPCG_B_sphere', 'rPCG_C_sphere', 'rPCG_D_sphere', 
                     'ACC_A_sphere', 
                     'PCC_A_sphere',
                     'rAI_A_sphere', 
                     'lMFG_A_sphere', 'lMFG_B_sphere',
                     'mPFC_A_sphere', 'mPFC_B_sphere', 'mPFC_C_sphere')
DANLookMat = as.data.frame(matrix(NaN, 70, 27))
names(DANLookMat) <- c('lLOC_A_sphere', 'lLOC_B_sphere', 'lLOC_C_sphere', 'lLOC_D_sphere', 'lLOC_E_sphere', 
                      'rLOC_A_sphere', 'rLOC_B_sphere',
                      'lSPC_A_sphere', 'lSPC_B_sphere', 'lSPC_C_sphere', 'lSPC_D_sphere', 'lSPC_E_sphere',
                      'rSPC_A_sphere', 'rSPC_B_sphere',
                      'lPCG_A_sphere',
                      'rPCG_A_sphere', 'rPCG_B_sphere', 'rPCG_C_sphere', 'rPCG_D_sphere', 
                      'ACC_A_sphere', 
                      'PCC_A_sphere',
                      'rAI_A_sphere', 
                      'lMFG_A_sphere', 'lMFG_B_sphere',
                      'mPFC_A_sphere', 'mPFC_B_sphere', 'mPFC_C_sphere')

VANFarMat = as.data.frame(matrix(NaN, 70, 21))
names(VANFarMat) <- c('lLOC_A_sphere', 'lLOC_B_sphere', 'lLOC_C_sphere', 'lLOC_D_sphere', 
                     'rLOC_A_sphere', 'rLOC_B_sphere', 'rLOC_C_sphere',
                     'lFFG_A_sphere', 'lFFG_B_sphere', 'lFFG_C_sphere',
                     'rFFG_A_sphere', 'rFFG_B_sphere', 'rFFG_C_sphere', 'rFFG_D_sphere', 'rFFG_E_sphere',
                     'ACC_A_sphere',
                     'lAI_A_sphere',
                     'rAI_A_sphere', 
                     'lOP_A_sphere',
                     'rOP_A_sphere',
                     'lIFG_A_sphere')
VANLookMat = as.data.frame(matrix(NaN, 70, 21))
names(VANLookMat) <- c('lLOC_A_sphere', 'lLOC_B_sphere', 'lLOC_C_sphere', 'lLOC_D_sphere', 
                      'rLOC_A_sphere', 'rLOC_B_sphere', 'rLOC_C_sphere',
                      'lFFG_A_sphere', 'lFFG_B_sphere', 'lFFG_C_sphere',
                      'rFFG_A_sphere', 'rFFG_B_sphere', 'rFFG_C_sphere', 'rFFG_D_sphere', 'rFFG_E_sphere',
                      'ACC_A_sphere',
                      'lAI_A_sphere',
                      'rAI_A_sphere', 
                      'lOP_A_sphere',
                      'rOP_A_sphere',
                      'lIFG_A_sphere')

##Create vectors to hold output
outSimER = c(rep(NaN, 70))
modDiffER = c(rep(NaN, 70))
commDiffER = c(rep(NaN, 70))

outSimDMN = c(rep(NaN, 70))
modDiffDMN = c(rep(NaN, 70))
commDiffDMN = c(rep(NaN, 70))

outSimFPN = c(rep(NaN, 70))
modDiffFPN = c(rep(NaN, 70))
commDiffFPN = c(rep(NaN, 70))

outSimSaN = c(rep(NaN, 70))
modDiffSaN = c(rep(NaN, 70))
commDiffSaN = c(rep(NaN, 70))

outSimDAN = c(rep(NaN, 70))
modDiffDAN = c(rep(NaN, 70))
commDiffDAN = c(rep(NaN, 70))

outSimVAN = c(rep(NaN, 70))
modDiffVAN = c(rep(NaN, 70))
commDiffVAN = c(rep(NaN, 70))


##While looping over subjects, go through each network and calculate differences in connectome similarity, modularity, and community number. 



for (i in 1:length(subs)) {
  #Emotion Regulation Network (defined via using peaks reported in Buhle, Silvers 2014 meta-analysis)
  farER <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/%s_expanded_farNeg_lss_ts.txt", subs[i]))
  names(farER) <- c("ldlPFC_A", "ldlPFC_B", "ldlPFC_C", "ldlPFC_D", "ldlPFC_E", 
                  "ldlPFC_F", "ldlPFC_G", "rdlPFC_A", "rdlPFC_B", "rdlPFC_C",
                  "rdlPFC_D", "rvlPFC_A", "rvlPFC_B", "rvlPFC_C", "rdmPFC_A", 
                  "rdmPFC_B", "rdmPFC_C", "rdmPFC_D", "rdmPFC_E", "rdmPFC_F", 
                  "rdmPFC_G", "lMTG_A", "lMTG_B", "lSPL_A", "lSPL_B", 
                  "lSPL_C", "lSPL_D", "lSPL_E", "rSPL_A", "rSPL_B", "rSPL_C", 
                  "rSPL_D")
  
  lookER <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/%s_expanded_lookNeg_lss_ts.txt", subs[i]))
  names(lookER) <- c("ldlPFC_A", "ldlPFC_B", "ldlPFC_C", "ldlPFC_D", "ldlPFC_E", 
                   "ldlPFC_F", "ldlPFC_G", "rdlPFC_A", "rdlPFC_B", "rdlPFC_C",
                   "rdlPFC_D", "rvlPFC_A", "rvlPFC_B", "rvlPFC_C", "rdmPFC_A", 
                   "rdmPFC_B", "rdmPFC_C", "rdmPFC_D", "rdmPFC_E", "rdmPFC_F", 
                   "rdmPFC_G", "lMTG_A", "lMTG_B", "lSPL_A", "lSPL_B", 
                   "lSPL_C", "lSPL_D", "lSPL_E", "rSPL_A", "rSPL_B", "rSPL_C", 
                   "rSPL_D")
  ##First, the connectivity RSA
  
  
  #Create similarity matrices for each task state (Far, Look)
  farSimER = corr.test(farER, method = "spearman")[[1]] 
  lookSimER = corr.test(lookER, method = "spearman")[[1]]
  
  #Lop off redundant parts of said matrices, and vectorize them
  farSimVecER = farSimER[lower.tri(farSimER)]
  lookSimVecER = lookSimER[lower.tri(lookSimER)]
  
  #Variance stabilization via Fisher Z transform
  farSimVecZER = fisherz(farSimVecER)
  lookSimVecZER = fisherz(lookSimVecER)
  
  #correlate similarity vectors a la spearman
  connectSimER = cor(farSimVecZER, lookSimVecZER, method="spearman")
  
  #Assign value of correlation to output vector created outside the loop
  outSimER[i] = connectSimER
  
  
  ##Now for modularity & community; start with Far and then do Look. Steps for each task state follow.
  
  #Fisher transform the similarity matrix from above (but not vectorize this time)
  #Turn into adjacency matrix; criterion for an edge is a Z value > 0.5
  #Use the adjacency matrix to create  agraph
  #Calculate modularity (using walk trap algorithm)
  #Calculate number of communities (using walk trap algorithm)
  
  farSimZER = fisherz(farSimER)
  farAdjmER = farSimZER; farAdjmER[ farSimZER == Inf] = 0; farAdjmER[ farSimZER < .5000 ] = 0
  farGraphER = graph_from_adjacency_matrix(farAdjmER, mode = "undirected", weighted=TRUE)
  farModER = modularity(cluster_walktrap(farGraphER))
  farCommER = length(cluster_walktrap(farGraphER))
  
  lookSimZER = fisherz(lookSimER)
  lookAdjmER = lookSimZER; lookAdjmER[ lookSimZER == Inf] = 0; lookAdjmER[ lookSimZER < .5000 ] = 0
  lookGraphER = graph_from_adjacency_matrix(lookAdjmER, mode = "undirected", weighted=TRUE)
  lookModER = modularity(cluster_walktrap(lookGraphER))
  lookCommER = length(cluster_walktrap(lookGraphER))
  
  
  #Take differences between task states and assign to output
  modDiffER[i] = farModER - lookModER
  commDiffER[i] = farCommER - lookCommER
  
  #Default Mode Network (defined using forward inference maps on NeuroSynth)
  farDMN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/DMN/%s_expanded_farNeg_lss_DMN_ts.txt", subs[i]))
  names(farDMN) <- c('ACC_A_sphere', 'ACC_B_sphere', 'ACC_C_sphere', 'ACC_D_sphere', 'ACC_E_sphere',
                  'lAI_A_sphere',
                  'rAI_A_sphere', 
                  'ldlPFC_A_sphere', 'ldlPFC_B_sphere',
                  'rdlPFC_A_sphere',
                  'lHip_A_sphere', 'lHip_B_sphere', 'lHip_C_sphere', 
                  'rHip_A_sphere',
                  'lTPJ_A_sphere', 'lTPJ_B_sphere',
                  'rTPJ_A_sphere', 'rTPJ_B_sphere',
                  'mPFC_A_sphere', 'mPFC_B_sphere', 'mPFC_C_sphere', 
                  'PCC_A_sphere', 'PCC_B_sphere', 'PCC_C_sphere',
                  'Tha_A_sphere', 
                  'lTS_A_sphere')
  
  lookDMN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/DMN/%s_expanded_lookNeg_lss_DMN_ts.txt", subs[i]))
  names(lookDMN) <- c('ACC_A_sphere', 'ACC_B_sphere', 'ACC_C_sphere', 'ACC_D_sphere', 'ACC_E_sphere',
                   'lAI_A_sphere',
                   'rAI_A_sphere', 
                   'ldlPFC_A_sphere', 'ldlPFC_B_sphere',
                   'rdlPFC_A_sphere',
                   'lHip_A_sphere', 'lHip_B_sphere', 'lHip_C_sphere', 
                   'rHip_A_sphere',
                   'lTPJ_A_sphere', 'lTPJ_B_sphere',
                   'rTPJ_A_sphere', 'rTPJ_B_sphere',
                   'mPFC_A_sphere', 'mPFC_B_sphere', 'mPFC_C_sphere', 
                   'PCC_A_sphere', 'PCC_B_sphere', 'PCC_C_sphere',
                   'Tha_A_sphere', 
                   'lTS_A_sphere')  
  
  farSimDMN = corr.test(farDMN, method = "spearman")[[1]]
  lookSimDMN = corr.test(lookDMN, method = "spearman")[[1]]
  
  farSimVecDMN = farSimDMN[lower.tri(farSimDMN)]
  lookSimVecDMN = lookSimDMN[lower.tri(lookSimDMN)]
  
  farSimVecZDMN = fisherz(farSimVecDMN)
  lookSimVecZDMN = fisherz(lookSimVecDMN)
  
  connectSimDMN = cor(farSimVecZDMN, lookSimVecZDMN, method="spearman")
  
  outSimDMN[i] = connectSimDMN
  
  
  farSimZDMN = fisherz(farSimDMN)
  farAdjmDMN = farSimZDMN; farAdjmDMN[ farSimZDMN == Inf] = 0; farAdjmDMN[ farSimZDMN < .5000 ] = 0
  farGraphDMN = graph_from_adjacency_matrix(farAdjmDMN, mode = "undirected", weighted=TRUE)
  farModDMN = modularity(cluster_walktrap(farGraphDMN))
  farCommDMN = length(cluster_walktrap(farGraphDMN))
  
  lookSimZDMN = fisherz(lookSimDMN)
  lookAdjmDMN = lookSimZDMN; lookAdjmDMN[ lookSimZDMN == Inf] = 0; lookAdjmDMN[ lookSimZDMN < .5000 ] = 0
  lookGraphDMN = graph_from_adjacency_matrix(lookAdjmDMN, mode = "undirected", weighted=TRUE)
  lookModDMN = modularity(cluster_walktrap(lookGraphDMN))
  lookCommDMN = length(cluster_walktrap(lookGraphDMN))
  
  modDiffDMN[i] = farModDMN - lookModDMN
  commDiffDMN[i] = farCommDMN - lookCommDMN
  
  #Fronto-Parietal Network (defined using forward inference maps on NeuroSynth)
  farFPN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/FPN/%s_expanded_farNeg_lss_FPN_ts.txt", subs[i]))
  names(farFPN) <- c('ACC_A_sphere', 'ACC_B_sphere', 'ACC_C_sphere',
                  'lAI_A_sphere',
                  'rAI_A_sphere', 
                  'ldlPFC_A_sphere', 'ldlPFC_B_sphere',
                  'rdlPFC_A_sphere', 'rdlPFC_B_sphere', 'rdlPFC_C_sphere',
                  'lSPC_A_sphere', 'lSPC_B_sphere', 'lSPC_C_sphere', 
                  'rSPC_A_sphere', 'rSPC_B_sphere', 'rSPC_C_sphere', 'rSPC_D_sphere', 'rSPC_E_sphere', 'rSPC_F_sphere', 'rSPC_G_sphere', 'rSPC_H_sphere',
                  'Tha_A_sphere', 
                  'Tha_B_sphere',
                  'lPMC_A_sphere',
                  'rPMC_A_sphere')
  
  lookFPN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/FPN/%s_expanded_lookNeg_lss_FPN_ts.txt", subs[i]))
  names(lookFPN) <- c('ACC_A_sphere', 'ACC_B_sphere', 'ACC_C_sphere',
                   'lAI_A_sphere',
                   'rAI_A_sphere', 
                   'ldlPFC_A_sphere', 'ldlPFC_B_sphere',
                   'rdlPFC_A_sphere', 'rdlPFC_B_sphere', 'rdlPFC_C_sphere',
                   'lSPC_A_sphere', 'lSPC_B_sphere', 'lSPC_C_sphere', 
                   'rSPC_A_sphere', 'rSPC_B_sphere', 'rSPC_C_sphere', 'rSPC_D_sphere', 'rSPC_E_sphere', 'rSPC_F_sphere', 'rSPC_G_sphere', 'rSPC_H_sphere',
                   'Tha_A_sphere', 
                   'Tha_B_sphere',
                   'lPMC_A_sphere',
                   'rPMC_A_sphere')
  
  farSimFPN = corr.test(farFPN, method = "spearman")[[1]]
  lookSimFPN = corr.test(lookFPN, method = "spearman")[[1]]
  
  farSimVecFPN = farSimFPN[lower.tri(farSimFPN)]
  lookSimVecFPN = lookSimFPN[lower.tri(lookSimFPN)]
  
  farSimVecZFPN = fisherz(farSimVecFPN)
  lookSimVecZFPN = fisherz(lookSimVecFPN)
  
  connectSimFPN = cor(farSimVecZFPN, lookSimVecZFPN, method="spearman")
  
  outSimFPN[i] = connectSimFPN
  
  
  farSimZFPN = fisherz(farSimFPN)
  farAdjmFPN = farSimZFPN; farAdjmFPN[ farSimZFPN == Inf] = 0; farAdjmFPN[ farSimZFPN < .5000 ] = 0
  farGraphFPN = graph_from_adjacency_matrix(farAdjmFPN, mode = "undirected", weighted=TRUE)
  farModFPN = modularity(cluster_walktrap(farGraphFPN))
  farCommFPN = length(cluster_walktrap(farGraphFPN))
  
  lookSimZFPN = fisherz(lookSimFPN)
  lookAdjmFPN = lookSimZFPN; lookAdjmFPN[ lookSimZFPN == Inf] = 0; lookAdjmFPN[ lookSimZFPN < .5000 ] = 0
  lookGraphFPN = graph_from_adjacency_matrix(lookAdjmFPN, mode = "undirected", weighted=TRUE)
  lookModFPN = modularity(cluster_walktrap(lookGraphFPN))
  lookCommFPN = length(cluster_walktrap(lookGraphFPN))
  
  modDiffFPN[i] = farModFPN - lookModFPN
  commDiffFPN[i] = farCommFPN - lookCommFPN
  
  #Salience Network (defined using forward inference maps on NeuroSynth)
  farSaN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/SaN/%s_expanded_farNeg_lss_SaN_ts.txt", subs[i]))
  names(farSaN) <- c('ACC_A_sphere', 'ACC_B_sphere',
                  'PCC_A_sphere', 'PCC_B_sphere', 'PCC_C_sphere',
                  'lAI_A_sphere', 'lAI_B_sphere', 'lAI_C_sphere', 
                  'rAI_A_sphere', 'rAI_B_sphere', 'rAI_C_sphere', 
                  'lTPJ_A_sphere', 'lTPJ_B_sphere', 'lTPJ_C_sphere')
  
  lookSaN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/SaN/%s_expanded_lookNeg_lss_SaN_ts.txt", subs[i]))
  names(lookSaN) <- c('ACC_A_sphere', 'ACC_B_sphere',
                   'PCC_A_sphere', 'PCC_B_sphere', 'PCC_C_sphere',
                   'lAI_A_sphere', 'lAI_B_sphere', 'lAI_C_sphere', 
                   'rAI_A_sphere', 'rAI_B_sphere', 'rAI_C_sphere', 
                   'lTPJ_A_sphere', 'lTPJ_B_sphere', 'lTPJ_C_sphere')
  
  farSimSAN = corr.test(farSaN, method = "spearman")[[1]]
  lookSimSAN = corr.test(lookSaN, method = "spearman")[[1]]
  
  farSimVecSAN = farSimSAN[lower.tri(farSimSAN)]
  lookSimVecSAN = lookSimSAN[lower.tri(lookSimSAN)]
  
  farSimVecZSAN = fisherz(farSimVecSAN)
  lookSimVecZSAN = fisherz(lookSimVecSAN)
  
  connectSimSAN = cor(farSimVecZSAN, lookSimVecZSAN, method="spearman")
  
  outSimSaN[i] = connectSimSAN
  
  
  farSimZSAN = fisherz(farSimSAN)
  farAdjmSAN = farSimZSAN; farAdjmSAN[ farSimZSAN == Inf] = 0; farAdjmSAN[ farSimZSAN < .5000 ] = 0
  farGraphSAN = graph_from_adjacency_matrix(farAdjmSAN, mode = "undirected", weighted=TRUE)
  farModSAN = modularity(cluster_walktrap(farGraphSAN))
  farCommSAN = length(cluster_walktrap(farGraphSAN))
  
  lookSimZSAN = fisherz(lookSimSAN)
  lookAdjmSAN = lookSimZSAN; lookAdjmSAN[ lookSimZSAN == Inf] = 0; lookAdjmSAN[ lookSimZSAN < .5000 ] = 0
  lookGraphSAN = graph_from_adjacency_matrix(lookAdjmSAN, mode = "undirected", weighted=TRUE)
  lookModSAN = modularity(cluster_walktrap(lookGraphSAN))
  lookCommSAN = length(cluster_walktrap(lookGraphSAN))
  
  modDiffSaN[i] = farModSAN - lookModSAN
  commDiffSaN[i] = farCommSAN - lookCommSAN
  
  #Dorsal Attention Network (defined using forward inference maps on NeuroSynth)
  farDAN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/DAN/%s_expanded_farNeg_lss_DAN_ts.txt", subs[i]))
  names(farDAN) <- c('lLOC_A_sphere', 'lLOC_B_sphere', 'lLOC_C_sphere', 'lLOC_D_sphere', 'lLOC_E_sphere', 
                  'rLOC_A_sphere', 'rLOC_B_sphere',
                  'lSPC_A_sphere', 'lSPC_B_sphere', 'lSPC_C_sphere', 'lSPC_D_sphere', 'lSPC_E_sphere',
                  'rSPC_A_sphere', 'rSPC_B_sphere',
                  'lPCG_A_sphere',
                  'rPCG_A_sphere', 'rPCG_B_sphere', 'rPCG_C_sphere', 'rPCG_D_sphere', 
                  'ACC_A_sphere', 
                  'PCC_A_sphere',
                  'rAI_A_sphere', 
                  'lMFG_A_sphere', 'lMFG_B_sphere',
                  'mPFC_A_sphere', 'mPFC_B_sphere', 'mPFC_C_sphere')
  
  lookDAN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/DAN/%s_expanded_lookNeg_lss_DAN_ts.txt", subs[i]))
  names(lookDAN) <- c('lLOC_A_sphere', 'lLOC_B_sphere', 'lLOC_C_sphere', 'lLOC_D_sphere', 'lLOC_E_sphere', 
                   'rLOC_A_sphere', 'rLOC_B_sphere',
                   'lSPC_A_sphere', 'lSPC_B_sphere', 'lSPC_C_sphere', 'lSPC_D_sphere', 'lSPC_E_sphere',
                   'rSPC_A_sphere', 'rSPC_B_sphere',
                   'lPCG_A_sphere',
                   'rPCG_A_sphere', 'rPCG_B_sphere', 'rPCG_C_sphere', 'rPCG_D_sphere', 
                   'ACC_A_sphere', 
                   'PCC_A_sphere',
                   'rAI_A_sphere', 
                   'lMFG_A_sphere', 'lMFG_B_sphere',
                   'mPFC_A_sphere', 'mPFC_B_sphere', 'mPFC_C_sphere')
  
  farSimDAN = corr.test(farDAN, method = "spearman")[[1]]
  lookSimDAN = corr.test(lookDAN, method = "spearman")[[1]]
  
  farSimVecDAN = farSimDAN[lower.tri(farSimDAN)]
  lookSimVecDAN = lookSimDAN[lower.tri(lookSimDAN)]
  
  farSimVecZDAN = fisherz(farSimVecDAN)
  lookSimVecZDAN = fisherz(lookSimVecDAN)
  
  connectSimDAN = cor(farSimVecZDAN, lookSimVecZDAN, method="spearman")
  
  outSimDAN[i] = connectSimDAN
  
  
  farSimZDAN = fisherz(farSimDAN)
  farAdjmDAN = farSimZDAN; farAdjmDAN[ farSimZDAN == Inf] = 0; farAdjmDAN[ farSimZDAN < .5000 ] = 0
  farGraphDAN = graph_from_adjacency_matrix(farAdjmDAN, mode = "undirected", weighted=TRUE)
  farModDAN = modularity(cluster_walktrap(farGraphDAN))
  farCommDAN = length(cluster_walktrap(farGraphDAN))
  
  lookSimZDAN = fisherz(lookSimDAN)
  lookAdjmDAN = lookSimZDAN; lookAdjmDAN[ lookSimZDAN == Inf] = 0; lookAdjmDAN[ lookSimZDAN < .5000 ] = 0
  lookGraphDAN = graph_from_adjacency_matrix(lookAdjmDAN, mode = "undirected", weighted=TRUE)
  lookModDAN = modularity(cluster_walktrap(lookGraphDAN))
  lookCommDAN = length(cluster_walktrap(lookGraphDAN))
  
  modDiffDAN[i] = farModDAN - lookModDAN
  commDiffDAN[i] = farCommDAN - lookCommDAN
  
  #Ventral Attention Network (defined using forward inference maps on NeuroSynth)
  farVAN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/VAN/%s_expanded_farNeg_lss_VAN_ts.txt", subs[i]))
  names(farVAN) <- c('lLOC_A_sphere', 'lLOC_B_sphere', 'lLOC_C_sphere', 'lLOC_D_sphere', 
                  'rLOC_A_sphere', 'rLOC_B_sphere', 'rLOC_C_sphere',
                  'lFFG_A_sphere', 'lFFG_B_sphere', 'lFFG_C_sphere',
                  'rFFG_A_sphere', 'rFFG_B_sphere', 'rFFG_C_sphere', 'rFFG_D_sphere', 'rFFG_E_sphere',
                  'ACC_A_sphere',
                  'lAI_A_sphere',
                  'rAI_A_sphere', 
                  'lOP_A_sphere',
                  'rOP_A_sphere',
                  'lIFG_A_sphere')
  
  lookVAN <- read.table(sprintf("C:/Users/jguas/Desktop/McReapp/lss_trials/VAN/%s_expanded_lookNeg_lss_VAN_ts.txt", subs[i]))
  names(lookVAN) <- c('lLOC_A_sphere', 'lLOC_B_sphere', 'lLOC_C_sphere', 'lLOC_D_sphere', 
                   'rLOC_A_sphere', 'rLOC_B_sphere', 'rLOC_C_sphere',
                   'lFFG_A_sphere', 'lFFG_B_sphere', 'lFFG_C_sphere',
                   'rFFG_A_sphere', 'rFFG_B_sphere', 'rFFG_C_sphere', 'rFFG_D_sphere', 'rFFG_E_sphere',
                   'ACC_A_sphere',
                   'lAI_A_sphere',
                   'rAI_A_sphere', 
                   'lOP_A_sphere',
                   'rOP_A_sphere',
                   'lIFG_A_sphere')
  
  farSimVAN = corr.test(farVAN, method = "spearman")[[1]]
  lookSimVAN = corr.test(lookVAN, method = "spearman")[[1]]
  
  farSimVecVAN = farSimVAN[lower.tri(farSimVAN)]
  lookSimVecVAN = lookSimVAN[lower.tri(lookSimVAN)]
  
  farSimVecZVAN = fisherz(farSimVecVAN)
  lookSimVecZVAN = fisherz(lookSimVecVAN)
  
  connectSimVAN = cor(farSimVecZVAN, lookSimVecZVAN, method="spearman")
  
  outSimVAN[i] = connectSimVAN
  
  
  farSimZVAN = fisherz(farSimVAN)
  farAdjmVAN = farSimZVAN; farAdjmVAN[ farSimZVAN == Inf] = 0; farAdjmVAN[ farSimZVAN < .5000 ] = 0
  farGraphVAN = graph_from_adjacency_matrix(farAdjmVAN, mode = "undirected", weighted=TRUE)
  farModVAN = modularity(cluster_walktrap(farGraphVAN))
  farCommVAN = length(cluster_walktrap(farGraphVAN))
  
  lookSimZVAN = fisherz(lookSimVAN)
  lookAdjmVAN = lookSimZVAN; lookAdjmVAN[ lookSimZVAN == Inf] = 0; lookAdjmVAN[ lookSimZVAN < .5000 ] = 0
  lookGraphVAN = graph_from_adjacency_matrix(lookAdjmVAN, mode = "undirected", weighted=TRUE)
  lookModVAN = modularity(cluster_walktrap(lookGraphVAN))
  lookCommVAN = length(cluster_walktrap(lookGraphVAN))
  
  modDiffVAN[i] = farModVAN - lookModVAN
  commDiffVAN[i] = farCommVAN - lookCommVAN
}

#Read in age & capacity scores, then winsorize outliers on the neuro data
behavDat = read_excel("C:/Users/jguas/Desktop/McReapp/behavData.xlsx")
#hist(outSimER)
#order(outSimER)
outSimER[54] = outSimER[14]
#hist(modDiffER)
#hist(commDiffER)
#hist(outSimDMN)
#hist(modDiffDMN)
#hist(commDiffDMN)
#hist(outSimFPN)
#hist(modDiffFPN)
#hist(commDiffFPN)
#hist(outSimSaN)
#hist(modDiffSaN)
#hist(commDiffSaN)
#order(commDiffSaN)
commDiffSaN[48] = commDiffSaN[18]
#hist(outSimDAN)
#hist(modDiffDAN)
#hist(commDiffDAN)
commDiffDAN[31] = commDiffDAN[54]
#hist(outSimVAN)
#order(outSimVAN)
outSimVAN[54] = outSimVAN[55]
#hist(modDiffVAN)
#hist(commDiffVAN)
#behavDat$outSimER

##add brain data to age & capacity scores
behavDat = cbind(behavDat, outSimER, modDiffER, commDiffER, outSimDMN, modDiffDMN, commDiffDMN,
                 outSimFPN, modDiffFPN, commDiffFPN, outSimSaN, modDiffSaN, commDiffSaN,
                 outSimDAN, modDiffDAN, commDiffDAN, outSimVAN, modDiffVAN, commDiffVAN)

##Create df with only brain dadta
netDat = behavDat[-(1:4)]
#netDatT = t(netDat)

library(ape)
library(dendextend)

##Dendrogram!!
nd <- as.dist((1-cor(scale(netDat)))/2)
plot(hclust(nd))

##Dendrogram with only RSA scores
netDatSimOnly = cbind(outSimER, outSimDMN, outSimFPN, outSimSaN, outSimDAN, outSimVAN)
colnames(netDatSimOnly) = c("CRN", "DMN", "FPN", "SaN", "DAN", "VAN")
ndso <- as.dist((1-cor(scale(netDatSimOnly)))/2)
ndso = hclust(ndso)
op = par(bg = "#E8DDCB")
plot(as.phylo(ndso), type = "unrooted", tip.color = c("blue", "darkgreen", "red", "purple", "orange", "black"))
ind = c(1:70)
ndso_boot = c(rep(NaN, 10000))
set.seed(300)
for (i in 1:10000) {
  samp = sample(ind, size = 70, replace=TRUE)
  dthisi = netDatSimOnly[samp,]
  clustthisi = hclust(as.dist((1-cor(scale(dthisi)))/2))
  ndso_boot[i] = cor_cophenetic(as.dendrogram(ndso), as.dendrogram(clustthisi))
}
mean(ndso_boot)
quantile(ndso_boot, c(.025, .975))
hist(ndso_boot)
#plot(hclust(ndso))   obsolete can delete soon


##Dendrogram with only community diff scores
netDatCommOnly = cbind(commDiffER, commDiffDMN, commDiffFPN, commDiffSaN, commDiffDAN, commDiffVAN)
colnames(netDatCommOnly) = c("CRN", "DMN", "FPN", "SaN", "DAN", "VAN")
ndco <- as.dist((1-cor(scale(netDatCommOnly)))/2)
ndco = hclust(ndco)
op = par(bg = "#E8DDCB")
plot(as.phylo(ndco), type = "unrooted", tip.color = c("blue", "darkgreen", "red", "purple", "orange", "black"))
ind = c(1:70)
ndco_boot = c(rep(NaN, 10000))
set.seed(30)
for (i in 1:10000) {
  samp = sample(ind, size = 70, replace=TRUE)
  dthisi = netDatCommOnly[samp,]
  clustthisi = hclust(as.dist((1-cor(scale(dthisi)))/2))
  ndco_boot[i] = cor_cophenetic(as.dendrogram(ndco), as.dendrogram(clustthisi))
}
mean(ndco_boot)
quantile(ndco_boot, c(.025, .975))
hist(ndco_boot)

##Dendrogram with only modularity diff scores
netDatModOnly = cbind(modDiffER, modDiffDMN, modDiffFPN, modDiffSaN, modDiffDAN, modDiffVAN)
colnames(netDatModOnly) = c("CRN", "DMN", "FPN", "SaN", "DAN", "VAN")
ndmo <- as.dist((1-cor(scale(netDatModOnly)))/2)
ndmo = hclust(ndmo)
op = par(bg = "#E8DDCB")
plot(as.phylo(ndmo), type = "unrooted", tip.color = c("blue", "darkgreen", "red", "purple", "orange", "black"))
ind = c(1:70)
ndmo_boot = c(rep(NaN, 10000))
set.seed(300)
for (i in 1:10000) {
  samp = sample(ind, size = 70, replace=TRUE)
  dthisi = netDatModOnly[samp,]
  clustthisi = hclust(as.dist((1-cor(scale(dthisi)))/2))
  ndmo_boot[i] = cor_cophenetic(as.dendrogram(ndmo), as.dendrogram(clustthisi))
}
mean(ndmo_boot)
quantile(ndmo_boot, c(.025, .975))
hist(ndmo_boot)

##Compare the dendrograms


cor_cophenetic(as.dendrogram(ndso), as.dendrogram(ndco))
ind = c(1:70)
sc_boot = c(rep(NaN, 10000))
set.seed(300)
for (i in 1:10000) {
  samp = sample(ind, size = 70, replace=TRUE)
  dsthisi = netDatSimOnly[samp,]; dcthisi = netDatCommOnly[samp,]
  cluststhisi = hclust(as.dist((1-cor(scale(dsthisi)))/2)); clustcthisi = hclust(as.dist((1-cor(scale(dcthisi)))/2))
  sc_boot[i] = cor_cophenetic(as.dendrogram(clustcthisi), as.dendrogram(cluststhisi))
}
quantile(sc_boot, c(.025, .975))
hist(sc_boot)

cor_cophenetic(as.dendrogram(ndso), as.dendrogram(ndmo))
ind = c(1:70)
sm_boot = c(rep(NaN, 10000))
set.seed(300)
for (i in 1:10000) {
  samp = sample(ind, size = 70, replace=TRUE)
  dsthisi = netDatSimOnly[samp,]; dmthisi = netDatModOnly[samp,]
  cluststhisi = hclust(as.dist((1-cor(scale(dsthisi)))/2)); clustmthisi = hclust(as.dist((1-cor(scale(dmthisi)))/2))
  sm_boot[i] = cor_cophenetic(as.dendrogram(clustmthisi), as.dendrogram(cluststhisi))
}
quantile(sm_boot, c(.025, .975))
hist(sm_boot)

cor_cophenetic(as.dendrogram(ndco), as.dendrogram(ndmo))
ind = c(1:70)
cm_boot = c(rep(NaN, 10000))
set.seed(300)
for (i in 1:10000) {
  samp = sample(ind, size = 70, replace=TRUE)
  dcthisi = netDatSimOnly[samp,]; dmthisi = netDatModOnly[samp,]
  clustcthisi = hclust(as.dist((1-cor(scale(dcthisi)))/2)); clustmthisi = hclust(as.dist((1-cor(scale(dmthisi)))/2))
  cm_boot[i] = cor_cophenetic(as.dendrogram(clustcthisi), as.dendrogram(cluststhisi))
}
quantile(cm_boot, c(.025, .975))
hist(cm_boot)


ERN = rowMeans(scale(netDat[,c(1:3)]))
DMN = rowMeans(scale(netDat[,c(4:6)]))
FPN = rowMeans(scale(netDat[,c(7:9)]))
SaN = rowMeans(scale(netDat[,c(10:12)]))
DAN = rowMeans(scale(netDat[,c(13:15)]))
VAN = rowMeans(scale(netDat[,c(16:18)]))
netMat = cbind(ERN, DMN, FPN, SaN, DAN, VAN)
nda <- as.dist((1-cor(scale(netMat)))/2)
plot(hclust(nda))
##Create helper function to normalize the data
####Normalize data function####
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}
normDat <- normalize(behavDat[-1])

##Correlations with age and capacity
ERN_corr = corr.test(normDat[c(1,2,4,5,6)], method = "spearman", alpha=.10)
DMN_corr = corr.test(normDat[c(1,2,7,8,9)], method = "spearman", alpha=.10)
FPN_corr = corr.test(normDat[c(1,2,10,11,12)], method = "spearman", alpha=.10)
SaN_corr = corr.test(normDat[c(1,2,13,14,15)], method = "spearman", alpha=.10)
DAN_corr = corr.test(normDat[c(1,2,16,17,18)], method = "spearman", alpha=.10)
VAN_corr = corr.test(normDat[c(1,2,19,20,21)], method = "spearman", alpha=.10)


##Now comes K-Nearest Neighbor Analyses
####Split into test and train sets####
library(class)
library(gmodels)
set.seed(1234)
##Assign labels to age & capcity
agelab = ifelse(behavDat$cAge < 0, "child", "teen")
caplab = ifelse(behavDat$Capacity < 20, "low_cap", "high_cap") 



#samps <- matrix(sample(2, 70000, replace=TRUE, prob=c(0.67, 0.33)), 1000, 70)

#ind <- sample(2, nrow(normDat), replace=TRUE, prob=c(0.67, 0.33))
#ind <- samps[i,]
  
#normDat.Train <- normDat[ind==1,]
#normDat.Test <- normDat[ind==2,]
#normDat.TrainLabels.Age <- agelab[ind==1]
#normDat.TestLabels.Age <- agelab[ind==2]
#normDat.TrainLabels.Cap <- caplab[ind==1]
#normDat.TestLabels.Cap <- caplab[ind==2]

##Model building time!! Uses leave-one-out cross validation

##Model orders
#Predicting age, K = 5, using all brain data
#Predicting capacity, K = 5, using all brain data
#Predicting age, K = 5, using just similarity data
#Predicting capacity, K = 5, using just similarity data

pred_age_all_5 = knn.cv(normDat[-(1:3)], agelab, k = 5, prob=TRUE)
pred_cap_all_5 = knn.cv(normDat[-(1:3)], caplab, k = 5, prob=TRUE)
pred_age_sim_5 = knn.cv(normDat[c(4, 7, 10, 13, 16, 19)], agelab, k = 5, prob=TRUE)
pred_cap_sim_5 = knn.cv(normDat[c(4, 7, 10, 13, 16, 19)], caplab, k = 5, prob=TRUE)

#Predicting age, K = 3, using all brain data
#Predicting capacity, K = 3, using all brain data
#Predicting age, K = 3, using just similarity data
#Predicting capacity, K = 3, using just similarity data

pred_age_all_3 = knn.cv(normDat[-(1:3)], agelab, k = 3, prob=TRUE)
pred_cap_all_3 = knn.cv(normDat[-(1:3)], caplab, k = 3, prob=TRUE)
pred_age_sim_3 = knn.cv(normDat[c(4, 7, 10, 13, 16, 19)], agelab, k = 3, prob=TRUE)
pred_cap_sim_3 = knn.cv(normDat[c(4, 7, 10, 13, 16, 19)], caplab, k = 3, prob=TRUE)

#Predicting age, K = 7, using all brain data
#Predicting capacity, K = 7, using all brain data
#Predicting age, K = 7, using just similarity data
#Predicting capacity, K = 7, using just similarity data

pred_age_all_7 = knn.cv(normDat[-(1:3)], agelab, k = 7, prob=TRUE)
pred_cap_all_7 = knn.cv(normDat[-(1:3)], caplab, k = 7, prob=TRUE)
pred_age_sim_7 = knn.cv(normDat[c(4, 7, 10, 13, 16, 19)], agelab, k = 7, prob=TRUE)
pred_cap_sim_7 = knn.cv(normDat[c(4, 7, 10, 13, 16, 19)], caplab, k = 7, prob=TRUE)

##Now break down and train by networks. K = 5 for all

#ER
pred_age_ER_5 = knn.cv(normDat[4:6], agelab, k = 5, prob=TRUE)
pred_cap_ER_5 = knn.cv(normDat[4:6], caplab, k = 5, prob=TRUE)
pred_cap_ER_3 = knn.cv(normDat[4:6], caplab, k = 3, prob=TRUE)
#DMN
pred_age_DMN_5 = knn.cv(normDat[7:9], agelab, k = 5, prob=TRUE)
pred_cap_DMN_5 = knn.cv(normDat[7:9], caplab, k = 5, prob=TRUE)
pred_cap_DMN_3 = knn.cv(normDat[7:9], caplab, k = 3, prob=TRUE)
#FPN
pred_age_FPN_5 = knn.cv(normDat[10:12], agelab, k = 5, prob=TRUE)
pred_cap_FPN_5 = knn.cv(normDat[10:12], caplab, k = 5, prob=TRUE)
pred_cap_FPN_3 = knn.cv(normDat[10:12], caplab, k = 3, prob=TRUE)
#SaN
pred_age_SaN_5 = knn.cv(normDat[13:15], agelab, k = 5, prob=TRUE)
pred_cap_SaN_5 = knn.cv(normDat[13:15], caplab, k = 5, prob=TRUE)
pred_cap_SaN_3 = knn.cv(normDat[13:15], caplab, k = 3, prob=TRUE)
#DAN
pred_age_DAN_5 = knn.cv(normDat[16:18], agelab, k = 5, prob=TRUE)
pred_cap_DAN_5 = knn.cv(normDat[16:18], caplab, k = 5, prob=TRUE)
pred_cap_DAN_3 = knn.cv(normDat[16:18], agelab, k = 3, prob=TRUE)
#VAN
pred_age_VAN_5 = knn.cv(normDat[19:21], agelab, k = 5, prob=TRUE)
pred_cap_VAN_5 = knn.cv(normDat[19:21], agelab, k = 5, prob=TRUE)
pred_cap_VAN_3 = knn.cv(normDat[19:21], agelab, k = 3, prob=TRUE)

##Tabulate our results
CrossTable(x = agelab, y = pred_age_all_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_all_5, prop.chisq=FALSE)
CrossTable(x = agelab, y = pred_age_sim_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_sim_5, prop.chisq=FALSE)

CrossTable(x = agelab, y = pred_age_all_3, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_all_3, prop.chisq=FALSE)
CrossTable(x = agelab, y = pred_age_sim_3, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_sim_3, prop.chisq=FALSE)

CrossTable(x = agelab, y = pred_age_all_7, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_all_7, prop.chisq=FALSE)
CrossTable(x = agelab, y = pred_age_sim_7, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_sim_7, prop.chisq=FALSE)



CrossTable(x = agelab, y = pred_age_ER_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_ER_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_ER_3, prop.chisq=FALSE)

CrossTable(x = agelab, y = pred_age_DMN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_DMN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_DMN_3, prop.chisq=FALSE)

CrossTable(x = agelab, y = pred_age_FPN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_FPN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_FPN_3, prop.chisq=FALSE)

CrossTable(x = agelab, y = pred_age_SaN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_SaN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_SaN_3, prop.chisq=FALSE)

CrossTable(x = agelab, y = pred_age_DAN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_DAN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_DAN_3, prop.chisq=FALSE)

CrossTable(x = agelab, y = pred_age_VAN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_VAN_5, prop.chisq=FALSE)
CrossTable(x = caplab, y = pred_cap_VAN_3, prop.chisq=FALSE)


###ROC curves + AUC metrics###

#library(ROCR)
#library(pracma)

#auc_func <- function(fpv, tpv){
#  dfpv <- c(0, diff(fpv))
#  dtpv <- c(0, diff(tpv))
#  sum(tpv * dfpv) + sum(dtpv * dfpv)/2
#}

#knn_roc_auc <- function(model, class){
#  prob <- attr(model, "prob")
  
#  if (class = "teen") {
#    pred_knn <- prediction(prob, agelab)
#    pred_knn <- performance(pred_knn, "tpr", "fpr")
#    fp = attr(pred_knn, "x.values")
#    tp = attr(pred_knn, "y.values")
#    fp.bt = linspace(fp[[1]][1], fp[[1]][2], 5)
#    fp.mt = linspace(fp[[1]][2], fp[[1]][3], 5)
#    fp.tt = linspace(fp[[1]][3], fp[[1]][4], 5)
#    fp.int = c(fp.bt, fp.mt[2:4], fp.tt)
#    tp.bt = linspace(tp[[1]][1], tp[[1]][2], 5)
#    tp.mt = linspace(tp[[1]][2], tp[[1]][3], 5)
#    tp.tt = linspace(tp[[1]][3], tp[[1]][4], 5)
#    tp.int = c(tp.bt, tp.mt[2:4], tp.tt)
#    auc_func(fp.int, tp.int)
      
    #plot(pred_knn, avg="threshold", colorize=F, lwd=3)
    
#  } else if (class = "child") {
#    prob <- 2*ifelse(model == "teen", 1-prob, prob) - 1
#    pred_knn <- prediction(prob, agelab)
#    pred_knn <- performance(pred_knn, "tpr", "fpr")
#    fp = attr(pred_knn, "x.values")
#    tp = attr(pred_knn, "y.values")
#    fp.bt = linspace(fp[[1]][1], fp[[1]][2], 5)
#    fp.mt = linspace(fp[[1]][2], fp[[1]][3], 5)
#    fp.tt = linspace(fp[[1]][3], fp[[1]][4], 5)
#    fp.int = c(fp.bt, fp.mt[2:4], fp.tt)
#    tp.bt = linspace(tp[[1]][1], tp[[1]][2], 5)
#    tp.mt = linspace(tp[[1]][2], tp[[1]][3], 5)
#    tp.tt = linspace(tp[[1]][3], tp[[1]][4], 5)
#    tp.int = c(tp.bt, tp.mt[2:4], tp.tt)
#    auc_func(fp.int, tp.int)
#  }
  
#}

#pred_age_all_5_acc[i,] = c(CrossTable(x = agelab, y = pred_age_all_5, prop.chisq=FALSE)$prop.row[1], CrossTable(x = agelab, y = pred_age_all_5, prop.chisq=FALSE)$prop.row[4])
#pred_cap_all_5_acc[i,] = c(CrossTable(x = caplab, y = pred_cap_all_5, prop.chisq=FALSE)$prop.row[1], CrossTable(x = caplab, y = pred_cap_all_5, prop.chisq=FALSE)$prop.row[4])
#pred_age_sim_5_acc[i,] = c(CrossTable(x = agelab, y = pred_age_sim_5, prop.chisq=FALSE)$prop.row[1], CrossTable(x = agelab, y = pred_age_sim_5, prop.chisq=FALSE)$prop.row[4])
#pred_cap_sim_5_acc[i,] = c(CrossTable(x = caplab, y = pred_cap_sim_5, prop.chisq=FALSE)$prop.row[1], CrossTable(x = caplab, y = pred_cap_sim_5, prop.chisq=FALSE)$prop.row[4])

#pred_age_all_3_acc[i,] = c(CrossTable(x = agelab, y = pred_age_all_3, prop.chisq=FALSE)$prop.row[1], CrossTable(x = agelab, y = pred_age_all_3, prop.chisq=FALSE)$prop.row[4])
#pred_cap_all_3_acc[i,] = c(CrossTable(x = caplab, y = pred_cap_all_3, prop.chisq=FALSE)$prop.row[1], CrossTable(x = caplab, y = pred_cap_all_3, prop.chisq=FALSE)$prop.row[4])
#pred_age_sim_3_acc[i,] = c(CrossTable(x = agelab, y = pred_age_sim_3, prop.chisq=FALSE)$prop.row[1], CrossTable(x = agelab, y = pred_age_sim_3, prop.chisq=FALSE)$prop.row[4])
#pred_cap_sim_3_acc[i,] = c(CrossTable(x = caplab, y = pred_cap_sim_3, prop.chisq=FALSE)$prop.row[1], CrossTable(x = caplab, y = pred_cap_sim_3, prop.chisq=FALSE)$prop.row[4])


####Using Caret now####
#library(caret)
#set.seed(1234)

#pred_all_acc = matrix(NA, 1000, 2)

#for (i in 1:1000) {
#indexAge <- createDataPartition(agelab, p=0.60, list=FALSE)
#indexCap <- createDataPartition(caplab, p=0.60, list=FALSE)

#normDat.Train.Age <- normDat[indexAge,]
#normDat.Test.Age <- normDat[-indexAge,]
#normDat.Train.Cap <- normDat[indexCap,]
#normDat.Test.Cap <- normDat[-indexCap,]
#normDat.Test.Age$lab = agelab[-indexAge]
#normDat.Test.Cap$lab = caplab[-indexCap]

#model_knn_age_all <- train(normDat.Train.Age[-(1:3)], agelab[indexAge], method = "knn")
#model_knn_cap_all <- train(normDat.Train.Cap[-(1:3)], caplab[indexCap], method = "knn")

#predictions_age_all <- predict(object=model_knn_age_all,normDat.Test.Age[-c(1:3,22)])
#predictions_cap_all <- predict(object=model_knn_cap_all,normDat.Test.Cap[-c(1:3,22)])
#table(predictions_age_all)
#table(predictions_cap_all)
#confusionMatrix(predictions_age_all, as.factor(normDat.Test.Age$lab))
#confusionMatrix(predictions_cap_all, as.factor(normDat.Test.Cap$lab))

#pred_all_acc[i, ] = c(confusionMatrix(predictions_age_all, as.factor(normDat.Test.Age$lab))$overall[1],
#                          confusionMatrix(predictions_cap_all, as.factor(normDat.Test.Cap$lab))$overall[1])

#}


