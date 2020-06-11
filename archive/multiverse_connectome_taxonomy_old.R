library(psych)
library(igraph)
library(readr)
library(pracma)
library(stringr)
library(ape)
library(dendextend)

subs = c(1001, 1002, 1005, 1006, 1007, 1008, 1031, 1032, 1034, 1036,   ##input subject IDs
         1072, 1077, 1078, 1081, 1089, 1090, 1091, 1098, 1105, 1106,
         1111, 1115, 1116, 1117, 1118, 1119, 1125, 1126, 1127, 1128,
         1132, 1133, 1134, 1136, 1142, 1143, 1145, 1146, 1147, 1152,
         1153, 1154, 1157, 1166, 1167, 1173, 1176, 1185, 1195, 1207,
         1208, 1218, 1226, 1227, 1228, 1233, 1237, 1239, 1240, 1244,
         1245, 1246, 1248, 1249, 1250, 1252, 1253, 1256, 1262, 1263)

##Import helper functions
datPath = Sys.glob(file.path("~", "Desktop", "McReapp"))
file.sources = Sys.glob(file.path(sprintf("%s", datPath), "multiverse", "helperFuncs", "*.R"))
sapply(file.sources,FUN=source)

##Set up the conditions of the multiverse analysis
detrend = c("detrend", "none") #linear detrending options of beta series
parcellation = c("schaefer7", "schaefer17", "ns1", "ns2") #parcellation options
thresh = c(.4000, .5000, .6000) #threshold options
co_corr = c()
co_corr_CI = c()
co_corr_gtr_zero = c()
analysis_id = c() #vector to keep track of which analysis corresponds to which fit metric in the output vector c

##Loop over each of the options above to begin the multiverse analysis
for (d in detrend) {
  for (p in parcellation) {
    for (t in thresh) {
      
      #Update the analysis ID vector
      analysis_id = c(analysis_id,paste(p, as.character(t), d, sep = "_"))
      
      #Preprocess the network data
      netDat = procDat(subs, parc = p, preproc_bs = d, net_thresh = t)
      #remove outliers by winsorizing
      netDat = winzOutlier(netDat) 
      #scale predictors ahead of analysis
      netDat = scale(netDat)
      
      #create the taxonomy
      taxResults = runTax(netDat)
      
      #store the output
      co_corr = c(co_corr, taxResults[[1]])
      co_corr_CI = c(co_corr_CI, taxResults[[2]])
      co_corr_gtr_zero = c(co_corr_gtr_zero, taxResults[[3]])
      
      print(paste(p, as.character(t), d, sep = "_"))
      
    }
    
    
  }
  
  
}

##Specification Curves
plot(sort(co_corr, decreasing = TRUE), pch = 20, xlab = "Analyses", ylab = "Average Bootstrapped Cophenetic Correlation", main = "Specification Curve")
analysis_id[order(co_corr, decreasing = TRUE)][1:5]
plot(sort(co_corr_gtr_zero, decreasing = TRUE), pch = 20, xlab = "Analyses", ylab = "Bootstrapped Cophenetic Correlations > 0 (%)", main = "Specification Curve")
analysis_id[order(co_corr_gtr_zero, decreasing = TRUE)][1:5]

CI_mat = matrix(co_corr_CI, 24, 2, byrow = TRUE)


##Interrogate each of the top three taxonomies
specs = data.frame(parc = c("ns1", "ns2", "ns1"),
                   trsh = c(0.5, 0.6, 0.6),
                   trnd = c("none", "detrend", "detrend"),
                   stringsAsFactors = FALSE)

outTaxList = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,1], preproc_bs = specs[r,3], net_thresh = specs[r,2])
  #remove outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #scale predictors ahead of analysis
  netDat = scale(netDat)
  
  #create the taxonomy
  taxResults = runTax(netDat)
  
  outTaxList[[r]] = taxResults[[4]]
  
}



##Interrogate each of the schaefer17 taxonomies
specs = data.frame(parc = c("schaefer17", "schaefer17", "schaefer17", "schaefer17"),
                   trsh = c(0.4, 0.5, 0.6, 0.6),
                   trnd = c("detrend", "none", "none", "detrend"),
                   stringsAsFactors = FALSE)

outTaxList = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,1], preproc_bs = specs[r,3], net_thresh = specs[r,2])
  #remove outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #scale predictors ahead of analysis
  netDat = scale(netDat)
  
  #create the taxonomy
  taxResults = runTax(netDat)
  
  outTaxList[[r]] = taxResults[[5]]
  
}

dAg = (outTaxList[[1]]+outTaxList[[2]]+outTaxList[[3]]+outTaxList[[4]])/4
