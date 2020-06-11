library(psych)
library(readxl)
library(igraph)
library(readr)
library(pracma)
library(glmnet)
library(matrixStats)

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

##Import age and capacity
behavDat = read_excel("C:/Users/jguas/Desktop/McReapp/behavData.xlsx")

##Schaefer 17 age models
##Bootstrapping!
specs = data.frame(parc = c(rep("schaefer17", 5)),
                   thresh = c(.4000, .4000, .4000, .6000, .5000),
                   detrend = c(rep("detrend",2), rep("none",2), "detrend"),
                   nb = c("NB-Pred", "no-NB-Pred", rep("NB-Pred", 3)),
                   stringsAsFactors = FALSE)

modList_s17_age = list()
modList_s17_age_CI = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,"parc"], preproc_bs = specs[r,"detrend"], net_thresh = specs[r,"thresh"])
  #remove outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #set lambdas that we're going to use for ridge regression
  lambdas = 10^seq(3, -2, by = -.01)
  
  cimat = matrix(NaN,55,5000)
  
  for (b in 1:5000) {
    
    samp = sample(c(1:70), replace = TRUE)
    
    if (specs[r,"nb"] == "NB-Pred") {
      
      x = cbind(scale(netDat[samp,]), scale(behavDat$Capacity[samp])); y = scale(behavDat$cAge[samp])
      cv_fit = cv.glmnet(x, y, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
      opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
      mod = glmnet(x, y, alpha = 0, lambda = opt_lambda)
      cimat[,b] = as.vector(mod$beta)
      
    } else {
      
      x = scale(netDat[samp,]); y = scale(behavDat$cAge[samp])
      cv_fit = cv.glmnet(x, y, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
      opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
      mod = glmnet(x, y, alpha = 0, lambda = opt_lambda)
      cimat[,b] = c(as.vector(mod$beta),NA)
      
    }
    
    ci = rowQuantiles(cimat, probs = c(.025, .975), na.rm = TRUE)
    
   }
  
  modList_s17_age_CI[[r]] = ci
  
  netDat = scale(netDat)
  if (specs[r,"nb"] == "NB-Pred") {
    
    cv_fit = cv.glmnet(cbind(netDat, scale(behavDat$Capacity)), scale(behavDat$cAge), alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(cbind(netDat, scale(behavDat$Capacity)), scale(behavDat$cAge), alpha = 0, lambda = opt_lambda)
    
    modList_s17_age[[r]] = mod
    
  } else {
    
    cv_fit = cv.glmnet(netDat, scale(behavDat$cAge), alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(netDat, scale(behavDat$cAge), alpha = 0, lambda = opt_lambda)
    
    modList_s17_age[[r]] = mod
    
  }
  
  
}

bMat_schaefer17 = matrix(c(as.vector(modList_s17_age[[1]]$beta),
                           as.vector(modList_s17_age[[2]]$beta),
                           c(as.vector(modList_s17_age[[3]]$beta),NA),
                           as.vector(modList_s17_age[[4]]$beta),
                           as.vector(modList_s17_age[[5]]$beta)),55,5,byrow = FALSE)

rowMeans(bMat_schaefer17, na.rm=TRUE)

ciMat_schaefer17 = matrix(c(as.vector(modList_s17_age_CI[[1]]),
                          as.vector(modList_s17_age_CI[[2]]),
                          as.vector(modList_s17_age_CI[[3]]),
                          as.vector(modList_s17_age_CI[[4]]),
                          as.vector(modList_s17_age_CI[[5]])),55,10,byrow=FALSE)

rowMeans(ciMat_schaefer17[,c(1,3,5,7,9)], na.rm=TRUE)
rowMeans(ciMat_schaefer17[,c(2,4,6,8,10)], na.rm=TRUE)


##Mixed age models
##Bootstrapped
specs = data.frame(parc = c(rep("ns1", 5)),
                   thresh = c(.5000, .6000, .4000, .5000, .4000),
                   detrend = c(rep("none", 5)),
                   nb = c(rep("NB-Pred", 3), rep("no-NB-Pred",2)),
                   stringsAsFactors = FALSE)

modList_mixed_age = list()
modList_mixed_age_CI = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,"parc"], preproc_bs = specs[r,"detrend"], net_thresh = specs[r,"thresh"])
  #remove outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #set lambdas that we're going to use for ridge regression
  lambdas = 10^seq(3, -2, by = -.01)
  
  cimat = matrix(NaN,dim(netDat)[2]+1,5000)
  
  for (b in 1:5000) {
    
    samp = sample(c(1:70), replace = TRUE)
    
    x = cbind(scale(netDat[samp,]), scale(behavDat$Capacity[samp])); y = scale(behavDat$cAge[samp])
    cv_fit = cv.glmnet(x, y, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(x, y, alpha = 0, lambda = opt_lambda)
    cimat[,b] = as.vector(mod$beta)
    
  }
  
  ci = rowQuantiles(cimat, probs = c(.025, .975), na.rm = TRUE)
  modList_mixed_age_CI[[r]] = ci
  
  cv_fit = cv.glmnet(cbind(scale(netDat), scale(behavDat$Capacity)), scale(behavDat$cAge), alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
  opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
  mod = glmnet(cbind(scale(netDat), scale(behavDat$Capacity)), scale(behavDat$cAge), alpha = 0, lambda = opt_lambda)
  
  
  modList_mixed_age[[r]] = mod
  
  
}

bMat_ns1 = matrix(c(as.vector(modList_mixed_age[[1]]$beta),
                    as.vector(modList_mixed_age[[2]]$beta),
                    as.vector(modList_mixed_age[[3]]$beta),
                    as.vector(modList_mixed_age[[4]]$beta),
                    as.vector(modList_mixed_age[[5]]$beta)),19,5,byrow = FALSE)

rowMeans(bMat_ns1, na.rm=TRUE)

ciMat_ns1 = matrix(c(as.vector(modList_mixed_age_CI[[1]]),
                     as.vector(modList_mixed_age_CI[[2]]),
                     as.vector(modList_mixed_age_CI[[3]]),
                     as.vector(modList_mixed_age_CI[[4]]),
                     as.vector(modList_mixed_age_CI[[5]])),
                     19,10,byrow=FALSE)

rowMeans(ciMat_ns1[,c(1,3,5,7,9)], na.rm=TRUE)
rowMeans(ciMat_ns1[,c(2,4,6,8,10)], na.rm=TRUE)

##Schaefer 17 capacity models
##Bootstrapping!

specs = data.frame(parc = c(rep("schaefer17", 5)),
                   thresh = c(.4000, .4000, .4000, .5000, .6000),
                   detrend = c("detrend", "none", "detrend", rep("none",2)),
                   nb = c(rep("NB-Pred", 2), "no-NB-Pred", rep("NB-Pred", 2)),
                   stringsAsFactors = FALSE)

modList_s17_cap = list()
modList_s17_cap_CI = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,"parc"], preproc_bs = specs[r,"detrend"], net_thresh = specs[r,"thresh"])
  #remove outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #set lambdas that we're going to use for ridge regression
  lambdas = 10^seq(3, -2, by = -.01)
  
  cimat = matrix(NaN,55,5000)
  
  for (b in 1:5000) {
    
    samp = sample(c(1:70), replace = TRUE)
    
    if (specs[r,"nb"] == "NB-Pred") {
      
      x = cbind(scale(netDat[samp,]), scale(behavDat$cAge[samp])); y = scale(behavDat$Capacity[samp])
      cv_fit = cv.glmnet(x,y, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
      opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
      mod = glmnet(x, y, alpha = 0, lambda = opt_lambda)
      cimat[,b] = as.vector(mod$beta)
      
    } else {
      
      x = scale(netDat[samp,]); y = scale(behavDat$Capacity[samp])
      cv_fit = cv.glmnet(x,y, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
      opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
      mod = glmnet(x, y, alpha = 0, lambda = opt_lambda)
      cimat[,b] = c(as.vector(mod$beta),NA)
      
    }
    
    ci = rowQuantiles(cimat, probs = c(.025, .975), na.rm = TRUE)
    
  }
  
  modList_s17_cap_CI[[r]] = ci
  
  netDat = scale(netDat)
  if (specs[r,"nb"] == "NB-Pred") {
    
    cv_fit = cv.glmnet(cbind(netDat, scale(behavDat$cAge)), scale(behavDat$Capacity), alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(cbind(netDat, scale(behavDat$cAge)), scale(behavDat$Capacity), alpha = 0, lambda = opt_lambda)
    
    modList_s17_cap[[r]] = mod
    
  } else {
    
    cv_fit = cv.glmnet(netDat, scale(behavDat$Capacity), alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(netDat, scale(behavDat$Capacity), alpha = 0, lambda = opt_lambda)
    
    modList_s17_cap[[r]] = mod
    
  }
  
  
}

bMat_schaefer17 = matrix(c(as.vector(modList_s17_cap[[1]]$beta),
                           as.vector(modList_s17_cap[[2]]$beta),
                           c(as.vector(modList_s17_cap[[3]]$beta),NA),
                           as.vector(modList_s17_cap[[4]]$beta),
                           as.vector(modList_s17_cap[[5]]$beta)),55,5,byrow = FALSE)

rowMeans(bMat_schaefer17, na.rm=TRUE)

ciMat_schaefer17 = matrix(c(as.vector(modList_s17_cap_CI[[1]]),
                            as.vector(modList_s17_cap_CI[[2]]),
                            as.vector(modList_s17_cap_CI[[3]]),
                            as.vector(modList_s17_cap_CI[[4]]),
                            as.vector(modList_s17_cap_CI[[5]])),55,10,byrow=FALSE)

rowMeans(ciMat_schaefer17[,c(1,3,5,7,9)], na.rm=TRUE)
rowMeans(ciMat_schaefer17[,c(2,4,6,8,10)], na.rm=TRUE)


##Mixed capacity models
##Bootstrapping!
specs = data.frame(parc = c(rep("ns1", 5)),
                   thresh = c(.4000, .6000, .5000, .6000, .4000),
                   detrend = c(rep("none",3),rep("detrend",2)),
                   nb = c(rep("NB-Pred", 3), rep("no-NB-Pred", 2)),
                   stringsAsFactors = FALSE)

modList_mixed_cap = list()
modList_mixed_cap_CI = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,"parc"], preproc_bs = specs[r,"detrend"], net_thresh = specs[r,"thresh"])
  #remove outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #set lambdas that we're going to use for ridge regression
  lambdas = 10^seq(3, -2, by = -.01)
  
  cimat = matrix(NaN,dim(netDat)[2]+1,5000)
  
  for (b in 1:5000) {
    
    samp = sample(c(1:70), replace = TRUE)
    
    x = cbind(scale(netDat[samp,]), scale(behavDat$cAge[samp])); y = scale(behavDat$Capacity[samp])
    cv_fit = cv.glmnet(x, y, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(x, y, alpha = 0, lambda = opt_lambda)
    cimat[,b] = as.vector(mod$beta)
    
  }
  
  ci = rowQuantiles(cimat, probs = c(.025, .975), na.rm = TRUE)
  modList_mixed_cap_CI[[r]] = ci
  
  cv_fit = cv.glmnet(cbind(scale(netDat), scale(behavDat$cAge)), behavDat$Capacity, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
  opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
  mod = glmnet(cbind(scale(netDat), scale(behavDat$cAge)), behavDat$Capacity, alpha = 0, lambda = opt_lambda)
  
  modList_mixed_cap[[r]] = mod
  
  
}

bMat_ns1 = matrix(c(as.vector(modList_mixed_cap[[1]]$beta),
                    as.vector(modList_mixed_cap[[2]]$beta),
                    as.vector(modList_mixed_cap[[3]]$beta),
                    as.vector(modList_mixed_cap[[4]]$beta),
                    as.vector(modList_mixed_cap[[5]]$beta)),19,5,byrow = FALSE)

rowMeans(bMat_ns1, na.rm=TRUE)

ciMat_ns1 = matrix(c(as.vector(modList_mixed_cap_CI[[1]]),
                     as.vector(modList_mixed_cap_CI[[2]]),
                     as.vector(modList_mixed_cap_CI[[3]]),
                     as.vector(modList_mixed_cap_CI[[4]]),
                     as.vector(modList_mixed_cap_CI[[5]])),
                   19,10,byrow=FALSE)

rowMeans(ciMat_ns1[,c(1,3,5,7,9)], na.rm=TRUE)
rowMeans(ciMat_ns1[,c(2,4,6,8,10)], na.rm=TRUE)



#plot histogram of all network metrics
for (c in 1:dim(netDat)[2]) {
  
  hist(netDat[,c], main = sprintf("%s", colnames(netDat)[c]))
  
}












