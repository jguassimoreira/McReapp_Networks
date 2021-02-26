#The packages we will need
library(psych)
library(readxl)
library(readr)
library(igraph)
library(readr)
library(pracma)
library(glmnet)
library(matrixStats)
library(gridExtra)

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

##Set up the conditions of the multiverse analysis
detrend = c("detrend", "none") #linear detrending options of beta series
parcellation = c("schaefer7", "schaefer17") #parcellation options
thresh = c(.4000, .5000, .6000) #threshold options
cap_rmse = c() #vector to hold root mean square error for  capacity
analysis_id = c() #vector to keep track of which analysis corresponds to which fit metric in the output vector c

##Loop over each of the options above to begin the multiverse analysis
for (d in detrend) {
  for (p in parcellation) {
    for (t in thresh) {
      
      #Update the analysis ID vector
      analysis_id = c(analysis_id,
                      paste(p, as.character(t), d, "no-NB-Pred", sep = "_"))
      
      #Preprocess the network data
      netDat = procDat(subs, parc = p, preproc_bs = d, net_thresh = t)
      #handle outliers by winsorizing
      netDat = winzOutlier(netDat) 
      #scale predictors ahead of analysis
      netDat = scale(netDat)
      
      #set lambdas that we're going to use for ridge regression
      lambdas = 10^seq(3, -2, by = -.01)
      
      #predicting capacity
      mod_fit_cap_1 = runRidge(cbind(netDat, scale(behavDat$cAge)), scale(behavDat$Capacity), lambdas)
      cap_rmse = c(cap_rmse, mod_fit_cap_1[[1]])
      
      print(paste(p, as.character(t), d, "NB-Pred", sep = "_"))
      
    }
    
    
  }
  
  
}

#sort(cap_rmse)
#[1] 0.8266603 0.8852656 0.9298616 0.9395251 0.9455366 1.1191973 1.3176497 1.3475500 1.5665008 1.6606826 1.6607053 1.6607615

#analysis_id[order(cap_rmse)]
#[1] "schaefer17_0.4_detrend_no-NB-Pred" "schaefer17_0.6_detrend_no-NB-Pred" "schaefer17_0.4_none_no-NB-Pred"   
#[4] "schaefer17_0.5_none_no-NB-Pred"    "schaefer17_0.6_none_no-NB-Pred"    "schaefer17_0.5_detrend_no-NB-Pred"
#[7] "schaefer7_0.4_none_no-NB-Pred"     "schaefer7_0.5_none_no-NB-Pred"     "schaefer7_0.6_none_no-NB-Pred"    
#[10] "schaefer7_0.4_detrend_no-NB-Pred"  "schaefer7_0.6_detrend_no-NB-Pred"  "schaefer7_0.5_detrend_no-NB-Pred" 


##Schaefer 17 capacity model
specs = data.frame(parc = "schaefer17",
                   thresh = .4000,
                   detrend = "detrend",
                   nb = "NB-Pred",
                   stringsAsFactors = FALSE)

modList_s17_cap = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,"parc"], preproc_bs = specs[r,"detrend"], net_thresh = specs[r,"thresh"])
  #handle outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #scale predictors ahead of analysis
  netDat = scale(netDat)
  #set lambdas that we're going to use for ridge regression
  lambdas = 10^seq(3, -2, by = -.01)
  
  
  if (specs[r,"nb"] == "NB-Pred") {
    
    cv_fit = cv.glmnet(cbind(netDat, scale(behavDat$cAge)), behavDat$Capacity, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(cbind(netDat, scale(behavDat$cAge)), behavDat$Capacity, alpha = 0, lambda = opt_lambda)
    
    modList_s17_cap[[r]] = mod
    
  } else {
    
    cv_fit = cv.glmnet(netDat, behavDat$Capacity, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(netDat, behavDat$Capacity, alpha = 0, lambda = opt_lambda)
    
    modList_s17_cap[[r]] = mod
    
  }
  
}

##Schaefer 17 capacity models
##Bootstrapping to get confidence intervals

modList_s17_cap_CI = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,"parc"], preproc_bs = specs[r,"detrend"], net_thresh = specs[r,"thresh"])
  #handle outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #set lambdas that we're going to use for ridge regression
  lambdas = 10^seq(3, -2, by = -.01)
  
  cimat = matrix(NaN,55,5000)
  
  for (b in 1:5000) {
    
    set.seed(b)
    
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
  
}

bMat_schaefer17 = matrix(c(as.vector(modList_s17_cap[[1]]$beta)),55,1,byrow = FALSE)


##semi-partial and partial correlations to estimate utility of brain predictors
##As well as OLS regression

netDat = as.data.frame(netDat)

#OLS regression
summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))


#squared semipartial correlation (unique variance accounted for)
age.res = as.vector(lm(behavDat$cAge ~ netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB)$residuals)
simConC.res = as.vector(lm(netDat$simCoNC ~ behavDat$cAge + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB)$residuals)
simLiNB.res = as.vector(lm(netDat$simLiNB ~ behavDat$cAge + netDat$simCoNC + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB)$residuals)
modCoNC.res = as.vector(lm(netDat$modCoNC ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modDMNB + netDat$comCoNB)$residuals)
modDMNB.res = as.vector(lm(netDat$modDMNB ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$comCoNB)$residuals)
comCoNB.res = as.vector(lm(netDat$comCoNB ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB)$residuals)

cor(age.res, behavDat$Capacity)[1]^2
cor(simConC.res, behavDat$Capacity)[1]^2
cor(simLiNB.res, behavDat$Capacity)[1]^2
cor(modCoNC.res, behavDat$Capacity)[1]^2
cor(modDMNB.res, behavDat$Capacity)[1]^2
cor(comCoNB.res, behavDat$Capacity)[1]^2

#check by looking at R.square
#age
summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))$r.squared -
summary(lm(behavDat$Capacity ~ netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))$r.squared

#simCoNC
summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))$r.squared -
summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))$r.squared

#simLiNB
summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))$r.squared -
  summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))$r.squared

#modCoNC
summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))$r.squared -
  summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modDMNB + netDat$comCoNB))$r.squared

#ModDMNB
summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))$r.squared -
  summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$comCoNB))$r.squared

#comCoNB
summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB + netDat$comCoNB))$r.squared -
  summary(lm(behavDat$Capacity ~ behavDat$cAge + netDat$simCoNC + netDat$simLiNB + netDat$modCoNC + netDat$modDMNB))$r.squared


psych::corr.test(netDat[,c("simCoNA", "simCoNB", "simCoNC")])
#Correlation matrix 
#simCoNA simCoNB simCoNC
#simCoNA    1.00    0.40    0.26
#simCoNB    0.40    1.00    0.27
#simCoNC    0.26    0.27    1.00

psych::corr.test(netDat[,c("comCoNA", "comCoNB", "comCoNC")])
#Correlation matrix 
#comCoNA comCoNB comCoNC
#comCoNA    1.00   -0.07   -0.02
#comCoNB   -0.07    1.00    0.26
#comCoNC   -0.02    0.26    1.00

psych::corr.test(netDat[,c("modCoNA", "modCoNB", "modCoNC")])
#Correlation matrix 
#modCoNA modCoNB modCoNC
#modCoNA    1.00    0.38    0.22
#modCoNB    0.38    1.00    0.11
#modCoNC    0.22    0.11    1.00



