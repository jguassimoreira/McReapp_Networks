library(psych)
library(readxl)
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
parcellation = c("schaefer7", "schaefer17", "ns1", "ns2") #parcellation options
thresh = c(.4000, .5000, .6000) #threshold options
age_rmse = c(); cap_rmse = c() #vectors to hold root mean square error for age and capacity
age_BIC = c(); cap_BIC = c()#vectors to hold BIC for age and capacity
age_mae = c(); cap_mae = c() #vectors to hold MAE for age and capacity
analysis_id = c() #vector to keep track of which analysis corresponds to which fit metric in the output vector c

##Loop over each of the options above to begin the multiverse analysis
for (d in detrend) {
  for (p in parcellation) {
    for (t in thresh) {
      
      #Update the analysis ID vector
      analysis_id = c(analysis_id,
                      paste(p, as.character(t), d, "no-NB-Pred", sep = "_"),
                      paste(p, as.character(t), d, "NB-Pred", sep = "_"))
      
      #Preprocess the network data
      netDat = procDat(subs, parc = p, preproc_bs = d, net_thresh = t)
      #remove outliers by winsorizing
      netDat = winzOutlier(netDat) 
      #scale predictors ahead of analysis
      netDat = scale(netDat)
      
      #set lambdas that we're going to use for ridge regression
      lambdas = 10^seq(3, -2, by = -.01)
      
      #run models, with and without non-brain predictors
      #predicting age
      mod_fit_age_1 = runRidge(netDat, scale(behavDat$cAge), lambdas)
      mod_fit_age_2 = runRidge(cbind(netDat, scale(behavDat$Capacity)), scale(behavDat$cAge), lambdas)
      age_rmse = c(age_rmse, mod_fit_age_1[[1]], mod_fit_age_2[[1]])
      age_BIC = c(age_BIC, mod_fit_age_1[[3]], mod_fit_age_2[[3]])
      age_mae = c(age_mae, mod_fit_age_1[[4]], mod_fit_age_2[[4]])
      
      #predicting capacity
      mod_fit_cap_1 = runRidge(netDat, scale(behavDat$Capacity), lambdas)
      mod_fit_cap_2 = runRidge(cbind(netDat, scale(behavDat$cAge)), scale(behavDat$Capacity), lambdas)
      cap_rmse = c(cap_rmse, mod_fit_cap_1[[1]], mod_fit_cap_2[[1]])
      cap_BIC = c(cap_BIC, mod_fit_cap_1[[3]], mod_fit_cap_2[[3]])
      cap_mae = c(cap_mae, mod_fit_cap_1[[4]], mod_fit_cap_2[[4]])
      
      print(paste(p, as.character(t), d, "no-NB-Pred", sep = "_"))
      print(paste(p, as.character(t), d, "NB-Pred", sep = "_"))
      
    }
    
    
  }
  
  
}

library(ggplot2)

rmse = data.frame(ageRMSE = age_rmse, capRMSE = cap_rmse, num = 1:length(age_rmse))

##Age
plot(sort(age_rmse), pch = 20, xlab = "Analyses", ylab = "Root Mean Square Error", main = "Specification Curve (Age)")
#sort(age_rmse)
#[1] 0.9640737 1.0247624 1.0322768 1.0993674 1.1020104 1.1159951 1.1191409 1.1191975 1.1268487 1.1294982 1.1296144 1.1296714 1.4869653
#[14] 1.5157774 1.5676087 1.5691289 1.5691539 1.5691643 1.5691650 1.5692538 1.5937985 1.5981043 1.5981474 1.5981730 1.5981845 1.5981983
#[27] 1.5982747 1.6001678 1.6199464 1.6607667 1.6950324 1.6950595 1.6951491 1.6951689 1.6951979 1.6952515 1.7393739 1.7430106 1.8956239
#[40] 1.9051290 1.9051756 1.9052524 1.9574477 1.9575937 1.9576095 1.9576211 1.9576573 1.9577363
analysis_id[order(age_rmse)][1:5]
plot(sort(age_BIC), pch = 20, xlab = "Analyses", ylab = "BIC", main = "Specification Curve (Age)")
analysis_id[order(age_BIC)][1:5]
plot(sort(age_mae), pch = 20, xlab = "Analyses", ylab = "Mean Absolute Error", main = "Specification Curve (Age)")
analysis_id[order(age_mae)][1:5]

aplot = ggplot(rmse, aes(x=num, y = sort(ageRMSE))) + geom_point(aes(), fill = "gray", color = 'black', pch=21, size = 5) + labs(x = "Models", y = "Root Mean Square Error", title = "Age Specification Curve") + theme(text = element_text(size = 15))
cplot = ggplot(rmse, aes(x=num, y = sort(capRMSE))) + geom_point(aes(), fill = "gray", color = 'black', pch=21, size = 5) + labs(x = "Models", y = "Root Mean Square Error", title = "Ability Specification Curve") + theme(text = element_text(size = 15))
grid.arrange(aplot, cplot, ncol=2)

ggsave(file.path("C:", "Users", "jguas", "Documents", "Publications & Posters", "Papers", "2018", "McReapp_Net_Sim", "JNeuro", "SecondSub", "age_speccurve.png"), plot = aplot, dpi = 900, width = 7.32, height = 5.11, units = c("in"))
ggsave(file.path("C:", "Users", "jguas", "Documents", "Publications & Posters", "Papers", "2018", "McReapp_Net_Sim", "JNeuro", "SecondSub", "cap_speccurve.png"), plot = cplot, dpi = 900, width = 7.32, height = 5.11, units = c("in"))
ggsave(file.path("C:", "Users", "jguas", "Documents", "Publications & Posters", "Papers", "2018", "McReapp_Net_Sim", "JNeuro", "SecondSub", "spec_curves.png"), arrangeGrob(aplot, cplot), dpi = 900, width = 12.75, height = 8, units = c("in"))

##Cap
plot(sort(cap_rmse), pch = 20, xlab = "Analyses", ylab = "Root Mean Square Error", main = "Specification Curve (Ability)")
#sort(cap_rmse)
#[1] 0.8814798 0.8932871 0.8933022 0.9730053 1.0113278 1.0140241 1.0502288 1.0832202 1.1191973 1.1253214 1.1257268 1.1296715 1.2167978
#[14] 1.2590956 1.3299145 1.3634978 1.3819683 1.3853775 1.4264974 1.4639328 1.4796586 1.4932527 1.5060406 1.5178419 1.5432379 1.5697833
#[27] 1.5980205 1.5980513 1.5981038 1.5981122 1.6155668 1.6262343 1.6281925 1.6311750 1.6606826 1.6607615 1.6640987 1.6951656 1.6951888
#[40] 1.6952462 1.7098095 1.7607622 1.7784966 1.8967970 1.9573952 1.9574453 1.9574816 1.9574909
analysis_id[order(cap_rmse)][1:5]
plot(sort(cap_BIC), pch = 20, xlab = "Analyses", ylab = "BIC", main = "Specification Curve (Ability)")
analysis_id[order(cap_BIC)][1:5]
plot(sort(cap_mae), pch = 20, xlab = "Analyses", ylab = "Mean Absolute Error", main = "Specification Curve (Ability)")
analysis_id[order(cap_mae)][1:5]


##Schaefer 17 age models
specs = data.frame(parc = c(rep("schaefer17", 5)),
                   thresh = c(.4000, .5000, .6000, .5000, .4000),
                   detrend = c(rep("detrend",3), rep("none",2)),
                   nb = c(rep("NB-Pred", 2), "no-NB-Pred", rep("NB-Pred", 2)),
                   stringsAsFactors = FALSE)

modList_s17_age = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,"parc"], preproc_bs = specs[r,"detrend"], net_thresh = specs[r,"thresh"])
  #remove outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #scale predictors ahead of analysis
  netDat = scale(netDat)
  #set lambdas that we're going to use for ridge regression
  lambdas = 10^seq(3, -2, by = -.01)
  
  
  if (specs[r,"nb"] == "NB-Pred") {
    
    cv_fit = cv.glmnet(cbind(netDat, scale(behavDat$Capacity)), behavDat$cAge, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(cbind(netDat, scale(behavDat$Capacity)), behavDat$cAge, alpha = 0, lambda = opt_lambda)
    
    modList_s17_age[[r]] = mod
    
  } else {
    
    cv_fit = cv.glmnet(netDat, behavDat$cAge, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
    opt_lambda <- cv_fit$lambda.min #get optimal lambda, according to CV
    mod = glmnet(netDat, behavDat$cAge, alpha = 0, lambda = opt_lambda)
    
    modList_s17_age[[r]] = mod
    
  }
  
}

bMat_schaefer17 = matrix(c(as.vector(modList_s17_age[[1]]$beta),
          as.vector(modList_s17_age[[2]]$beta),
          c(as.vector(modList_s17_age[[3]]$beta),NA),
          as.vector(modList_s17_age[[4]]$beta),
          as.vector(modList_s17_age[[5]]$beta)),55,5,byrow = FALSE)

rowMeans(scale(bMat_schaefer17), na.rm=TRUE)

##Schaefer 17 age models
##Bootstrapping!
specs = data.frame(parc = c(rep("schaefer17", 5)),
                   thresh = c(.4000, .5000, .6000, .5000, .4000),
                   detrend = c(rep("detrend",3), rep("none",2)),
                   nb = c(rep("NB-Pred", 2), "no-NB-Pred", rep("NB-Pred", 2)),
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



##Schaefer 17 capacity models
specs = data.frame(parc = c(rep("schaefer17", 5)),
                   thresh = c(.4000, .6000, .5000, .4000, .4000),
                   detrend = c("detrend", rep("none",3), "detrend"),
                   nb = c(rep("NB-Pred", 4), "no-NB-Pred"),
                   stringsAsFactors = FALSE)

modList_s17_cap = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,"parc"], preproc_bs = specs[r,"detrend"], net_thresh = specs[r,"thresh"])
  #remove outliers by winsorizing
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

bMat_schaefer17 = matrix(c(as.vector(modList_s17_cap[[1]]$beta),
                           as.vector(modList_s17_cap[[2]]$beta),
                           as.vector(modList_s17_cap[[3]]$beta),
                           as.vector(modList_s17_cap[[4]]$beta),
                           c(as.vector(modList_s17_cap[[5]]$beta),NA)),55,5,byrow = FALSE)

rowMeans(scale(bMat_schaefer17), na.rm=TRUE)

##Schaefer 17 capacity models
##Bootstrapping!

specs = data.frame(parc = c(rep("schaefer17", 5)),
                   thresh = c(.4000, .6000, .5000, .4000, .4000),
                   detrend = c("detrend", rep("none",3), "detrend"),
                   nb = c(rep("NB-Pred", 4), "no-NB-Pred"),
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
                           as.vector(modList_s17_cap[[3]]$beta),
                           as.vector(modList_s17_cap[[4]]$beta),
                           c(as.vector(modList_s17_cap[[5]]$beta),NA)),55,5,byrow = FALSE)

rowMeans(bMat_schaefer17, na.rm=TRUE)

ciMat_schaefer17 = matrix(c(as.vector(modList_s17_cap_CI[[1]]),
                            as.vector(modList_s17_cap_CI[[2]]),
                            as.vector(modList_s17_cap_CI[[3]]),
                            as.vector(modList_s17_cap_CI[[4]]),
                            as.vector(modList_s17_cap_CI[[5]])),55,10,byrow=FALSE)

rowMeans(ciMat_schaefer17[,c(1,3,5,7,9)], na.rm=TRUE)
rowMeans(ciMat_schaefer17[,c(2,4,6,8,10)], na.rm=TRUE)


##semi-partial and partial correlations to estimate utility of brain predictors
##As well as moderation analyses
specs = data.frame(parc = c(rep("schaefer17", 5)),
                   thresh = c(.4000, .6000, .5000, .4000, .4000),
                   detrend = c("detrend", rep("none",3), "detrend"),
                   nb = c(rep("NB-Pred", 4), "no-NB-Pred"),
                   stringsAsFactors = FALSE)

datList_s17 = list()

for (r in 1:dim(specs)[1]) {
  
  #Preprocess the network data
  netDat = procDat(subs, parc = specs[r,"parc"], preproc_bs = specs[r,"detrend"], net_thresh = specs[r,"thresh"])
  #remove outliers by winsorizing
  netDat = winzOutlier(netDat) 
  #scale predictors ahead of analysis
  netDat = scale(netDat)

  datList_s17[[r]] = netDat
  
}


avgNetMat = (datList_s17[[1]] + datList_s17[[2]] + datList_s17[[3]] + datList_s17[[4]] + datList_s17[[5]])/5


subNet = as.data.frame(avgNetMat[,c('simCoNC', 'modDMNB')])
subNet$ability = scale(behavDat$Capacity)
subNet$age = scale(behavDat$cAge)

#moderation
lmMod = lm(ability ~ age*simCoNC + age*modDMNB, data=subNet)

#semipartial correlation
age.dmn_con = as.vector(lm(age ~ modDMNB + simCoNC, data = subNet)$residuals)
dmn.age_con = as.vector(lm(modDMNB ~ age + simCoNC, data = subNet)$residuals)
con.age_dmn = as.vector(lm(simCoNC ~ age + modDMNB, data = subNet)$residuals)

cor(age.dmn_con, subNet$ability)[1]^2
cor(dmn.age_con, subNet$ability)[1]^2
cor(con.age_dmn, subNet$ability)[1]^2

#check by looking at R.square
r = summary(lm(ability ~ modDMNB + simCoNC, data = subNet))$r.squared
f = summary(lm(ability ~ modDMNB + simCoNC + age, data = subNet))$r.squared
f-r

r = summary(lm(ability ~ age + simCoNC, data = subNet))$r.squared
f = summary(lm(ability ~ age + simCoNC + modDMNB, data = subNet))$r.squared
f-r

r = summary(lm(ability ~ modDMNB + age, data = subNet))$r.squared
f = summary(lm(ability ~ modDMNB + age + simCoNC, data = subNet))$r.squared
f-r

#look at the brain predictors as a set
r = summary(lm(ability ~ age, data = subNet))$r.squared
f = summary(lm(ability ~ modDMNB + simCoNC + age, data = subNet))$r.squared
f-r






