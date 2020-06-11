runRidge = function(x, y, lambdas) {
  
  x = as.matrix(x)
  #will assume that capacity and age are already in the dataframe
  cv_fit = cv.glmnet(x, y, alpha = 0, lambda = lambdas) #alpha needs to be zero in order to it to be ridge regression! 1 = LASSO
  opt_lambda = cv_fit$lambda.min #get optimal lambda, according to CV
  mod = glmnet(x, y, alpha = 0, lambda = opt_lambda) #run the model
  b0 = mod$a0; betas = mod$beta #pull out the necessary parameters
  
  #compute fitted value
  fitted_val = (x %*% betas) + b0
  #calculate RMSE
  #mse = mean((y - fitted_val)^2)
  mae = mean(abs(y - fitted_val))
  sse = sum((y - fitted_val)^2)
  mse = sse/mod$df
  rmse = sqrt(mse)
  IC = glmnetIC(mod)
  
  return(list(rmse, IC[[1]], IC[[2]], mae))
  
}