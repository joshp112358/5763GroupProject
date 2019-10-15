baselineBootstrap <- function(inputData, num_var,formula, nBoots){
  library(foreach)
  library(parallel)
  
  #randomly sample data
  
  bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
  
  bootLM <- lm(formula, data = bootData)
  
  # store the coefs
  #a parallel version
  bootResults <- foreach(i = 1:nBoots, .combine = rbind) %dopar% coef(bootLM)
  
  return(bootResults)
  
}

coefficients <- baselineBootstrap(fitness,3,Age~Weight+Oxygen,100)
library(microbenchmark)


microbenchmark(baselineBootstrap(fitness,3,Age~Weight+Oxygen,100))


