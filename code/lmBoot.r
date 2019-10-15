lmBoot <- function(inputData, nBoot){
  
  
  #argument 
  
  for(i in 1:nBoot){
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    
    # fit the model under this alternative reality
    bootLM <- lm(Age ~ Weight, data = bootData)
    
    # store the coefs
    if(i == 1){
      bootResults <- matrix(coef(bootLM), ncol = 2)
      } else {
      bootResults<- rbind(bootResults, matrix(coef(bootLM), ncol = 2))
    }
    
  } # end of i loop
  bootResults
}

lmBoot(fitness,14)


ggg <- function(n){
   return(n*n)
}
