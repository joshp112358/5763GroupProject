baselineBootstrap <- function(inputData, num_var,formula, nBoots){
  
  for(i in 1:nBoots){
    #randomly sample data
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    bootLM <- lm(formula, data = bootData)
    # store the coefs
    #for optimization put 
    if(i == 1){
      bootResults <- matrix(coef(bootLM), ncol = num_var)
    } else {
      bootResults<- rbind(bootResults, matrix(coef(bootLM), ncol = num_var))
    }
  } # end of i loop
  return(bootResults)
}


#import fitness data 
coefficients <- baselineBootstrap(fitness,3,Age~Weight+Oxygen,100)

#plot histogram of the second coefficient
hist(coefficients[,2])
#get quantiles
quantile(coefficients[,2], probs = c(0.025,0.975))

#get microbenchmark
library(microbenchmark)
microbenchmark(baselineBootstrap(fitness,3,Age~Weight+Oxygen,100))
