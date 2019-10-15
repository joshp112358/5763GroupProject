speededBootstrap <- function(inputData, num_var,formula, nBoots){
  mat <- matrix(0L , nrow = nBoots, ncol = num_var)
  for(i in 1:nBoots){
    #randomly sample data
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    bootLM <- lm(formula, data = bootData)
    # store the coefs
    mat[i,] <- matrix(coef(bootLM), ncol = num_var)
  } # end of i loop
  return(mat)
}


#import fitness data 
coefficients <- speededBootstrap(fitness,3,Age~Weight+Oxygen,7)

for(i in 1:3){
  #plot histogram of the second coefficient
  hist(coefficients[,i])
  #get quantiles
  print(quantile(coefficients[,i], probs = c(0.025,0.975)))
}

#get microbenchmark
library(microbenchmark)
microbenchmark(baselineBootstrap(fitness,3,Age~Weight+Oxygen,100))
