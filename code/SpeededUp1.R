number_of_variables <- 3
form <- Age~Weight+Oxygen
n = 7


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
coefficients <- speededBootstrap(fitness
                                 ,number_of_variables
                                 ,form
                                 ,n)

get_quantiles <- function(coeff, num_var){
  for(i in 1:num_var){
    #get quantiles
    #how does one accesss the index of a quantile
    print(quantile(coeff[,i], probs = c(0.025,0.975)))
  }
}

# get quantiles
get_quantiles(coefficients, number_of_variables)

