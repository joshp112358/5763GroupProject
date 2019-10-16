rboot <- function(inputData, num_var,formula,nBoots){
  library(foreach)
  foreach(i = 1:nBoots,
          .combine = "rbind") %dopar% {
            bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
            bootLM <- lm(formula,data=bootData)
            bootLM$coef
          }
 }

r <- rboot(fitness,3,Age~Weight+Oxygen,200)

library(microbenchmark)
microbenchmark(baselineBootstrap(fitness,3,Age~Weight+Oxygen,200),r <- rboot(fitness,3,Age~Weight+Oxygen,200))
