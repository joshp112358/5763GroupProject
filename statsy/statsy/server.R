library(shiny)

server <- function(input, output) {
  output$mean <- renderText(
    {inFile <- input$data
    data <- read.csv(inFile$datapath)
    number_of_variables <- input$num_of_var
    form <- input$formula
    n <- input$num_of_boots
    set.seed(9)
    
    speedyBoot <- function(inputData, num_var,formula, nBoots){
      mat <- matrix(0L, nrow = nBoots, ncol = num_var)
      for(i in 1:nBoots){
        bootData <- inputData[sample(nrow(inputData), nrow(inputData), replace = T),]
        bootLM <- lm(formula, data = bootData)
        # store the coefs
        mat[i,] <- coef(bootLM)
      } # end of i loop
      return(mat)
    }
    
    coefficients2 <-speedyBoot(inputData = data,
                               num_var = number_of_variables,
                               formula = form,
                               nBoots = n)
    mean1<-mean(coefficients2[,1])
    mean2<-mean(coefficients2[,2])
    print(paste("intercept", mean1,"coeff1", mean2));

    })
  
  output$table <- renderTable({
    inFile <- input$data
    data <- read.csv(inFile$datapath)
    number_of_variables <- input$num_of_var
    form <- input$formula
    n <- input$num_of_boots
    set.seed(9)
    
    speedyBoot <- function(inputData, num_var,formula, nBoots){
      mat <- matrix(0L, nrow = nBoots, ncol = num_var)
      for(i in 1:nBoots){
        bootData <- inputData[sample(nrow(inputData), nrow(inputData), replace = T),]
        bootLM <- lm(formula, data = bootData)
        # store the coefs
        mat[i,] <- coef(bootLM)
      } # end of i loop
      return(mat)
    }
    
    coefficients2 <-speedyBoot(inputData = data,
                               num_var = number_of_variables,
                               formula = form,
                               nBoots = n)
      
      get_quantiles <- function(coeff, num_var){
        mat <- matrix(0L, nrow = 2, ncol = num_var)
        for(i in 1:num_var){
          mat[,i]<-matrix(quantile(coeff[,i], probs = c(0.025,0.975)))
        }
        #print(mat)
        return(mat)
      }
      
      get_quantiles(coefficients2,number_of_variables)

    }
  )
  
  output$hist1 <- renderPlot({
    inFile <- input$data
    data <- read.csv(inFile$datapath)
    number_of_variables <- input$num_of_var
    form <- input$formula
    n <- input$num_of_boots
    
    # Helper Function
    set.seed(9)
    speedyBoot <- function(inputData, num_var,formula, nBoots){
      mat <- matrix(0L, nrow = nBoots, ncol = num_var)
      for(i in 1:nBoots){
        bootData <- inputData[sample(nrow(inputData), nrow(inputData), replace = T),]
        bootLM <- lm(formula, data = bootData)
        # store the coefs
        mat[i,] <- coef(bootLM)
      } # end of i loop
      return(mat)
    }
    coefficients2 <-speedyBoot(inputData = data,
                               num_var = number_of_variables,
                               formula = form,
                               nBoots = n)
    
    
    hist(coefficients2[,1])
    
    })
  output$hist2 <- renderPlot({
    inFile <- input$data
    data <- read.csv(inFile$datapath)
    number_of_variables <- input$num_of_var
    form <- input$formula
    n <- input$num_of_boots
    
    # Helper Function
    set.seed(9)
    speedyBoot <- function(inputData, num_var,formula, nBoots){
      mat <- matrix(0L, nrow = nBoots, ncol = num_var)
      for(i in 1:nBoots){
        bootData <- inputData[sample(nrow(inputData), nrow(inputData), replace = T),]
        bootLM <- lm(formula, data = bootData)
        # store the coefs
        mat[i,] <- coef(bootLM)
      } # end of i loop
      return(mat)
    }
    coefficients2 <-speedyBoot(inputData = data,
                               num_var = number_of_variables,
                               formula = form,
                               nBoots = n)
    
    
    hist(coefficients2[,2])
    
  })
}
