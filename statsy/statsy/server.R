library(shiny)

server <- function(input, output) {

  
  output$hist <- renderPlot({
    inFile <- input$data
    data <- read.csv(inFile$datapath)
    number_of_variables <- input$num_of_var
    form <- input$formula
    n <- input$num_of_boots

    
    # Helper Function
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
    coefficients1 <- speedyBoot(inputData = data,
                                num_var = number_of_variables,
                                formula = form,
                                nBoots = n)
    hist(coefficients1[,1])
  })
}
