library(shiny)

server <- function(input, output) {
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
  
  coefficients2 <-reactive({
        inFile <- input$data
        data <- read.csv(inFile$datapath)
        number_of_variables <- input$num_of_var
        form <- input$formula
        n <- input$num_of_boots
        set.seed(9)
        speedyBoot(inputData = data,
                             num_var = number_of_variables,
                             formula = form,
                             nBoots = n)})
  
  
  
  output$mean <- renderText(
    {
      number_of_variables <- input$num_of_var
      holder = numeric(number_of_variables)
      for(i in 1:number_of_variables){
        holder[i]<-mean(coefficients2()[,i])
      }
    print(holder);
    })
  
  output$table <- renderTable({
    number_of_variables <- input$num_of_var
      get_quantiles <- function(coeff, num_var){
        mat <- matrix(0L, nrow = 2, ncol = num_var)
        for(i in 1:num_var){
          mat[,i]<-matrix(quantile(coeff[,i], probs = c(0.025,0.975)))
        }
        #print(mat)
        rownames(mat)=c("2.5% Quartile","97.5% Quartiles")
        return(mat)
      }
      
      get_quantiles(coefficients2(),number_of_variables)

    },
    rownames = TRUE
  )
  
  output$hist1 <- renderPlot({
    hist(coefficients2()[,1],
         main = "Intercept Value",
         xlab = "Bootstrap Values")
    
    
    })
  output$hist2 <- renderPlot({
    
    hist(coefficients2()[,2],
         main = "Coefficient 1",
         xlab = "Bootstrap Values")
    
  })
  output$hist3 <- renderPlot({
    
    hist(coefficients2()[,3],
         main = "Coefficient 3",
         xlab = "Bootstrap Values")
    
  })
  output$hist4 <- renderPlot({
    
    hist(coefficients2()[,4],
         main = "Coefficient 4",
         xlab = "Bootstrap Values")
    
  })
  output$hist5 <- renderPlot({
    
    hist(coefficients2()[,5],
         main = "Coefficient 5",
         xlab = "Bootstrap Values")
    
  })
}
