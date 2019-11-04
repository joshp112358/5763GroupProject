
library(shiny)
library(leaflet)
library(htmlwidgets)
library(owmr)
library(whisker)
library(shinyjs)
library(V8)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  # App title ----
  titlePanel("Bootstrap App"),  
  
  sidebarPanel("This function will accept an arbitrary number of covariates,
                 but requires you to type in the formula and only display coefficients
                 for the first five terms",
               fileInput(inputId = "data",
                         label = "Input CSV Below by Pressing Browse",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                         buttonLabel = "Browse..."),
               textInput(inputId = "formula",
                         label = "Linear Regression Formula in R format",
                         value = "Age~Weight"),
               numericInput(inputId = "num_of_var",
                            label = "Number of Variables (Covariates + 1)",
                            value = 2),
               numericInput(inputId = "num_of_boots",
                            label = "Number of Bootstraps",
                            value = 1000),
  ),
  mainPanel(
    tabsetPanel(id = "tab_being_displayed", # will set input$tab_being_displayed
        tabPanel("Summary", plotOutput("summary"),
                 "Mean overview",
                 uiOutput("mean"),
                 "Confidence Intervals",
                 uiOutput("table")),
        tabPanel("Plot", verbatimTextOutput("plot"),
                 "Bootstrapping visualisaiton",
                 plotOutput("hist1"),
                 plotOutput("hist2"),
                 plotOutput("hist3"),
                 plotOutput("hist4"),
                 plotOutput("hist5")),
        tabPanel("Weather",
                 "Here, you can select a city to check the weather real time.
                   You are able to zoom in or zoom out. You can check the exact temperature
                   of a city by clicking on the weather icon.",
                 br(),
                 shinyjs::useShinyjs(),
                 shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                 actionButton("refresh", "Refresh"),
                 br(),
                 leafletOutput("map", width = "600px", height = "600px"),
        ),
        selected = "Summary"
  )
           
  )
)

# Server ------------------------------------------------------------------

library(shiny)


server <- function(input, output, session) {
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
  

  
  output$mean <- renderTable(
    {
      number_of_variables <- input$num_of_var
      mat_mean <- matrix(0L, nrow = 1, ncol = number_of_variables)
  
      for(i in 1:number_of_variables){
              mat_mean[1,i]<-round(mean(coefficients2()[,i]),2)
              }
      return(mat_mean)

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
  
  library(ggplot2)
  
  output$hist1 <- renderPlot({    
    mx<-mean(coefficients2()[,1])
    ggplot(data= as.data.frame(coefficients2() ), aes(x = coefficients2()[,1])) + geom_histogram(col="black", fill="blue", alpha = .2)  +
      #format histogram
      labs(x="Proportions", y="Frequency") +
      ggtitle("Intercept (V1)") +
      theme(plot.title = element_text(size = 18)) + 
      
      # add line for mean
      geom_vline(aes(xintercept = mx),color ="red",size=1.5) +
      
      #add line for 0.025 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,1], probs = 0.025)) , colour = "green", size = 1) +
      
      #add line for 0.975 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,1], probs = 0.975)) , colour = "green", size = 1) +
      
      # add text for mean
      geom_text(aes(x=mx), y=0, label= round(mx,2), size=4, angle=0, vjust=1, hjust= 0) +
      
      # add text for  0.025 quantile
      geom_text(aes(x=quantile(coefficients2()[,1], probs = 0.025), y=0, 
                    label= round(quantile(coefficients2()[,1], probs = 0.025),2), 
                    angle=0, vjust=1, hjust= 0)) +
      
      # add text for  0.975 quantile
      geom_text(aes(x=quantile(coefficients2()[,1], probs = 0.975), y=0, 
                    label= round(quantile(coefficients2()[,1], probs = 0.975), 2), 
                    angle=0, vjust=1, hjust= 0))   
    
  })
  
  
  output$hist2 <- renderPlot({    
    mx<-mean(coefficients2()[,2])
    ggplot(data= as.data.frame(coefficients2() ), aes(x = coefficients2()[,2])) + geom_histogram(col="black", fill="blue", alpha = .2)  +
      #format histogram
      labs(x="Proportions", y="Frequency") +
      ggtitle("Covariate 1 (V2)") +
      theme(plot.title = element_text(size = 18)) + 
      
      # add line for mean
      geom_vline(aes(xintercept = mx),color ="red",size=1.5) +
      
      #add line for 0.025 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,2], probs = 0.025)) , colour = "green", size = 1) +
      
      #add line for 0.975 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,2], probs = 0.975)) , colour = "green", size = 1) +
      
      # add text for mean
      geom_text(aes(x=mx), y=0, label= round(mx,2), size=4, angle=0, vjust=1, hjust= 0) +
      
      # add text for  0.025 quantile
      geom_text(aes(x=quantile(coefficients2()[,2], probs = 0.025), y=0, 
                    label= round(quantile(coefficients2()[,2], probs = 0.025),2), 
                    angle=0, vjust=1, hjust= 0)) +
      
      # add text for  0.975 quantile
      geom_text(aes(x=quantile(coefficients2()[,2], probs = 0.975), y=0, 
                    label= round(quantile(coefficients2()[,2], probs = 0.975), 2), 
                    angle=0, vjust=1, hjust= 0))   
    
  })
  
  
  output$hist3 <- renderPlot({    
    mx<-mean(coefficients2()[,3])
    ggplot(data= as.data.frame(coefficients2() ), aes(x = coefficients2()[,3])) + geom_histogram(col="black", fill="blue", alpha = .2)  +
      #format histogram
      labs(x="Proportions", y="Frequency") +
      ggtitle("Covariate 2 (V3)") +
      theme(plot.title = element_text(size = 18)) + 
      
      # add line for mean
      geom_vline(aes(xintercept = mx),color ="red",size=1.5) +
      
      #add line for 0.025 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,3], probs = 0.025)) , colour = "green", size = 1) +
      
      #add line for 0.975 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,3], probs = 0.975)) , colour = "green", size = 1) +
      
      # add text for mean
      geom_text(aes(x=mx), y=0, label= round(mx,2), size=4, angle=0, vjust=1, hjust= 0) +
      
      # add text for  0.025 quantile
      geom_text(aes(x=quantile(coefficients2()[,3], probs = 0.025), y=0, 
                    label= round(quantile(coefficients2()[,3], probs = 0.025),2), 
                    angle=0, vjust=1, hjust= 0)) +
      
      # add text for  0.975 quantile
      geom_text(aes(x=quantile(coefficients2()[,3], probs = 0.975), y=0, 
                    label= round(quantile(coefficients2()[,3], probs = 0.975), 2), 
                    angle=0, vjust=1, hjust= 0))   
    
  })
  
  
  
  
  output$hist4 <- renderPlot({    
    mx<-mean(coefficients2()[,4])
    ggplot(data= as.data.frame(coefficients2() ), aes(x = coefficients2()[,4])) + geom_histogram(col="black", fill="blue", alpha = .2)  +
      #format histogram
      labs(x="Proportions", y="Frequency") +
      ggtitle("Covariate 3" (V4)) +
      theme(plot.title = element_text(size = 18)) + 
      
      # add line for mean
      geom_vline(aes(xintercept = mx),color ="red",size=1.5) +
      
      #add line for 0.025 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,4], probs = 0.025)) , colour = "green", size = 1) +
      
      #add line for 0.975 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,4], probs = 0.975)) , colour = "green", size = 1) +
      
      # add text for mean
      geom_text(aes(x=mx), y=0, label= round(mx,2), size=4, angle=0, vjust=1, hjust= 0) +
      
      # add text for  0.025 quantile
      geom_text(aes(x=quantile(coefficients2()[,4], probs = 0.025), y=0, 
                    label= round(quantile(coefficients2()[,4], probs = 0.025),2), 
                    angle=0, vjust=1, hjust= 0)) +
      
      # add text for  0.975 quantile
      geom_text(aes(x=quantile(coefficients2()[,4], probs = 0.975), y=0, 
                    label= round(quantile(coefficients2()[,4], probs = 0.975), 2), 
                    angle=0, vjust=1, hjust= 0))   
    
  })
  
  
  
  output$hist5 <- renderPlot({    
    mx<-mean(coefficients2()[,5])
    ggplot(data= as.data.frame(coefficients2() ), aes(x = coefficients2()[,5])) + geom_histogram(col="black", fill="blue", alpha = .2)  +
      #format histogram
      labs(x="Proportions", y="Frequency") +
      ggtitle("Covariate 4 (V5)") +
      theme(plot.title = element_text(size = 18)) + 
      
      # add line for mean
      geom_vline(aes(xintercept = mx),color ="red",size=1.5) +
      
      #add line for 0.025 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,5], probs = 0.025)) , colour = "green", size = 1) +
      
      #add line for 0.975 quantile
      geom_vline(aes(xintercept = quantile(coefficients2()[,5], probs = 0.975)) , colour = "green", size = 1) +
      
      # add text for mean
      geom_text(aes(x=mx), y=0, label= round(mx,2), size=4, angle=0, vjust=1, hjust= 0) +
      
      # add text for  0.025 quantile
      geom_text(aes(x=quantile(coefficients2()[,5], probs = 0.025), y=0, 
                    label= round(quantile(coefficients2()[,5], probs = 0.025),2), 
                    angle=0, vjust=1, hjust= 0)) +
      
      # add text for  0.975 quantile
      geom_text(aes(x=quantile(coefficients2()[,5], probs = 0.975), y=0, 
                    label= round(quantile(coefficients2()[,5], probs = 0.975), 2), 
                    angle=0, vjust=1, hjust= 0))   
    
  })
  
  # store API key in an environment variable called OWM_API_KEY
  owmr_settings("e366d11329936ebfaaf4cf08af0ff523")
  Sys.setenv(OWM_API_KEY = 'e366d11329936ebfaaf4cf08af0ff523')
  
  owm_data = find_city('London, uk', units = 'metric') %>%
    owmr_as_tibble()
  
  find_city <- function(city = NA, ...) {
    get <- owmr_wrap_get("find")
    get(city, ...) %>%
      owmr_parse() %>%
      owmr_class("owmr_find")
  }
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(owm_data) %>% addTiles() %>%
      add_weather(owm_data,
                  template = '<b>{{name}}</b>, {{temp}}°C',
                  icon = owm_data$weather_icon)
  })
  observe({
    req(input$tab_being_displayed == "Weather") # Only display if tab is 'Map Tab' 
    leafletProxy("map", data = owm_data)
  })
  
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
}

# Call shiny --------------------------------------------------------------

shinyApp(ui = ui, server = server)


