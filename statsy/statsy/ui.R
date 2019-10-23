
library(shiny)
#a ui is a dictionary of dictionaries, which sets up 
#how the webpage should look. The first entry in the 
#dictionary are the panels, which sets up sections, and
#within each panel places further objects, or display 
#functions.Each function has an input, which can be 
#controlled by the user and an output which the code reacts to.

ui <- fluidPage(
    sidebarPanel("Place Data Here",
                 fileInput(inputId = "data",
                           label = "Input CSV",
                           accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"),
                           buttonLabel = "Browse..."),
                 textInput(inputId = "formula",
                           label = "Linear Regression Formula",
                           value = "Age~Weight"),
                 numericInput(inputId = "num_of_var",
                              label = "Number of Variables",
                              value = 2),
                 numericInput(inputId = "num_of_boots",
                              label = "Number of Boots",
                              value = 1000),
                           ),
    mainPanel("main panel",
              uiOutput("mean"), 
              uiOutput("table"),
              plotOutput("hist1"),
              plotOutput("hist2"))
)


