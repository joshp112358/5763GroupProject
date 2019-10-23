
library(shiny)
#a ui is a dictionary of dictionaries, which sets up 
#how the webpage should look. The first entry in the 
#dictionary are the panels, which sets up sections, and
#within each panel places further objects, or display 
#functions.Each function has an input, which can be 
#controlled by the user and an output which the code reacts to.

ui <- fluidPage(
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
    mainPanel("Results",
              uiOutput("mean"), 
              uiOutput("table"),
              plotOutput("hist1"),
              plotOutput("hist2"),
              plotOutput("hist3"),
              plotOutput("hist4"),
              plotOutput("hist5"))
)


