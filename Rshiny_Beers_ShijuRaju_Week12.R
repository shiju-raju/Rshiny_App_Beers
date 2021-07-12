---
  title: "Ole Faithful Full"
author: "Bivin"
date: "4/4/2019"
output: html_document
runtime: shiny
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r echo = FALSE}

library(shiny)

ui <- fluidPage(
  # App title ----
  titlePanel("Beers!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      #Copy the line below to make a select box 
      #selectInput("select", label = h3("Select the Data Set"), 
      #choices = list("Eruptions" = "eruptions", "Waiting" = "waiting"), 
      #selected = 1),
      
      selectInput("select", label = h3("IBU or ABV?"), 
                  choices = list("IBU" = "IBU", "ABV" = "ABV"), 
                  selected = 1),
      
      selectInput("select", label = h3("BoxPlot or Histogram?"), 
                  choices = list("BoxPlot" = "BoxPlot", "Histogram" = "Histogram"), 
                  selected = 1),
      
      selectInput("select", label = h3("Scatter Plot: Yes or No ?"), 
                  choices = list("Yes" = "Yes", "No" = "No"), 
                  selected = 1),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)
Beersnew<-na.omit(Beers)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    if(input$select == "IBU")
      if(input$select == "Histogram")
      {
        x    <- Beersnew$IBU
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "Internatioal Bitterness Unit (IBU)",
             main = "Histogram of IBU")
        

        
      }
      if(input$select == "BoxPlot")
      {
        x    <- Beersnew$IBU
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
        boxplot(x, breaks = bins, col = "#75AADB", border = "white",
              xlab = "Alcohol by Volume (IBU)",
              main = "Boxplot of IBU")
      }
    if(input$select == "ABV")
    {
      x    <- Beersnew$ABV
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      Beersnew%>% ggplot(aes(x = IBU, y = ABV)) + geom_point() +
      geom_smooth(method = "lm") + ggtitle("IBV vs ABV") + 
      xlab("Bitterness(IBU)") + ylab("Alcohol by volume(ABV)")
      
      
      hist(x, breaks = bins, col = "#75AADB", border = "white",
           xlab = "Alcohol by Volume (ABV)",
           main = "Histogram of ABV")
    }
    if(input$select == "Yes")
    {
      x_scatter    <- Beersnew$IBU
      y_scatter <-Beersnew$ABV
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      Beersnew%>% ggplot(aes(x = x_scatter, y = y_scatter)) + geom_point() +
        geom_smooth(method = "lm") + ggtitle("IBV vs ABV") + 
        xlab("Bitterness(IBU)") + ylab("Alcohol by volume(ABV)")
      
    }
    
    if(input$select == "BoxPlot")
    {
      x    <- Beersnew$ABV
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      boxplot(x, breaks = bins, col = "#75AADB", border = "white",
              xlab = "Alcohol by Volume (ABV)",
              main = "Histogram of ABV")
    }
  })
  
}
shinyApp(ui, server)