library(shiny)
library(shinydashboard)
library(ggplot2)

chinese_business = read.csv("data/chinese.csv")

# Define server logic required to draw a histogram
shinyServer(function(input,output){
    output$attribute = renderPlot({
      ggplot(data = chinese_business, aes_string(x=input$attInput, y="stars", fill=input$attInput)) + 
        geom_boxplot(alpha=0.8) + 
        theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position = "none") +
        labs(y = "Stars") +
        ggtitle(paste("Boxplot of Attribute"))
    })
})
