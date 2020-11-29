shinyServer(function(input,output){
  # boxplot of attributes
  attnum = reactive({which(colnames(chinese_business)==input$attInput)})
  output$attribute = renderPlot({
    data = subset(chinese_business, !is.na(chinese_business[, attnum()]))
    ggplot(data = data, aes_string(x=input$attInput, y="stars", fill=input$attInput)) + 
      geom_boxplot(alpha=0.8) + 
      theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position = "none") +
      labs(x = input$attInput, y = "Stars") +
      ggtitle(paste("Boxplot of", input$attInput))
    })
  
  # review word analysis
  output$meat <- renderPlot({
    if (input$wordclass == "Meat")  {print(meat_plot)}   
    if (input$wordclass == "Taste")  {print(taste_plot)}  
  })
})
