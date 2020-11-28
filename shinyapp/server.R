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
    ggplot(food_prob, aes(fill=stars, y=frequency, x=stars)) + 
      geom_bar(position="dodge", stat="identity") +
      scale_fill_viridis(discrete = T, option = "E") +
      ggtitle("Probability") +
      facet_wrap(~meat) +
      theme_ipsum() +
      theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position="none") +
      xlab("")
  })
})
