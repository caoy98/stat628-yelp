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
  output$word <- renderPlot({
    if (input$wordclass == "Meat")  {print(meat_plot)}   
    if (input$wordclass == "Taste")  {print(taste_plot)}  
  })
  
  output$star = renderPlot({
    if (input$starclass == "Star1")  {print(star1_plot)}
    if (input$starclass == "Star2")  {print(star2_plot)}
    if (input$starclass == "Star3")  {print(star3_plot)}
    if (input$starclass == "Star4")  {print(star4_plot)}
    if (input$starclass == "Star5")  {print(star5_plot)}
  })
  
  
  output$highfqword = renderPlot({
    review_words_counts = review_filtered %>%
      count(word, sort = T) %>%
      top_n(100)
    
    wordcloud2(review_words_counts, size = 1)
  })
})
