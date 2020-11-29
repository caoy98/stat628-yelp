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
  
  output$starword = renderWordcloud2({
    word_star_counts = word_filtered %>%
      filter(stars==input$starclass) %>%
      count(word, sort = T) %>%
      top_n(100)
    
    wordcloud2(word_star_counts, size = 0.5)
  })
  
  
  output$starbigram = renderWordcloud2({
    bigram_star_counts = bigram_filtered %>%
      filter(stars==input$starclassbigram) %>%
      count(bigram, sort = T) %>%
      top_n(100)
    
    wordcloud2(bigram_star_counts, size = 0.5)
  })
})
