dashboardPage(
  # title
  dashboardHeader(title = "Yelp for Chinese Restaurant", titleWidth = 300),
  # sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Popular Words", tabName = "words", icon = icon("book")),
    menuItem("Contact Us", tabName = "contact", icon = icon("comment")),
    menuItem("Reference", tabName = "ref", icon = icon("book"))
    )
  ),
  # main body
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 18px;
      }
    '))),
    
    tabItems(
    #First tab is dashboard
      tabItem(tabName = "dashboard",
        fluidRow(
          # A simple readme
          column(width = 12,
            box(title = "Read me first", status = "primary", width = NULL,
              p("This website will give some suggestions about how to increase Yelp review based on analysis about Yelp data.")
            )
          ),
          # Input box 
          box(title = "Choose your interested attribute", status = "primary",
              height = 500,
              varSelectInput("attInput", "Attribute:", chinese_business[, c(-1,-2)]),
              selectInput("wordclass", "Aspect", 
                          choices = c("Meat", "Taste")),
              selectInput("starclass", "Star",
                          choices = c("Star1","Star2","Star3","Star4","Star5"))
          ),
          
          # Result box 
          box(title = "Comparison of your chosen attribute", status = "primary",
              height = 500, plotOutput("attribute")
          ),
          box(title = "Word in Review", status = "primary", 
              plotOutput("word"))
        ),
        fluidRow(
          # Suggestions
        column(width = 12,
          box(title = "Our suggestions for you", status = "primary", width = NULL,
              p("These are some suggestions we want to share."))
          )
        )
      ),
      
      tabItem(
        # Second tab is popular words
        tabName = "words",
        fluidRow(
          # Input box 
          box(title = "Choose your interested attribute", status = "primary",
              height = 500,
              varSelectInput("attInput", "Attribute:", chinese_business[, c(-1,-2)]),
              selectInput("wordclass", "Aspect", 
                          choices = c("Meat", "Taste")),
              selectInput("starclass", "Star",
                          choices = c("Star1","Star2","Star3","Star4","Star5"))
          ),
          
          # Result box 
          box(title = "High frequency words", status = "primary",
              height = 500, wordcloud2Output("highfqword")
          ),
          box(title = "High frequency bigrams", status = "primary", 
              wordcloud2Output(""))
        ),
      ),
      
      # second tab is contact information
      tabItem(
        tabName = "contact",
        h4("Contact Us"),
        p("This shiny app webpage is created by Yuan Cao, Zeyu Li and Yuxiao Li."),
        p("If you have any problem when browsing this webpage, please 
          feel free to contact us with email."),
        p("Yuxiao Li ()"),
        p("Yuan Cao (cao234@wisc.edu)"),
        p("Zeyu Li ()")
      ),
      
      # third tab contains some references
      tabItem(
        tabName = "ref",
        h4("Reference"),
        p("Siri W. E. The gross composition of the body. Advances in Biological and Medical Physics. 1956; 4:239–280.")
      )
    )
  )
)