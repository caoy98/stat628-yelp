dashboardPage(
  # title
  dashboardHeader(title = "Yelp for Chinese Restaurant", titleWidth = 300),
  # sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Popular Words", tabName = "words", icon = icon("book")),
    menuItem("Popular Bigrams", tabName = "bigrams", icon = icon("book")),
    menuItem("Contact Us", tabName = "contact", icon = icon("comment"))
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
            box(title = "What this website does", status = "primary", width = NULL,
              p("This website is built based on review data of Chinese restaurants
              on Yelp, it shows the 
              connections between Yelp star and review text as well as business 
              attributes (take out available? noise too loud?). We share our
              suggestions about how to increase your Yelp review. You can see these
              connections and suggestions on this page, and browse the most frequent 
              words and bigrams within different star levels in Popular Words and
                Popular Bigrams pages on the right."),
              varSelectInput("attInput", "Choose an attribute you interested:", 
                             chinese_business[, c(-1,-2)]),
              selectInput("wordclass", "Choose a kind of words you interested:", 
                          choices = c("Meat", "Taste", "Service")) 
            )
          ),
          # Result box 
          box(title = "Comparison of your chosen attribute", status = "primary",
              height = 500, plotOutput("attribute")),
          box(title = "Comparison of your chosen kind of words", status = "primary", 
              height = 500, plotOutput("word"))
        ),
        fluidRow(
          # Suggestions
        column(width = 12,
          box(title = "Our suggestions for you", status = "primary", width = NULL,
              p("These are some suggestions we want to share."),
              h4("1. Offer these meats"),
              p("First, we suggest that chicken, shrimp, beef, pork, and crab are safe 
                choices for Chinese restaurants. In meat probability figure at Word 
                in Review, the words of these meats 
                are neutral since there is no obvious difference among frequencies 
                across restaurants with different ratings."),
              h4("2. Make sure delicious spicy food on your menu"),
              p("Second, serving spicy foods would efficiently help with ratings 
                on Yelp. If you look at taste probability figure, you can see the 
                probabilities of “Spicy” in higher ratings are larger than that of 
                lower ratings. Technically, statistical test also supports its 
                significance."),
              h4("3. Launch reservation and control noise"),
              p("Does your business support reservation service? Have you paid attention
                to the noise level in your restaurant? If yes, congratulations! When you
                offer reservation and decrease your noise in restaurant, the odds that 
                you get a higher review are 2.1 and 1.75 times, respectively."))
          )
        )
      ),
      
      tabItem(
        # Second tab is popular words
        tabName = "words",
        fluidRow(
          # Input box 
          box(title = "Choose a star level you interested", status = "primary",
              height = 500,
              selectInput("starclass", "Star",
                          choices = list("Star1"=1,"Star2"=2,"Star3"=3,"Star4"=4,"Star5"=5))
          ),
          
          # Result box 
          box(title = "High frequency words", status = "primary",
              height = 500, wordcloud2Output("starword")
          )
        )
      ),
      
      tabItem(
        tabName = "bigrams",
        fluidRow(
          # Input box 
          box(title = "Choose a star level you interested", status = "primary",
              height = 500, selectInput("starclassbigram", "Star",
                          choices = list("Star1"=1,"Star2"=2,"Star3"=3,"Star4"=4,"Star5"=5))
          ),
          box(title = "High frequency bigrams", status = "primary", height = 500,
              wordcloud2Output("starbigram"))
        )
      ),
      
      # third tab is contact information
      tabItem(
        tabName = "contact",
        h4("Contact Us"),
        p("This shiny app webpage is created by Yuan Cao, Zeyu Li and Yuxiao Li."),
        p("If you have any problem when browsing this webpage, please 
          feel free to contact us with email."),
        p("Yuxiao Li (li2268@wisc.edu)"),
        p("Yuan Cao (cao234@wisc.edu)"),
        p("Zeyu Li (zli995@wisc.edu)")
      )
    )
  )
)