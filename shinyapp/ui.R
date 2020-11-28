dashboardPage(
  # title
  dashboardHeader(title = "Yelp for Chinese Restaurant", titleWidth = 300),
  # sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Contact Us", tabName = "contact", icon = icon("comment")),
    menuItem("Reference", tabName = "ref", icon = icon("book"))
    )
  ),
  # main body
  dashboardBody(
  #Multiple Tabs
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
           #selectInput("attInput", "Choose an attribute", 
           #            choices = list("Take Out"="RestaurantsTakeOut"))
            varSelectInput("attInput", "Attribute:", chinese_business[, c(-1,-2)])
          ),
          
          # Result box 
          box(title = "Comparison of your chosen attribute", status = "primary",
              plotOutput("attribute")
          )
        ),
        fluidRow(
          # Suggestions
        column(width = 12,
          box(title = "Our suggestions for you", status = "primary", width = NULL)
          )
        )
      ),
      
      # second tab is contact information
      tabItem(
        tabName = "contact",
        h4("Contact Us"),
        p("This Shiny App is created by Yuan Cao, Zeyu Li and Yuxiao Li."),
        p("If you have any problem when using this calculator, please 
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