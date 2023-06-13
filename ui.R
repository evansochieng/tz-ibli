header <- shinydashboard::dashboardHeader( 
  title = "TANZANIA IBLI",
  titleWidth = 400
)

sidebar <- shinydashboard::dashboardSidebar(uiOutput("sidebarpanel"))
body <- shinydashboard::dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-shinydashboard::dashboardPage(header, sidebar, body, skin = "blue")
