header <- shinydashboard::dashboardHeader( 
  title = "TANZANIA IBLI",
  titleWidth = 400
  # add SFSA social media links
  # tags$div(
  #   
  #   tags$li(class="dropdown", tags$a(href="https://twitter.com/SyngentaFDN", icon("twitter"), "Twitter", target="_blank")),
  #   tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/company/syngenta-foundation-for-sustainable-agriculture", icon("linkedin"), "LinkedIn", target="_blank")),
  #   tags$li(class="dropdown", tags$a(href="https://www.youtube.com/channel/UCnKl2XDnW5dH9W4lpvWf1vA/videos", icon("youtube"), "YouTube", target="_blank"))
  # ),
)

sidebar <- shinydashboard::dashboardSidebar(uiOutput("sidebarpanel"))
body <- shinydashboard::dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-shinydashboard::dashboardPage(header, sidebar, body, skin = "blue")
