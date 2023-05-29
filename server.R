server <- function(input, output, session) {
  
  # Define variables for choices of rainfall pattern and season
  rainPattern <- c("Unimodal", "Bimodal")
  # rainSeason <- c("Long Rains", "Short Rains", "Single Rains")
  # biRainSeason <- c("Long Rains", "Short Rains")
  # singleRainSeason <- c("Single Rains")
  yearMonths <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  
  # ###########Observe rainfall pattern to determine season#########################################
  #   
  # # Observe choice of rainfall pattern to inform choice of season
  # observe({
  #   if(req(input$pattern) == "Bimodal") {
  #     updateSelectInput(session, "season", 
  #                       choices = biRainSeason, 
  #                       selected = isolate("Long Rains") )
  #   }
  # })
  # 
  # observe({
  #   if(req(input$pattern) == "Unimodal")
  #     updateSelectInput(session, "season",
  #                       choices = singleRainSeason,
  #                       selected = isolate("Single Rains") )
  # })
  # ######################################################################
  
  ###################################
  observeEvent(input$pattern, {
    if (input$pattern == "Unimodal") {
      shinyjs::show("unimodaldates")
      shinyjs::hide("bimodaldates")
    } else {
      shinyjs::hide("unimodaldates")
      shinyjs::show("bimodaldates")
    }
  })
  ###################################
  
  output$sidebarpanel <- renderUI({
    shinydashboard::sidebarMenu(
      # menu items
      shinydashboard::menuItem(text = "Pricing Module", tabName = "payout", icon = icon("th"))
    )
  })
  
  output$body <- renderUI({
    shinydashboard::tabItems(
      
      # First tab
      shinydashboard::tabItem(tabName = "payout", class = "active",
                              fluidRow(
                                shinydashboard::box(width = 12, tags$div(
                                  style = "text-align: center",
                                  h2("PRICING TANZANIA IBLI PRODUCT")
                                ))
                              ),
                              fluidRow(
                                column(
                                  width=6,
                                  h3("Select Product Parameters", style="text-align:center;"),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      selectInput(inputId = "uai", label = "UAI", 
                                                  choices = c("Ajaj", "Murhal", "Alaf", "Eena", "Daoul"), 
                                                  selected = "Ajaj"),
                                      selectInput(inputId = "suminsured", label = "Policy Sum Insured", 
                                                  choices = seq(50, 300, 10), 
                                                  selected = 100),
                                      selectInput(inputId = "triggerlevel", label = "Trigger Level (Percentile Value)", 
                                                  choices = seq(0.1, 1, 0.05), selected = 0.2),
                                    ),
                                    column(
                                      width = 6,
                                      selectInput(inputId = "pattern", label = "Rainfall Pattern", 
                                                  choices = rainPattern,
                                                  selected = "Unimodal"),
                                      selectInput(inputId = "maxpayout", label = "Policy Maximum Payout", 
                                                  choices = seq(0.5, 1, 0.1), selected = 1),
                                      selectInput(inputId = "exitoption", label = "Exit Level", 
                                                  choices = c("Minimum", "1st Percentile", "5th Percentile"), selected = "5th Percentile")
                                    )
                                  ),
                                  tags$div(
                                    id = "unimodaldates", 
                                    style = "display: none;",
                                    fluidRow(
                                      column(
                                        width=6,
                                        selectInput(inputId = "statmonth", label = "Start Month", 
                                                    choices = yearMonths, selected = "November")
                                      ),
                                      column(
                                        width=6,
                                        selectInput(inputId = "edmonth", label = "End Month", 
                                                    choices = yearMonths, selected = "May")
                                      )
                                    )),
                                  tags$div(
                                    id = "unimodaldates", 
                                    style = "display: none;",
                                    fluidRow(
                                      column(
                                        width=6,
                                        selectInput(inputId = "startmonth", label = "Start Month", 
                                                    choices = yearMonths, selected = "November")
                                      ),
                                      column(
                                        width=6,
                                        selectInput(inputId = "endmonth", label = "End Month", 
                                                    choices = yearMonths, selected = "May")
                                      )
                                    )),
                                  tags$div(
                                    id = "bimodaldates", 
                                    style = "display: none;",
                                    fluidRow(
                                      column(
                                        width=6,
                                        selectInput(inputId = "lrldstartmonth", label = "LRLD Start Month", 
                                                    choices = yearMonths, selected = "March")
                                      ),
                                      column(
                                        width=6,
                                        selectInput(inputId = "lrldendmonth", label = "LRLD End Month", 
                                                    choices = yearMonths, selected = "June")
                                      )
                                    ),
                                    fluidRow(
                                      column(
                                        width=6,
                                        selectInput(inputId = "srsdstartmonth", label = "SRSD Start Month", 
                                                    choices = yearMonths, selected = "October")
                                      ),
                                      column(
                                        width=6,
                                        selectInput(inputId = "srsdendmonth", label = "SRSD End Month", 
                                                    choices = yearMonths, selected = "December")
                                      )
                                    )),
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$div(
                                    style = "border: 1px solid black !important;",
                                    tags$div(
                                      style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
                                      strong(h4("Summary of Selected Parameters", style="margin-right:150px")),
                                      # Download button for summary of parameters
                                      shiny::downloadButton("downloadparams", "")
                                    ),
                                    # strong(h4("Summary of Selected Parameters")),
                                    tableOutput("params"),
                                    tags$br()
                                    # # Download button for summary of parameters
                                    # shiny::downloadButton("downloadparams", "Download Parameters")
                                  ),
                                  tags$br(),
                                  # Download termsheet
                                  shiny::downloadButton("termsheet", "Download Termsheet")
                                ),
                                column(
                                  width = 6,
                                  h3("Historical Payout Analysis", style="text-align:center;"),
                                  #dataTableOutput('payouts'),
                                  plotly::plotlyOutput("payoutBarPlot"),
                                  tags$br(),
                                  # Download button for actual payouts
                                  shiny::downloadButton("downloadpayouts", "Download Actual Payouts"),
                                  tags$br(),
                                  # strong(h4("Actual Historical Payouts Values")),
                                  # DT::dataTableOutput("payouts"),
                                  tags$br(),
                                  shinydashboard::valueBoxOutput("premium", width = 20)
                                )
                              )
      ))
  })
  
  # grab the inputs
  uai <- reactive({
    as.character(input$uai)
  })
  
  triggerlevel <- reactive({
    as.numeric(input$triggerlevel)
  })
  
  exitoption <- reactive({
    as.character(input$exitoption)
  })
  
  maxPayout <- reactive({
    as.numeric(input$maxpayout)
  })
  
  # call the function
  payoutFunc <- reactive({
    payouts <- payoutCalculator(ndviFile = ndviFile, uai = uai(),
                                triggerlevel = triggerlevel(), exitlevel = exitoption(), 
                                maxPayout = maxPayout())
    # return the payouts
    return(payouts)
  })
  
  # bar graph of historical payouts
  output$payoutBarPlot <- plotly::renderPlotly({
    
    # plot bar graph of annual payouts
    histPayoutsBarGraph <- payoutFunc()$payouts |>
      plotly::plot_ly(x = ~Year, y = ~Payouts) |>
      plotly::add_bars() |>
      plotly::layout(
        title = "Historical Payouts",
        xaxis = list(title = "Years"),
        yaxis = list(title = "Payouts (%)")
      )
    histPayoutsBarGraph
    
  })
  
  # premium rate
  output$premium <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      "Premium Rate",
      paste(payoutFunc()$premiumRate, '%', sep = ' '),
      icon = icon("hand-holding dollar", verify_fa = FALSE),
      width = 2
    )
  })
  
  ##########################################################################
  # # display payouts
  # output$payouts <-  DT::renderDataTable({
  #   DT::datatable(payoutFunc()$claims, options = list(scroller = TRUE,
  #                                                     scrollX = 500))
  # })
  #################################################################
  # Downloadable csv of selected dataset ----
  output$downloadpayouts <- downloadHandler(
    filename = function() {
      paste(input$uai, "historicalpayouts.csv", sep = "")
    },
    content = function(file) {
      write.csv(payoutFunc()$claims, file, row.names = FALSE)
    }
  )
  
  
  # display table of params
  output$params <- renderTable(payoutFunc()$triEx, bordered = TRUE)
  
}