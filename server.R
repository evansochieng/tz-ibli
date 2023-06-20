server <- function(input, output, session) {
  
  # Define variable for choices of rainfall pattern
  rainPattern <- c("Unimodal", "Bimodal")
  
  ###########Observe uai and autopopulate rainfall pattern#########################################

  # Observe choice of uai to inform choice of rainfall pattern
  observe({
    if(req(input$uai) == "Tanga") {
      updateSelectInput(session, "pattern",
                        #choices = rainPattern,
                        selected = isolate("Bimodal") )
    }
  })

  observe({
    if(req(input$uai) == "Morogoro")
      updateSelectInput(session, "pattern",
                        #choices = rainPattern,
                        selected = isolate("Unimodal") )
  })
  ######################################################################
  
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
                                                  choices = c("Morogoro", "Tanga"),
                                                  selected = "Morogoro"),
                                      selectInput(inputId = "triggerlevel", label = "Trigger Level (Percentile Value)", 
                                                  choices = seq(0.1, 1, 0.05), selected = 0.25),
                                    ),
                                    column(
                                      width = 6,
                                      selectInput(inputId = "pattern", label = "Rainfall Pattern", 
                                                  choices = rainPattern,
                                                  selected = "Unimodal"),
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
                                        # create a date input for the start of the season
                                        dateInput(inputId = "rsstartdate", label = "Start of Season", 
                                                  value = "2023-11-01", format = "dd-m-yyyy")
                                      ),
                                      column(
                                        width=6,
                                        # create a date input for the end of the season
                                        dateInput(inputId = "rsenddate", label = "End of Season", 
                                                  value = "2024-05-31", format = "dd-m-yyyy")
                                      )
                                    )),
                                  tags$div(
                                    id = "bimodaldates", 
                                    style = "display: none;",
                                    fluidRow(
                                      column(
                                        width=6,
                                        # create a date input for the start of the long rain long dry season
                                        dateInput(inputId = "lrstartdate", label = "LRLD Start Date", 
                                                  value = "2023-03-01", format = "dd-m-yyyy")
                                      ),
                                      column(
                                        width=6,
                                        # create a date input for the end of the long rain long dry season
                                        dateInput(inputId = "lrenddate", label = "LRLD End Date", 
                                                  value = "2023-06-30", format = "dd-m-yyyy")
                                      )
                                    ),
                                    fluidRow(
                                      column(
                                        width=6,
                                        # create a date input for the start of the short rain short dry season
                                        dateInput(inputId = "srstartdate", label = "SRSD Start Date", 
                                                  value = "2023-10-01", format = "dd-m-yyyy")
                                      ),
                                      column(
                                        width=6,
                                        # create a date input for the end of the short rain short dry season
                                        dateInput(inputId = "srenddate", label = "SRSD End Date", 
                                                  value = "2023-12-31", format = "dd-m-yyyy")
                                      )
                                    )),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      selectInput(inputId = "suminsured", label = "Policy Sum Insured", 
                                                  choices = seq(50, 300, 10), 
                                                  selected = 100)
                                    ),
                                    column(
                                      width = 6,
                                      selectInput(inputId = "maxpayout", label = "Policy Maximum Payout", 
                                                  choices = seq(0.5, 1, 0.1), selected = 1)
                                    )
                                  ),
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
                                  ),
                                  tags$br(),
                                  # Download termsheet
                                  shiny::downloadButton("termsheet", "Download Termsheet")
                                ),
                                column(
                                  width = 6,
                                  h3("Historical Payout Analysis", style="text-align:center;"),
                                  plotly::plotlyOutput("payoutBarPlot"),
                                  tags$br(),
                                  # Download button for actual payouts
                                  shiny::downloadButton("downloadpayouts", "Download Actual Payouts"),
                                  tags$br(),
                                  tags$br(),
                                  shinydashboard::valueBoxOutput("premium", width = 20)
                                )
                              )
      ))
  })
  
  ##################################
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
  
  pattern <- reactive({
    as.character(input$pattern)
  })
  
  sumInsured <- reactive({
    as.numeric(input$suminsured)
  })
  
  RSstartDate <- reactive({
    input$rsstartdate
  })
  
  RSendDate <- reactive({
    input$rsenddate
  })
  
  LRstartDate <- reactive({
    input$lrstartdate
  })
  
  LRendDate <- reactive({
    input$lrenddate
  })
  
  SRstartDate <- reactive({
    input$srstartdate
  })
  
  SRendDate <- reactive({
    input$srenddate
  })
  
  #################################
  ###### Outputs ######
  
  # call the function to calculate payouts + other outputs
  payoutFunc <- reactive({
    payouts <- claimCalculator(stackedTZData = stackedTZData, uai = uai(), pattern = pattern(),
                                triggerLevel = triggerlevel(), exitLevel = exitoption(), 
                                maxPayout = maxPayout(), sumInsured = sumInsured(), 
                               RSstartDate = RSstartDate(), RSendDate = RSendDate(), LRstartDate = LRstartDate(), 
                               LRendDate = LRendDate(), SRstartDate = SRstartDate(), SRendDate = SRendDate())
    # return the payouts
    return(payouts)
  })
  
  # bar graph of historical payouts
  histPayoutPlot <- reactive({
    if (uai() == "Tanga") {
      # draw a stack bar graph for the long rains and short rains seasons payouts using plot_ly
      histPayoutsBarGraph <- payoutFunc()$payoutsPlot |>
        plotly::plot_ly(x = ~Year,
                        y = ~`LR Season Payout`, type = 'bar', name = 'LR Season') |>
        plotly::add_trace(y = ~`SR Season Payout`, name = 'SR Season') |>
        plotly::layout(yaxis = list(title = 'Annual Payouts'),
                       barmode = 'stack',title="Historical Payouts")
      
      # return plot
      return(histPayoutsBarGraph)
    } else if (uai() == "Morogoro") {
      # draw a bar graph for the rainy season payouts using plot_ly
      histPayoutsBarGraph <- payoutFunc()$payoutsPlot |>
        plotly::plot_ly(x = ~Year,
                        y = ~`RS Payouts`, type = 'bar', name = 'Rainy Season') |>
        plotly::layout(yaxis = list(title = 'Annual Payouts'), 
                       title="Historical Payouts")
      
      # return plot
      return(histPayoutsBarGraph)
    }
  })
  
  output$payoutBarPlot <- plotly::renderPlotly({
    histPayoutPlot()
  })
  
  # premium rate
  output$premium <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      "Premium Rate",
      paste(payoutFunc()$premiumRate * 100, '%', sep = ' '),
      icon = icon("hand-holding-dollar", verify_fa = FALSE),
      width = 2
    )
  })

  # Downloadable payouts of the selected uai as csv
  output$downloadpayouts <- downloadHandler(
    filename = function() {
      paste(input$uai, "historicalpayouts.csv", sep = "")
    },
    content = function(file) {
      write.csv(payoutFunc()$claims, file, row.names = FALSE)
    }
  )
  
  # Downloadable summary of parameters of the selected uai as csv
  output$downloadparams <- downloadHandler(
    filename = function() {
      paste(input$uai, "summaryparameters.csv", sep = "")
    },
    content = function(file) {
      write.csv(payoutFunc()$inputParameters, file, row.names = FALSE)
    }
  )
  
  # display table of params
  output$params <- renderTable(payoutFunc()$sumParameters, bordered = TRUE)
  
  # download termsheet
  output$termsheet <- downloadHandler(
    # For HTML output, change this to "termsheet.html"
    filename = function() {
      paste0(input$uai, "_", "termsheet", ".pdf", sep = "")
    },
    content = function(file) {
      # # Copy the report file to a temporary directory before processing it, in
      # # case we don't have write permissions to the current working dir (which
      # # can happen when deployed).
      tempReport <- file.path(tempdir(), "termsheetreport.Rmd")
      file.copy("termsheetreport.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(uai = uai(), trigger = triggerlevel(), exit = exitoption(),
                     maxPayout = maxPayout(), pattern = pattern(),
                     sumInsured = sumInsured(), premium = payoutFunc()$premiumRate)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      # rmarkdown::render("termsheetreport.Rmd", output_file = file,
      #                   params = params,
      #                   envir = new.env(parent = globalenv())
      # )
      rmarkdown::render("termsheetreport.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}