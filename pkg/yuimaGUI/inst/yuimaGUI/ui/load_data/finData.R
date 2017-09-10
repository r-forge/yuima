tabItem(tabName="finData",
        fluidRow(
          column(12,
                 h3("Load Financial and Economic data",class = "hTitle"),
                 h4("For Stocks and Shares, select Yahoo as the source using the symbols you can find ",
                    a("here", href="http://finance.yahoo.com/lookup", target = "_blank"), ".",
                    br(),
                    "For currencies and metals, select Oanda as the source and type the two symbols divided by '/' (e.g. EUR/USD or XAU/USD ).",
                    "The symbols are available ", a("here",href="http://www.oanda.com/help/currency-iso-code", target = "_blank"), ".",
                    br(),
                    "Economic series are available from ",a("Federal Reserve Bank of St. Louis", href="https://research.stlouisfed.org/fred2/", target = "_blank"), ".",
                    "Follow this ", a("example",href="example.jpg", target = "_blank"), " to find the symbols.",
                    br(),
                    "Multiple symbols are allowed if divided by empty spaces and/or commas (e.g. AAPL FB CSCO or AAPL,FB,CSCO)."),
                 hr(class = "hrHeader")
          )
        ),
        fluidRow(column(12,bsAlert("finDataAlert"))),
        fluidRow(
          column(6,
                 textInput(inputId="symb", value = NULL,label = "Insert Symbol"),
                 dateRangeInput(inputId="dR", label = "Download data from", start = "1900-01-01" ,end = Sys.Date()),
                 selectInput(inputId="sources", label = "Source", choices = c("Yahoo (OHLC data)" = "yahoo", "Oanda (Currencies &  Metals)" = "oanda", "Federal Reserve Bank of St. Louis" = "FRED")),
                 tags$button(type="button", id="finDataGo", class = "action-button", em("Load data")),
                 br(),br(),br(),
                 column(9),
                 column(3,shinyjs::hidden(selectInput("scale_finDataPlot", label = "Chart Scale", choices = c("Linear", "Logarithmic"))))
          ),
          column(6,
                 plotOutput("finDataPlot", height = "350px", brush = brushOpts(id = "finDataPlot_brush", delayType = "debounce", clip = TRUE, delay = 10000, resetOnNew = TRUE), dblclick = "finDataPlot_dbclick")
          )
        ),
        br(),
        fluidRow(
          column(12, DT::dataTableOutput("database1"))
        ),
        shinyjs::hidden(div(id="buttons_DataIO_fin", br(),
                            fluidRow(
                              column(6),
                              column(2,downloadButton(outputId = "finDataSave", label = "Save")),
                              bsTooltip("finDataSave", title = "Save data to file", placement = "top"),
                              column(2,actionButton(inputId = "finDataDelete", label = "Delete")),
                              bsTooltip("finDataDelete", title = "Delete selected data", placement = "top"),
                              column(2,actionButton(inputId = "finDataDeleteAll", label = "Delete All")),
                              bsTooltip("finDataDeleteAll", title = "Delete all data that are displayed", placement = "top")
                            )
        ))
)