tabItem(tabName = "hedging",
        fluidRow(
          column(12,
                 h3("Here you can manage the risk of a portfolio composed of options and the underlying asset.",class = "hTitle"),
                 h4("The evolution of the underlying asset is simulated by the models you estimated in the Modeling section.", br(),
                    "After performing the simulation, click on the 'Show P&L' button in the 'Profit&Loss' tab and customize your portfolio.",br(),
                    "The Profit&Loss distribution of your portfolio will be displayed (it includes transaction costs that you can customize)."),
                 hr(class = "hrHeader")
          )
        ),
        fluidRow(column(12,bsAlert("hedging_alert"))),
        tabsetPanel(id = "panel_hedging", type = "tabs",
                    tabPanel(title = "Start simulations",
                             fluidRow(column(12, br(),
                                             h4("Click on the model by which to simulate the evolution of the underlying asset"),
                                             DT::dataTableOutput("hedging_databaseModels")
                             )
                             ),
                             br(),
                             fluidRow(column(12,div(align="center",
                                                    br(),
                                                    fluidRow(
                                                      column(3,selectInput("hedging_type", label="Option Type:", c(Call="call", Put="put"))),
                                                      column(3,uiOutput("hedging_strike")),
                                                      column(3,dateInput("hedging_maturity", label="Maturity:", value = Sys.Date()+30)),
                                                      column(3,numericInput("hedging_optMarketPrice", label="Option Market Price:", value=NA, min = 0))
                                                    ),
                                                    fluidRow(
                                                      column(3),
                                                      column(3,uiOutput("hedging_assMarketPrice")),
                                                      column(3,numericInput("hedging_lotMult", label="Number of Options per Lot:", value=1, min = 1)),
                                                      column(3)
                                                    ),
                                                    fluidRow(
                                                      column(4),
                                                      column(4,numericInput("hedging_nSim", label="Number of Simulations", value=1000, min = 1)),
                                                      column(4)
                                                    ),
                                                    fluidRow(
                                                      column(4),
                                                      column(4, actionButton("hedging_button_startComputation", label = "Start Computation", width = "50%"))
                                                    )
                             )))
                    ),
                    tabPanel(title = "Profit&Loss",
                             bsModal(id="hedging_commissionPlan", trigger = "hedging_button_show", title = "Profit & Loss", size = "large",
                                     div(id="hedging_body",align="center",
                                         fluidRow(
                                           column(3),
                                           column(6, uiOutput("hedging_modal_id")),
                                           column(3, uiOutput("hedging_modal_id_hidden"))
                                         ),
                                         fluidRow(
                                           column(3),
                                           column(3,uiOutput("hedging_nOptLot_hedge")),
                                           column(3,uiOutput("hedging_nAss_hedge"))
                                         ),
                                         fluidRow(
                                           column(9, plotOutput("hedging_plot_distribution")),
                                           column(3,
                                                  sliderInput("hedging_slider_nBin", label = "Adjust bin width", min=1, max=100, value = 30, step = 1, ticks = FALSE),
                                                  br(),
                                                  sliderInput("hedging_slider_rangeHist", label = "Quantiles (%)", min = 0, max = 100, value = c(5,95), ticks = FALSE, step = 0.01),
                                                  uiOutput("hedging_quantiles_text"),
                                                  br(),br(),
                                                  uiOutput("hedging_capital_text"),
                                                  br(),br(),
                                                  actionButton("hedging_button_saveHedging", "Save Changes", width = "80%")
                                           )
                                         ),
                                         br(),
                                         box(title = p("Modify Option", style="color:black; font-weight: bold;"),collapsible = TRUE, collapsed = FALSE, width = 12,
                                             fluidRow(
                                               column(4,uiOutput("hedging_type2")),
                                               column(4,uiOutput("hedging_strike2")),
                                               column(4,uiOutput("hedging_optMarketPrice2"))
                                             )
                                         ),
                                         box(title = p("Trading Costs", style="color:black; font-weight: bold;"),collapsible = TRUE, collapsed = FALSE, width = 12,
                                             fluidRow(
                                               column(3,br(),numericInput("hedging_percCostAss", label="Asset: Trading cost (%):", value=0.19, min = 0)),
                                               column(3,br(),numericInput("hedging_minCostAss", label="Asset: Min trading cost:", value=2.95, min = 0)),
                                               column(3,numericInput("hedging_rateShort", label="Asset: Yearly interest rate short position (%):", value=4.95, min = 0)),
                                               column(3,numericInput("hedging_lotCostOpt", label="Option: Trading cost per lot:", value=5.95, min = 0))
                                             )
                                         )
                                     )
                             ),
                             fluidRow(column(12,br(),
                                             DT::dataTableOutput("hedging_table_results")
                             )),
                             br(),
                             fluidRow(
                               column(2,actionButton(inputId = "hedging_button_show", label = "Show P&L")),
                               column(6),
                               column(2,actionButton(inputId = "hedging_button_delete", label = "Delete")),
                               column(2,actionButton(inputId = "hedging_button_deleteAll", label = "Delete All"))
                             )         
                    )
        )
)