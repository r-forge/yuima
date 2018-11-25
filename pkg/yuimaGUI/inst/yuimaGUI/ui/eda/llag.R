tabItem(tabName = "llag",
        fluidRow(
          column(12,
                 h3("Lead-Lag and Correlation Analysis",class = "hTitle"),
                 h4("Select the series you wish to analyze and the kind of analysis you want to perform (Lead-Lag or Correlation).", br(),
                    "Choose the correlation measure (if you selected Correlation) or the maximum lag to use (if you selected Lead-Lag).",br(),
                    "You can specify which interval to use over the whole series for your analysis."),
                 hr(class = "hrHeader")
          )
        ),
        fluidRow(column(12,bsAlert("llag_alert"))),
        fluidRow(column(12,
                        column(4,
                               h4("Available data"),
                               DT::dataTableOutput("llag_table_select")
                        ),
                        column(4,
                               h4("Selected data"),
                               DT::dataTableOutput("llag_table_selected")
                        ),
                        column(4,br(),br(),
                               div(align="center",
                                   selectInput("llag_type", label = "Type of analysis", choices = c("Lead-Lag"="llag", "Correlation"="corr"), selected = "llag"),
                                   numericInput("llag_maxLag", label = "Max Lag", value = 20, min = 1, step = 1),
                                   bsTooltip("llag_maxLag", title = "Max Lag is expressed in days if you are using series indexed by date. It is expressed in the same unit of measure of the index if you are using numerical indexes.", placement = "top"),
                                   shinyjs::hidden(selectInput("llag_corr_method", label = "Method", choices = c("Pearson"="pearson", "Kendall"="kendall", "Spearman"="spearman", "Hayashi-Yoshida"="HY", "Pre-averaged Hayashi-Yoshida"="PHY", "Modulated Realized Covariance"="MRC", "Two Scales realized CoVariance"="TSCV", "Generalized Multiscale Estimator"="GME", "Realized Kernel"="RK", "Quasi Maximum Likelihood Estimator"="QMLE", "Separating Information Maximum Likelihood"="SIML", "Truncated Hayashi-Yoshida"="THY", "Pre-averaged Truncated Hayashi-Yoshida"="PTHY", "Subsampled Realized Covariance"="SRC", "Subsampled realized BiPower Covariation"="SBPC"), selected = "HY")),
                                   dateRangeInput("llag_range_date", label = "Range", start = Sys.Date()-365, end = Sys.Date()),
                                   shinyjs::hidden(div(id="llag_range_numeric",
                                                       column(6,numericInput("llag_range_numeric1", label = "From", value = 0)),
                                                       column(6,numericInput("llag_range_numeric2", label = "To", value = 1))
                                   ))
                               )
                        )
        )),
        br(),
        fluidRow(column(12,
                        column(2,actionButton("llag_button_select",label = "Select", align = "center")),
                        bsTooltip("llag_button_select", title = "Select data", placement = "top"),
                        column(2,actionButton("llag_button_selectAll",label = "Select All", align = "center")),
                        bsTooltip("llag_button_selectAll", title = "Select all data that are displayed", placement = "top"),
                        column(2,actionButton("llag_button_delete",label = "Delete", align = "center")),
                        bsTooltip("llag_button_delete", title = "Delete selected data", placement = "top"),
                        column(2,actionButton("llag_button_deleteAll",label = "Delete All", align = "center")),
                        bsTooltip("llag_button_deleteAll", title = "Delete all data that are displayed", placement = "top"),
                        column(4,actionButton("llag_button_startEstimation", label = "Start Analysis", align = "center"))
        )),
        br(),
        fluidRow(column(12,
                        shinyjs::hidden(div(id = "llag_plot_body", align = "center",
                                            hr(class = "hrHeader"),
                                            fluidRow(
                                              column(4),
                                              column(4,uiOutput("llag_analysis_id"))
                                            ),
                                            fluidRow(
                                              column(12,
                                                     div(align="center", numericInput("llag_plot_confidence", label = "Confidence Level",width = "20%",  value = 0.001, min = 0, max = 1, step = 0.0001)),
                                                     div(align="center", uiOutput("llag_plot_corr_method"))
                                              ),
                                              bsTooltip(id = "llag_plot_confidence", title = "The evaluated p-values should carefully be interpreted because they are calculated based on pointwise confidence intervals rather than simultaneous confidence intervals (so there would be a multiple testing problem). Evaluation of p-values based on the latter will be implemented in the future extension of this function: Indeed, so far no theory has been developed for this. However, it is conjectured that the error distributions of the estimated cross-correlation functions are asymptotically independent if the grid is not dense too much, so p-values evaluated by this function will still be meaningful as long as sufficiently low significance levels are used.")
                                            ),
                                            fluidRow(
                                              column(1),
                                              column(10,plotOutput("llag_plot", height = "600px"))
                                            ),
                                            fluidRow(
                                              column(1),
                                              column(2,actionButton("llag_delete_analysis", label = "Delete")),
                                              column(6),
                                              column(2,actionButton("llag_deleteAll_analysis", label = "Delete All"))
                                            ),
                                            HTML("<div id = 'llag_plot_howToRead'><h4><b>How to read the plot:</b><br/>If the lead-lag is positive: 'row.name' anticipates 'col.name of 'X' periods<br/>If the lead-lag is negative: 'row.name' follows 'col.name' with 'X' delay periods<br/><br/><b>'X'</b> are the numbers in the plot above.<br/>They are expressed in days if you are using time series, or in the same unit of measure of time if you are using numerical time index.<br/>The numbers in round brackets are correlations between the series shifted by the corresponding estimated lead-lag parameter.</h4></div>")
                        )))
        )
)