tabItem(tabName = "changepoint",
        fluidRow(
          column(12,
                 h3("Change Point Estimation",class = "hTitle"),
                 h4("Select the data you wish to estimate change points for.", br(),
                    "Choose the algorithm you want to use for estimation.", br(),
                    "Results will be shown below by plotting the series and the detected change points."),
                 hr(class = "hrHeader")
          )
        ),
        fluidRow(column(12,tabsetPanel(id = "panel_cpoint", type = "tabs",
                                       tabPanel(title = "Nonparametric",
                                                fluidRow(column(12,bsAlert("nonparametric_changepoint_alert"))),
                                                fluidRow(column(12,
                                                                column(4,
                                                                       h4("Available data"),
                                                                       DT::dataTableOutput("changepoint_table_select")
                                                                ),
                                                                column(4,
                                                                       h4("Selected data"),
                                                                       DT::dataTableOutput("changepoint_table_selected")
                                                                ),
                                                                column(4,br(),br(),br(),br(),
                                                                       div(align="center", selectInput("changepoint_method", "Method", choices = c("Percentage Increments Distribution"="KSperc", "Increments Distribution"="KSdiff"))),
                                                                       div(align="center", numericInput("changepoint_pvalue", label = "p-value", value=0.01, min=0, max=1))
                                                                )
                                                )),
                                                br(),
                                                fluidRow(column(12,
                                                                column(2,actionButton("changepoint_button_select",label = "Select", align = "center")),
                                                                bsTooltip("changepoint_button_select", title = "Select data", placement = "top"),
                                                                column(2,actionButton("changepoint_button_selectAll",label = "Select All", align = "center")),
                                                                bsTooltip("changepoint_button_selectAll", title = "Select all data that are displayed", placement = "top"),
                                                                column(2,actionButton("changepoint_button_delete",label = "Delete", align = "center")),
                                                                bsTooltip("changepoint_button_delete", title = "Delete selected data", placement = "top"),
                                                                column(2,actionButton("changepoint_button_deleteAll",label = "Delete All", align = "center")),
                                                                bsTooltip("changepoint_button_deleteAll", title = "Delete all data that are displayed", placement = "top"),
                                                                column(4,actionButton("changepoint_button_startEstimation", label = "Start Estimation", align = "center"))
                                                )),
                                                br(),br(),
                                                fluidRow(column(12,shinyjs::hidden(div(id="changepoint_charts",
                                                                                       hr(class = "hrHeader"),
                                                                                       uiOutput("changepoint_symb", align="center"),
                                                                                       div(fluidRow(
                                                                                         column(6, div(align = "left", selectInput("changepoint_scale", label = "Scale", choices=c("Linear","Logarithmic (Y)","Logarithmic (X)", "Logarithmic (XY)"), width = "150px"))),
                                                                                         column(6, div(align = "right", br(), a(id = "linkChangePointInfo", "Change Points Info", style = "font-size: 140%;", href = "")))
                                                                                       )),
                                                                                       bsModal(id = "ChangePointInfo", trigger = "linkChangePointInfo", title = "Change Points Info",
                                                                                               column(12,
                                                                                                      fluidRow(uiOutput("text_ChangePointInfo")),
                                                                                                      br(),
                                                                                                      fluidRow(div(tableOutput("table_ChangePointInfo"), align="center"))
                                                                                               )
                                                                                       ),
                                                                                       fluidRow(
                                                                                         column(6,plotOutput("changepoint_plot_series", brush = brushOpts(id = "changePoint_brush", delayType = "debounce", delay = 10000, resetOnNew = TRUE), dblclick = "changePoint_dbclick")),
                                                                                         column(6,plotOutput("changepoint_plot_incr", brush = brushOpts(id = "changePoint_brush", delayType = "debounce", delay = 10000, resetOnNew = TRUE), dblclick = "changePoint_dbclick"))
                                                                                       ),br(),
                                                                                       fluidRow(
                                                                                         column(8),
                                                                                         column(2,actionButton("changepoint_button_delete_estimated",label = "Delete", align = "center")),
                                                                                         bsTooltip("changepoint_button_delete_estimated", title = "Delete selected series", placement = "top"),
                                                                                         column(2,actionButton("changepoint_button_deleteAll_estimated",label = "Delete All", align = "center")),
                                                                                         bsTooltip("changepoint_button_deleteAll_estimated", title = "Delete all series", placement = "top")
                                                                                       )
                                                ))))
                                       ),
                                       tabPanel(title = "Parametric",
                                                fluidRow(column(12,bsAlert("parametric_changepoint_alert"))),
                                                fluidRow(column(12,
                                                                column(4,
                                                                       h4("Available data"),
                                                                       DT::dataTableOutput("parametric_changepoint_table_select")
                                                                ),
                                                                column(4,
                                                                       h4("Selected data"),
                                                                       DT::dataTableOutput("parametric_changepoint_table_selected")
                                                                ),
                                                                column(4,br(),br(),div(align="center", 
                                                                                       uiOutput("parametric_changepoint_model"),
                                                                                       sliderInput("parametric_modal_rangeFraction", label = "Training set (%)",min = 0, max = 100, value = c(20,80), step = 1, ticks = F),
                                                                                       br(),
                                                                                       column(6,div(actionButton("parametric_button_setRange", width = '95%', label = "Set Range"), align = "center")),
                                                                                       column(6,div(actionButton("parametric_button_settings", width = '95%', label = "Advanced Settings"), align = "center"))
                                                                )),
                                                                bsModal(id="parametric_plotsRange", trigger = "parametric_button_setRange", title = "Select range to use for change point estimation", size = "large",
                                                                        div(id="parametric_plotsRangeErrorMessage",align = "center",h3("Select some series from table 'Available Data'", class = "hModal")),
                                                                        div(id="parametric_plotsRangeAll",
                                                                            fluidRow(
                                                                              column(8,
                                                                                     plotOutput("parametric_selectRange", height = "350px", brush = brushOpts(id = "parametric_selectRange_brush", delayType = "debounce", delay = 10000, resetOnNew = TRUE), dblclick = "parametric_selectRange_dbclick"),
                                                                                     br(),
                                                                                     plotOutput("parametric_selectRangeReturns", height = "350px", brush = brushOpts(id = "parametric_selectRange_brush", delayType = "debounce", delay = 10000, resetOnNew = TRUE), dblclick = "parametric_selectRange_dbclick")
                                                                              ),
                                                                              column(4,
                                                                                     div(selectInput("parametric_scale_selectRange", label = "Chart Scale", choices = c("Linear", "Logarithmic (Y)", "Logarithmic (X)", "Logarithmic (XY)")), align = "center"),
                                                                                     br(),br(),br(),
                                                                                     uiOutput("parametric_plotsRangeSeries", align = "center"),
                                                                                     uiOutput("parametric_chooseRange", align = "center"),
                                                                                     uiOutput("parametric_chooseRange_specify", align = "center"),
                                                                                     column(6,
                                                                                            tags$button(type="button", id="parametric_buttonApplyRange", class = "action-button", em("Apply")),
                                                                                            bsTooltip("parametric_buttonApplyRange", title = "Apply Range to selected symbol", placement = "top")
                                                                                     ),
                                                                                     column(6,
                                                                                            tags$button(type="button", id="parametric_buttonApplyAllRange", class = "action-button", em("Apply All")),
                                                                                            bsTooltip("parametric_buttonApplyAllRange", title = "Apply Range to all symbols that are displayed", placement = "bottom")
                                                                                     )
                                                                              )
                                                                            )
                                                                        )
                                                                ),
                                                                bsModal(id="parametric_modal_id", title="Advanced Settings", trigger = "parametric_button_settings", size = "large",
                                                                        div(id="parametric_modal_errorMessage", align = "center", h3("Select some series (from table 'Available Data')", class = "hModal")),
                                                                        div(id="parametric_modal_body",
                                                                            fluidRow(
                                                                              column(6,
                                                                                     box(width = 12,div(align="center",
                                                                                                        h3("Series Settings", class = "hModal"),
                                                                                                        uiOutput("parametric_modal_series", align="center"),
                                                                                                        fluidRow(
                                                                                                          column(6,uiOutput("parametric_modal_delta", align="center")),
                                                                                                          column(6,uiOutput("parametric_modal_toLog", align="center"))       
                                                                                                        ),
                                                                                                        fluidRow(
                                                                                                          column(6, tags$button(type="button", id="parametric_modal_button_applyDelta", class = "action-button", em("Apply"))),
                                                                                                          column(6, tags$button(type="button", id="parametric_modal_button_applyAllDelta", class = "action-button", em("Apply to All series")))
                                                                                                        )
                                                                                     )),
                                                                                     box(width = 12,div(align="center",
                                                                                                        h3("General Settings", class = "hModal"),
                                                                                                        uiOutput("parametric_modal_method", align="center"),
                                                                                                        fluidRow(
                                                                                                          column(6,uiOutput("parametric_modal_trials", align="center")),
                                                                                                          column(6,uiOutput("parametric_modal_seed", align="center"))
                                                                                                        ),
                                                                                                        fluidRow(
                                                                                                          column(6, tags$button(type="button", id="parametric_modal_button_applyGeneral", class = "action-button", em("Apply"))),
                                                                                                          column(6, tags$button(type="button", id="parametric_modal_button_applyAllModelGeneral", class = "action-button", em("Apply to All series")))
                                                                                                        )
                                                                                     ))
                                                                              ),
                                                                              column(6,
                                                                                     box(width = 12,div(align="center",
                                                                                                        h3("Model Settings", class = "hModal"),
                                                                                                        uiOutput("parametric_modal_model", align="center"),
                                                                                                        uiOutput("parametric_modal_parameter", align="center"),
                                                                                                        uiOutput("parametric_modal_start", align="center"),
                                                                                                        fluidRow(
                                                                                                          column(6,uiOutput("parametric_modal_startMin", align="center")),
                                                                                                          column(6,uiOutput("parametric_modal_startMax", align="center"))
                                                                                                        ),
                                                                                                        fluidRow(
                                                                                                          column(6,uiOutput("parametric_modal_lower", align="center")),
                                                                                                          column(6,uiOutput("parametric_modal_upper", align="center"))
                                                                                                        ),
                                                                                                        fluidRow(
                                                                                                          column(6, tags$button(type="button", id="parametric_modal_button_applyModel", class = "action-button", em("Apply"))),
                                                                                                          column(6, tags$button(type="button", id="parametric_modal_button_applyAllModel", class = "action-button", em("Apply to All series")))
                                                                                                        )
                                                                                     ))
                                                                              )
                                                                            )
                                                                        )
                                                                )
                                                )),
                                                br(),
                                                fluidRow(column(12,
                                                                column(2,actionButton("parametric_changepoint_button_select",label = "Select", align = "center")),
                                                                bsTooltip("changepoint_button_select", title = "Select data", placement = "top"),
                                                                column(2,actionButton("parametric_changepoint_button_selectAll",label = "Select All", align = "center")),
                                                                bsTooltip("changepoint_button_selectAll", title = "Select all data that are displayed", placement = "top"),
                                                                column(2,actionButton("parametric_changepoint_button_delete",label = "Delete", align = "center")),
                                                                bsTooltip("changepoint_button_delete", title = "Delete selected data", placement = "top"),
                                                                column(2,actionButton("parametric_changepoint_button_deleteAll",label = "Delete All", align = "center")),
                                                                bsTooltip("changepoint_button_deleteAll", title = "Delete all data that are displayed", placement = "top"),
                                                                column(4,actionButton("parametric_changepoint_button_startEstimation", label = "Start Estimation", align = "center"))
                                                )),
                                                br(),br(),
                                                fluidRow(column(12,shinyjs::hidden(div(id="parametric_changepoint_charts",
                                                                                       hr(class = "hrHeader"),
                                                                                       uiOutput("parametric_changepoint_symb", align="center"),
                                                                                       div(fluidRow(
                                                                                         column(12, selectInput("parametric_changepoint_scale", label = "Scale", choices=c("Linear","Logarithmic (Y)","Logarithmic (X)", "Logarithmic (XY)"), width = "150px"))
                                                                                       )),
                                                                                       fluidRow(
                                                                                         column(6,plotOutput("parametric_changepoint_plot_series", brush = brushOpts(id = "parametric_changePoint_brush", delayType = "debounce", delay = 10000, resetOnNew = TRUE), dblclick = "parametric_changePoint_dbclick")),
                                                                                         column(6,
                                                                                                div(uiOutput("parametric_changepoint_info"),
                                                                                                    a(id = "parametric_linkChangePointInfo", "Change Point Info", style = "font-size: 140%;", href = ""),
                                                                                                    align="center"),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                                                                fluidRow(
                                                                                                  column(2),
                                                                                                  column(4,actionButton("parametric_changepoint_button_delete_estimated",label = "Delete", align = "center")),
                                                                                                  bsTooltip("parametric_changepoint_button_delete_estimated", title = "Delete selected series", placement = "top"),
                                                                                                  column(4,actionButton("parametric_changepoint_button_deleteAll_estimated",label = "Delete All", align = "center")),
                                                                                                  bsTooltip("parametric_changepoint_button_deleteAll_estimated", title = "Delete all series", placement = "top")
                                                                                                )
                                                                                         )
                                                                                       ),
                                                                                       bsModal(id = "parametric_changepoint_modal_info", title = "Change Point Info", trigger = "parametric_linkChangePointInfo",
                                                                                               fluidRow(column(12, uiOutput("parametric_changepoint_modal_info_text"))), br(),
                                                                                               fluidRow(
                                                                                                 column(6, div(h5("Estimates before the Change Point", class = "hModal"), tableOutput("parametric_changepoint_modal_info_tableL"), align="center")),
                                                                                                 column(6, div(h5("Estimates after the Change Point", class = "hModal"), tableOutput("parametric_changepoint_modal_info_tableR"), align="center"))
                                                                                               )
                                                                                               
                                                                                       )
                                                ))))
                                       )
        )))
)