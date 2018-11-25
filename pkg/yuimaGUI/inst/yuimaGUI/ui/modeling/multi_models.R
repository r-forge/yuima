tabItem(tabName="multi_models", 
        fluidRow(column(12,
                        fluidRow(
                          column(12,
                                 h3("Multivariate Model Estimation",class = "hTitle"),
                                 h4("Select the data and the model you wish to estimate. The model will be fitted to the selected series.",
                                    br(),
                                    "Click on buttons Set Range and Advanced Settings to customize the estimation process.",
                                    br(),
                                    'Building your own multivariate model is not possible at the moment.'
                                    #"Some default models are available but you can set your own model (tab 'Set model') and use it for estimation and/or simulation purposes."
                                    ),
                                 hr(class = "hrHeader")
                          )
                        ),
                        fluidRow(column(12,tabsetPanel(id = "multi_panel_estimates", type = "tabs",
                                                       tabPanel(title = "Run estimation",
                                                                fluidRow(column(12,bsAlert("multi_panel_run_estimation_alert"))),
                                                                br(),
                                                                fluidRow(
                                                                  column(4,div(align="center",
                                                                               selectInput("multi_modelClass",label = "Model Class", choices = c("Diffusion process"), selected = "Diffusion process"),
                                                                               uiOutput("multi_model"),
                                                                               uiOutput("multi_jumps"),
                                                                               uiOutput("multi_pq_C")
                                                                  )),
                                                                  column(5,
                                                                         fluidRow(shinyjs::hidden(h4(id="multi_titlePrintModelLatex","Models to estimate:", style="font-size: 2em;"))),
                                                                         fluidRow(uiOutput("multi_PrintModelLatex"))
                                                                  )
                                                                ),
                                                                br(),
                                                                fluidRow(
                                                                  column(4,
                                                                         h4("Available data"),
                                                                         DT::dataTableOutput("multi_database3")
                                                                  ),
                                                                  column(4,
                                                                         h4("Selected data"),
                                                                         DT::dataTableOutput("multi_database4")
                                                                  ),
                                                                  column(4,
                                                                         br(),br(),br(),br(),br(),br(),
                                                                         div(actionButton("multi_DisplayPlotsRange", label = "Set Range"), align = "center"),
                                                                         br(),
                                                                         div(actionButton("multi_advancedSettingsButton", label = "Advanced Settings", align = "center"), align = "center")
                                                                  )
                                                                ),
                                                                br(),
                                                                fluidRow(
                                                                  column(2,actionButton("multi_buttonSelect_models_Univariate",label = "Select", align = "center")),
                                                                  bsTooltip("multi_buttonSelect_models_Univariate", title = "Select data to model", placement = "top"),
                                                                  column(2,actionButton("multi_buttonSelectAll_models_Univariate",label = "Select All", align = "center")),
                                                                  bsTooltip("multi_buttonSelectAll_models_Univariate", title = "Select all data that are displayed", placement = "top") ,
                                                                  column(2,actionButton("multi_buttonDelete_models_Univariate",label = "Delete", align = "center")),
                                                                  bsTooltip("multi_buttonDelete_models_Univariate", title = "Delete selected data", placement = "top"),
                                                                  column(2,actionButton("multi_buttonDeleteAll_models_Univariate",label = "Delete All", align = "center")),
                                                                  bsTooltip("multi_buttonDeleteAll_models_Univariate", title = "Delete all data that are displayed", placement = "top"),
                                                                  column(4,actionButton("multi_EstimateModels", label = "Start Models Estimation", align = "center"))
                                                                )
                                                       ),
                                                       # tabPanel(title = "Set model",
                                                       #          fluidRow(column(12,bsAlert("multi_panel_set_model_alert"))),
                                                       #          br(),
                                                       #          fluidRow(div(align="center",
                                                       #                       column(6,
                                                       #                              fluidRow(selectInput("multi_usr_modelClass",label = "Model Class", width = "50%", choices = c("Diffusion process", "Fractional process", "Compound Poisson"), selected = "Diffusion process")),
                                                       #                              fluidRow(textInput("multi_usr_model_name", label = "Model Name", width = "50%")),
                                                       #                              fluidRow(uiOutput("multi_usr_modelClass_latex")),
                                                       #                              fluidRow(uiOutput("multi_usr_model_coeff")),
                                                       #                              br(),br(),
                                                       #                              fluidRow(
                                                       #                                column(4),
                                                       #                                column(4,actionButton("multi_usr_model_button_save", label = "Save Model"))
                                                       #                              )
                                                       #                       ),
                                                       #                       column(6,div(id="multi_usr_model_saved_div",align="center",
                                                       #                                    uiOutput("multi_usr_model_saved"),
                                                       #                                    uiOutput("multi_usr_model_saved_latex"),
                                                       #                                    br(),
                                                       #                                    actionButton("multi_usr_model_button_delete", label = "Delete Model(s)")
                                                       #                       ))
                                                       #          ))
                                                       # ),
                                                       tabPanel(title = "Estimates",
                                                                fluidRow(column(12,bsAlert("multi_panel_estimates_alert"))),
                                                                shinyjs::hidden(div(id="multi_estimates_info", fluidRow(
                                                                  column(12,
                                                                         textOutput("multi_SymbolName"),
                                                                         a(id = "multi_linkMoreInfo", tags$u("More Info"), href = "#"),
                                                                         bsModal(id = "multi_MoreInfo", trigger = "multi_linkMoreInfo", title = "Info", size = "large",
                                                                                 column(12,
                                                                                        fluidRow(uiOutput("multi_text_MoreInfo")),
                                                                                        br(),
                                                                                        fluidRow(div(tableOutput("multi_table_MoreInfo"), align="center")),
                                                                                        bsTooltip(id = "multi_table_MoreInfo" ,"Estimates and Std. Errors are coherent with delta that has been used. No conversion to other units of measure has been applied.")
                                                                                 )
                                                                         ),
                                                                         uiOutput("multi_estimatedModelsLatex")
                                                                  ),
                                                                  column(12,
                                                                         div(align = "center",
                                                                             tableOutput("multi_estimatedModelsTable"),
                                                                             shinyjs::hidden(selectInput(inputId = "multi_baseModels", label = "Base", width = "150px",  choices = c("Yearly","Semestral","Quarterly","Trimestral","Bimestral","Monthly","Weekly","Daily"), selected = "Yearly"))
                                                                         )
                                                                  )
                                                                ))),
                                                                fluidRow(
                                                                  column(12, br(), DT::dataTableOutput("multi_databaseModels"))
                                                                ),
                                                                br(),
                                                                fluidRow(
                                                                  column(2,actionButton(inputId = "multi_databaseModels_button_showResults", label = "Show Fitting")),
                                                                  bsTooltip("multi_databaseModels_button_showResults", title = "Available for: Diffusive Processes", placement = "top"),
                                                                  column(6),
                                                                  column(2,actionButton(inputId = "multi_databaseModelsDelete", label = "Delete")),
                                                                  bsTooltip("multi_databaseModelsDelete", title = "Delete selected model", placement = "top"),
                                                                  column(2,actionButton(inputId = "multi_databaseModelsDeleteAll", label = "Delete All")),
                                                                  bsTooltip("multi_databaseModelsDeleteAll", title = "Delete all models that are displayed", placement = "top")
                                                                ),
                                                                bsModal(id = "multi_model_modal_fitting", title = "Fitting", trigger = "multi_databaseModels_button_showResults", size = "Large",
                                                                        div(id = "multi_model_modal_fitting_body",
                                                                            fluidRow(
                                                                              column(2),
                                                                              column(8, 
                                                                                     uiOutput("multi_model_modal_model_id", align = "center"),
                                                                                     uiOutput("multi_model_modal_series_id", align = "center")
                                                                                     )
                                                                            ),
                                                                            fluidRow(
                                                                              column(12, 
                                                                                     plotOutput("multi_model_modal_plot_variance"),
                                                                                     plotOutput("multi_model_modal_plot_intensity"),
                                                                                     plotOutput("multi_model_modal_plot_distr"),
                                                                                     uiOutput("multi_model_modal_plot_test", align = "center")
                                                                              )
                                                                            )
                                                                        )
                                                                )
                                                       )
                        ))),
                        bsModal(id="multi_plotsRange", trigger = "multi_DisplayPlotsRange", title = "Select range to use for models estimation", size = "large",
                                div(id="multi_plotsRangeErrorMessage",align = "center",h3("Select some series from table 'Available Data'", class = "hModal")),
                                div(id="multi_plotsRangeAll",
                                    fluidRow(
                                      column(8,
                                             plotOutput("multi_selectRange", height = "350px", brush = brushOpts(id = "multi_selectRange_brush", delayType = "debounce", delay = 10000, resetOnNew = TRUE), dblclick = "multi_selectRange_dbclick"),
                                             br(),
                                             plotOutput("multi_selectRangeReturns", height = "350px", brush = brushOpts(id = "multi_selectRange_brush", delayType = "debounce", delay = 10000, resetOnNew = TRUE), dblclick = "multi_selectRange_dbclick")
                                      ),
                                      column(4,
                                             div(selectInput("multi_scale_selectRange", label = "Chart Scale", choices = c("Linear", "Logarithmic (Y)", "Logarithmic (X)", "Logarithmic (XY)")), align = "center"),
                                             br(),br(),br(),
                                             uiOutput("multi_plotsRangeSeries", align = "center"),
                                             uiOutput("multi_chooseRange", align = "center"),
                                             uiOutput("multi_chooseRange_specify", align = "center"),
                                             column(6,
                                                    tags$button(type="button", id="multi_buttonApplyRange", class = "action-button", em("Apply")),
                                                    bsTooltip("multi_buttonApplyRange", title = "Apply Range to selected symbol", placement = "top")
                                             ),
                                             column(6,
                                                    tags$button(type="button", id="multi_buttonApplyAllRange", class = "action-button", em("Apply All")),
                                                    bsTooltip("multi_buttonApplyAllRange", title = "Apply Range to all symbols that are displayed", placement = "bottom")
                                             )
                                      )
                                    )
                                )
                        ),
                        bsModal(id="multi_advancedSettings", title="Advanced Settings", trigger = "multi_advancedSettingsButton", size = "large",
                                div(id="multi_advancedSettingsErrorMessage",align = "center",h3("Select some models and series (from table 'Available Data')", class = "hModal")),
                                div(id="multi_advancedSettingsAll",
                                    fluidRow(
                                      column(6,
                                             box(width = 12,div(align="center",
                                                                h3("Series Settings", class = "hModal"),
                                                                uiOutput("multi_advancedSettingsSeries", align="center"),
                                                                fluidRow(
                                                                  column(6,uiOutput("multi_advancedSettingsDelta", align="center")),
                                                                  column(6,uiOutput("multi_advancedSettingsToLog", align="center"))       
                                                                ),
                                                                fluidRow(
                                                                  column(6, tags$button(type="button", id="multi_advancedSettingsButtonApplyDelta", class = "action-button", em("Apply"))),
                                                                  column(6, tags$button(type="button", id="multi_advancedSettingsButtonApplyAllDelta", class = "action-button", em("Apply to All series")))
                                                                )
                                             )),
                                             box(width = 12,div(align="center",
                                                                h3("General Settings", class = "hModal"),
                                                                uiOutput("multi_advancedSettingsMethod", align="center"),
                                                                uiOutput("multi_advancedSettingsThreshold", align="center"),
                                                                fluidRow(
                                                                  column(6,uiOutput("multi_advancedSettingsTrials", align="center")),
                                                                  column(6,uiOutput("multi_advancedSettingsSeed", align="center"))
                                                                ),
                                                                uiOutput("multi_advancedSettingsJoint", align="center"),
                                                                uiOutput("multi_advancedSettingsAggregation", align="center"),
                                                                fluidRow(
                                                                  column(3),
                                                                  column(6, tags$button(type="button", id="multi_advancedSettingsButtonApplyGeneral", class = "action-button", em("Apply")))
                                                                )
                                             ))
                                      ),
                                      column(6,
                                             box(width = 12,div(align="center",
                                                                h3("Model Settings", class = "hModal"),
                                                                uiOutput("multi_advancedSettingsModel", align="center"),
                                                                uiOutput("multi_advancedSettingsParameter", align="center"),
                                                                uiOutput("multi_advancedSettingsFixed", align="center"),
                                                                uiOutput("multi_advancedSettingsStart", align="center"),
                                                                fluidRow(
                                                                  column(6,uiOutput("multi_advancedSettingsStartMin", align="center")),
                                                                  column(6,uiOutput("multi_advancedSettingsStartMax", align="center"))
                                                                ),
                                                                fluidRow(
                                                                  column(6,uiOutput("multi_advancedSettingsLower", align="center")),
                                                                  column(6,uiOutput("multi_advancedSettingsUpper", align="center"))
                                                                ),
                                                                fluidRow(
                                                                  column(3),
                                                                  column(6, tags$button(type="button", id="multi_advancedSettingsButtonApplyModel", class = "action-button", em("Apply")))
                                                                )
                                             ))
                                      )
                                    )
                                )
                        )
        )
        )
)