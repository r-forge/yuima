tabItem(tabName="models", 
        fluidRow(column(12,
                        fluidRow(
                          column(12,
                                 h3("Univariate Model Estimation",class = "hTitle"),
                                 h4("Select the data and the model you wish to estimate. Each model will be fitted to each selected series.",
                                    br(),
                                    "Click on the Set Range and Advanced Settings buttons to customize the estimation process.",
                                    br(),
                                    "A number of default models are available but you can set your own model (tab Set model) and use it for estimation and/or simulation purposes."),
                                 hr(class = "hrHeader")
                          )
                        ),
                        fluidRow(column(12,tabsetPanel(id = "panel_estimates", type = "tabs",
                                                       tabPanel(title = "Run estimation",
                                                                fluidRow(column(12,bsAlert("panel_run_estimation_alert"))),
                                                                br(),
                                                                fluidRow(
                                                                  column(4,div(align="center",
                                                                               selectInput("modelClass",label = "Model Class", choices = c("Diffusion process", "Fractional process", "Compound Poisson","Point Process", #"Levy process", 
                                                                                                                                           "CARMA", "COGARCH"), selected = "Diffusion process"),
                                                                               uiOutput("model"),
                                                                               uiOutput("jumps"),
                                                                               uiOutput("pq_C")
                                                                  )),
                                                                  column(5,
                                                                         fluidRow(shinyjs::hidden(h4(id="titlePrintModelLatex","Models to estimate:", style="font-size: 2em;"))),
                                                                         fluidRow(uiOutput("PrintModelLatex"))
                                                                  )
                                                                ),
                                                                br(),
                                                                fluidRow(
                                                                  column(4,
                                                                         h4("Available data"),
                                                                         DT::dataTableOutput("database3")
                                                                  ),
                                                                  column(4,
                                                                         h4("Selected data"),
                                                                         DT::dataTableOutput("database4")
                                                                  ),
                                                                  column(4,
                                                                         br(),br(),br(),br(),br(),br(),
                                                                         div(actionButton("DisplayPlotsRange", label = "Set Range"), align = "center"),
                                                                         br(),
                                                                         div(actionButton("advancedSettingsButton", label = "Advanced Settings", align = "center"), align = "center")
                                                                  )
                                                                ),
                                                                br(),
                                                                fluidRow(
                                                                  column(2,actionButton("buttonSelect_models_Univariate",label = "Select", align = "center")),
                                                                  bsTooltip("buttonSelect_models_Univariate", title = "Select data to model", placement = "top"),
                                                                  column(2,actionButton("buttonSelectAll_models_Univariate",label = "Select All", align = "center")),
                                                                  bsTooltip("buttonSelectAll_models_Univariate", title = "Select all data that are displayed", placement = "top") ,
                                                                  column(2,actionButton("buttonDelete_models_Univariate",label = "Delete", align = "center")),
                                                                  bsTooltip("buttonDelete_models_Univariate", title = "Delete selected data", placement = "top"),
                                                                  column(2,actionButton("buttonDeleteAll_models_Univariate",label = "Delete All", align = "center")),
                                                                  bsTooltip("buttonDeleteAll_models_Univariate", title = "Delete all data that are displayed", placement = "top"),
                                                                  column(4,actionButton("EstimateModels", label = "Start Models Estimation", align = "center"))
                                                                )
                                                       ),
                                                       tabPanel(title = "Set model",
                                                                fluidRow(column(12,bsAlert("panel_set_model_alert"))),
                                                                br(),
                                                                fluidRow(div(align="center",
                                                                             column(6,
                                                                                    fluidRow(selectInput("usr_modelClass",label = "Model Class", width = "50%", choices = c("Diffusion process", "Fractional process", "Compound Poisson"), selected = "Diffusion process")),
                                                                                    fluidRow(textInput("usr_model_name", label = "Model Name", width = "50%")),
                                                                                    fluidRow(uiOutput("usr_modelClass_latex")),
                                                                                    fluidRow(uiOutput("usr_model_coeff")),
                                                                                    br(),br(),
                                                                                    fluidRow(
                                                                                      column(4),
                                                                                      column(4,actionButton("usr_model_button_save", label = "Save Model"))
                                                                                    )
                                                                             ),
                                                                             column(6,div(id="usr_model_saved_div",align="center",
                                                                                          uiOutput("usr_model_saved"),
                                                                                          uiOutput("usr_model_saved_latex"),
                                                                                          br(),
                                                                                          actionButton("usr_model_button_delete", label = "Delete Model(s)")
                                                                             ))
                                                                ))
                                                       ),
                                                       tabPanel(title = "Estimates",
                                                                fluidRow(column(12,bsAlert("panel_estimates_alert"))),
                                                                shinyjs::hidden(div(id="estimates_info", fluidRow(
                                                                  column(12,
                                                                         textOutput("SymbolName"),
                                                                         a(id = "linkMoreInfo", tags$u("More Info"), href = "#"),
                                                                         bsModal(id = "MoreInfo", trigger = "linkMoreInfo", title = "Info", size = "large",
                                                                                 column(12,
                                                                                        fluidRow(uiOutput("text_MoreInfo")),
                                                                                        br(),
                                                                                        fluidRow(div(tableOutput("table_MoreInfo"), align="center")),
                                                                                        bsTooltip(id = "table_MoreInfo" ,"Estimates and Std. Errors are coherent with delta that has been used. No conversion to other units of measure has been applied.")
                                                                                 )
                                                                         ),
                                                                         uiOutput("estimatedModelsLatex")
                                                                  ),
                                                                  column(12,
                                                                         div(align = "center",
                                                                             tableOutput("estimatedModelsTable"),
                                                                             shinyjs::hidden(selectInput(inputId = "baseModels", label = "Base", width = "150px",  choices = c("Yearly","Semestral","Quarterly","Trimestral","Bimestral","Monthly","Weekly","Daily"), selected = "Yearly"))
                                                                         )
                                                                  )
                                                                ))),
                                                                fluidRow(
                                                                  column(12, br(), DT::dataTableOutput("databaseModels"))
                                                                ),
                                                                br(),
                                                                fluidRow(
                                                                  column(2,actionButton(inputId = "databaseModels_button_showResults", label = "Show Fitting")),
                                                                  bsTooltip("databaseModels_button_showResults", title = "Available for: Diffusive Processes, Compound Poisson and COGARCH", placement = "top"),
                                                                  column(6),
                                                                  column(2,actionButton(inputId = "databaseModelsDelete", label = "Delete")),
                                                                  bsTooltip("databaseModelsDelete", title = "Delete selected model", placement = "top"),
                                                                  column(2,actionButton(inputId = "databaseModelsDeleteAll", label = "Delete All")),
                                                                  bsTooltip("databaseModelsDeleteAll", title = "Delete all models that are displayed", placement = "top")
                                                                ),
                                                                bsModal(id = "model_modal_fitting", title = "Fitting", trigger = "databaseModels_button_showResults", size = "Large",
                                                                        div(id = "model_modal_fitting_body",
                                                                            fluidRow(
                                                                              column(2),
                                                                              column(8, uiOutput("model_modal_model_id", align = "center"))
                                                                            ),
                                                                            fluidRow(
                                                                              column(12, 
                                                                                     plotOutput("model_modal_plot_variance"),
                                                                                     plotOutput("model_modal_plot_intensity"),
                                                                                     plotOutput("model_modal_plot_distr"),
                                                                                     uiOutput("model_modal_plot_test", align = "center")
                                                                              )
                                                                            )
                                                                        )
                                                                )
                                                       )
                        ))),
                        bsModal(id="plotsRange", trigger = "DisplayPlotsRange", title = "Select range to use for models estimation", size = "large",
                                div(id="plotsRangeErrorMessage",align = "center",h3("Select some series from table 'Available Data'", class = "hModal")),
                                div(id="plotsRangeAll",
                                    fluidRow(
                                      column(8,
                                             plotOutput("selectRange", height = "350px", brush = brushOpts(id = "selectRange_brush", delayType = "debounce", delay = 10000, resetOnNew = TRUE), dblclick = "selectRange_dbclick"),
                                             br(),
                                             plotOutput("selectRangeReturns", height = "350px", brush = brushOpts(id = "selectRange_brush", delayType = "debounce", delay = 10000, resetOnNew = TRUE), dblclick = "selectRange_dbclick")
                                      ),
                                      column(4,
                                             div(selectInput("scale_selectRange", label = "Chart Scale", choices = c("Linear", "Logarithmic (Y)", "Logarithmic (X)", "Logarithmic (XY)")), align = "center"),
                                             br(),br(),br(),
                                             uiOutput("plotsRangeSeries", align = "center"),
                                             uiOutput("chooseRange", align = "center"),
                                             uiOutput("chooseRange_specify", align = "center"),
                                             column(6,
                                                    tags$button(type="button", id="buttonApplyRange", class = "action-button", em("Apply")),
                                                    bsTooltip("buttonApplyRange", title = "Apply Range to selected symbol", placement = "top")
                                             ),
                                             column(6,
                                                    tags$button(type="button", id="buttonApplyAllRange", class = "action-button", em("Apply All")),
                                                    bsTooltip("buttonApplyAllRange", title = "Apply Range to all symbols that are displayed", placement = "bottom")
                                             )
                                      )
                                    )
                                )
                        ),
                        bsModal(id="advancedSettings", title="Advanced Settings", trigger = "advancedSettingsButton", size = "large",
                                div(id="advancedSettingsErrorMessage",align = "center",h3("Select some models and series (from table 'Available Data')", class = "hModal")),
                                div(id="advancedSettingsAll",
                                    fluidRow(
                                      column(6,
                                             box(width = 12,div(align="center",
                                                                h3("Series Settings", class = "hModal"),
                                                                uiOutput("advancedSettingsSeries", align="center"),
                                                                fluidRow(
                                                                  column(6,uiOutput("advancedSettingsDelta", align="center")),
                                                                  column(6,uiOutput("advancedSettingsToLog", align="center"))       
                                                                ),
                                                                fluidRow(
                                                                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyDelta", class = "action-button", em("Apply"))),
                                                                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyAllDelta", class = "action-button", em("Apply to All series")))
                                                                )
                                             )),
                                             box(width = 12,div(align="center",
                                                                h3("General Settings", class = "hModal"),
                                                                uiOutput("advancedSettingsMethod", align="center"),
                                                                uiOutput("advancedSettingsThreshold", align="center"),
                                                                fluidRow(
                                                                  column(6,uiOutput("advancedSettingsTrials", align="center")),
                                                                  column(6,uiOutput("advancedSettingsSeed", align="center"))
                                                                ),
                                                                uiOutput("advancedSettingsJoint", align="center"),
                                                                uiOutput("advancedSettingsAggregation", align="center"),
                                                                uiOutput("advancedSettingsTimeout", align="center"),
                                                                fluidRow(
                                                                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyGeneral", class = "action-button", em("Apply"))),
                                                                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyAllModelGeneral", class = "action-button", em("Apply to All series")))
                                                                ),
                                                                fluidRow(column(12, tags$button(type="button", id="advancedSettingsButtonApplyAllGeneral", class = "action-button", em("Apply to All series & models"))))
                                             ))
                                      ),
                                      column(6,
                                             box(width = 12,div(align="center",
                                                                h3("Model Settings", class = "hModal"),
                                                                uiOutput("advancedSettingsModel", align="center"),
                                                                uiOutput("advancedSettingsParameter", align="center"),
                                                                uiOutput("advancedSettingsFixed", align="center"),
                                                                uiOutput("advancedSettingsStart", align="center"),
                                                                fluidRow(
                                                                  column(6,uiOutput("advancedSettingsStartMin", align="center")),
                                                                  column(6,uiOutput("advancedSettingsStartMax", align="center"))
                                                                ),
                                                                fluidRow(
                                                                  column(6,uiOutput("advancedSettingsLower", align="center")),
                                                                  column(6,uiOutput("advancedSettingsUpper", align="center"))
                                                                ),
                                                                fluidRow(
                                                                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyModel", class = "action-button", em("Apply"))),
                                                                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyAllModel", class = "action-button", em("Apply to All series")))
                                                                )
                                             ))
                                      )
                                    )
                                )
                        )
        ))
)