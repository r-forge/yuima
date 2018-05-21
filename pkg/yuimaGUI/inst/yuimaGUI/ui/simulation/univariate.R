tabItem(tabName = "simulate",
        fluidRow(
          column(12,
                 h3("Univariate Simulation",class = "hTitle"),
                 h4("Select the estimated models you wish to simulate.",
                    br(),
                    "If you want to simulate a model that has not been estimated, you can use the 'Non-estimated models' tab.",
                    br(),
                    "Click on the 'Set Simulation' and 'Advanced Settings' buttons to customize the simulation process."),
                 hr(class = "hrHeader")
          )
        ),
        fluidRow(column(12,tabsetPanel(id = "panel_simulations", type = "tabs",
                                       tabPanel(title = "Estimated models",
                                                fluidRow(column(12,bsAlert("panel_simulate_model_alert"))),
                                                fluidRow(column(12, br(),
                                                                h4("Available models"),
                                                                DT::dataTableOutput("simulate_databaseModels"),
                                                                br(),
                                                                fluidRow(
                                                                  column(8),
                                                                  column(2,actionButton(inputId = "simulate_button_selectModels", label = "Select")),
                                                                  bsTooltip("simulate_button_selectModels", title = "Select model", placement = "top"),
                                                                  column(2,actionButton(inputId = "simulate_button_selectAllModels", label = "Select All")),
                                                                  bsTooltip("simulate_button_selectAllModels", title = "Select all models that are displayed", placement = "top")
                                                                )
                                                )
                                                )),
                                       tabPanel(title = "Non-estimated models",
                                                fluidRow(column(12,bsAlert("panel_simulate_equation_alert"))),
                                                fluidRow(
                                                  uiOutput("simulate_PrintModelLatex")       
                                                ),
                                                fluidRow(
                                                  column(6, br(), div(align="center",
                                                                      fluidRow(
                                                                        column(1),
                                                                        column(5,selectInput("simulate_model_usr_selectClass", label = "Class", choices = c("Diffusion process", "Fractional process", "Compound Poisson", "Point Process"))),
                                                                        column(5,uiOutput("simulate_model_usr_selectModel"))
                                                                      ),
                                                                      uiOutput("simulate_model_usr_ID"),
                                                                      uiOutput("simulate_model_usr_selectJumps"),
                                                                      fluidRow(
                                                                        column(1),
                                                                        column(5,uiOutput("simulate_model_usr_selectParam")),
                                                                        column(5,uiOutput("simulate_model_usr_param"))
                                                                      ),
                                                                      fluidRow(
                                                                        column(4),
                                                                        column(4,actionButton("simulate_model_usr_button_save", label = "Save", align = "center"))
                                                                      )
                                                  )),
                                                  column(6,
                                                         br(),
                                                         DT::dataTableOutput("simulate_model_usr_table"),
                                                         br(),
                                                         fluidRow(
                                                           column(3,actionButton("simulate_model_usr_button_select", label = "Select")),
                                                           column(3,actionButton("simulate_model_usr_button_selectAll", label = "Select All")),
                                                           column(3,actionButton("simulate_model_usr_button_delete", label = "Delete")),
                                                           column(3,actionButton("simulate_model_usr_button_deleteAll", label = "Delete All"))
                                                         )
                                                  )
                                                )
                                       ),
                                       tabPanel(title = "Simulations",
                                                fluidRow(column(12,bsAlert("panel_simulations_alert"))),
                                                br(),
                                                fluidRow(column(12, DT::dataTableOutput("simulate_monitor_table"))),
                                                br(),
                                                fluidRow(
                                                  column(2,actionButton(inputId = "simulate_monitor_button_showSimulation", label = "Show Simulations")),
                                                  bsTooltip("simulate_monitor_button_showSimulation", title = "Show selected simulation", placement = "top"),
                                                  column(6),
                                                  column(2,actionButton(inputId = "simulate_monitor_button_delete", label = "Delete")),
                                                  bsTooltip("simulate_monitor_button_delete", title = "Delete selected simulation", placement = "top"),
                                                  column(2,actionButton(inputId = "simulate_monitor_button_deleteAll", label = "Delete All")),
                                                  bsTooltip("simulate_monitor_button_deleteAll", title = "Delete all simulations that are displayed", placement = "top")
                                                )
                                       )
        ))),
        div(id="div_simulations", 
            fluidRow(
              column(12,br(),br(),br()),
              column(8,
                     h4("Selected Models"),
                     DT::dataTableOutput("simulate_selectedModels")
              ),
              column(4,
                     br(),br(),br(),br(),br(),br(),
                     div(actionButton("simulate_button_setSimulation", label = "Set Simulation"), align = "center"),
                     br(),
                     div(actionButton("simulate_button_advancedSettings", label = "Advanced Settings", align = "center"), align = "center")
              )
            ),
            br(),
            fluidRow(
              column(4,actionButton("simulation_button_deleteModels",label = "Delete", align = "center")),
              bsTooltip("simulation_button_deleteModels", title = "Delete selected models", placement = "top"),
              column(4,actionButton("simulation_button_deleteAllModels",label = "Delete All", align = "center")),
              bsTooltip("simulation_button_deleteAllModels", title = "Delete all models that are displayed", placement = "top"),
              column(4,actionButton("simulate_simulateModels", label = "Start Simulation", align = "center"))
            )
        ),
        bsModal(id="simulate_showSimulation", trigger = "simulate_monitor_button_showSimulation", title = "Simulation", size = "large",
                fluidRow(column(12,
                                fluidRow(column(8,div(align="center",uiOutput("simulate_showSimulation_simID")))),
                                fluidRow(div(id="simulate_showSimulation_plot_div", align = "center",
                                             column(8,
                                                    plotOutput("simulate_showSimulation_plot", height = "350px", click = "simulate_showSimulation_plot_click")
                                             ),
                                             column(4,br(),br(),br(),
                                                    div(selectInput("simulate_showSimulation_plot_scale", label = "Chart Scale", choices = c("Linear", "Logarithmic (Y)", "Logarithmic (X)", "Logarithmic (XY)")), align = "center"),
                                                    br(),br(),br(),br(),br(),br(),br(),
                                                    downloadButton(outputId = "simulate_showSimulation_button_saveTrajectory", label = "Save Trajectories")
                                             )
                                )),
                                br(),
                                fluidRow(div(id="simulate_showSimulation_hist_div",
                                             column(8,
                                                    plotOutput("simulate_showSimulation_hist", height = "350px")
                                             ),
                                             column(4,
                                                    div(align="center",br(),br(),br(),
                                                        sliderInput("simulate_showSimulation_hist_nBins", width = "75%",min = 1, max = 100, step = 1,value = 30, ticks = FALSE, label = "Adjust bin width"),
                                                        sliderInput("simulate_showSimulation_hist_probability_slider", width = "75%", min = 0, max = 100, value = c(5, 95), label = "Quantiles (%)", step = 0.01, ticks=FALSE),
                                                        uiOutput("simulate_showSimulation_hist_text"),
                                                        br(),
                                                        downloadButton(outputId = "simulate_showSimulation_button_saveHist", label = "Save Histogram")
                                                    )
                                             )
                                ))
                ))
        ),
        bsModal(id="simulate_setSimulation", trigger = "simulate_button_setSimulation", title = "Set Simulation", size = "small",
                tags$style(type = "text/css", ".datepicker{z-index: 1100 !important;}"),
                div(id="simulate_setSimulation_errorMessage",align = "center", h3("Select some models first", class = "hModal")),
                div(id="simulate_setSimulation_body", align = "center",
                    uiOutput("simulate_modelID"),
                    br(),
                    box(width = 12,
                        uiOutput("simulate_range"),
                        column(6,tags$button(type="button", id="simulate_button_apply_range", class = "action-button", em("Apply"))),
                        column(6,tags$button(type="button", id="simulate_button_applyAll_range", class = "action-button", em("Apply All")))
                    ),
                    box(width =12,
                        uiOutput("simulate_xinit"),
                        column(6,tags$button(type="button", id="simulate_button_apply_xinit", class = "action-button", em("Apply"))),
                        column(6,tags$button(type="button", id="simulate_button_applyAll_xinit", class = "action-button", em("Apply All")))
                    ),
                    box(width = 12,
                        uiOutput("simulate_nsim"),
                        uiOutput("simulate_nstep"),
                        column(6,tags$button(type="button", id="simulate_button_apply_nsim", class = "action-button", em("Apply"))),
                        column(6,tags$button(type="button", id="simulate_button_applyAll_nsim", class = "action-button", em("Apply All")))
                    )
                )
        ),
        bsModal(id="simulate_advancedSettings", trigger = "simulate_button_advancedSettings", title = "Advanced Settings", size = "small",
                div(id="simulate_advancedSettings_errorMessage", align = "center", h3("Select some models first", class = "hModal")),
                div(id="simulate_advancedSettings_body", align = "center",
                    uiOutput("simulate_advancedSettings_modelID"),
                    uiOutput("simulate_seed"),
                    uiOutput("simulate_traj"),
                    column(6,tags$button(type="button", id="simulate_button_apply_advancedSettings", class = "action-button", em("Apply"))),
                    column(6,tags$button(type="button", id="simulate_button_applyAll_advancedSettings", class = "action-button", em("Apply All")))
                )
        )
)