header<-dashboardHeader(
  title = "yuima"
)

sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data I/O", tabName = "data", icon = icon("upload"),
             menuSubItem("Financial & Economic Data", tabName = "finData"),
             menuSubItem("Your Data", tabName = "yourData")
             ),
    menuItem("Explorative Data Analysis", tabName = "eda", icon = icon("map"),
             menuSubItem("Clustering", tabName = "cluster")
             ),
    menuItem("Modelling & Model Selection", tabName = "models", icon = icon("sliders")),
    menuItem("Simulate", tabName = "simulate", icon = icon("area-chart")),
    hr(),
    menuItem("Miscellaneous", tabName = "misc",
             menuSubItem("...", tabName = "smth")
            ),
    menuItem("Finance", tabName = "finance",
             menuSubItem("Option Pricing", tabName = "opt")
            )
  )
)

body<-dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  shinyjs::useShinyjs(),
  withMathJax(),

  tabItems(
    tabItem(tabName = "home"
    ),
    #########################
    tabItem(tabName="finData",
      fluidRow(
        column(12,
          h3("In this section you can load financial and economic data.",style="color:#edeeed"),
          h4("For stock data choose Yahoo source using symbols you can find ",
             a("here", href="http://finance.yahoo.com/lookup", target = "_blank"), ".",
             br(),
             "For currencies and metals select Oanda source and type the two symbols divided by '/' (i.e. EUR/USD or XAU/USD ).",
             "Symbols are available ", a("here",href="http://www.oanda.com/help/currency-iso-code", target = "_blank"), ".",
             br(),
             "Economic series are available on ",a("Federal Reserve Bank of St. Louis", href="https://research.stlouisfed.org/fred2/", target = "_blank"), ".",
             "Find symbols as shown in this ", a("example",href="example.jpg", target = "_blank"), ".",
             br(),
             "Multiple symbols are allowed if divided by empty space and/or commas (i.e. AAPL FB CSCO or AAPL,FB,CSCO).",
             style="color:#CDCECD"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,bsAlert("finDataAlert"))),
      fluidRow(
        column(6,
          textInput(inputId="symb", value = NULL,label = span(style="color:#CDCECD", "Insert Symbol")),
          dateRangeInput(inputId="dR", label = span(style="color:#CDCECD", "Download data from"), start = "1900-01-01" ,end = Sys.Date()),
          selectInput(inputId="sources", label = span(style="color:#CDCECD", "Source"), choices = c("Yahoo (OHLC data)" = "yahoo", "Oanda (Currencies &  Metals)" = "oanda", "Federal Reserve Bank of St. Louis" = "FRED")),
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
      fluidRow(
        column(6),
        column(2,downloadButton(outputId = "finDataSave", label = "Save")),
        bsTooltip("finDataSave", title = "Save data to file", placement = "top"),
        column(2,actionButton(inputId = "finDataDelete", label = "Delete")),
        bsTooltip("finDataDelete", title = "Delete selected data", placement = "top"),
        column(2,actionButton(inputId = "finDataDeleteAll", label = "Delete All")),
        bsTooltip("finDataDeleteAll", title = "Delete all data that are displayed", placement = "top")
      )
    ),
    #########################
    tabItem("yourData",
      fluidRow(
        column(12,
          h3("In this section you can load data from your own files.",style="color:#edeeed"),
          h4("Please upload your file and specify its structure. A preview will be shown below.",
             br(),
             "First, declare if the file contains raw and/or column headers and specify what kind of field separator has to be used to read data.",
             br(),
             "Each column will be uploaded as a different series. So you might want to switch columns with rows if your file is organized differently.",
             br(),
             "Finally specify what column to use to index series and its format.",
              style="color:#CDCECD"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,bsAlert("yourDataAlert"))),
      fluidRow(
        column(5,
          fileInput(inputId = "yourFile", label = "Choose file to upload", multiple = FALSE),
          selectInput('yourFileHeader', 'Headers', choices = c("Default","Only columns", "Only rows", "Both", "None"), selected = "Default"),
          selectInput(inputId = 'yourFileSep', label = 'Field Separator', choices = c("Space"="default", "Comma"=',', "Semicolon"=';', "Tab"='\t'), selected = "default"),
          selectInput('yourFileSwitch', 'Switch rows/columns', choices = c("No"=FALSE, "Yes"=TRUE)),
          uiOutput("yourFileIndex"),
          uiOutput("yourFileFUN")
        ),
        column(7,
          textOutput("yourFilePreviewText"),
          DT::dataTableOutput("yourFilePreview"),
          br(),
          uiOutput("yourFileButton", align = "center")
        )
      ),
      br(),
      br(),
      fluidRow(
        column(12,
          DT::dataTableOutput("database2")
        )
      ),
      fluidRow(
        column(6),
        column(2, downloadButton(outputId = "yourFileSave", label = "Save")),
        bsTooltip("yourFileSave", title = "Save data to file", placement = "top"),
        column(2,actionButton(inputId = "yourFileDelete", label = "Delete")),
        bsTooltip("yourFileDelete", title = "Delete selected data", placement = "top"),
        column(2,actionButton(inputId = "yourFileDeleteAll", label = "Delete All")),
        bsTooltip("yourFileDeleteAll", title = "Delete all data that are displayed", placement = "top")
      )
    ),
    ########################
    tabItem(tabName="models", fluidRow(column(12,
      fluidRow(
        column(12,
          h3("In this section you can estimate models and/or define new ones.",style="color:#edeeed"),
          h4("To estimate models simply click those you are interested in and select data you wish to model. You can customize the estimation process by clicking on buttons 'Set Range' and 'Advanced Settings'.",
             br(),
             "Some default models are available but you can set your own model (tab 'Set model') and use it for estimation and/or simulation purposes.",
             br(),
             "Estimated models are shown in tab 'Estimates'.",
             style="color:#CDCECD"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,bsAlert("modelsAlert"))),
      fluidRow(column(12,tabsetPanel(id = "panel_estimates", type = "tabs",
        tabPanel(title = "Start estimation",
          br(),
          fluidRow(
            column(4,div(align="center",
              selectInput("modelClass",label = "Model Class", choices = c("Diffusion processes"), selected = "Diffusion processes"),
              uiOutput("model")
            )),
            column(5,
              shinyjs::hidden(h4(id="titlePrintModelLatex","Models to estimate:", style="color:#CDCECD;font-size: 2em; font-family: Goudy Old Style, Serif")),
              uiOutput("PrintModelLatex")
            )
          ),
          br(),
          fluidRow(
            column(4,
                   h4("Available data", style="color:#CDCECD"),
                   DT::dataTableOutput("database3"),
                   br(),
                   fluidRow(
                     column(6,actionButton("buttonSelect_models_Univariate",label = "Select", align = "center")),
                     bsTooltip("buttonSelect_models_Univariate", title = "Select data to model", placement = "top"),
                     column(6,actionButton("buttonSelectAll_models_Univariate",label = "Select All", align = "center")),
                     bsTooltip("buttonSelectAll_models_Univariate", title = "Select all data that are displayed", placement = "top")
                   )
            ),
            column(4,
                   h4("Selected data", style="color:#CDCECD"),
                   DT::dataTableOutput("database4"),
                   br(),
                   fluidRow(
                     column(6,actionButton("buttonDelete_models_Univariate",label = "Delete", align = "center")),
                     bsTooltip("buttonDelete_models_Univariate", title = "Delete selected data", placement = "top"),
                     column(6,actionButton("buttonDeleteAll_models_Univariate",label = "Delete All", align = "center")),
                     bsTooltip("buttonDeleteAll_models_Univariate", title = "Delete all data that are displayed", placement = "top")
                   )
            ),
            column(4,
                   br(),br(),br(),br(),
                   div(actionButton("DisplayPlotsRange", label = "Set Range"), align = "center"),
                   br(),
                   div(actionButton("advancedSettingsButton", label = "Advanced Settings", align = "center"), align = "center"),
                   br(),br(),br(),br(),br(),
                   actionButton("EstimateModels", label = "Start Models Estimation", align = "center")
            )
          )
        ),
        tabPanel(title = "Set model",
          br(),
          fluidRow(div(align="center",
            column(6,
              selectInput("usr_modelClass",label = "Model Class", width = "50%", choices = c("Diffusion processes"), selected = "Diffusion processes"),
              textInput("usr_model_name", label = "Model Name", width = "50%"),
              uiOutput("usr_modelClass_latex"),
              uiOutput("usr_model_coeff"),
              column(12,br(),br(),actionButton("usr_model_button_save", label = "Save Model"))
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
          shinyjs::hidden(div(id="estimates_info", fluidRow(
            column(12,
              textOutput("SymbolName"),
              a(id = "linkMoreInfo", tags$u("More Info"), href = "", style="color:#FFF48B"),
              bsModal(id = "MoreInfo", trigger = "linkMoreInfo", title = "Info",
                fluidRow(
                  column(12, uiOutput("text_MoreInfo")),
                  column(12, div(tableOutput("table_MoreInfo"), align="center")),
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
            column(8),
            column(2,actionButton(inputId = "databaseModelsDelete", label = "Delete")),
            bsTooltip("databaseModelsDelete", title = "Delete selected model", placement = "top"),
            column(2,actionButton(inputId = "databaseModelsDeleteAll", label = "Delete All")),
            bsTooltip("databaseModelsDeleteAll", title = "Delete all models that are displayed", placement = "top")
          )
        )
      ))),
      bsModal(id="plotsRange", trigger = "DisplayPlotsRange", title = div(h4(em("Select range to use for models estimation")), align="center"), size = "large",
        div(id="plotsRangeErrorMessage",h5("Select some series from table 'Available Data'")),
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
      bsModal(id="advancedSettings", title=div(h4(em(a("Advanced Settings", style="color:blue", href="http://www.rdocumentation.org/packages/yuima/functions/qmle", target = "_blank"))),align="center"), trigger = "advancedSettingsButton", size = "large",
        div(id="advancedSettingsErrorMessage",h5("Select some models and series (from table 'Available Data')")),
        div(id="advancedSettingsAll",
          fluidRow(
            column(6,
              box(width = 12,div(align="center",
                h3("Series Settings"),
                uiOutput("advancedSettingsSeries", align="center"),
                uiOutput("advancedSettingsDelta", align="center"),
                column(6, tags$button(type="button", id="advancedSettingsButtonApplyDelta", class = "action-button", em("Apply"))),
                column(6, tags$button(type="button", id="advancedSettingsButtonApplyAllDelta", class = "action-button", em("Apply All")))
              )),
              box(width = 12,div(align="center",
                h3("General Settings"),
                uiOutput("advancedSettingsMethod", align="center"),
                uiOutput("advancedSettingsTries", align="center"),
                uiOutput("advancedSettingsSeed", align="center"),
                uiOutput("advancedSettingsJoint", align="center"),
                uiOutput("advancedSettingsAggregation", align="center"),
                uiOutput("advancedSettingsThreshold", align="center"),
                column(6, tags$button(type="button", id="advancedSettingsButtonApplyGeneral", class = "action-button", em("Apply"))),
                column(6, tags$button(type="button", id="advancedSettingsButtonApplyAllModelGeneral", class = "action-button", em("Apply to Model"))),
                column(12, tags$button(type="button", id="advancedSettingsButtonApplyAllGeneral", class = "action-button", em("Apply to All")))
              ))
            ),
            column(6,
              box(width = 12,div(align="center",
                h3("Model Settings"),
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
                column(6, tags$button(type="button", id="advancedSettingsButtonApplyModel", class = "action-button", em("Apply"))),
                column(6, tags$button(type="button", id="advancedSettingsButtonApplyAllModel", class = "action-button", em("Apply to Model")))
              ))
            )
          )
        )
      )
    ))),
    ########################
    tabItem(tabName = "simulate",
      fluidRow(
        column(12,
          h3("In this section you can perform simulations.",style="color:#edeeed"),
          h4("To simulate models that have been estimated on data simply select those you are interested in from table 'Available Models'.",
             br(),
             "If you want to simulate a model that has not been estimated you can specify its parameters values in tab 'Simuate equation' and select it.",
             br(),
             "You can customize the simulation process by clicking on buttons 'Set Simulation' and 'Advanced Settings'.",
             br(),
             "Simulations are shown in tab 'Simulations'",
             style="color:#CDCECD"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,bsAlert("simulate_alert"))),
      fluidRow(column(12,tabsetPanel(id = "panel_simulations", type = "tabs",
        tabPanel(title = "Simulate model",
          fluidRow(column(12, br(),
            h4("Available models", style="color:#CDCECD"),
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
        tabPanel(title = "Simulate equation",
          fluidRow(
            column(4, br(), div(align="center",
              uiOutput("simulate_model_usr_selectModel"),
              uiOutput("simulate_model_usr_ID"),
              column(6,uiOutput("simulate_model_usr_selectParam")),
              column(6,uiOutput("simulate_model_usr_param")),
              column(12,actionButton("simulate_model_usr_button_save", label = "Save", align = "center"))
            )),
            column(8,
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
      div(id="div_simulations", fluidRow(
        column(12,br(),br(),br()),
        column(8,
          h4("Selected Models", style="color:#CDCECD"),
          DT::dataTableOutput("simulate_selectedModels"),
          br(),
          fluidRow(
            column(6,actionButton("simulation_button_deleteModels",label = "Delete", align = "center")),
            bsTooltip("simulation_button_deleteModels", title = "Delete selected models", placement = "top"),
            column(6,actionButton("simulation_button_deleteAllModels",label = "Delete All", align = "center")),
            bsTooltip("simulation_button_deleteAllModels", title = "Delete all models that are displayed", placement = "top")
          )
        ),
        column(4,
          br(),br(),br(),br(),
          div(actionButton("simulate_button_setSimulation", label = "Set Simulation"), align = "center"),
          br(),
          div(actionButton("simulate_button_advancedSettings", label = "Advanced Settings", align = "center"), align = "center"),
          br(),br(),br(),br(),br(),
          actionButton("simulate_simulateModels", label = "Start Models Simulation", align = "center")
        )
      )),
      bsModal(id="simulate_showSimulation", trigger = "simulate_monitor_button_showSimulation", title = div(h4(em("Simulation")), align="center"), size = "large",
        fluidRow(column(12,
          fluidRow(column(8,div(align="center",uiOutput("simulate_showSimulation_simID")))),
          fluidRow(div(id="simulate_showSimulation_plot_div", align = "center",
            column(8,
              plotOutput("simulate_showSimulation_plot", height = "350px", click = "simulate_showSimulation_plot_click")
            ),
            column(4,br(),br(),br(),
              div(selectInput("simulate_showSimulation_plot_scale", label = "Chart Scale", choices = c("Linear", "Logarithmic (Y)", "Logarithmic (X)", "Logarithmic (XY)")), align = "center"),
              br(),br(),br(),br(),br(),br(),br(),
              downloadButton(outputId = "simulate_showSimulation_button_saveTrajectory", label = "Save")
            )
          )),
          br(),
          fluidRow(div(id="simulate_showSimulation_hist_div",
            column(8,
              plotOutput("simulate_showSimulation_hist", height = "350px")
            ),
            column(4,
              div(align="center",br(),br(),br(),
                uiOutput("simulate_showSimulation_hist_nBins"),
                uiOutput("simulate_showSimulation_hist_probability_slider"),
                textOutput("simulate_showSimulation_hist_probability_text"),
                textOutput("simulate_showSimulation_hist_mean_text"),
                br(),
                downloadButton(outputId = "simulate_showSimulation_button_saveHist", label = "Save")
              )
            )
          ))
        ))
      ),
      bsModal(id="simulate_setSimulation", trigger = "simulate_button_setSimulation", title = div(h4(em("Set Simulation")), align="center"), size = "small",
        tags$style(type = "text/css", ".datepicker{z-index: 1100 !important;}"),
        div(id="simulate_setSimulation_errorMessage",h5("Select some models first")),
        div(id="simulate_setSimulation_body", align = "center",
          uiOutput("simulate_modelID"),
          br(),
          box(width = 12,
            uiOutput("simulate_nsim"),
            uiOutput("simulate_nstep"),
            column(6,tags$button(type="button", id="simulate_button_apply_nsim", class = "action-button", em("Apply"))),
            column(6,tags$button(type="button", id="simulate_button_applyAll_nsim", class = "action-button", em("Apply All")))
          ),
          box(width =12,
            uiOutput("simulate_xinit"),
            column(6,tags$button(type="button", id="simulate_button_apply_xinit", class = "action-button", em("Apply"))),
            column(6,tags$button(type="button", id="simulate_button_applyAll_xinit", class = "action-button", em("Apply All")))
          ),
          box(width = 12,
            uiOutput("simulate_range"),
            column(6,tags$button(type="button", id="simulate_button_apply_range", class = "action-button", em("Apply"))),
            column(6,tags$button(type="button", id="simulate_button_applyAll_range", class = "action-button", em("Apply All")))
          )
        )
      ),
      bsModal(id="simulate_advancedSettings", trigger = "simulate_button_advancedSettings", title = div(h4(em("Advanced Settings")), align="center"), size = "small",
        div(id="simulate_advancedSettings_errorMessage",h5("Select some models first")),
        div(id="simulate_advancedSettings_body", align = "center",
          uiOutput("simulate_advancedSettings_modelID"),
          uiOutput("simulate_seed"),
          uiOutput("simulate_traj"),
          column(6,tags$button(type="button", id="simulate_button_apply_advancedSettings", class = "action-button", em("Apply"))),
          column(6,tags$button(type="button", id="simulate_button_applyAll_advancedSettings", class = "action-button", em("Apply All")))
        )
      )
    ),
    ####################################################
    tabItem(tabName = "cluster",
      fluidRow(column(12,bsAlert("cluster_alert"))),
      fluidRow(column(12,
        column(4,
          h4("Available data", style="color:#CDCECD"),
          DT::dataTableOutput("cluster_table_select"),
          br(),
          fluidRow(
            column(6,actionButton("cluster_button_select",label = "Select", align = "center")),
            bsTooltip("cluster_button_select", title = "Select data to cluster", placement = "top"),
            column(6,actionButton("cluster_button_selectAll",label = "Select All", align = "center")),
            bsTooltip("cluster_button_selectAll", title = "Select all data that are displayed", placement = "top")
          )
        ),
        column(4,
          h4("Selected data", style="color:#CDCECD"),
          DT::dataTableOutput("cluster_table_selected"),
          br(),
          fluidRow(
            column(6,actionButton("cluster_button_delete",label = "Delete", align = "center")),
            bsTooltip("cluster_button_delete", title = "Delete selected data", placement = "top"),
            column(6,actionButton("cluster_button_deleteAll",label = "Delete All", align = "center")),
            bsTooltip("cluster_button_deleteAll", title = "Delete all data that are displayed", placement = "top")
          )
        ),
        column(4,br(),br(),br(),br(),
          div(align="center",selectInput("cluster_distance", "Distance", choices = c("Markov Operator"="MOdist", "My distance"="MYdist"))),
          br(),br(),br(),br(),br(),br(),
          actionButton("cluster_button_startCluster", label = "Start Clustering", align = "center")
        )
      )),
      br(),
      fluidRow(
        column(8, plotOutput("cluster_dendogram", click = "cluster_dendrogram_click")),        
        column(4, plotOutput("cluster_scaling2D"))
      )
    )
    ########################new tab items below
  )
)

ui <- dashboardPage(header,sidebar,body)
