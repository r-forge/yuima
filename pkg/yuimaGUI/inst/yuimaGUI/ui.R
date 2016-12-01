header<-dashboardHeader(
  title = "yuimaGUI"
)
 
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data I/O", tabName = "data_section", icon = icon("upload"),
             menuSubItem("Financial & Economic Data", tabName = "finData"),
             menuSubItem("Your Data", tabName = "yourData")
             ),
    menuItem("Explorative Data Analysis", tabName = "eda_section", icon = icon("map"),
             menuSubItem("Change Point Estimation", tabName = "changepoint"),
             menuSubItem("Clustering", tabName = "cluster"),
             menuSubItem("Lead-Lag & Correlation", tabName = "llag")
             ),
    menuItem("Modeling", tabName = "models_section", icon = icon("sliders"),
             menuSubItem("Univariate", tabName = "models")
             ),
    menuItem("Simulation", tabName = "simulate_section", icon = icon("area-chart"),
             menuSubItem("Univariate", tabName = "simulate")
             )#,
    #hr(),
    #menuItem("Finance", tabName = "finance",
    #         menuSubItem("P&L distribution", tabName = "hedging")
    #        )
  )
)

body<-dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  shinyjs::useShinyjs(),
  withMathJax(),

  tabItems(
    tabItem(tabName = "home",
      fluidRow(
        column(12,
          h1("Welcome to yuimaGUI", style="color:#edeeed", align = "center"),
          h4("an amazingly powerful tool for your analysis", style="color:#edeeed; font-family: Times New Roman, Georgia, Serif;", align = "center"), 
          hr(class = "hrHeader"),
          h4("Get acquainted with yuimaGUI and learn how to best exploit it in a few simple steps:", style="color:#edeeed", align = "center"),
          br()
      )),
      fluidRow(
        column(8,
          h4("Step 1", style="color:#edeeed"),
          h4("Load the data you wish to analyze in section 'Data I/O'.", br(), 
             "An easy way to load economic data (i.e. GDP) or financial series (stocks and shares) from the Internet is provided. Otherwise you can load data from your own files.",br(),
             "Once data are loaded, you can go and use sections 'Explorative Data Analysis' and 'Modeling'.", style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
          h4("Step 2", style="color:#edeeed"),
          h4("Model data in section 'Modeling'.", br(),
             "Here you can fit models choosing between some default options or defining your own model.", br(),
             "Now you are ready to go to section 'Simulate'.", style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
          h4("Step 3", style="color:#edeeed"),
          h4("Read the short explanation at the beginning of every section for further information. Enjoy!", style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;")
        ),
        column(4,
          br(), br(),
          uiOutput("video_intro", align = "center")
        )
      ),
      fluidRow(
        column(8,h4(),br(),br(),br(),
          h4("Tips", style="color:#edeeed"),
          h4("Press F11 to go to full screen.", br(),
             "Press CTRL+ or CTRL- to zoom in and out.", style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;")#, 
          #br(),
          #uiOutput("certificates")       
        ),
        column(4,
          h3(em("Developed by"), style="color:#edeeed", align = "center"),
          h4("Emanuele Guidotti", style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;", align = "center"),
          h3(em("in collaboration with"), style="color:#edeeed", align = "center"),
          h4("Stefano M. Iacus & Lorenzo Mercuri", style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;", align = "center")
        )
      )
    ),
    #########################
    tabItem(tabName="finData",
      fluidRow(
        column(12,
          h3("Load Financial and Economic data",style="color:#edeeed"),
          h4("For Stocks and Shares select Yahoo source using symbols you can find ",
             a("here", href="http://finance.yahoo.com/lookup", target = "_blank"), ".",
             br(),
             "For currencies and metals select Oanda source and type the two symbols divided by '/' (e.g. EUR/USD or XAU/USD ).",
             "Symbols are available ", a("here",href="http://www.oanda.com/help/currency-iso-code", target = "_blank"), ".",
             br(),
             "Economic series are available on ",a("Federal Reserve Bank of St. Louis", href="https://research.stlouisfed.org/fred2/", target = "_blank"), ".",
             "Follow this ", a("example",href="example.jpg", target = "_blank"), " to find symbols.",
             br(),
             "Multiple symbols are allowed if divided by empty space and/or commas (e.g. AAPL FB CSCO or AAPL,FB,CSCO).",
             style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
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
    ),
    #########################
    tabItem("yourData",
      fluidRow(
        column(12,
          h3("Load data from Your Own Files",style="color:#edeeed"),
          h4("Upload your file and specify its structure. A preview will be shown below.",
             br(),
             "Declare if the file contains raw and/or column headers and specify what kind of field separator has to be used to read the data.",
             br(),
             "Each column will be uploaded as a different series. So you might want to switch columns with rows if your file is organized differently.",
             br(),
             "Specify the format and the column to use as index.",
              style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
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
      shinyjs::hidden(div(id="buttons_DataIO_file", br(),
        fluidRow(
          column(6),
          column(2, downloadButton(outputId = "yourFileSave", label = "Save")),
          bsTooltip("yourFileSave", title = "Save data to file", placement = "top"),
          column(2,actionButton(inputId = "yourFileDelete", label = "Delete")),
          bsTooltip("yourFileDelete", title = "Delete selected data", placement = "top"),
          column(2,actionButton(inputId = "yourFileDeleteAll", label = "Delete All")),
          bsTooltip("yourFileDeleteAll", title = "Delete all data that are displayed", placement = "top")
        )
      ))
    ),
    ########################
    tabItem(tabName="models", fluidRow(column(12,
      fluidRow(
        column(12,
          h3("Univariate Model Estimation",style="color:#edeeed"),
          h4("Select the data and the model you wish to estimate. Every model will be fitted to every selected series.",
             br(),
             "Click on buttons 'Set Range' and 'Advanced Settings' to customize the estimation process.",
             br(),
             "Some default models are available but you can set your own model (tab 'Set model') and use it for estimation and/or simulation purposes.",
             style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,tabsetPanel(id = "panel_estimates", type = "tabs",
        tabPanel(title = "Run estimation",
          fluidRow(column(12,bsAlert("panel_run_estimation_alert"))),
          br(),
          fluidRow(
            column(4,div(align="center",
              selectInput("modelClass",label = "Model Class", choices = c("Diffusion process", "Fractional process", "Compound Poisson", #"Levy process", 
                                                                          "CARMA", "COGARCH"), selected = "Diffusion process"),
              uiOutput("model"),
              uiOutput("jumps"),
              uiOutput("pq_C")
            )),
            column(5,
              fluidRow(shinyjs::hidden(h4(id="titlePrintModelLatex","Models to estimate:", style="color:#CDCECD;font-size: 2em; font-family: Goudy Old Style, Serif"))),
              fluidRow(uiOutput("PrintModelLatex"))
            )
          ),
          br(),
          fluidRow(
            column(4,
                   h4("Available data", style="color:#CDCECD"),
                   DT::dataTableOutput("database3")
            ),
            column(4,
                   h4("Selected data", style="color:#CDCECD"),
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
              a(id = "linkMoreInfo", tags$u("More Info"), href = "", style="color:#FFF48B"),
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
            bsTooltip("databaseModels_button_showResults", title = "Available for: Diffusive Processes", placement = "top"),
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
      bsModal(id="plotsRange", trigger = "DisplayPlotsRange", title = div(h4(em("Select range to use for models estimation")), align="center"), size = "large",
        div(id="plotsRangeErrorMessage",align = "center",h3("Select some series from table 'Available Data'")),
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
      bsModal(id="advancedSettings", title=div(h4(em(a("Advanced Settings", style="color:blue", href="http://www.rdocumentation.org/packages/yuima/functions/qmle", target = "_blank"))),align="center"), trigger = "advancedSettingsButton", size = "large",
        div(id="advancedSettingsErrorMessage",align = "center",h3("Select some models and series (from table 'Available Data')")),
        div(id="advancedSettingsAll",
          fluidRow(
            column(6,
              box(width = 12,div(align="center",
                h3("Series Settings"),
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
                h3("General Settings"),
                uiOutput("advancedSettingsMethod", align="center"),
                uiOutput("advancedSettingsThreshold", align="center"),
                fluidRow(
                  column(6,uiOutput("advancedSettingsTrials", align="center")),
                  column(6,uiOutput("advancedSettingsSeed", align="center"))
                ),
                uiOutput("advancedSettingsJoint", align="center"),
                uiOutput("advancedSettingsAggregation", align="center"),
                fluidRow(
                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyGeneral", class = "action-button", em("Apply"))),
                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyAllModelGeneral", class = "action-button", em("Apply to All series")))
                ),
                fluidRow(column(12, tags$button(type="button", id="advancedSettingsButtonApplyAllGeneral", class = "action-button", em("Apply to All series & models"))))
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
                fluidRow(
                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyModel", class = "action-button", em("Apply"))),
                  column(6, tags$button(type="button", id="advancedSettingsButtonApplyAllModel", class = "action-button", em("Apply to All series")))
                )
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
          h3("Univariate Simulation",style="color:#edeeed"),
          h4("Select the estimated models you wish to simulate.",
             br(),
             "If you want to simulate a model that has not been estimated you can use tab 'Simuate equation'.",
             br(),
             "Click on buttons 'Set Simulation' and 'Advanced Settings' to customize the simulation process.",
             style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,tabsetPanel(id = "panel_simulations", type = "tabs",
        tabPanel(title = "Simulate model",
          fluidRow(column(12,bsAlert("panel_simulate_model_alert"))),
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
          fluidRow(column(12,bsAlert("panel_simulate_equation_alert"))),
          fluidRow(
            uiOutput("simulate_PrintModelLatex")       
          ),
          fluidRow(
            column(6, br(), div(align="center",
              fluidRow(
                column(1),
                column(5,selectInput("simulate_model_usr_selectClass", label = "Class", choices = c("Diffusion process", "Fractional process", "Compound Poisson"))),
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
            h4("Selected Models", style="color:#CDCECD"),
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
        div(id="simulate_setSimulation_errorMessage",align = "center", h3("Select some models first")),
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
      bsModal(id="simulate_advancedSettings", trigger = "simulate_button_advancedSettings", title = div(h4(em("Advanced Settings")), align="center"), size = "small",
        div(id="simulate_advancedSettings_errorMessage", align = "center", h3("Select some models first")),
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
      fluidRow(
        column(12,
          h3("Clustering",style="color:#edeeed"),
          h4("Select data you want to cluster.", br(),
             "Choose the distance you want to use and the kind of linkage for the hierarchical cluster analysis.", br(),
             "Results will be shown below by plotting dendrogram and multidimensional scaling output.",
                        style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,bsAlert("cluster_alert"))),
      fluidRow(column(12,
        column(4,
          h4("Available data", style="color:#CDCECD"),
          DT::dataTableOutput("cluster_table_select")
        ),
        column(4,
          h4("Selected data", style="color:#CDCECD"),
          DT::dataTableOutput("cluster_table_selected")
        ),
        column(4,br(),br(),
          div(align="center",
            selectInput("cluster_linkage", "Linkage", choices = c("Complete"="complete", "Single"="single", "Average"="average", "Ward"="ward.D", "Ward squared"="ward.D2", "McQuitty"="mcquitty", "Median"="median", "Centroid"="centroid")),
            selectInput("cluster_distance", "Distance", choices = c("Markov Operator"="MOdist", "Percentage Increments Distribution"="MYdist_perc", "Increments Distribution"="MYdist_ass", "Euclidean"="euclidean", "Maximum"="maximum", "Manhattan"="manhattan", "Canberra"="canberra", "Minkowski"="minkowski")),
            shinyjs::hidden(numericInput("cluster_distance_minkowskiPower", label = "Power", value = 2)))
        )
      )),
      br(),
      fluidRow(column(12,
        column(2,actionButton("cluster_button_select",label = "Select", align = "center")),
        bsTooltip("cluster_button_select", title = "Select data to cluster", placement = "top"),
        column(2,actionButton("cluster_button_selectAll",label = "Select All", align = "center")),
        bsTooltip("cluster_button_selectAll", title = "Select all data that are displayed", placement = "top"),
        column(2,actionButton("cluster_button_delete",label = "Delete", align = "center")),
        bsTooltip("cluster_button_delete", title = "Delete selected data", placement = "top"),
        column(2,actionButton("cluster_button_deleteAll",label = "Delete All", align = "center")),
        bsTooltip("cluster_button_deleteAll", title = "Delete all data that are displayed", placement = "top"),           
        column(4,actionButton("cluster_button_startCluster", label = "Start Clustering", align = "center"))
      )),
      div(id="cluster_charts", align = "center",
        br(),br(),
        hr(class = "hrHeader"),
        fluidRow(
          column(4),
          column(4, uiOutput("cluster_analysis_id"))
        ),
        fluidRow(column(11, div(align="left", uiOutput("cluster_moreInfo")))),
        fluidRow(column(12,
          column(8, plotOutput("cluster_dendogram", click = "cluster_dendrogram_click")),        
          column(4, plotOutput("cluster_scaling2D"))
        )),
        br(),
        fluidRow(column(12,
          column(2, div(actionButton("cluster_button_delete_analysis", label = "Delete"))),
          column(2, div(actionButton("cluster_button_deleteAll_analysis", label = "Delete All"))),
          column(4),
          column(2, div(downloadButton("cluster_button_saveDendogram", label = "Dendrogram"))), 
          column(2, div(downloadButton("cluster_button_saveScaling2D", label = "Scaling")))
        ))
      )
    ),
    ####################################################
    tabItem(tabName = "changepoint",
      fluidRow(
        column(12,
          h3("Change Point Estimation",style="color:#edeeed"),
          h4("Select the data you wish to estimate change points for.", br(),
            "Choose the algorithm you want to use for estimation.", br(),
            "Results will be shown below by plotting the series and the detected change points.",
            style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,tabsetPanel(id = "panel_cpoint", type = "tabs",
        tabPanel(title = "Nonparametric",
          fluidRow(column(12,bsAlert("nonparametric_changepoint_alert"))),
          fluidRow(column(12,
            column(4,
              h4("Available data", style="color:#CDCECD"),
              DT::dataTableOutput("changepoint_table_select")
            ),
            column(4,
              h4("Selected data", style="color:#CDCECD"),
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
              column(6, div(align = "right", a(id = "linkChangePointInfo", tags$u(h4("Change Points Info")), href = "", style="color:#FFF48B")))
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
                                        h4("Available data", style="color:#CDCECD"),
                                        DT::dataTableOutput("parametric_changepoint_table_select")
                                 ),
                                 column(4,
                                        h4("Selected data", style="color:#CDCECD"),
                                        DT::dataTableOutput("parametric_changepoint_table_selected")
                                 ),
                                 column(4,br(),br(),div(align="center", 
                                        uiOutput("parametric_changepoint_model"),
                                        sliderInput("parametric_modal_rangeFraction", label = "Training set (%)",min = 0, max = 100, value = c(20,80), step = 1, ticks = F),
                                        br(),
                                        actionButton("parametric_button_settings", label = "Advanced Settings")
                                )),
                                bsModal(id="parametric_modal_id", title=div(h4(em(a("Settings", style="color:blue", href="http://www.rdocumentation.org/packages/yuima/functions/qmle", target = "_blank"))),align="center"), trigger = "parametric_button_settings", size = "large",
                                        div(id="parametric_modal_errorMessage", align = "center", h3("Select some series (from table 'Available Data')")),
                                        div(id="parametric_modal_body",
                                            fluidRow(
                                              column(6,
                                                     box(width = 12,div(align="center",
                                                                        h3("Series Settings"),
                                                                        uiOutput("parametric_modal_series", align="center"),
                                                                        fluidRow(
                                                                          column(6,uiOutput("parametric_modal_delta", align="center")),
                                                                          column(6,uiOutput("parametric_modal_toLog", align="center"))       
                                                                        ),
                                                                        fluidRow(uiOutput("parametric_modal_range")),
                                                                        fluidRow(
                                                                          column(6, tags$button(type="button", id="parametric_modal_button_applyDelta", class = "action-button", em("Apply"))),
                                                                          column(6, tags$button(type="button", id="parametric_modal_button_applyAllDelta", class = "action-button", em("Apply to All series")))
                                                                        )
                                                     )),
                                                     box(width = 12,div(align="center",
                                                                        h3("General Settings"),
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
                                                                        h3("Model Settings"),
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
                                                                     a(id = "parametric_linkChangePointInfo", tags$u(h4("Change Point Info")), href = "", style="color:#FFF48B"),
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
                                                                  column(6, div(h5("Estimates before the Change Point"), tableOutput("parametric_changepoint_modal_info_tableL"), align="center")),
                                                                  column(6, div(h5("Estimates after the Change Point"), tableOutput("parametric_changepoint_modal_info_tableR"), align="center"))
                                                                )
                                                                
                                                        )
                 ))))
        )
      )))
    ),
    tabItem(tabName = "llag",
      fluidRow(
        column(12,
          h3("Lead-Lag and Correlation Analysis",style="color:#edeeed"),
          h4("Select the series you wish to analyze and the kind of analysis you want to perform (Lead-Lag or Correlation).", br(),
             "Choose the correlation measure (if you selected Correlation) or the maximum lag to use (if you selected Lead-Lag).",br(),
             "You can specify which interval over the whole series to use for the analysis.",
              style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,bsAlert("llag_alert"))),
      fluidRow(column(12,
        column(4,
          h4("Available data", style="color:#CDCECD"),
          DT::dataTableOutput("llag_table_select")
        ),
        column(4,
          h4("Selected data", style="color:#CDCECD"),
          DT::dataTableOutput("llag_table_selected")
        ),
        column(4,br(),br(),
          div(align="center",
            selectInput("llag_type", label = "Type of analysis", choices = c("Lead-Lag"="llag", "Correlation"="corr"), selected = "llag"),
            numericInput("llag_maxLag", label = "Max Lag", value = 20, min = 1, step = 1),
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
          HTML("<div id = 'llag_plot_howToRead' style='color:#CDCECD;'><b>How to read the plot:</b><br/>If the lead-lag is positive: 'row.name' anticipates 'col.name of 'X' periods<br/>If the lead-lag is negative: 'row.name' follows 'col.name' with 'X' delay periods<br/><br/><b>'X'</b> are the numbers in the plot above.<br/>They are expressed in days if you are using time series, or in the same unit of measure of time if you are using numerical time index.</div>")
        )))
      )
    ),
    ########################hedging
    tabItem(tabName = "hedging",
      fluidRow(
        column(12,
          h3("Here you can manage risk deriving from buying options and the underlying asset.",style="color:#edeeed"),
          h4("The evolution of the underlying asset is simulated by models you estimated in section Modeling.", br(),
             "After performing the simulation click on rows of the table in tab 'Profit&Loss' in order to choose the number of options and assets to buy/sell.",br(),
             "The Profit&Loss distribution of your position will be displayed (it includes transaction costs that you can customize).",
            style="color:#CDCECD; font-family: Times New Roman, Georgia, Serif;"),
          hr(class = "hrHeader")
        )
      ),
      fluidRow(column(12,bsAlert("hedging_alert"))),
      tabsetPanel(id = "panel_hedging", type = "tabs",
        tabPanel(title = "Start simulations",
          fluidRow(column(12, br(),
              h4("Click on the model by which to simulate the evolution of the underlying asset", style="color:#CDCECD"),
              DT::dataTableOutput("hedging_databaseModels")
            )
          ),
          br(),
          fluidRow(column(12,div(align="center",
            br(),
            fluidRow(
              column(3,selectInput("hedging_type", label="Option Type:", c(Call="call", Put="put"))),
              column(3,numericInput("hedging_strike", label="Strike Price:", value=0, min = 0, max = NA, step = NA, width = NULL)),
              column(3,dateInput("hedging_maturity", label="Maturity:", value = Sys.Date()+30)),
              column(3,numericInput("hedging_lotMult", label="Number of Options per Lot:", value=1000, min = 1))
            ),
            fluidRow(
              column(3),
              column(3,numericInput("hedging_optMarketPrice", label="Option Market Price:", value=NA, min = 0)),
              column(3,uiOutput("hedging_assMarketPrice")),
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
          shinyjs::hidden(div(id="hedging_body",align="center",br(),
            fluidRow(
              column(3,numericInput("hedging_maxCapital", label = "Available Capital", value = 10000)),
              column(3,uiOutput("hedging_nOptLot_hedge")),
              column(3,uiOutput("hedging_nAss_hedge"))
            ),
            fluidRow(
              column(9, plotOutput("hedging_plot_distribution")),
              column(3,
                sliderInput("hedging_slider_nBin", label = "Adjust bin width", min=1, max=100, value = 30, step = 1, ticks = FALSE),
                br(),
                uiOutput("hedging_slider_rangeHist"),
                textOutput("hedging_probability_text"),
                textOutput("hedging_mean_text"),
                br(),
                textOutput("hedging_capital_text"),
                textOutput("hedging_meanPerc_text"),
                br(),br(),br(),
                actionButton("hedging_button_commissionPlan", label = "Transaction Costs", width = "80%")
              )
            ),
            br(),
            fluidRow(
              column(3,selectInput("hedging_type2", label="Modify Option Type", c(" "="default", Call="call", Put="put"))),
              column(3,numericInput("hedging_strike2", label = "Modify Strike", min = 0, value = NA)),
              column(3,numericInput("hedging_optMarketPrice2", label = "Modify Option Price", min = 0, value = NA)),
              column(3,br(),actionButton("hedging_button_saveHedging", "Save Changes", width = "80%"))
            ),
            bsModal(id="hedging_commissionPlan", trigger = "hedging_button_commissionPlan", title = div(h4(em("Commission Plan")), align="center"), size = "small",
              div(align = "center",
                box(width = 12,
                  numericInput("hedging_percCostAss", label="Asset - Trading cost (%):", value=0.19, min = 0),
                  numericInput("hedging_minCostAss", label="Asset - Min trading cost:", value=2.95, min = 0),
                  numericInput("hedging_rateShort", label="Asset - Annual interest rate for short position (%):", value=4.95, min = 0),
                  numericInput("hedging_lotCostOpt", label="Option - Trading cost per lot:", value=5.95, min = 0)
                )
              )
            )
          )),
          fluidRow(column(12,br(),
            DT::dataTableOutput("hedging_table_results")
          )),
          br(),
          fluidRow(
            column(8),
            column(2,actionButton(inputId = "hedging_button_delete", label = "Delete")),
            column(2,actionButton(inputId = "hedging_button_deleteAll", label = "Delete All"))
          )         
        )
      )
    )
    ########################new tab items below
  )
)

ui <- dashboardPage(header,sidebar,body)
