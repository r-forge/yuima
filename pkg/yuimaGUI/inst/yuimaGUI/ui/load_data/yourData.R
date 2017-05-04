tabItem("yourData",
        fluidRow(
          column(12,
                 h3("Load data from Your Own Files",class = "hTitle"),
                 h4("Upload your file and specify its structure. A preview will be shown below.",
                    br(),
                    "Declare if the file contains raw and/or column headers and specify what kind of field separator has to be used to read the data.",
                    br(),
                    "Each column will be uploaded as a different series. So you might want to switch columns with rows if your file is organized differently.",
                    br(),
                    "Specify the format and the column to use as index."),
                 hr(class = "hrHeader")
          )
        ),
        fluidRow(column(12,bsAlert("yourDataAlert"))),
        fluidRow(
          column(5,
                 fileInput(inputId = "yourFile", width = "60%", label = "Choose file to upload", multiple = FALSE),
                 selectInput('yourFileHeader', 'Headers',width = "60%", choices = c("Auto"="Default","Only columns", "Only rows", "Both", "None"), selected = "Default"),
                 selectInput(inputId = 'yourFileSep',width = "60%", label = 'Field Separator', choices = c("Space"="default", "Comma"=',', "Semicolon"=';', "Tab"='\t'), selected = "default"),
                 uiOutput("yourFileIndex"),
                 uiOutput("yourFileFUN"),
                 br(),
                 div(align = "center", style="width:55%", box(background = switch(getOption("yuimaGUItheme"), "black"="black", "white"=NULL), width = 12,  title = "More Settings", collapsible = TRUE, id = "yourFileMoreSettings", collapsed = TRUE,
                                                              textInput('yourFileDec', 'Decimal Separator', value = "."),
                                                              textInput('yourFileThnd', 'Thousands Separator', value = ""),
                                                              textInput("yourFileNA", "Missing Value string", value = "NA"),
                                                              numericInput("yourFileLine", "Begin from line", value = 1, min = 1, step = 1),
                                                              selectInput('yourFileSwitch', 'Switch rows/columns', choices = c("No"=FALSE, "Yes"=TRUE))
                 )
                 )),
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
)
