header <- dashboardHeader(
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
             menuSubItem("Univariate", tabName = "models"),
             menuSubItem("Multivariate", tabName = "multi_models")
             ),
    menuItem("Simulation", tabName = "simulate_section", icon = icon("area-chart"),
             menuSubItem("Univariate", tabName = "simulate"),
             menuSubItem("Multivariate", tabName = "multi_simulate")
             ),
    hr(),
    menuItem("Finance", tabName = "finance",
             menuSubItem("P&L distribution", tabName = "hedging")
            ),
    hr(),br(),
    div(id="sessionButtons",
      fluidRow( downloadButton("saveSession", label = "Save Session")),
      br(),
	  fluidRow(column(1),column(9,fileInput("loadSession", label = "Load Session", multiple=FALSE)))
    ),
    br(),
    div(id="theyuimaprojct",
        a("User Guide", href="https://yuima-project.com/manuals/", target="_blank"),br(),
        a("Troubleshooting", href="https://yuima-project.com/category/troubleshooting-yuimagui/", target="_blank"),br(),
        br(),
        a("Spreading the word", href="https://yuima-project.com/category/conferences/", target="_blank"),br(),
        a("About Us", href="https://yuima-project.com/the-yuima-team/", target="_blank"),br(),
        br(),
        a("Contact Us", href="https://yuima-project.com/contact-us/", target="_blank")
    )
  )
)

body<-dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = paste(getOption("yuimaGUItheme"), ".css", sep = ""))),
  tags$head(tags$link(rel="shortcut icon", href="yuimaLogo.ico")),
  shinyjs::useShinyjs(),
  withMathJax(),

  tabItems(
    source("ui/home/home.R", local = TRUE)$value,
    
    source("ui/load_data/finData.R", local = TRUE)$value,
    source("ui/load_data/yourData.R", local = TRUE)$value,
    
    source("ui/eda/cluster.R", local = TRUE)$value,
    source("ui/eda/changepoint.R", local = TRUE)$value,
    source("ui/eda/llag.R", local = TRUE)$value,
    
    source("ui/modeling/models.R", local = TRUE)$value,
    source("ui/modeling/multi_models.R", local = TRUE)$value,
    
    source("ui/simulation/univariate.R", local = TRUE)$value,
    source("ui/simulation/multivariate.R", local = TRUE)$value,
    
    source("ui/finance/hedging.R", local = TRUE)$value
  )
)

ui <- dashboardPage(header,sidebar,body)
