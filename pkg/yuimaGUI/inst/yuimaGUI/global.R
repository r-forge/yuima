suppressMessages(require(DT))
suppressMessages(require(shinyjs))
suppressMessages(require(yuima))
suppressMessages(require(shiny))
suppressMessages(require(sde))
suppressMessages(require(quantmod)) 
suppressMessages(require(shinydashboard)) 
suppressMessages(require(shinyBS))
suppressMessages(require(ggplot2))
suppressMessages(require(plotly))


if(!exists("yuimaGUIdata"))
  yuimaGUIdata <- reactiveValues(series=list(), 
                                 model=list(), multimodel=list(), 
                                 usr_model = list(), usr_multimodel = list(), 
                                 simulation=list(), multisimulation=list(), 
                                 usr_simulation = list(), usr_multisimulation = list(), 
                                 cp=list(), 
                                 cpYuima=list(), 
                                 llag = list(), 
                                 cluster = list(), 
                                 hedging = list())

if(is.null(getOption("yuimaGUItheme"))) options(yuimaGUItheme = "black")

# getSimulation <- function(symb, n = 1){
#   return(isolate({yuimaGUIdata$simulation[[symb]][[n]]}))
# }
# 
# getSeries <- function(symb){
#   return(isolate({yuimaGUIdata$series[[symb]]}))
# }
