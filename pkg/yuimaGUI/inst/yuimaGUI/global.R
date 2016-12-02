require(DT)
require(shinyjs)
require(yuima)
require(shiny)
require(sde)
require(quantmod)
require(shinydashboard)
require(shinyBS)
require(ggplot2)



if(!exists("yuimaGUIdata"))
  yuimaGUIdata <<- reactiveValues(series=list(), cp=list(), cpYuima=list(), model=list(), simulation=list(), hedging = list(), llag = list(), cluster = list())

if(!exists("estimateSettings"))
  estimateSettings <<- list()

if(!exists("deltaSettings"))
  deltaSettings <<- list()

if(!exists("toLogSettings"))
  toLogSettings <<- list()

if(!exists("usr_models"))
  usr_models <<- reactiveValues(model=list(), simulation=list())


