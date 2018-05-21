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
suppressMessages(require(ghyp))


# if(!exists("yuimaGUIdata"))
#   yuimaGUIdata <- reactiveValues(series=list(), 
#                                  model=list(), multimodel=list(), 
#                                  usr_model = list(), usr_multimodel = list(), 
#                                  simulation=list(), multisimulation=list(), 
#                                  usr_simulation = list(), usr_multisimulation = list(), 
#                                  cp=list(), 
#                                  cpYuima=list(), 
#                                  llag = list(), 
#                                  cluster = list(), 
#                                  hedging = list())
 
if(is.null(getOption("yuimaGUItheme"))) options(yuimaGUItheme = "black")

#NIG distribution
dNIG.gui <- function(x, alpha, delta, beta, mu){
  g <- NIG.ad(alpha = alpha, delta = delta, beta = beta, mu = mu)
  dghyp(x = x, object = g)
}
rNIG.gui <- function(n, alpha, delta, beta, mu){
  g <- NIG.ad(alpha = alpha, delta = delta, beta = beta, mu = mu)
  rghyp(n = n, object = g)
}

#hyp distribution
dhyp.gui <- function(x, alpha, delta, beta, mu){
  g <- hyp.ad(alpha = alpha, delta = delta, beta = beta, mu = mu)
  dghyp(x = x, object = g)
}
rhyp.gui <- function(n, alpha, delta, beta, mu){
  g <- hyp.ad(alpha = alpha, delta = delta, beta = beta, mu = mu)
  rghyp(n = n, object = g)
}

#VG distribution
dVG.gui <- function(x, lambda, alpha, beta, mu){
  g <- VG.ad(lambda = lambda, alpha = alpha, beta = beta, mu = mu)
  dghyp(x = x, object = g)
}
rVG.gui <- function(n, lambda, alpha, beta, mu){
  g <- VG.ad(lambda = lambda, alpha = alpha, beta = beta, mu = mu)
  rghyp(n = n, object = g)
}

#ghyp distribution
dghyp.gui <- function(x, lambda, alpha, delta, beta, mu){
  g <- ghyp.ad(lambda = lambda, alpha = alpha, delta = delta, beta = beta, mu = mu)
  dghyp(x = x, object = g)
}
rghyp.gui <- function(n, lambda, alpha, delta, beta, mu){
  g <- ghyp.ad(lambda = lambda, alpha = alpha, delta = delta, beta = beta, mu = mu)
  rghyp(n = n, object = g)
}
