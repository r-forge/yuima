require(shiny)
require(DT)
require(sde)
require(quantmod)
require(shinydashboard)
require(shinyBS)
require(yuima)
require(shinyjs)
require(corrplot)


if(!exists("yuimaGUItable"))
  yuimaGUItable <<- reactiveValues(series=data.frame(),  model=data.frame(), simulation=data.frame())

if(!exists("yuimaGUIdata"))
  yuimaGUIdata <<- reactiveValues(series=list(), cp=list(), model=list(), simulation=list())

if(!exists("estimateSettings"))
  estimateSettings <<- list()

if(!exists("deltaSettings"))
  deltaSettings <<- list()

if(!exists("usr_models"))
  usr_models <<- reactiveValues(model=list(), simulation=list())


rbind.fill <- function(..., rep = NA){
  dots <- list(...)
  names <- c()
  for (i in length(dots):1){
    if (length(rownames(dots[[i]]))==0)
      dots[i] <- NULL
    else
      names <- unique(c(names, colnames(dots[[i]])))
  }
  for (symb in names)
    for (i in 1:length(dots))
      if (!(symb %in% colnames(dots[[i]])))
        dots[[i]][,symb] <- rep
  return (do.call("rbind", dots))
}


observeEvent(yuimaGUIdata$series, priority = 10, {
  yuimaGUItable$series <<- data.frame()
  for (symb in names(yuimaGUIdata$series)){
    test <- try(rbind(yuimaGUItable$series, data.frame(Symb = as.character(symb), From = as.character(start(yuimaGUIdata$series[[symb]])), To = as.character(end(yuimaGUIdata$series[[symb]])))))
    if (class(test)!="try-error")
      yuimaGUItable$series <<- test
    else
      yuimaGUIdata$series <<- yuimaGUIdata$series[-which(names(yuimaGUIdata$series)==symb)]
  }
  if (length(yuimaGUItable$series)!=0)
    rownames(yuimaGUItable$series) <<- yuimaGUItable$series[,"Symb"]
})

observeEvent(yuimaGUIdata$model, priority = 10, {
  yuimaGUItable$model <<- data.frame()
  for (symb in names(yuimaGUIdata$model)){
    for (i in 1:length(yuimaGUIdata$model[[symb]])){
      newRow <- data.frame(
        Symb = as.character(symb),
        Model = as.character(yuimaGUIdata$model[[symb]][[i]]$info$modName),
        From = as.character(start(yuimaGUIdata$model[[symb]][[i]]$model@data@original.data)),
        To = as.character(end(yuimaGUIdata$model[[symb]][[i]]$model@data@original.data)),
        AIC = as.character(yuimaGUIdata$model[[symb]][[i]]$aic),
        BIC = as.character(yuimaGUIdata$model[[symb]][[i]]$bic))
      rownames(newRow) <- as.character(paste(symb," ", i, sep=""))
      yuimaGUItable$model <<- rbind(yuimaGUItable$model, newRow)
    }
  }
})

observeEvent(yuimaGUIdata$simulation, priority = 10, {
  yuimaGUItable$simulation <<- data.frame()
  for (symb in names(yuimaGUIdata$simulation)){
    for (i in 1:length(yuimaGUIdata$simulation[[symb]])){
      newRow <- data.frame(
        Symb = as.character(symb),
        Model = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$model),
        "N sim" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$nsim),
        "Simulated from" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$simulate.from),
        "Simulated to" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$simulate.to),
        "Estimated from" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$estimate.from),
        "Estimated to" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$estimate.to))
      rownames(newRow) <- as.character(paste(symb," ", i, sep=""))
      yuimaGUItable$simulation <<- rbind(yuimaGUItable$simulation, newRow)
    }
  }
})

observe({
  differ <- names(yuimaGUIdata$cp)[!(names(yuimaGUIdata$cp) %in% names(yuimaGUIdata$series))]
  if (length(differ)!=0)
    for (i in differ)
      yuimaGUIdata$cp[[i]] <<- NULL
})


addData <- function(x, typeIndex, session, anchorId, printSuccess = TRUE){
  x <- data.frame(x, check.names = FALSE)
  err <- c()
  alreadyIn <- c()
  for (symb in colnames(x)){
    if (symb %in% names(yuimaGUIdata$series))
      alreadyIn <- c(alreadyIn, symb)
    else{
      temp <- data.frame("Index" = rownames(x), symb = as.numeric(as.character(x[,symb])))
      temp <- temp[complete.cases(temp), ]
      rownames(temp) <- temp[,"Index"]
      if (typeIndex=="numeric"){
        test <- try(read.zoo(temp, FUN=as.numeric))
        if (class(test)!="try-error")
          yuimaGUIdata$series[[symb]] <<- test
        else
          err <- c(err, symb)
      }
      else{
        test <- try(read.zoo(temp, FUN=as.Date, format = typeIndex))
        if (class(test)!="try-error")
          yuimaGUIdata$series[[symb]] <<- test
        else
          err <- c(err, symb)
      }
    }
  }
  if (length(err)==0 & length(alreadyIn)==0 & printSuccess)
    createAlert(session = session, anchorId = anchorId, content = paste("Data uploaded successfully"), style = "success")
  if (length(err)!=0)
    createAlert(session = session, anchorId = anchorId, content = paste("Unable to upload following symbols:",paste(err,collapse = " ")), style = "error")
  if (length(alreadyIn)!=0)
    createAlert(session = session, anchorId = anchorId, content = paste("Following data already uploaded:", paste(alreadyIn, collapse = " ")), style = "warning")
}

getDataNames <- function(){
  return(isolate({yuimaGUItable$series}))
}

getData <- function(symb){
  return(isolate({yuimaGUIdata$series[[symb]]}))
}

delData <- function(symb){
  for (i in symb)
    yuimaGUIdata$series <<- yuimaGUIdata$series[-which(names(yuimaGUIdata$series)==i)]
}


defaultModels <-  c("Diffusion process"="Brownian Motion",
                    "Diffusion process"="Geometric Brownian Motion",
                    "Diffusion process"="Ornstein-Uhlenbeck (OU)",
                    "Diffusion process"="Vasicek model (VAS)",
                    "Diffusion process"="Constant elasticity of variance (CEV)",
                    "Diffusion process"= "Cox-Ingersoll-Ross (CIR)",
                    "Diffusion process"="Chan-Karolyi-Longstaff-Sanders (CKLS)",
                    "Diffusion process"="Hyperbolic (Barndorff-Nielsen)",
                    "Diffusion process"="Hyperbolic (Bibby and Sorensen)")

defaultStart <- function(name, default = 1000){
  if (name %in% names(isolate({usr_models$model}))){
    par <- setModelByName(name)@parameter@all
    startmin <- rep(-default, length(par))
    startmax <- rep(default, length(par))
    names(startmin) <- par
    names(startmax) <- par
    return(list(min=as.list(startmin), max=as.list(startmax)))
  }
  if (name == "Brownian Motion" | name == "Bm")
    return (list(min=list("sigma"=0, "mu"=-default), max=list("sigma"=default, "mu"=default)))
  if (name == "Geometric Brownian Motion" | name == "gBm")
    return (list(min=list("sigma"=0, "mu"=-default), max=list("sigma"=default, "mu"=default)))
  if (name == "Ornstein-Uhlenbeck (OU)" | name == "OU")
    return(list(min=list("theta"=-default),max=list("theta"=default)))
  if (name == "Vasicek model (VAS)" | name == "VAS")
    return(list(min=list("theta3"=0, "theta1"=-default, "theta2"=-default),max=list("theta3"=default, "theta1"=default, "theta2"=default)))
  if (name == "Constant elasticity of variance (CEV)" | name == "CEV")
    return(list(min=list("mu"=-default, "sigma"=-default, "gamma"=-3),max=list("mu"=default, "sigma"=default, "gamma"=3)))
  if (name == "Cox-Ingersoll-Ross (CIR)" | name == "CIR")
    return(list(min=list("theta1"=0,"theta2"=0,"theta3"=0),max=list("theta1"=default,"theta2"=default,"theta3"=default)))
  if (name == "Chan-Karolyi-Longstaff-Sanders (CKLS)" | name == "CKLS")
    return(list(min=list("theta1"=-default, "theta2"=-default, "theta3"=0, "theta4"=-3),max=list("theta1"=default, "theta2"=default, "theta3"=default, "theta4"=3)))
  if (name == "Hyperbolic (Barndorff-Nielsen)" | name == "hyp1")
    return(list(min=list("delta"=0, "alpha"=0, "beta"=0, "sigma"=-default, "mu"=-default),max=list("delta"=default, "alpha"=default, "beta"=default, "sigma"=default, "mu"=default)))
  if (name == "Hyperbolic (Bibby and Sorensen)" | name == "hyp2")
    return(list(min=list("delta"=0, "alpha"=0, "beta"=0, "sigma"=-default, "mu"=-default),max=list("delta"=default, "alpha"=default, "beta"=default, "sigma"=default, "mu"=default)))
}


defaultBounds <- function(name){
  if (name %in% names(isolate({usr_models$model})))
    return(list(lower=list(),upper=list()))
  if (name == "Brownian Motion" | name == "Bm")
    return (list(lower=list("sigma"=0), upper=list()))
  if (name == "Geometric Brownian Motion" | name == "gBm")
    return(list(lower=list("sigma"=0),upper=list()))
  if (name == "Ornstein-Uhlenbeck (OU)" | name == "OU")
    return(list(lower=list(),upper=list()))
  if (name == "Vasicek model (VAS)" | name == "VAS")
    return(list(lower=list("theta3"=0),upper=list()))
  if (name == "Constant elasticity of variance (CEV)" | name == "CEV")
    return(list(lower=list(),upper=list()))
  if (name == "Cox-Ingersoll-Ross (CIR)" | name == "CIR")
    return(list(lower=list("theta1"=0,"theta2"=0,"theta3"=0),upper=list()))
  if (name == "Chan-Karolyi-Longstaff-Sanders (CKLS)" | name == "CKLS")
    return(list(lower=list("theta3"=0),upper=list()))
  if (name == "Hyperbolic (Barndorff-Nielsen)" | name == "hyp1")
    return(list(lower=list("delta"=0, "alpha"=0, "beta"=0),upper=list()))
  if (name == "Hyperbolic (Bibby and Sorensen)" | name == "hyp2")
    return(list(lower=list("delta"=0, "alpha"=0, "beta"=0),upper=list()))
}


setModelByName <- function(name){
  if (name %in% names(isolate({usr_models$model})))
    return(isolate({usr_models$model[[name]]$object}))
  if (name == "Brownian Motion" | name == "Bm")
    return(yuima::setModel(drift="mu", diffusion="sigma", solve.variable = "x"))
  if (name == "Geometric Brownian Motion" | name == "gBm")
    return(yuima::setModel(drift="mu*x", diffusion="sigma*x", solve.variable = "x"))
  if (name == "Ornstein-Uhlenbeck (OU)" | name == "OU")
    return(yuima::setModel(drift="-theta*x", diffusion="1", solve.variable = "x"))
  if (name == "Vasicek model (VAS)" | name == "VAS")
    return(yuima::setModel(drift="theta1-theta2*x", diffusion="theta3", solve.variable = "x"))
  if (name == "Constant elasticity of variance (CEV)" | name == "CEV")
    return(yuima::setModel(drift="mu*x", diffusion="sigma*x^gamma", solve.variable = "x"))
  if (name == "Cox-Ingersoll-Ross (CIR)" | name == "CIR")
    return(yuima::setModel(drift="theta1-theta2*x", diffusion="theta3*sqrt(x)", solve.variable = "x"))
  if (name == "Chan-Karolyi-Longstaff-Sanders (CKLS)" | name == "CKLS")
    return(yuima::setModel(drift="theta1+theta2*x", diffusion="theta3*x^theta4", solve.variable = "x"))
  if (name == "Hyperbolic (Barndorff-Nielsen)" | name == "hyp1")
    return(yuima::setModel(drift="(sigma/2)^2*(beta-alpha*((x-mu)/(sqrt(delta^2+(x-mu)^2))))", diffusion="sigma", solve.variable = "x"))
  if (name == "Hyperbolic (Bibby and Sorensen)" | name == "hyp2")
    return(yuima::setModel(drift="0", diffusion="sigma*exp(0.5*(alpha*sqrt(delta^2+(x-mu)^2)-beta*(x-mu)))", solve.variable = "x"))
}

printModelLatex <- function(names){
  mod <- ""
  for (name in names){
    if (name %in% names(isolate({usr_models$model}))){
      if (isolate({usr_models$model[[name]]$class=="Diffusion process"})){
        text <- toLatex(setModelByName(name))
        x <- paste(text[2:9], collapse = "")
        x <- substr(x,3,nchar(x))
        x <- gsub(x, pattern = "'", replacement = "")
        x <- gsub(x, pattern = "x", replacement = "X_t")
        x <- gsub(x, pattern = "W1", replacement = "W_t")
        mod <- paste(mod, ifelse(mod=="","","\\\\"), x)
      }
    }
    if (name == "Brownian Motion" | name == "Bm")
      mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = \\mu \\; dt + \\sigma \\; dW_t")
    if (name == "Geometric Brownian Motion" | name == "gBm")
      mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = \\mu X_t \\; dt + \\sigma X_t \\; dW_t")
    if (name == "Ornstein-Uhlenbeck (OU)" | name == "OU")
      mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = -\\theta X_t \\; dt + \\; dW_t")
    if (name == "Vasicek model (VAS)" | name == "VAS")
      mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = (\\theta_1 - \\theta_2 X_t) \\;dt + \\theta_3 \\; dW_t")
    if (name == "Constant elasticity of variance (CEV)" | name == "CEV")
      mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = \\mu X_t \\;dt + \\sigma X_t^\\gamma \\; dW_t")
    if (name == "Cox-Ingersoll-Ross (CIR)" | name == "CIR")
      mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = (\\theta_1-\\theta_2 X_t) \\; dt + \\theta_3 \\sqrt{X_t} \\; dW_t")
    if (name == "Chan-Karolyi-Longstaff-Sanders (CKLS)" | name == "CKLS")
      mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = (\\theta_1+\\theta_2 X_t) \\; dt + \\theta_3 X_t^{\\theta_4} \\; dW_t")
    if (name == "Hyperbolic (Barndorff-Nielsen)" | name == "hyp1")
      mod <- paste(mod, ifelse(mod=="","","\\\\"),"dX_t = \\frac{\\sigma}{2}^2 \\Bigl (\\beta-\\alpha \\frac{X_t-\\mu}{\\sqrt{\\delta^2+(X_t-\\mu)^2}} \\Bigl ) \\; dt + \\sigma \\; dW_t")
    if (name == "Hyperbolic (Bibby and Sorensen)" | name == "hyp2")
      mod <- paste(mod, ifelse(mod=="","","\\\\"),"dX_t = \\sigma \\; exp\\Bigl[\\frac{1}{2} \\Bigl( \\alpha \\sqrt{\\delta^2+(X_t-\\mu)^2}-\\beta (X_t-\\mu)\\Bigl)\\Bigl] \\; dW_t")
  }
  return(paste("$$",mod,"$$"))
}


###Function to convert unit of measure of the estimates
changeBase <- function(param, StdErr, delta, original.data, paramName, modelName, newBase, session, choicesUI, anchorId, alertId){
  if (newBase == "delta")
    return(list("Estimate"= param, "Std. Error"=StdErr))
  if(class(index(original.data))=="Date"){
    shinyjs::show(choicesUI)
    closeAlert(session, alertId = alertId)
    seriesLength <- as.numeric(difftime(end(original.data),start(original.data)),units="days")
    if (newBase == "Yearly") dt1 <- seriesLength/365/(length(original.data)-1)
    if (newBase == "Semestral") dt1 <- seriesLength/182.50/(length(original.data)-1)
    if (newBase == "Quarterly") dt1 <- seriesLength/120/(length(original.data)-1)
    if (newBase == "Trimestral") dt1 <- seriesLength/90/(length(original.data)-1)
    if (newBase == "Bimestral") dt1 <- seriesLength/60/(length(original.data)-1)
    if (newBase == "Monthly") dt1 <- seriesLength/30/(length(original.data)-1)
    if (newBase == "Weekly") dt1 <- seriesLength/7/(length(original.data)-1)
    if (newBase == "Daily") dt1 <- seriesLength/(length(original.data)-1)
  }
  if(class(index(original.data))=="numeric"){
    shinyjs::hide(choicesUI)
    dt1 <- as.numeric(end(original.data) - start(original.data))/(length(original.data)-1)
    closeAlert(session, alertId)
    createAlert(session = session, anchorId = anchorId, alertId = alertId, content = "Parameters are in the same unit of measure of input data", style = "info")
  }
  if (modelName %in% c("Brownian Motion","Bm","Geometric Brownian Motion","gBm")){
    if(paramName == "mu") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1))
    if(paramName == "sigma") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1)))
  }
  if (modelName %in% c("Ornstein-Uhlenbeck (OU)","OU")){
    if(paramName == "theta") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1))
  }
  if (modelName %in% c("Vasicek model (VAS)","VAS")){
    if(paramName == "theta1") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1))
    if(paramName == "theta2") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1))
    if(paramName == "theta3") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1)))
  }
  if (modelName %in% c("Constant elasticity of variance (CEV)","CEV")){
    if(paramName == "mu") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1))
    if(paramName == "sigma") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1)))
    if(paramName == "gamma") return(list("Estimate"= param, "Std. Error"=StdErr))
  }
  if (modelName %in% c("Cox-Ingersoll-Ross (CIR)","CIR")){
    if(paramName == "theta1") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1))
    if(paramName == "theta2") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1))
    if(paramName == "theta3") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1)))
  }
  if (modelName %in% c("Chan-Karolyi-Longstaff-Sanders (CKLS)","CKLS")){
    if(paramName == "theta1") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1))
    if(paramName == "theta2") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1))
    if(paramName == "theta3") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1)))
    if(paramName == "theta4") return(list("Estimate"= param, "Std. Error"=StdErr))
  }
  closeAlert(session, alertId)
  createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste("No parameters conversion available for this model. Parameters have been obtained using delta = ", delta), style = "warning")
  shinyjs::hide(choicesUI)
  return(list("Estimate"= param, "Std. Error"=StdErr))
}


addModel <- function(modName, symbName, data, delta, start, startMin, startMax, tries, seed, method="BFGS", fixed = list(), lower, upper, joint=FALSE, aggregation=TRUE, threshold=NULL, session, anchorId){
  info <- list(
    modName = modName,
    method=method,
    delta = delta,
    start = start,
    startMin = startMin,
    startMax = startMax,
    tries = tries,
    seed = seed,
    fixed = fixed,
    lower = lower,
    upper = upper,
    joint = joint,
    aggregation = aggregation,
    threshold = threshold
  )
  clearNA <- function(List){
    for (i in names(List))
      if (is.na(List[[i]]))
        List[[i]] <- NULL
    return (List)
  }
  if(!is.na(seed)) set.seed(seed)
  if(is.na(seed)) set.seed(NULL)
  start <- clearNA(start)
  fixed <- clearNA(fixed)
  lower <- clearNA(lower)
  upper <- clearNA(upper)
  model <- setYuima(data = setData(data, delta = delta), model=setModelByName(name = modName))
  parameters <- setModelByName(modName)@parameter
  if (all(parameters@all %in% c(names(start),names(fixed)))){
    QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #REMOVE# joint = joint, aggregation = aggregation,
                 threshold = threshold))
    if (class(QMLE)=="try-error"){
      createAlert(session = session, anchorId = anchorId, content = paste("Unable to estimate", modName,"on", symbName), style = "danger")
      return()
    }
  }
  else{
    if (modName == "Geometric Brownian Motion" | modName == "gBm"){
      X <- as.numeric(na.omit(Delt(data, type = "log")))
      alpha <- mean(X)/delta
      sigma <- sqrt(var(X)/delta)
      mu <- alpha +0.5*sigma^2
      if (is.null(start$sigma)) start$sigma <- sigma
      if (is.null(start$mu)) start$mu <- mu
      QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #REMOVE# joint = joint, aggregation = aggregation,
                   threshold = threshold))
      if (class(QMLE)=="try-error"){
        createAlert(session = session, anchorId = anchorId, content = paste("Unable to estimate", modName,"on", symbName), style = "danger")
        return()
      }
    #} else if (modName == "Brownian Motion" | modName == "Bm") {
      #Delta <- ifelse(is.null(delta), 1, delta)
      #X <- as.numeric(na.omit(Delt(data, type = "arithmetic")))
      #mu <- mean(X)/Delta
      #sigma <- sqrt(var(X)/Delta)
      #if (is.null(start$sigma)) start$sigma <- sigma
      #if (is.null(start$mu)) start$mu <- mu
      #QMLE <- qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #joint = joint, aggregation = aggregation,
      #             threshold = threshold)
    } else {
      miss <- parameters@all[!(parameters@all %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:tries){
          incProgress(1/tries, detail = paste(iter,"(/", tries ,")"))
          for(j in 1:3){
            for (i in miss)
              start[[i]] <- runif(1, min = max(lower[[i]],startMin[[i]], na.rm = TRUE), max = min(upper[[i]],startMax[[i]],na.rm = TRUE))
            QMLEtemp <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #joint = joint, aggregation = aggregation,
                             threshold = threshold))
            if (class(QMLEtemp)!="try-error")
              break
          }
          if (class(QMLEtemp)!="try-error"){
            repeat{
              m2logL <- summary(QMLEtemp)@m2logL
              coefTable <- summary(QMLEtemp)@coef
              for (param in names(start))
                start[[param]] <- as.numeric(coefTable[param,"Estimate"])
              QMLEtemp <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #joint = joint, aggregation = aggregation,
                               threshold = threshold))
              if (class(QMLEtemp)=="try-error")
                break
              else if (summary(QMLEtemp)@m2logL>=m2logL*abs(sign(m2logL)-0.001))
                break
            }
            if(is.na(m2logL_prec) & class(QMLEtemp)!="try-error"){
              QMLE <- QMLEtemp
              m2logL_prec <- summary(QMLE)@m2logL
              na_prec <- sum(is.na(coefTable))
            }
            else if (class(QMLEtemp)!="try-error"){
              if (sum(is.na(coefTable)) < na_prec){
                QMLE <- QMLEtemp
                m2logL_prec <- summary(QMLE)@m2logL
                na_prec <- sum(is.na(coefTable))
              }
              else if(summary(QMLEtemp)@m2logL < m2logL_prec & sum(is.na(coefTable))==na_prec){
                QMLE <- QMLEtemp
                m2logL_prec <- summary(QMLE)@m2logL
                na_prec <- sum(is.na(coefTable))
              }
            }
          }
          if (iter==tries & class(QMLEtemp)=="try-error" & !exists("QMLE")){
            createAlert(session = session, anchorId = anchorId, content = paste("Unable to estimate", modName,"on", symbName), style = "danger")
            return()
          }
        }
      })
    }
  }
  
  if(!exists("QMLE"))
    return()
  
  yuimaGUIdata$model[[symbName]][[ifelse(is.null(length(yuimaGUIdata$model[[symbName]])),1,length(yuimaGUIdata$model[[symbName]])+1)]] <<- list(
   model = model,
   qmle = QMLE,
   aic = AIC(QMLE),
   bic = BIC(QMLE),
   info = info
 )
}





getModelNames <- function(){
  return(isolate({yuimaGUItable$model}))
}

getModel <- function(symb){
  return(isolate({yuimaGUIdata$model[[symb]]}))
}

delModel <- function(symb, n=1){
  for(i in length(symb):1){
    yuimaGUIdata$model[[symb[i]]][as.numeric(n[i])] <<- NULL
    if (length(yuimaGUIdata$model[[symb[i]]])==0)
      yuimaGUIdata$model[[symb[i]]] <<- NULL
  }
}



addSimulation <- function(model, symbName, info = list("simulate.from"=NA, "simulate.to"=NA,"model"=NA, "estimate.from"=NA, "estimate.to"=NA), xinit, true.parameter, nsim, saveTraj = TRUE, seed=NULL, sampling, subsampling = NULL, space.discretized = FALSE, method = "euler", session, anchorId){
  if(!is.na(seed)) set.seed(seed)
  if(is.na(seed)) set.seed(NULL)
  if(saveTraj==TRUE){
    trajectory <- zoo::zoo(order.by = numeric())
    hist <- NA
  }
  if(saveTraj==FALSE){
    trajectory <- NA
    hist <- vector()
  }
  is.valid <- TRUE
  withProgress(message = 'Simulating: ', value = 0, {
    for (i in 1:nsim){
      incProgress(1/nsim, detail = paste("Simulating:",i,"(/",nsim,")"))
      if(is.null(subsampling))
        simulation <- try(yuima::simulate(object = model, xinit = xinit, true.parameter = true.parameter, nsim = nsim, sampling = sampling, space.discretized = space.discretized, method = method))
      else
        simulation <- try(yuima::simulate(object = model, xinit = xinit, true.parameter = true.parameter, nsim = nsim, sampling = sampling, subsampling = subsampling, space.discretized = space.discretized, method = method))
      if (class(simulation)=="try-error"){
        is.valid <- FALSE
        break()
      }
      if(is.valid){
        if (saveTraj==TRUE)
          trajectory <- merge(trajectory, simulation@data@zoo.data[[1]])
        else
          hist <- c(hist, as.numeric(tail(simulation@data@zoo.data[[1]],1)))
      }
    }
  })
  if (!is.valid){
    createAlert(session = session, anchorId = anchorId, content = paste("Unable to simulate", symbName,"by", info$model), style = "danger")
    return()
  }

  if(saveTraj==TRUE){
    if(class(info$simulate.from)=="Date")
      index(trajectory) <- as.POSIXlt(24*60*60*index(trajectory)/simulation@sampling@delta*as.numeric(info$simulate.to-info$simulate.from)/(simulation@sampling@n), origin = info$simulate.from)
    if(class(info$simulate.from)=="numeric")
      index(trajectory) <- as.numeric(index(trajectory)/simulation@sampling@delta*as.numeric(info$simulate.to-info$simulate.from)/(simulation@sampling@n))
    if(!is.null(colnames(trajectory)))
      colnames(trajectory) <- seq(1:length(colnames(trajectory)))
  }

  info$nsim <- nsim
  yuimaGUIdata$simulation[[symbName]][[ifelse(is.null(length(yuimaGUIdata$simulation[[symbName]])),1,length(yuimaGUIdata$simulation[[symbName]])+1)]] <<- list(
    trajectory = trajectory,
    hist = hist,
    true.parameter = true.parameter,
    info = info
  )
}

delSimulation <- function(symb, n=1){
  for(i in length(symb):1){
    yuimaGUIdata$simulation[[symb[i]]][as.numeric(n[i])] <<- NULL
    if (length(yuimaGUIdata$simulation[[symb[i]]])==0)
      yuimaGUIdata$simulation[[symb[i]]] <<- NULL
  }
}



MYdist <- function(object){
  l <- length(colnames(object))
  d <- matrix(ncol = l, nrow = l)
  f <- function(x, dens){
    res <- c()
    for(xi in x){
      if(xi %in% dens$x)
        res <- c(res,dens$y[which(dens$x==xi)])
      else{
        if (xi > max(dens$x) | xi < min(dens$x))
          res <- c(res,0)
        else{
          i_x1 <- which.min(abs(dens$x-xi))
          i_x2 <- min(i_x1+1,length(dens$x))
          res <- c(res, 0.5*(dens$y[i_x1]+dens$y[i_x2]))
        }
      }
    }
    return(res)
  }
  withProgress(message = 'Clustering: ', value = 0, {
    k <- 1
    for(i in 1:l){
      delta_i <- as.numeric(abs(mean(diff(index(object)[!is.na(object[,i])]))))
      data_i <- as.vector(Delt(na.omit(object[,i])))
      data_i <- data_i[data_i!="Inf"]
      dens1 <-  density(data_i/sqrt(delta_i)+mean(data_i, na.rm = TRUE)*(1/delta_i-1/sqrt(delta_i)), na.rm = TRUE)
      for(j in i:l)
        if (i!=j){
          incProgress(2/(l*(l-1)), detail = paste(k,"(/", l*(l-1)/2 ,")"))
          delta_j <- as.numeric(abs(mean(diff(index(object)[!is.na(object[,j])]))))
          data_j <- as.vector(Delt(na.omit(object[,j])))
          data_j <- data_j[data_j!="Inf"]
          dens2 <-  density(data_j/sqrt(delta_j)+mean(data_j, na.rm = TRUE)*(1/delta_j-1/sqrt(delta_j)), na.rm = TRUE)
          f_dist <- function(x) {abs(f(x,dens1)-f(x,dens2))}
          npoints <- 1000
          dist <- (max(tail(dens1$x,1), tail(dens2$x,1))-min(dens1$x[1],dens2$x[1]))/npoints*0.5*sum(f_dist(seq(from=min(dens1$x[1], dens2$x[1]), to=max(tail(dens1$x,1), tail(dens2$x,1)), length.out = npoints)))
          d[j,i] <- ifelse(dist > 1, 1, dist)
          k <- k + 1
        }
    }
  })
  rownames(d) <- colnames(object)
  colnames(d) <- colnames(object)
  return(as.dist(d))
}



CPanalysis <- function(x, method = c("lSQ", "KSdiff", "KSperc"), pvalue = 0.01){
  if (pvalue > 0.1){
    pvalue <- 0.1
    warning("pvalue re-defined: 0.1")
  }
  if(method=="lSQ"){
    tau <- cpoint(x)$tau0
    return(list(tau=tau, pvalue=NA))
  }
  if(method=="KSdiff" | method=="KSperc"){
    x_incr <- switch (method,
                      "KSdiff" = na.omit(diff(x)),
                      "KSperc" =  na.omit(Delt(x)))
    x_incr_num <- as.numeric(x_incr)
    tau <- c()
    p.value<-c()	
    nTot <-length(x_incr_num)
    n0 <- 1
    repeat{
      ks<-c()
      for (i in seq(from = n0, to=(nTot-1), by = as.integer(1+(nTot-n0)/100))){
        ks[i]<- suppressWarnings(ks.test(x_incr_num[n0:i],x_incr_num[(i+1):nTot])$p.value)
      }
      ifelse(
        min(ks, na.rm=TRUE) > pvalue,
        {
          break
        },
        {
          n0 <- which.min(ks)
          tau <- c(index(x_incr)[n0], tau)
          p.value <- c(ks[n0], p.value)
        }
      )
    }
    if (length(tau)==0){
      tau <- NA
      p.value <- NA
    }
    return (list(tau=tau,pvalue=p.value))
  }  
}




