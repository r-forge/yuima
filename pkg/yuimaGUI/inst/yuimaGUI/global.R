require(DT)
require(shinyjs)
require(yuima)
require(shiny)
require(sde)
require(quantmod)
require(shinydashboard)
require(shinyBS)
require(ggplot2)
require(reshape2)

options(warn=-1) 

if(!exists("yuimaGUItable"))
  yuimaGUItable <<- reactiveValues(series=data.frame(),  model=data.frame(), simulation=data.frame(), hedging=data.frame())

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
        Class = as.character(yuimaGUIdata$model[[symb]][[i]]$info$class),
        Model = as.character(yuimaGUIdata$model[[symb]][[i]]$info$modName),
        Jumps = as.character(yuimaGUIdata$model[[symb]][[i]]$info$jumps),
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
        "Symb" = as.character(symb),
        "Class" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$class),
        "Model" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$model),
        "Jumps" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$jumps),
        "N sim" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$nsim),
        "Simulated from" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$simulate.from),
        "Simulated to" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$simulate.to),
        "Estimated from" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$estimate.from),
        "Estimated to" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$estimate.to),
        check.names = FALSE)
      rownames(newRow) <- as.character(paste(symb," ", i, sep=""))
      yuimaGUItable$simulation <<- rbind(yuimaGUItable$simulation, newRow)
    }
  }
})

observeEvent(yuimaGUIdata$hedging, priority = 10, {
  yuimaGUItable$hedging <<- data.frame()
  if (length(yuimaGUIdata$hedging)!=0){
    for (i in 1:length(yuimaGUIdata$hedging)){
      newRow <- data.frame(
        "Symb" = as.character(yuimaGUIdata$hedging[[i]]$symb),
        "Profit (%)" = round(as.numeric(yuimaGUIdata$hedging[[i]]$info$profit*100),2),
        "Std.Err (%)" = round(as.numeric(yuimaGUIdata$hedging[[i]]$info$stdErr*100),2),
        "Option Lots" = as.integer(yuimaGUIdata$hedging[[i]]$info$LotsToBuy),
        "Assets to Buy" = as.integer(yuimaGUIdata$hedging[[i]]$info$buy),
        "Assets to Sell" = as.integer(yuimaGUIdata$hedging[[i]]$info$sell),
        "Asset Price" = as.numeric(yuimaGUIdata$hedging[[i]]$info$assPrice),
        "Option Price" = as.numeric(yuimaGUIdata$hedging[[i]]$info$optPrice),
        "Option Type" = yuimaGUIdata$hedging[[i]]$info$type,
        "Strike" = as.numeric(yuimaGUIdata$hedging[[i]]$info$strike),
        "Maturity" = as.Date(yuimaGUIdata$hedging[[i]]$info$maturity),
        "Model" = as.character(yuimaGUIdata$hedging[[i]]$info$model),
        "Estimated from" = as.Date(yuimaGUIdata$hedging[[i]]$info$estimate.from),
        "Estimated to" = as.Date(yuimaGUIdata$hedging[[i]]$info$estimate.to),
        "AIC" = as.numeric(yuimaGUIdata$hedging[[i]]$aic),
        "BIC" = as.numeric(yuimaGUIdata$hedging[[i]]$bic),
        check.names = FALSE)
      yuimaGUItable$hedging <<- rbind.fill(yuimaGUItable$hedging, newRow)
    }
  }
})


setDataGUI <- function(original.data, delta){
  t <- index(original.data)
  t0 <- 0
  if(is.numeric(t)){
    delta.original.data <- mean(diff(t), na.rm = TRUE)
    t0 <- min(t, na.rm = TRUE)*delta/delta.original.data
  }
  setData(original.data = original.data, delta = delta, t0 = t0)
}


addData <- function(x, typeIndex, session, anchorId, printSuccess = TRUE){
  x <- data.frame(x, check.names = TRUE)
  err <- c()
  alreadyIn <- c()
  for (symb in colnames(x)){
    if (symb %in% names(yuimaGUIdata$series))
      alreadyIn <- c(alreadyIn, symb)
    else{
      temp <- data.frame("Index" = rownames(x), "symb" = as.numeric(as.character(x[,symb])))
      temp <- temp[complete.cases(temp), ]
      rownames(temp) <- temp[,"Index"]
      colnames(temp) <- c("Index", symb)
      if (typeIndex=="numeric"){
        test <- try(read.zoo(temp, FUN=as.numeric, drop = FALSE))
        if (class(test)!="try-error")
          yuimaGUIdata$series[[symb]] <<- test
        else
          err <- c(err, symb)
      }
      else{
        test <- try(read.zoo(temp, FUN=as.Date, format = typeIndex, drop = FALSE))
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


defaultModels <-  c("Diffusion process"="Geometric Brownian Motion",
                    "Diffusion process"="Brownian Motion",
                    "Diffusion process"="Ornstein-Uhlenbeck (OU)",
                    "Diffusion process"="Vasicek model (VAS)",
                    "Diffusion process"="Constant elasticity of variance (CEV)",
                    "Diffusion process"= "Cox-Ingersoll-Ross (CIR)",
                    "Diffusion process"="Chan-Karolyi-Longstaff-Sanders (CKLS)",
                    "Diffusion process"="Hyperbolic (Barndorff-Nielsen)",
                    "Diffusion process"="Hyperbolic (Bibby and Sorensen)",
                    "Compound Poisson" = "Constant Intensity",
                    "Compound Poisson" = "Linear Intensity",
                    "Compound Poisson" = "Power Low Intensity",
                    "Compound Poisson" = "Exponentially Decaying Intensity",
                    "Compound Poisson" = "Periodic Intensity",
                    #"Fractional process"="Frac. Geometric Brownian Motion",
                    #"Fractional process"="Frac. Brownian Motion",
                    "Fractional process"="Frac. Ornstein-Uhlenbeck (OU)",
                    "CARMA" = "Carma(p,q)",
                    "COGARCH" = "Cogarch(p,q)"
                    )

defaultJumps <- c("Gaussian", "Uniform")

defaultBounds <- function(name, delta, strict, jumps = NA, AR_C = NA, MA_C = NA, data){
  lastPrice = last(data)
  if (name %in% names(isolate({usr_models$model}))){
    par <- setModelByName(name = name, jumps = jumps,  AR_C = AR_C, MA_C = MA_C)@parameter@all
    if(strict==TRUE){
      lower <- rep(NA, length(par))
      upper <- rep(NA, length(par))
    } else {
      if (usr_models$model[[name]]$class=="Compound Poisson"){
        lower <- rep(0, length(par))
        upper <- rep(1, length(par))
      } else {
        lower <- rep(-100, length(par))
        upper <- rep(100, length(par))
      }
      
    }
    names(lower) <- par
    names(upper) <- par
    if (!is.na(jumps)){
      boundsJump <- jumpBounds(jumps = jumps, strict = strict, data = data)
      for (i in par[par %in% names(boundsJump$lower)]){
        lower[[i]] <- boundsJump$lower[[i]]
        upper[[i]] <- boundsJump$upper[[i]]
      }
    }
    return(list(lower=as.list(lower), upper=as.list(upper)))
  }
  if (name %in% defaultModels[names(defaultModels) == "COGARCH"]){
    par <- setModelByName(name = name, jumps = jumps,  AR_C = AR_C, MA_C = MA_C)@parameter
    par <- unique(c(par@drift, par@xinit))
    if(strict==TRUE){
      lower <- rep(NA, length(par))
      upper <- rep(NA, length(par))
    } else {
      lower <- rep(0, length(par))
      upper <- rep(10, length(par))
    }
    names(lower) <- par
    names(upper) <- par
    return(list(lower=as.list(lower), upper=as.list(upper)))
  }
  if (name %in% defaultModels[names(defaultModels) == "CARMA"]){
    par <- setModelByName(name = name, jumps = jumps,  AR_C = AR_C, MA_C = MA_C)@parameter
    par <- par@drift
    if(strict==TRUE){
      lower <- rep(NA, length(par))
      upper <- rep(NA, length(par))
      names(lower) <- par
      names(upper) <- par
    } else {
      lower <- rep(0, length(par))
      upper <- rep(1, length(par))
      names(lower) <- par
      names(upper) <- par
      lower["MA0"] <- min(lastPrice*0.5, lastPrice*1.5)
      upper["MA0"] <- max(lastPrice*0.5, lastPrice*1.5)
    }
    return(list(lower=as.list(lower), upper=as.list(upper)))
  }
  if (name == "Brownian Motion" | name == "Bm"){
    if (strict==TRUE) return (list(lower=list("sigma"=0, "mu"=NA), upper=list("sigma"=NA, "mu"=NA)))
    else return (list(lower=list("sigma"=0, "mu"=-1/delta), upper=list("sigma"=1/sqrt(delta), "mu"=1/delta)))
  }
  if (name == "Geometric Brownian Motion" | name == "gBm") {
    if (strict==TRUE) return (list(lower=list("sigma"=0, "mu"=NA), upper=list("sigma"=NA, "mu"=NA)))
    else return (list(lower=list("sigma"=0, "mu"=-1/delta), upper=list("sigma"=1/sqrt(delta), "mu"=1/delta)))
  }
  if (name == "Ornstein-Uhlenbeck (OU)" | name == "OU"){
    if (strict==TRUE) return(list(lower=list("theta"=0, "sigma"=0),upper=list("theta"=NA, "sigma"=NA)))
    else return(list(lower=list("theta"=0, "sigma"=0),upper=list("theta"=1/delta, "sigma"=1/sqrt(delta))))
  }
  if (name == "Vasicek model (VAS)" | name == "VAS"){
    if (strict==TRUE) return(list(lower=list("theta3"=0, "theta1"=NA, "theta2"=NA), upper=list("theta3"=NA, "theta1"=NA, "theta2"=NA)))
    else return(list(lower=list("theta3"=0, "theta1"=-1/delta, "theta2"=-1/delta), upper=list("theta3"=1/sqrt(delta), "theta1"=1/delta, "theta2"=1/delta)))
  }
  if (name == "Constant elasticity of variance (CEV)" | name == "CEV"){
    if (strict==TRUE) return(list(lower=list("mu"=NA, "sigma"=0, "gamma"=0), upper=list("mu"=NA, "sigma"=NA, "gamma"=NA)))
    else return(list(lower=list("mu"=-1/delta, "sigma"=0, "gamma"=0), upper=list("mu"=1/delta, "sigma"=1/sqrt(delta), "gamma"=3)))
  }
  if (name == "Cox-Ingersoll-Ross (CIR)" | name == "CIR"){
    if (strict==TRUE) return(list(lower=list("theta1"=0,"theta2"=0,"theta3"=0),upper=list("theta1"=NA,"theta2"=NA,"theta3"=NA)))
    else return(list(lower=list("theta1"=0,"theta2"=0,"theta3"=0),upper=list("theta1"=1/delta,"theta2"=1/delta,"theta3"=1/sqrt(delta))))
  }
  if (name == "Chan-Karolyi-Longstaff-Sanders (CKLS)" | name == "CKLS"){
    if (strict==TRUE) return(list(lower=list("theta1"=NA, "theta2"=NA, "theta3"=0, "theta4"=0), upper=list("theta1"=NA, "theta2"=NA, "theta3"=NA, "theta4"=NA)))
    else return(list(lower=list("theta1"=-1/delta, "theta2"=-1/delta, "theta3"=0, "theta4"=0), upper=list("theta1"=1/delta, "theta2"=1/delta, "theta3"=1/sqrt(delta), "theta4"=3)))
  }
  if (name == "Hyperbolic (Barndorff-Nielsen)" | name == "hyp1"){
    if (strict==TRUE) return(list(lower=list("delta"=0, "alpha"=0, "beta"=0, "sigma"=0, "mu"=0), upper=list("delta"=NA, "alpha"=NA, "beta"=NA, "sigma"=NA, "mu"=NA)))
    else return(list(lower=list("delta"=0, "alpha"=0, "beta"=0, "sigma"=0, "mu"=0), upper=list("delta"=100, "alpha"=10, "beta"=10, "sigma"=1/sqrt(delta), "mu"=mean(as.numeric(data), na.rm = TRUE))))
    
  }
  if (name == "Hyperbolic (Bibby and Sorensen)" | name == "hyp2"){
    if (strict==TRUE) return(list(lower=list("delta"=0, "alpha"=0, "beta"=0, "sigma"=0, "mu"=0), upper=list("delta"=NA, "alpha"=NA, "beta"=NA, "sigma"=NA, "mu"=NA)))
    else return(list(lower=list("delta"=0, "alpha"=0, "beta"=0, "sigma"=0, "mu"=0),upper=list("delta"=10, "alpha"=1, "beta"=10, "sigma"=1/sqrt(delta), "mu"=mean(as.numeric(data), na.rm = TRUE))))
  }
  if (name == "Constant Intensity"){
    boundsJump <- jumpBounds(jumps = jumps, strict = strict, data = data)
    if (strict==TRUE) return(list(lower=c(list("lambda"=0), boundsJump$lower),upper=c(list("lambda"=NA), boundsJump$upper)))
    else return(list(lower=c(list("lambda"=0), boundsJump$lower),upper=c(list("lambda"=1/delta), boundsJump$upper)))
  }
  if (name == "Power Low Intensity"){
    boundsJump <- jumpBounds(jumps = jumps, strict = strict, data = data)
    if (strict==TRUE) return(list(lower=c(list("alpha"=0, "beta"=NA), boundsJump$lower),upper=c(list("alpha"=NA, "beta"=NA), boundsJump$upper)))
    else return(list(lower=c(list("alpha"=0, "beta"=-3), boundsJump$lower),upper=c(list("alpha"=0.1/delta^(3/2), "beta"=3), boundsJump$upper)))
  }
  if (name == "Linear Intensity"){
    boundsJump <- jumpBounds(jumps = jumps, strict = strict, data = data)
    if (strict==TRUE) return(list(lower=c(list("alpha"=0, "beta"=0), boundsJump$lower),upper=c(list("alpha"=NA, "beta"=NA), boundsJump$upper)))
    else return(list(lower=c(list("alpha"=0, "beta"=0), boundsJump$lower),upper=c(list("alpha"=1/delta, "beta"=0.1/delta^2), boundsJump$upper)))
  }
  if (name == "Exponentially Decaying Intensity"){
    boundsJump <- jumpBounds(jumps = jumps, strict = strict, data = data)
    if (strict==TRUE) return(list(lower=c(list("alpha"=0, "beta"=0), boundsJump$lower),upper=c(list("alpha"=NA, "beta"=NA), boundsJump$upper)))
    else return(list(lower=c(list("alpha"=0, "beta"=0), boundsJump$lower),upper=c(list("alpha"=1/delta, "beta"=1/delta), boundsJump$upper)))
  }
  if (name == "Periodic Intensity"){
    boundsJump <- jumpBounds(jumps = jumps, strict = strict, data = data)
    if (strict==TRUE) return(list(lower=c(list("a"=0, "b"=0, "omega"=0, "phi"=0), boundsJump$lower),upper=c(list("a"=NA, "b"=NA, "omega"=NA, "phi"=2*pi), boundsJump$upper)))
    else return(list(lower=c(list("a"=0, "b"=0, "omega"=0, "phi"=0), boundsJump$lower),upper=c(list("a"=1/delta, "b"=1/delta, "omega"=1/delta, "phi"=2*pi), boundsJump$upper)))
  }
}


setJumps <- function(jumps){
  switch (jumps,
          "Gaussian" = list("dnorm(z, mean = mu_jump, sd = sigma_jump)"),
          "Uniform" = list("dunif(z, min = a_jump, max = b_jump)")
  )
}

jumpBounds <- function(jumps, data, strict){
  switch(jumps,
         "Gaussian" = {
           if(strict==TRUE) return(list(lower=list("mu_jump"=NA, "sigma_jump"=0), upper=list("mu_jump"=NA, "sigma_jump"=NA)))
           else {
             mu <- mean(diff(data))
             s <- sd(diff(data))
             return(list(lower=list("mu_jump"=mu, "sigma_jump"=s), upper=list("mu_jump"=mu, "sigma_jump"=s)))
           }
          },
         "Uniform" = {
            if(strict==TRUE) return(list(lower=list("a_jump"=NA, "b_jump"=NA), upper=list("a_jump"=NA, "b_jump"=NA)))
            else {
              a <- min(diff(data))
              b <- max(diff(data))
              return(list(lower=list("a_jump"=a, "b_jump"=b), upper=list("a_jump"=a, "b_jump"=b)))
            }
           }
  )
}

latexJumps <- function(jumps){
  if (!is.null(jumps)){
    switch (jumps,
            "Gaussian" = "Y_i \\sim N(\\mu_{jump}, \\; \\sigma_{jump})",
            "Uniform" = "Y_i \\sim Unif(a_{jump}, \\; b_{jump})"
    )
  }
}



setModelByName <- function(name, jumps = NA, AR_C = NA, MA_C = NA, XinExpr = FALSE){
  if (name %in% names(isolate({usr_models$model}))){
    if (isolate({usr_models$model[[name]]$class=="Diffusion process" | usr_models$model[[name]]$class=="Fractional process"}))
      return(isolate({usr_models$model[[name]]$object}))
    if (isolate({usr_models$model[[name]]$class=="Compound Poisson"}))
      return(setPoisson(intensity = isolate({usr_models$model[[name]]$intensity}), df = setJumps(jumps = jumps), solve.variable = "x"))
  }
  if (name == "Brownian Motion" | name == "Bm") return(yuima::setModel(drift="mu", diffusion="sigma", solve.variable = "x"))
  if (name == "Geometric Brownian Motion" | name == "gBm") return(yuima::setModel(drift="mu*x", diffusion="sigma*x", solve.variable = "x"))
  if (name == "Ornstein-Uhlenbeck (OU)" | name == "OU") return(yuima::setModel(drift="-theta*x", diffusion="sigma", solve.variable = "x"))
  if (name == "Vasicek model (VAS)" | name == "VAS") return(yuima::setModel(drift="theta1-theta2*x", diffusion="theta3", solve.variable = "x"))
  if (name == "Constant elasticity of variance (CEV)" | name == "CEV") return(yuima::setModel(drift="mu*x", diffusion="sigma*x^gamma", solve.variable = "x"))
  if (name == "Cox-Ingersoll-Ross (CIR)" | name == "CIR") return(yuima::setModel(drift="theta1-theta2*x", diffusion="theta3*sqrt(x)", solve.variable = "x"))
  if (name == "Chan-Karolyi-Longstaff-Sanders (CKLS)" | name == "CKLS") return(yuima::setModel(drift="theta1+theta2*x", diffusion="theta3*x^theta4", solve.variable = "x"))
  if (name == "Hyperbolic (Barndorff-Nielsen)" | name == "hyp1") return(yuima::setModel(drift="(sigma/2)^2*(beta-alpha*((x-mu)/(sqrt(delta^2+(x-mu)^2))))", diffusion="sigma", solve.variable = "x"))
  if (name == "Hyperbolic (Bibby and Sorensen)" | name == "hyp2") return(yuima::setModel(drift="0", diffusion="sigma*exp(0.5*(alpha*sqrt(delta^2+(x-mu)^2)-beta*(x-mu)))", solve.variable = "x"))
  if (name == "Frac. Brownian Motion" | name == "Bm") return(yuima::setModel(drift="mu", diffusion="sigma", solve.variable = "x", hurst = NA))
  if (name == "Frac. Geometric Brownian Motion" | name == "gBm") return(yuima::setModel(drift="mu*x", diffusion="sigma*x", solve.variable = "x", hurst = NA))
  if (name == "Frac. Ornstein-Uhlenbeck (OU)" | name == "OU") return(yuima::setModel(drift="-theta*x", diffusion="sigma", solve.variable = "x", hurst = NA))
  if (name == "Power Low Intensity") return(yuima::setPoisson(intensity="alpha*t^(beta)", df=setJumps(jumps = jumps), solve.variable = "x"))
  if (name == "Constant Intensity") return(yuima::setPoisson(intensity="lambda", df=setJumps(jumps = jumps), solve.variable = "x"))
  if (name == "Linear Intensity") return(yuima::setPoisson(intensity="alpha+beta*t", df=setJumps(jumps = jumps), solve.variable = "x"))
  if (name == "Exponentially Decaying Intensity") return(yuima::setPoisson(intensity="alpha*exp(-beta*t)", df=setJumps(jumps = jumps), solve.variable = "x"))
  if (name == "Periodic Intensity") return(yuima::setPoisson(intensity="a/2*(1+cos(omega*t+phi))+b", df=setJumps(jumps = jumps), solve.variable = "x"))
  if (name == "Cogarch(p,q)") return(yuima::setCogarch(p = MA_C, q = AR_C, measure.type = "CP", measure = list(intensity = "lambda", df = setJumps(jumps = "Gaussian")), XinExpr = XinExpr, Cogarch.var="y", V.var="v", Latent.var="x", ma.par="MA", ar.par="AR")) 
  if (name == "Carma(p,q)") return(yuima::setCarma(p = AR_C, q = MA_C, ma.par="MA", ar.par="AR", XinExpr = XinExpr))
}

printModelLatex <- function(names, process, jumps = NA){
  if (process=="Diffusion process"){
    mod <- ""
    for (name in names){
      if (name %in% names(isolate({usr_models$model}))){
        text <- toLatex(setModelByName(name))
        x <- paste(text[2:9], collapse = "")
        x <- substr(x,3,nchar(x))
        x <- gsub(x, pattern = "'", replacement = "")
        x <- gsub(x, pattern = "x", replacement = "X_t")
        x <- gsub(x, pattern = "W1", replacement = "W_t")
        mod <- paste(mod, ifelse(mod=="","","\\\\"), x)
      }
      if (name == "Brownian Motion" | name == "Bm")
        mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = \\mu \\; dt + \\sigma \\; dW_t")
      if (name == "Geometric Brownian Motion" | name == "gBm")
        mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = \\mu X_t \\; dt + \\sigma X_t \\; dW_t")
      if (name == "Ornstein-Uhlenbeck (OU)" | name == "OU")
        mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = -\\theta X_t \\; dt + \\sigma \\; dW_t")
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
  if (process=="Fractional process"){
    mod <- ""
    for (name in names){
      if (name %in% names(isolate({usr_models$model}))){
        text <- toLatex(setModelByName(name))
        x <- paste(text[2:9], collapse = "")
        x <- substr(x,3,nchar(x))
        x <- gsub(x, pattern = "'", replacement = "")
        x <- gsub(x, pattern = "x", replacement = "X_t")
        x <- gsub(x, pattern = "W1", replacement = "W_t^H")
        mod <- paste(mod, ifelse(mod=="","","\\\\"), x)
      }
      if (name == "Frac. Brownian Motion" | name == "Bm")
        mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = \\mu \\; dt + \\sigma \\; dW_t^H")
      if (name == "Frac. Geometric Brownian Motion" | name == "gBm")
        mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = \\mu X_t \\; dt + \\sigma X_t \\; dW_t^H")
      if (name == "Frac. Ornstein-Uhlenbeck (OU)" | name == "OU")
        mod <- paste(mod, ifelse(mod=="","","\\\\"), "dX_t = -\\theta X_t \\; dt + \\sigma \\; dW_t^H")
    }
    return(paste("$$",mod,"$$"))
  }
  if (process=="Compound Poisson"){
    mod <- paste("X_t = X_0+\\sum_{i=0}^{N_t} Y_i \\; : \\;\\;\\;  N_t \\sim Poi\\Bigl(\\int_0^t \\lambda(t)dt\\Bigl)", ifelse(!is.null(jumps), paste(", \\;\\;\\;\\; ", latexJumps(jumps)),""))
    for (name in names){
      if (name %in% names(isolate({usr_models$model}))){
        text <- paste("\\lambda(t)=",usr_models$model[[name]]$intensity)
        mod <- paste(mod, ifelse(mod=="","","\\\\"), text)
      }
      if (name == "Power Low Intensity") mod <- paste(mod, ifelse(mod=="","","\\\\"), "\\lambda(t)=\\alpha \\; t^{\\beta}")
      if (name == "Constant Intensity")  mod <- paste(mod, ifelse(mod=="","","\\\\"), "\\lambda(t)=\\lambda")
      if (name == "Linear Intensity")  mod <- paste(mod, ifelse(mod=="","","\\\\"), "\\lambda(t)=\\alpha+\\beta \\; t")
      if (name == "Exponentially Decaying Intensity")  mod <- paste(mod, ifelse(mod=="","","\\\\"), "\\lambda(t)=\\alpha \\; e^{-\\beta t}")
      if (name == "Periodic Intensity")  mod <- paste(mod, ifelse(mod=="","","\\\\"), "\\lambda(t)=\\frac{a}{2}\\bigl(1+cos(\\omega t + \\phi)\\bigl)+b")
    }
    return(paste("$$",mod,"$$"))
  }
  if (process=="COGARCH"){
    return(paste("$$","COGARCH(p,q)","$$"))
  }
  if (process=="CARMA"){
    return(paste("$$","CARMA(p,q)","$$"))
  }
}


###Function to convert unit of measure of the estimates
changeBaseP <- function(param, StdErr, delta, original.data, paramName, modelName, newBase, allParam){
  msg <- NULL
  if (newBase == "delta")
    return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
  if(class(index(original.data))=="Date"){
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
    dt1 <- as.numeric(end(original.data) - start(original.data))/(length(original.data)-1)
    msg <- "Parameters are in the same unit of measure of input data"
  }
  if (modelName %in% c("Brownian Motion","Bm","Geometric Brownian Motion","gBm")){
    if(paramName == "mu") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "sigma") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1), "msg"=msg))
  }
  if (modelName %in% c("Ornstein-Uhlenbeck (OU)","OU")){
    if(paramName == "theta") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "sigma") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1), "msg"=msg))
  }
  if (modelName %in% c("Vasicek model (VAS)","VAS")){
    if(paramName == "theta1") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "theta2") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "theta3") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1), "msg"=msg))
  }
  if (modelName %in% c("Constant elasticity of variance (CEV)","CEV")){
    if(paramName == "mu") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "sigma") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1), "msg"=msg))
    if(paramName == "gamma") return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
  }
  if (modelName %in% c("Cox-Ingersoll-Ross (CIR)","CIR")){
    if(paramName == "theta1") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "theta2") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "theta3") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1), "msg"=msg))
  }
  if (modelName %in% c("Chan-Karolyi-Longstaff-Sanders (CKLS)","CKLS")){
    if(paramName == "theta1") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "theta2") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "theta3") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1), "msg"=msg))
    if(paramName == "theta4") return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
  }
  if (modelName %in% c("Hyperbolic (Barndorff-Nielsen)", "Hyperbolic (Bibby and Sorensen)")){
    if(paramName == "sigma") return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1), "msg"=msg))
    if(paramName == "beta") return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
    if(paramName == "alpha") return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
    if(paramName == "mu") return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
    if(paramName == "delta") return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
  }
  if (modelName %in% c("Constant Intensity")){
    if(paramName == "lambda") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName %in% c("mu_jump", "sigma_jump", "a_jump", "b_jump")) return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
  }
  if (modelName %in% c("Linear Intensity")){
    if(paramName == "alpha") return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName == "beta") return(list("Estimate"= param*(delta/dt1)^2, "Std. Error"=StdErr*(delta/dt1)^2, "msg"=msg))
    if(paramName %in% c("mu_jump", "sigma_jump", "a_jump", "b_jump")) return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
  }
  if (modelName %in% c("Power Low Intensity")){
    beta <- as.numeric(allParam["beta"])
    if(paramName == "alpha") return(list("Estimate"= param*(delta/dt1)^(beta+1), "Std. Error"=StdErr*(delta/dt1)^(beta+1), "msg"=msg))
    if(paramName %in% c("beta", "mu_jump", "sigma_jump", "a_jump", "b_jump")) return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
  }
  if (modelName %in% c("Exponentially Decaying Intensity")){
    if(paramName %in% c("alpha", "beta")) return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName %in% c("mu_jump", "sigma_jump", "a_jump", "b_jump")) return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
  }
  if (modelName %in% c("Periodic Intensity")){
    if(paramName %in% c("a", "b", "omega")) return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(paramName %in% c("phi", "mu_jump", "sigma_jump", "a_jump", "b_jump")) return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg))
  }
  msg <- paste("No parameters conversion available for this model. Parameters have been obtained using delta = ", delta)
  return(list("Estimate"= param, "Std. Error"=StdErr, "msg"=msg, "conversion"=FALSE))
}

###Function to manipulate digits
signifDigits <- function(value, sd){
  if (is.na(sd) | sd=="NaN" | sd==0)
    return (value)
  else{
    pow <- 10^(1-as.integer(log10(as.numeric(sd))))
    return(round(as.numeric(value)*pow)/pow)
  }
}

changeBase <- function(table, yuimaGUI, newBase = input$baseModels, session = session, choicesUI="baseModels", anchorId = "modelsAlert", alertId = "modelsAlert_conversion"){
  closeAlert(session, alertId)
  shinyjs::toggle(id = choicesUI, condition = (class(index(yuimaGUI$model@data@original.data))=="Date"))
  outputTable <- data.frame()
  for (param in unique(colnames(table))){
    temp <- changeBaseP(param = as.numeric(table["Estimate",param]), StdErr = as.numeric(table["Std. Error",param]), delta = yuimaGUI$model@sampling@delta, original.data = yuimaGUI$model@data@original.data, paramName = param, modelName = yuimaGUI$info$modName, newBase = newBase, allParam = table["Estimate",])
    outputTable["Estimate",param] <- as.character(signifDigits(temp[["Estimate"]],temp[["Std. Error"]]))
    outputTable["Std. Error",param] <- as.character(signifDigits(temp[["Std. Error"]],temp[["Std. Error"]]))
  }
  colnames(outputTable) <- unique(colnames(table))
  style <- "info"
  msg <- NULL
  if (any(outputTable["Std. Error",] %in% c(0, "NA", "NaN", "<NA>", NA, NaN))){
    msg <- "The estimated model does not satisfy theoretical properties."
    style <- "warning"
  }
  if (!is.null(temp$conversion)) if (temp$conversion==FALSE) shinyjs::hide(choicesUI)
  if (yuimaGUI$info$class=="COGARCH") {
    test <- try(Diagnostic.Cogarch(yuimaGUI$model, param = as.list(coef(yuimaGUI$qmle))))
    if (class(test)=="try-error") createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste("The estimated model does not satisfy theoretical properties.", temp$msg), style = "warning")
    else if(test$stationary==FALSE | test$positivity==FALSE) createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste("The estimated model does not satisfy theoretical properties.", temp$msg), style = "warning")
    else createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste(msg, temp$msg), style = style)
  } 
  else if (yuimaGUI$info$class=="CARMA") {
    test <- try(Diagnostic.Carma(yuimaGUI$qmle))
    if (class(test)=="try-error") createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste("The estimated model does not satisfy theoretical properties.", temp$msg), style = "warning")
    else if(test==FALSE) createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste("The estimated model does not satisfy theoretical properties.", temp$msg), style = "warning")
    else createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste(msg, temp$msg), style = style)
  }
  else if (!is.null(temp$msg) | !is.null(msg)) createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste(msg, temp$msg), style = style)
  return(outputTable)
}



qmleGUI <- function(upper, lower, ...){
  if(length(upper)!=0 & length(lower)!=0)
    return (qmle(upper = upper, lower = lower, ...))
  if(length(upper)!=0 & length(lower)==0)
    return (qmle(upper = upper, ...))
  if(length(upper)==0 & length(lower)!=0)
    return (qmle(lower = lower, ...))
  if(length(upper)==0 & length(lower)==0)
    return (qmle(...))
}

clearNA <- function(List){
  for (i in names(List))
    if (is.na(List[[i]]))
      List[[i]] <- NULL
    return (List)
}

addModel <- function(modName, modClass, AR_C, MA_C, jumps, symbName, data, toLog, delta, start, startMin, startMax, trials, seed, method="BFGS", fixed = list(), lower, upper, joint=FALSE, aggregation=TRUE, threshold=NULL, session, anchorId, alertId){
  info <- list(
    class = modClass,
    modName = modName,
    AR = AR_C,
    MA = MA_C,
    jumps = ifelse(is.null(jumps),NA,jumps),
    method=method,
    delta = delta,
    toLog = toLog,
    start = start,
    startMin = startMin,
    startMax = startMax,
    trials = trials,
    seed = seed,
    fixed = fixed,
    lower = lower,
    upper = upper,
    joint = joint,
    aggregation = aggregation,
    threshold = threshold
  )
  if(!is.na(seed)) set.seed(seed)
  if(is.na(seed)) set.seed(NULL)
  start <- clearNA(start)
  fixed <- clearNA(fixed)
  lower <- clearNA(lower)
  upper <- clearNA(upper)
  if(toLog==TRUE) data <- try(log(data))
  if(class(data)=="try-error"){
    createAlert(session = session, anchorId = anchorId, alertId = alertId, content =  paste("Cannot convert series ", symbName, "to log. Try to use 'Advanced Settings' and customize estimation.", sep = ""), style = "error")
    return()
  }
  model <- setYuima(data = setDataGUI(data, delta = delta), model=setModelByName(name = modName, jumps = jumps, MA_C = MA_C, AR_C = AR_C))
  index(model@data@original.data) <- index(data)
  parameters <- model@model@parameter
  if (modName == "Geometric Brownian Motion" | modName == "gBm"){
    X <- as.numeric(na.omit(Delt(data, type = "log")))
    alpha <- mean(X)/delta
    sigma <- sqrt(var(X)/delta)
    mu <- alpha +0.5*sigma^2
    if (is.null(start$sigma)) start$sigma <- sigma
    if (is.null(start$mu)) start$mu <- mu
    QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, rcpp = TRUE))
    if (class(QMLE)=="try-error"){
      createAlert(session = session, anchorId = anchorId, alertId = alertId, content =  paste("Unable to estimate ", modName," on ", symbName, ". Try to use 'Advanced Settings' and customize estimation.", sep = ""), style = "danger")
      return()
    }
  } 
  else if (modClass == "Fractional process"){
    QMLEtemp <- try(mmfrac(model))
    if(class(QMLEtemp)!="try-error") {
      estimates <- QMLEtemp[[1]]
      dev <- diag(QMLEtemp[[2]])
      QMLE <- rbind(estimates, dev)
      col <- gsub(colnames(QMLE), pattern = "\\(", replacement = "")
      col <- gsub(col, pattern = "\\)", replacement = "")
      colnames(QMLE) <- col
      rownames(QMLE) <- c("Estimate", "Std. Error")
    }
  }
  else if (modClass=="CARMA") {
    allParam <- parameters@drift
    if (all(allParam %in% c(names(start),names(fixed))))
      QMLE <- try(qmleGUI(model, start = start, method = method, lower = lower, upper = upper))
    else {
      miss <- allParam[!(allParam %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:trials){
          incProgress(1/trials, detail = paste(iter,"(/", trials ,")"))
          for(j in 1:3){
            for (i in miss)
              start[[i]] <- runif(1, min = max(lower[[i]],startMin[[i]], na.rm = TRUE), max = min(upper[[i]],startMax[[i]],na.rm = TRUE))
            QMLEtemp <- try(qmleGUI(model, start = start, method = method, lower = lower, upper = upper))
            if (class(QMLEtemp)!="try-error") if (all(!is.na(summary(QMLEtemp)@coef[,"Estimate"])))
              break
          }
          if (class(QMLEtemp)!="try-error") if (all(!is.na(summary(QMLEtemp)@coef[,"Estimate"]))){
            repeat{
              m2logL <- summary(QMLEtemp)@m2logL
              coefTable <- summary(QMLEtemp)@coef
              for (param in rownames(coefTable))
                start[[param]] <- as.numeric(coefTable[param,"Estimate"])
              QMLEtemp <- try(qmleGUI(model, start = start, method = method, lower = lower, upper = upper))
              if (class(QMLEtemp)=="try-error") break
              else if(summary(QMLEtemp)@m2logL>=m2logL*abs(sign(m2logL)-0.001)) break
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
              else {
                test <- summary(QMLEtemp)@m2logL
                if(test < m2logL_prec & sum(is.na(coefTable))==na_prec){
                  QMLE <- QMLEtemp
                  m2logL_prec <- test
                  na_prec <- sum(is.na(coefTable))
                }
              }
            }
          }
        }
      })
    }
  }
  else if (modClass=="COGARCH") {
    allParam <- unique(c(parameters@drift, parameters@xinit))
    if (all(allParam %in% c(names(start),names(fixed))))
      QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #REMOVE# joint = joint, aggregation = aggregation,
                       threshold = threshold, grideq = TRUE, rcpp = TRUE))
    else {
      miss <- allParam[!(allParam %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:trials){
          incProgress(1/trials, detail = paste(iter,"(/", trials ,")"))
          for(j in 1:3){
            for (i in miss)
              start[[i]] <- runif(1, min = max(lower[[i]],startMin[[i]], na.rm = TRUE), max = min(upper[[i]],startMax[[i]],na.rm = TRUE))
            QMLEtemp <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #joint = joint, aggregation = aggregation,
                                 threshold = threshold, grideq = TRUE, rcpp = TRUE))
            if (class(QMLEtemp)!="try-error") if (all(!is.na(summary(QMLEtemp)@coef[,"Estimate"])))
              break
          }
          if (class(QMLEtemp)!="try-error") if (all(!is.na(summary(QMLEtemp)@coef[,"Estimate"]))){
            repeat{
              m2logL <- summary(QMLEtemp)@objFunVal
              coefTable <- summary(QMLEtemp)@coef
              for (param in rownames(coefTable))
                start[[param]] <- as.numeric(coefTable[param,"Estimate"])
              QMLEtemp <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #joint = joint, aggregation = aggregation,
                                   threshold = threshold, grideq = TRUE, rcpp = TRUE))
              if (class(QMLEtemp)=="try-error") break
              else if(summary(QMLEtemp)@objFunVal>=m2logL*abs(sign(m2logL)-0.001)) break
            }
            if(is.na(m2logL_prec) & class(QMLEtemp)!="try-error"){
              QMLE <- QMLEtemp
              m2logL_prec <- summary(QMLE)@objFunVal
              na_prec <- sum(is.na(coefTable))
            }
            else if (class(QMLEtemp)!="try-error"){
              if (sum(is.na(coefTable)) < na_prec){
                QMLE <- QMLEtemp
                m2logL_prec <- summary(QMLE)@objFunVal
                na_prec <- sum(is.na(coefTable))
              }
              else {
                test <- summary(QMLEtemp)@objFunVal
                if(test < m2logL_prec & sum(is.na(coefTable))==na_prec){
                  QMLE <- QMLEtemp
                  m2logL_prec <- test
                  na_prec <- sum(is.na(coefTable))
                }
              }
            }
          }
        }
      })
    }
  }
  else if (modClass == "Compound Poisson") {
    if (all(parameters@all %in% c(names(start),names(fixed))))
      QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #REMOVE# joint = joint, aggregation = aggregation,
                        threshold = threshold))
    else {
      miss <- parameters@all[!(parameters@all %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:trials){
          incProgress(1/trials, detail = paste(iter,"(/", trials ,")"))
          for(j in 1:3){
            for (i in miss)
              start[[i]] <- runif(1, min = max(lower[[i]],startMin[[i]], na.rm = TRUE), max = min(upper[[i]],startMax[[i]],na.rm = TRUE))
            QMLEtemp <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #joint = joint, aggregation = aggregation,
                                 threshold = threshold))
            if (class(QMLEtemp)!="try-error") if (all(!is.na(summary(QMLEtemp)@coef[,"Estimate"])))
              break
          }
          if (class(QMLEtemp)!="try-error") if (all(!is.na(summary(QMLEtemp)@coef[,"Estimate"]))){
            repeat{
              m2logL <- summary(QMLEtemp)@m2logL
              coefTable <- summary(QMLEtemp)@coef
              for (param in names(start))
                start[[param]] <- as.numeric(coefTable[param,"Estimate"])
              QMLEtemp <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #joint = joint, aggregation = aggregation,
                                   threshold = threshold))
              if (class(QMLEtemp)=="try-error") break
              else if (summary(QMLEtemp)@m2logL>=m2logL*abs(sign(m2logL)-0.001)) break
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
              else {
                test <- summary(QMLEtemp)@m2logL
                if(test < m2logL_prec & sum(is.na(coefTable))==na_prec){
                  QMLE <- QMLEtemp
                  m2logL_prec <- test
                  na_prec <- sum(is.na(coefTable))
                }
              }
            }
          }
        }
      })
    }
  }
  else {
    if (all(parameters@all %in% c(names(start),names(fixed))))
      QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #REMOVE# joint = joint, aggregation = aggregation,
                       threshold = threshold, rcpp = TRUE))
    else {
      miss <- parameters@all[!(parameters@all %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:trials){
          incProgress(1/trials, detail = paste(iter,"(/", trials ,")"))
          for(j in 1:3){
            for (i in miss)
              start[[i]] <- runif(1, min = max(lower[[i]],startMin[[i]], na.rm = TRUE), max = min(upper[[i]],startMax[[i]],na.rm = TRUE))
            QMLEtemp <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #joint = joint, aggregation = aggregation,
                                 threshold = threshold, rcpp = TRUE))
            if (class(QMLEtemp)!="try-error") if (all(!is.na(summary(QMLEtemp)@coef[,"Estimate"])))
              break
          }
          if (class(QMLEtemp)!="try-error") if (all(!is.na(summary(QMLEtemp)@coef[,"Estimate"]))){
            repeat{
              m2logL <- summary(QMLEtemp)@m2logL
              coefTable <- summary(QMLEtemp)@coef
              for (param in names(start))
                start[[param]] <- as.numeric(coefTable[param,"Estimate"])
              QMLEtemp <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #joint = joint, aggregation = aggregation,
                                   threshold = threshold, rcpp = TRUE))
              if (class(QMLEtemp)=="try-error") break
              else if (summary(QMLEtemp)@m2logL>=m2logL*abs(sign(m2logL)-0.001)) break
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
              else {
                test <- summary(QMLEtemp)@m2logL
                if(test < m2logL_prec & sum(is.na(coefTable))==na_prec){
                  QMLE <- QMLEtemp
                  m2logL_prec <- test
                  na_prec <- sum(is.na(coefTable))
                }
              }
            }
          }
        }
      })
    }
  }
  
  if (!exists("QMLE")){
    createAlert(session = session, anchorId = anchorId, alertId = alertId, content =  paste("Unable to estimate ", modName," on ", symbName, ". Try to use 'Advanced Settings' and customize estimation.", sep = ""), style = "error")
    return()
  }
  
  yuimaGUIdata$model[[symbName]][[ifelse(is.null(length(yuimaGUIdata$model[[symbName]])),1,length(yuimaGUIdata$model[[symbName]])+1)]] <<- list(
   model = model,
   qmle = QMLE,
   aic = ifelse(!(modClass %in% c("CARMA","COGARCH","Fractional process")), AIC(QMLE), NA),
   bic = ifelse(!(modClass %in% c("CARMA","COGARCH","Fractional process")), BIC(QMLE), NA),
   info = info
 )
}



addCPoint <- function(modelName, symb, from, to, delta, toLog, start, startMin, startMax, method, trials, seed, lower, upper, fracL, fracR){
  series <- getData(symb)
  if(class(index(series)[1])=="Date") series <- window(series, start = as.Date(from), end = as.Date(to))
  else series <- window(series, start = as.numeric(from), end = as.numeric(to))
  mod <- setModelByName(name = modelName)
  if(!is.na(seed)) set.seed(seed)
  if(is.na(seed)) set.seed(NULL)
  start <- clearNA(start)
  lower <- clearNA(lower)
  upper <- clearNA(upper)
  if(toLog==TRUE) series <- try(log(series))
  if(class(series)=="try-error") stop()
  info <- list(
    symb = symb,
    seed = seed,
    model = modelName,
    toLog = toLog,
    trials = trials,
    method = "L-BFGS-B"
  )
  yuima <- setYuima(data = setDataGUI(series, delta = delta), model = mod)
  t0 <- start(yuima@data@zoo.data[[1]])
  miss <- mod@parameter@all[!(mod@parameter@all %in% names(start))]
  m2logL_prec <- NA
  na_prec <- NA
  for(iter in 1:trials){
    for(j in 1:3){
      for (i in miss)
        start[[i]] <- runif(1, min = max(lower[[i]],startMin[[i]], na.rm = TRUE), max = min(upper[[i]],startMax[[i]],na.rm = TRUE))
      QMLEtempL <- try(qmleL(yuima = yuima, t = t0 + fracL*length(series)*delta, start = start, method="L-BFGS-B", lower = lower, upper = upper, rcpp = TRUE))
      if (class(QMLEtempL)!="try-error") if (all(!is.na(summary(QMLEtempL)@coef[,"Estimate"])))
        break
    }
    if (class(QMLEtempL)!="try-error") if (all(!is.na(summary(QMLEtempL)@coef[,"Estimate"]))){
      repeat{
        m2logL <- summary(QMLEtempL)@m2logL
        coefTable <- summary(QMLEtempL)@coef
        for (param in names(start))
          start[[param]] <- as.numeric(coefTable[param,"Estimate"])
        QMLEtempL <- try(qmleL(yuima = yuima, t = t0 + fracL*length(series)*delta, start = start, method="L-BFGS-B", lower = lower, upper = upper, rcpp = TRUE))
        if (class(QMLEtempL)=="try-error") break
        else if (summary(QMLEtempL)@m2logL>=m2logL*abs(sign(m2logL)-0.001)) break
      }
      if(is.na(m2logL_prec) & class(QMLEtempL)!="try-error"){
        QMLEL <- QMLEtempL
        m2logL_prec <- summary(QMLEL)@m2logL
        na_prec <- sum(is.na(coefTable))
      }
      else if (class(QMLEtempL)!="try-error"){
        if (sum(is.na(coefTable)) < na_prec){
          QMLEL <- QMLEtempL
          m2logL_prec <- summary(QMLEL)@m2logL
          na_prec <- sum(is.na(coefTable))
        }
        else {
          test <- summary(QMLEtempL)@m2logL
          if(test < m2logL_prec & sum(is.na(coefTable))==na_prec){
            QMLEL <- QMLEtempL
            m2logL_prec <- test
            na_prec <- sum(is.na(coefTable))
          }
        }
      }
    }
  }
  if (!exists("QMLEL")) stop()
  
  tmpL <- QMLEL
  tmpR <- try(qmleR(yuima = yuima, t = t0 + fracR*length(series)*delta, start = as.list(coef(tmpL)), method="L-BFGS-B", lower = lower, upper = upper, rcpp = TRUE))

  if (class(tmpR)=="try-error") stop()
  
  cp_prec <- try(CPoint(yuima = yuima, param1=coef(tmpL), param2=coef(tmpR)))
  if(class(cp_prec)=="try-error") stop()
  diff_prec <- delta*nrow(series)
  repeat{
    tmpL <- try(qmleL(yuima, start=as.list(coef(tmpL)), t = cp_prec$tau, lower=lower, upper = upper, method="L-BFGS-B", rcpp = TRUE))
    if(class(tmpL)=="try-error") stop()
    tmpR <- try(qmleR(yuima, start=as.list(coef(tmpR)), t = cp_prec$tau, lower=lower, upper = upper, method="L-BFGS-B", rcpp = TRUE))
    if(class(tmpR)=="try-error") stop()
    cp <- try(CPoint(yuima = yuima, param1=coef(tmpL), param2=coef(tmpR)))
    if(class(cp)=="try-error") stop()
    if (abs(cp$tau - cp_prec$tau)<delta) break
    else if (abs(cp$tau - cp_prec$tau)>=diff_prec) stop()
    else {
      cp_prec <- cp
      diff_prec <- abs(cp$tau - cp_prec$tau)
    }
  }
  
  i <- 1
  symb_id <- symb
  repeat {
    if(symb_id %in% names(yuimaGUIdata$cpYuima)){
      symb_id <- paste(symb, i)
      i <- i+1
    } else break
  }
  yuimaGUIdata$cpYuima[[symb_id]] <<- list(tau = index(series)[as.integer((cp$tau-t0)/delta)], info = info, series = series, qmleR = tmpR, qmleL = tmpL)
  
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



addSimulation <- function(modelYuima, symbName, info, toLog = FALSE, xinit, true.parameter, nsim, data = NA, saveTraj = TRUE, seed=NULL, sampling, space.discretized = FALSE, method = "euler", session, anchorId){
  if(!is.na(seed)) set.seed(seed)
  if(is.na(seed)) set.seed(NULL)
  if(saveTraj==TRUE){
    trajectory <- zoo::zoo(order.by = numeric())
    hist <- NA
  }
  if(saveTraj==FALSE){
    trajectory <- NA
    hist <- numeric(nsim)
  }
  if(toLog==TRUE) xinit <- log(xinit)
  is.valid <- TRUE
  model <- modelYuima@model
  if (info$class=="COGARCH") {
    noise <- cogarchNoise(yuima = modelYuima, param = true.parameter)
    xinit <- c(xinit, as.numeric(last(yuima:::onezoo(noise$Cogarch)))[-1])
    increments <- noise$incr.L
  }
  if (info$class=="CARMA") {
    increments <- CarmaNoise(yuima = modelYuima, param = true.parameter)
    x <- try(yuima::simulate(object = model, increment.W = t(increments), xinit = as.numeric(first(modelYuima@data@original.data)), true.parameter = true.parameter, sampling = setSampling(Initial = modelYuima@sampling@Initial, delta = modelYuima@sampling@delta, n = length(increments)), space.discretized = space.discretized, method = method))
    if (class(x)=="try-error"){
      createAlert(session = session, anchorId = anchorId, content = paste("Unable to simulate ", symbName," by ", info$model, ". Probably something wrong with the estimation of this model", sep = ""), style = "danger")
      return()
    }
    xinit <- c(xinit, as.numeric(last(yuima:::onezoo(x)))[-1])
  }
  if (info$class=="Fractional process") if (true.parameter[["hurst"]]>=1 | true.parameter[["hurst"]]<=0) {
    createAlert(session = session, anchorId = anchorId, content = "Hurst coefficient must greater than 0 and less than 1", style = "danger")
    return()
  }
  withProgress(message = 'Simulating: ', value = 0, {
    for (i in 1:nsim){
      incProgress(1/nsim, detail = paste("Simulating:",i,"(/",nsim,")"))
      if (info$class=="COGARCH")
        simulation <- try(yuima::simulate(object = model, increment.L = sample(x = increments, size = sampling@n, replace = TRUE), xinit = xinit, true.parameter = true.parameter, sampling = sampling, space.discretized = space.discretized, method = method))
      else if (info$class=="CARMA")
        simulation <- try(yuima::simulate(object = model, increment.W = t(sample(x = increments, size = sampling@n, replace = TRUE)), xinit = xinit, true.parameter = true.parameter, sampling = sampling, space.discretized = space.discretized, method = method))
      else if (info$class=="Fractional process")
        simulation <- try(yuima::simulate(object = model, xinit = xinit, true.parameter = true.parameter, hurst = true.parameter[["hurst"]], sampling = sampling, space.discretized = space.discretized, method = method))
      else
        simulation <- try(yuima::simulate(object = model, xinit = xinit, true.parameter = true.parameter, sampling = sampling, space.discretized = space.discretized, method = method))
      if (class(simulation)=="try-error"){
        is.valid <- FALSE
        break()
      }
      else if (any(is.na(as.numeric(simulation@data@zoo.data[[1]])) | !is.finite(as.numeric(simulation@data@zoo.data[[1]])) | (toLog==TRUE & !is.finite(exp(as.numeric(simulation@data@zoo.data[[1]])))))){
        is.valid <- FALSE
        break()
      }
      else {
        if (saveTraj==TRUE)
          trajectory <- merge(trajectory, simulation@data@zoo.data[[1]])
        if (saveTraj==FALSE)
          hist[i] <- as.numeric(tail(simulation@data@zoo.data[[1]],1))
      }
    }
  })
  if (!is.valid){
    if(info$class %in% c("CARMA","COGARCH")) msg <- paste("Unable to simulate ", symbName," by ", info$model, ". Probably something wrong with the estimation of this model", sep = "")
    else msg <- paste("Unable to simulate", symbName,"by", info$model)
    createAlert(session = session, anchorId = anchorId, content = msg, style = "danger")
    return()
  }

  if(saveTraj==TRUE){
    times <- index(trajectory)
    if(class(info$simulate.from)=="Date")
      index(trajectory) <- as.POSIXct(24*60*60*(times-times[1])/simulation@sampling@delta*as.numeric(info$simulate.to-info$simulate.from)/(simulation@sampling@n), origin = info$simulate.from)
    if(class(info$simulate.from)=="numeric")
      index(trajectory) <- as.numeric(times/simulation@sampling@delta*as.numeric(info$simulate.to-info$simulate.from)/(simulation@sampling@n))
    if(!is.null(colnames(trajectory)))
      colnames(trajectory) <- seq(1:length(colnames(trajectory)))
  }

  if(toLog==TRUE){
    trajectory <- exp(trajectory)
    hist <- exp(hist)
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


profit_distribution <- function(nOpt, nAss, type, strike, priceAtMaturity, optMarketPrice, assMarketPrice, percCostAss, minCostAss, lotCostOpt, lotMultiplier, shortCostPerYear, t0=Sys.Date(), maturity){
  if (nOpt==0 & nAss==0)
    return(0)
  if (type=="call"){
    payoff <- pmax(priceAtMaturity-strike,0)
    return(nOpt*(payoff-optMarketPrice)-
             nAss*(priceAtMaturity-assMarketPrice)-
             pmax(nAss*assMarketPrice*percCostAss, minCostAss)*ifelse(nAss!=0,1,0)-
             pmax(nAss*priceAtMaturity*percCostAss, minCostAss)*ifelse(nAss!=0,1,0)-
             nOpt/lotMultiplier*lotCostOpt-
             shortCostPerYear*(nAss*assMarketPrice)*as.numeric(as.Date(maturity)-as.Date(t0))/365
    )
  }
  if (type=="put"){
    payoff <- pmax(strike-priceAtMaturity,0)
    return(nOpt*(payoff-optMarketPrice)+
             nAss*(priceAtMaturity-assMarketPrice)-
             pmax(nAss*assMarketPrice*percCostAss, minCostAss)*ifelse(nAss!=0,1,0)-
             pmax(nAss*priceAtMaturity*percCostAss, minCostAss)*ifelse(nAss!=0,1,0)-
             nOpt/lotMultiplier*lotCostOpt
    )
  }
}




addHedging <- function(model, symbName, info = list(), xinit, true.parameter, nsim, sampling, session, anchorId){
  closeAlert(session, "addHedging_alert")
  hist <- vector()
  is.valid <- TRUE
  modObj <- model$model@model
  withProgress(message = 'Simulating: ', value = 0, {
    for (i in 1:nsim){
      incProgress(1/nsim, detail = paste("Simulating:",i,"(/",nsim,")"))
      simulation <- try(yuima::simulate(object = modObj, xinit = xinit, true.parameter = true.parameter, nsim = nsim, sampling = sampling))
      if (class(simulation)=="try-error"){
        is.valid <- FALSE
        break()
      }
      if(is.valid)
        hist <- c(hist, as.numeric(tail(simulation@data@zoo.data[[1]],1)))
    }
  })
  if (!is.valid){
    createAlert(session = session, anchorId = anchorId, alertId = "addHedging_alert" , content = paste("Unable to simulate", symbName,"by", info$model), style = "danger")
    return()
  }
  profits <- profit_distribution(nOpt=1*info$optLotMult, 
                                                nAss=0, 
                                                type=info$type, 
                                                strike=info$strike, 
                                                priceAtMaturity=hist, 
                                                optMarketPrice=info$optPrice, 
                                                assMarketPrice=info$assPrice, 
                                                percCostAss=info$assPercCost, 
                                                minCostAss=info$assMinCost, 
                                                lotCostOpt=info$optLotCost, 
                                                lotMultiplier=info$optLotMult, 
                                                shortCostPerYear=info$assRateShortSelling, 
                                                t0=info$estimate.to, 
                                                maturity=info$maturity)
  info$profit <- mean(profits)/(info$optLotMult*info$optPrice+info$optLotCost)
  info$stdErr <- sd(profits)/sqrt(length(profits))/(info$optLotMult*info$optPrice+info$optLotCost)
  info$nsim <- nsim
  info$buy <- ifelse(info$type=="call",NA,0)
  info$sell <- ifelse(info$type=="put",NA,0)
  info$LotsToBuy <- 1
  yuimaGUIdata$hedging[[length(yuimaGUIdata$hedging)+1]] <<- list(
    hist = hist,
    true.parameter = true.parameter,
    info = info,
    aic = model$aic,
    bic = model$bic,
    symb = symbName
  )
}



delHedging <- function(n){
  yuimaGUIdata$hedging <<- yuimaGUIdata$hedging[-n]
}




MYdist <- function(object, percentage = TRUE){
  l <- length(colnames(object))
  d <- matrix(ncol = l, nrow = l)
  f <- function(x, dens){
    res <- c()
    getY <- function(xi){
        i <- which(dens$x==xi)
        if (length(i)!=0) return(dens$y[i])
        else {
          i_x1 <- which.min(abs(dens$x-xi))
          i_x2 <- min(i_x1+1,length(dens$x))
          return(0.5*(dens$y[i_x1]+dens$y[i_x2]))
        }
    }
    res <- sapply(X = x, FUN = getY)
    return(res)
  }
  withProgress(message = 'Clustering: ', value = 0, {
    k <- 1
    for(i in 1:l){
      delta_i <- as.numeric(abs(mean(diff(index(object)[!is.na(object[,i])]), na.rm = TRUE)))
      if (percentage == TRUE) data_i <- as.vector(na.omit(Delt(object[,i])))
      else data_i <- as.vector(na.omit(diff(object[,i])))
      data_i <- data_i[data_i!="Inf"]
      dens1 <-  density(data_i/sqrt(delta_i)+mean(data_i, na.rm = TRUE)*(1/delta_i-1/sqrt(delta_i)), na.rm = TRUE)
      for(j in i:l)
        if (i!=j){
          incProgress(2/(l*(l-1)), detail = paste(k,"(/", l*(l-1)/2 ,")"))
          delta_j <- as.numeric(abs(mean(diff(index(object)[!is.na(object[,j])]), na.rm = TRUE)))
          if (percentage == TRUE) data_j <- as.vector(na.omit(Delt(object[,j])))
          else data_j <- as.vector(na.omit(diff(object[,j])))
          data_j <- data_j[data_j!="Inf"]
          dens2 <-  density(data_j/sqrt(delta_j)+mean(data_j, na.rm = TRUE)*(1/delta_j-1/sqrt(delta_j)), na.rm = TRUE)
          f_dist <- function(x) {0.5*abs(f(x,dens1)-f(x,dens2))}
          dist <- try(integrate(f_dist, lower = min(dens1$x[1],dens2$x[1]), upper = max(last(dens1$x), last(dens2$x)), subdivisions = 100000, rel.tol = 0.01))
          d[j,i] <- min(1, ifelse(class(dist)=="try-error", 1, dist$value))
          k <- k + 1
        }
    }
  })
  rownames(d) <- colnames(object)
  colnames(d) <- colnames(object)
  return(as.dist(d))
}



CPanalysis <- function(x, method = c("KSdiff", "KSperc"), pvalue = 0.01, symb){
  if (pvalue > 0.1){
    pvalue <- 0.1
    warning("pvalue re-defined: 0.1")
  }
  if(method=="KSdiff" | method=="KSperc"){
    x_incr <- switch (method,
                      "KSdiff" = na.omit(diff(x)),
                      "KSperc" =  na.omit(Delt(x)))
    index_x_incr <- index(x_incr)
    x_incr_num <- as.numeric(x_incr)
    tau <- NULL
    p.value <- NULL
    getCPoint <- function(n0, nTot){
      if(abs(nTot-n0)<10) return()
      grid <- seq(from = n0, to=(nTot-1), by = as.integer(1+(nTot-n0)/100))
      ks<-matrix(nrow = length(grid), ncol = 2, dimnames = list(NULL, c("index", "pvalue")))
      j <- 1
      for (i in grid){
        ks[j,"index"] <- i
        ks[j, "pvalue"]<- suppressWarnings(ks.test(x_incr_num[n0:i],x_incr_num[(i+1):nTot])$p.value)
        j <- j+1
      }
      if(min(ks[,"pvalue"], na.rm=TRUE) > pvalue) return()
      else {
        new_n0 <- as.integer(ks[which.min(ks[,"pvalue"]), "index"])
        env <- environment(getCPoint)
        assign(x = "tau", value = append(x = get("tau", envir = env), values = index_x_incr[new_n0]), envir = env)
        assign(x = "p.value", value = append(x = get("p.value", envir = env), values = as.numeric(ks[which(ks[,"index"]==new_n0), "pvalue"])), envir = env)
        getCPoint(n0 = n0, nTot = new_n0)
        getCPoint(n0 = new_n0+1, nTot = nTot)
      }
    }
    getCPoint(n0 = 1, nTot = length(x_incr_num))
    if (is.null(tau)){
      tau <- NA
      p.value <- NA
    }
    return (list(tau=tau,pvalue=p.value, method=method, series = x, symb = symb))
  }  
}

addCPoint_distribution <- function(symb, method = c("KSdiff", "KSperc"), pvalue = 0.01){
  temp <- try(CPanalysis(x=getData(symb), method = method, pvalue = pvalue, symb = symb))
  if (class(temp)!="try-error") {
    i <- 1
    symb_id <- symb
    repeat {
      if(symb_id %in% names(yuimaGUIdata$cp)){
        symb_id <- paste(symb, i)
        i <- i+1
      } else break
    }
    yuimaGUIdata$cp[[symb_id]] <<- temp
    return(list(error=NULL))
  } else return(list(error=symb))
}




