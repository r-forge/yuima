options(shiny.maxRequestSize = 100*1024^2)
options("getSymbols.warning4.0"=FALSE)

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })

  yuimaGUItable <- reactiveValues(series=data.frame(),  model=data.frame(), simulation=data.frame(), hedging=data.frame())
  yuimaGUIsettings <- list(simulation = list(), estimation = list(), delta = list(), toLog = list())
  
  
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
  
  melt <- function(x){
    V1 <- rep(rownames(x), ncol(x))
    V2 <- sort(V1)
    xx <- data.frame(Var1 = V1, Var2 = V2, value = NA)
    for (i in 1:nrow(xx)) xx[i,"value"] <- x[as.character(xx[i,"Var1"]), as.character(xx[i,"Var2"])]
    return(xx)
  }
  
  mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
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
          Symb = symb,
          Class = yuimaGUIdata$model[[symb]][[i]]$info$class,
          Model = yuimaGUIdata$model[[symb]][[i]]$info$modName,
          Jumps = yuimaGUIdata$model[[symb]][[i]]$info$jumps,
          From = as.character(start(yuimaGUIdata$model[[symb]][[i]]$model@data@original.data)),
          To = as.character(end(yuimaGUIdata$model[[symb]][[i]]$model@data@original.data)),
          AIC = yuimaGUIdata$model[[symb]][[i]]$aic,
          BIC = yuimaGUIdata$model[[symb]][[i]]$bic,
          stringsAsFactors = FALSE)
        rownames(newRow) <- as.character(paste(symb," ", i, sep=""))
        yuimaGUItable$model <<- rbind(yuimaGUItable$model, newRow)
      }
    }
  })
  
  observeEvent(yuimaGUIdata$simulation, priority = 10, {
    yuimaGUItable$simulation <<- data.frame()
    for (symb in names(yuimaGUIdata$simulation)){
      for (i in 1:length(yuimaGUIdata$simulation[[symb]])){
        estimated.from <- NA
        estimated.to <- NA
        if (!is.null(yuimaGUIdata$simulation[[symb]][[i]]$model$model@data@original.data)){
          estimated.from <- as.character(start(yuimaGUIdata$simulation[[symb]][[i]]$model$model@data@original.data))
          estimated.to <- as.character(end(yuimaGUIdata$simulation[[symb]][[i]]$model$model@data@original.data))
        }
        newRow <- data.frame(
          "Symb" = symb,
          "Class" = yuimaGUIdata$simulation[[symb]][[i]]$model$info$class,
          "Model" = yuimaGUIdata$simulation[[symb]][[i]]$model$info$modName,
          "Jumps" = yuimaGUIdata$simulation[[symb]][[i]]$model$info$jumps,
          "N sim" = yuimaGUIdata$simulation[[symb]][[i]]$info$nsim,
          "N step" = yuimaGUIdata$simulation[[symb]][[i]]$info$nstep,
          "delta" = yuimaGUIdata$simulation[[symb]][[i]]$info$delta,
          "Simulated from" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$simulate.from),
          "Simulated to" = as.character(yuimaGUIdata$simulation[[symb]][[i]]$info$simulate.to),
          "Estimated from" = estimated.from,
          "Estimated to" = estimated.to,
          check.names = FALSE, stringsAsFactors = FALSE)
        rownames(newRow) <- as.character(paste(symb," ", i, sep=""))
        yuimaGUItable$simulation <<- rbind(yuimaGUItable$simulation, newRow)
      }
    }
  })
  
  observeEvent(yuimaGUIdata$series, priority = 10, {
    n <- names(yuimaGUIdata$series)
    for (i in names(yuimaGUIsettings$estimation)) if(!(i %in% n)) yuimaGUIsettings$estimation[[i]] <<- NULL
    for (i in names(yuimaGUIsettings$delta)) if(!(i %in% n)) yuimaGUIsettings$delta[[i]] <<- NULL
  })
  
  observeEvent(yuimaGUIdata$hedging, priority = 10, {
    yuimaGUItable$hedging <<- data.frame()
    if (length(yuimaGUIdata$hedging)!=0){
      for (i in 1:length(yuimaGUIdata$hedging)){
        newRow <- data.frame(
          "Symb" = yuimaGUIdata$hedging[[i]]$symb,
          "Number of Simulations" = yuimaGUIdata$hedging[[i]]$info$nsim,
          "Average Return (%)" = round(yuimaGUIdata$hedging[[i]]$info$profit*100,2),
          "Option Lots_to_Buy" = yuimaGUIdata$hedging[[i]]$info$LotsToBuy,
          "Assets to Buy" = yuimaGUIdata$hedging[[i]]$info$buy,
          "Assets to Sell" = yuimaGUIdata$hedging[[i]]$info$sell,
          "Asset Price" = yuimaGUIdata$hedging[[i]]$info$assPrice,
          "Option Price" = yuimaGUIdata$hedging[[i]]$info$optPrice,
          "Option Type" = yuimaGUIdata$hedging[[i]]$info$type,
          "Strike" = yuimaGUIdata$hedging[[i]]$info$strike,
          "Maturity" = yuimaGUIdata$hedging[[i]]$info$maturity,
          "Lot Multiplier"=yuimaGUIdata$hedging[[i]]$info$optLotMult,
          "Trading_Cost per Lot"=yuimaGUIdata$hedging[[i]]$info$optLotCost,
          "Asset Trading_Cost (%)"=yuimaGUIdata$hedging[[i]]$info$assPercCost*100,
          "Asset Min Trading_Cost"=yuimaGUIdata$hedging[[i]]$info$assMinCost,
          "Asset Yearly_Short_Rate (%)"=yuimaGUIdata$hedging[[i]]$info$assRateShortSelling*100,
          "Model" = yuimaGUIdata$hedging[[i]]$model$info$modName,
          "Estimated from" = start(yuimaGUIdata$hedging[[i]]$model$model@data@original.data),
          "Estimated to" = end(yuimaGUIdata$hedging[[i]]$model$model@data@original.data),
          "AIC" = yuimaGUIdata$hedging[[i]]$model$aic,
          "BIC" = yuimaGUIdata$hedging[[i]]$model$bic,
          check.names = FALSE, stringsAsFactors = FALSE)
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
  
  
  addData <- function(x, typeIndex){
    x <- data.frame(x, check.names = TRUE)
    err <- c()
    alreadyIn <- c()
    for (symb in colnames(x)){
      if (symb %in% names(yuimaGUIdata$series))
        alreadyIn <- c(alreadyIn, symb)
      else{
        temp <- data.frame("Index" = rownames(x), "symb" = as.numeric(gsub(as.character(x[,symb]), pattern = ",", replacement = ".")))
        temp <- temp[complete.cases(temp), ]
        rownames(temp) <- temp[,"Index"]
        colnames(temp) <- c("Index", symb)
        if (all(is.na(temp[,2]))) err <- c(err, symb)
        else if (typeIndex=="numeric"){
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
    return(list(err = err, already_in = alreadyIn))
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
                      "COGARCH" = "Cogarch(p,q)",
                      "Levy process" = "Geometric Brownian Motion with Jumps"
  )
  
  defaultJumps <- c("Gaussian", "Uniform")
  
  defaultBounds <- function(name, delta, strict, jumps = NA, AR_C = NA, MA_C = NA, data, intensity = NULL, threshold = NULL){
    lastPrice = last(data)
    if (name %in% names(isolate({yuimaGUIdata$usr_model}))){
      par <- setModelByName(name = name, jumps = jumps,  AR_C = AR_C, MA_C = MA_C)@parameter@all
      if(strict==TRUE){
        lower <- rep(NA, length(par))
        upper <- rep(NA, length(par))
      } else {
        if (yuimaGUIdata$usr_model[[name]]$class=="Compound Poisson"){
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
      else { 
        x <- as.numeric(diff(data))
        mu <- mean(x)
        sigma <- sd(x)
        return (list(lower=list("sigma"=sigma/sqrt(delta), "mu"=mu/delta), upper=list("sigma"=sigma/sqrt(delta), "mu"=mu/delta)))
      }
    }
    if (name == "Geometric Brownian Motion" | name == "gBm") {
      if (strict==TRUE) return (list(lower=list("sigma"=0, "mu"=NA), upper=list("sigma"=NA, "mu"=NA)))
      else {
        x <- as.numeric(na.omit(Delt(data)))
        mu <- mean(x)
        sigma <- sd(x)
        return (list(lower=list("sigma"=sigma/sqrt(delta), "mu"=mu/delta), upper=list("sigma"=sigma/sqrt(delta), "mu"=mu/delta)))
      }
    }
    if (name == "Ornstein-Uhlenbeck (OU)" | name == "OU"){
      if (strict==TRUE) return(list(lower=list("theta"=0, "sigma"=0),upper=list("theta"=NA, "sigma"=NA)))
      else return(list(lower=list("theta"=0, "sigma"=0),upper=list("theta"=1/delta, "sigma"=1/sqrt(delta))))
    }
    if (name == "Vasicek model (VAS)" | name == "VAS"){
      if (strict==TRUE) return(list(lower=list("theta3"=0, "theta1"=NA, "theta2"=NA), upper=list("theta3"=NA, "theta1"=NA, "theta2"=NA)))
      else {
        mu <- abs(mean(as.numeric(data), na.rm = TRUE))
        return(list(lower=list("theta3"=0, "theta1"=-0.1*mu/delta, "theta2"=-0.1/delta), upper=list("theta3"=1/sqrt(delta), "theta1"=0.1*mu/delta, "theta2"=0.1/delta)))
      }
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
      else {
        x <- as.numeric(diff(data))
        counts <- length(x[x!=0 & !is.na(x)])
        lambda <- counts/(length(x)*delta)
        return(list(lower=c(list("lambda"=lambda), boundsJump$lower),upper=c(list("lambda"=lambda), boundsJump$upper)))
      }
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
    if (name == "Geometric Brownian Motion with Jumps"){
      boundsJump <- jumpBounds(jumps = jumps, strict = strict, data = data, threshold = threshold)
      boundsIntensity <- intensityBounds(intensity = intensity, strict = strict, delta = delta)
      if (strict==TRUE) return(list(lower=c(list("mu"=NA, "sigma"=0), boundsJump$lower, boundsIntensity$lower),upper=c(list("mu"=NA, "sigma"=NA), boundsJump$upper, boundsIntensity$upper)))
      else return(list(lower=c(list("mu"=-1, "sigma"=0), boundsJump$lower, boundsIntensity$lower),upper=c(list("mu"=1, "sigma"=1), boundsJump$upper, boundsIntensity$upper)))
    }
  }
  
  
  setThreshold <- function(class, data){
    if(class!="Levy process") return(NA)
    else {
      return(0)
    }
  }
  
  setJumps <- function(jumps){
    if(is.na(jumps)) return("")
    else switch (jumps,
                 "Gaussian" = list("dnorm(z, mean = mu_jump, sd = sigma_jump)"),
                 "Uniform" = list("dunif(z, min = a_jump, max = b_jump)")
    )
  }
  
  jumpBounds <- function(jumps, data, strict, threshold = 0){
    switch(jumps,
           "Gaussian" = {
             if(strict==TRUE) return(list(lower=list("mu_jump"=NA, "sigma_jump"=0), upper=list("mu_jump"=NA, "sigma_jump"=NA)))
             else {
               x <- na.omit(diff(data))
               x <- x[abs(x)>threshold]
               x <- x-sign(x)*threshold
               mu <- mean(x)
               s <- sd(x)
               return(list(lower=list("mu_jump"=mu, "sigma_jump"=s), upper=list("mu_jump"=mu, "sigma_jump"=s)))
             }
           },
           "Uniform" = {
             if(strict==TRUE) return(list(lower=list("a_jump"=NA, "b_jump"=NA), upper=list("a_jump"=NA, "b_jump"=NA)))
             else {
               x <- na.omit(diff(data))
               x <- x[abs(x)>threshold]
               x <- x-sign(x)*threshold
               a <- min(x)
               b <- max(x)
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
  
  intensityBounds <- function(intensity, strict, delta){
    switch(intensity,
           "lambda" = {
             if(strict==TRUE) return(list(lower=list("lambda"=0), upper=list("lambda"=NA)))
             else return(list(lower=list("lambda"=0), upper=list("lambda"=1/delta)))
           }
    )  
  }
  
  
  setModelByName <- function(name, jumps = NA, AR_C = NA, MA_C = NA, XinExpr = FALSE, intensity = NA){
    if (name %in% names(isolate({yuimaGUIdata$usr_model}))){
      if (isolate({yuimaGUIdata$usr_model[[name]]$class=="Diffusion process" | yuimaGUIdata$usr_model[[name]]$class=="Fractional process"}))
        return(isolate({yuimaGUIdata$usr_model[[name]]$object}))
      if (isolate({yuimaGUIdata$usr_model[[name]]$class=="Compound Poisson"}))
        return(setPoisson(intensity = isolate({yuimaGUIdata$usr_model[[name]]$intensity}), df = setJumps(jumps = jumps), solve.variable = "x"))
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
    if (name == "Geometric Brownian Motion with Jumps") {
      if(intensity=="None") return(yuima::setModel(drift="mu*x", diffusion="sigma*x", jump.coeff="x", measure.type = "code", measure = list(df = setJumps(jumps = jumps)), solve.variable = "x"))
      else return(yuima::setModel(drift="mu*x", diffusion="sigma*x", jump.coeff="x", measure.type = "CP", measure = list(intensity = intensity, df = setJumps(jumps = jumps)), solve.variable = "x"))
    }
  }
  
  printModelLatex <- function(names, process, jumps = NA){
    if (process=="Diffusion process"){
      mod <- ""
      for (name in names){
        if (name %in% names(isolate({yuimaGUIdata$usr_model}))){
          text <- toLatex(setModelByName(name))
          x <- paste(text[2:9], collapse = "")
          x <- substr(x,3,nchar(x))
          x <- gsub(x, pattern = "'", replacement = "")
          x <- gsub(x, pattern = "x", replacement = "X_t")
          x <- gsub(x, pattern = "W1", replacement = "W_t")
          x <- gsub(x, pattern = "\\$", replacement = "")
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
        if (name %in% names(isolate({yuimaGUIdata$usr_model}))){
          text <- toLatex(setModelByName(name))
          x <- paste(text[2:9], collapse = "")
          x <- substr(x,3,nchar(x))
          x <- gsub(x, pattern = "'", replacement = "")
          x <- gsub(x, pattern = "x", replacement = "X_t")
          x <- gsub(x, pattern = "W1", replacement = "W_t^H")
          x <- gsub(x, pattern = "\\$", replacement = "")
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
        if (name %in% names(isolate({yuimaGUIdata$usr_model}))){
          text <- paste("\\lambda(t)=",yuimaGUIdata$usr_model[[name]]$intensity)
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
    if (process=="Levy process"){
      return(paste("$$","dX_t = \\mu X_t \\; dt + \\sigma X_t \\; dW_t + X_t \\; dZ_t","$$"))
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
      capture.output(test <- try(Diagnostic.Cogarch(yuimaGUI$model, param = as.list(coef(yuimaGUI$qmle)))))
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
  
  addModel <- function(modName, intensity_levy, modClass, AR_C, MA_C, jumps, symbName, data, toLog, delta, start, startMin, startMax, trials, seed, method="BFGS", fixed = list(), lower, upper, joint=FALSE, aggregation=TRUE, threshold=NULL, session, anchorId, alertId){
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
    model <- setYuima(data = setDataGUI(data, delta = delta), model=setModelByName(name = modName, intensity = intensity_levy, jumps = jumps, MA_C = MA_C, AR_C = AR_C))
    index(model@data@original.data) <- index(data)
    parameters <- model@model@parameter
    if (modClass == "Fractional process"){
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
    else if (modClass == "Levy process") {
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
      method = method
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
        QMLEtempL <- try(qmleL(yuima = yuima, t = t0 + fracL*length(series)*delta, start = start, method=method, lower = lower, upper = upper, rcpp = TRUE))
        if (class(QMLEtempL)!="try-error") if (all(!is.na(summary(QMLEtempL)@coef[,"Estimate"])))
          break
      }
      if (class(QMLEtempL)!="try-error") if (all(!is.na(summary(QMLEtempL)@coef[,"Estimate"]))){
        repeat{
          m2logL <- summary(QMLEtempL)@m2logL
          coefTable <- summary(QMLEtempL)@coef
          for (param in names(start))
            start[[param]] <- as.numeric(coefTable[param,"Estimate"])
          QMLEtempL <- try(qmleL(yuima = yuima, t = t0 + fracL*length(series)*delta, start = start, method=method, lower = lower, upper = upper, rcpp = TRUE))
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
    tmpR <- try(qmleR(yuima = yuima, t = t0 + fracR*length(series)*delta, start = as.list(coef(tmpL)), method=method, lower = lower, upper = upper, rcpp = TRUE))
    
    if (class(tmpR)=="try-error") stop()
    
    cp_prec <- try(CPoint(yuima = yuima, param1=coef(tmpL), param2=coef(tmpR)))
    if(class(cp_prec)=="try-error") stop()
    diff_prec <- delta*nrow(series)
    repeat{
      tmpL <- try(qmleL(yuima, start=as.list(coef(tmpL)), t = cp_prec$tau, lower=lower, upper = upper, method=method, rcpp = TRUE))
      if(class(tmpL)=="try-error") stop()
      tmpR <- try(qmleR(yuima, start=as.list(coef(tmpR)), t = cp_prec$tau, lower=lower, upper = upper, method=method, rcpp = TRUE))
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
  
  
  simulateGUI <- function(symbName, modelYuimaGUI, xinit, nsim, nstep, simulate.from, simulate.to, saveTraj, space.discretized, method, session, anchorId, alertId = NULL, true.parameter = NULL){
    modelYuima <- modelYuimaGUI$model
    model <- modelYuima@model
    toLog <- ifelse(is.null(modelYuimaGUI$info$toLog), FALSE, modelYuimaGUI$info$toLog)
    if(simulate.from >= simulate.to){
      createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste("Unable to simulate ", symbName," by ", modelYuimaGUI$info$modName, ": ending time before starting time.", sep = ""), style = "danger")
      return()
    }
    if(toLog==TRUE) xinit <- log(xinit)
    if(saveTraj==TRUE){
      trajectory <- zoo::zoo(order.by = numeric())
      hist <- NA
    }
    if(saveTraj==FALSE){
      trajectory <- NA
      hist <- numeric(nsim)
    }
    if(is.null(true.parameter)){
      convert <- TRUE
      if (modelYuimaGUI$info$class=="Fractional process") true.parameter <- as.list(modelYuimaGUI$qmle["Estimate",])
      else true.parameter <- as.list(modelYuimaGUI$qmle@coef)
      data <- modelYuima@data@original.data
      data_index <- index(data)
      real_delta <- as.numeric(last(data_index)-data_index[1])/(length(data_index)-1)
      used_delta <- modelYuima@sampling@delta 
      if(is.numeric(data_index)){
        Initial <- round(digits = 0, simulate.from/real_delta)*used_delta
        Terminal <- round(digits = 0, simulate.to/real_delta)*used_delta
      } else {
        Initial <- round(digits = 0, as.numeric(simulate.from-start(data))/real_delta)*used_delta
        Terminal <- round(digits = 0, as.numeric(simulate.to-start(data))/real_delta)*used_delta
      }
      if (modelYuimaGUI$info$class %in% c("COGARCH", "CARMA") | is.na(nstep))
        nstep <- (Terminal-Initial)/used_delta
      sampling <- setSampling(Initial = Initial, Terminal = Terminal, n = nstep)
    } else {
      convert <- FALSE
      sampling <- setSampling(Initial = simulate.from, Terminal = simulate.to, n = nstep)
    }
    is.valid <- TRUE
    if (modelYuimaGUI$info$class=="COGARCH") {
      noise <- cogarchNoise(yuima = modelYuima, param = true.parameter)
      xinit <- c(xinit, as.numeric(last(yuima:::onezoo(noise$Cogarch)))[-1])
      increments <- noise$incr.L
    }
    if (modelYuimaGUI$info$class=="CARMA") {
      increments <- CarmaNoise(yuima = modelYuima, param = true.parameter)
      x <- try(yuima::simulate(object = model, increment.W = t(increments), xinit = as.numeric(first(modelYuima@data@original.data)), true.parameter = true.parameter, sampling = setSampling(Initial = modelYuima@sampling@Initial, delta = used_delta, n = length(increments)), space.discretized = space.discretized, method = method))
      if (class(x)=="try-error"){
        createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste("Unable to simulate ", symbName," by ", modelYuimaGUI$info$modName, ". Probably something wrong with the estimation of this model", sep = ""), style = "danger")
        return()
      }
      xinit <- c(xinit, as.numeric(last(yuima:::onezoo(x)))[-1])
    }
    if (modelYuimaGUI$info$class=="Fractional process") if (true.parameter[["hurst"]]>=1 | true.parameter[["hurst"]]<=0) {
      createAlert(session = session, anchorId = anchorId, alertId = alertId, content = "Hurst coefficient must greater than 0 and less than 1", style = "danger")
      return()
    }
    withProgress(message = 'Simulating: ', value = 0, {
      for (i in 1:nsim){
        incProgress(1/nsim, detail = paste("Simulating:",i,"(/",nsim,")"))
        if (modelYuimaGUI$info$class=="COGARCH")
          simulation <- try(yuima::simulate(object = model, increment.L = sample(x = increments, size = sampling@n, replace = TRUE), xinit = xinit, true.parameter = true.parameter, sampling = sampling, space.discretized = space.discretized, method = method))
        else if (modelYuimaGUI$info$class=="CARMA")
          simulation <- try(yuima::simulate(object = model, increment.W = t(sample(x = increments, size = sampling@n, replace = TRUE)), xinit = xinit, true.parameter = true.parameter, sampling = sampling, space.discretized = space.discretized, method = method))
        else if (modelYuimaGUI$info$class=="Fractional process")
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
      if(modelYuimaGUI$info$class %in% c("CARMA","COGARCH")) msg <- paste("Unable to simulate ", symbName," by ", modelYuimaGUI$info$modName, ". Probably something wrong with the estimation of this model", sep = "")
      else msg <- paste("Unable to simulate", symbName,"by", modelYuimaGUI$info$modName)
      createAlert(session = session, anchorId = anchorId, alertId = alertId, content = msg, style = "danger")
      return()
    }
    if(saveTraj==TRUE & convert==TRUE){
      times <- index(trajectory)
      if(is.numeric(data_index))
        index(trajectory) <- as.numeric(times/used_delta*real_delta)
      else
        index(trajectory) <- as.POSIXct(24*60*60*(times-times[1])/used_delta*real_delta, origin = simulate.from)
      if(!is.null(colnames(trajectory)))
        colnames(trajectory) <- seq(1:length(colnames(trajectory)))
    }
    if(toLog==TRUE){
      trajectory <- exp(trajectory)
      hist <- exp(hist)
    }
    return(list(hist=hist, trajectory=trajectory, nstep = sampling@n[1], simulate.from = simulate.from, simulate.to = simulate.to, delta = sampling@delta))
  }
  
  
  
  addSimulation <- function(modelYuimaGUI, symbName, xinit, nsim, nstep, simulate.from, simulate.to, saveTraj, seed, sampling, true.parameter = NULL, space.discretized = FALSE, method = "euler", session, anchorId){
    if(!is.na(seed)) set.seed(seed)
    if(is.na(seed)) set.seed(NULL)
    sim <- simulateGUI(symbName = symbName, modelYuimaGUI = modelYuimaGUI, xinit = xinit, nsim = nsim, nstep = nstep, simulate.from = simulate.from, simulate.to = simulate.to, saveTraj = saveTraj, space.discretized = space.discretized, method = method, session = session, anchorId = anchorId, true.parameter = true.parameter)
    if(!is.null(sim)){
      yuimaGUIdata$simulation[[symbName]][[ifelse(is.null(length(yuimaGUIdata$simulation[[symbName]])),1,length(yuimaGUIdata$simulation[[symbName]])+1)]] <<- list(
        model = modelYuimaGUI,
        trajectory = sim$trajectory,
        hist = sim$hist,
        info = list(nsim = nsim, nstep = sim$nstep, simulate.from = sim$simulate.from, simulate.to = sim$simulate.to, delta = sim$delta)
      )
    }
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
  
  
  
  
  addHedging <- function(modelYuimaGUI, symbName, info, xinit, nsim, nstep, simulate.from, simulate.to, session, anchorId){
    alertId <- "addHedging_alert"
    closeAlert(session, alertId)
    sim <- simulateGUI(symbName = symbName, modelYuimaGUI = modelYuimaGUI, xinit = xinit, simulate.from = simulate.from, simulate.to = simulate.to, nstep = nstep, nsim = nsim, saveTraj = FALSE, space.discretized = FALSE, method = "euler", session = session, anchorId = anchorId, alertId = alertId)
    if(!is.null(sim)){
      today <- simulate.from
      profits <- profit_distribution(nOpt=1*info$optLotMult, 
                                     nAss=0, 
                                     type=info$type, 
                                     strike=info$strike, 
                                     priceAtMaturity=sim$hist, 
                                     optMarketPrice=info$optPrice, 
                                     assMarketPrice=info$assPrice, 
                                     percCostAss=info$assPercCost, 
                                     minCostAss=info$assMinCost, 
                                     lotCostOpt=info$optLotCost, 
                                     lotMultiplier=info$optLotMult, 
                                     shortCostPerYear=info$assRateShortSelling, 
                                     t0=today, 
                                     maturity=info$maturity)
      info$profit <- mean(profits)/(info$optLotMult*info$optPrice+info$optLotCost)
      info$stdErr <- sd(profits)/sqrt(length(profits))/(info$optLotMult*info$optPrice+info$optLotCost)
      info$nsim <- nsim
      info$buy <- ifelse(info$type=="call",NA,0)
      info$sell <- ifelse(info$type=="put",NA,0)
      info$LotsToBuy <- 1
      info$today <- today
      yuimaGUIdata$hedging[[length(yuimaGUIdata$hedging)+1]] <<- list(
        model = modelYuimaGUI,
        hist = sim$hist,
        info = info,
        symb = symbName
      )
    }
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
        #delta_i <- as.numeric(abs(mean(diff(index(object)[!is.na(object[,i])]), na.rm = TRUE)))
        if (percentage == TRUE) data_i <- as.vector(na.omit(Delt(object[,i])))
        else data_i <- as.vector(na.omit(diff(object[,i])))
        data_i <- data_i[data_i!="Inf"]
        dens1 <-  density(data_i, na.rm = TRUE)#/sqrt(delta_i)+mean(data_i, na.rm = TRUE)*(1/delta_i-1/sqrt(delta_i)), na.rm = TRUE)
        for(j in i:l)
          if (i!=j){
            incProgress(2/(l*(l-1)), detail = paste(k,"(/", l*(l-1)/2 ,")"))
            #delta_j <- as.numeric(abs(mean(diff(index(object)[!is.na(object[,j])]), na.rm = TRUE)))
            if (percentage == TRUE) data_j <- as.vector(na.omit(Delt(object[,j])))
            else data_j <- as.vector(na.omit(diff(object[,j])))
            data_j <- data_j[data_j!="Inf"]
            dens2 <-  density(data_j, na.rm = TRUE)#/sqrt(delta_j)+mean(data_j, na.rm = TRUE)*(1/delta_j-1/sqrt(delta_j)), na.rm = TRUE)
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
  
  
  
  ###Save all available data
  saveData <- function() {
    dataDownload_series <- reactive({
      for (symb in names(yuimaGUIdata$series)){
        data <- getData(symb)
        if(class(index(data)[1])=="numeric") {
          if (!exists("data_num", inherits = FALSE)) data_num <- data
          else data_num <- merge(data_num, data)
        }
        else {
          if (!exists("data_date", inherits = FALSE)) data_date <- data
          else data_date <- merge(data_date, data)
        }
      }
      if (exists("data_date") & !exists("data_num")) return(as.data.frame(data_date[order(index(data_date)), ]))
      if (!exists("data_date") & exists("data_num")) return(as.data.frame(data_num[order(index(data_num)), ]))
      if (exists("data_date") & exists("data_num")) return(rbind.fill(as.data.frame(data_num[order(index(data_num)), ]), as.data.frame(data_date[order(index(data_date)), ])))
    })
    downloadHandler(
      filename = "yuimaGUIdata.txt",
      content = function(file) {
        write.table(dataDownload_series(), file, quote = FALSE)
      }
    )
  }
  
  jumps_shortcut <- function(class, jumps){
    switch(class, "Diffusion process" = NA, "Fractional process" = NA,"Compound Poisson" = jumps, "COGARCH"=NA, "CARMA" = NA, "Levy process" = jumps)
  }
  
  ### Home
  output$video_intro <- renderUI({
    HTML('<iframe width="90%" height="250px" src="https://www.youtube.com/embed/XX_bmCrI_gc?rel=0" frameborder="0" allowfullscreen></iframe>')
  })
  
  
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################

  
  output$saveSession <- {
    downloadHandler(
      filename = "session.yuimaGUI",
      content = function(file) {
        save("yuimaGUIdata", file = file)
      }
    )
  } 
  
  observeEvent(input$loadSession, {
    try(load(choose.files(caption = "Select a .yuimaGUI file", multi = FALSE)))
    yuimaGUIdata$series <<- yuimaGUIdata$series
    yuimaGUIdata$model <<- yuimaGUIdata$model
    yuimaGUIdata$usr_model <<- yuimaGUIdata$usr_model
    yuimaGUIdata$simulation <<- yuimaGUIdata$simulation
    yuimaGUIdata$usr_simulation <<- yuimaGUIdata$usr_simulation
    yuimaGUIdata$cp <<- yuimaGUIdata$cp
    yuimaGUIdata$cpYuima <<- yuimaGUIdata$cpYuima
    yuimaGUIdata$llag <<- yuimaGUIdata$llag
    yuimaGUIdata$cluster <<- yuimaGUIdata$cluster
  })
  
  ########################Load Economic and Financial Data
  ########################
  ########################


  ###Download data and display message
  observeEvent(input$finDataGo, priority = 1, {
    if (input$symb!=""){
      closeAlert(session, "finDataAlert_err")
      closeAlert(session, "finDataAlert_warn")
      closeAlert(session, "finDataAlert_succ")
      symb <- unlist(strsplit(input$symb, split = "[, ]+" , fixed = FALSE))
      err <- c()
      already_in <- c()
      withProgress(message = 'Loading: ', value = 0, {
        for (i in symb){
          incProgress(1/length(symb), detail = i)
          x <- try(getSymbols(i, src = input$sources ,auto.assign = FALSE, from = input$dR[1], to = input$dR[2]))
          if (class(x)[1]=="try-error")
            err <- cbind(err,i)
          else {
            info <- addData(x, typeIndex = "%Y-%m-%d")
            err <- c(err, info$err)
            already_in <- c(already_in, info$already_in)
          }
        }
      })
      if(!is.null(err))
        createAlert(session = session, anchorId = "finDataAlert", alertId = "finDataAlert_err", content = paste("Unable to load following symbols:", paste(err,collapse = " ")), style = "error")
      if(!is.null(already_in))
        createAlert(session = session, anchorId = "finDataAlert", alertId = "finDataAlert_warn", content = paste("WARNING! Following symbols already loaded:", paste(already_in,collapse = " ")), style = "warning")
      if(is.null(err) & is.null(already_in))
        createAlert(session = session, anchorId = "finDataAlert", alertId = "finDataAlert_succ", content = paste("All symbols loaded successfully"), style = "success")
    }
  })

  ###Display available data
  output$database1 <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)!=0)
      return(yuimaGUItable$series)
  })

  ###Interactive range of finDataPlot chart
  range_finDataPlot <- reactiveValues(x=NULL, y=NULL)
  observe({
    if (!is.null(input$finDataPlot_brush)){
      range_finDataPlot$x <- c(as.Date(input$finDataPlot_brush$xmin), as.Date(input$finDataPlot_brush$xmax))
      range_finDataPlot$y <- c(input$finDataPlot_brush$ymin, input$finDataPlot_brush$ymax)
    }
  })
  observeEvent(input$finDataPlot_dbclick,{
    range_finDataPlot$x <- c(NULL, NULL)
    range_finDataPlot$y <- c(NULL, NULL)
  })

  ###Display chart of last clicked symbol
  observeEvent(input$database1_rows_selected, priority = -1, {
    symb <- yuimaGUItable$series$Symb[tail(input$database1_rows_selected,1)]
    shinyjs::show("finDataPlot")
    shinyjs::show("scale_finDataPlot")
    valid_data <- NULL
    range_finDataPlot$x <- c(NULL, NULL)
    output$finDataPlot <- renderPlot({
      if (length(yuimaGUItable$series)==0){
        shinyjs::hide("finDataPlot")
        shinyjs::hide("scale_finDataPlot")
      }
      else{
        if (!(symb %in% as.character(yuimaGUItable$series[,"Symb"]))){
          shinyjs::hide("finDataPlot")
          shinyjs::hide("scale_finDataPlot")
        }
        else {
          data <- window(getData(symb), start = range_finDataPlot$x[1], end = range_finDataPlot$x[2])
          if(is.null(valid_data) | length(index(data))>3) valid_data <<- data
          par(bg="black")
          plot.zoo(valid_data, main=symb, log=ifelse(input$scale_finDataPlot=="Linear","","y"), xlab="Index", ylab=NA, col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
          grid(col="grey")
        }
      }
    })
  })

  ###Delete Button
  observeEvent(input$finDataDelete, priority = 1,{
      delData(yuimaGUItable$series$Symb[input$database1_rows_selected])
  })

  ###DeleteAll Button
  observeEvent(input$finDataDeleteAll, priority = 1,{
    delData(yuimaGUItable$series$Symb[input$database1_rows_all])
  })

  ###Save Button
  output$finDataSave <- {
    saveData()
  }




  ########################Load Your Data
  ########################
  ########################

  
  
  ###Read file
  fileUp_O <- reactive({
    if (!is.null(input$yourFile$datapath)){
      sep <- input$yourFileSep
      if(input$yourFileSep=="default") sep <- ""
      skip <- input$yourFileLine-1
      if(is.na(skip)) skip <- 0
      dec <- input$yourFileDec
      if(input$yourFileDec=="") dec <- "."
      if(input$yourFileHeader=="Only rows")
        z <- read.csv(input$yourFile$datapath ,sep = sep, header = FALSE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
      if(input$yourFileHeader=="Only columns"){
        z <- read.csv(input$yourFile$datapath, sep = sep, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
        z <- data.frame(t(z), row.names = 1, check.names = FALSE)
        z <- data.frame(t(z), check.names = FALSE)
      }
      if (input$yourFileHeader=="Both")
        z <- read.csv(input$yourFile$datapath, sep = sep, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
      if (input$yourFileHeader=="None")
        z <- read.csv(input$yourFile$datapath, sep = sep, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
      if (input$yourFileHeader=="Default")
        z <- read.csv(input$yourFile$datapath, sep = sep, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
      if (input$yourFileHeader=="Only rows" | identical(colnames(z),paste("V",seq(1,length(colnames(z))),sep="")))
        colnames(z) <- paste("X",seq(1,length(colnames(z))),"_",make.names(input$yourFile$name),sep="")
      dec <- isolate({ifelse(input$yourFileDec=="", ".", input$yourFileDec)})
      if(dec==".") dec <- "\\."
      thnd <- input$yourFileThnd
      if(thnd==".") thnd <- "\\."
      zz <- data.frame(row.names = rownames(z), x = apply(z, 2, function(x) gsub(pattern =  dec, replacement =  ".", x = gsub(pattern =  thnd, replacement =  "", x = as.character(x)))))
      colnames(zz) <- colnames(z)
      return(zz)
    }
  })

  ###Display Index choices: columns of file or transposed file
  output$yourFileIndex <- renderUI({
    temp <- try(colnames(fileUp_O()))
    if (input$yourFileSwitch==TRUE){
      temp <- try(rownames(fileUp_O()))
      if(class(temp)!="try-error")      
        if (input$yourFileHeader=="Only columns" | identical(temp,paste("V",seq(1,length(temp)),sep="")))
          temp <- paste("X",seq(1,length(temp)),"_",make.names(input$yourFile$name),sep="")
    }
    if (class(temp)=="try-error")
      return(selectInput("yourFileIndex",label = "Index", choices = c("Row Headers"="default","Numeric"="numeric"), selected = "default"))
    if (class(temp)!="try-error")
      return(selectInput("yourFileIndex",label = "Index", choices = c("Row Headers"="default","Numeric"="numeric",temp), selected = "default"))
  })


  ###File to upload
  fileUp <- reactive({
    if (!is.null(input$yourFile$datapath)){
      z <- fileUp_O()
      if (input$yourFileSwitch==TRUE) {
        z <- as.data.frame(t(z), check.names = FALSE)
        if (identical(colnames(z), as.character(seq(1,length(colnames(z))))))
          colnames(z) <- paste("X",seq(1,length(colnames(z))),"_",make.names(input$yourFile$name),sep="")
      }
      ###Display choices for Index Type and set to "numeric" if Index is "numeric"
      output$yourFileFUN <- renderUI({
        if (!is.null(input$yourFileIndex)){
          sel <- "%Y-%m-%d"
          if (input$yourFileIndex=="numeric" | !all(is.na(as.numeric(as.character(rownames(z))))) )
            sel <- "numeric"
          selectInput("yourFileFUN", label = "Index Format", choices = c("Numeric"="numeric", "Year-Month-Day    (yyyy-mm-dd)"="%Y-%m-%d", "Month-Day-Year    (mm-dd-yyyy)"="%m-%d-%Y", "Month-Day-Year    (mm-dd-yy)"="%m-%d-%y", "Day-Month-Year    (dd-mm-yyyy)"="%d-%m-%Y", "Day-Month-Year    (dd-mm-yy)"="%d-%m-%y", "Year/Month/Day    (yyyy/mm/dd)"="%Y/%m/%d", "Month/Day/Year    (mm/dd/yyyy)"="%m/%d/%Y", "Month/Day/Year    (mm/dd/yy)"="%m/%d/%y", "Day/Month/Year    (dd/mm/yyyy)"="%d/%m/%Y", "Day/Month/Year    (dd/mm/yy)"="%d/%m/%y"), selected = sel)
        }
      })
      if(input$yourFileIndex!="default" & input$yourFileIndex!="numeric")
        z <- data.frame(z, row.names = which(colnames(z)==input$yourFileIndex), check.names = FALSE)
      if(input$yourFileIndex=="numeric")
        z <- data.frame(z, row.names = seq(1,length(rownames(z))), check.names = FALSE)
      return (z)
    }
  })

  ###Display Upload Button
  output$yourFileButton <- renderUI ({
    if (!is.null(input$yourFile$datapath))
      return(tags$button(type="button", id="yourFileGo", class = "action-button", em("Load data")))
  })
  
  observe({
    shinyjs::toggle("yourFileButton", condition = "try-error"!=(class(try(fileUp()))))
  })
  
  ###Display text "Preview"
  output$yourFilePreviewText <- renderText ({
    if (!is.null(input$yourFile$datapath))
      return("Preview")
  })

  ###Display Preview of file to upload
  output$yourFilePreview <- DT::renderDataTable(options=list(scrollX=TRUE, scrollY = 250, scrollCollapse = FALSE, deferRender = TRUE, dom = 'frtiS'), extensions = 'Scroller', selection = "none", rownames = TRUE, {
    if (!is.null(input$yourFile$datapath))
      return (fileUp())
  })

  ###Upload file
  observeEvent(input$yourFileGo, priority = 1, {
    closeAlert(session, "yourDataAlert_err")
    closeAlert(session, "yourDataAlert_warn")
    closeAlert(session, "yourDataAlert_succ")
    info <- addData(fileUp(), typeIndex = input$yourFileFUN)
    if(!is.null(info$err))
      createAlert(session = session, anchorId = "yourDataAlert", alertId = "yourDataAlert_err", content = paste("Unable to load following symbols:", paste(info$err,collapse = " ")), style = "error")
    if(!is.null(info$already_in))
      createAlert(session = session, anchorId = "yourDataAlert", alertId = "yourDataAlert_warn", content = paste("WARNING! Following symbols already loaded:", paste(info$already_in,collapse = " ")), style = "warning")
    if(is.null(info$err) & is.null(info$already_in))
      createAlert(session = session, anchorId = "yourDataAlert", alertId = "yourDataAlert_succ", content = paste("All symbols loaded successfully"), style = "success")
  })

  ###Display data available
  output$database2 <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)!=0)
      return (yuimaGUItable$series)
  })

  ###Delete Button
  observeEvent(input$yourFileDelete, priority = 1,{
    delData(yuimaGUItable$series$Symb[input$database2_rows_selected])
  })

  ###DeleteAll Button
  observeEvent(input$yourFileDeleteAll, priority = 1,{
    delData(yuimaGUItable$series$Symb[input$database2_rows_all])
  })

  ###Save Button
  output$yourFileSave <- {
    saveData()
  }

  observe({
    shinyjs::toggle("buttons_DataIO_file", condition = length(yuimaGUIdata$series)!=0)
    shinyjs::toggle("buttons_DataIO_fin", condition = length(yuimaGUIdata$series)!=0)
  })
  
  ########################Univariate Models
  ########################
  ########################

  ###Model Input depending on Class Input
  output$model <- renderUI({
    choices <- as.vector(defaultModels[names(defaultModels)==input$modelClass])
    if(input$modelClass!="Fractional process")
      for(i in names(yuimaGUIdata$usr_model))
        if (yuimaGUIdata$usr_model[[i]]$class==input$modelClass) {
          if(input$modelClass!="Diffusion process") choices <- c(i, choices)
          else if (length(setModelByName(name = i)@parameter@all)!=0) choices <- c(i, choices)
        }
    return (selectInput("model",label = "Model Name", choices = choices, multiple = TRUE))
  })
  
  output$jumps <- renderUI({
    if (input$modelClass=="Compound Poisson")
      return(selectInput("jumps",label = "Jumps", choices = defaultJumps))
    if (input$modelClass=="Levy process"){
      jump_choices <- defaultJumps
      jump_sel <- NULL
      if(!is.null(input$model)){
        if(input$model=="Geometric Brownian Motion with Jumps") jump_sel <- "Gaussian"
      }
      return(div(
        column(6,selectInput("model_levy_intensity", label = "Intensity", choices = c(#"None",
                                                                             "Constant"="lambda"))),
        column(6,selectInput("jumps",label = "Jumps", choices = jump_choices, selected = jump_sel)))
      )
    }
    
  })
  
  output$pq_C <- renderUI({
    if (input$modelClass=="CARMA")
      return(div(
        column(6,numericInput("AR_C",label = "AR degree (p)", value = 2, min = 1, step = 1)),
        column(6,numericInput("MA_C",label = "MA degree (q)", value = 1, min = 1, step = 1))
      ))
    if (input$modelClass=="COGARCH")
      return(div(
        column(6,numericInput("AR_C",label = "AR degree (p)", value = 1, min = 1, step = 1)),
        column(6,numericInput("MA_C",label = "MA degree (q)", value = 1, min = 1, step = 1))
      ))
  })

  ###Print last selected model in Latex
  output$PrintModelLatex <- renderUI({
    shinyjs::hide("titlePrintModelLatex")
    if (!is.null(input$model)){
      shinyjs::show("titlePrintModelLatex")
      class <- isolate({input$modelClass})
      return(withMathJax(printModelLatex(names = input$model, process = class, jumps = jumps_shortcut(class = class, jumps = input$jumps))))
    }
  })

  output$usr_modelClass_latex <- renderUI({
    if (input$usr_modelClass=="Diffusion process")
      return(withMathJax("$$dX=a(t,X,\\theta)\\;dt\\;+\\;b(t,X,\\theta)\\;dW$$"))
    if (input$usr_modelClass=="Fractional process")
      return(withMathJax("$$dX=a(t,X,\\theta)\\;dt\\;+\\;b(t,X,\\theta)\\;dW^H$$"))
    if (input$usr_modelClass=="Compound Poisson")
      return(withMathJax("$$X_t = X_0+\\sum_{i=0}^{N_t} Y_i \\; : \\;\\;\\;  N_t \\sim Poi\\Bigl(\\int_0^t \\lambda(t)dt\\Bigl)$$"))
    if (input$usr_modelClass=="Levy process")
      return(withMathJax("$$dX_t = \\mu X_t \\; dt + \\sigma X_t \\; dW_t + X_t \\; dZ_t$$"))
  })
  
  observe({
    if (input$usr_modelClass=="Fractional process") createAlert(session = session, anchorId = "panel_set_model_alert", alertId = "alert_fracinfo", style = "info", content = "Fractional process you set here will be available for simulation purposes, but not for estimation.")
    else closeAlert(session = session, alertId = "alert_fracinfo")
  })

  output$usr_model_coeff <- renderUI({
    if (input$usr_modelClass=="Diffusion process")
      return(
        div(align="center", 
          column(6, textInput("usr_model_coeff_drift", width = "70%", label = withMathJax("$$a(t,X,\\theta)$$"))),
          column(6, textInput("usr_model_coeff_diff", width = "70%", label = withMathJax("$$b(t,X,\\theta)$$")))
        )
      )
    if (input$usr_modelClass=="Fractional process")
      return(
        div(align="center", 
            column(6, textInput("usr_model_coeff_drift", width = "70%", label = withMathJax("$$a(t,X,\\theta)$$"))),
            column(6, textInput("usr_model_coeff_diff", width = "70%", label = withMathJax("$$b(t,X,\\theta)$$")))
        )
      )
    if (input$usr_modelClass=="Compound Poisson")
      return(
        div(align="center",
           textInput("usr_model_coeff_intensity", width = "45%", label = withMathJax("$$\\lambda(t)$$"))
        )
      )
    if (input$usr_modelClass=="Levy process")
      return(
        div(align="center",
            fluidRow(column(12,textInput("usr_model_coeff_intensity", width = "45%", label = withMathJax("$$\\lambda(t)$$")))),
            fluidRow(
              column(6, textInput("usr_model_coeff_drift", width = "70%", label = withMathJax("$$a(t,X,\\theta)$$"))),
              column(6, textInput("usr_model_coeff_diff", width = "70%", label = withMathJax("$$b(t,X,\\theta)$$")))
            )
        )
      )
  })

  observeEvent(input$usr_model_button_save, {
    entered <- FALSE
    switch(input$usr_modelClass,
           "Diffusion process" = {
             if (input$usr_model_name!="" & (input$usr_model_coeff_drift!="" | input$usr_model_coeff_diff!="")){
               mod <- try(setModel(drift = tolower(input$usr_model_coeff_drift), diffusion = tolower(input$usr_model_coeff_diff), solve.variable = "x"))
               if(class(mod)!="try-error") yuimaGUIdata$usr_model[[input$usr_model_name]] <<- list(object=mod, class=input$usr_modelClass)
               entered <- TRUE
             }
           },
           "Fractional process" = {
             if (input$usr_model_name!="" & (input$usr_model_coeff_drift!="" | input$usr_model_coeff_diff!="")){
               mod <- try(setModel(drift = tolower(input$usr_model_coeff_drift), diffusion = tolower(input$usr_model_coeff_diff), hurst = NA, solve.variable = "x"))
               if(class(mod)!="try-error") yuimaGUIdata$usr_model[[input$usr_model_name]] <<- list(object=mod, class=input$usr_modelClass)
               entered <- TRUE
             }
           },
           "Compound Poisson" = {
             if (input$usr_model_name!="" & (input$usr_model_coeff_intensity!="")){
               mod <- try(setPoisson(intensity = tolower(input$usr_model_coeff_intensity), df = "", solve.variable = "x"))
               if(class(mod)!="try-error") yuimaGUIdata$usr_model[[input$usr_model_name]] <<- list(intensity=tolower(input$usr_model_coeff_intensity), class=input$usr_modelClass)
               entered <- TRUE
             }
           },
           "Levy process" = {
             if (input$usr_model_name!=""){
               mod <- try(setModel(drift=input$usr_model_coeff_drift, diffusion=input$usr_model_coeff_diff, measure.type = ifelse(is.na(input$usr_model_coeff_intensity), "code", "CP"), measure = list(intensity = input$usr_model_coeff_intensity, df = ""), solve.variable = "x"))
               if(class(mod)!="try-error") yuimaGUIdata$usr_model[[input$usr_model_name]] <<- list(intensity=tolower(input$usr_model_coeff_intensity), drift = input$usr_model_coeff_drift, diffusion = input$usr_model_coeff_diff, class=input$usr_modelClass)
               entered <- TRUE
             }
           } 
          )
    if (entered){
      yuimaGUIsettings$estimation[[input$usr_model_name]] <<- list()
      closeAlert(session, "alert_savingModels")
      if(class(mod)!="try-error") createAlert(session = session, anchorId = "panel_set_model_alert", alertId = "alert_savingModels", style = "success", content = "Model saved successfully")
      else createAlert(session = session, anchorId = "panel_set_model_alert", alertId = "alert_savingModels", style = "error", content = "Model is not correctly specified")
    }
  })

  observe({
    for(mod in names(yuimaGUIsettings$estimation))
      if (!(mod %in% c(names(yuimaGUIdata$usr_model), names(defaultModels))))
        yuimaGUIsettings$estimation[[mod]] <<- list()
  })
  
  output$usr_model_saved <- renderUI({
    if (length(names(yuimaGUIdata$usr_model))!=0)
      selectInput("usr_model_saved", label = "Saved Models", choices = names(yuimaGUIdata$usr_model), selected = tail(names(yuimaGUIdata$usr_model),1))
  })

  output$usr_model_saved_latex <- renderUI({
    input$usr_model_button_save
    if (!is.null(input$usr_model_saved)) if (input$usr_model_saved %in% names(yuimaGUIdata$usr_model))
      withMathJax(printModelLatex(input$usr_model_saved, process = yuimaGUIdata$usr_model[[input$usr_model_saved]]$class))
  })

  observeEvent(input$usr_model_button_delete, {
    for (i in input$usr_model_saved)
      yuimaGUIdata$usr_model[i] <<- NULL
  })


  ###Display available data
  output$database3 <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (yuimaGUItable$series)
  })

  ###Table of selected data to model
  seriesToEstimate <- reactiveValues(table=data.frame())

  ###Select Button
  observeEvent(input$buttonSelect_models_Univariate, priority = 1, {
    seriesToEstimate$table <<- rbind(seriesToEstimate$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% rownames(yuimaGUItable$series)[input$database3_rows_selected]) & !(rownames(yuimaGUItable$series) %in% rownames(seriesToEstimate$table)),])
  })

  ###SelectAll Button
  observeEvent(input$buttonSelectAll_models_Univariate, priority = 1, {
    seriesToEstimate$table <<- rbind(seriesToEstimate$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% rownames(yuimaGUItable$series)[input$database3_rows_all]) & !(rownames(yuimaGUItable$series) %in% rownames(seriesToEstimate$table)),])
  })

  ###Display Selected Data
  output$database4 <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = FALSE, selection = "multiple",{
    if (nrow(seriesToEstimate$table)==0){
      NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (seriesToEstimate$table)
  })


  ###Control selected data to be in yuimaGUIdata$series
  observe({
    if(length(seriesToEstimate$table)!=0){
        if (length(yuimaGUItable$series)==0)
          seriesToEstimate$table <<- data.frame()
        else
          seriesToEstimate$table <<- seriesToEstimate$table[which(as.character(seriesToEstimate$table[,"Symb"]) %in% as.character(yuimaGUItable$series[,"Symb"])),]
    }
  })

  ###Delete Button
  observeEvent(input$buttonDelete_models_Univariate, priority = 1,{
    if (!is.null(input$database4_rows_selected))
      seriesToEstimate$table <<- seriesToEstimate$table[-input$database4_rows_selected,]
  })

  ###DeleteAll Button
  observeEvent(input$buttonDeleteAll_models_Univariate, priority = 1,{
    if (!is.null(input$database4_rows_all))
      seriesToEstimate$table <<- seriesToEstimate$table[-input$database4_rows_all,]
  })

  ###Interactive range of selectRange chart
  range_selectRange <- reactiveValues(x=NULL, y=NULL)
  observe({
    if (!is.null(input$selectRange_brush) & !is.null(input$plotsRangeSeries)){
      data <- getData(input$plotsRangeSeries)
      test <- (length(index(window(data, start = input$selectRange_brush$xmin, end = input$selectRange_brush$xmax))) > 3)
      if (test==TRUE){
        range_selectRange$x <- c(as.Date(input$selectRange_brush$xmin), as.Date(input$selectRange_brush$xmax))
        range_selectRange$y <- c(input$selectRange_brush$ymin, input$selectRange_brush$ymax)
      }
    }
  })


  observe({
    shinyjs::toggle(id="plotsRangeErrorMessage", condition = nrow(seriesToEstimate$table)==0)
    shinyjs::toggle(id="plotsRangeAll", condition = nrow(seriesToEstimate$table)!=0)
  })

  ###Display charts: series and its increments
  observe({
    symb <- input$plotsRangeSeries
    if(!is.null(symb))
      if (symb %in% rownames(yuimaGUItable$series)){
        data <- getData(symb)
        incr <- na.omit(Delt(data, type = "arithmetic"))
        condition <- all(is.finite(incr))
        shinyjs::toggle("selectRangeReturns", condition = condition)
        range_selectRange$x <- NULL
        range_selectRange$y <- NULL
        start <- as.character(seriesToEstimate$table[input$plotsRangeSeries,"From"])
        end <- as.character(seriesToEstimate$table[input$plotsRangeSeries,"To"])
        if(class(index(data))=="numeric"){
          start <- as.numeric(start)
          end <- as.numeric(end)
        }
        output$selectRange <- renderPlot({
          if ((symb %in% rownames(yuimaGUItable$series) & (symb %in% rownames(seriesToEstimate$table)))){
            par(bg="black")
            plot.zoo(window(data, start = range_selectRange$x[1], end = range_selectRange$x[2]), main=symb, xlab="Index", ylab=NA, log=switch(input$scale_selectRange,"Linear"="","Logarithmic (Y)"="y", "Logarithmic (X)"="x", "Logarithmic (XY)"="xy"), col="grey", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
            lines(window(data, start = start, end = end), col = "green")
            grid(col="grey")
          }
        })
        output$selectRangeReturns <- renderPlot({
          if (symb %in% rownames(yuimaGUItable$series) & (symb %in% rownames(seriesToEstimate$table)) & condition){
            par(bg="black")
            plot.zoo( window(incr, start = range_selectRange$x[1], end = range_selectRange$x[2]), main=paste(symb, " - Percentage Increments"), xlab="Index", ylab=NA, log=switch(input$scale_selectRange,"Linear"="","Logarithmic (Y)"="", "Logarithmic (X)"="x", "Logarithmic (XY)"="x"), col="grey", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
            lines(window(incr, start = start,  end = end), col = "green")
            grid(col="grey")
          }
        })
      }
  })


  output$plotsRangeSeries <- renderUI({
    selectInput("plotsRangeSeries", label = "Series", choices = rownames(seriesToEstimate$table), selected = input$plotsRangeSeries)
  })

  ###Choose Range input set to "Select range from charts" if charts have been brushed
  output$chooseRange <- renderUI({
    sel <- "full"
    if (!is.null(range_selectRange$x)) sel <- "selected"
    selectInput("chooseRange", label = "Range", choices = c("Full Range" = "full", "Select Range from Charts" = "selected", "Specify Range" = "specify"), selected = sel)
  })
  
  output$chooseRange_specify <- renderUI({
    if(!is.null(input$plotsRangeSeries)) {
      data <- getData(input$plotsRangeSeries)
    if(class(index(data))=="numeric") 
      return(div(
        column(6,numericInput("chooseRange_specify_t0", label = "From", min = start(data), max = end(data), value = start(data))),
        column(6,numericInput("chooseRange_specify_t1", label = "To", min = start(data), max = end(data), value = end(data)))
      ))
    if(class(index(data))=="Date")
      return(dateRangeInput("chooseRange_specify_date", start = start(data), end = end(data), label = "Specify Range"))
    }
  })

  
  observe({
    shinyjs::toggle(id = "chooseRange_specify", condition = (input$chooseRange)=="specify")
  })

  ###Function to update data range to use to estimate models
  updateRange_seriesToEstimate <- function(symb, range = c("full","selected","specify"), type = c("Date", "numeric")){
    for (i in symb){
      data <- getData(i)
      if (range == "full"){
        levels(seriesToEstimate$table[,"From"]) <- c(levels(seriesToEstimate$table[,"From"]), as.character(start(data)))
        levels(seriesToEstimate$table[,"To"]) <- c(levels(seriesToEstimate$table[,"To"]), as.character(end(data)))
        seriesToEstimate$table[i,"From"] <<- as.character(start(data))
        seriesToEstimate$table[i,"To"] <<- as.character(end(data))
      }
      if (range == "selected"){
        if(!is.null(range_selectRange$x) & class(index(data))==type){
          start <- range_selectRange$x[1]
          end <- range_selectRange$x[2]
          if(class(index(data))=="numeric"){
            start <- as.numeric(start)
            end <- as.numeric(end)
          }
          start <- max(start(data),start)
          end <- min(end(data), end)
          levels(seriesToEstimate$table[,"From"]) <- c(levels(seriesToEstimate$table[,"From"]), as.character(start))
          levels(seriesToEstimate$table[,"To"]) <- c(levels(seriesToEstimate$table[,"To"]), as.character(end))
          seriesToEstimate$table[i,"From"] <<- as.character(start)
          seriesToEstimate$table[i,"To"] <<- as.character(end)
        }
      }
      if (range == "specify"){
        if(class(index(data))==type){
          if(class(index(data))=="Date"){
            start <- input$chooseRange_specify_date[1]
            end <- input$chooseRange_specify_date[2]
          }
          if(class(index(data))=="numeric"){
            start <- input$chooseRange_specify_t0
            end <- input$chooseRange_specify_t1
          }
          start <- max(start(data),start)
          end <- min(end(data), end)
          levels(seriesToEstimate$table[,"From"]) <- c(levels(seriesToEstimate$table[,"From"]), as.character(start))
          levels(seriesToEstimate$table[,"To"]) <- c(levels(seriesToEstimate$table[,"To"]), as.character(end))
          seriesToEstimate$table[i,"From"] <<- as.character(start)
          seriesToEstimate$table[i,"To"] <<- as.character(end)
        }
      }
    }
  }

  ###Apply selected range by double click
  observeEvent(input$selectRange_dbclick, priority = 1, {
    updateRange_seriesToEstimate(input$plotsRangeSeries, range = "selected", type = class(index(getData(input$plotsRangeSeries))))
  })

  ###Apply selected range
  observeEvent(input$buttonApplyRange, priority = 1, {
    updateRange_seriesToEstimate(input$plotsRangeSeries, range = input$chooseRange, type = class(index(getData(input$plotsRangeSeries))))
  })

  ###ApplyAll selected range
  observeEvent(input$buttonApplyAllRange, priority = 1, {
    updateRange_seriesToEstimate(rownames(seriesToEstimate$table), range = input$chooseRange, type = class(index(getData(input$plotsRangeSeries))))
  })

  
  prev_buttonDelta <- 0
  prev_buttonAllDelta <- 0
  observe({
    class <- isolate({input$modelClass})
    for (symb in rownames(seriesToEstimate$table)){
      if (is.null(yuimaGUIsettings$delta[[symb]])) {
        i <- index(getData(symb))
        if(is.numeric(i)) yuimaGUIsettings$delta[[symb]] <<- mode(diff(i))
        else yuimaGUIsettings$delta[[symb]] <<- 0.01
      }
      if (is.null(yuimaGUIsettings$toLog[[symb]])) yuimaGUIsettings$toLog[[symb]] <<- FALSE
      data <- na.omit(as.numeric(getData(symb)))
      if (yuimaGUIsettings$toLog[[symb]]==TRUE) data <- log(data)
      for (modName in input$model){
        if (class(try(setModelByName(modName, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = class, jumps = input$jumps), AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA))))!="try-error"){
          if (is.null(yuimaGUIsettings$estimation[[modName]]))
            yuimaGUIsettings$estimation[[modName]] <<- list()
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]]))
            yuimaGUIsettings$estimation[[modName]][[symb]] <<- list()
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]] <<- list()
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["start"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["start"]] <<- list()
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]] <<- setThreshold(class = class, data = data)
          
          startMinMax <- defaultBounds(name = modName, 
                                       jumps = jumps_shortcut(class = class, jumps = input$jumps), 
                                       intensity = input$model_levy_intensity,
                                       threshold = yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]],
                                       AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), 
                                       MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA), 
                                       strict = FALSE,
                                       data = data,
                                       delta = yuimaGUIsettings$delta[[symb]])
          upperLower <- defaultBounds(name = modName, 
                                      jumps = jumps_shortcut(class = class, jumps = input$jumps), 
                                      intensity = input$model_levy_intensity,
                                      threshold = yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]],
                                      AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), 
                                      MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA),
                                      strict = TRUE,
                                      data = data,
                                      delta = yuimaGUIsettings$delta[[symb]])
          
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]] <<- startMinMax$lower
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]] <<- startMinMax$upper
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]] <<- upperLower$upper
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]] <<- upperLower$lower
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["method"]])){
            if(class=="COGARCH" | class=="CARMA") yuimaGUIsettings$estimation[[modName]][[symb]][["method"]] <<- "SANN"
            else yuimaGUIsettings$estimation[[modName]][[symb]][["method"]] <<- "L-BFGS-B"
          }
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]] <<- 1
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]] <<- NA
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]] <<- FALSE
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]] <<- TRUE
        }
      }
    }
    prev_buttonDelta <<- input$advancedSettingsButtonApplyDelta
    prev_buttonAllDelta <<- input$advancedSettingsButtonApplyAllDelta
  })
  
  observe({
    valid <- TRUE
    if (nrow(seriesToEstimate$table)==0 | is.null(input$model)) valid <- FALSE
    else for(mod in input$model) if  (class(try(setModelByName(mod, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps), AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))))=="try-error")  valid <- FALSE
    shinyjs::toggle(id="advancedSettingsAll", condition = valid)
    shinyjs::toggle(id="advancedSettingsErrorMessage", condition = !valid)
  })
  output$advancedSettingsSeries <- renderUI({
    if (nrow(seriesToEstimate$table)!=0)
      selectInput(inputId = "advancedSettingsSeries", label = "Series", choices = rownames(seriesToEstimate$table))
  })
  output$advancedSettingsDelta <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      return (numericInput("advancedSettingsDelta", label = paste("delta", input$advancedSettingsSeries), value = yuimaGUIsettings$delta[[input$advancedSettingsSeries]]))
  })
  output$advancedSettingsToLog <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries)){
      choices <- FALSE
      if (all(getData(input$advancedSettingsSeries)>0)) choices <- c(FALSE, TRUE)
      return (selectInput("advancedSettingsToLog", label = "Convert to log", choices = choices, selected = yuimaGUIsettings$toLog[[input$advancedSettingsSeries]]))
    }
  })
  output$advancedSettingsModel <- renderUI({
    if(!is.null(input$model))
      selectInput(inputId = "advancedSettingsModel", label = "Model", choices = input$model)
  })
  output$advancedSettingsParameter <- renderUI({
    if (!is.null(input$model))
      if (!is.null(input$advancedSettingsModel)){
        parL <- setModelByName(input$advancedSettingsModel, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps), AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))@parameter
        par <- parL@all
        if (input$modelClass=="COGARCH") par <- unique(c(parL@drift, parL@xinit))
        if (input$modelClass=="CARMA") par <- parL@drift
        selectInput(inputId = "advancedSettingsParameter", label = "Parameter", choices = par)
      }
  })
  #REMOVE# output$advancedSettingsFixed <- renderUI({
  #REMOVE#  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
  #REMOVE#    numericInput(inputId = "advancedSettingsFixed", label = "fixed", value = ifelse(is.null(yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]]),NA,yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]]))
  #REMOVE#})
  output$advancedSettingsStart <- renderUI({
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      #REMOVE# if (is.na(input$advancedSettingsFixed))
        numericInput(inputId = "advancedSettingsStart", label = "start", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["start"]][[input$advancedSettingsParameter]])
  })
  output$advancedSettingsStartMin <- renderUI({
    input$advancedSettingsButtonApplyDelta
    input$advancedSettingsButtonApplyAllDelta
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsStart) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      if (#REMOVE# is.na(input$advancedSettingsFixed) & 
        is.na(input$advancedSettingsStart))
        numericInput(inputId = "advancedSettingsStartMin", label = "start: Min", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMin"]][[input$advancedSettingsParameter]])
  })
  output$advancedSettingsStartMax <- renderUI({
    input$advancedSettingsButtonApplyDelta
    input$advancedSettingsButtonApplyAllDelta
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsStart) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      if (#REMOVE# is.na(input$advancedSettingsFixed) & 
        is.na(input$advancedSettingsStart))
        numericInput(inputId = "advancedSettingsStartMax", label = "start: Max", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMax"]][[input$advancedSettingsParameter]])
  })
  output$advancedSettingsLower <- renderUI({
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      #REMOVE# if (is.na(input$advancedSettingsFixed))
        if (input$advancedSettingsMethod=="L-BFGS-B" | input$advancedSettingsMethod=="Brent")
          numericInput("advancedSettingsLower", label = "lower", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["lower"]][[input$advancedSettingsParameter]])
  })
  output$advancedSettingsUpper <- renderUI({
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      #REMOVE# if (is.na(input$advancedSettingsFixed))
        if (input$advancedSettingsMethod=="L-BFGS-B" | input$advancedSettingsMethod=="Brent")
          numericInput("advancedSettingsUpper", label = "upper", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["upper"]][[input$advancedSettingsParameter]])
  })
  #REMOVE# output$advancedSettingsJoint <- renderUI({
  #REMOVE#   if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
  #REMOVE#     selectInput("advancedSettingsJoint", label = "joint", choices = c(FALSE, TRUE), selected = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["joint"]])
  #REMOVE# })
  output$advancedSettingsMethod <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      selectInput("advancedSettingsMethod", label = "method", choices = c("L-BFGS-B", "Nelder-Mead", "BFGS", "CG", "SANN", "Brent"), selected = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]])
  })
  #REMOVE# output$advancedSettingsAggregation <- renderUI({
  #REMOVE#   if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
  #REMOVE#     selectInput("advancedSettingsAggregation", label = "aggregation", choices = c(TRUE, FALSE), selected = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["aggregation"]])
  #REMOVE# })
  output$advancedSettingsThreshold <- renderUI({
     if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries)) if(isolate({input$modelClass})=="Levy process")
       numericInput("advancedSettingsThreshold", label = "threshold", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["threshold"]])
  })
  output$advancedSettingsTrials <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsMethod))
      numericInput("advancedSettingsTrials", label = "trials", min = 1, value = ifelse(input$advancedSettingsMethod=="SANN" & yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]]!="SANN",1,yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["trials"]]))
  })
  output$advancedSettingsSeed <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      numericInput("advancedSettingsSeed", label = "seed", min = 1, value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["seed"]])
  })



  observeEvent(input$advancedSettingsButtonApplyDelta, {
      yuimaGUIsettings$delta[[input$advancedSettingsSeries]] <<- input$advancedSettingsDelta
      yuimaGUIsettings$toLog[[input$advancedSettingsSeries]] <<- input$advancedSettingsToLog
  })
  observeEvent(input$advancedSettingsButtonApplyAllDelta, {
    for (symb in rownames(seriesToEstimate$table)){
      yuimaGUIsettings$delta[[symb]] <<- input$advancedSettingsDelta
      if (input$advancedSettingsToLog==FALSE) yuimaGUIsettings$toLog[[symb]] <<- input$advancedSettingsToLog
      else if (all(getData(symb)>0)) yuimaGUIsettings$toLog[[symb]] <<- input$advancedSettingsToLog
    }
  })
  observeEvent(input$advancedSettingsButtonApplyModel,{
    #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsFixed
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["start"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStart
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMin"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMin
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMax"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMax
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["lower"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsLower
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["upper"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsUpper
  })
  observeEvent(input$advancedSettingsButtonApplyAllModel,{
    for (symb in rownames(seriesToEstimate$table)){
      #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["fixed"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsFixed
      yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["start"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStart
      yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["startMin"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMin
      yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["startMax"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMax
      yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["lower"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsLower
      yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["upper"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsUpper
    }
  })
  observeEvent(input$advancedSettingsButtonApplyGeneral,{
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]] <<- input$advancedSettingsMethod
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["trials"]] <<- input$advancedSettingsTrials
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["seed"]] <<- input$advancedSettingsSeed
    #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["joint"]] <<- input$advancedSettingsJoint
    #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["aggregation"]] <<- input$advancedSettingsAggregation
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["threshold"]] <<- input$advancedSettingsThreshold
  })
  observeEvent(input$advancedSettingsButtonApplyAllModelGeneral,{
    for (symb in rownames(seriesToEstimate$table)){
      yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["method"]] <<- input$advancedSettingsMethod
      yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["trials"]] <<- input$advancedSettingsTrials
      yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["seed"]] <<- input$advancedSettingsSeed
      #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["joint"]] <<- input$advancedSettingsJoint
      #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["aggregation"]] <<- input$advancedSettingsAggregation
      yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["threshold"]] <<- input$advancedSettingsThreshold
    }
  })
  observeEvent(input$advancedSettingsButtonApplyAllGeneral,{
    for (mod in input$model){
      for (symb in rownames(seriesToEstimate$table)){
        yuimaGUIsettings$estimation[[mod]][[symb]][["method"]] <<- input$advancedSettingsMethod
        yuimaGUIsettings$estimation[[mod]][[symb]][["trials"]] <<- input$advancedSettingsTrials
        yuimaGUIsettings$estimation[[mod]][[symb]][["seed"]] <<- input$advancedSettingsSeed
        #REMOVE# yuimaGUIsettings$estimation[[mod]][[symb]][["joint"]] <<- input$advancedSettingsJoint
        #REMOVE# yuimaGUIsettings$estimation[[mod]][[symb]][["aggregation"]] <<- input$advancedSettingsAggregation
        yuimaGUIsettings$estimation[[mod]][[symb]][["threshold"]] <<- input$advancedSettingsThreshold
      }
    }
  })

  observe({
    closeAlert(session = session, alertId = "CARMA_COGARCH_err")
    if(!is.null(input$modelClass)) if(input$modelClass=="CARMA" ) if(!is.null(input$AR_C)) if(!is.null(input$MA_C)) if(!is.na(input$AR_C) & !is.na(input$MA_C)) {
      if(input$AR_C<=input$MA_C)
        createAlert(session = session, anchorId = "panel_run_estimation_alert", alertId = "CARMA_COGARCH_err", style = "error", content = "AR degree (p) must be greater than MA degree (q)")
      if(input$AR_C== 0 | input$MA_C==0)
        createAlert(session = session, anchorId = "panel_run_estimation_alert", alertId = "CARMA_COGARCH_err", style = "error", content = "AR and MA degree (p,q) must be positive")
    }
    if(!is.null(input$modelClass)) if(input$modelClass=="COGARCH" ) if(!is.null(input$AR_C)) if(!is.null(input$MA_C)) if(!is.na(input$AR_C) & !is.na(input$MA_C)) {
      if(input$AR_C<input$MA_C)
        createAlert(session = session, anchorId = "panel_run_estimation_alert", alertId = "CARMA_COGARCH_err", style = "error", content = "AR degree (p) must be greater than or equal to MA degree (q)")
      if(input$AR_C== 0 | input$MA_C==0)
        createAlert(session = session, anchorId = "panel_run_estimation_alert", alertId = "CARMA_COGARCH_err", style = "error", content = "AR and MA degree (p,q) must be positive")
    }  
  })


  ###Estimate models
  observeEvent(input$EstimateModels,{
    closeAlert(session = session, alertId = "modelsErr")
    valid <- TRUE
    if(is.null(input$model) | nrow(seriesToEstimate$table)==0) valid <- FALSE
    else if (input$modelClass=="Compound Poisson" & is.null(input$jumps)) valid <- FALSE
    else for(mod in input$model) if (class(try(setModelByName(mod, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps), AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))))=="try-error")  valid <- FALSE
    if(!valid){
      createAlert(session = session, anchorId = "panel_run_estimation_alert", alertId = "modelsAlert_err", content = "Select some series and (valid) models to estimate", style = "warning")
    }
    if(valid){
      withProgress(message = 'Estimating: ',{
        for (modName in input$model){
          for (i in rownames(seriesToEstimate$table)){
            symb <- as.character(seriesToEstimate$table[i,"Symb"])
            incProgress(1/(length(input$model)*nrow(seriesToEstimate$table)), detail = paste(symb,"-",modName))
            data <- getData(symb)
            start <- as.character(seriesToEstimate$table[i,"From"])
            end <- as.character(seriesToEstimate$table[i,"To"])
            times <- index(data)
            if (class(times)=="numeric")
              data <- data[(times >= as.numeric(start)) & (times <= as.numeric(end)), , drop = FALSE]
            else
              data <- data[(times >= start) & (times <= end), , drop = FALSE]
            addModel(
              modName = modName,
              modClass = input$modelClass,
              intensity_levy = input$model_levy_intensity,
              AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), 
              MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA),
              jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps),
              symbName = symb,
              data = data,
              delta = yuimaGUIsettings$delta[[symb]],
              toLog = yuimaGUIsettings$toLog[[symb]],
              start = yuimaGUIsettings$estimation[[modName]][[symb]][["start"]],
              startMin = yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]],
              startMax = yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]],
              method=yuimaGUIsettings$estimation[[modName]][[symb]][["method"]],
              trials=yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]],
              seed = yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]],
              fixed = yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]],
              lower = yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]],
              upper = yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]],
              joint = yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]],
              aggregation = yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]],
              threshold = yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]],
              session = session,
              anchorId = "panel_estimates_alert",
              alertId = NULL
            )
          }
        }
      })
      updateTabsetPanel(session = session,  inputId = "panel_estimates", selected = "Estimates")
    }
  })

  observe({
    valid <- TRUE
    if(is.null(input$model) | nrow(seriesToEstimate$table)==0) valid <- FALSE
    else if (input$modelClass=="Compound Poisson" & is.null(input$jumps)) valid <- FALSE
    if(valid) closeAlert(session, alertId = "modelsAlert_err")
  })

  observe({
    if("Symb" %in% colnames(seriesToEstimate$table))
      seriesToEstimate$table[,"Symb"] <<- as.character(seriesToEstimate$table[,"Symb"])
    if("Symb" %in% colnames(yuimaGUItable$series))
      yuimaGUItable$series[,"Symb"] <<- as.character(yuimaGUItable$series[,"Symb"])
    if("Symb" %in% colnames(yuimaGUItable$model))
      yuimaGUItable$model[,"Symb"] <<- as.character(yuimaGUItable$model[,"Symb"])
    if("AIC" %in% colnames(yuimaGUItable$model))
      yuimaGUItable$model[,"AIC"] <<- as.numeric(as.character(yuimaGUItable$model[,"AIC"]))
    if("BIC" %in% colnames(yuimaGUItable$model))
      yuimaGUItable$model[,"BIC"] <<- as.numeric(as.character(yuimaGUItable$model[,"BIC"]))
  })

  ###Display estimated models
  output$databaseModels <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
    if (length(yuimaGUItable$model)==0){
      NoData <- data.frame("Symb"=NA,"Here will be stored models you estimate in the previous tabs"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (yuimaGUItable$model)
  })

  rowToPrint <- reactiveValues(id = NULL)
  observe(priority = 1, {
    rowToPrint$id <<- NULL
    n <- nrow(yuimaGUItable$model)
    if (n > 0) {
      rowToPrint$id <<- n
      if (!is.null(input$databaseModels_row_last_clicked)) rowToPrint$id <- min(n, input$databaseModels_row_last_clicked)
    }
  })

  ###Print estimated model in Latex
  output$estimatedModelsLatex <- renderUI({
    if (!is.null(rowToPrint$id))
      withMathJax(printModelLatex(as.character(yuimaGUItable$model[rowToPrint$id, "Model"]), process = as.character(yuimaGUItable$model[rowToPrint$id, "Class"]), jumps = as.character(yuimaGUItable$model[rowToPrint$id, "Jumps"])))
  })

  ###Print Symbol
  output$SymbolName <- renderText({
    if (!is.null(rowToPrint$id))
      unlist(strsplit(rownames(yuimaGUItable$model)[rowToPrint$id], split = " "))[1]

  })

  ###More Info
  output$text_MoreInfo <- renderUI({
    id <- unlist(strsplit(rownames(yuimaGUItable$model)[rowToPrint$id], split = " "))
    info <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info
    div(
      h3(id[1], " - " , info$modName, class = "hModal"),
      h4(
        em("delta:"), info$delta, br(),
        em("series to log:"), info$toLog, br(),
        em("method:"), info$method, br(),
        em("threshold:"), info$threshold, br(),
        em("trials:"), info$trials, br(),
        em("seed:"), info$seed, br(),
        #REMOVE# em("joint:"), info$joint, br(),
        #REMOVE# em("aggregation:"), info$aggregation, br(),
        #REMOVE# em("threshold:"), info$threshold
        class = "hModal"
      ),
      align="center"
    )
  })

  output$table_MoreInfo <- renderTable(digits=5, rownames = TRUE, {
    id <- unlist(strsplit(rownames(yuimaGUItable$model)[rowToPrint$id], split = " "))
    info <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info
    if (info$class=="Fractional process") coef <- as.data.frame(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$qmle)
    else coef <- as.data.frame(t(summary(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$qmle)@coef))
    params <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@model@parameter@all
    lower <- data.frame(info$lower)
    upper <- data.frame(info$upper)
    fixed <- data.frame(info$fixed)
    start <- data.frame(info$start)
    startMin <- data.frame(info$startMin)
    startMax <- data.frame(info$startMax)
    if(length(lower)==0) lower[1,params[1]] <- NA
    if(length(upper)==0) upper[1,params[1]] <- NA
    #if(length(fixed)==0) fixed[1,params[1]] <- NA
    if(length(start)==0) start[1,params[1]] <- NA
    if(length(startMin)==0) startMin[1,params[1]] <- NA
    if(length(startMax)==0) startMax[1,params[1]] <- NA
    table <- rbind.fill(coef[,unique(colnames(coef))], #fixed, 
                        start, startMin, startMax, lower, upper)
    rownames(table) <- c("Estimate", "Std. Error", #"fixed", 
                         "start", "startMin", "startMax", "lower", "upper")
    return(t(table))
  })


  ###Print estimates
  observe({
    if (!is.null(rowToPrint$id)){
      symb <- unlist(strsplit(rownames(yuimaGUItable$model)[rowToPrint$id], split = " "))[1]
      modN <- as.numeric(unlist(strsplit(rownames(yuimaGUItable$model)[rowToPrint$id], split = " "))[2])
      if (yuimaGUIdata$model[[symb]][[modN]]$info$class=="Fractional process") table <- yuimaGUIdata$model[[symb]][[modN]]$qmle
      else table <- t(summary(yuimaGUIdata$model[[symb]][[modN]]$qmle)@coef)
      outputTable <- changeBase(table = table, yuimaGUI = yuimaGUIdata$model[[symb]][[modN]], newBase = input$baseModels, session = session, choicesUI="baseModels", anchorId = "panel_estimates_alert", alertId = "modelsAlert_conversion")
      output$estimatedModelsTable <- renderTable(rownames = TRUE, {
        if (!is.null(rowToPrint$id))
          return(outputTable)
      })
    }
  })

  observe({
    shinyjs::toggle("estimates_info", condition = !is.null(input$databaseModels_rows_all))
  })

  observe({
    shinyjs::toggle("usr_model_saved_div", condition = length(names(yuimaGUIdata$usr_model))!=0)
  })
  
  observe({
    test <- FALSE
    choices <- NULL
    if(length(names(yuimaGUIdata$model))!=0) for (i in names(yuimaGUIdata$model)) for (j in 1:length(yuimaGUIdata$model[[i]])) 
      if(yuimaGUIdata$model[[i]][[j]]$info$class %in% c("Diffusion process", "Compound Poisson", "Levy process", "COGARCH")){
        test <- TRUE
        choices <- c(choices, paste(i,j))
      }
    shinyjs::toggle(id = "model_modal_fitting_body", condition = test)
    shinyjs::toggle(id = "databaseModels_button_showResults", condition = test)
    output$model_modal_model_id <- renderUI({
      if (test==TRUE){
        selectInput("model_modal_model_id", label = "Model ID", choices = choices)
      }
    })
  })
  
  observe({
    if(!is.null(input$model_modal_model_id)) {
      id <- unlist(strsplit(input$model_modal_model_id, split = " " , fixed = FALSE))
      type <- isolate({yuimaGUIdata$model})[[id[1]]][[as.numeric(id[2])]]$info$class
      shinyjs::toggle(id = "model_modal_plot_intensity", condition = type %in% c("Compound Poisson", "Levy process"))
      shinyjs::toggle(id = "model_modal_plot_variance", condition = type %in% c("COGARCH"))
      shinyjs::toggle(id = "model_modal_plot_distr", condition = type %in% c("Diffusion process","Compound Poisson", "Levy process"))
      shinyjs::toggle(id = "model_modal_plot_test", condition = type %in% c("Diffusion process","Compound Poisson", "Levy process"))
    }
  })
  
  observeEvent(input$model_modal_model_id,{
    if(!is.null(input$model_modal_model_id)){
      id <- unlist(strsplit(input$model_modal_model_id, split = " " , fixed = FALSE))
      isolated_yuimaGUIdataModel <- isolate({yuimaGUIdata$model}) 
      if(id[1] %in% names(isolated_yuimaGUIdataModel)) if (length(isolated_yuimaGUIdataModel[[id[1]]])>=as.integer(id[2])){
        y <- isolated_yuimaGUIdataModel[[id[1]]][[as.numeric(id[2])]]
        
        if (y$info$class=="Diffusion process"){
          
          delta <- y$model@sampling@delta
          t <- y$model@sampling@grid[[1]][-length(y$model@sampling@grid[[1]])]
          x <- as.numeric(y$model@data@zoo.data[[1]])
          dx <- diff(x)
          x <- x[-length(x)]
          for (i in names(y$qmle@coef)) assign(i, value = as.numeric(y$qmle@coef[i]))
          z <- (dx-eval(y$model@model@drift)*delta)/(eval(y$model@model@diffusion[[1]])*sqrt(delta))
          z <- data.frame("V1" = z)
          output$model_modal_plot_distr <- renderPlot({
            return(
              ggplot(z, aes(x = V1)) + 
                theme(
                  plot.title = element_text(size=14, face= "bold", hjust = 0.5),
                  axis.title=element_text(size=12),
                  legend.position="none"
                ) +
                stat_function(fun = dnorm, args = list(mean = 0, sd = 1), fill = "blue",color = "blue", geom = 'area', alpha = 0.5) +
                geom_density(alpha = 0.5, fill = "green", color = "green") +
                xlim(-4, 4) + 
                labs(fill="", title = "Empirical VS Theoretical Distribution", x = "Standardized Increments", y = "Density")
            )
          })
          ksTest <- try(ks.test(x = as.numeric(z$V1), "pnorm"))
          output$model_modal_plot_test <- renderUI({
            if(class(ksTest)!="try-error")
              HTML(paste("<div><h5 class='hModal'>Kolmogorov-Smirnov p-value (the two distributions coincide): ", format(ksTest$p.value, scientific=T, digits = 2), "</h5></div>"))
          })
        }
        
        else if (y$info$class=="COGARCH"){
          
          dx <- diff(y$model@data@original.data[,1])
          v <- sqrt(cogarchNoise(y$model, param = as.list(coef(y$qmle)))$Cogarch@original.data[,"v"])
          v <- v/mean(v)*sd(dx)
          z <- data.frame("dx" = dx, "vplus" = v[-1], "vminus" = -v[-1], "time" = index(dx))
          output$model_modal_plot_variance <- renderPlot({
            return(
              ggplot(z, aes(x = time)) + 
                geom_line(aes(y = dx), size = 1, color = "black") +
                geom_line(aes(y = vplus), size = 1, color = "green") +
                geom_line(aes(y = vminus), size = 1, color = "green") +
                scale_color_manual(values=c("black", "green", "green")) +
                theme(
                  plot.title = element_text(size=14, face= "bold", hjust = 0.5),
                  axis.title=element_text(size=12),
                  legend.position="none"
                ) +
                labs(fill="", title = "Empirical VS Estimated Volatility", x = "", y = "Increments")
            )
          })
        }
        
        else if (y$info$class=="Compound Poisson" | y$info$class=="Levy process"){
          if (is.null(y$info$threshold)) threshold <- 0
          else threshold <- ifelse(is.na(y$info$threshold), 0, y$info$threshold)          
          x <- as.numeric(y$model@data@zoo.data[[1]])
          dx <- diff(x)
          dx <- dx[abs(dx)>threshold]
          #dx <- dx-sign(dx)*threshold
          for (i in names(y$qmle@coef)) assign(i, value = as.numeric(y$qmle@coef[i]))
          dx <- data.frame("V1" = dx)
          if(y$info$jumps=="Gaussian"){
            output$model_modal_plot_distr <- renderPlot({
              return(
                ggplot(dx, aes(x = V1)) + 
                  theme(
                    plot.title = element_text(size=14, face= "bold", hjust = 0.5),
                    axis.title=element_text(size=12),
                    legend.position="none"
                  ) +
                  stat_function(fun = dnorm, args = list(mean = mu_jump, sd = sigma_jump), fill = "blue",color = "blue", geom = 'area', alpha = 0.5) +
                  geom_density(alpha = 0.5, fill = "green", color = "green") +
                  xlim(-4, 4) + 
                  labs(fill="", title = "Empirical VS Estimated Distribution", x = "Increments", y = "Density")
              )
            })
            ksTest <- try(ks.test(x = as.numeric(dx$V1), "pnorm", mean = mu_jump, sd = sigma_jump))
            output$model_modal_plot_test <- renderUI({
              if(class(ksTest)!="try-error")
                HTML(paste("<div><h5 class='hModal'>Kolmogorov-Smirnov p-value (the two distributions coincide): ", format(ksTest$p.value, scientific=T, digits = 2), "</h5></div>"))
            })
          }
          if(y$info$jumps=="Uniform"){
            output$model_modal_plot_distr <- renderPlot({
              return(
                ggplot(dx, aes(x = V1)) + 
                  theme(
                    plot.title = element_text(size=14, face= "bold", hjust = 0.5),
                    axis.title=element_text(size=12),
                    legend.position="none"
                  ) +
                  stat_function(fun = dunif, args = list(min = a_jump, max = b_jump), fill = "blue",color = "blue", geom = 'area', alpha = 0.5) +
                  geom_density(alpha = 0.5, fill = "green", color = "green") +
                  xlim(min(dx$V1),max(dx$V1)) + 
                  labs(fill="", title = "Empirical VS Estimated Distribution", x = "Increments", y = "Density")
              )
            })
            ksTest <- try(ks.test(x = as.numeric(dx$V1), "punif", min = a_jump, max = b_jump))
            output$model_modal_plot_test <- renderUI({
              if(class(ksTest)!="try-error")
                HTML(paste("<div><h5 class='hModal'>Kolmogorov-Smirnov p-value (the two distributions coincide): ", format(ksTest$p.value, scientific=T, digits = 2), "</h5></div>"))
            })
          }
          
          
          delta <- y$model@sampling@delta
          jumps <- ifelse(abs(diff(x))>threshold,1,0)
          jumps[is.na(jumps)] <- 0
          empirical_Lambda <- cumsum(jumps)
          t <- y$model@sampling@grid[[1]][-1]
          theory_Lambda <- cumsum(eval(y$model@model@measure$intensity)*rep(delta, length(t)))
          Lambda <- data.frame(empirical = empirical_Lambda, theory = theory_Lambda, time = index(y$model@data@original.data)[-1]) 
          output$model_modal_plot_intensity <- renderPlot({
            return(
              ggplot(Lambda, aes(x = time)) +
                geom_line(aes(y = empirical), size = 1, color = "green") +
                geom_line(aes(y = theory), size = 1, color = "blue") +
                scale_color_manual(values=c("green", "blue")) +
                theme(
                  plot.title = element_text(size=14, face= "bold", hjust = 0.5),
                  axis.title=element_text(size=12),
                  legend.position="none"
                ) +
                labs(fill="", title = "Empirical VS Estimated Intensity", x = "", y = "Number of Jumps")
            )

          })
        
        }
      }
    }
  })
  
  
  
  

  ###Delete Model
  observeEvent(input$databaseModelsDelete, priority = 1, {
    if(!is.null(input$databaseModels_rows_selected) & !is.null(input$databaseModels_row_last_clicked)){
      if(input$databaseModels_row_last_clicked %in% input$databaseModels_rows_selected){
        rowname <- unlist(strsplit(rownames(yuimaGUItable$model)[input$databaseModels_row_last_clicked], split = " " , fixed = FALSE))
        delModel(symb=rowname[1], n=rowname[2])
        closeAlert(session, alertId = "modelsAlert_conversion")
      }
    }
  })

  ###DeleteAll Model
  observeEvent(input$databaseModelsDeleteAll, priority = 1, {
    if(!is.null(input$databaseModels_rows_all)){
      closeAlert(session, alertId = "modelsAlert_conversion")
      rowname <- unlist(strsplit(rownames(yuimaGUItable$model)[input$databaseModels_rows_all], split = " " , fixed = FALSE))
      delModel(symb=rowname[seq(1,length(rowname),2)], n=rowname[seq(2,length(rowname),2)])
    }
  })










  ########################Simulation
  ########################
  ########################

  output$simulate_databaseModels <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
    if (length(yuimaGUItable$model)==0){
      NoData <- data.frame("Symb"=NA,"Please estimate some models first"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (yuimaGUItable$model)
  })

  modelsToSimulate <- reactiveValues(table=data.frame())

  ###Select Button
  observeEvent(input$simulate_button_selectModels, priority = 1, {
    modelsToSimulate$table <<- rbind.fill(modelsToSimulate$table, yuimaGUItable$model[(rownames(yuimaGUItable$model) %in% rownames(yuimaGUItable$model)[input$simulate_databaseModels_rows_selected]) & !(rownames(yuimaGUItable$model) %in% rownames(modelsToSimulate$table)),])
  })

  ###SelectAll Button
  observeEvent(input$simulate_button_selectAllModels, priority = 1, {
    modelsToSimulate$table <<- rbind.fill(modelsToSimulate$table, yuimaGUItable$model[(rownames(yuimaGUItable$model) %in% rownames(yuimaGUItable$model)[input$simulate_databaseModels_rows_all]) & !(rownames(yuimaGUItable$model) %in% rownames(modelsToSimulate$table)),])
  })
  
  output$simulate_PrintModelLatex <- renderUI({
    if (!is.null(input$simulate_model_usr_selectModel)){
      return(withMathJax(printModelLatex(names = input$simulate_model_usr_selectModel, process = isolate({input$simulate_model_usr_selectClass}), jumps = jumps_shortcut(class = isolate({input$simulate_model_usr_selectClass}), jumps = input$simulate_model_usr_selectJumps))))
    }
  })

  output$simulate_model_usr_selectModel <- renderUI({
    choices <- as.vector(defaultModels[names(defaultModels)==input$simulate_model_usr_selectClass])
    sel <- choices[1]
    for(i in names(yuimaGUIdata$usr_model))
      if (yuimaGUIdata$usr_model[[i]]$class==input$simulate_model_usr_selectClass)
        choices <- c(i, choices)
    selectInput("simulate_model_usr_selectModel", label = "Model Name", choices = choices, selected = sel)
  })

  output$simulate_model_usr_selectJumps <- renderUI({
    if(input$simulate_model_usr_selectClass %in% c("Compound Poisson", "Levy process"))
      return(selectInput("simulate_model_usr_selectJumps",label = "Jumps", choices = defaultJumps))
  })
  
  output$simulate_model_usr_selectParam <- renderUI({
    valid <- TRUE
    if (is.null(input$simulate_model_usr_selectModel)) valid <- FALSE
    else if (isolate({input$simulate_model_usr_selectClass=="Compound Poisson"}) & is.null(input$simulate_model_usr_selectJumps)) valid <- FALSE
    if (valid) {
      choices <- setModelByName(input$simulate_model_usr_selectModel, jumps = jumps_shortcut(class = isolate({input$simulate_model_usr_selectClass}), jumps = input$simulate_model_usr_selectJumps))@parameter@all
      if (input$simulate_model_usr_selectClass=="Fractional process") choices <- c(choices, "hurst")
      return(selectInput("simulate_model_usr_selectParam", label = "Parameter", choices = choices))
    }
  })

  output$simulate_model_usr_param <- renderUI({
    numericInput("simulate_model_usr_param", label = "Parameter value", value = NA)
  })

  output$simulate_model_usr_ID <- renderUI({
    textInput("simulate_model_usr_ID", label = "Simulation ID")
  })


  observeEvent(input$simulate_model_usr_button_save, {
    if(input$simulate_model_usr_ID!=""){
      id <- gsub(pattern = " ", x = input$simulate_model_usr_ID, replacement = "")
      if (is.null(yuimaGUIdata$usr_simulation[[id]])){
        yuimaGUIdata$usr_simulation[[id]] <<- list()
      }
      yuimaGUIdata$usr_simulation[[id]][["Class"]] <<- input$simulate_model_usr_selectClass
      yuimaGUIdata$usr_simulation[[id]][["Model"]] <<- input$simulate_model_usr_selectModel
      yuimaGUIdata$usr_simulation[[id]][["Jumps"]] <<- input$simulate_model_usr_selectJumps
      if (is.null(yuimaGUIdata$usr_simulation[[id]][["true.param"]])){
        yuimaGUIdata$usr_simulation[[id]][["true.param"]] <<- list()
      }
      allparam <- setModelByName(input$simulate_model_usr_selectModel, jumps = input$simulate_model_usr_selectJumps)@parameter@all
      if (input$simulate_model_usr_selectClass=="Fractional process") allparam <- c(allparam, "hurst")
      if (length(allparam)==0)
        yuimaGUIdata$usr_simulation[[id]]["true.param"] <<- NULL
      if (length(allparam)!=0){
        for(i in c(allparam, names(yuimaGUIdata$usr_simulation[[id]][["true.param"]]))){
          if (!(i %in% names(yuimaGUIdata$usr_simulation[[id]][["true.param"]])))
            yuimaGUIdata$usr_simulation[[id]][["true.param"]][[i]] <<- "MISSING"
          if(!(i %in% allparam))
            yuimaGUIdata$usr_simulation[[id]][["true.param"]][i] <<- NULL
        }
      }
    }
  })

  observe({
    if (!is.null(input$simulate_model_usr_ID) & !is.null(input$simulate_model_usr_selectParam) & !is.null(input$simulate_model_usr_param)){
      id <- gsub(pattern = " ", x = input$simulate_model_usr_ID, replacement = "")
      if (!is.null(yuimaGUIdata$usr_simulation[[id]])){
        valid <- TRUE
        if(yuimaGUIdata$usr_simulation[[id]][["Model"]]!=input$simulate_model_usr_selectModel | input$simulate_model_usr_selectParam=="") 
          valid <- FALSE
        else if (yuimaGUIdata$usr_simulation[[id]][["Class"]] %in% c("Compound Poisson", "Levy process")) if (yuimaGUIdata$usr_simulation[[id]][["Jumps"]]!=input$simulate_model_usr_selectJumps)  
          valid <- FALSE
        if (valid)
          yuimaGUIdata$usr_simulation[[id]][["true.param"]][[input$simulate_model_usr_selectParam]] <- ifelse(is.na(input$simulate_model_usr_param),"MISSING",input$simulate_model_usr_param)
      }
    }
  })

  observe({
    for(i in names(yuimaGUIdata$usr_simulation))
      if (!(yuimaGUIdata$usr_simulation[[i]]$Model %in% c(defaultModels, names(yuimaGUIdata$usr_model))))
          yuimaGUIdata$usr_simulation[i] <<- NULL
  })

  output$simulate_model_usr_table <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollX=TRUE, scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
    table <- data.frame()
    for (i in names(yuimaGUIdata$usr_simulation)){
      newRow <- as.data.frame(yuimaGUIdata$usr_simulation[[i]])
      colnames(newRow) <- gsub(pattern = "true.param.", x = colnames(newRow), replacement = "")
      table <- rbind.fill(table, newRow)
    }
    if (length(table)==0){
      NoData <- data.frame("Model"=NA, "Parameters"=NA)
      return(NoData[-1,])
    }
    return (data.frame(table, row.names = names(yuimaGUIdata$usr_simulation)))
  })

  observeEvent(input$simulate_model_usr_button_select, {
    if (!is.null(input$simulate_model_usr_table_rows_selected)){
      table <- data.frame()
      for (i in names(yuimaGUIdata$usr_simulation)[input$simulate_model_usr_table_rows_selected]){
        if ("MISSING" %in% yuimaGUIdata$usr_simulation[[i]][["true.param"]]){
          createAlert(session = session, anchorId = "panel_simulate_equation_alert", alertId = "simulate_alert_usr_button_select", content = "There are still missing values in selected models", style = "error")
        }
        else {
          closeAlert(session, "simulate_alert_usr_button_select")
          newRow <- as.data.frame(yuimaGUIdata$usr_simulation[[i]], row.names=i)
          colnames(newRow) <- gsub(pattern = "true.param.", x = colnames(newRow), replacement = "")
          table <- rbind.fill(table, newRow)
        }
      }
      if (length(rownames(table))!=0)
        modelsToSimulate$table <<- modelsToSimulate$table[-which(rownames(modelsToSimulate$table) %in% rownames(table)),]
      modelsToSimulate$table <<- rbind.fill(modelsToSimulate$table, table)
    }
  })

  observeEvent(input$simulate_model_usr_button_selectAll, {
    if (!is.null(input$simulate_model_usr_table_rows_all)){
      table <- data.frame()
      for (i in names(yuimaGUIdata$usr_simulation)[input$simulate_model_usr_table_rows_all]){
        if ("MISSING" %in% yuimaGUIdata$usr_simulation[[i]][["true.param"]]){
          createAlert(session = session, anchorId = "panel_simulate_equation_alert", alertId = "simulate_alert_usr_button_select", content = "There are still missing values in selected models", style = "error")
        }
        else {
          closeAlert(session, "simulate_alert_usr_button_select")
          newRow <- as.data.frame(yuimaGUIdata$usr_simulation[[i]], row.names=i)
          colnames(newRow) <- gsub(pattern = "true.param.", x = colnames(newRow), replacement = "")
          table <- rbind.fill(table, newRow)
        }
      }
      if (length(rownames(table))!=0)
        modelsToSimulate$table <<- modelsToSimulate$table[-which(rownames(modelsToSimulate$table) %in% rownames(table)),]
      modelsToSimulate$table <<- rbind.fill(modelsToSimulate$table, table)
    }
  })

  observeEvent(input$simulate_model_usr_button_delete, {
    if (!is.null(input$simulate_model_usr_table_rows_selected)){
      for (i in input$simulate_model_usr_table_rows_selected){
        yuimaGUIdata$usr_simulation[i] <- NULL
      }
    }
  })

  observeEvent(input$simulate_model_usr_button_deleteAll, {
    if (!is.null(input$simulate_model_usr_table_rows_all)){
      for (i in input$simulate_model_usr_table_rows_all){
        yuimaGUIdata$usr_simulation[i] <- NULL
      }
    }
  })

  observe({
    if("AIC" %in% colnames(modelsToSimulate$table))
      modelsToSimulate$table[,"AIC"] <<- as.numeric(as.character(modelsToSimulate$table[,"AIC"]))
    if("BIC" %in% colnames(modelsToSimulate$table))
      modelsToSimulate$table[,"BIC"] <<- as.numeric(as.character(modelsToSimulate$table[,"BIC"]))
  })

  ###Control selected models to be in yuimaGUIdata$model or user defined
  observe({
    if(length(rownames(modelsToSimulate$table))!=0){
      names.valid <- c(names(yuimaGUIdata$usr_simulation), rownames(yuimaGUItable$model))
      col <- colnames(modelsToSimulate$table)
      updatedtable <- data.frame(modelsToSimulate$table[which(rownames(modelsToSimulate$table) %in% names.valid),], row.names = rownames(modelsToSimulate$table)[rownames(modelsToSimulate$table) %in% names.valid])
      colnames(updatedtable) <- col
      modelsToSimulate$table <<- updatedtable
    }
  })

  output$simulate_selectedModels <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollX=TRUE, scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
    if (length(rownames(modelsToSimulate$table))==0){
      NoData <- data.frame("Symb"=NA,"Please select models from the table above"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (modelsToSimulate$table)
  })

  ###Delete Button
  observeEvent(input$simulation_button_deleteModels, priority = 1,{
    if (!is.null(input$simulate_selectedModels_rows_selected))
      modelsToSimulate$table <<- modelsToSimulate$table[-input$simulate_selectedModels_rows_selected,]
  })

  ###DeleteAll Button
  observeEvent(input$simulation_button_deleteAllModels, priority = 1,{
    if (!is.null(input$simulate_selectedModels_rows_all))
      modelsToSimulate$table <<- modelsToSimulate$table[-input$simulate_selectedModels_rows_all,]
  })

  observe({
    shinyjs::toggle(id="simulate_setSimulation_errorMessage", condition = length(rownames(modelsToSimulate$table))==0)
    shinyjs::toggle(id="simulate_setSimulation_body", condition = length(rownames(modelsToSimulate$table))!=0)
  })
  observe({
    shinyjs::toggle(id="simulate_advancedSettings_errorMessage", condition = length(rownames(modelsToSimulate$table))==0)
    shinyjs::toggle(id="simulate_advancedSettings_body", condition = length(rownames(modelsToSimulate$table))!=0)
  })

  observe({
    for (modID in rownames(modelsToSimulate$table)[input$simulate_selectedModels_rows_all]){
      if (modID %in% names(yuimaGUIdata$usr_simulation)){
        if (is.null(yuimaGUIsettings$simulation[[modID]]))
          yuimaGUIsettings$simulation[[modID]] <<- list()
        if (is.null(yuimaGUIsettings$simulation[[modID]][["xinit"]]))
          yuimaGUIsettings$simulation[[modID]][["xinit"]] <<- 1
        if (is.null(yuimaGUIsettings$simulation[[modID]][["nstep"]]))
          yuimaGUIsettings$simulation[[modID]][["nstep"]] <<- 1000
        if (is.null(yuimaGUIsettings$simulation[[modID]][["nsim"]]))
          yuimaGUIsettings$simulation[[modID]][["nsim"]] <<- 1
        if (is.null(yuimaGUIsettings$simulation[[modID]][["t0"]]))
          yuimaGUIsettings$simulation[[modID]][["t0"]] <<- 0
        if (is.null(yuimaGUIsettings$simulation[[modID]][["t1"]]))
          yuimaGUIsettings$simulation[[modID]][["t1"]] <<- 1
        if (is.null(yuimaGUIsettings$simulation[[modID]][["traj"]]))
          yuimaGUIsettings$simulation[[modID]][["traj"]] <<- TRUE
        if (is.null(yuimaGUIsettings$simulation[[modID]][["seed"]]))
          yuimaGUIsettings$simulation[[modID]][["seed"]] <<- NA
      }
      if (modID %in% rownames(yuimaGUItable$model)){
        id <- unlist(strsplit(modID, split = " "))
        if (is.null(yuimaGUIsettings$simulation[[modID]]))
          yuimaGUIsettings$simulation[[modID]] <<- list()
        if (is.null(yuimaGUIsettings$simulation[[modID]][["xinit"]])){
          xinit <- as.numeric(tail(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data,1))[1] 
          toLog <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$toLog
          if(toLog==TRUE) xinit <- exp(xinit)
          yuimaGUIsettings$simulation[[modID]][["xinit"]] <<- xinit 
        }
        if (is.null(yuimaGUIsettings$simulation[[modID]][["nstep"]]))
          yuimaGUIsettings$simulation[[modID]][["nstep"]] <<- NA
        if (is.null(yuimaGUIsettings$simulation[[modID]][["nsim"]]))
          yuimaGUIsettings$simulation[[modID]][["nsim"]] <<- 1
        if (is.null(yuimaGUIsettings$simulation[[modID]][["t0"]]))
          yuimaGUIsettings$simulation[[modID]][["t0"]] <<- end(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)
        if (is.null(yuimaGUIsettings$simulation[[modID]][["t1"]]))
          if(is.numeric(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)))
            yuimaGUIsettings$simulation[[modID]][["t1"]] <<- 2*end(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)-start(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)
          else
            yuimaGUIsettings$simulation[[modID]][["t1"]] <<- end(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)+365
        if (is.null(yuimaGUIsettings$simulation[[modID]][["traj"]]))
          yuimaGUIsettings$simulation[[modID]][["traj"]] <<- TRUE
        if (is.null(yuimaGUIsettings$simulation[[modID]][["seed"]]))
          yuimaGUIsettings$simulation[[modID]][["seed"]] <<- NA
      }
    }
  })

  output$simulate_modelID <- renderUI({
    selectInput("simulate_modelID", label = "Simulation ID", choices = rownames(modelsToSimulate$table))
  })

  output$simulate_advancedSettings_modelID <- renderUI({
    selectInput("simulate_advancedSettings_modelID", label = "Simulation ID", choices = rownames(modelsToSimulate$table))
  })

  output$simulate_seed <- renderUI({
    if(!is.null(input$simulate_advancedSettings_modelID))
      numericInput("simulate_seed", label = "RNG seed", step = 1, min = 0, value = yuimaGUIsettings$simulation[[input$simulate_advancedSettings_modelID]][["seed"]])
  })

  output$simulate_traj <- renderUI({
    if(!is.null(input$simulate_advancedSettings_modelID))
      selectInput("simulate_traj", label = "Save trajectory", choices = c(TRUE,FALSE), selected = yuimaGUIsettings$simulation[[input$simulate_advancedSettings_modelID]][["traj"]])
  })

  output$simulate_nsim <- renderUI({
    if(!is.null(input$simulate_modelID))
      numericInput("simulate_nsim", label = "Number of simulations", value = yuimaGUIsettings$simulation[[input$simulate_modelID]][["nsim"]], min = 1, step = 1)
  })

  output$simulate_nstep <- renderUI({
    if(!is.null(input$simulate_modelID)){
      id <- unlist(strsplit(input$simulate_modelID, split = " "))
      if (input$simulate_modelID %in% names(yuimaGUIdata$usr_simulation)){
        numericInput("simulate_nstep", label = "Number of steps per simulation", value = yuimaGUIsettings$simulation[[input$simulate_modelID]][["nstep"]], min = 1, step = 1)
      } else if (!(isolate({yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$class}) %in% c("COGARCH", "CARMA")))
        numericInput("simulate_nstep", label = "Number of steps per simulation", value = yuimaGUIsettings$simulation[[input$simulate_modelID]][["nstep"]], min = 1, step = 1)
    }
  })

  output$simulate_xinit <- renderUI({
    if(!is.null(input$simulate_modelID))
      numericInput("simulate_xinit", label = "Initial value", value = yuimaGUIsettings$simulation[[input$simulate_modelID]][["xinit"]])
  })

  output$simulate_range <- renderUI({
    if(!is.null(input$simulate_modelID)){
      if (input$simulate_modelID %in% names(yuimaGUIdata$usr_simulation)){
        return(div(
          column(6,numericInput("simulate_rangeNumeric_t0", label = "From", min = 0 ,value = yuimaGUIsettings$simulation[[input$simulate_modelID]][["t0"]])),
          column(6,numericInput("simulate_rangeNumeric_t1", label = "To", min = 0, value = yuimaGUIsettings$simulation[[input$simulate_modelID]][["t1"]]))
        ))
      }
      if (input$simulate_modelID %in% rownames(yuimaGUItable$model)){
        id <- unlist(strsplit(input$simulate_modelID, split = " "))
        if (class(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date")
          dateRangeInput("simulate_rangeDate", label = "Simulation interval", start = yuimaGUIsettings$simulation[[input$simulate_modelID]][["t0"]], end = yuimaGUIsettings$simulation[[input$simulate_modelID]][["t1"]])
        else{
          div(
            column(6,numericInput("simulate_rangeNumeric_t0", label = "From", min = 0 ,value = yuimaGUIsettings$simulation[[input$simulate_modelID]][["t0"]])),
            column(6,numericInput("simulate_rangeNumeric_t1", label = "To", min = 0, value = yuimaGUIsettings$simulation[[input$simulate_modelID]][["t1"]]))
          )
        }
      }
    }
  })

  observeEvent(input$simulate_button_apply_advancedSettings, {
    yuimaGUIsettings$simulation[[input$simulate_advancedSettings_modelID]][["seed"]] <<- input$simulate_seed
    yuimaGUIsettings$simulation[[input$simulate_advancedSettings_modelID]][["traj"]] <<- input$simulate_traj
  })
  observeEvent(input$simulate_button_applyAll_advancedSettings, {
    for (modID in rownames(modelsToSimulate$table)){
      yuimaGUIsettings$simulation[[modID]][["seed"]] <<- input$simulate_seed
      yuimaGUIsettings$simulation[[modID]][["traj"]] <<- input$simulate_traj
    }
  })
  observeEvent(input$simulate_button_apply_nsim, {
    yuimaGUIsettings$simulation[[input$simulate_modelID]][["nsim"]] <<- input$simulate_nsim
    yuimaGUIsettings$simulation[[input$simulate_modelID]][["nstep"]] <<- input$simulate_nstep
  })
  observeEvent(input$simulate_button_applyAll_nsim, {
    for (modID in rownames(modelsToSimulate$table)){
      yuimaGUIsettings$simulation[[modID]][["nsim"]] <<- input$simulate_nsim
      yuimaGUIsettings$simulation[[modID]][["nstep"]] <<- input$simulate_nstep
    }
  })
  observeEvent(input$simulate_button_apply_xinit, {
    yuimaGUIsettings$simulation[[input$simulate_modelID]][["xinit"]] <<- input$simulate_xinit
  })
  observeEvent(input$simulate_button_applyAll_xinit, {
    for (modID in rownames(modelsToSimulate$table))
      yuimaGUIsettings$simulation[[modID]][["xinit"]] <<- input$simulate_xinit
  })
  observeEvent(input$simulate_button_apply_range, {
    if (input$simulate_modelID %in% names(yuimaGUIdata$usr_simulation)){
      yuimaGUIsettings$simulation[[input$simulate_modelID]][["t0"]] <<- input$simulate_rangeNumeric_t0
      yuimaGUIsettings$simulation[[input$simulate_modelID]][["t1"]] <<- input$simulate_rangeNumeric_t1
    }
    if (input$simulate_modelID %in% rownames(yuimaGUItable$model)){
      id <- unlist(strsplit(input$simulate_modelID, split = " "))
      if (class(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date"){
        yuimaGUIsettings$simulation[[input$simulate_modelID]][["t0"]] <<- input$simulate_rangeDate[1]
        yuimaGUIsettings$simulation[[input$simulate_modelID]][["t1"]] <<- input$simulate_rangeDate[2]
      }
      else{
        yuimaGUIsettings$simulation[[input$simulate_modelID]][["t0"]] <<- input$simulate_rangeNumeric_t0
        yuimaGUIsettings$simulation[[input$simulate_modelID]][["t1"]] <<- input$simulate_rangeNumeric_t1
      }
    }
  })
  observeEvent(input$simulate_button_applyAll_range, {
    for (modID in rownames(modelsToSimulate$table)){
      if (modID %in% names(yuimaGUIdata$usr_simulation)){
        yuimaGUIsettings$simulation[[modID]][["t0"]] <<- input$simulate_rangeNumeric_t0
        yuimaGUIsettings$simulation[[modID]][["t1"]] <<- input$simulate_rangeNumeric_t1
      }
      if (modID %in% rownames(yuimaGUItable$model)){
        id <- unlist(strsplit(modID, split = " "))
        if (class(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date" & !is.null(input$simulate_rangeDate)){
          yuimaGUIsettings$simulation[[modID]][["t0"]] <<- input$simulate_rangeDate[1]
          yuimaGUIsettings$simulation[[modID]][["t1"]] <<- input$simulate_rangeDate[2]
        }
        if (class(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="numeric" & !is.null(input$simulate_rangeNumeric_t0)){
          yuimaGUIsettings$simulation[[modID]][["t0"]] <<- input$simulate_rangeNumeric_t0
          yuimaGUIsettings$simulation[[modID]][["t1"]] <<- input$simulate_rangeNumeric_t1
        }
      }
    }
  })
  
  observe({
    if (!is.null(modelsToSimulate$table)) if (nrow(modelsToSimulate$table)!=0) {
      closeAlert(session, alertId = "simulate_alert_buttonEstimate1")
      closeAlert(session, alertId = "simulate_alert_buttonEstimate2")
    }
  })

  observeEvent(input$simulate_simulateModels, {
    if (is.null(modelsToSimulate$table)) {
      if (input$panel_simulations=="Estimated models") createAlert(session = session, anchorId = "panel_simulate_model_alert", alertId = "simulate_alert_buttonEstimate1", content = "Table 'Selected Models' is empty", style = "warning")
      if (input$panel_simulations=="Non-estimated models") createAlert(session = session, anchorId = "panel_simulate_equation_alert", alertId = "simulate_alert_buttonEstimate2", content = "Table 'Selected Models' is empty", style = "warning")
    } else if (nrow(modelsToSimulate$table)==0) {
      if (input$panel_simulations=="Estimated models") createAlert(session = session, anchorId = "panel_simulate_model_alert", alertId = "simulate_alert_buttonEstimate1", content = "Table 'Selected Models' is empty", style = "warning")
      if (input$panel_simulations=="Non-estimated models") createAlert(session = session, anchorId = "panel_simulate_equation_alert", alertId = "simulate_alert_buttonEstimate2", content = "Table 'Selected Models' is empty", style = "warning")
    }
    else{
      withProgress(message = 'Simulating: ', value = 0, {
        for (modID in rownames(modelsToSimulate$table)){
          if(modID %in% names(yuimaGUIdata$usr_simulation)){
            incProgress(1/nrow(modelsToSimulate$table), detail = paste(modID,"-",yuimaGUIdata$usr_simulation[[modID]][["Model"]]))
            jumps <- ifelse(is.null(yuimaGUIdata$usr_simulation[[modID]][["Jumps"]]),NA, yuimaGUIdata$usr_simulation[[modID]][["Jumps"]])
            modName <- yuimaGUIdata$usr_simulation[[modID]][["Model"]]
            modelYuimaGUI <- list(model = setYuima(model = setModelByName(name = modName, jumps = jumps)),
                                  info = list(class = yuimaGUIdata$usr_simulation[[modID]][["Class"]], 
                                              modName = modName,
                                              jumps = jumps
                                              )
                                  )
            addSimulation(
              modelYuimaGUI = modelYuimaGUI,
              symbName = modID,
              xinit = yuimaGUIsettings$simulation[[modID]][["xinit"]],
              true.parameter = yuimaGUIdata$usr_simulation[[modID]][["true.param"]],
              nsim = yuimaGUIsettings$simulation[[modID]][["nsim"]],
              nstep = yuimaGUIsettings$simulation[[modID]][["nstep"]],
              simulate.from = yuimaGUIsettings$simulation[[modID]][["t0"]],
              simulate.to = yuimaGUIsettings$simulation[[modID]][["t1"]],
              saveTraj = yuimaGUIsettings$simulation[[modID]][["traj"]],
              seed = yuimaGUIsettings$simulation[[modID]][["seed"]],
              session = session,
              anchorId = "panel_simulations_alert"
            )
          }
          else if(modID %in% rownames(yuimaGUItable$model)){
            id <- unlist(strsplit(modID, split = " "))
            incProgress(1/nrow(modelsToSimulate$table), detail = paste(id[1],"-",yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$modName))
            addSimulation(
              modelYuimaGUI = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]],
              symbName = id[1],
              xinit = yuimaGUIsettings$simulation[[modID]][["xinit"]],
              nsim = yuimaGUIsettings$simulation[[modID]][["nsim"]],
              nstep = yuimaGUIsettings$simulation[[modID]][["nstep"]],
              simulate.from = yuimaGUIsettings$simulation[[modID]][["t0"]],
              simulate.to = yuimaGUIsettings$simulation[[modID]][["t1"]],
              saveTraj = yuimaGUIsettings$simulation[[modID]][["traj"]],
              seed = yuimaGUIsettings$simulation[[modID]][["seed"]],
              session = session,
              anchorId = "panel_simulations_alert"
            )
          }
        }
      })
      updateTabsetPanel(session = session,  inputId = "panel_simulations", selected = "Simulations")
    }
  })

  observe({
    shinyjs::toggle("div_simulations", condition = (input$panel_simulations!="Simulations"))
  })

  ###Create simulations table
  output$simulate_monitor_table <- DT::renderDataTable(options=list(scrollY = 200, scrollX=TRUE, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
    if (length(yuimaGUItable$simulation)==0){
      NoData <- data.frame("Symb"=NA,"Here will be stored simulations you run in the previous tabs"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (yuimaGUItable$simulation)
  })

  observe({
    shinyjs::toggle("simulate_monitor_button_showSimulation", condition = (length(names(yuimaGUIdata$simulation))!=0))
  })

  ###Delete Simulation
  observeEvent(input$simulate_monitor_button_delete, priority = 1, {
    if(!is.null(input$simulate_monitor_table_rows_selected) & !is.null(input$simulate_monitor_table_row_last_clicked)){
      if(input$simulate_monitor_table_row_last_clicked %in% input$simulate_monitor_table_rows_selected){
        rowname <- unlist(strsplit(rownames(yuimaGUItable$simulation)[input$simulate_monitor_table_row_last_clicked], split = " " , fixed = FALSE))
        delSimulation(symb=rowname[1], n=rowname[2])
      }
    }
  })

  ###DeleteAll Simulation
  observeEvent(input$simulate_monitor_button_deleteAll, priority = 1, {
    if(!is.null(input$simulate_monitor_table_rows_all)){
      rowname <- unlist(strsplit(rownames(yuimaGUItable$simulation)[input$simulate_monitor_table_rows_all], split = " " , fixed = FALSE))
      delSimulation(symb=rowname[seq(1,length(rowname),2)], n=rowname[seq(2,length(rowname),2)])
    }
  })

  output$simulate_showSimulation_simID <- renderUI({
    selectInput(inputId = "simulate_showSimulation_simID", label = "Simulation ID", choices = rownames(yuimaGUItable$simulation))
  })

  observationTime <- reactiveValues(x = numeric())
  observeEvent(input$simulate_showSimulation_simID,{
    id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
    if(!is.na(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory[[1]]))
      observationTime$x <<- as.numeric(end(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory))
  })
  observe({
    if (!is.null(input$simulate_showSimulation_plot_click$x) & !is.null(input$simulate_showSimulation_simID)){
      id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
      if(!is.na(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory[[1]]))
        observationTime$x <<- input$simulate_showSimulation_plot_click$x
    }
  })

  observe({
    if(!is.null(input$simulate_showSimulation_simID)){
      if(input$simulate_showSimulation_simID %in% rownames(yuimaGUItable$simulation)){
        id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
        shinyjs::toggle("simulate_showSimulation_plot_div", condition = !is.na(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory[1]))
        if(!is.na(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory[[1]])){
          output$simulate_showSimulation_plot <- renderPlot({
            if(input$simulate_showSimulation_simID %in% rownames(yuimaGUItable$simulation)){
              par(bg="black")
              plot(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory, screens = 1, main = "Trajectory", xlab = "Index", ylab = "", log=switch(input$simulate_showSimulation_plot_scale,"Linear"="","Logarithmic (Y)"="y", "Logarithmic (X)"="x", "Logarithmic (XY)"="xy"), col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
              abline(v = observationTime$x, col="yellow")
              grid(col="grey")
            }
          })
        }
      }
    }
  })

  simulation_hist <- reactiveValues(distribution=list(), values=vector())
  observe({
    if(!is.null(input$simulate_showSimulation_simID)){
      if(input$simulate_showSimulation_simID %in% rownames(yuimaGUItable$simulation)) {
        id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
        shinyjs::toggle("simulate_showSimulation_hist_div", condition = yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$info$nsim > 10)
        if(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$info$nsim > 10){
          if(!is.na(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$hist[1])){
            if(!is.null(input$simulate_showSimulation_hist_nBins)){
              output$simulate_showSimulation_hist <- renderPlot({
                par(bg="black")
                simulation_hist$values <<- yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$hist
                simulation_hist$distribution[[1]] <<- hist(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$hist, freq = FALSE ,main = "Histogram", xlab = "", breaks = input$simulate_showSimulation_hist_nBins, col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
                lines(density(simulation_hist$values), col = "orange")
                grid()
              })
            }
          }
          else{
            if(!is.null(input$simulate_showSimulation_hist_nBins)){
              traj <- yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory
              if(class(index(traj))[1]=="numeric"){
                data <- traj[which(abs(index(traj)-observationTime$x) == min(abs(index(traj)-observationTime$x))),]
              } else {
                x <- as.Date(as.POSIXct.numeric(observationTime$x, origin = "1970-01-01"))
                data <- traj[which(abs(as.Date(index(traj))-x) == min(abs(as.Date(index(traj))-x))),]
              }
              output$simulate_showSimulation_hist <- renderPlot({
                par(bg="black")
                simulation_hist$values <<- data
                simulation_hist$distribution[[1]] <<- hist(data, freq = FALSE ,main = "Histogram", xlab = "", breaks = input$simulate_showSimulation_hist_nBins, col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
                lines(density(simulation_hist$values), col = "orange")
                grid()
              })
            }
         }
        }
      }
    }
  })

  output$simulate_showSimulation_hist_text <- renderUI({
    if(length(simulation_hist$values)!=0 & !is.null(input$simulate_showSimulation_hist_probability_slider)){
      val <- as.numeric(simulation_hist$values)
      qq <- quantile(val, probs = input$simulate_showSimulation_hist_probability_slider/100)
      HTML(paste("<div>", "Lower:", qq[1],"<br/>", "Upper: ", qq[2], "<br/>", "Mean: ", mean(val[val>=qq[1] & val<=qq[2]]), "</div>"))
    }
  })

  ###Save Trajectory Button
  output$simulate_showSimulation_button_saveTrajectory <- {
    dataDownload_traj <- reactive({
      id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
      x <- yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory
      d <- data.frame(x, row.names = index(x))
      colnames(d) <- paste(id[1],id[2],"_",seq(1, ncol(d)), sep = "")
      return(d)
    })
    downloadHandler(
      filename = function() {
        paste(input$simulate_showSimulation_simID, ".txt", sep="")
      },
      content = function(file) {
        write.table(dataDownload_traj(), file, row.names = TRUE, col.names = TRUE, quote = TRUE)
      }
    )
  }

  ###Save Histogram Button
  output$simulate_showSimulation_button_saveHist <- {
    dataDownload_hist <- reactive({
      h <- simulation_hist$distribution[[1]]
      data.frame(midPoints = h$mids, counts = h$counts, frequency=h$counts/sum(h$counts),density = h$density)
    })
    downloadHandler(
      filename = function() {
        paste(input$simulate_showSimulation_simID, "_hist",".txt", sep="")
      },
      content = function(file) {
        write.table(dataDownload_hist(), file, row.names = FALSE, col.names = TRUE)
      }
    )
  }

  
  
  
  
  
  
  
  
  
  
  ########################Clustering
  ########################
  ########################
  
  ###Display available data
  output$cluster_table_select <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (yuimaGUItable$series)
  })
  
  ###Table of selected data to cluster
  seriesToCluster <- reactiveValues(table=data.frame())
  
  ###Select Button
  observeEvent(input$cluster_button_select, priority = 1, {
    if (length(input$cluster_table_select_rows_selected)!=0){
      closeAlert(session, "cluster_alert_indexes")
      if (nrow(seriesToCluster$table)==0)
        seriesToCluster$table <<- rbind(seriesToCluster$table, yuimaGUItable$series[rownames(yuimaGUItable$series)[input$cluster_table_select_rows_selected[1]],])
      for (symb in rownames(yuimaGUItable$series)[input$cluster_table_select_rows_selected]){
        if (class(index(yuimaGUIdata$series[[symb]]))==class(index(yuimaGUIdata$series[[rownames(seriesToCluster$table)[1]]]))){
          if (!(symb %in% rownames(seriesToCluster$table)))
            seriesToCluster$table <<- rbind(seriesToCluster$table, yuimaGUItable$series[symb,])
        } else {
          createAlert(session, anchorId = "cluster_alert", alertId = "cluster_alert_indexes", append = FALSE, content = "Cannot cluster series with different type of index (numeric/date)", style = "warning")
        }
      }
    }
  })
  
  ###SelectAll Button
  observeEvent(input$cluster_button_selectAll, priority = 1, {
    if (length(input$cluster_table_select_rows_all)!=0){
      closeAlert(session, "cluster_alert_indexes")
      if (nrow(seriesToCluster$table)==0)
        seriesToCluster$table <<- rbind(seriesToCluster$table, yuimaGUItable$series[rownames(yuimaGUItable$series)[input$cluster_table_select_rows_all[1]],])
      for (symb in rownames(yuimaGUItable$series)[input$cluster_table_select_rows_all]){
        if (class(index(yuimaGUIdata$series[[symb]]))==class(index(yuimaGUIdata$series[[rownames(seriesToCluster$table)[1]]]))){
          if (!(symb %in% rownames(seriesToCluster$table)))
            seriesToCluster$table <<- rbind(seriesToCluster$table, yuimaGUItable$series[symb,])
        } else {
          createAlert(session, anchorId = "cluster_alert", alertId = "cluster_alert_indexes", append = FALSE, content = "Cannot cluster series with different type of index (numeric/date)", style = "warning")
        }
      }
    }
  })
  
  ###Display Selected Data
  output$cluster_table_selected <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = FALSE, selection = "multiple",{
    if (length(rownames(seriesToCluster$table))==0){
      NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (seriesToCluster$table)
  })
  
  ###Control selected data to be in yuimaGUIdata$series
  observe({
    if(length(seriesToCluster$table)!=0){
      if (length(yuimaGUItable$series)==0)
        seriesToCluster$table <<- data.frame()
      else
        seriesToCluster$table <<- seriesToCluster$table[which(as.character(seriesToCluster$table[,"Symb"]) %in% as.character(yuimaGUItable$series[,"Symb"])),]
    }
  })
  
  ###Delete Button
  observeEvent(input$cluster_button_delete, priority = 1,{
    if (!is.null(input$cluster_table_selected_rows_selected))
      seriesToCluster$table <<- seriesToCluster$table[-input$cluster_table_selected_rows_selected,]
  })
  
  ###DeleteAll Button
  observeEvent(input$cluster_button_deleteAll, priority = 1,{
    if (!is.null(input$cluster_table_selected_rows_all))
      seriesToCluster$table <<- seriesToCluster$table[-input$cluster_table_selected_rows_all,]
  })
  
  observe({
    shinyjs::toggle("cluster_distance_minkowskiPower", condition = (input$cluster_distance=="minkowski"))
  })
  
  observeEvent(input$cluster_button_startCluster, {
    closeAlert(session, "cluster_alert_dist")
    if (length(rownames(seriesToCluster$table))<=2)
      createAlert(session, anchorId = "cluster_alert", alertId = "cluster_alert_dist", content = "Select at least 3 series", style = "error")
    if (length(rownames(seriesToCluster$table))>2){ withProgress(value = 1, message = "Calculating...", {
      names_list <- rownames(seriesToCluster$table)
      x <- yuimaGUIdata$series[[names_list[1]]]
      for(i in names_list[-1])
        x <- merge(x, yuimaGUIdata$series[[i]])
      colnames(x) <- names_list
      d <- switch(
        input$cluster_distance,
        "MOdist" = try(sde::MOdist(na.omit(x))),
        "MYdist_perc" = try(MYdist(x, percentage = TRUE)),
        "MYdist_ass" = try(MYdist(x, percentage = FALSE)),
        "euclidean" = try(dist(t(as.data.frame(x)), method = "euclidean")),
        "maximum" = try(dist(t(as.data.frame(x)), method = "maximum")),
        "manhattan" = try(dist(t(as.data.frame(x)), method = "manhattan")),
        "canberra" = try(dist(t(as.data.frame(x)), method = "canberra")),
        "minkowski" = try(dist(t(as.data.frame(x)), method = "minkowski", p = input$cluster_distance_minkowskiPower))
      )
      if (class(d)=="try-error")
        createAlert(session, anchorId = "cluster_alert", alertId = "cluster_alert_dist", content = "Error in clustering", style = "error")
      else{
        hc <- hclust(d, method = input$cluster_linkage)
        i <- 1
        id <- "Clustering"
        repeat {
          if(id %in% names(yuimaGUIdata$cluster)){
            id <- paste("Clustering", i)
            i <- i+1
          } else break
        }
        yuimaGUIdata$cluster[[id]] <<- list(d = d, linkage = input$cluster_linkage, distance = input$cluster_distance, power = input$cluster_distance_minkowskiPower)
      }
    })}
  })
  
  output$cluster_analysis_id <- renderUI({
    n <- names(yuimaGUIdata$cluster)
    if(length(n)!=0)
      selectInput("cluster_analysis_id", label = "Clustering ID", choices = sort(n), selected = last(n))
  })
  
  observeEvent(input$cluster_analysis_id, {
    if(!is.null(input$cluster_analysis_id)) if (input$cluster_analysis_id %in% names(yuimaGUIdata$cluster)){
      d <- yuimaGUIdata$cluster[[input$cluster_analysis_id]]$d
      hc <- hclust(d, method = yuimaGUIdata$cluster[[input$cluster_analysis_id]]$linkage)
      labelColors <- c("#CDB380", "#FF0000", "#036564", "#FF00FF", "#EB6841", "#7FFFD4", "#EDC951","#FF8000", "#FFE4E1", "#A2CD5A", "#71C671", "#AAAAAA", "#555555", "#FFA07A", "#8B6508", "#FFC125", "#FFFACD", "#808000",   "#458B00", "#54FF9F", "#43CD80", "#008B8B", "#53868B", "#B0E2FF", "#0000FF", "#F8F8FF", "#551A8B", "#AB82FF", "#BF3EFF", "#FF83FA", "#8B1C62", "#CD6839", "#8E8E38", "#1E1E1E")
      dendrClick <- reactiveValues(y = NULL)
      output$cluster_dendogram <- renderPlot({
        if(!is.null(input$cluster_dendrogram_click$y))
          dendrClick$y <- input$cluster_dendrogram_click$y
        if(!is.null(dendrClick$y)){
          clusMember = cutree(hc, h = dendrClick$y)
          colLab <- function(n) {
            if (is.leaf(n)) {
              a <- attributes(n)
              labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
              attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
            }
            n
          }
          hc <- dendrapply(as.dendrogram(hc), colLab)
        }
        if(is.null(dendrClick$y)){
          colDefault <- function(n){  
            if (is.leaf(n))
              attr(n, "nodePar") <- c(attributes(n)$nodePar, lab.col = labelColors[1])
            return(n)
          }
          hc <- dendrapply(as.dendrogram(hc), colDefault)
        }
        output$cluster_button_saveDendogram <- downloadHandler(
          filename = "Dendrogram.png",
          content = function(file) {
            png(file, width = 960)
            par(bg="black", xaxt = "n", mar= c(10, 4, 4, 2)+0.1)
            plot(hc, ylab = "", xlab = "", main = "Dendrogram", edgePar=list(col="grey50"), col.main = "#FFF68F", col.axis="grey")
            dev.off()
          }
        )
        par(bg="black", xaxt = "n", mar= c(10, 4, 4, 2)+0.1)
        plot(hc, ylab = "", xlab = "", main = "Dendrogram", edgePar=list(col="grey50"), col.main = "#FFF68F", col.axis="grey")
      })
      output$cluster_scaling2D <- renderPlot({
        points <- cmdscale(d)
        if(!is.null(dendrClick$y))
          g1 <- cutree(hclust(d), h = dendrClick$y)
        else
          g1 <- 1
        output$cluster_button_saveScaling2D <- downloadHandler(
          filename = "Multidimensional scaling.png",
          content = function(file) {
            png(file)
            par(bg="black", xaxt = "n", yaxt = "n", bty="n")
            plot(points, col=labelColors[g1], pch=16, cex=2, main = "Multidimensional scaling", col.main = "#FFF68F", xlab="", ylab="")
            dev.off()
          }
        )
        par(bg="black", xaxt = "n", yaxt = "n", bty="n")
        plot(points, col=labelColors[g1], pch=16, cex=2, main = "Multidimensional scaling", col.main = "#FFF68F", xlab="", ylab="")
      })  
    }
  })
  
  output$cluster_moreInfo <- renderUI({
    if(!is.null(input$cluster_analysis_id)) if (input$cluster_analysis_id %in% names(isolate({yuimaGUIdata$cluster}))){
      info <- isolate({yuimaGUIdata$cluster[[input$cluster_analysis_id]]})
      dist <- switch(info$distance, 
                     "MOdist"="Markov Operator", 
                     "MYdist_perc"="Percentage Increments Distribution", 
                     "MYdist_ass"="Increments Distribution", 
                     "euclidean"="Euclidean", 
                     "maximum"="Maximum", 
                     "manhattan"="Manhattan", 
                     "canberra"="Canberra", 
                     "minkowski"="Minkowski")
      linkage <- switch(info$linkage,
                        "complete"="Complete", 
                        "single"="Single", 
                        "average"="Average", 
                        "ward.D"="Ward", 
                        "ward.D2"="Ward squared", 
                        "mcquitty"="McQuitty", 
                        "Median"="median", 
                        "centroid"="Centroid")
      if (dist=="Minkowski") dist <- paste(dist, " (", info$power,")", sep = "")
      return(HTML(paste("<div><h4>&nbsp &nbsp Linkage:",linkage, " &nbsp &nbsp &nbsp &nbsp Distance:", dist, "</h4></div>")))
    }
  })
  
  observeEvent(input$cluster_button_delete_analysis, {
    yuimaGUIdata$cluster[[input$cluster_analysis_id]] <<- NULL
  })
  
  observeEvent(input$cluster_button_deleteAll_analysis, {
    yuimaGUIdata$cluster <<- list()
  })

  observe({
    shinyjs::toggle("cluster_charts", condition = length(names(yuimaGUIdata$cluster))!=0)
  })
  
  
  ########################Nonparametric Change Point
  ########################
  ########################
  
  ###Display available data
  output$changepoint_table_select <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (yuimaGUItable$series)
  })
  
  ###Table of selected data to change point
  seriesToChangePoint <- reactiveValues(table=data.frame())
  
  ###Select Button
  observeEvent(input$changepoint_button_select, priority = 1, {
    seriesToChangePoint$table <<- rbind(seriesToChangePoint$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% rownames(yuimaGUItable$series)[input$changepoint_table_select_rows_selected]) & !(rownames(yuimaGUItable$series) %in% rownames(seriesToChangePoint$table)),])
  })
  
  ###SelectAll Button
  observeEvent(input$changepoint_button_selectAll, priority = 1, {
    seriesToChangePoint$table <<- rbind(seriesToChangePoint$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% rownames(yuimaGUItable$series)[input$changepoint_table_select_rows_all]) & !(rownames(yuimaGUItable$series) %in% rownames(seriesToChangePoint$table)),])
  })
  
  ###Display Selected Data
  output$changepoint_table_selected <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = FALSE, selection = "multiple",{
    if (length(rownames(seriesToChangePoint$table))==0){
      NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (seriesToChangePoint$table)
  })
  
  ###Control selected data to be in yuimaGUIdata$series
  observe({
    if(length(seriesToChangePoint$table)!=0){
      if (length(yuimaGUItable$series)==0)
        seriesToChangePoint$table <<- data.frame()
      else
        seriesToChangePoint$table <<- seriesToChangePoint$table[which(as.character(seriesToChangePoint$table[,"Symb"]) %in% as.character(yuimaGUItable$series[,"Symb"])),]
    }
  })
  
  ###Delete Button
  observeEvent(input$changepoint_button_delete, priority = 1,{
    if (!is.null(input$changepoint_table_selected_rows_selected))
      seriesToChangePoint$table <<- seriesToChangePoint$table[-input$changepoint_table_selected_rows_selected,]
  })
  
  ###DeleteAll Button
  observeEvent(input$changepoint_button_deleteAll, priority = 1,{
    if (!is.null(input$changepoint_table_selected_rows_all))
      seriesToChangePoint$table <<- seriesToChangePoint$table[-input$changepoint_table_selected_rows_all,]
  })
  
  observe({
    shinyjs::toggle("changepoint_charts", condition = (length(names(yuimaGUIdata$cp))!=0))
  })
  
  observe({
    shinyjs::toggle("parametric_changepoint_charts", condition = (length(names(yuimaGUIdata$cpYuima))!=0))
  })
  
  output$changepoint_symb <- renderUI({
    n <- names(yuimaGUIdata$cp)
    if(length(n)!=0)
      selectInput("changepoint_symb", "Symbol", choices = sort(n), selected = last(n))  
  })
  
  observeEvent(input$changepoint_button_startEstimation, {
    if (length(rownames(seriesToChangePoint$table))!=0)
      withProgress(message = 'Analyzing: ', value = 0, {
        errors <- c()
        for (i in rownames(seriesToChangePoint$table)){
          incProgress(1/length(rownames(seriesToChangePoint$table)), detail = i)
          errors <- c(errors, addCPoint_distribution(symb = i, method = input$changepoint_method, pvalue = input$changepoint_pvalue)$error)
        }
        if(!is.null(errors)) 
          createAlert(session = session, anchorId = "nonparametric_changepoint_alert", content = paste("Unable to estimate change points of:", paste(errors, collapse = " ")), dismiss = T, style = "error")
      })
  })
  
  range_changePoint <- reactiveValues(x=NULL, y=NULL)
  observe({
    if (!is.null(input$changePoint_brush) & !is.null(input$changepoint_symb)){
      data <- yuimaGUIdata$cp[[input$changepoint_symb]]$series
      test <- (length(index(window(data, start = input$changePoint_brush$xmin, end = input$changePoint_brush$xmax))) > 3)
      if (test==TRUE){
        range_changePoint$x <- c(as.Date(input$changePoint_brush$xmin), as.Date(input$changePoint_brush$xmax))
        range_changePoint$y <- c(input$changePoint_brush$ymin, input$changePoint_brush$ymax)
      }
    }
  })
  
  observeEvent(input$changePoint_dbclick,{
    range_changePoint$x <- c(NULL, NULL)
  })
  
  observeEvent(input$changepoint_symb, {
    range_changePoint$x <- c(NULL, NULL)
    output$changepoint_plot_series <- renderPlot({
      if(!is.null(input$changepoint_symb)) {
        cp <- isolate({yuimaGUIdata$cp[[input$changepoint_symb]]})
        par(bg="black")
        plot(window(cp$series, start = range_changePoint$x[1], end = range_changePoint$x[2]), main=cp$symb, xlab="Index", ylab=NA, log=switch(input$changepoint_scale,"Linear"="","Logarithmic (Y)"="y", "Logarithmic (X)"="x", "Logarithmic (XY)"="xy"), col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
        abline(v=cp$tau, col = "red")
        grid(col="grey")
      }
    })
    output$changepoint_plot_incr <- renderPlot({
      if(!is.null(input$changepoint_symb)) {
        cp <- isolate({yuimaGUIdata$cp[[input$changepoint_symb]]})
        if(cp$method=="KSdiff") {
          x <- diff(cp$series)
          title <- " - Increments"
        }
        else {
          x <- Delt(cp$series)
          title <- " - Percentage Increments"
        }
        x <- x[x[,1]!="Inf"]
        par(bg="black")
        plot(window(x, start = range_changePoint$x[1], end = range_changePoint$x[2]), main=paste(cp$symb, title), xlab="Index", ylab=NA, log=switch(input$changepoint_scale,"Linear"="","Logarithmic (Y)"="", "Logarithmic (X)"="x", "Logarithmic (XY)"="x"), col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
        abline(v=cp$tau, col = "red")
        grid(col="grey")
      }
    })
  })
  
  
  output$text_ChangePointInfo <- renderUI({
    if(!is.null(input$changepoint_symb)){
      info <- yuimaGUIdata$cp[[input$changepoint_symb]]
      div(
        h3(input$changepoint_symb, class = "hModal"),
        h4(
          em(switch(info$method, "KSdiff"="Increments Distriution", "KSperc"="Percentage Increments Distriution")), br(),
          class = "hModal"
        ),
        align="center"
      )
    }
  })
  
  
  output$table_ChangePointInfo <- renderTable(digits = 4, {
    table <- data.frame(Time = as.character(yuimaGUIdata$cp[[input$changepoint_symb]]$tau), "p.value" = yuimaGUIdata$cp[[input$changepoint_symb]]$pvalue, check.names = FALSE, row.names = yuimaGUIdata$cp[[input$changepoint_symb]]$tau)
    return(table[order(rownames(table), decreasing = TRUE),])
  })
  
  observeEvent(input$changepoint_button_delete_estimated, {
    yuimaGUIdata$cp[[input$changepoint_symb]] <<- NULL
  })
  
  observeEvent(input$changepoint_button_deleteAll_estimated, {
    yuimaGUIdata$cp <<- list()
  })
  
  
  ########################Parametric Change Point
  ########################
  ########################
  
  ###Display available data
  output$parametric_changepoint_table_select <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (yuimaGUItable$series)
  })
  
  ###Table of selected data to change point
  parametric_seriesToChangePoint <- reactiveValues(table=data.frame())
  
  ###Select Button
  observeEvent(input$parametric_changepoint_button_select, priority = 1, {
    parametric_seriesToChangePoint$table <<- rbind(parametric_seriesToChangePoint$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% rownames(yuimaGUItable$series)[input$parametric_changepoint_table_select_rows_selected]) & !(rownames(yuimaGUItable$series) %in% rownames(parametric_seriesToChangePoint$table)),])
  })
  
  ###SelectAll Button
  observeEvent(input$parametric_changepoint_button_selectAll, priority = 1, {
    parametric_seriesToChangePoint$table <<- rbind(parametric_seriesToChangePoint$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% rownames(yuimaGUItable$series)[input$parametric_changepoint_table_select_rows_all]) & !(rownames(yuimaGUItable$series) %in% rownames(parametric_seriesToChangePoint$table)),])
  })
  
  ###Display Selected Data
  output$parametric_changepoint_table_selected <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = FALSE, selection = "multiple",{
    if (length(rownames(parametric_seriesToChangePoint$table))==0){
      NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (parametric_seriesToChangePoint$table)
  })
  
  ###Control selected data to be in yuimaGUIdata$series
  observe({
    if(length(parametric_seriesToChangePoint$table)!=0){
      if (length(yuimaGUItable$series)==0)
        parametric_seriesToChangePoint$table <<- data.frame()
      else
        parametric_seriesToChangePoint$table <<- parametric_seriesToChangePoint$table[which(as.character(parametric_seriesToChangePoint$table[,"Symb"]) %in% as.character(yuimaGUItable$series[,"Symb"])),]
    }
  })
  
  ###Delete Button
  observeEvent(input$parametric_changepoint_button_delete, priority = 1,{
    if (!is.null(input$parametric_changepoint_table_selected_rows_selected))
      parametric_seriesToChangePoint$table <<- parametric_seriesToChangePoint$table[-input$parametric_changepoint_table_selected_rows_selected,]
  })
  
  ###DeleteAll Button
  observeEvent(input$parametric_changepoint_button_deleteAll, priority = 1,{
    if (!is.null(input$parametric_changepoint_table_selected_rows_all))
      parametric_seriesToChangePoint$table <<- parametric_seriesToChangePoint$table[-input$parametric_changepoint_table_selected_rows_all,]
  })
  
  output$parametric_changepoint_model <- renderUI({
    choices <- as.vector(defaultModels[names(defaultModels)=="Diffusion process"])
    sel <- choices[1]
    for(i in names(yuimaGUIdata$usr_model))
      if (yuimaGUIdata$usr_model[[i]]$class=="Diffusion process") choices <- c(i, choices)
    selectInput("parametric_changepoint_model", label = "Model", choices = choices, multiple = FALSE, selected = sel)
  })
  
  
  ###Interactive range of selectRange chart
  parametric_range_selectRange <- reactiveValues(x=NULL, y=NULL)
  observe({
    if (!is.null(input$parametric_selectRange_brush) & !is.null(input$parametric_plotsRangeSeries)){
      data <- getData(input$parametric_plotsRangeSeries)
      test <- (length(index(window(data, start = input$parametric_selectRange_brush$xmin, end = input$parametric_selectRange_brush$xmax))) > 3)
      if (test==TRUE){
        parametric_range_selectRange$x <- c(as.Date(input$parametric_selectRange_brush$xmin), as.Date(input$parametric_selectRange_brush$xmax))
        parametric_range_selectRange$y <- c(input$parametric_selectRange_brush$ymin, input$parametric_selectRange_brush$ymax)
      }
    }
  })
  
  
  observe({
    shinyjs::toggle(id="parametric_plotsRangeErrorMessage", condition = nrow(parametric_seriesToChangePoint$table)==0)
    shinyjs::toggle(id="parametric_plotsRangeAll", condition = nrow(parametric_seriesToChangePoint$table)!=0)
  })
  
  ###Display charts: series and its increments
  observe({
    symb <- input$parametric_plotsRangeSeries
    if(!is.null(symb))
      if (symb %in% rownames(yuimaGUItable$series)){
        data <- getData(symb)
        incr <- na.omit(Delt(data, type = "arithmetic"))
        condition <- all(is.finite(incr))
        shinyjs::toggle("parametric_selectRangeReturns", condition = condition)
        parametric_range_selectRange$x <- NULL
        parametric_range_selectRange$y <- NULL
        start <- as.character(parametric_seriesToChangePoint$table[input$parametric_plotsRangeSeries,"From"])
        end <- as.character(parametric_seriesToChangePoint$table[input$parametric_plotsRangeSeries,"To"])
        if(class(index(data))=="numeric"){
          start <- as.numeric(start)
          end <- as.numeric(end)
        }
        output$parametric_selectRange <- renderPlot({
          if ((symb %in% rownames(yuimaGUItable$series) & (symb %in% rownames(parametric_seriesToChangePoint$table)))){
            par(bg="black")
            plot.zoo(window(data, start = parametric_range_selectRange$x[1], end = parametric_range_selectRange$x[2]), main=symb, xlab="Index", ylab=NA, log=switch(input$parametric_scale_selectRange,"Linear"="","Logarithmic (Y)"="y", "Logarithmic (X)"="x", "Logarithmic (XY)"="xy"), col="grey", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
            lines(window(data, start = start, end = end), col = "green")
            grid(col="grey")
          }
        })
        output$parametric_selectRangeReturns <- renderPlot({
          if (symb %in% rownames(yuimaGUItable$series) & (symb %in% rownames(parametric_seriesToChangePoint$table)) & condition){
            par(bg="black")
            plot.zoo( window(incr, start = parametric_range_selectRange$x[1], end = parametric_range_selectRange$x[2]), main=paste(symb, " - Percentage Increments"), xlab="Index", ylab=NA, log=switch(input$parametric_scale_selectRange,"Linear"="","Logarithmic (Y)"="", "Logarithmic (X)"="x", "Logarithmic (XY)"="x"), col="grey", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
            lines(window(incr, start = start,  end = end), col = "green")
            grid(col="grey")
          }
        })
      }
  })
  
  
  output$parametric_plotsRangeSeries <- renderUI({
    selectInput("parametric_plotsRangeSeries", label = "Series", choices = rownames(parametric_seriesToChangePoint$table), selected = input$parametric_plotsRangeSeries)
  })
  
  ###Choose Range input set to "Select range from charts" if charts have been brushed
  output$parametric_chooseRange <- renderUI({
    sel <- "full"
    if (!is.null(parametric_range_selectRange$x)) sel <- "selected"
    selectInput("parametric_chooseRange", label = "Range", choices = c("Full Range" = "full", "Select Range from Charts" = "selected", "Specify Range" = "specify"), selected = sel)
  })
  
  output$parametric_chooseRange_specify <- renderUI({
    if(!is.null(input$parametric_plotsRangeSeries)) {
      data <- getData(input$parametric_plotsRangeSeries)
      if(class(index(data))=="numeric") 
        return(div(
          column(6,numericInput("parametric_chooseRange_specify_t0", label = "From", min = start(data), max = end(data), value = start(data))),
          column(6,numericInput("parametric_chooseRange_specify_t1", label = "To", min = start(data), max = end(data), value = end(data)))
        ))
      if(class(index(data))=="Date")
        return(dateRangeInput("parametric_chooseRange_specify_date", start = start(data), end = end(data), label = "Specify Range"))
    }
  })
  
  
  observe({
    shinyjs::toggle(id = "parametric_chooseRange_specify", condition = (input$parametric_chooseRange)=="specify")
  })
  
  ###Function to update data range to use to estimate models
  updateRange_parametric_seriesToChangePoint <- function(symb, range = c("full","selected","specify"), type = c("Date", "numeric")){
    for (i in symb){
      data <- getData(i)
      if (range == "full"){
        levels(parametric_seriesToChangePoint$table[,"From"]) <- c(levels(parametric_seriesToChangePoint$table[,"From"]), as.character(start(data)))
        levels(parametric_seriesToChangePoint$table[,"To"]) <- c(levels(parametric_seriesToChangePoint$table[,"To"]), as.character(end(data)))
        parametric_seriesToChangePoint$table[i,"From"] <<- as.character(start(data))
        parametric_seriesToChangePoint$table[i,"To"] <<- as.character(end(data))
      }
      if (range == "selected"){
        if(!is.null(parametric_range_selectRange$x) & class(index(data))==type){
          start <- parametric_range_selectRange$x[1]
          end <- parametric_range_selectRange$x[2]
          if(class(index(data))=="numeric"){
            start <- as.numeric(start)
            end <- as.numeric(end)
          }
          start <- max(start(data),start)
          end <- min(end(data), end)
          levels(parametric_seriesToChangePoint$table[,"From"]) <- c(levels(parametric_seriesToChangePoint$table[,"From"]), as.character(start))
          levels(parametric_seriesToChangePoint$table[,"To"]) <- c(levels(parametric_seriesToChangePoint$table[,"To"]), as.character(end))
          parametric_seriesToChangePoint$table[i,"From"] <<- as.character(start)
          parametric_seriesToChangePoint$table[i,"To"] <<- as.character(end)
        }
      }
      if (range == "specify"){
        if(class(index(data))==type){
          if(class(index(data))=="Date"){
            start <- input$parametric_chooseRange_specify_date[1]
            end <- input$parametric_chooseRange_specify_date[2]
          }
          if(class(index(data))=="numeric"){
            start <- input$parametric_chooseRange_specify_t0
            end <- input$parametric_chooseRange_specify_t1
          }
          start <- max(start(data),start)
          end <- min(end(data), end)
          levels(parametric_seriesToChangePoint$table[,"From"]) <- c(levels(parametric_seriesToChangePoint$table[,"From"]), as.character(start))
          levels(parametric_seriesToChangePoint$table[,"To"]) <- c(levels(parametric_seriesToChangePoint$table[,"To"]), as.character(end))
          parametric_seriesToChangePoint$table[i,"From"] <<- as.character(start)
          parametric_seriesToChangePoint$table[i,"To"] <<- as.character(end)
        }
      }
    }
  }
  
  ###Apply selected range by double click
  observeEvent(input$parametric_selectRange_dbclick, priority = 1, {
    updateRange_parametric_seriesToChangePoint(input$parametric_plotsRangeSeries, range = "selected", type = class(index(getData(input$parametric_plotsRangeSeries))))
  })
  
  ###Apply selected range
  observeEvent(input$parametric_buttonApplyRange, priority = 1, {
    updateRange_parametric_seriesToChangePoint(input$parametric_plotsRangeSeries, range = input$parametric_chooseRange, type = class(index(getData(input$parametric_plotsRangeSeries))))
  })
  
  ###ApplyAll selected range
  observeEvent(input$parametric_buttonApplyAllRange, priority = 1, {
    updateRange_parametric_seriesToChangePoint(rownames(parametric_seriesToChangePoint$table), range = input$parametric_chooseRange, type = class(index(getData(input$parametric_plotsRangeSeries))))
  })
  
  
  ### Estimation Settings
  parametric_modal_prev_buttonDelta <- 0
  parametric_modal_prev_buttonAllDelta <- 0
  observe({
    for (symb in rownames(parametric_seriesToChangePoint$table)){
      if (is.null(yuimaGUIsettings$delta[[symb]])) yuimaGUIsettings$delta[[symb]] <<- 0.01
      if (is.null(yuimaGUIsettings$toLog[[symb]])) yuimaGUIsettings$toLog[[symb]] <<- FALSE
      data <- na.omit(as.numeric(getData(symb)))
      if (yuimaGUIsettings$toLog[[symb]]==TRUE) data <- log(data)
      for (modName in input$parametric_changepoint_model){
        if (class(try(setModelByName(modName, jumps = NA, AR_C = NA, MA_C = NA)))!="try-error"){
          if (is.null(yuimaGUIsettings$estimation[[modName]]))
            yuimaGUIsettings$estimation[[modName]] <<- list()
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]]))
            yuimaGUIsettings$estimation[[modName]][[symb]] <<- list()
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]] <<- list()
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["start"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["start"]] <<- list()
          
          startMinMax <- defaultBounds(name = modName, 
                                       jumps = NA, 
                                       AR_C = NA, 
                                       MA_C = NA, 
                                       strict = FALSE,
                                       data = data,
                                       delta = yuimaGUIsettings$delta[[symb]])
          upperLower <- defaultBounds(name = modName, 
                                      jumps = NA, 
                                      AR_C = NA, 
                                      MA_C = NA, 
                                      strict = TRUE,
                                      data = data,
                                      delta = yuimaGUIsettings$delta[[symb]])
          
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]] <<- startMinMax$lower
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]] <<- startMinMax$upper
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]] <<- upperLower$upper
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]] <<- upperLower$lower
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["method"]])){
            yuimaGUIsettings$estimation[[modName]][[symb]][["method"]] <<- "L-BFGS-B"
          }
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]] <<- 1
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]] <<- NA
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]] <<- FALSE
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]] <<- TRUE
          if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]]))
            yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]] <<- NA
        }
      }
    }
    parametric_modal_prev_buttonDelta <<- input$advancedSettingsButtonApplyDelta
    parametric_modal_prev_buttonAllDelta <<- input$advancedSettingsButtonApplyAllDelta
  })
  
  observe({
    shinyjs::toggle(id="parametric_modal_body", condition = nrow(parametric_seriesToChangePoint$table)!=0)
    shinyjs::toggle(id="parametric_modal_errorMessage", condition = nrow(parametric_seriesToChangePoint$table)==0)
  })
  output$parametric_modal_series <- renderUI({
    if (nrow(parametric_seriesToChangePoint$table)!=0)
      selectInput(inputId = "parametric_modal_series", label = "Series", choices = rownames(parametric_seriesToChangePoint$table))
  })
  output$parametric_modal_delta <- renderUI({
    if (!is.null(input$parametric_modal_series))
      return (numericInput("parametric_modal_delta", label = paste("delta", input$parametric_modal_series), value = yuimaGUIsettings$delta[[input$parametric_modal_series]]))
  })
  output$parametric_modal_toLog <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series)){
      choices <- FALSE
      if (all(getData(input$parametric_modal_series)>0)) choices <- c(FALSE, TRUE)
      return (selectInput("parametric_modal_toLog", label = "Convert to log", choices = choices, selected = yuimaGUIsettings$toLog[[input$parametric_modal_series]]))
    }
  })
  output$parametric_modal_model <- renderUI({
    if(!is.null(input$parametric_changepoint_model))
      selectInput(inputId = "parametric_modal_model", label = "Model", choices = input$parametric_changepoint_model)
  })
  output$parametric_modal_parameter <- renderUI({
    if (!is.null(input$parametric_modal_model)){
      par <- setModelByName(input$parametric_modal_model, jumps = NA, AR_C = NA, MA_C = NA)@parameter@all
      selectInput(inputId = "parametric_modal_parameter", label = "Parameter", choices = par)
    }
  })
  output$parametric_modal_start <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
      numericInput(inputId = "parametric_modal_start", label = "start", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["start"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_startMin <- renderUI({
    input$parametric_modal_button_applyDelta
    input$parametric_modal_button_applyAllDelta
    if (!is.null(input$parametric_modal_start) & !is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
      if (is.na(input$parametric_modal_start))
        numericInput(inputId = "parametric_modal_startMin", label = "start: Min", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMin"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_startMax <- renderUI({
    input$parametric_modal_button_applyDelta
    input$parametric_modal_button_applyAllDelta
    if (!is.null(input$parametric_modal_start) & !is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
      if (is.na(input$parametric_modal_start))
        numericInput(inputId = "parametric_modal_startMax", label = "start: Max", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMax"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_lower <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
      if (input$parametric_modal_method=="L-BFGS-B" | input$parametric_modal_method=="Brent")
        numericInput("parametric_modal_lower", label = "lower", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["lower"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_upper <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
      if (input$parametric_modal_method=="L-BFGS-B" | input$parametric_modal_method=="Brent")
        numericInput("parametric_modal_upper", label = "upper", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["upper"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_method <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series))
      selectInput("parametric_modal_method", label = "method", choices = c("L-BFGS-B", "Nelder-Mead", "BFGS", "CG", "SANN", "Brent"), selected = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["method"]])
  })
  output$parametric_modal_trials <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_method))
      numericInput("parametric_modal_trials", label = "trials", min = 1, value = ifelse(input$parametric_modal_method=="SANN" & yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["method"]]!="SANN",1,yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["trials"]]))
  })
  output$parametric_modal_seed <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series))
      numericInput("parametric_modal_seed", label = "seed", min = 1, value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["seed"]])
  })
  
  
  
  observeEvent(input$parametric_modal_button_applyDelta, {
    yuimaGUIsettings$delta[[input$parametric_modal_series]] <<- input$parametric_modal_delta
    yuimaGUIsettings$toLog[[input$parametric_modal_series]] <<- input$parametric_modal_toLog
  })
  observeEvent(input$parametric_modal_button_applyAllDelta, {
    for (symb in rownames(parametric_seriesToChangePoint$table)){
      yuimaGUIsettings$delta[[symb]] <<- input$parametric_modal_delta
      if (input$parametric_modal_toLog==FALSE) yuimaGUIsettings$toLog[[symb]] <<- input$parametric_modal_toLog
      else if (all(getData(symb)>0)) yuimaGUIsettings$toLog[[symb]] <<- input$parametric_modal_toLog
    }
  })
  observeEvent(input$parametric_modal_button_applyModel,{
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["start"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_start
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMin"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMin
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMax"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMax
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["lower"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_lower
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["upper"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_upper
  })
  observeEvent(input$parametric_modal_button_applyAllModel,{
    for (symb in rownames(parametric_seriesToChangePoint$table)){
      yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["start"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_start
      yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["startMin"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMin
      yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["startMax"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMax
      yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["lower"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_lower
      yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["upper"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_upper
    }
  })
  observeEvent(input$parametric_modal_button_applyGeneral,{
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["method"]] <<- input$parametric_modal_method
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["trials"]] <<- input$parametric_modal_trials
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["seed"]] <<- input$parametric_modal_seed
  })
  observeEvent(input$parametric_modal_button_applyAllModelGeneral,{
    for (symb in rownames(parametric_seriesToChangePoint$table)){
      yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["method"]] <<- input$parametric_modal_method
      yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["trials"]] <<- input$parametric_modal_trials
      yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["seed"]] <<- input$parametric_modal_seed
    }
  })
  
  output$parametric_changepoint_symb <- renderUI({
    n <- names(yuimaGUIdata$cpYuima)
    if(length(n)!=0)
      selectInput("parametric_changepoint_symb", "Symbol", choices = sort(n), selected = last(n))  
  })

  ### Start Estimation
  observeEvent(input$parametric_changepoint_button_startEstimation, {
    if (length(rownames(parametric_seriesToChangePoint$table))!=0)
      closeAlert(session = session, alertId = "parametric_changepoint_alert_err")
      withProgress(message = 'Analyzing: ', value = 0, {
        errors <- c()
        for (symb in rownames(parametric_seriesToChangePoint$table)){
          incProgress(1/length(rownames(parametric_seriesToChangePoint$table)), detail = symb)
          test <- try(addCPoint(modelName = input$parametric_changepoint_model, 
                        symb = symb, 
                        fracL = input$parametric_modal_rangeFraction[1]/100,
                        fracR = input$parametric_modal_rangeFraction[2]/100,
                        from = as.character(parametric_seriesToChangePoint$table[symb, "From"]),
                        to = as.character(parametric_seriesToChangePoint$table[symb, "To"]),
                        delta = yuimaGUIsettings$delta[[symb]],
                        toLog = yuimaGUIsettings$toLog[[symb]],
                        start = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["start"]],
                        startMin = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["startMin"]],
                        startMax = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["startMax"]],
                        method = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["method"]],
                        trials = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["trials"]],
                        seed = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["seed"]],
                        lower = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["lower"]],
                        upper = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["upper"]]))
          if(class(test)=="try-error") 
            errors <- c(errors, symb)
        }
        if (!is.null(errors))
          createAlert(session = session, anchorId = "parametric_changepoint_alert", alertId = "parametric_changepoint_alert_err", style = "error", dismiss = TRUE, content = paste("Unable to estimate Change Point of:", paste(errors, collapse = " ")))
      })
  })
  
  parametric_range_changePoint <- reactiveValues(x=NULL, y=NULL)
  observe({
    if (!is.null(input$parametric_changePoint_brush) & !is.null(input$parametric_changepoint_symb)){
      data <- yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]]$series
      test <- (length(index(window(data, start = input$parametric_changePoint_brush$xmin, end = input$parametric_changePoint_brush$xmax))) > 3)
      if (test==TRUE){
        parametric_range_changePoint$x <- c(as.Date(input$parametric_changePoint_brush$xmin), as.Date(input$parametric_changePoint_brush$xmax))
        parametric_range_changePoint$y <- c(input$parametric_changePoint_brush$ymin, input$parametric_changePoint_brush$ymax)
      }
    }
  })
  
  observeEvent(input$parametric_changePoint_dbclick,{
    parametric_range_changePoint$x <- c(NULL, NULL)
  })
  
  observeEvent(input$parametric_changepoint_symb, {
    parametric_range_changePoint$x <- c(NULL, NULL)
    output$parametric_changepoint_plot_series <- renderPlot({
      if(!is.null(input$parametric_changepoint_symb)) {
        cp <- isolate({yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]]})
        data <- cp$series
        toLog <- cp$info$toLog
        symb <- cp$info$symb
        par(bg="black")
        plot(window(data, start = parametric_range_changePoint$x[1], end = parametric_range_changePoint$x[2]), main=ifelse(toLog==TRUE, paste("log(",symb,")", sep = ""), symb), xlab="Index", ylab=NA, log=switch(input$parametric_changepoint_scale,"Linear"="","Logarithmic (Y)"="y", "Logarithmic (X)"="x", "Logarithmic (XY)"="xy"), col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
        abline(v=cp$tau, col = "red")
        grid(col="grey")
      }
    })
  })
  
  
  output$parametric_changepoint_info <- renderUI({
    if(!is.null(input$parametric_changepoint_symb)){
      info <- isolate({yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]]$info})
      div(
        h3(info$model),
        withMathJax(printModelLatex(names = info$model, process = "Diffusion process")), br(),
        h4(
          em(paste("Change Point:", as.character(isolate({yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]]$tau}))))
        ),
        align="center"
      )
    }
  })
  
  output$parametric_changepoint_modal_info_text <- renderUI({
    info <- yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]]$info
    div(
      h3(input$parametric_changepoint_symb, " - " , info$model, class = "hModal"),
      h4(
        em("series to log:"), info$toLog, br(),
        em("method:"), info$method, br(),
        em("trials:"), info$trials, br(),
        em("seed:"), info$seed, br(), class = "hModal"
      ),
      align="center")
  })
  
  output$parametric_changepoint_modal_info_tableL <- renderTable(rownames = T, {
    cp <- yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]]
    tL <- summary(cp$qmleL)@coef
    for (i in 1:nrow(tL)){
      tL[i,"Estimate"] <- signifDigits(value = tL[i,"Estimate"], sd = tL[i,"Std. Error"])
      tL[i,"Std. Error"] <- signifDigits(value = tL[i,"Std. Error"], sd = tL[i,"Std. Error"])
    }
    return(tL)
  })
  
  output$parametric_changepoint_modal_info_tableR <- renderTable(rownames = T, {
    cp <- yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]]
    tR <- summary(cp$qmleR)@coef
    for (i in 1:nrow(tR)){
      tR[i,"Estimate"] <- signifDigits(value = tR[i,"Estimate"], sd = tR[i,"Std. Error"])
      tR[i,"Std. Error"] <- signifDigits(value = tR[i,"Std. Error"], sd = tR[i,"Std. Error"])
    }
    return(tR)
  })
  
  
  
  observeEvent(input$parametric_changepoint_button_delete_estimated, {
    yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]] <<- NULL
  })
  
  observeEvent(input$parametric_changepoint_button_deleteAll_estimated, {
    yuimaGUIdata$cpYuima <<- list()
  })

  
  
  
  
  
  
  ########################Lead Lag
  ########################
  ########################
  
  ###Display available data
  output$llag_table_select <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (yuimaGUItable$series)
  })
  
  ###Table of selected data to change point
  seriesToLeadLag <- reactiveValues(table=data.frame())
  
  ###Select Button
  observeEvent(input$llag_button_select, priority = 1, {
    if (length(input$llag_table_select_rows_selected)!=0){
      closeAlert(session, "llag_alert_select")
      if (nrow(seriesToLeadLag$table)==0)
        seriesToLeadLag$table <<- rbind(seriesToLeadLag$table, yuimaGUItable$series[rownames(yuimaGUItable$series)[input$llag_table_select_rows_selected[1]],])
      for (symb in rownames(yuimaGUItable$series)[input$llag_table_select_rows_selected]){
        if (class(index(yuimaGUIdata$series[[symb]]))==class(index(yuimaGUIdata$series[[rownames(seriesToLeadLag$table)[1]]]))){
          if (!(symb %in% rownames(seriesToLeadLag$table)))
            seriesToLeadLag$table <<- rbind(seriesToLeadLag$table, yuimaGUItable$series[symb,])
        } else {
          createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", append = FALSE, content = "Cannot analyze Lead-Lag for series with different type of index (numeric/date)", style = "warning")
        }
      }
    }
  })
  
  ###SelectAll Button
  observeEvent(input$llag_button_selectAll, priority = 1, {
    if (length(input$llag_table_select_rows_all)!=0){
      closeAlert(session, "llag_alert_select")
      if (nrow(seriesToLeadLag$table)==0)
        seriesToLeadLag$table <<- rbind(seriesToLeadLag$table, yuimaGUItable$series[rownames(yuimaGUItable$series)[input$llag_table_select_rows_all[1]],])
      for (symb in rownames(yuimaGUItable$series)[input$llag_table_select_rows_all]){
        if (class(index(yuimaGUIdata$series[[symb]]))==class(index(yuimaGUIdata$series[[rownames(seriesToLeadLag$table)[1]]]))){
          if (!(symb %in% rownames(seriesToLeadLag$table)))
            seriesToLeadLag$table <<- rbind(seriesToLeadLag$table, yuimaGUItable$series[symb,])
        } else {
          createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", append = FALSE, content = "Cannot analyze Lead-Lag for series with different type of index (numeric/date)", style = "warning")
        }
      }
    }
  })
  
  ###Display Selected Data
  output$llag_table_selected <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = FALSE, selection = "multiple",{
    if (length(rownames(seriesToLeadLag$table))==0){
      NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (seriesToLeadLag$table)
  })
  
  ###Control selected data to be in yuimaGUIdata$series
  observe({
    if(length(seriesToLeadLag$table)!=0){
      if (length(yuimaGUItable$series)==0)
        seriesToLeadLag$table <<- data.frame()
      else
        seriesToLeadLag$table <<- seriesToLeadLag$table[which(as.character(seriesToLeadLag$table[,"Symb"]) %in% as.character(yuimaGUItable$series[,"Symb"])),]
    }
  })
  
  ###Delete Button
  observeEvent(input$llag_button_delete, priority = 1,{
    if (!is.null(input$llag_table_selected_rows_selected))
      seriesToLeadLag$table <<- seriesToLeadLag$table[-input$llag_table_selected_rows_selected,]
  })
  
  ###DeleteAll Button
  observeEvent(input$llag_button_deleteAll, priority = 1,{
    if (!is.null(input$llag_table_selected_rows_all))
      seriesToLeadLag$table <<- seriesToLeadLag$table[-input$llag_table_selected_rows_all,]
  })
  
  observe({
    if (length(rownames(seriesToLeadLag$table))!=0){
      type <- try(class(index(yuimaGUIdata$series[[rownames(seriesToLeadLag$table)[1]]])[1]))
      if(type!="try-error"){
        shinyjs::toggle(id = "llag_range_date", condition = type=="Date")
        shinyjs::toggle(id = "llag_range_numeric", condition = type!="Date")
      }
    }
    else {
      shinyjs::show(id = "llag_range_date")
      shinyjs::hide(id = "llag_range_numeric")
    }
  })
  
  observe({
    shinyjs::toggle("llag_maxLag", condition = input$llag_type=="llag")
    shinyjs::toggle("llag_corr_method", condition = input$llag_type=="corr")
  })
  
  
  observeEvent(input$llag_button_startEstimation, {
    closeAlert(session, alertId = "llag_alert_select")
    if (is.na(input$llag_maxLag) | input$llag_maxLag <= 0)
      createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Lag max must be greater than zero", style = "warning")
    else {
      series <- rownames(seriesToLeadLag$table)
      if (length(series)<=1)
        createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Select at least two series", style = "warning")
      else {
        withProgress(message = "Calculating...",  value = 1, {
          data <- yuimaGUIdata$series[[series[1]]]
          type <- class(index(data)[1])
          for (i in 2:length(series))
            data <- merge(data, yuimaGUIdata$series[[series[i]]])
          colnames(data) <- series
          if(type=="Date") {
            start <- input$llag_range_date[1]
            end <- input$llag_range_date[2]
          } else {
            start <- input$llag_range_numeric1
            end <- input$llag_range_numeric2
          }
          data <- window(data, start = start, end = end)
          if(is.regular(data)){
            delta <- mode(na.omit(diff(index(data))))
            yuimaData <- setDataGUI(data, delta = delta)
            if(input$llag_type=="llag"){
              res <- try(llag(yuimaData, ci=TRUE, plot=FALSE, grid = seq(from = -input$llag_maxLag, to = input$llag_maxLag, by = delta)))
              if (class(res)=="try-error")
                createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Error in computing lead-lag", style = "error")
              else {
                i <- 1
                id <- "Lead-Lag Analysis"
                repeat {
                  if(id %in% names(yuimaGUIdata$llag)){
                    id <- paste("Lead-Lag Analysis", i)
                    i <- i+1
                  } else break
                }
                yuimaGUIdata$llag[[id]] <<- list(type = "llag", maxLag = input$llag_maxLag, delta = delta, llag = res$lagcce, p.values = res$p.values, start = start, end = end)
              }
            }
            if(input$llag_type=="corr"){
              if(input$llag_corr_method %in% c("pearson", "kendall", "spearman")){
                x <- as.matrix(yuimaData@original.data)
                res <- try(cor(x, method = input$llag_corr_method, use = "pairwise.complete.obs"))
              } 
              else 
                res <- try(cce(x = yuimaData, method = input$llag_corr_method)$cormat)
              if (class(res)=="try-error")
                createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Error in computing the correlation matrix", style = "error")
              else {
                i <- 1
                id <- "Correlation Analysis"
                repeat {
                  if(id %in% names(yuimaGUIdata$llag)){
                    id <- paste("Correlation Analysis", i)
                    i <- i+1
                  } else break
                }
                yuimaGUIdata$llag[[id]] <<- list(type = "corr", cormat = res, method = input$llag_corr_method, start = start, end = end)
              }
            }
          } else{
            createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Cannot analyze non-regular grid of observations", style = "error")
          }
        })
      }
    }
  })
  
  observe({
    shinyjs::toggle("llag_plot_body", condition = length(names(yuimaGUIdata$llag))!=0)
  })
  
  output$llag_analysis_id <- renderUI({
    n <- names(yuimaGUIdata$llag)
    if(length(n)!=0)
      selectInput("llag_analysis_id", label = "Analysis ID", choices = sort(n), selected = last(n))
  })
  
  output$llag_plot_corr_method <- renderUI({
    if(!is.null(input$llag_analysis_id)) if (input$llag_analysis_id %in% names(isolate({yuimaGUIdata$llag}))){
      info <- isolate({yuimaGUIdata$llag})[[input$llag_analysis_id]]
      if (info$type=="corr"){
        method <- switch(info$method,
                         "HY"="Hayashi-Yoshida", 
                         "PHY"="Pre-averaged Hayashi-Yoshida", 
                         "MRC"="Modulated Realized Covariance", 
                         "TSCV"="Two Scales realized CoVariance", 
                         "GME"="Generalized Multiscale Estimator", 
                         "RK"="Realized Kernel", 
                         "QMLE"="Quasi Maximum Likelihood Estimator", 
                         "SIML"="Separating Information Maximum Likelihood", 
                         "THY"="Truncated Hayashi-Yoshida", 
                         "PTHY"="Pre-averaged Truncated Hayashi-Yoshida", 
                         "SRC"="Subsampled Realized Covariance", 
                         "SBPC"="Subsampled realized BiPower Covariation")
        return(HTML(paste("<div><h4>&nbsp &nbsp Method:", method, "</h4></div>")))
      }
    }
  })
  
  observe({
    if(!is.null(input$llag_analysis_id)) if (input$llag_analysis_id %in% isolate({names(yuimaGUIdata$llag)})) {
      type <- isolate({yuimaGUIdata$llag})[[input$llag_analysis_id]]$type
      shinyjs::toggle("llag_plot_confidence", condition = type=="llag")
      shinyjs::toggle("llag_plot_corr_method", condition = type=="corr")   
      shinyjs::toggle("llag_plot_howToRead", condition = type=="llag")
    }
  })

  output$llag_plot <- renderPlot({
    if(!is.null(input$llag_analysis_id) & !is.null(input$llag_plot_confidence)) if (input$llag_analysis_id %in% isolate({names(yuimaGUIdata$llag)})) {
      info <- isolate({yuimaGUIdata$llag[[input$llag_analysis_id]]})
      if(info$type=="llag"){
        co <- ifelse(info$p.values > input$llag_plot_confidence | is.na(info$p.values), 0, info$llag)
        co<-melt(t(co))
        digits <- 1+as.integer(abs(log10(info$delta)))
      }
      if(info$type=="corr"){
        co <- info$cormat
        co<-melt(t(co))
        digits <- 2
      }
      fillColor <- switch(getOption("yuimaGUItheme"), "black"="#282828", "white"="#f0f4f5")
      textColor <- switch(getOption("yuimaGUItheme"), "black"="#CDCECD", "white"="black")
      ggplot(co, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
        geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
        geom_text(aes(label = round(co$value, digits))) + # write the values
        scale_fill_gradient2(low = "#ffa500", 
                             mid = switch(getOption("yuimaGUItheme"), "black"="gray30", "white"="#C7E2DF"), 
                             high = "#74d600", 
                             midpoint = 0) + # determine the colour
        theme(panel.grid.major.x=element_blank(), #no gridlines
              panel.grid.minor.x=element_blank(), 
              panel.grid.major.y=element_blank(), 
              panel.grid.minor.y=element_blank(),
              panel.background=element_rect(fill=fillColor), # background=white
              plot.background = element_rect(fill = fillColor, linetype = 0, color = fillColor),
              axis.text.x = element_text(angle=90,hjust = 1, size = 12,face = "bold", colour = textColor),
              plot.title = element_text(size=20,face="bold", colour = textColor, hjust = 0.5),
              axis.text.y = element_text(size = 12,face = "bold",  colour = textColor)) + 
        ggtitle(paste("Analyzed data from", info$start, "to", info$end)) + 
        theme(legend.title=element_text(face="bold", size=14)) + 
        scale_x_discrete(name="") +
        scale_y_discrete(name="") +
        labs(fill="")
    }
  })
  
  observeEvent(input$llag_delete_analysis, {
    yuimaGUIdata$llag[[input$llag_analysis_id]] <<- NULL
  })
  
  observeEvent(input$llag_deleteAll_analysis, {
    yuimaGUIdata$llag <<- list()
  })
  
  
  ########################Hedging
  ########################
  ########################
  
  hedging_databaseModels_table <- data.frame()
  output$hedging_databaseModels <- DT::renderDataTable(options=list(scrollY = 200, scrollX = TRUE, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
    if (length(yuimaGUItable$model)==0){
      NoData <- data.frame("Symb"=NA,"Please estimate some models first"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    date_indexed <- c()
    for (row in rownames(yuimaGUItable$model)){
      id <- unlist(strsplit(row, split = " "))
      if (class(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date")
        date_indexed <- c(date_indexed,row)
    }
    hedging_databaseModels_table <<- yuimaGUItable$model[rownames(yuimaGUItable$model) %in% date_indexed,]
    return (hedging_databaseModels_table)
  })
  
  output$hedging_assMarketPrice <- renderUI({
    if (is.null(input$hedging_databaseModels_rows_selected))
      numericInput("hedging_assMarketPrice", label="Asset Market Price:", value=NA, min = 0)
    else {
      if(input$hedging_databaseModels_row_last_clicked %in% input$hedging_databaseModels_rows_selected){
        id <- unlist(strsplit(rownames(hedging_databaseModels_table)[input$hedging_databaseModels_row_last_clicked], split = " "))
        numericInput("hedging_assMarketPrice", label="Asset Market Price:", value=as.numeric(tail(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data,1)), min = 0)
      }
      else
        numericInput("hedging_assMarketPrice", label="Asset Market Price:", value=NA, min = 0)
    }
  })

  output$hedging_strike <- renderUI({
    if (is.null(input$hedging_databaseModels_rows_selected))
      numericInput("hedging_strike", label="Strike Price:", value=0, min = 0, max = NA, step = NA, width = NULL)
    else {
      if(input$hedging_databaseModels_row_last_clicked %in% input$hedging_databaseModels_rows_selected){
        id <- unlist(strsplit(rownames(hedging_databaseModels_table)[input$hedging_databaseModels_row_last_clicked], split = " "))
        numericInput("hedging_strike", label="Strike Price:", value=as.numeric(tail(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data,1)), min = 0, max = NA, step = NA, width = NULL)
      }
      else
        numericInput("hedging_assMarketPrice", label="Asset Market Price:", value=NA, min = 0)
    }
  })
  
  observeEvent(input$hedging_button_startComputation, {
    closeAlert(session, "hedging_alert_selectRow")
    if (is.na(input$hedging_optMarketPrice)){
      createAlert(session, anchorId = "hedging_alert", alertId = "hedging_alert_selectRow", content = "Option market price is missing", style = "error")
      return()
    }
    if (input$hedging_optMarketPrice<=0){
      createAlert(session, anchorId = "hedging_alert", alertId = "hedging_alert_selectRow", content = "Option market price must be positive", style = "error")
      return()
    }
    if (!is.null(input$hedging_databaseModels_rows_selected) & !is.null(input$hedging_databaseModels_row_last_clicked)){
      if(input$hedging_databaseModels_row_last_clicked %in% input$hedging_databaseModels_rows_selected){
        modID <- rownames(hedging_databaseModels_table)[input$hedging_databaseModels_row_last_clicked]
        id <- unlist(strsplit(modID, split = " "))
        info = list(
          "maturity"= input$hedging_maturity,
          "strike"=input$hedging_strike, 
          "type"=input$hedging_type, 
          "optPrice"=input$hedging_optMarketPrice, 
          "optLotMult"=input$hedging_lotMult,
          "optLotCost"= ifelse(is.na(input$hedging_lotCostOpt), 0, input$hedging_lotCostOpt),
          "assPrice"=input$hedging_assMarketPrice,
          "assPercCost"= ifelse(is.na(input$hedging_percCostAss), 0, input$hedging_percCostAss/100),
          "assMinCost"= ifelse(is.na(input$hedging_minCostAss), 0, input$hedging_minCostAss),
          "assRateShortSelling"= ifelse(is.na(input$hedging_rateShort), 0, input$hedging_rateShort/100))
        addHedging(
          symbName = id[1],
          modelYuimaGUI = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]],
          info = info,
          xinit = input$hedging_assMarketPrice,
          nsim = input$hedging_nSim,
          nstep = NA,
          simulate.from = end(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data),
          simulate.to = input$hedging_maturity,
          session = session,
          anchorId = "hedging_alert"
        )
        updateTabsetPanel(session = session,  inputId = "panel_hedging", selected = "Profit&Loss")
      }
      else createAlert(session, anchorId = "hedging_alert", alertId = "hedging_alert_selectRow" , content = "Please select a model to simulate the evolution of the underlying asset", style = "error")
    }
    else createAlert(session, anchorId = "hedging_alert", alertId = "hedging_alert_selectRow", content = "Please select a model to simulate the evolution of the underlying asset", style = "error")
  })
  
  output$hedging_table_results <- DT::renderDataTable(options=list(scrollX=TRUE, scrollY = 200, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
    if (length(yuimaGUItable$hedging)==0){
      NoData <- data.frame("Symb"=NA, "Here will be stored simulations you run in the previous tab"=NA, check.names = FALSE)
      return(NoData[-1,])
    }
    return (yuimaGUItable$hedging)
  })
  
  
  
  
  
  hedging_values <- reactiveValues(profits=NULL, symb=NULL, model=NULL, return = NA)
  hedging_values2 <- reactiveValues(number_of_ids = 0)
  na_zero <- function(x){ifelse(is.na(x), 0, x)}
  
  observe({
    hedging_values2$number_of_ids <<- length(yuimaGUIdata$hedging)
    shinyjs::toggle(id = "hedging_button_show", condition = hedging_values2$number_of_ids>0)
  })
  
  output$hedging_modal_id <- renderUI({
    if(hedging_values2$number_of_ids>0) 
      selectInput("hedging_modal_id", label = "ID", choices = seq(1, hedging_values2$number_of_ids))
  })
  
  output$hedging_modal_id_hidden <- renderUI({
    selectInput("hedging_modal_id_hidden", label = "ID", choices = input$hedging_modal_id, selected = input$hedging_modal_id)
  })
  output$hedging_nAss_hedge <- renderUI({
    if (!is.null(input$hedging_modal_id)){
      id <- as.integer(input$hedging_modal_id)
      if (hedging_values2$number_of_ids>=id){
        info <- yuimaGUIdata$hedging[[id]]$info
        val <- switch (info$type, "call" = info$sell, "put" = info$buy)
        lab <- paste("Number of Assets to", ifelse(info$type=="call", "Sell", "Buy"))
        numericInput("hedging_nAss_hedge", label = lab, min = 0, value = val, step = 1)
      }
    }
  })
  output$hedging_nOptLot_hedge <- renderUI({
    if (!is.null(input$hedging_modal_id)){
      id <- as.integer(input$hedging_modal_id)
      if (hedging_values2$number_of_ids>=id){
        info <- yuimaGUIdata$hedging[[id]]$info
        nOpt <- info$LotsToBuy
        numericInput("hedging_nOptLot_hedge", label = "Option - number of Lots", min = 0, value = nOpt, step = 1)
      }
    }
  })
  output$hedging_type2 <- renderUI({
    if (!is.null(input$hedging_modal_id)){
      id <- as.integer(input$hedging_modal_id)
      if (hedging_values2$number_of_ids>=id){
        type <- yuimaGUIdata$hedging[[id]]$info$type
        selectInput("hedging_type2", width = "75%", label="Modify Type", c(Call="call", Put="put"), selected = type)
      }
    }
  })
  output$hedging_strike2 <- renderUI({
    if (!is.null(input$hedging_modal_id)){
      id <- as.integer(input$hedging_modal_id)
      if (hedging_values2$number_of_ids>=id){
        strike <- yuimaGUIdata$hedging[[id]]$info$strike
        numericInput("hedging_strike2", width = "75%", label = "Modify Strike", min = 0, value = strike)
      }
    }
  })
  output$hedging_optMarketPrice2 <- renderUI({
    if (!is.null(input$hedging_modal_id)){
      id <- as.integer(input$hedging_modal_id)
      if (hedging_values2$number_of_ids>=id){
        optPrice <- yuimaGUIdata$hedging[[id]]$info$optPrice
        numericInput("hedging_optMarketPrice2", width = "75%", label = "Modify Market Price", min = 0, value = optPrice)
      }
    }
  })
  
  observe({
    id <- input$hedging_modal_id_hidden
    if (!is.null(id) & !is.null(input$hedging_strike2) & !is.null(input$hedging_nAss_hedge)){
      id <- as.integer(id)
      if(hedging_values2$number_of_ids>=id){
        info <- yuimaGUIdata$hedging[[id]]$info
        profits <- profit_distribution(nOpt= na_zero(input$hedging_nOptLot_hedge)*info$optLotMult, 
                                       nAss= na_zero(input$hedging_nAss_hedge), 
                                       type=input$hedging_type2,
                                       strike=ifelse(is.na(input$hedging_strike2), info$strike, input$hedging_strike2),
                                       priceAtMaturity=yuimaGUIdata$hedging[[id]]$hist, 
                                       optMarketPrice=ifelse(is.na(input$hedging_optMarketPrice2), info$optPrice, input$hedging_optMarketPrice2),
                                       assMarketPrice=info$assPrice, 
                                       percCostAss=na_zero(input$hedging_percCostAss)/100, 
                                       minCostAss=na_zero(input$hedging_minCostAss), 
                                       lotCostOpt=na_zero(input$hedging_lotCostOpt), 
                                       lotMultiplier=info$optLotMult, 
                                       shortCostPerYear=na_zero(input$hedging_rateShort)/100, 
                                       t0=info$today, 
                                       maturity=info$maturity)
        hedging_values$profits <- profits
        hedging_values$symb <- yuimaGUIdata$hedging[[id]]$symb
        hedging_values$model <- yuimaGUIdata$hedging[[id]]$model$info$modName
      }
    }
  })
  
  output$hedging_plot_distribution <- renderPlot({
    par(bg="black")
    if (!is.null(hedging_values$profits) & !is.null(hedging_values$model) & !is.null(hedging_values$symb))
      hist(hedging_values$profits, main = paste(hedging_values$symb,"-",hedging_values$model), xlab = "Profit & Loss", breaks = input$hedging_slider_nBin, col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black", right = FALSE)
    grid()
  })
  output$hedging_quantiles_text <- renderUI({
    if(!is.null(input$hedging_slider_rangeHist) & !is.null(hedging_values$profits)){
      val <- hedging_values$profits
      qq <- quantile(val, probs = input$hedging_slider_rangeHist/100)
      HTML(paste("<div>", "Lower:", round(qq[1],0),"<br/>", "Upper: ", round(qq[2],0), "<br/>", "Mean: ", round(mean(val[val>=qq[1] & val<=qq[2]]),0), "</div>"))
    }
  })
  output$hedging_capital_text <- renderUI({
    if (!is.null(input$hedging_modal_id) & !is.null(hedging_values$profits)){
      id <- as.integer(input$hedging_modal_id)
      if (hedging_values2$number_of_ids>=id){
        info <- isolate({yuimaGUIdata$hedging[[id]]$info})
        optPrice <- ifelse(is.na(input$hedging_optMarketPrice2), info$optPrice, input$hedging_optMarketPrice2)
        percCostAss <- na_zero(input$hedging_percCostAss)/100
        minCostAss <- na_zero(input$hedging_minCostAss)
        lotCostOpt <- na_zero(input$hedging_lotCostOpt)
        nOptLot <- na_zero(input$hedging_nOptLot_hedge)
        nAss <- na_zero(input$hedging_nAss_hedge)
        cap <- nOptLot*(info$optLotMult*optPrice+lotCostOpt)+nAss*info$assPrice + ifelse(nAss>0,max(nAss*info$assPrice*percCostAss,minCostAss),0)
        val <- hedging_values$profits
        ret <- mean(val)/cap
        hedging_values$return <- ret
        HTML(paste("Invested Capital: ", round(cap,0), "<br/>", "Average Return: ", round(ret*100,2), "%"))
      }
    }
  })
  
    
  observeEvent(input$hedging_button_saveHedging, {
    id <- as.integer(input$hedging_modal_id)
    yuimaGUIdata$hedging[[id]]$info$assPercCost <<- ifelse(is.na(input$hedging_percCostAss), 0, input$hedging_percCostAss/100)
    yuimaGUIdata$hedging[[id]]$info$assMinCost <<- ifelse(is.na(input$hedging_minCostAss), 0, input$hedging_minCostAss)
    yuimaGUIdata$hedging[[id]]$info$assRateShortSelling <<- ifelse(is.na(input$hedging_rateShort), 0, input$hedging_rateShort/100)
    yuimaGUIdata$hedging[[id]]$info$optLotCost <<- ifelse(is.na(input$hedging_lotCostOpt), 0, input$hedging_lotCostOpt)
    yuimaGUIdata$hedging[[id]]$info$type <<- input$hedging_type2
    if (yuimaGUIdata$hedging[[id]]$info$type=="put"){
      yuimaGUIdata$hedging[[id]]$info$buy <<- na_zero(input$hedging_nAss_hedge)
      yuimaGUIdata$hedging[[id]]$info$sell <<- NA
    }
    if (yuimaGUIdata$hedging[[id]]$info$type=="call"){
      yuimaGUIdata$hedging[[id]]$info$sell <<- na_zero(input$hedging_nAss_hedge)
      yuimaGUIdata$hedging[[id]]$info$buy <<- NA
    }
    yuimaGUIdata$hedging[[id]]$info$LotsToBuy <<- na_zero(input$hedging_nOptLot_hedge)
    if (!is.na(input$hedging_strike2))
      yuimaGUIdata$hedging[[id]]$info$strike <<- input$hedging_strike2
    if (!is.na(input$hedging_optMarketPrice2))
      yuimaGUIdata$hedging[[id]]$info$optPrice <<- input$hedging_optMarketPrice2
    yuimaGUIdata$hedging[[id]]$info$profit <<- hedging_values$return
  })

  observe({
    shinyjs::toggle("hedging_alert_selectRow", condition = (input$panel_hedging=="Start simulations"))
  })
  
  ###Delete Hedging
  observeEvent(input$hedging_button_delete, priority = 1, {
    if(!is.null(input$hedging_table_results_rows_selected) & !is.null(input$hedging_modal_id)){
      if(input$hedging_table_results_row_last_clicked %in% input$hedging_table_results_rows_selected){
        delHedging(n=input$hedging_table_results_row_last_clicked)
      }
    }
  })
  
  ###DeleteAll Hedging
  observeEvent(input$hedging_button_deleteAll, priority = 1, {
    if(!is.null(input$hedging_table_results_rows_all))
      delHedging(n=input$hedging_table_results_rows_all)
  })
  
  
}




