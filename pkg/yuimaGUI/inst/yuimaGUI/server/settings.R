#shinyapp web app
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

yuimaGUItable <- reactiveValues(series=data.frame(),  
                                model=data.frame(), multimodel=data.frame(), 
                                simulation=data.frame(), multisimulation=data.frame(), 
                                hedging=data.frame())

yuimaGUIsettings <- list(simulation = list(), estimation = list(), delta = list(), toLog = list())


output$saveSession <- {
  downloadHandler(
    filename = "session.yuimaGUI",
    content = function(file) {
      save("yuimaGUIdata", file = file)
    }
  )
} 

observeEvent(input$loadSession, {
	if (!is.null(input$loadSession$datapath)){
		try(load(input$loadSession$datapath))
		for(i in names(yuimaGUIdata)) yuimaGUIdata[[i]] <<- yuimaGUIdata[[i]]
	}
})


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

observeEvent(yuimaGUIdata$multimodel, priority = 10, {
  yuimaGUItable$multimodel <<- data.frame()
  for (symb in names(yuimaGUIdata$multimodel)){
    for (i in 1:length(yuimaGUIdata$multimodel[[symb]])){
      newRow <- data.frame(
        Symb = symb,
        Class = yuimaGUIdata$multimodel[[symb]][[i]]$info$class,
        Model = yuimaGUIdata$multimodel[[symb]][[i]]$info$modName,
        Jumps = yuimaGUIdata$multimodel[[symb]][[i]]$info$jumps,
        From = as.character(start(yuimaGUIdata$multimodel[[symb]][[i]]$model@data@original.data)),
        To = as.character(end(yuimaGUIdata$multimodel[[symb]][[i]]$model@data@original.data)),
        AIC = yuimaGUIdata$multimodel[[symb]][[i]]$aic,
        BIC = yuimaGUIdata$multimodel[[symb]][[i]]$bic,
        stringsAsFactors = FALSE)
      rownames(newRow) <- as.character(paste(symb," ", i, sep=""))
      yuimaGUItable$multimodel <<- rbind(yuimaGUItable$multimodel, newRow)
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



observeEvent(yuimaGUIdata$multisimulation, priority = 10, {
  yuimaGUItable$multisimulation <<- data.frame()
  for (symb in names(yuimaGUIdata$multisimulation)){
    for (i in 1:length(yuimaGUIdata$multisimulation[[symb]])){
      estimated.from <- NA
      estimated.to <- NA
      if (!is.null(yuimaGUIdata$multisimulation[[symb]][[i]]$model$model@data@original.data)){
        estimated.from <- as.character(start(yuimaGUIdata$multisimulation[[symb]][[i]]$model$model@data@original.data))
        estimated.to <- as.character(end(yuimaGUIdata$multisimulation[[symb]][[i]]$model$model@data@original.data))
      }
      newRow <- data.frame(
        "Symb" = symb,
        "Class" = yuimaGUIdata$multisimulation[[symb]][[i]]$model$info$class,
        "Model" = yuimaGUIdata$multisimulation[[symb]][[i]]$model$info$modName,
        "Jumps" = yuimaGUIdata$multisimulation[[symb]][[i]]$model$info$jumps,
        "N sim" = yuimaGUIdata$multisimulation[[symb]][[i]]$info$nsim,
        "N step" = yuimaGUIdata$multisimulation[[symb]][[i]]$info$nstep,
        "delta" = yuimaGUIdata$multisimulation[[symb]][[i]]$info$delta,
        "Simulated from" = as.character(yuimaGUIdata$multisimulation[[symb]][[i]]$info$simulate.from),
        "Simulated to" = as.character(yuimaGUIdata$multisimulation[[symb]][[i]]$info$simulate.to),
        "Estimated from" = estimated.from,
        "Estimated to" = estimated.to,
        check.names = FALSE, stringsAsFactors = FALSE)
      rownames(newRow) <- as.character(paste(symb," ", i, sep=""))
      yuimaGUItable$multisimulation <<- rbind(yuimaGUItable$multisimulation, newRow)
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
                    "Point Process" = "Hawkes",
					"Point Process" = "Hawkes Power Law Kernel",
                    #"Fractional process"="Frac. Geometric Brownian Motion",
                    #"Fractional process"="Frac. Brownian Motion",
                    "Fractional process"="Frac. Ornstein-Uhlenbeck (OU)",
                    "CARMA" = "Carma(p,q)",
                    "COGARCH" = "Cogarch(p,q)",
                    "Levy process" = "Geometric Brownian Motion with Jumps"
)

defaultMultiModels <-  c("Diffusion process" = "Correlated Brownian Motion")

defaultJumps <- c("Gaussian",
				  "Constant",
                  "Uniform", 
                  "Student t", 
                  "Variance Gamma", 
                  "Inverse Gaussian", 
                  "Normal Inverse Gaussian", 
                  "Hyperbolic", 
                  "Generalized Hyperbolic")
