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
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}


isUserDefined <- function(name){
	n <- names(isolate({yuimaGUIdata$usr_model}))
	if (length(n)!=0) return (name %in% n)
	return (FALSE)
}

setDataGUI <- function(original.data, delta){
  original.data <- na.omit(original.data)
  delta <- max(delta)
  t <- index(original.data)
  t0 <- 0
  if(is.numeric(t)){
    delta.original.data <- mode(diff(t))
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


getData <- function(symb){
  return(isolate({yuimaGUIdata$series[[symb]]}))
}

delData <- function(symb){
  for (i in symb)
    yuimaGUIdata$series <<- yuimaGUIdata$series[-which(names(yuimaGUIdata$series)==i)]
}


defaultBounds <- function(name, delta, strict, jumps = NA, AR_C = NA, MA_C = NA, data, intensity = NULL, threshold = NULL){
  lastPrice = as.numeric(last(data))
  if ( isUserDefined(name) ){
    mod <- setModelByName(name = name, jumps = jumps,  AR_C = AR_C, MA_C = MA_C)
	  par <- getAllParams(mod, yuimaGUIdata$usr_model[[name]]$class)
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
  if (name == "Hawkes"){
    if (strict==TRUE) return (list(lower=list("nu1"=0, "c11"=0, "a11"=0), upper=list("nu1"=NA, "c11"=100, "a11"=NA)))
    else { 
      x <- as.numeric(diff(data))
      t1 <- tail(time(data),n=1)
      t0 <- time(data)[1]
      n <- length(x[x!=0])
      nu1 <- n/as.numeric(t1-t0)
      c11 <- 0
      a11 <- 1
      return (list(lower=list("nu1"=nu1, "c11"=c11, "a11"=a11), upper=list("nu1"=nu1, "c11"=c11, "a11"=a11)))
    }
  }
  if (name %in% defaultModels[names(defaultModels) == "COGARCH"]){
    mod <- setModelByName(name = name, jumps = jumps,  AR_C = AR_C, MA_C = MA_C)
    par <- getAllParams(mod, "COGARCH")
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
    mod <- setModelByName(name = name, jumps = jumps,  AR_C = AR_C, MA_C = MA_C)
	  par <- getAllParams(mod, "CARMA")
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
    else {
      x <- as.numeric(diff(data))
      counts <- length(x[x!=0 & !is.na(x)])
      alpha <- counts/(length(x)*delta)
      return(list(lower=c(list("alpha"=0, "beta"=0), boundsJump$lower),upper=c(list("alpha"=alpha, "beta"=0), boundsJump$upper)))
    }
  }
  if (name == "Linear Intensity"){
    boundsJump <- jumpBounds(jumps = jumps, strict = strict, data = data)
    if (strict==TRUE) return(list(lower=c(list("alpha"=0, "beta"=0), boundsJump$lower),upper=c(list("alpha"=NA, "beta"=NA), boundsJump$upper)))
    else {
      x <- as.numeric(diff(data))
      counts <- length(x[x!=0 & !is.na(x)])
      alpha <- counts/(length(x)*delta)
      return(list(lower=c(list("alpha"=0, "beta"=0), boundsJump$lower),upper=c(list("alpha"=alpha, "beta"=0), boundsJump$upper)))
    }
  }
  if (name == "Exponentially Decaying Intensity"){
    boundsJump <- jumpBounds(jumps = jumps, strict = strict, data = data)
    if (strict==TRUE) return(list(lower=c(list("alpha"=0, "beta"=0), boundsJump$lower),upper=c(list("alpha"=NA, "beta"=NA), boundsJump$upper)))
    else {
      x <- as.numeric(diff(data))
      counts <- length(x[x!=0 & !is.na(x)])
      alpha <- counts/(length(x)*delta)
      return(list(lower=c(list("alpha"=0, "beta"=0), boundsJump$lower),upper=c(list("alpha"=alpha, "beta"=0), boundsJump$upper)))
    }
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
  if (name == "Correlated Brownian Motion"){
    mod <- setModelByName(name = name, jumps = jumps,  AR_C = AR_C, MA_C = MA_C, dimension = ncol(data))
	par <- getAllParams(mod, "Diffusion process", FALSE)
    drift <- rep(NA, length(par@drift))
    diffusion <- rep(NA, length(par@diffusion))
    names(drift) <- par@drift
    names(diffusion) <- par@diffusion
    if (strict==TRUE) {
      diffusion[] <- 0; lower_diffusion <- diffusion
      diffusion[] <- NA; upper_diffusion <- diffusion
      drift[] <- NA; lower_drift <- drift
      drift[] <- NA; upper_drift <- drift
      return (list(lower=as.list(c(lower_drift, lower_diffusion)), upper=as.list(c(upper_drift, upper_diffusion))))
    }
    else { 
      x <- na.omit(diff(data))
      mu <- colMeans(x)
      sigma <- sapply(x, sd)
      drift[] <- mu/delta; lower_drift <- drift; upper_drift <- drift
      diffusion[] <- 0; diffusion[paste("s",seq(1,ncol(data)),seq(1,ncol(data)), sep = "")] <- sigma/sqrt(delta); lower_diffusion <- diffusion; upper_diffusion <- diffusion
      return (list(lower=as.list(c(lower_drift, lower_diffusion)), upper=as.list(c(upper_drift, upper_diffusion))))
    }
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
  if(jumps=='Gaussian') {
    return(list("dnorm(z, mean = mu_jump, sd = sigma_jump)"))
  }
  if(jumps=='Constant') {
    return(list("dconst(z, k = k_jump)"))
  }
  if(jumps=='Uniform') {
    return(list("dunif(z, min = a_jump, max = b_jump)"))
  }
  if(jumps=='Inverse Gaussian') {
    return(list("dIG(z, delta = delta_jump, gamma = gamma_jump)"))
  }
  if(jumps=='Normal Inverse Gaussian') {
    return(list("dNIG.gui(z, alpha = alpha_jump, beta = beta_jump, delta = delta_jump, mu = mu_jump)"))
  }
  if(jumps=='Hyperbolic') {
    return(list("dhyp.gui(z, alpha = alpha_jump, beta = beta_jump, delta = delta_jump, mu = mu_jump)"))
  }
  if(jumps=='Student t') {
    return(list("dt(z, df = nu_jump, ncp = mu_jump)"))
  }
  if(jumps=='Variance Gamma') {
    return(list("dVG.gui(z, lambda = lambda_jump, alpha = alpha_jump, beta = beta_jump, mu = mu_jump)"))
  }
  if(jumps=='Generalized Hyperbolic') {
    return(list("dghyp.gui(z, lambda = lambda_jump, alpha = alpha_jump, delta = delta_jump, beta = beta_jump, mu = mu_jump)"))
  }
}

jumpBounds <- function(jumps, data, strict, threshold = 0){
  x <- na.omit(as.numeric(diff(data)))
  x <- x[abs(x)>threshold]
  x <- x-sign(x)*threshold
  switch(jumps,
         "Gaussian" = {
           if(strict==TRUE) return(list(lower=list("mu_jump"=NA, "sigma_jump"=0), upper=list("mu_jump"=NA, "sigma_jump"=NA)))
           else {
             mu <- mean(x)
             s <- sd(x)
             return(list(lower=list("mu_jump"=mu, "sigma_jump"=s), upper=list("mu_jump"=mu, "sigma_jump"=s)))
           }
         },
         "Uniform" = {
           if(strict==TRUE) return(list(lower=list("a_jump"=NA, "b_jump"=NA), upper=list("a_jump"=NA, "b_jump"=NA)))
           else {
             a <- min(x)
             b <- max(x)
             return(list(lower=list("a_jump"=a, "b_jump"=b), upper=list("a_jump"=a, "b_jump"=b)))
           }
         },
		 "Constant" = {
           if(strict==TRUE) return(list(lower=list("k_jump"=NA), upper=list("k_jump"=NA)))
           else {
             k <- median(x)
             return(list(lower=list("k_jump"=k), upper=list("k_jump"=k)))
           }
         },
         "Inverse Gaussian" = {
           if(strict==TRUE) return(list(lower=list("delta_jump"=NA, "gamma_jump"=NA), upper=list("delta_jump"=NA, "gamma_jump"=NA)))
           else {
             x <- x[x>0]
             delta <- mean(x)
             gamma <- delta^3/var(x)
             return(list(lower=list("delta_jump"=delta, "gamma_jump"=gamma), upper=list("delta_jump"=delta, "gamma_jump"=gamma)))
           }
         },
         "Normal Inverse Gaussian" = {
           if(strict==TRUE) return(list(lower=list("alpha_jump"=0, "beta_jump"=NA, "delta_jump"=0, "mu_jump"=NA), upper=list("alpha_jump"=NA, "beta_jump"=NA, "delta_jump"=NA, "mu_jump"=NA)))
           else {
             fit <- try(coef(fit.NIGuv(x), type = 'alpha.delta'))
             if(class(fit)!='try-error'){
               alpha <- fit$alpha
               beta <- fit$beta
               delta <- fit$delta
               mu <- fit$mu
             } else {
               alpha <- 1.5
               beta <- 0
               delta <- 1
               mu <- mean(x)
             }
             return(list(lower=list("alpha_jump"=alpha, "beta_jump"=beta, "delta_jump"=delta, "mu_jump" = mu), upper=list("alpha_jump"=alpha, "beta_jump"=beta, "delta_jump"=delta, "mu_jump" = mu)))
           }
         },
         "Hyperbolic" = {
           if(strict==TRUE) return(list(lower=list("alpha_jump"=NA, "beta_jump"=NA, "delta_jump"=NA, "mu_jump"=NA), upper=list("alpha_jump"=NA, "beta_jump"=NA, "delta_jump"=NA, "mu_jump"=NA)))
           else {
             fit <- try(coef(fit.hypuv(x), type = 'alpha.delta'))
             if(class(fit)!='try-error'){
               alpha <- fit$alpha
               beta <- fit$beta
               delta <- fit$delta
               mu <- fit$mu
             } else {
               alpha <- 1.5
               beta <- 0
               delta <- 1
               mu <- mean(x)
             }
             return(list(lower=list("alpha_jump"=alpha, "beta_jump"=beta, "delta_jump"=delta, "mu_jump" = mu), upper=list("alpha_jump"=alpha, "beta_jump"=beta, "delta_jump"=delta, "mu_jump" = mu)))
           }
         },
         "Student t" = {
           if(strict==TRUE) return(list(lower=list("nu_jump"=0, "mu_jump"=NA), upper=list("nu_jump"=NA, "mu_jump"=NA)))
           else {
             mu <- mean(x)
             nu <- 1
             return(list(lower=list("nu_jump"=nu, "mu_jump" = mu), upper=list("nu_jump"=nu, "mu_jump" = mu)))
           }
         },
         "Variance Gamma" = {
           if(strict==TRUE) return(list(lower=list("lambda_jump"=0, "alpha_jump"=NA, "beta_jump"=NA, "mu_jump"=NA), upper=list("lambda_jump"=NA, "alpha_jump"=NA, "beta_jump"=NA, "mu_jump"=NA)))
           else {
             fit <- try(coef(fit.VGuv(x), type = 'alpha.delta'))
             if(class(fit)!='try-error'){
               lambda <- fit$lambda
               alpha <- fit$alpha
               beta <- fit$beta
               mu <- fit$mu
             } else {
               lambda <- 1
               alpha <- 1.5
               beta <- 0
               mu <- mean(x)
             }
             return(list(lower=list("lambda_jump"=lambda, "alpha_jump"=alpha, "beta_jump"=beta, "mu_jump" = mu), upper=list("lambda_jump"=lambda, "alpha_jump"=alpha, "beta_jump"=beta, "mu_jump" = mu)))
           }
         },
         "Generalized Hyperbolic" = {
           if(strict==TRUE) return(list(lower=list("lambda_jump"=NA, "alpha_jump"=NA, "delta_jump"=NA, "beta_jump"=NA, "mu_jump"=NA), upper=list("lambda_jump"=NA, "alpha_jump"=NA, "delta_jump"=NA, "beta_jump"=NA, "mu_jump"=NA)))
           else {
             fit <- try(coef(fit.ghypuv(x), type = 'alpha.delta'))
             if(class(fit)!='try-error'){
               lambda <- fit$lambda
               alpha <- fit$alpha
               delta <- fit$delta
               beta <- fit$beta
               mu <- fit$mu
             } else {
               lambda <- 0.5
               alpha <- 1.5
               delta <- 1
               beta <- 0
               mu <- mean(x)
             }
             return(list(lower=list("lambda_jump"=lambda, "alpha_jump"=alpha, "delta_jump"=delta, "beta_jump"=beta, "mu_jump" = mu), upper=list("lambda_jump"=lambda, "alpha_jump"=alpha, "delta_jump"=delta, "beta_jump"=beta, "mu_jump" = mu)))
           }
         }
  )
}

latexJumps <- function(jumps){
  if (!is.null(jumps)){
    switch (jumps,
		"Gaussian" = "Y_i \\sim N(\\mu_{jump}, \\; \\sigma_{jump})",
		"Constant" = "Y_i = k_{jump}",
		"Uniform" = "Y_i \\sim Unif(a_{jump}, \\; b_{jump})",
		"Inverse Gaussian" = "Y_i \\sim IG(\\delta_{jump}, \\; \\gamma_{jump})",
		"Normal Inverse Gaussian" = "Y_i \\sim NIG( \\alpha_{jump}, \\; \\beta_{jump}, \\; \\delta_{jump}, \\; \\mu_{jump})",
		"Hyperbolic" = "Y_i \\sim HYP( \\alpha_{jump}, \\; \\beta_{jump}, \\; \\delta_{jump}, \\; \\mu_{jump})",
		"Student t" = "Y_i \\sim t( \\nu_{jump}, \\; \\mu_{jump} )",
		"Variance Gamma" = "Y_i \\sim VG( \\lambda_{jump}, \\; \\alpha_{jump}, \\; \\beta_{jump}, \\; \\mu_{jump})",
		"Generalized Hyperbolic" = "Y_i \\sim GH( \\lambda_{jump}, \\; \\alpha_{jump}, \\; \\beta_{jump}, \\; \\delta_{jump}, \\; \\mu_{jump})"
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


setModelByName <- function(name, jumps = NA, AR_C = NA, MA_C = NA, XinExpr = FALSE, intensity = NA, dimension = 1){
  dimension <- max(1, dimension)
  if ( isUserDefined(name) ){
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
  if (name == "Hawkes") return(yuima::setHawkes())
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
  if (name == "Correlated Brownian Motion") {
    mat <- matrix(rep(1:dimension, dimension),dimension,dimension)
    diff <- matrix(paste("s",mat,t(mat),sep=""), dimension, dimension)
    diff[lower.tri(diff, diag = FALSE)] <- 0
    return(yuima::setModel(drift=paste("mu", seq(1,dimension), sep = ""), diffusion=diff, solve.variable = paste("x", seq(1,dimension))))
  }
}

getAllParams <- function(mod, class, all = TRUE){
  if(is(mod)=='yuima' & class!="Point Process") mod <- mod@model
  
	if(all==TRUE){
		if (class=="Point Process")
			return(mod@PPR@allparamPPR)
		else if (class=="Fractional process")
			return(c(mod@parameter@all, "hurst"))
		else if (class=="COGARCH")
			return(c(mod@parameter@drift, mod@parameter@xinit))
		else if (class=="CARMA")
			return(mod@parameter@drift)
		else 
			return(mod@parameter@all)
	} else {
		if (class=="Point Process")
			return(mod@PPR)
		else 
			return(mod@parameter)
	}
  
}

printModelLatex <- function(names, process, jumps = NA, multi = FALSE, dimension = 1, symb = character(0)){
  dimension <- max(dimension, 1)
  if(length(symb)>0) dimension <- length(symb)
  if (multi==TRUE){
    if (process=="Diffusion process"){
      text <- toLatex(setModelByName(names, dimension = dimension))
      x <- paste(text[-1], collapse = "")
      if(length(symb)>0) for (i in 1:dimension) {
        x <- gsub(x, pattern = paste("x", i), replacement = paste("X_{", symb[i], "}", sep = ""))
      } else 
        x <- gsub(x, pattern = "x ", replacement = "X_")
        x <- gsub(x, pattern = "dW", replacement = "dW_")
        x <- gsub(x, pattern = "\\$\\$\\$\\$.*", replacement = "$$")
        return(x)
    }
  } else {
    if (process=="Diffusion process"){
      mod <- ""
      for (name in names){
        if ( isUserDefined(name) ){
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
        if ( isUserDefined(name) ){
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
    if (process=="Point Process"){
      mod <- "\\lambda_t = \\nu_1+\\int_{0}^{t_-}kern(t-s)\\mbox{d}N_s"
      for (name in names){
        if ( isUserDefined(name) ){
        
        }
        if (name == "Hawkes") mod <- paste(mod, ifelse(mod=="","","\\\\"), "kern(t-s) = c_{11}\\exp\\left[-a_{11}\\left(t-s\\right)\\right]")
      }
      return(paste("$$",mod,"$$"))
    }
    if (process=="Compound Poisson"){
        mod <- paste("X_t = X_0+\\sum_{i=0}^{N_t} Y_i \\; : \\;\\;\\;  N_t \\sim Poi\\Bigl(\\int_0^t \\lambda(t)dt\\Bigl)", ifelse(!is.null(jumps), paste(", \\;\\;\\;\\; ", latexJumps(jumps)),""))
      for (name in names){
        if ( isUserDefined(name) ){
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
    dt1 <- as.numeric(end(original.data) - start(original.data))/(length(index(original.data))-1)
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
  if (modelName %in% c("Correlated Brownian Motion")){
    if(startsWith(paramName, "mu")) return(list("Estimate"= param*delta/dt1, "Std. Error"=StdErr*delta/dt1, "msg"=msg))
    if(startsWith(paramName, "s")) return(list("Estimate"= param*sqrt(delta/dt1), "Std. Error"=StdErr*sqrt(delta/dt1), "msg"=msg))
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

changeBase <- function(table, yuimaGUI, newBase, session = session, choicesUI, anchorId, alertId){
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

addModel <- function(timeout = Inf, modName, multi = FALSE, intensity_levy, modClass, AR_C, MA_C, jumps, symbName, data, toLog, delta, start, startMin, startMax, trials, seed, method="BFGS", fixed = list(), lower, upper, joint=FALSE, aggregation=TRUE, threshold=NULL, session, anchorId, alertId){
  info <- list(
    symb = names(data),
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
  for (i in 1:length(toLog)) if(toLog[i]==TRUE) {
    tmp <- try(log(data[,i]))
    if(class(data)!="try-error")
      data[,i] <- tmp
    else {
      createAlert(session = session, anchorId = anchorId, alertId = alertId, content =  paste("Cannot convert series ", symbName, "to log. Try to use 'Advanced Settings' and customize estimation.", sep = ""), style = "error")
      return()
    }
  }
  if(modClass=='Point Process'){
  	model <- setModelByName(name = modName, dimension = ncol(data), intensity = intensity_levy, jumps = jumps, MA_C = MA_C, AR_C = AR_C)
  	t1 <- tail(time(data),n=1)
  	t0 <- time(data)[1]
  	if(!is.numeric(t0) | !is.numeric(t1)){
  	  t0 <- 0
  	  t1 <- as.numeric(t1-t0)/365
  	}
  	samp <- setSampling(t0, t1, n = as.integer(as.numeric(t1-t0)/delta)+1)
  	colnames(data) <- model@model@solve.variable
  	model <- DataPPR(CountVar = data, yuimaPPR = model, samp = samp)
  } else { 
	model <- try(setYuima(data = setDataGUI(data, delta = delta), model=setModelByName(name = modName, dimension = ncol(data), intensity = intensity_levy, jumps = jumps, MA_C = MA_C, AR_C = AR_C)))
  }
  if (class(model)=="try-error"){
    createAlert(session = session, anchorId = anchorId, alertId = alertId, content =  "Unable to construct a synchronous grid for the data provided", style = "error")
    return()
  }
  #index(model@data@original.data) <- index(na.omit(data))
  parameters <- getAllParams(model, modClass)
  
  
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
    if (all(parameters %in% c(names(start),names(fixed))))
      QMLE <- try(qmleGUI(model, start = start, method = method, lower = lower, upper = upper))
    else {
      miss <- parameters[!(parameters %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:trials){
          setTimeLimit(cpu = timeout, transient = TRUE)
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
    if (all(parameters %in% c(names(start),names(fixed))))
      QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #REMOVE# joint = joint, aggregation = aggregation,
                       threshold = threshold, grideq = TRUE, rcpp = TRUE))
    else {
      miss <- parameters[!(parameters %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:trials){
          setTimeLimit(cpu = timeout, transient = TRUE)
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
    if (all(parameters %in% c(names(start),names(fixed))))
      QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #REMOVE# joint = joint, aggregation = aggregation,
                       threshold = threshold))
    else {
      miss <- parameters[!(parameters %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:trials){
          setTimeLimit(cpu = timeout, transient = TRUE)
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
    if (all(parameters %in% c(names(start),names(fixed))))
      QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #REMOVE# joint = joint, aggregation = aggregation,
                       threshold = threshold))
    else {
      miss <- parameters[!(parameters %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:trials){
          setTimeLimit(cpu = timeout, transient = TRUE)
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
    if (all(parameters %in% c(names(start),names(fixed))))
      QMLE <- try(qmle(model, start = start, fixed = fixed, method = method, lower = lower, upper = upper, #REMOVE# joint = joint, aggregation = aggregation,
                       threshold = threshold, rcpp = TRUE))
    else {
      miss <- parameters[!(parameters %in% c(names(start),names(fixed)))]
      m2logL_prec <- NA
      na_prec <- NA
      withProgress(message = 'Step: ', value = 0, {
        for(iter in 1:trials){
          setTimeLimit(cpu = timeout, transient = TRUE)
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
  
  if(multi==FALSE)
    yuimaGUIdata$model[[symbName]][[ifelse(is.null(length(yuimaGUIdata$model[[symbName]])),1,length(yuimaGUIdata$model[[symbName]])+1)]] <<- list(
      model = model,
      qmle = QMLE,
      aic = ifelse(!(modClass %in% c("CARMA","COGARCH","Fractional process")), AIC(QMLE), NA),
      bic = ifelse(!(modClass %in% c("CARMA","COGARCH","Fractional process")), BIC(QMLE), NA),
      info = info
    )
  else 
    yuimaGUIdata$multimodel[[symbName]][[ifelse(is.null(length(yuimaGUIdata$multimodel[[symbName]])),1,length(yuimaGUIdata$multimodel[[symbName]])+1)]] <<- list(
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
  par <- getAllParams(mod, "Diffusion process")
  miss <- par[!(par %in% names(start))]
  m2logL_prec <- NA
  na_prec <- NA
  
  qmleL <- function(yuima, t, start, method, lower , upper , rcpp){
    yuima@data@zoo.data[[1]] <- window(yuima@data@zoo.data[[1]], end = t)
    qmle(yuima = yuima, start = start, method = method, upper = upper, lower = lower, rcpp = rcpp)
  }
  qmleR <- function(yuima, t, start, method, lower , upper , rcpp){
    yuima@data@zoo.data[[1]] <- window(yuima@data@zoo.data[[1]], start = t)
    qmle(yuima = yuima, start = start, method = method, upper = upper, lower = lower, rcpp = rcpp)
  }
  
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


delMultiModel <- function(symb, n=1){
  for(i in length(symb):1){
    yuimaGUIdata$multimodel[[symb[i]]][as.numeric(n[i])] <<- NULL
    if (length(yuimaGUIdata$multimodel[[symb[i]]])==0)
      yuimaGUIdata$multimodel[[symb[i]]] <<- NULL
  }
}

simulateGUI <- function(symbName, modelYuimaGUI, xinit, nsim, nstep, simulate.from, simulate.to, saveTraj, space.discretized, method, session, anchorId, alertId = NULL, true.parameter = NULL){
  modelYuima <- modelYuimaGUI$model
  model <- modelYuima@model
  if(is.null(modelYuimaGUI$info$toLog)) toLog <- FALSE else toLog <- modelYuimaGUI$info$toLog
  if(simulate.from >= simulate.to){
    createAlert(session = session, anchorId = anchorId, alertId = alertId, content = paste("Unable to simulate ", symbName," by ", modelYuimaGUI$info$modName, ": ending time before starting time.", sep = ""), style = "danger")
    return()
  }
  if(!is.null(names(xinit))) seriesnames <- names(xinit) else seriesnames <- model@solve.variable
  xinit <- as.numeric(xinit)
  xinit[toLog==TRUE] <- log(xinit[toLog==TRUE])
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
    if (modelYuimaGUI$info$class %in% c("COGARCH", "CARMA") | !is.numeric(nstep))
      nstep <- (Terminal-Initial)/used_delta
    sampling <- setSampling(Initial = Initial, Terminal = Terminal, n = nstep)
  } else {
    convert <- FALSE
    sampling <- setSampling(Initial = simulate.from, Terminal = simulate.to, n = nstep)
  }
  
  if(nsim*sampling@n*length(xinit) > 1000*252*2) saveTraj <- FALSE
  
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
      else if (modelYuimaGUI$info$class=="Point Process")
        simulation <- try(yuima::simulate(object = modelYuima, xinit = xinit, true.parameter = true.parameter, sampling = sampling, space.discretized = space.discretized, method = method))
      else
        simulation <- try(yuima::simulate(object = model, xinit = xinit, true.parameter = true.parameter, sampling = sampling, space.discretized = space.discretized, method = method))
      if (class(simulation)=="try-error"){
        is.valid <- FALSE
        break()
      }
      else {
        dimension <- length(simulation@data@zoo.data)
        if (modelYuimaGUI$info$class=="COGARCH") dimension <- dimension - 2
        if (saveTraj==TRUE){
          x <- do.call(merge,simulation@data@zoo.data)
          if(i==1) {
            timeindex <- index(x)
            x <- as.matrix(x)
            trajectory <- matrix(nrow = nrow(x), ncol = nsim*dimension)
            colnames(trajectory) <- seq(1:ncol(trajectory))
            hist <- NA 
          }
          else 
            x <- as.matrix(x)
          x[,toLog==TRUE] <- exp(x[,toLog==TRUE])
          if(any( is.na(x) | !is.finite(x) )){
            is.valid <- FALSE
            break()
          }
          colindex <- seq(1+(i-1)*dimension, i*dimension)
          trajectory[,colindex] <- x[,1:dimension]
          colnames(trajectory)[colindex] <- paste(seriesnames[1:dimension], i, sep = "_sim")
        } else {
          x <- do.call(c, lapply(simulation@data@zoo.data, FUN = function(x) as.numeric(last(x))))
          if(i==1) {
            trajectory <- NA
            hist <- matrix(nrow = dimension, ncol = nsim, dimnames = list(seriesnames[1:dimension]))
          }
          hist[,i] <- x
        }
      }
    }
  })
  
  if (!is.valid){
    if(modelYuimaGUI$info$class %in% c("CARMA","COGARCH")) msg <- paste("Unable to simulate ", symbName," by ", modelYuimaGUI$info$modName, ". Probably something wrong with the estimation of this model", sep = "")
    else msg <- paste("Unable to simulate", symbName,"by", modelYuimaGUI$info$modName)
    createAlert(session = session, anchorId = anchorId, alertId = alertId, content = msg, style = "danger")
    return()
  }
  
  if(saveTraj==TRUE){
    trajectory <- zoo(trajectory, order.by = timeindex)  
    if(convert==TRUE){
      if(is.numeric(data_index))
        index(trajectory) <- as.numeric(timeindex/used_delta*real_delta)
      else
        index(trajectory) <- as.POSIXct(24*60*60*(timeindex-timeindex[1])/used_delta*real_delta, origin = simulate.from)
    }
  }
  
  return(list(hist=hist, trajectory=trajectory, nstep = sampling@n[1], simulate.from = simulate.from, simulate.to = simulate.to, delta = sampling@delta))
}



addSimulation <- function(modelYuimaGUI, symbName, xinit, nsim, nstep, simulate.from, simulate.to, saveTraj, seed, sampling, true.parameter = NULL, space.discretized = FALSE, method = "euler", session, anchorId, is.multi = FALSE){
  if(!is.na(seed)) set.seed(seed)
  if(is.na(seed)) set.seed(NULL)
  sim <- simulateGUI(symbName = symbName, modelYuimaGUI = modelYuimaGUI, xinit = xinit, nsim = nsim, nstep = nstep, simulate.from = simulate.from, simulate.to = simulate.to, saveTraj = saveTraj, space.discretized = space.discretized, method = method, session = session, anchorId = anchorId, true.parameter = true.parameter)
  if(!is.null(sim)){
    if(is.multi==FALSE)
      yuimaGUIdata$simulation[[symbName]][[ifelse(is.null(length(yuimaGUIdata$simulation[[symbName]])),1,length(yuimaGUIdata$simulation[[symbName]])+1)]] <<- list(
        model = modelYuimaGUI,
        trajectory = sim$trajectory,
        hist = sim$hist,
        info = list(nsim = nsim, nstep = sim$nstep, simulate.from = sim$simulate.from, simulate.to = sim$simulate.to, delta = sim$delta)
      )
    else
      yuimaGUIdata$multisimulation[[symbName]][[ifelse(is.null(length(yuimaGUIdata$multisimulation[[symbName]])),1,length(yuimaGUIdata$multisimulation[[symbName]])+1)]] <<- list(
        model = modelYuimaGUI,
        trajectory = sim$trajectory,
        hist = sim$hist,
        info = list(nsim = nsim, nstep = sim$nstep, simulate.from = sim$simulate.from, simulate.to = sim$simulate.to, delta = sim$delta)
      )
  }
}



delSimulation <- function(symb, n=1, multi=FALSE){
  if(multi==FALSE){
    for(i in length(symb):1){
      yuimaGUIdata$simulation[[symb[i]]][as.numeric(n[i])] <<- NULL
      if (length(yuimaGUIdata$simulation[[symb[i]]])==0)
        yuimaGUIdata$simulation[[symb[i]]] <<- NULL
    }
  }
  else {
    for(i in length(symb):1){
      yuimaGUIdata$multisimulation[[symb[i]]][as.numeric(n[i])] <<- NULL
      if (length(yuimaGUIdata$multisimulation[[symb[i]]])==0)
        yuimaGUIdata$multisimulation[[symb[i]]] <<- NULL
    }
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
      if(is.numeric(index(data))) {
        if (!exists("data_num", inherits = FALSE)) data_num <- data
        else data_num <- merge(data_num, data)
      }
      else {
        if (!exists("data_date", inherits = FALSE)) data_date <- data
        else data_date <- merge(data_date, data)
      }
    }
    if (exists("data_date") & !exists("data_num")) return(as.data.frame(data_date[order(index(data_date)), , drop = FALSE]))
    if (!exists("data_date") & exists("data_num")) return(as.data.frame(data_num[order(index(data_num)), , drop = FALSE]))
    if (exists("data_date") & exists("data_num")) return(rbind.fill(as.data.frame(data_num[order(index(data_num)), , drop = FALSE]), as.data.frame(data_date[order(index(data_date)), , drop = FALSE])))
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
