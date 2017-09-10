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
    rownames(yuimaGUItable$model)[rowToPrint$id]
})

###More Info
output$text_MoreInfo <- renderUI({
  id <- unlist(strsplit(rownames(yuimaGUItable$model)[rowToPrint$id], split = " "))
  info <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info
  div(
    h3(id[1], " - " , info$modName, class = "hModal"),
    h4(
      em("series:"), info$symb, br(),
      em("series to log:"), info$toLog, br(),
      em("delta:"), info$delta, br(),
      br(),
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



