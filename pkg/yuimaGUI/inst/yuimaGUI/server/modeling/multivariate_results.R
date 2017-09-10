###Display estimated models
output$multi_databaseModels <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
  if (length(yuimaGUItable$multimodel)==0){
    NoData <- data.frame("Symb"=NA,"Here will be stored models you estimate in the previous tabs"=NA, check.names = FALSE)
    return(NoData[-1,])
  }
  return (yuimaGUItable$multimodel)
})

multi_rowToPrint <- reactiveValues(id = NULL)
observe(priority = 1, {
  multi_rowToPrint$id <<- NULL
  n <- nrow(yuimaGUItable$multimodel)
  if (n > 0) {
    multi_rowToPrint$id <<- n
    if (!is.null(input$multi_databaseModels_row_last_clicked)) multi_rowToPrint$id <- min(n, input$multi_databaseModels_row_last_clicked)
  }
})

###Print estimated model in Latex
output$multi_estimatedModelsLatex <- renderUI({
  if (!is.null(multi_rowToPrint$id)){
    id <- rownames(yuimaGUItable$multimodel)[multi_rowToPrint$id]
    id1 <- unlist(strsplit(id, split = " "))[1]
    id2 <- as.numeric(unlist(strsplit(id, split = " "))[2])
    symbs <- yuimaGUIdata$multimodel[[id1]][[id2]]$info$symb
    withMathJax(printModelLatex(multi = TRUE, symb = symbs, as.character(yuimaGUItable$multimodel[multi_rowToPrint$id, "Model"]), process = as.character(yuimaGUItable$multimodel[multi_rowToPrint$id, "Class"]), jumps = as.character(yuimaGUItable$multimodel[multi_rowToPrint$id, "Jumps"])))
  }
})

###Print Symbol
output$multi_SymbolName <- renderText({
  if (!is.null(multi_rowToPrint$id))
    rownames(yuimaGUItable$multimodel)[multi_rowToPrint$id]
})

###More Info
output$multi_text_MoreInfo <- renderUI({
  id <- unlist(strsplit(rownames(yuimaGUItable$multimodel)[multi_rowToPrint$id], split = " "))
  info <- yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$info
  div(
    h3(id[1], " - " , info$modName, class = "hModal"),
    h4(
      em("series:"), paste(info$symb, collapse = ", "), br(),
      em("series to log:"), paste(info$toLog, collapse = ", "), br(),
      em("delta:"), max(info$delta), br(),
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

output$multi_table_MoreInfo <- renderTable(digits=5, rownames = TRUE, {
  id <- unlist(strsplit(rownames(yuimaGUItable$multimodel)[multi_rowToPrint$id], split = " "))
  info <- yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$info
  if (info$class=="Fractional process") coef <- as.data.frame(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$qmle)
  else coef <- as.data.frame(t(summary(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$qmle)@coef))
  params <- yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@model@parameter@all
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
  if (!is.null(multi_rowToPrint$id)){
    symb <- unlist(strsplit(rownames(yuimaGUItable$multimodel)[multi_rowToPrint$id], split = " "))[1]
    modN <- as.numeric(unlist(strsplit(rownames(yuimaGUItable$multimodel)[multi_rowToPrint$id], split = " "))[2])
    if (yuimaGUIdata$multimodel[[symb]][[modN]]$info$class=="Fractional process") table <- yuimaGUIdata$multimodel[[symb]][[modN]]$qmle
    else table <- t(summary(yuimaGUIdata$multimodel[[symb]][[modN]]$qmle)@coef)
    outputTable <- changeBase(table = table, yuimaGUI = yuimaGUIdata$multimodel[[symb]][[modN]], newBase = input$multi_baseModels, session = session, choicesUI="multi_baseModels", anchorId = "multi_panel_estimates_alert", alertId = "multi_modelsAlert_conversion")
    output$multi_estimatedModelsTable <- renderTable(rownames = TRUE, {
      if (!is.null(multi_rowToPrint$id))
        return(outputTable)
    })
  }
})

observe({
  shinyjs::toggle("multi_estimates_info", condition = !is.null(input$multi_databaseModels_rows_all))
})


observe({
  test <- FALSE
  choices <- NULL
  if(length(names(yuimaGUIdata$multimodel))!=0) for (i in names(yuimaGUIdata$multimodel)) for (j in 1:length(yuimaGUIdata$multimodel[[i]])) 
    if(yuimaGUIdata$multimodel[[i]][[j]]$info$class %in% c("Diffusion process", "Compound Poisson", "Levy process", "COGARCH")){
      test <- TRUE
      choices <- c(choices, paste(i,j))
    }
  shinyjs::toggle(id = "multi_model_modal_fitting_body", condition = test)
  shinyjs::toggle(id = "multi_databaseModels_button_showResults", condition = test)
  output$multi_model_modal_model_id <- renderUI({
    if (test==TRUE){
      selectInput("multi_model_modal_model_id", label = "Model ID", choices = choices)
    }
  })
  output$multi_model_modal_series_id <- renderUI({
    if (!is.null(input$multi_model_modal_model_id)){
      id <- unlist(strsplit(input$multi_model_modal_model_id, split = " " , fixed = FALSE))
      symb <- try(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$info$symb)
      choices <- 1:length(symb)
      names(choices) <- symb
      if(class(choices)!='try-error')
        selectInput("multi_model_modal_series_id", label = "Series", choices = choices)
    }
  })
})

observe({
  if(!is.null(input$multi_model_modal_model_id)) {
    id <- unlist(strsplit(input$multi_model_modal_model_id, split = " " , fixed = FALSE))
    type <- isolate({yuimaGUIdata$multimodel})[[id[1]]][[as.numeric(id[2])]]$info$class
    shinyjs::toggle(id = "multi_model_modal_plot_intensity", condition = type %in% c("Compound Poisson", "Levy process"))
    shinyjs::toggle(id = "multi_model_modal_plot_variance", condition = type %in% c("COGARCH"))
    shinyjs::toggle(id = "multi_model_modal_plot_distr", condition = type %in% c("Diffusion process","Compound Poisson", "Levy process"))
    shinyjs::toggle(id = "multi_model_modal_plot_test", condition = type %in% c("Diffusion process","Compound Poisson", "Levy process"))
  }
})

observe({
  if(!is.null(input$multi_model_modal_model_id)){
    id <- unlist(strsplit(input$multi_model_modal_model_id, split = " " , fixed = FALSE))
    isolated_yuimaGUIdataModel <- isolate({yuimaGUIdata$multimodel}) 
    if(id[1] %in% names(isolated_yuimaGUIdataModel)) if (length(isolated_yuimaGUIdataModel[[id[1]]])>=as.integer(id[2])){
      y <- isolated_yuimaGUIdataModel[[id[1]]][[as.numeric(id[2])]]
      
      if (y$info$class=="Diffusion process"){
        
        delta <- y$model@sampling@delta
        t <- y$model@sampling@grid[[1]][-length(y$model@sampling@grid[[1]])]
        for (i in 1:length(y$model@data@zoo.data)) {
          if(i==1) x <- y$model@data@zoo.data[[i]]
          else x <- merge(x, y$model@data@zoo.data[[i]])
        }
        dx <- diff(x)
        x <- x[-length(x),]
        for (i in names(y$qmle@coef)) assign(i, value = as.numeric(y$qmle@coef[i]))
        mu <- sapply(y$model@model@drift, function(x) eval(x))
        sigma <- do.call(rbind, (lapply(y$model@model@diffusion, FUN = function(x) sapply(x, function(y) eval(y)))))
        sigma_inv <- try(solve(sigma))
        if(class(sigma_inv)=="try-error"){
          shinyjs::hide("multi_model_modal_plot_distr")
          output$multi_model_modal_plot_test <- renderUI({
              HTML(paste("<div><h2 class='hModal'>Tool not available for this model.<br/>The diffusion matrix is not invertible.</h2></div>"))
          })
        } else {
          z <- apply(as.matrix((dx-mu*delta)/sqrt(delta)), MARGIN = 1, FUN = function(y) sigma_inv%*%y)
          if(class(z)=="numeric") z <- t(z)
          if(!is.null(input$multi_model_modal_series_id)) if(nrow(z)>=input$multi_model_modal_series_id){
            z_univ <- data.frame("V1" = as.numeric(z[as.numeric(input$multi_model_modal_series_id),]))
            shinyjs::show("multi_model_modal_plot_distr")
            output$multi_model_modal_plot_distr <- renderPlot({
              return(
                ggplot(z_univ, aes(x = V1)) + 
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
            output$multi_model_modal_plot_test <- renderUI({
              ksTest <- try(ks.test(x = z_univ$V1, "pnorm"))
              if(class(ksTest)!="try-error")
                HTML(paste("<div><h5 class='hModal'>Kolmogorov-Smirnov p-value (the two distributions coincide): ", format(ksTest$p.value, scientific=T, digits = 2), "</h5></div>"))
            })
          }
        }
      }
      
    #   else if (y$info$class=="COGARCH"){
    #     
    #     dx <- diff(y$model@data@original.data[,1])
    #     v <- sqrt(cogarchNoise(y$model, param = as.list(coef(y$qmle)))$Cogarch@original.data[,"v"])
    #     v <- v/mean(v)*sd(dx)
    #     z <- data.frame("dx" = dx, "vplus" = v[-1], "vminus" = -v[-1], "time" = index(dx))
    #     output$multi_model_modal_plot_variance <- renderPlot({
    #       return(
    #         ggplot(z, aes(x = time)) + 
    #           geom_line(aes(y = dx), size = 1, color = "black") +
    #           geom_line(aes(y = vplus), size = 1, color = "green") +
    #           geom_line(aes(y = vminus), size = 1, color = "green") +
    #           scale_color_manual(values=c("black", "green", "green")) +
    #           theme(
    #             plot.title = element_text(size=14, face= "bold", hjust = 0.5),
    #             axis.title=element_text(size=12),
    #             legend.position="none"
    #           ) +
    #           labs(fill="", title = "Empirical VS Estimated Volatility", x = "", y = "Increments")
    #       )
    #     })
    #   }
    #   
    #   else if (y$info$class=="Compound Poisson" | y$info$class=="Levy process"){
    #     if (is.null(y$info$threshold)) threshold <- 0
    #     else threshold <- ifelse(is.na(y$info$threshold), 0, y$info$threshold)          
    #     x <- as.numeric(y$model@data@zoo.data[[1]])
    #     dx <- diff(x)
    #     dx <- dx[abs(dx)>threshold]
    #     #dx <- dx-sign(dx)*threshold
    #     for (i in names(y$qmle@coef)) assign(i, value = as.numeric(y$qmle@coef[i]))
    #     dx <- data.frame("V1" = dx)
    #     if(y$info$jumps=="Gaussian"){
    #       output$multi_model_modal_plot_distr <- renderPlot({
    #         return(
    #           ggplot(dx, aes(x = V1)) + 
    #             theme(
    #               plot.title = element_text(size=14, face= "bold", hjust = 0.5),
    #               axis.title=element_text(size=12),
    #               legend.position="none"
    #             ) +
    #             stat_function(fun = dnorm, args = list(mean = mu_jump, sd = sigma_jump), fill = "blue",color = "blue", geom = 'area', alpha = 0.5) +
    #             geom_density(alpha = 0.5, fill = "green", color = "green") +
    #             xlim(-4, 4) + 
    #             labs(fill="", title = "Empirical VS Estimated Distribution", x = "Increments", y = "Density")
    #         )
    #       })
    #       ksTest <- try(ks.test(x = as.numeric(dx$V1), "pnorm", mean = mu_jump, sd = sigma_jump))
    #       output$multi_model_modal_plot_test <- renderUI({
    #         if(class(ksTest)!="try-error")
    #           HTML(paste("<div><h5 class='hModal'>Kolmogorov-Smirnov p-value (the two distributions coincide): ", format(ksTest$p.value, scientific=T, digits = 2), "</h5></div>"))
    #       })
    #     }
    #     if(y$info$jumps=="Uniform"){
    #       output$multi_model_modal_plot_distr <- renderPlot({
    #         return(
    #           ggplot(dx, aes(x = V1)) + 
    #             theme(
    #               plot.title = element_text(size=14, face= "bold", hjust = 0.5),
    #               axis.title=element_text(size=12),
    #               legend.position="none"
    #             ) +
    #             stat_function(fun = dunif, args = list(min = a_jump, max = b_jump), fill = "blue",color = "blue", geom = 'area', alpha = 0.5) +
    #             geom_density(alpha = 0.5, fill = "green", color = "green") +
    #             xlim(min(dx$V1),max(dx$V1)) + 
    #             labs(fill="", title = "Empirical VS Estimated Distribution", x = "Increments", y = "Density")
    #         )
    #       })
    #       ksTest <- try(ks.test(x = as.numeric(dx$V1), "punif", min = a_jump, max = b_jump))
    #       output$multi_model_modal_plot_test <- renderUI({
    #         if(class(ksTest)!="try-error")
    #           HTML(paste("<div><h5 class='hModal'>Kolmogorov-Smirnov p-value (the two distributions coincide): ", format(ksTest$p.value, scientific=T, digits = 2), "</h5></div>"))
    #       })
    #     }
    #     
    #     
    #     delta <- y$model@sampling@delta
    #     jumps <- ifelse(abs(diff(x))>threshold,1,0)
    #     jumps[is.na(jumps)] <- 0
    #     empirical_Lambda <- cumsum(jumps)
    #     t <- y$model@sampling@grid[[1]][-1]
    #     theory_Lambda <- cumsum(eval(y$model@model@measure$intensity)*rep(delta, length(t)))
    #     Lambda <- data.frame(empirical = empirical_Lambda, theory = theory_Lambda, time = index(y$model@data@original.data)[-1]) 
    #     output$multi_model_modal_plot_intensity <- renderPlot({
    #       return(
    #         ggplot(Lambda, aes(x = time)) +
    #           geom_line(aes(y = empirical), size = 1, color = "green") +
    #           geom_line(aes(y = theory), size = 1, color = "blue") +
    #           scale_color_manual(values=c("green", "blue")) +
    #           theme(
    #             plot.title = element_text(size=14, face= "bold", hjust = 0.5),
    #             axis.title=element_text(size=12),
    #             legend.position="none"
    #           ) +
    #           labs(fill="", title = "Empirical VS Estimated Intensity", x = "", y = "Number of Jumps")
    #       )
    #       
    #     })
    #     
    #   }
    }
  }
})





###Delete Model
observeEvent(input$multi_databaseModelsDelete, priority = 1, {
  if(!is.null(input$multi_databaseModels_rows_selected) & !is.null(input$multi_databaseModels_row_last_clicked)){
    if(input$multi_databaseModels_row_last_clicked %in% input$multi_databaseModels_rows_selected){
      rowname <- unlist(strsplit(rownames(yuimaGUItable$multimodel)[input$multi_databaseModels_row_last_clicked], split = " " , fixed = FALSE))
      delMultiModel(symb=rowname[1], n=rowname[2])
      closeAlert(session, alertId = "modelsAlert_conversion")
    }
  }
})

###DeleteAll Model
observeEvent(input$multi_databaseModelsDeleteAll, priority = 1, {
  if(!is.null(input$multi_databaseModels_rows_all)){
    closeAlert(session, alertId = "modelsAlert_conversion")
    rowname <- unlist(strsplit(rownames(yuimaGUItable$multimodel)[input$multi_databaseModels_rows_all], split = " " , fixed = FALSE))
    delMultiModel(symb=rowname[seq(1,length(rowname),2)], n=rowname[seq(2,length(rowname),2)])
  }
})



