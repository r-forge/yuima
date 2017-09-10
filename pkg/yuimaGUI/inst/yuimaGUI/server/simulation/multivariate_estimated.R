output$multi_simulate_databaseModels <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
  if (length(yuimaGUItable$multimodel)==0){
    NoData <- data.frame("Symb"=NA,"Please estimate some models first"=NA, check.names = FALSE)
    return(NoData[-1,])
  }
  return (yuimaGUItable$multimodel)
})

multi_modelsToSimulate <- reactiveValues(table=data.frame())

###Select Button
observeEvent(input$multi_simulate_button_selectModels, priority = 1, {
  multi_modelsToSimulate$table <<- rbind.fill(multi_modelsToSimulate$table, yuimaGUItable$multimodel[(rownames(yuimaGUItable$multimodel) %in% rownames(yuimaGUItable$multimodel)[input$multi_simulate_databaseModels_rows_selected]) & !(rownames(yuimaGUItable$multimodel) %in% rownames(multi_modelsToSimulate$table)),])
})

###SelectAll Button
observeEvent(input$multi_simulate_button_selectAllModels, priority = 1, {
  multi_modelsToSimulate$table <<- rbind.fill(multi_modelsToSimulate$table, yuimaGUItable$multimodel[(rownames(yuimaGUItable$multimodel) %in% rownames(yuimaGUItable$multimodel)[input$multi_simulate_databaseModels_rows_all]) & !(rownames(yuimaGUItable$multimodel) %in% rownames(multi_modelsToSimulate$table)),])
})



observe({
  if("AIC" %in% colnames(multi_modelsToSimulate$table))
    multi_modelsToSimulate$table[,"AIC"] <<- as.numeric(as.character(multi_modelsToSimulate$table[,"AIC"]))
  if("BIC" %in% colnames(multi_modelsToSimulate$table))
    multi_modelsToSimulate$table[,"BIC"] <<- as.numeric(as.character(multi_modelsToSimulate$table[,"BIC"]))
})

###Control selected models to be in yuimaGUIdata$multimodel or user defined
observe({
  if(length(rownames(multi_modelsToSimulate$table))!=0){
    names.valid <- c(names(yuimaGUIdata$usr_multisimulation), rownames(yuimaGUItable$multimodel))
    col <- colnames(multi_modelsToSimulate$table)
    updatedtable <- data.frame(multi_modelsToSimulate$table[which(rownames(multi_modelsToSimulate$table) %in% names.valid),], row.names = rownames(multi_modelsToSimulate$table)[rownames(multi_modelsToSimulate$table) %in% names.valid])
    colnames(updatedtable) <- col
    multi_modelsToSimulate$table <<- updatedtable
  }
})

output$multi_simulate_selectedModels <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollX=TRUE, scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
  if (length(rownames(multi_modelsToSimulate$table))==0){
    NoData <- data.frame("Symb"=NA,"Please select models from the table above"=NA, check.names = FALSE)
    return(NoData[-1,])
  }
  return (multi_modelsToSimulate$table)
})

###Delete Button
observeEvent(input$multi_simulation_button_deleteModels, priority = 1,{
  if (!is.null(input$multi_simulate_selectedModels_rows_selected))
    multi_modelsToSimulate$table <<- multi_modelsToSimulate$table[-input$multi_simulate_selectedModels_rows_selected,]
})

###DeleteAll Button
observeEvent(input$multi_simulation_button_deleteAllModels, priority = 1,{
  if (!is.null(input$multi_simulate_selectedModels_rows_all))
    multi_modelsToSimulate$table <<- multi_modelsToSimulate$table[-input$multi_simulate_selectedModels_rows_all,]
})

observe({
  shinyjs::toggle(id="multi_simulate_setSimulation_errorMessage", condition = length(rownames(multi_modelsToSimulate$table))==0)
  shinyjs::toggle(id="multi_simulate_setSimulation_body", condition = length(rownames(multi_modelsToSimulate$table))!=0)
})
observe({
  shinyjs::toggle(id="multi_simulate_advancedSettings_errorMessage", condition = length(rownames(multi_modelsToSimulate$table))==0)
  shinyjs::toggle(id="multi_simulate_advancedSettings_body", condition = length(rownames(multi_modelsToSimulate$table))!=0)
})

observe({
  for (modID in rownames(multi_modelsToSimulate$table)[input$multi_simulate_selectedModels_rows_all]){
    if (modID %in% names(yuimaGUIdata$usr_multisimulation)){
      if (is.null(yuimaGUIsettings$simulation[[modID]]))
        yuimaGUIsettings$simulation[[modID]] <<- list()
      if (is.null(yuimaGUIsettings$simulation[[modID]][["xinit"]])){
        dim <- yuimaGUIdata$usr_multisimulation[[modID]]$Dimension
        if(!is.null(dim)) 
          yuimaGUIsettings$simulation[[modID]][["xinit"]] <<- as.data.frame(t(data.frame(rep(1, dim), row.names = paste("x", 1:dim, sep = ""))))
        else {
          ###add other models
        }
      }
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
    if (modID %in% rownames(yuimaGUItable$multimodel)){
      id <- unlist(strsplit(modID, split = " "))
      if (is.null(yuimaGUIsettings$simulation[[modID]]))
        yuimaGUIsettings$simulation[[modID]] <<- list()
      if (is.null(yuimaGUIsettings$simulation[[modID]][["xinit"]])){
        xinit <- last(as.xts(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))
        toLog <- yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$info$toLog
        xinit[1,toLog==TRUE] <- exp(xinit[1,toLog==TRUE])
        yuimaGUIsettings$simulation[[modID]][["xinit"]] <<- xinit 
      }
      if (is.null(yuimaGUIsettings$simulation[[modID]][["nstep"]]))
        yuimaGUIsettings$simulation[[modID]][["nstep"]] <<- NA
      if (is.null(yuimaGUIsettings$simulation[[modID]][["nsim"]]))
        yuimaGUIsettings$simulation[[modID]][["nsim"]] <<- 1
      if (is.null(yuimaGUIsettings$simulation[[modID]][["t0"]]))
        yuimaGUIsettings$simulation[[modID]][["t0"]] <<- end(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)
      if (is.null(yuimaGUIsettings$simulation[[modID]][["t1"]]))
        if(is.numeric(index(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)))
          yuimaGUIsettings$simulation[[modID]][["t1"]] <<- 2*end(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)-start(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)
        else
          yuimaGUIsettings$simulation[[modID]][["t1"]] <<- end(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)+365
        if (is.null(yuimaGUIsettings$simulation[[modID]][["traj"]]))
          yuimaGUIsettings$simulation[[modID]][["traj"]] <<- TRUE
        if (is.null(yuimaGUIsettings$simulation[[modID]][["seed"]]))
          yuimaGUIsettings$simulation[[modID]][["seed"]] <<- NA
    }
  }
})

output$multi_simulate_modelID <- renderUI({
  selectInput("multi_simulate_modelID", label = "Simulation ID", choices = rownames(multi_modelsToSimulate$table))
})

output$multi_simulate_advancedSettings_modelID <- renderUI({
  selectInput("multi_simulate_advancedSettings_modelID", label = "Simulation ID", choices = rownames(multi_modelsToSimulate$table))
})

output$multi_simulate_seed <- renderUI({
  if(!is.null(input$multi_simulate_advancedSettings_modelID))
    numericInput("multi_simulate_seed", label = "RNG seed", step = 1, min = 0, value = yuimaGUIsettings$simulation[[input$multi_simulate_advancedSettings_modelID]][["seed"]])
})

output$multi_simulate_traj <- renderUI({
  if(!is.null(input$multi_simulate_advancedSettings_modelID))
    selectInput("multi_simulate_traj", label = "Save trajectory", choices = c(TRUE,FALSE), selected = yuimaGUIsettings$simulation[[input$multi_simulate_advancedSettings_modelID]][["traj"]])
})

output$multi_simulate_nsim <- renderUI({
  if(!is.null(input$multi_simulate_modelID))
    numericInput("multi_simulate_nsim", label = "Number of simulations", value = yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["nsim"]], min = 1, step = 1)
})

output$multi_simulate_nstep <- renderUI({
  if(!is.null(input$multi_simulate_modelID)){
    id <- unlist(strsplit(input$multi_simulate_modelID, split = " "))
    if (input$multi_simulate_modelID %in% names(yuimaGUIdata$usr_multisimulation)){
      numericInput("multi_simulate_nstep", label = "Number of steps per simulation", value = yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["nstep"]], min = 1, step = 1)
    } else if (!(isolate({yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$info$class}) %in% c("COGARCH", "CARMA")))
      numericInput("multi_simulate_nstep", label = "Number of steps per simulation", value = yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["nstep"]], min = 1, step = 1)
  }
})

output$multi_simulate_xinit_symb <- renderUI({
  if(!is.null(input$multi_simulate_modelID))
    selectInput("multi_simulate_xinit_symb", label = "Symb", choices = names(yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["xinit"]]))
})

observe({
  if(!is.null(input$multi_simulate_modelID) & !is.null(input$multi_simulate_xinit_symb)) if (input$multi_simulate_xinit_symb %in% names(yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["xinit"]]))
    output$multi_simulate_xinit <- renderUI({
        isolate({numericInput("multi_simulate_xinit", label = "Initial value", value = as.numeric(yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["xinit"]][,input$multi_simulate_xinit_symb]))})
    })
})
output$multi_simulate_range <- renderUI({
  if(!is.null(input$multi_simulate_modelID)){
    if (input$multi_simulate_modelID %in% names(yuimaGUIdata$usr_multisimulation)){
      return(div(
        column(6,numericInput("multi_simulate_rangeNumeric_t0", label = "From", min = 0 ,value = yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t0"]])),
        column(6,numericInput("multi_simulate_rangeNumeric_t1", label = "To", min = 0, value = yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t1"]]))
      ))
    }
    if (input$multi_simulate_modelID %in% rownames(yuimaGUItable$multimodel)){
      id <- unlist(strsplit(input$multi_simulate_modelID, split = " "))
      if (class(index(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date")
        dateRangeInput("multi_simulate_rangeDate", label = "Simulation interval", start = yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t0"]], end = yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t1"]])
      else{
        div(
          column(6,numericInput("multi_simulate_rangeNumeric_t0", label = "From", min = 0 ,value = yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t0"]])),
          column(6,numericInput("multi_simulate_rangeNumeric_t1", label = "To", min = 0, value = yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t1"]]))
        )
      }
    }
  }
})

observeEvent(input$multi_simulate_button_apply_advancedSettings, {
  yuimaGUIsettings$simulation[[input$multi_simulate_advancedSettings_modelID]][["seed"]] <<- input$multi_simulate_seed
  yuimaGUIsettings$simulation[[input$multi_simulate_advancedSettings_modelID]][["traj"]] <<- input$multi_simulate_traj
})
observeEvent(input$multi_simulate_button_applyAll_advancedSettings, {
  for (modID in rownames(multi_modelsToSimulate$table)){
    yuimaGUIsettings$simulation[[modID]][["seed"]] <<- input$multi_simulate_seed
    yuimaGUIsettings$simulation[[modID]][["traj"]] <<- input$multi_simulate_traj
  }
})
observeEvent(input$multi_simulate_button_apply_nsim, {
  yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["nsim"]] <<- input$multi_simulate_nsim
  yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["nstep"]] <<- input$multi_simulate_nstep
})
observeEvent(input$multi_simulate_button_applyAll_nsim, {
  for (modID in rownames(multi_modelsToSimulate$table)){
    yuimaGUIsettings$simulation[[modID]][["nsim"]] <<- input$multi_simulate_nsim
    yuimaGUIsettings$simulation[[modID]][["nstep"]] <<- input$multi_simulate_nstep
  }
})
observeEvent(input$multi_simulate_button_apply_xinit, {
  yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["xinit"]][,input$multi_simulate_xinit_symb] <<- input$multi_simulate_xinit
})
observeEvent(input$multi_simulate_button_applyAll_xinit, {
  for (modID in rownames(multi_modelsToSimulate$table))
    if(input$multi_simulate_xinit_symb %in% names(yuimaGUIsettings$simulation[[modID]][["xinit"]]))
      yuimaGUIsettings$simulation[[modID]][["xinit"]][,input$multi_simulate_xinit_symb] <<- input$multi_simulate_xinit
})
observeEvent(input$multi_simulate_button_apply_range, {
  if (input$multi_simulate_modelID %in% names(yuimaGUIdata$usr_multisimulation)){
    yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t0"]] <<- input$multi_simulate_rangeNumeric_t0
    yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t1"]] <<- input$multi_simulate_rangeNumeric_t1
  }
  if (input$multi_simulate_modelID %in% rownames(yuimaGUItable$multimodel)){
    id <- unlist(strsplit(input$multi_simulate_modelID, split = " "))
    if (class(index(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date"){
      yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t0"]] <<- input$multi_simulate_rangeDate[1]
      yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t1"]] <<- input$multi_simulate_rangeDate[2]
    }
    else{
      yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t0"]] <<- input$multi_simulate_rangeNumeric_t0
      yuimaGUIsettings$simulation[[input$multi_simulate_modelID]][["t1"]] <<- input$multi_simulate_rangeNumeric_t1
    }
  }
})
observeEvent(input$multi_simulate_button_applyAll_range, {
  for (modID in rownames(multi_modelsToSimulate$table)){
    if (modID %in% names(yuimaGUIdata$usr_multisimulation)){
      yuimaGUIsettings$simulation[[modID]][["t0"]] <<- input$multi_simulate_rangeNumeric_t0
      yuimaGUIsettings$simulation[[modID]][["t1"]] <<- input$multi_simulate_rangeNumeric_t1
    }
    if (modID %in% rownames(yuimaGUItable$multimodel)){
      id <- unlist(strsplit(modID, split = " "))
      if (class(index(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date" & !is.null(input$multi_simulate_rangeDate)){
        yuimaGUIsettings$simulation[[modID]][["t0"]] <<- input$multi_simulate_rangeDate[1]
        yuimaGUIsettings$simulation[[modID]][["t1"]] <<- input$multi_simulate_rangeDate[2]
      }
      if (class(index(yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="numeric" & !is.null(input$multi_simulate_rangeNumeric_t0)){
        yuimaGUIsettings$simulation[[modID]][["t0"]] <<- input$multi_simulate_rangeNumeric_t0
        yuimaGUIsettings$simulation[[modID]][["t1"]] <<- input$multi_simulate_rangeNumeric_t1
      }
    }
  }
})

observe({
  if (!is.null(multi_modelsToSimulate$table)) if (nrow(multi_modelsToSimulate$table)!=0) {
    closeAlert(session, alertId = "multi_simulate_alert_buttonEstimate1")
    closeAlert(session, alertId = "multi_simulate_alert_buttonEstimate2")
  }
})

observeEvent(input$multi_simulate_simulateModels, {
  if (is.null(multi_modelsToSimulate$table)) {
    if (input$panel_multi_simulations=="Estimated models") createAlert(session = session, anchorId = "panel_multi_simulate_model_alert", alertId = "multi_simulate_alert_buttonEstimate1", content = "Table 'Selected Models' is empty", style = "warning")
    if (input$panel_multi_simulations=="Non-estimated models") createAlert(session = session, anchorId = "panel_multi_simulate_equation_alert", alertId = "multi_simulate_alert_buttonEstimate2", content = "Table 'Selected Models' is empty", style = "warning")
  } else if (nrow(multi_modelsToSimulate$table)==0) {
    if (input$panel_multi_simulations=="Estimated models") createAlert(session = session, anchorId = "panel_multi_simulate_model_alert", alertId = "multi_simulate_alert_buttonEstimate1", content = "Table 'Selected Models' is empty", style = "warning")
    if (input$panel_multi_simulations=="Non-estimated models") createAlert(session = session, anchorId = "panel_multi_simulate_equation_alert", alertId = "multi_simulate_alert_buttonEstimate2", content = "Table 'Selected Models' is empty", style = "warning")
  }
  else{
    withProgress(message = 'Simulating: ', value = 0, {
      for (modID in rownames(multi_modelsToSimulate$table)){
        if(modID %in% names(yuimaGUIdata$usr_multisimulation)){
          incProgress(1/nrow(multi_modelsToSimulate$table), detail = paste(modID,"-",yuimaGUIdata$usr_multisimulation[[modID]][["Model"]]))
          jumps <- ifelse(is.null(yuimaGUIdata$usr_multisimulation[[modID]][["Jumps"]]),NA, yuimaGUIdata$usr_multisimulation[[modID]][["Jumps"]])
          dimension <- ifelse(is.null(yuimaGUIdata$usr_multisimulation[[modID]][["Dimension"]]),NA, yuimaGUIdata$usr_multisimulation[[modID]][["Dimension"]])
          modName <- yuimaGUIdata$usr_multisimulation[[modID]][["Model"]]
          modelYuimaGUI <- list(model = setYuima(model = setModelByName(name = modName, jumps = jumps, dimension = dimension)),
                                info = list(class = yuimaGUIdata$usr_multisimulation[[modID]][["Class"]], 
                                            modName = modName,
                                            jumps = jumps,
                                            dimension = dimension,
                                            symb = colnames(yuimaGUIsettings$simulation[[modID]][["xinit"]])
                                )
          )
          addSimulation(
            modelYuimaGUI = modelYuimaGUI,
            symbName = modID,
            xinit = yuimaGUIsettings$simulation[[modID]][["xinit"]],
            true.parameter = lapply(yuimaGUIdata$usr_multisimulation[[modID]][["true.param"]], FUN = function(x) as.numeric(x)),
            nsim = yuimaGUIsettings$simulation[[modID]][["nsim"]],
            nstep = yuimaGUIsettings$simulation[[modID]][["nstep"]],
            simulate.from = yuimaGUIsettings$simulation[[modID]][["t0"]],
            simulate.to = yuimaGUIsettings$simulation[[modID]][["t1"]],
            saveTraj = yuimaGUIsettings$simulation[[modID]][["traj"]],
            seed = yuimaGUIsettings$simulation[[modID]][["seed"]],
            session = session,
            anchorId = "panel_multi_simulations_alert",
            is.multi = TRUE
          )
        }
       else if(modID %in% rownames(yuimaGUItable$multimodel)){
          id <- unlist(strsplit(modID, split = " "))
          incProgress(1/nrow(multi_modelsToSimulate$table), detail = paste(id[1],"-",yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]]$info$modName))
          addSimulation(
            modelYuimaGUI = yuimaGUIdata$multimodel[[id[1]]][[as.numeric(id[2])]],
            symbName = id[1],
            xinit = yuimaGUIsettings$simulation[[modID]][["xinit"]],
            nsim = yuimaGUIsettings$simulation[[modID]][["nsim"]],
            nstep = yuimaGUIsettings$simulation[[modID]][["nstep"]],
            simulate.from = yuimaGUIsettings$simulation[[modID]][["t0"]],
            simulate.to = yuimaGUIsettings$simulation[[modID]][["t1"]],
            saveTraj = yuimaGUIsettings$simulation[[modID]][["traj"]],
            seed = yuimaGUIsettings$simulation[[modID]][["seed"]],
            session = session,
            anchorId = "panel_multi_simulations_alert",
            is.multi = TRUE
          )
        }
      }
    })
    updateTabsetPanel(session = session,  inputId = "panel_multi_simulations", selected = "Simulations")
  }
})

observe({
  shinyjs::toggle("multi_div_simulations", condition = (input$panel_multi_simulations!="Simulations"))
})




