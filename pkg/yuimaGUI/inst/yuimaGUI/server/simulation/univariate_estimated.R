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




