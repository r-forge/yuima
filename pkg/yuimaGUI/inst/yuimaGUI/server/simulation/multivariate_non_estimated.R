output$multi_simulate_PrintModelLatex <- renderUI({
  if (!is.null(input$multi_simulate_model_usr_selectModel)){
    return(withMathJax(printModelLatex(multi = TRUE, names = input$multi_simulate_model_usr_selectModel, process = isolate({input$multi_simulate_model_usr_selectClass}), dimension = input$multi_simulate_model_usr_selectDimension, jumps = jumps_shortcut(class = isolate({input$multi_simulate_model_usr_selectClass}), jumps = input$multi_simulate_model_usr_selectJumps))))
  }
})


output$multi_simulate_model_usr_selectModel <- renderUI({
  choices <- as.vector(defaultMultiModels[names(defaultMultiModels)==input$multi_simulate_model_usr_selectClass])
  sel <- choices[1]
  for(i in names(yuimaGUIdata$multiusr_model))
    if (yuimaGUIdata$multiusr_model[[i]]$class==input$multi_simulate_model_usr_selectClass)
      choices <- c(i, choices)
  selectInput("multi_simulate_model_usr_selectModel", label = "Model Name", choices = choices, selected = sel)
})

output$multi_simulate_model_usr_selectJumps <- renderUI({
  if(input$multi_simulate_model_usr_selectClass %in% c("Compound Poisson", "Levy process"))
    return(selectInput("multi_simulate_model_usr_selectJumps",label = "Jumps", choices = defaultJumps))
})

output$multi_simulate_model_usr_selectDimension <- renderUI({
  if(!is.null(input$multi_simulate_model_usr_selectModel))
    if(input$multi_simulate_model_usr_selectModel %in% c("Correlated Brownian Motion"))
      return(numericInput("multi_simulate_model_usr_selectDimension",label = "Dimension", value = 2, step = 1))
})

output$multi_simulate_model_usr_selectParam <- renderUI({
  valid <- TRUE
  if (is.null(input$multi_simulate_model_usr_selectModel)) valid <- FALSE
  else if (isolate({input$multi_simulate_model_usr_selectClass=="Compound Poisson"}) & is.null(input$multi_simulate_model_usr_selectJumps)) valid <- FALSE
  else if (isolate({input$multi_simulate_model_usr_selectModel %in% c("Correlated Brownian Motion")}) & is.null(input$multi_simulate_model_usr_selectDimension)) valid <- FALSE
  if (valid) {
	mod <- setModelByName(input$multi_simulate_model_usr_selectModel, dimension = input$multi_simulate_model_usr_selectDimension, jumps = jumps_shortcut(class = isolate({input$multi_simulate_model_usr_selectClass}), jumps = input$multi_simulate_model_usr_selectJumps))
    choices <- getAllParams(mod = mod, class = input$multi_simulate_model_usr_selectClass)
    if (input$multi_simulate_model_usr_selectClass=="Fractional process") choices <- c(choices, "hurst")
    return(selectInput("multi_simulate_model_usr_selectParam", label = "Parameter", choices = choices))
  }
})

output$multi_simulate_model_usr_param <- renderUI({
  numericInput("multi_simulate_model_usr_param", label = "Parameter value", value = NA)
})

output$multi_simulate_model_usr_ID <- renderUI({
  textInput("multi_simulate_model_usr_ID", label = "Simulation ID")
})


observeEvent(input$multi_simulate_model_usr_button_save, {
  if(input$multi_simulate_model_usr_ID!=""){
    id <- gsub(pattern = " ", x = input$multi_simulate_model_usr_ID, replacement = "")
    if (is.null(yuimaGUIdata$usr_multisimulation[[id]])){
      yuimaGUIdata$usr_multisimulation[[id]] <<- list()
    }
    yuimaGUIdata$usr_multisimulation[[id]][["Class"]] <<- input$multi_simulate_model_usr_selectClass
    yuimaGUIdata$usr_multisimulation[[id]][["Model"]] <<- input$multi_simulate_model_usr_selectModel
    yuimaGUIdata$usr_multisimulation[[id]][["Jumps"]] <<- input$multi_simulate_model_usr_selectJumps
    yuimaGUIdata$usr_multisimulation[[id]][["Dimension"]] <<- input$multi_simulate_model_usr_selectDimension
    if (is.null(yuimaGUIdata$usr_multisimulation[[id]][["true.param"]])){
      yuimaGUIdata$usr_multisimulation[[id]][["true.param"]] <<- list()
    }
	mod <- setModelByName(input$multi_simulate_model_usr_selectModel, jumps = input$multi_simulate_model_usr_selectJumps, dimension = input$multi_simulate_model_usr_selectDimension)
    allparam <- getAllParams(mod = mod, class = input$multi_simulate_model_usr_selectClass)
    if (length(allparam)==0)
      yuimaGUIdata$usr_multisimulation[[id]]["true.param"] <<- NULL
    if (length(allparam)!=0){
      for(i in c(allparam, names(yuimaGUIdata$usr_multisimulation[[id]][["true.param"]]))){
        if (!(i %in% names(yuimaGUIdata$usr_multisimulation[[id]][["true.param"]])))
          yuimaGUIdata$usr_multisimulation[[id]][["true.param"]][[i]] <<- "MISSING"
        if(!(i %in% allparam))
          yuimaGUIdata$usr_multisimulation[[id]][["true.param"]][i] <<- NULL
      }
    }
  }
})

observe({
  if (!is.null(input$multi_simulate_model_usr_ID) & !is.null(input$multi_simulate_model_usr_selectParam) & !is.null(input$multi_simulate_model_usr_param)){
    id <- gsub(pattern = " ", x = input$multi_simulate_model_usr_ID, replacement = "")
    if (!is.null(yuimaGUIdata$usr_multisimulation[[id]])){
      valid <- TRUE
      if(yuimaGUIdata$usr_multisimulation[[id]][["Model"]]!=input$multi_simulate_model_usr_selectModel | input$multi_simulate_model_usr_selectParam=="") 
        valid <- FALSE
      else if (yuimaGUIdata$usr_multisimulation[[id]][["Class"]] %in% c("Compound Poisson", "Levy process")) if (yuimaGUIdata$usr_multisimulation[[id]][["Jumps"]]!=input$multi_simulate_model_usr_selectJumps)  
        valid <- FALSE
      if (valid)
        yuimaGUIdata$usr_multisimulation[[id]][["true.param"]][[input$multi_simulate_model_usr_selectParam]] <- ifelse(is.na(input$multi_simulate_model_usr_param),"MISSING",input$multi_simulate_model_usr_param)
    }
  }
})

observe({
  for(i in names(yuimaGUIdata$usr_multisimulation))
    if (!(yuimaGUIdata$usr_multisimulation[[i]]$Model %in% c(defaultMultiModels, names(yuimaGUIdata$multiusr_model))))
      yuimaGUIdata$usr_multisimulation[i] <<- NULL
})

output$multi_simulate_model_usr_table <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollX=TRUE, scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
  table <- data.frame()
  for (i in names(yuimaGUIdata$usr_multisimulation)){
    newRow <- as.data.frame(yuimaGUIdata$usr_multisimulation[[i]])
    colnames(newRow) <- gsub(pattern = "true.param.", x = colnames(newRow), replacement = "")
    table <- rbind.fill(table, newRow)
  }
  if (length(table)==0){
    NoData <- data.frame("Model"=NA, "Parameters"=NA)
    return(NoData[-1,])
  }
  return (data.frame(table, row.names = names(yuimaGUIdata$usr_multisimulation)))
})

observeEvent(input$multi_simulate_model_usr_button_select, {
  if (!is.null(input$multi_simulate_model_usr_table_rows_selected)){
    table <- data.frame()
    for (i in names(yuimaGUIdata$usr_multisimulation)[input$multi_simulate_model_usr_table_rows_selected]){
      if ("MISSING" %in% yuimaGUIdata$usr_multisimulation[[i]][["true.param"]]){
        createAlert(session = session, anchorId = "panel_multi_simulate_equation_alert", alertId = "multi_simulate_alert_usr_button_select", content = "There are still missing values in selected models", style = "error")
      }
      else {
        closeAlert(session, "multi_simulate_alert_usr_button_select")
        newRow <- as.data.frame(yuimaGUIdata$usr_multisimulation[[i]], row.names=i)
        colnames(newRow) <- gsub(pattern = "true.param.", x = colnames(newRow), replacement = "")
        table <- rbind.fill(table, newRow)
      }
    }
    if (length(rownames(table))!=0)
      multi_modelsToSimulate$table <<- multi_modelsToSimulate$table[-which(rownames(multi_modelsToSimulate$table) %in% rownames(table)),]
    multi_modelsToSimulate$table <<- rbind.fill(multi_modelsToSimulate$table, table)
  }
})

observeEvent(input$multi_simulate_model_usr_button_selectAll, {
  if (!is.null(input$multi_simulate_model_usr_table_rows_all)){
    table <- data.frame()
    for (i in names(yuimaGUIdata$usr_multisimulation)[input$multi_simulate_model_usr_table_rows_all]){
      if ("MISSING" %in% yuimaGUIdata$usr_multisimulation[[i]][["true.param"]]){
        createAlert(session = session, anchorId = "panel_multi_simulate_equation_alert", alertId = "multi_simulate_alert_usr_button_select", content = "There are still missing values in selected models", style = "error")
      }
      else {
        closeAlert(session, "multi_simulate_alert_usr_button_select")
        newRow <- as.data.frame(yuimaGUIdata$usr_multisimulation[[i]], row.names=i)
        colnames(newRow) <- gsub(pattern = "true.param.", x = colnames(newRow), replacement = "")
        table <- rbind.fill(table, newRow)
      }
    }
    if (length(rownames(table))!=0)
      multi_modelsToSimulate$table <<- multi_modelsToSimulate$table[-which(rownames(multi_modelsToSimulate$table) %in% rownames(table)),]
    multi_modelsToSimulate$table <<- rbind.fill(multi_modelsToSimulate$table, table)
  }
})

observeEvent(input$multi_simulate_model_usr_button_delete, {
  if (!is.null(input$multi_simulate_model_usr_table_rows_selected)){
    for (i in input$multi_simulate_model_usr_table_rows_selected){
      yuimaGUIdata$usr_multisimulation[i] <- NULL
    }
  }
})

observeEvent(input$multi_simulate_model_usr_button_deleteAll, {
  if (!is.null(input$multi_simulate_model_usr_table_rows_all)){
    for (i in input$multi_simulate_model_usr_table_rows_all){
      yuimaGUIdata$usr_multisimulation[i] <- NULL
    }
  }
})
