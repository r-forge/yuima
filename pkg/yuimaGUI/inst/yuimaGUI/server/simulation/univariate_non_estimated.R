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
