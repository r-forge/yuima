options(shiny.maxRequestSize = 9*1024^2)


server <- function(input, output, session) {

  ###Save all available data 
  saveData <- function() { 
    dataDownload_series <- reactive({
      data <- data.frame()
      for (symb in names(yuimaGUIdata$series))
        data <- as.data.frame(rbind.fill(as.data.frame(data),as.data.frame(t(getData(symb)))))
      data <- as.data.frame(t(data))
      colnames(data) <- names(yuimaGUIdata$series)
      return(data)
    })
    downloadHandler(
      filename = "yuimaGUIdata.txt",
      content = function(file) {
        write.table(dataDownload_series(), file, quote = FALSE)
      }
    )
  }
  
  ########################Load Economic and Financial Data
  ########################
  ########################
  
  
  ###Download data and display message
  observeEvent(input$finDataGo, priority = 1, {
    if (input$symb!=""){
      closeAlert(session, "finDataAlert_err")
      closeAlert(session, "finDataAlert_succ")
      symb <- unlist(strsplit(input$symb, split = "[, ]+" , fixed = FALSE))
      err <- c()
      withProgress(message = 'Loading: ', value = 0, {
        for (i in symb){
          incProgress(1/length(symb), detail = i)
          x <- try(getSymbols(i, src = input$sources ,auto.assign = FALSE, from = input$dR[1], to = input$dR[2]))
          if (class(x)[1]=="try-error") 
            err <- cbind(err,i)
          else
            addData(x, typeIndex = "%Y-%m-%d", session = session, anchorId = "finDataAlert", printSuccess = FALSE)
        }
      })
      if(!is.null(err))
        createAlert(session = session, anchorId = "finDataAlert", alertId = "finDataAlert_err", content = paste("WARNING! Unable to download following symbols:", paste(err,collapse = " ")), style = "danger")
      if(is.null(err))
        createAlert(session = session, anchorId = "finDataAlert", alertId = "finDataAlert_succ", content = paste("All symbols downloaded successfully"), style = "success")
    }
  })
  
  ###Display available data
  output$database1 <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = TRUE, deferRender = TRUE, dom = 'frtiS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"From"=NA, "To"=NA)
      return(NoData[-1,])
    }
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
    range_finDataPlot$x <- NULL
    range_finDataPlot$y <- NULL
    symb <- tail(input$database1_rows_selected,1)
    shinyjs::show("finDataPlot")
    shinyjs::show("scale_finDataPlot")
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
          par(bg="black")
          plot.zoo(window(getData(symb), start = range_finDataPlot$x[1], end = range_finDataPlot$x[2]), main=symb, log=ifelse(input$scale_finDataPlot=="Linear","","y"), xlab="Index", ylab=NA, col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
          grid(col="grey")
        }
      }
    })
  })
  
  ###Delete Button
  observeEvent(input$finDataDelete, priority = 1,{
      delData(input$database1_rows_selected)
  })
  
  ###DeleteAll Button
  observeEvent(input$finDataDeleteAll, priority = 1,{
    delData(input$database1_rows_all)
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
      if(input$yourFileSep=="default")
        sep <- ""
      if(input$yourFileHeader=="Only rows")
        z <- read.table(input$yourFile$datapath ,sep = sep, header = FALSE, row.names = 1, check.names = FALSE)
      if(input$yourFileHeader=="Only columns"){
        z <- read.table(input$yourFile$datapath, sep = sep, header = FALSE, check.names = FALSE)
        z <- data.frame(t(z), row.names = 1, check.names = FALSE)
        z <- data.frame(t(z), check.names = FALSE)
      }
      if (input$yourFileHeader=="Both")
        z <- read.table(input$yourFile$datapath, sep = sep, header = TRUE, check.names = FALSE)
      if (input$yourFileHeader=="None")
        z <- read.table(input$yourFile$datapath, sep = sep, header = FALSE, check.names = FALSE)
      if (input$yourFileHeader=="Default")
        z <- read.table(input$yourFile$datapath, sep = sep, check.names = FALSE)
      if (input$yourFileHeader=="Only rows" | identical(colnames(z),paste("V",seq(1,length(colnames(z))),sep="")))
        colnames(z) <- paste("X",seq(1,length(colnames(z))),"_",make.names(input$yourFile$name),sep="")
      return(z)
    }
  })
  
  ###Exchange rows/columns of file
  fileUp_T <- reactive({
    if (!is.null(input$yourFile$datapath)){
      z <- as.data.frame(t(fileUp_O()), check.names = FALSE)
      if (input$yourFileHeader=="Only columns" | identical(colnames(z),paste("V",seq(1,length(colnames(z))),sep="")))
        colnames(z) <- paste("X",seq(1,length(colnames(z))),"_",make.names(input$yourFile$name),sep="")
      return(z)
    }
  })
  
  ###Display Index choices: columns of file or transposed file
  output$yourFileIndex <- renderUI({
    temp <- try(colnames(fileUp_O()))
    if (input$yourFileSwitch==TRUE)
      temp <- try(colnames(fileUp_T()))
    if (class(temp)=="try-error")
      return(selectInput("yourFileIndex",label = "Index", choices = c("Default"="default","Numeric"="numeric"), selected = "default"))
    if (class(temp)!="try-error")
      return(selectInput("yourFileIndex",label = "Index", choices = c("Default"="default","Numeric"="numeric",temp), selected = "default"))
  })
  
  ###Display choices for Index Type and set to "numeric" if Index is "numeric"
  output$yourFileFUN <- renderUI({
    if (!is.null(input$yourFileIndex)){
      sel <- "%Y-%m-%d"
      if (input$yourFileIndex=="numeric") 
        sel <- "numeric"
      selectInput("yourFileFUN", label = "Index Format", choices = c("Numeric"="numeric", "Year-Month-Day    (yyyy-mm-dd)"="%Y-%m-%d", "Month-Day-Year    (mm-dd-yyyy)"="%m-%d-%Y", "Month-Day-Year    (mm-dd-yy)"="%m-%d-%y", "Day-Month-Year    (dd-mm-yyyy)"="%d-%m-%Y", "Day-Month-Year    (dd-mm-yy)"="%d-%m-%y", "Year/Month/Day    (yyyy/mm/dd)"="%Y/%m/%d", "Month/Day/Year    (mm/dd/yyyy)"="%m/%d/%Y", "Month/Day/Year    (mm/dd/yy)"="%m/%d/%y", "Day/Month/Year    (dd/mm/yyyy)"="%d/%m/%Y", "Day/Month/Year    (dd/mm/yy)"="%d/%m/%y"), selected = sel)
    }
  })
  
  ###File to upload
  fileUp <- reactive({
    if (!is.null(input$yourFile$datapath)){
      z <- fileUp_O()
      if (input$yourFileSwitch==TRUE)
        z <- fileUp_T()
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
    addData(fileUp(), typeIndex = input$yourFileFUN, session = session, anchorId = "yourDataAlert")
  })
  
  ###Display data available
  output$database2 <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = TRUE, deferRender = TRUE, dom = 'frtiS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"From"=NA, "To"=NA)
      return(NoData[-1,])
    }
    return (yuimaGUItable$series)
  })
  
  ###Delete Button
  observeEvent(input$yourFileDelete, priority = 1,{
    delData(input$database2_rows_selected)
  })
  
  ###DeleteAll Button
  observeEvent(input$yourFileDeleteAll, priority = 1,{
    delData(input$database2_rows_all)
  })
  
  ###Save Button
  output$yourFileSave <- {
    saveData()
  }

  ########################Univariate Models
  ########################
  ########################
  
  ###Model Input depending on Class Input
  output$model <- renderUI({
    if (input$modelClass=="Diffusion processes"){
      choices <- as.vector(defaultModels[names(defaultModels)=="Diffusion process"])
      for(i in names(usr_models$model))
        if (usr_models$model[[i]]$class=="Diffusion process")
          choices <- c(choices, i)
    }
    return (selectInput("model",label = "Model Name", choices = choices, multiple = TRUE))
  })
  
  ###Print last selected model in Latex
  output$PrintModelLatex <- renderUI({
    shinyjs::hide("titlePrintModelLatex")
    if (!is.null(input$model)){
      shinyjs::show("titlePrintModelLatex")
      return(withMathJax(printModelLatex(input$model)))
    }
  })
  
  output$usr_modelClass_latex <- renderUI({
    if (input$modelClass=="Diffusion processes")
      return(withMathJax("$$dX=(f_1)\\;dt\\;+\\;(f_2)\\;dW$$"))
  })
  
  output$usr_model_coeff <- renderUI({
    if (input$modelClass=="Diffusion processes")
      return(
        div(align="center",
          column(6, textInput("usr_model_coeff_drift", width = "70%", label = withMathJax("$$f_1$$"))),
          column(6, textInput("usr_model_coeff_diff", width = "70%", label = withMathJax("$$f_2$$")))
        )
      )
  })
  
  observeEvent(input$usr_model_button_save, {
    if (input$modelClass=="Diffusion processes" & input$usr_model_name!="" & (input$usr_model_coeff_drift!="" | input$usr_model_coeff_diff!="")){
      mod <- try(setModel(drift = tolower(input$usr_model_coeff_drift), diffusion = tolower(input$usr_model_coeff_diff), solve.variable = "x"))
      closeAlert(session, "alert_savingModels")
      if(class(mod)!="try-error"){
        usr_models$model[[input$usr_model_name]] <<- list(object=mod, class="Diffusion process")
        createAlert(session = session, anchorId = "modelsAlert", alertId = "alert_savingModels", style = "success", content = "Model saved successfully") 
      }
      else
        createAlert(session = session, anchorId = "modelsAlert", alertId = "alert_savingModels", style = "error", content = "Model is not correctly specified")
    }
  })
  
  output$usr_model_saved <- renderUI({
    if (length(names(usr_models$model))!=0)
      selectInput("usr_model_saved", label = "Saved Models", choices = names(usr_models$model), multiple = TRUE, selected = tail(names(usr_models$model),1))
  })
  
  output$usr_model_saved_latex <- renderUI({
    input$usr_model_button_save
    if (!is.null(input$usr_model_saved))
      withMathJax(printModelLatex(input$usr_model_saved))
  })
  
  observeEvent(input$usr_model_button_delete, {
    for (i in input$usr_model_saved)
      usr_models$model[i] <<- NULL
  })
  
  
  ###Display available data
  output$database3 <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = TRUE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"From"=NA, "To"=NA)
      return(NoData[-1,])
    }
    return (yuimaGUItable$series)
  })
  
  ###Table of selected data to model
  seriesToEstimate <- reactiveValues(table=data.frame())
  
  ###Select Button
  observeEvent(input$buttonSelect_models_Univariate, priority = 1, {
    seriesToEstimate$table <<- rbind(seriesToEstimate$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% input$database3_rows_selected) & !(rownames(yuimaGUItable$series) %in% rownames(seriesToEstimate$table)),])
  })
 
  ###SelectAll Button
  observeEvent(input$buttonSelectAll_models_Univariate, priority = 1, {
    seriesToEstimate$table <<- rbind(seriesToEstimate$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% input$database3_rows_all) & !(rownames(yuimaGUItable$series) %in% rownames(seriesToEstimate$table)),])
  })
  
  ###Display Selected Data
  output$database4 <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollY = 150, scrollCollapse = FALSE, deferRender = TRUE, dom = 'frtS'), extensions = 'Scroller', rownames = FALSE, selection = "multiple",{
    if (length(seriesToEstimate$table)==0){
      NoData <- data.frame("Symb"=NA,"From"=NA, "To"=NA)
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
      seriesToEstimate$table <<- seriesToEstimate$table[-which(rownames(seriesToEstimate$table) %in% input$database4_rows_selected),]
  })
  
  ###DeleteAll Button
  observeEvent(input$buttonDeleteAll_models_Univariate, priority = 1,{
    seriesToEstimate$table <<- seriesToEstimate$table[-which(rownames(seriesToEstimate$table) %in% input$database4_rows_all),]  
  })
  
  ###Interactive range of selectRange chart
  range_selectRange <- reactiveValues(x=NULL, y=NULL)
  observe({
    if (!is.null(input$selectRange_brush)){
      range_selectRange$x <- c(as.Date(input$selectRange_brush$xmin), as.Date(input$selectRange_brush$xmax))
      range_selectRange$y <- c(input$selectRange_brush$ymin, input$selectRange_brush$ymax)
    }
  })
 
  
  observe({
    shinyjs::toggle(id="plotsRangeErrorMessage", condition = length(rownames(seriesToEstimate$table))==0)
    shinyjs::toggle(id="plotsRangeAll", condition = length(rownames(seriesToEstimate$table))!=0)
  })
  
  ###Display charts: series and its increments
  observe({
    symb <- input$plotsRangeSeries
    if(!is.null(symb))
      if (symb %in% rownames(yuimaGUItable$series)){
        data <- getData(symb)
        incr <- Delt(data, type = "arithmetic")
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
          if ((symb %in% rownames(yuimaGUItable$series) & (symb %in% rownames(seriesToEstimate$table)))){
            par(bg="black")
            plot.zoo( window(incr, start = range_selectRange$x[1], end = range_selectRange$x[2]), main=paste(symb, " - Percentage Increments"), xlab="Index", ylab=NA, log=switch(input$scale_selectRange,"Linear"="","Logarithmic (Y)"="", "Logarithmic (X)"="x", "Logarithmic (XY)"="x"), col="grey", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
            lines(window(incr, start = start,  end = end), col = "green")
            grid(col="grey")
          }
        })
      }
  })
  
  
  output$plotsRangeSeries <- renderUI({
    selectInput("plotsRangeSeries", label = "Series", choices = input$database4_rows_all)
  })
  
  ###Choose Range input set to "Select range from charts" if charts have been brushed
  output$chooseRange <- renderUI({
    sel <- "full"
    if (!is.null(range_selectRange$x))
      sel <- "selected"
    selectInput("chooseRange", label = "Range", choices = c("Full Range" = "full", "Select range from charts" = "selected"), selected = sel)
  })
  
  ###Function to update data range to use to estimate models
  updateRange_seriesToEstimate <- function(symb, range = c("full","selected"), type = c("Date", "numeric")){
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
              start <- as.integer(start)
              end <- as.integer(end)
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
    updateRange_seriesToEstimate(input$database4_rows_all, range = input$chooseRange, type = class(index(getData(input$plotsRangeSeries))))
  })
  
  
  
  observe({
    for (symb in input$database4_rows_all){
      if (is.null(deltaSettings[[symb]]))
        deltaSettings[[symb]] <<- 0.01
      for (modName in input$model){
        if (is.null(estimateSettings[[modName]]))
          estimateSettings[[modName]] <<- list()
        if (is.null(estimateSettings[[modName]][[symb]]))
          estimateSettings[[modName]][[symb]] <<- list()
        if (is.null(estimateSettings[[modName]][[symb]][["fixed"]]))
          estimateSettings[[modName]][[symb]][["fixed"]] <<- list()
        if (is.null(estimateSettings[[modName]][[symb]][["start"]]))
          estimateSettings[[modName]][[symb]][["start"]] <<- list()
        if (is.null(estimateSettings[[modName]][[symb]][["startMin"]]))
          estimateSettings[[modName]][[symb]][["startMin"]] <<- defaultStart(modName, 100)$min
        if (is.null(estimateSettings[[modName]][[symb]][["startMax"]]))
          estimateSettings[[modName]][[symb]][["startMax"]] <<- defaultStart(modName, 100)$max
        if (is.null(estimateSettings[[modName]][[symb]][["upper"]]))
          estimateSettings[[modName]][[symb]][["upper"]] <<- defaultBounds(modName)$upper
        if (is.null(estimateSettings[[modName]][[symb]][["lower"]]))
          estimateSettings[[modName]][[symb]][["lower"]] <<- defaultBounds(modName)$lower
        if (is.null(estimateSettings[[modName]][[symb]][["method"]]))
          estimateSettings[[modName]][[symb]][["method"]] <<- "L-BFGS-B"
        if (is.null(estimateSettings[[modName]][[symb]][["tries"]]))
          estimateSettings[[modName]][[symb]][["tries"]] <<- 1
        if (is.null(estimateSettings[[modName]][[symb]][["seed"]]))
          estimateSettings[[modName]][[symb]][["seed"]] <<- NA
        if (is.null(estimateSettings[[modName]][[symb]][["joint"]]))
          estimateSettings[[modName]][[symb]][["joint"]] <<- FALSE
        if (is.null(estimateSettings[[modName]][[symb]][["aggregation"]]))
          estimateSettings[[modName]][[symb]][["aggregation"]] <<- TRUE
        if (is.null(estimateSettings[[modName]][[symb]][["threshold"]]))
          estimateSettings[[modName]][[symb]][["threshold"]] <<- NA
      }
    }
  })
  
  observe({
    shinyjs::toggle(id="advancedSettingsErrorMessage", condition = (is.null(input$database4_rows_all) | is.null(input$model)))
    shinyjs::toggle(id="advancedSettingsAll", condition = (!is.null(input$database4_rows_all) & !is.null(input$model)))
  })
  output$advancedSettingsSeries <- renderUI({
    if (!is.null(input$database4_rows_all))
      selectInput(inputId = "advancedSettingsSeries", label = "Series", choices = input$database4_rows_all)
  })
  output$advancedSettingsDelta <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      return (numericInput("advancedSettingsDelta", label = paste("delta", input$advancedSettingsSeries), value = deltaSettings[[input$advancedSettingsSeries]]))
  })
  output$advancedSettingsModel <- renderUI({
    if(!is.null(input$model))
      selectInput(inputId = "advancedSettingsModel", label = "Model", choices = input$model)
  })
  output$advancedSettingsParameter <- renderUI({
    if (!is.null(input$model))
      if (!is.null(input$advancedSettingsModel))
        selectInput(inputId = "advancedSettingsParameter", label = "Parameter", choices = setModelByName(input$advancedSettingsModel)@parameter@all)
  })
  output$advancedSettingsFixed <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      numericInput(inputId = "advancedSettingsFixed", label = "fixed", value = ifelse(is.null(estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]]),NA,estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]]))
  })
  output$advancedSettingsStart <- renderUI({
    if (!is.null(input$advancedSettingsFixed) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      if (is.na(input$advancedSettingsFixed))
        numericInput(inputId = "advancedSettingsStart", label = "start", value = ifelse(is.null(estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["start"]][[input$advancedSettingsParameter]]),NA,estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["start"]][[input$advancedSettingsParameter]]))
  })
  output$advancedSettingsStartMin <- renderUI({
    if (!is.null(input$advancedSettingsFixed) & !is.null(input$advancedSettingsStart) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      if (is.na(input$advancedSettingsFixed) & is.na(input$advancedSettingsStart))
        numericInput(inputId = "advancedSettingsStartMin", label = "start: Min", value = ifelse(is.null(estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMin"]][[input$advancedSettingsParameter]]),-10,estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMin"]][[input$advancedSettingsParameter]]))
  })
  output$advancedSettingsStartMax <- renderUI({
    if (!is.null(input$advancedSettingsFixed) & !is.null(input$advancedSettingsStart) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      if (is.na(input$advancedSettingsFixed) & is.na(input$advancedSettingsStart))
        numericInput(inputId = "advancedSettingsStartMax", label = "start: Max", value = ifelse(is.null(estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMax"]][[input$advancedSettingsParameter]]),10,estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMax"]][[input$advancedSettingsParameter]]))
  })
  output$advancedSettingsLower <- renderUI({
    if (!is.null(input$advancedSettingsFixed) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      if (is.na(input$advancedSettingsFixed))
        if (input$advancedSettingsMethod=="L-BFGS-B" | input$advancedSettingsMethod=="Brent")
          numericInput("advancedSettingsLower", label = "lower", value = ifelse(is.null(estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["lower"]][[input$advancedSettingsParameter]]),NA,estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["lower"]][[input$advancedSettingsParameter]]))
  })
  output$advancedSettingsUpper <- renderUI({
    if (!is.null(input$advancedSettingsFixed) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      if (is.na(input$advancedSettingsFixed))
        if (input$advancedSettingsMethod=="L-BFGS-B" | input$advancedSettingsMethod=="Brent")
          numericInput("advancedSettingsUpper", label = "upper", value = ifelse(is.null(estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["upper"]][[input$advancedSettingsParameter]]),NA,estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["upper"]][[input$advancedSettingsParameter]]))
  })
  output$advancedSettingsJoint <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      selectInput("advancedSettingsJoint", label = "joint", choices = c(FALSE, TRUE), selected = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["joint"]])
  })
  output$advancedSettingsMethod <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      selectInput("advancedSettingsMethod", label = "method", choices = c("L-BFGS-B", "Nelder-Mead", "BFGS", "CG", "SANN", "Brent"), selected = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]])
  })
  output$advancedSettingsAggregation <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      selectInput("advancedSettingsAggregation", label = "aggregation", choices = c(TRUE, FALSE), selected = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["aggregation"]])
  })
  output$advancedSettingsThreshold <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      numericInput("advancedSettingsThreshold", label = "threshold", value = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["threshold"]])
  })
  output$advancedSettingsTries <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsMethod))
      numericInput("advancedSettingsTries", label = "tries", min = 1, value = ifelse(input$advancedSettingsMethod=="SANN" & estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]]!="SANN",1,estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["tries"]]))
  })
  output$advancedSettingsSeed <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      numericInput("advancedSettingsSeed", label = "seed", min = 1, value = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["seed"]])
  })
  
  
  
  observeEvent(input$advancedSettingsButtonApplyDelta, {
      deltaSettings[[input$advancedSettingsSeries]] <<- input$advancedSettingsDelta
  })
  observeEvent(input$advancedSettingsButtonApplyAllDelta, {
    for (symb in input$database4_rows_all)
      deltaSettings[[symb]] <<- input$advancedSettingsDelta
  })
  observeEvent(input$advancedSettingsButtonApplyModel,{
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsFixed
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["start"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed),NA,input$advancedSettingsStart)
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMin"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed) | !is.na(input$advancedSettingsStart),NA,input$advancedSettingsStartMin)
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMax"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed) | !is.na(input$advancedSettingsStart),NA,input$advancedSettingsStartMax)
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["lower"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed),NA,input$advancedSettingsLower)
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["upper"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed),NA,input$advancedSettingsUpper)
  })
  observeEvent(input$advancedSettingsButtonApplyAllModel,{
    for (symb in input$database4_rows_all){
      estimateSettings[[input$advancedSettingsModel]][[symb]][["fixed"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsFixed
      estimateSettings[[input$advancedSettingsModel]][[symb]][["start"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed),NA,input$advancedSettingsStart)
      estimateSettings[[input$advancedSettingsModel]][[symb]][["startMin"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed) | !is.na(input$advancedSettingsStart),NA,input$advancedSettingsStartMin)
      estimateSettings[[input$advancedSettingsModel]][[symb]][["startMax"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed) | !is.na(input$advancedSettingsStart),NA,input$advancedSettingsStartMax)
      estimateSettings[[input$advancedSettingsModel]][[symb]][["lower"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed),NA,input$advancedSettingsLower)
      estimateSettings[[input$advancedSettingsModel]][[symb]][["upper"]][[input$advancedSettingsParameter]] <<- ifelse(!is.na(input$advancedSettingsFixed),NA,input$advancedSettingsUpper)
    }
  })
  observeEvent(input$advancedSettingsButtonApplyGeneral,{
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]] <<- input$advancedSettingsMethod 
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["tries"]] <<- input$advancedSettingsTries
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["seed"]] <<- input$advancedSettingsSeed
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["joint"]] <<- input$advancedSettingsJoint 
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["aggregation"]] <<- input$advancedSettingsAggregation 
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["threshold"]] <<- input$advancedSettingsThreshold 
  })
  observeEvent(input$advancedSettingsButtonApplyAllModelGeneral,{
    for (symb in input$database4_rows_all){
      estimateSettings[[input$advancedSettingsModel]][[symb]][["method"]] <<- input$advancedSettingsMethod 
      estimateSettings[[input$advancedSettingsModel]][[symb]][["tries"]] <<- input$advancedSettingsTries
      estimateSettings[[input$advancedSettingsModel]][[symb]][["seed"]] <<- input$advancedSettingsSeed
      estimateSettings[[input$advancedSettingsModel]][[symb]][["joint"]] <<- input$advancedSettingsJoint 
      estimateSettings[[input$advancedSettingsModel]][[symb]][["aggregation"]] <<- input$advancedSettingsAggregation 
      estimateSettings[[input$advancedSettingsModel]][[symb]][["threshold"]] <<- input$advancedSettingsThreshold 
    }
  })
  observeEvent(input$advancedSettingsButtonApplyAllGeneral,{
    for (mod in input$model){
      for (symb in input$database4_rows_all){
        estimateSettings[[mod]][[symb]][["method"]] <<- input$advancedSettingsMethod 
        estimateSettings[[mod]][[symb]][["tries"]] <<- input$advancedSettingsTries
        estimateSettings[[mod]][[symb]][["seed"]] <<- input$advancedSettingsSeed
        estimateSettings[[mod]][[symb]][["joint"]] <<- input$advancedSettingsJoint 
        estimateSettings[[mod]][[symb]][["aggregation"]] <<- input$advancedSettingsAggregation 
        estimateSettings[[mod]][[symb]][["threshold"]] <<- input$advancedSettingsThreshold 
      }
    }
  })
  
  
  
  ###Estimate models
  observeEvent(input$EstimateModels,{
    if(is.null(input$model) | length(rownames(seriesToEstimate$table))==0 | is.null(rownames(seriesToEstimate$table))){
      createAlert(session = session, anchorId = "modelsAlert", alertId = "modelsAlert_err", content = "Select some series and models to estimate", style = "warning")
    }
    else{
      withProgress(message = 'Estimating: ',{
        for (modName in input$model){
          for (i in rownames(seriesToEstimate$table)){
            symb <- as.character(seriesToEstimate$table[i,"Symb"])
            incProgress(1/(length(input$model)*length(rownames(seriesToEstimate$table))), detail = paste(symb,"-",modName))
            data <- getData(symb)
            start <- as.character(seriesToEstimate$table[i,"From"])
            end <- as.character(seriesToEstimate$table[i,"To"])
            if (class(index(data))=="numeric")
              data <- window(data, start = as.numeric(start), end = as.numeric(end))
            else
              data <- window(data, start = start, end = end)
            addModel(
              modName = modName, 
              symbName = symb, 
              data = data, 
              delta = deltaSettings[[symb]], 
              start = estimateSettings[[modName]][[symb]][["start"]],
              startMin = estimateSettings[[modName]][[symb]][["startMin"]],
              startMax = estimateSettings[[modName]][[symb]][["startMax"]],
              method=estimateSettings[[modName]][[symb]][["method"]], 
              tries=estimateSettings[[modName]][[symb]][["tries"]], 
              seed = estimateSettings[[modName]][[symb]][["seed"]], 
              fixed = estimateSettings[[modName]][[symb]][["fixed"]], 
              lower = estimateSettings[[modName]][[symb]][["lower"]], 
              upper = estimateSettings[[modName]][[symb]][["upper"]], 
              joint = estimateSettings[[modName]][[symb]][["joint"]], 
              aggregation = estimateSettings[[modName]][[symb]][["aggregation"]], 
              threshold = estimateSettings[[modName]][[symb]][["threshold"]],
              session = session,
              anchorId = "modelsAlert",
              alertId = "modelsAlert_qmle"
            )
          }
        }
      })
      updateTabsetPanel(session = session,  inputId = "panel_estimates", selected = "Estimates")
    }
  })
  
  observe({
    if(!is.null(input$model) & length(rownames(seriesToEstimate$table))!=0 & !is.null(rownames(seriesToEstimate$table)))
      closeAlert(session, alertId = "modelsAlert_err")
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
  output$databaseModels <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = TRUE, deferRender = TRUE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
    if (length(yuimaGUItable$model)==0){
      NoData <- data.frame("Symb"=NA,"From"=NA, "To"=NA)
      return(NoData[-1,])
    }
    return (yuimaGUItable$model)
  })
  
  rowToPrint <- reactiveValues(id = NULL)
  observe(priority = 1, {
    rowToPrint$id <<- NULL
    if(!is.null(input$databaseModels_rows_all) & !is.null(input$baseModels))
      if(tail(input$databaseModels_rows_all,1) %in% rownames(yuimaGUItable$model))
        rowToPrint$id <<- tail(input$databaseModels_rows_all,1)
    if (!is.null(input$databaseModels_row_last_clicked) & !is.null(input$baseModels))
      if (input$databaseModels_row_last_clicked %in% rownames(yuimaGUItable$model))
        rowToPrint$id <<- input$databaseModels_row_last_clicked
  })
  
  ###Print estimated model in Latex
  output$estimatedModelsLatex <- renderUI({
    if (!is.null(rowToPrint$id))
      withMathJax(printModelLatex(as.character(yuimaGUItable$model[rowToPrint$id, "Model"])))
  })
  
  ###Print Symbol
  output$SymbolName <- renderText({
    if (!is.null(rowToPrint$id))
      unlist(strsplit(rowToPrint$id, split = " "))[1]
      
  })
  
  ###More Info
  output$text_MoreInfo <- renderUI({
    id <- unlist(strsplit(rowToPrint$id, split = " "))
    info <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info
    div(
      h3(id[1], " - " , info$modName),
      h4(
        em("delta:"), info$delta, br(),
        em("method:"), info$method, br(),
        em("tries:"), info$tries, br(),
        em("seed:"), info$seed, br(),
        em("joint:"), info$joint, br(),
        em("aggregation:"), info$aggregation, br(),
        em("threshold:"), info$threshold
      ),
      align="center"
    )
  })

  output$table_MoreInfo <- renderTable(digits=6,{
    id <- unlist(strsplit(rowToPrint$id, split = " "))
    coef <- as.data.frame(t(summary(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$qmle)@coef))
    info <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info
    params <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@model@parameter@all
    lower <- data.frame(info$lower)
    upper <- data.frame(info$upper)
    fixed <- data.frame(info$fixed)
    start <- data.frame(info$start)
    startMin <- data.frame(info$startMin)
    startMax <- data.frame(info$startMax)
    if(length(lower)==0) lower[1,params[1]] <- NA
    if(length(upper)==0) upper[1,params[1]] <- NA
    if(length(fixed)==0) fixed[1,params[1]] <- NA
    if(length(start)==0) start[1,params[1]] <- NA
    if(length(startMin)==0) startMin[1,params[1]] <- NA
    if(length(startMax)==0) startMax[1,params[1]] <- NA
    table <- rbind.fill(coef, fixed, start, startMin, startMax, lower, upper)
    rownames(table) <- c("Estimate", "Std. Error", "fixed", "start", "startMin", "startMax", "lower", "upper")
    return(table)
  })
  
  
  ###Function to manipulate digits
  signifDigits <- function(value, sd){
    if (is.na(sd) | sd=="NaN")
      return (value)
    else{
      pow <- 10^(1-as.integer(log10(as.numeric(sd))))
      return(round(as.numeric(value)*pow)/pow)
    }
  } 
  
  ###Print estimates
  observe({
    if (!is.null(rowToPrint$id)){
      symb <- unlist(strsplit(rowToPrint$id, split = " "))[1]
      modN <- as.numeric(unlist(strsplit(rowToPrint$id, split = " "))[2])
      table <- t(summary(yuimaGUIdata$model[[symb]][[modN]]$qmle)@coef)
      outputTable <- data.frame()
      for (param in unique(colnames(table))){
        temp <- changeBase(param = as.numeric(table["Estimate",param]), StdErr = as.numeric(table["Std. Error",param]), delta = yuimaGUIdata$model[[symb]][[modN]]$model@sampling@delta, original.data = yuimaGUIdata$model[[symb]][[modN]]$model@data@original.data, paramName = param, modelName = yuimaGUIdata$model[[symb]][[modN]]$info$modName, newBase = input$baseModels, session = session, choicesUI="baseModels", anchorId = "modelsAlert", alertId = "modelsAlert_conversion")
        outputTable["Estimate",param] <- as.character(signifDigits(temp[["Estimate"]],temp[["Std. Error"]]))
        outputTable["Std. Error",param] <- as.character(signifDigits(temp[["Std. Error"]],temp[["Std. Error"]]))
      }
      colnames(outputTable) <- unique(colnames(table))
      output$estimatedModelsTable <- renderTable({
        if (!is.null(rowToPrint$id))
          return(outputTable)
      })
    }
  })
  
  observe({
    shinyjs::toggle("estimates_info", condition = !is.null(input$databaseModels_rows_all))
  })
  
  observe({
    shinyjs::toggle("modelsAlert_err", condition = (input$panel_estimates=="Start estimation"))
    shinyjs::toggle("modelsAlert_conversion", condition = (input$panel_estimates=="Estimates"))
    shinyjs::toggle("usr_model_saved_div", condition = length(names(usr_models$model))!=0)
    shinyjs::toggle("alert_savingModels", condition = (input$panel_estimates=="Set model"))
  })

  ###Delete Model
  observeEvent(input$databaseModelsDelete, priority = 1, {
    if(!is.null(input$databaseModels_rows_selected) & !is.null(input$databaseModels_row_last_clicked)){
      if(input$databaseModels_row_last_clicked %in% input$databaseModels_rows_selected){
        rowname <- unlist(strsplit(input$databaseModels_row_last_clicked, split = " " , fixed = FALSE))
        delModel(symb=rowname[1], n=rowname[2])
        closeAlert(session, alertId = "modelsAlert_conversion")
      }
    }
  })
  
  ###DeleteAll Model
  observeEvent(input$databaseModelsDeleteAll, priority = 1, {
    if(!is.null(input$databaseModels_rows_all)){
      closeAlert(session, alertId = "modelsAlert_conversion")
      rowname <- unlist(strsplit(input$databaseModels_rows_all, split = " " , fixed = FALSE))
      delModel(symb=rowname[seq(1,length(rowname),2)], n=rowname[seq(2,length(rowname),2)])
    }
  })
  
  
  
  
  
  
  
  
  
  
  ########################Simulation
  ########################
  ########################
  
  output$simulate_databaseModels <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = TRUE, deferRender = TRUE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
    if (length(yuimaGUItable$model)==0){
      NoData <- data.frame("Symb"=NA,"From"=NA, "To"=NA)
      return(NoData[-1,])
    }
    return (yuimaGUItable$model)
  })
  
  modelsToSimulate <- reactiveValues(table=data.frame())
  
  ###Select Button
  observeEvent(input$simulate_button_selectModels, priority = 1, {
    modelsToSimulate$table <<- rbind.fill(modelsToSimulate$table, yuimaGUItable$model[(rownames(yuimaGUItable$model) %in% input$simulate_databaseModels_rows_selected) & !(rownames(yuimaGUItable$model) %in% rownames(modelsToSimulate$table)),])
  })
  
  ###SelectAll Button
  observeEvent(input$simulate_button_selectAllModels, priority = 1, {
    modelsToSimulate$table <<- rbind.fill(modelsToSimulate$table, yuimaGUItable$model[(rownames(yuimaGUItable$model) %in% input$simulate_databaseModels_rows_all) & !(rownames(yuimaGUItable$model) %in% rownames(modelsToSimulate$table)),])
  })
  
  output$simulate_model_usr_selectModel <- renderUI({
    selectInput("simulate_model_usr_selectModel", label = "Model Name", choices = c(as.vector(defaultModels),names(usr_models$model)))
  })
  
  output$simulate_model_usr_selectParam <- renderUI({
    if (!is.null(input$simulate_model_usr_selectModel))
      selectInput("simulate_model_usr_selectParam", label = "Parameter", choices = setModelByName(input$simulate_model_usr_selectModel)@parameter@all)
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
      if (is.null(usr_models$simulation[[id]])){
        usr_models$simulation[[id]] <<- list()
      }
      usr_models$simulation[[id]][["Model"]] <<- input$simulate_model_usr_selectModel
      if (is.null(usr_models$simulation[[id]][["true.param"]])){
        usr_models$simulation[[id]][["true.param"]] <<- list()
      }
      allparam <- setModelByName(input$simulate_model_usr_selectModel)@parameter@all
      if (length(allparam)==0)
        usr_models$simulation[[id]]["true.param"] <<- NULL
      if (length(allparam)!=0){
        for(i in c(allparam, names(usr_models$simulation[[id]][["true.param"]]))){
          if (!(i %in% names(usr_models$simulation[[id]][["true.param"]])))
            usr_models$simulation[[id]][["true.param"]][[i]] <<- "MISSING"
          if(!(i %in% allparam))
            usr_models$simulation[[id]][["true.param"]][i] <<- NULL
        }
      }
    }
  })
  
  observe({
    if (!is.null(input$simulate_model_usr_ID) & !is.null(input$simulate_model_usr_selectParam) & !is.null(input$simulate_model_usr_param)){
      id <- gsub(pattern = " ", x = input$simulate_model_usr_ID, replacement = "")
      if (!is.null(usr_models$simulation[[id]]))
        if(usr_models$simulation[[id]][["Model"]]==input$simulate_model_usr_selectModel & input$simulate_model_usr_selectParam!="")
          usr_models$simulation[[id]][["true.param"]][[input$simulate_model_usr_selectParam]] <- ifelse(is.na(input$simulate_model_usr_param),"MISSING",input$simulate_model_usr_param) 
    }
  })
  
  observe({
    for(i in names(usr_models$simulation))
      if (!(usr_models$simulation[[i]]$Model %in% c(defaultModels, names(usr_models$model))))
          usr_models$simulation[i] <<- NULL
  })
  
  output$simulate_model_usr_table <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollY = 150, scrollCollapse = FALSE, deferRender = TRUE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
    table <- data.frame()
    for (i in names(usr_models$simulation)){
      newRow <- as.data.frame(usr_models$simulation[[i]])
      colnames(newRow) <- gsub(pattern = "true.param.", x = colnames(newRow), replacement = "")
      table <- rbind.fill(table, newRow)
    }
    if (length(table)==0){
      NoData <- data.frame("Model"=NA, "parameters"=NA)
      return(NoData[-1,])
    }
    return (data.frame(table, row.names = names(usr_models$simulation)))
  })
  
  observeEvent(input$simulate_model_usr_button_select, {
    if (!is.null(input$simulate_model_usr_table_rows_selected)){
      table <- data.frame()
      for (i in input$simulate_model_usr_table_rows_selected){
        if ("MISSING" %in% usr_models$simulation[[i]][["true.param"]]){
          createAlert(session = session, anchorId = "simulate_alert", alertId = "simulate_alert_usr_button_select", content = "There are still missing values in selected models", style = "error")
        }
        else {
          closeAlert(session, "simulate_alert_usr_button_select")
          newRow <- as.data.frame(usr_models$simulation[[i]], row.names=i)
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
      for (i in input$simulate_model_usr_table_rows_all){
        if ("MISSING" %in% usr_models$simulation[[i]][["true.param"]]){
          createAlert(session = session, anchorId = "simulate_alert", alertId = "simulate_alert_usr_button_select", content = "There are still missing values in selected models", style = "error")
        }
        else {
          closeAlert(session, "simulate_alert_usr_button_select")
          newRow <- as.data.frame(usr_models$simulation[[i]], row.names=i)
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
        usr_models$simulation[i] <- NULL
      }
    }
  })
  
  observeEvent(input$simulate_model_usr_button_deleteAll, {
    if (!is.null(input$simulate_model_usr_table_rows_all)){
      for (i in input$simulate_model_usr_table_rows_all){
        usr_models$simulation[i] <- NULL
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
      names.valid <- c(names(usr_models$simulation), rownames(yuimaGUItable$model))
      col <- colnames(modelsToSimulate$table)
      updatedtable <<- data.frame(modelsToSimulate$table[which(rownames(modelsToSimulate$table) %in% names.valid),], row.names = rownames(modelsToSimulate$table)[rownames(modelsToSimulate$table) %in% names.valid])
      colnames(updatedtable) <- col  
      modelsToSimulate$table <<- updatedtable
    }
  })
  
  output$simulate_selectedModels <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollY = 150, scrollCollapse = FALSE, deferRender = TRUE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
    if (length(modelsToSimulate$table)==0){
      NoData <- data.frame("Symb"=NA,"From"=NA, "To"=NA)
      return(NoData[-1,])
    }
    return (modelsToSimulate$table)
  })
  
  ###Delete Button
  observeEvent(input$simulation_button_deleteModels, priority = 1,{
    if (!is.null(input$simulate_selectedModels_rows_selected))
      modelsToSimulate$table <<- modelsToSimulate$table[-which(rownames(modelsToSimulate$table) %in% input$simulate_selectedModels_rows_selected),]
  })
  
  ###DeleteAll Button
  observeEvent(input$simulation_button_deleteAllModels, priority = 1,{
    if (!is.null(input$simulate_selectedModels_rows_all))
      modelsToSimulate$table <<- modelsToSimulate$table[-which(rownames(modelsToSimulate$table) %in% input$simulate_selectedModels_rows_all),]
  })

  observe({
    shinyjs::toggle(id="simulate_setSimulation_errorMessage", condition = length(rownames(modelsToSimulate$table))==0)
    shinyjs::toggle(id="simulate_setSimulation_body", condition = length(rownames(modelsToSimulate$table))!=0)
  })
  observe({
    shinyjs::toggle(id="simulate_advancedSettings_errorMessage", condition = length(rownames(modelsToSimulate$table))==0)
    shinyjs::toggle(id="simulate_advancedSettings_body", condition = length(rownames(modelsToSimulate$table))!=0)
  })
  
  simulateSettings <- list()
  observe({
    for (modID in input$simulate_selectedModels_rows_all){
      if (modID %in% names(usr_models$simulation)){
        if (is.null(simulateSettings[[modID]]))
          simulateSettings[[modID]] <<- list()
        if (is.null(simulateSettings[[modID]][["xinit"]]))
          simulateSettings[[modID]][["xinit"]] <<- 1
        if (is.null(simulateSettings[[modID]][["nstep"]]))
          simulateSettings[[modID]][["nstep"]] <<- NA
        if (is.null(simulateSettings[[modID]][["nsim"]]))
          simulateSettings[[modID]][["nsim"]] <<- 1
        if (is.null(simulateSettings[[modID]][["t0"]]))
          simulateSettings[[modID]][["t0"]] <<- 0
        if (is.null(simulateSettings[[modID]][["t1"]]))
          simulateSettings[[modID]][["t1"]] <<- 1
        if (is.null(simulateSettings[[modID]][["traj"]]))
          simulateSettings[[modID]][["traj"]] <<- TRUE
        if (is.null(simulateSettings[[modID]][["seed"]]))
          simulateSettings[[modID]][["seed"]] <<- NA
      }
      if (modID %in% rownames(yuimaGUItable$model)){
        id <- unlist(strsplit(modID, split = " "))
        if (is.null(simulateSettings[[modID]]))
          simulateSettings[[modID]] <<- list()
        if (is.null(simulateSettings[[modID]][["xinit"]]))
          simulateSettings[[modID]][["xinit"]] <<- as.numeric(tail(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data,1))
        if (is.null(simulateSettings[[modID]][["nstep"]]))
          simulateSettings[[modID]][["nstep"]] <<- NA
        if (is.null(simulateSettings[[modID]][["nsim"]]))
          simulateSettings[[modID]][["nsim"]] <<- 1
        if (is.null(simulateSettings[[modID]][["t0"]]))
          simulateSettings[[modID]][["t0"]] <<- end(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)
        if (is.null(simulateSettings[[modID]][["t1"]]))
          simulateSettings[[modID]][["t1"]] <<- end(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data)+365
        if (is.null(simulateSettings[[modID]][["traj"]]))
          simulateSettings[[modID]][["traj"]] <<- TRUE
        if (is.null(simulateSettings[[modID]][["seed"]]))
          simulateSettings[[modID]][["seed"]] <<- NA
      }
    }
  })
  
  output$simulate_modelID <- renderUI({
    if(!is.null(input$simulate_selectedModels_rows_all))
      selectInput("simulate_modelID", label = "Simulation ID", choices = input$simulate_selectedModels_rows_all)
  })
  
  output$simulate_advancedSettings_modelID <- renderUI({
    if(!is.null(input$simulate_selectedModels_rows_all))
      selectInput("simulate_advancedSettings_modelID", label = "Simulation ID", choices = input$simulate_selectedModels_rows_all)
  })
  
  output$simulate_seed <- renderUI({
    if(!is.null(input$simulate_advancedSettings_modelID))
      numericInput("simulate_seed", label = "RNG seed", step = 1, min = 0, value = simulateSettings[[input$simulate_advancedSettings_modelID]][["seed"]])
  })
  
  output$simulate_traj <- renderUI({
    if(!is.null(input$simulate_advancedSettings_modelID))
      selectInput("simulate_traj", label = "Save trajectory", choices = c(TRUE,FALSE), selected = simulateSettings[[input$simulate_advancedSettings_modelID]][["traj"]])
  })
  
  output$simulate_nsim <- renderUI({
    if(!is.null(input$simulate_modelID))
      numericInput("simulate_nsim", label = "Number of simulations", value = simulateSettings[[input$simulate_modelID]][["nsim"]], min = 1, step = 1)
  })
  
  output$simulate_nstep <- renderUI({
    if(!is.null(input$simulate_modelID))
      numericInput("simulate_nstep", label = "Number of steps per simulation", value = simulateSettings[[input$simulate_modelID]][["nstep"]], min = 1, step = 1)
  })
  
  output$simulate_xinit <- renderUI({
    if(!is.null(input$simulate_modelID))
      numericInput("simulate_xinit", label = "Initial value", value = simulateSettings[[input$simulate_modelID]][["xinit"]])
  })
  
  output$simulate_range <- renderUI({
    if(!is.null(input$simulate_modelID)){
      if (input$simulate_modelID %in% names(usr_models$simulation)){
        return(div(
          column(6,numericInput("simulate_rangeNumeric_t0", label = "From", min = 0 ,value = simulateSettings[[input$simulate_modelID]][["t0"]])),
          column(6,numericInput("simulate_rangeNumeric_t1", label = "To", min = 0, value = simulateSettings[[input$simulate_modelID]][["t1"]]))
        ))
      }
      if (input$simulate_modelID %in% rownames(yuimaGUItable$model)){
        id <- unlist(strsplit(input$simulate_modelID, split = " "))
        if (class(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date")
          dateRangeInput("simulate_rangeDate", label = "Simulation interval", start = simulateSettings[[input$simulate_modelID]][["t0"]], end = simulateSettings[[input$simulate_modelID]][["t1"]])
        else{ 
          div(
            column(6,numericInput("simulate_rangeNumeric_t0", label = "From", min = 0 ,value = simulateSettings[[input$simulate_modelID]][["t0"]])),
            column(6,numericInput("simulate_rangeNumeric_t1", label = "To", min = 0, value = simulateSettings[[input$simulate_modelID]][["t1"]]))
          )
        }
      }
    }
  })
  
  observeEvent(input$simulate_button_apply_advancedSettings, {
    simulateSettings[[input$simulate_advancedSettings_modelID]][["seed"]] <<- input$simulate_seed
    simulateSettings[[input$simulate_advancedSettings_modelID]][["traj"]] <<- input$simulate_traj
  })
  observeEvent(input$simulate_button_applyAll_advancedSettings, {
    for (modID in input$simulate_selectedModels_rows_all){
      simulateSettings[[modID]][["seed"]] <<- input$simulate_seed
      simulateSettings[[modID]][["traj"]] <<- input$simulate_traj
    }
  })
  observeEvent(input$simulate_button_apply_nsim, {
    simulateSettings[[input$simulate_modelID]][["nsim"]] <<- input$simulate_nsim
    simulateSettings[[input$simulate_modelID]][["nstep"]] <<- input$simulate_nstep
  })
  observeEvent(input$simulate_button_applyAll_nsim, {
    for (modID in input$simulate_selectedModels_rows_all){
      simulateSettings[[modID]][["nsim"]] <<- input$simulate_nsim
      simulateSettings[[modID]][["nstep"]] <<- input$simulate_nstep
    }
  })
  observeEvent(input$simulate_button_apply_xinit, {
    simulateSettings[[input$simulate_modelID]][["xinit"]] <<- input$simulate_xinit
  })
  observeEvent(input$simulate_button_applyAll_xinit, {
    for (modID in input$simulate_selectedModels_rows_all)
      simulateSettings[[modID]][["xinit"]] <<- input$simulate_xinit
  })
  observeEvent(input$simulate_button_apply_range, {
    if (input$simulate_modelID %in% names(usr_models$simulation)){
      simulateSettings[[input$simulate_modelID]][["t0"]] <<- input$simulate_rangeNumeric_t0
      simulateSettings[[input$simulate_modelID]][["t1"]] <<- input$simulate_rangeNumeric_t1
    }
    if (input$simulate_modelID %in% rownames(yuimaGUItable$model)){
      id <- unlist(strsplit(input$simulate_modelID, split = " "))
      if (class(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date"){
        simulateSettings[[input$simulate_modelID]][["t0"]] <<- input$simulate_rangeDate[1]
        simulateSettings[[input$simulate_modelID]][["t1"]] <<- input$simulate_rangeDate[2]
      }
      else{
        simulateSettings[[input$simulate_modelID]][["t0"]] <<- input$simulate_rangeNumeric_t0
        simulateSettings[[input$simulate_modelID]][["t1"]] <<- input$simulate_rangeNumeric_t1
      }
    }
  })
  observeEvent(input$simulate_button_applyAll_range, {
    for (modID in input$simulate_selectedModels_rows_all){
      if (modID %in% names(usr_models$simulation)){
        simulateSettings[[modID]][["t0"]] <<- input$simulate_rangeNumeric_t0
        simulateSettings[[modID]][["t1"]] <<- input$simulate_rangeNumeric_t1
      }
      if (modID %in% rownames(yuimaGUItable$model)){
        id <- unlist(strsplit(modID, split = " "))
        if (class(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="Date" & !is.null(input$simulate_rangeDate)){
          simulateSettings[[modID]][["t0"]] <<- input$simulate_rangeDate[1]
          simulateSettings[[modID]][["t1"]] <<- input$simulate_rangeDate[2]
        }
        if (class(index(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data))=="numeric" & !is.null(input$simulate_rangeNumeric_t0)){
          simulateSettings[[modID]][["t0"]] <<- input$simulate_rangeNumeric_t0
          simulateSettings[[modID]][["t1"]] <<- input$simulate_rangeNumeric_t1
        }
      }
    }
  })
  
  observeEvent(input$simulate_simulateModels, {
    if (length(input$simulate_selectedModels_rows_all)==0){
      createAlert(session = session, anchorId = "simulate_alert", alertId = "simulate_alert_buttonEstimate", content = "Table 'Selected Models' is empty", style = "warning")
    }
    else{
      closeAlert(session, alertId = "simulate_alert_buttonEstimate")
      withProgress(message = 'Simulating: ', value = 0, {
        for (modID in input$simulate_selectedModels_rows_all){
          if(modID %in% names(usr_models$simulation)){
            incProgress(1/length(input$simulate_selectedModels_rows_all), detail = paste(modID,"-",usr_models$simulation[[modID]][["Model"]]))
            info <- list(
              "model" = usr_models$simulation[[modID]][["Model"]],
              "estimate.from" = NA,
              "estimate.to" = NA,
              "simulate.from" = as.numeric(simulateSettings[[modID]][["t0"]]),
              "simulate.to" = as.numeric(simulateSettings[[modID]][["t1"]]))
            Initial <- simulateSettings[[modID]][["t0"]]
            Terminal <- simulateSettings[[modID]][["t1"]]
            n <- ifelse(is.na(simulateSettings[[modID]][["nstep"]]),(Terminal-Initial)/0.01,simulateSettings[[modID]][["nstep"]])
            addSimulation(
              model = setModelByName(info$model),
              true.parameter = usr_models$simulation[[modID]][["true.param"]],
              symbName = modID,
              info = info,
              xinit = simulateSettings[[modID]][["xinit"]],
              nsim = simulateSettings[[modID]][["nsim"]],
              sampling = setSampling(Initial = Initial, Terminal = Terminal, n=n, delta = NA),
              saveTraj = simulateSettings[[modID]][["traj"]],
              seed = simulateSettings[[modID]][["seed"]],
              session = session,
              anchorId = "simulate_alert"
            )
          }
          if(modID %in% rownames(yuimaGUItable$model)){
            id <- unlist(strsplit(modID, split = " "))
            incProgress(1/length(input$simulate_selectedModels_rows_all), detail = paste(id[1],"-",yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$modName))
            data <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data
            if(class(index(data))=="Date"){
              info <- list(
                "model" = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$modName, 
                "estimate.from" = as.Date(start(data)),
                "estimate.to" = as.Date(end(data)),
                "simulate.from" = simulateSettings[[modID]][["t0"]],
                "simulate.to" = simulateSettings[[modID]][["t1"]])
              Initial <- 0
              Terminal <- as.numeric(simulateSettings[[modID]][["t1"]]-simulateSettings[[modID]][["t0"]])/as.numeric(end(data)-start(data))*length(data)*yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@sampling@delta
              n <- ifelse(is.na(simulateSettings[[modID]][["nstep"]]),as.numeric(simulateSettings[[modID]][["t1"]]-simulateSettings[[modID]][["t0"]])/as.numeric(end(data)-start(data))*length(data),simulateSettings[[modID]][["nstep"]])
            }
            if(class(index(data))=="numeric" | class(index(data))=="integer"){
              info <- list(
                "model" = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$modName, 
                "estimate.from" = as.numeric(start(data)),
                "estimate.to" = as.numeric(end(data)),
                "simulate.from" = as.numeric(simulateSettings[[modID]][["t0"]]),
                "simulate.to" = as.numeric(simulateSettings[[modID]][["t1"]]))
              Initial <- simulateSettings[[modID]][["t0"]]/(as.numeric(end(data))-as.numeric(start(data)))*length(data)*yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@sampling@delta
              Terminal <- simulateSettings[[modID]][["t1"]]/(as.numeric(end(data))-as.numeric(start(data)))*length(data)*yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@sampling@delta
              n <- ifelse(is.na(simulateSettings[[modID]][["nstep"]]),as.numeric(simulateSettings[[modID]][["t1"]]-simulateSettings[[modID]][["t0"]])/(as.numeric(end(data))-as.numeric(start(data)))*length(data),simulateSettings[[modID]][["nstep"]])
            }
            addSimulation(
              session = session,
              model = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@model,
              true.parameter = as.list(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$qmle@coef),
              symbName = id[1],
              info = info,
              xinit = simulateSettings[[modID]][["xinit"]],
              nsim = simulateSettings[[modID]][["nsim"]],
              sampling = setSampling(Initial = Initial, Terminal = Terminal, n=n, delta = NA),
              saveTraj = simulateSettings[[modID]][["traj"]],
              seed = simulateSettings[[modID]][["seed"]]
            )
          }
        }
      })
      updateTabsetPanel(session = session,  inputId = "panel_simulations", selected = "Simulations")
    }
  })
  
  observe({
    shinyjs::toggle("div_simulations", condition = (input$panel_simulations!="Simulations"))
    shinyjs::toggle("simulate_alert_buttonEstimate", condition = (input$panel_simulations!="Simulations"))
    shinyjs::toggle("simulate_alert_usr_button_select", condition = (input$panel_simulations=="Simulate equation"))
  })
  
  ###Create simulations table
  output$simulate_monitor_table <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = TRUE, deferRender = TRUE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
    if (length(yuimaGUItable$simulation)==0){
      NoData <- data.frame("Symb"=NA,"Model"=NA, "N sim" = NA, "Simulated from"=NA, "Simulated to"=NA, "Estimated from"=NA, "Estimated to"=NA)
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
        rowname <- unlist(strsplit(input$simulate_monitor_table_row_last_clicked, split = " " , fixed = FALSE))
        delSimulation(symb=rowname[1], n=rowname[2])
      }
    }
  })
  
  ###DeleteAll Simulation
  observeEvent(input$simulate_monitor_button_deleteAll, priority = 1, {
    if(!is.null(input$simulate_monitor_table_rows_all)){
      rowname <- unlist(strsplit(input$simulate_monitor_table_rows_all, split = " " , fixed = FALSE))
      delSimulation(symb=rowname[seq(1,length(rowname),2)], n=rowname[seq(2,length(rowname),2)])
    }
  })
  
  output$simulate_showSimulation_simID <- renderUI({
    selectInput(inputId = "simulate_showSimulation_simID", label = "Simulation ID", choices = input$simulate_monitor_table_rows_all)
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

  output$simulate_showSimulation_hist_nBins <- renderUI({
    if(!is.null(input$simulate_showSimulation_simID)){
      if(input$simulate_showSimulation_simID %in% rownames(yuimaGUItable$simulation)){
        id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
        Max <- yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$info$nsim
        sliderInput("simulate_showSimulation_hist_nBins", width = "75%",min = 1, max = as.integer(Max), step = 1,value = as.integer(Max/5),ticks = FALSE, round = TRUE, label = "Adjust bin width")
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
                grid()
              })
            }
         }
        }
      }
    }
  })
  
  
  output$simulate_showSimulation_hist_probability_slider <- renderUI({
    if(length(simulation_hist$values)!=0){
      Min <- min(simulation_hist$values)
      Max <- max(simulation_hist$values)
      sliderInput("simulate_showSimulation_hist_probability_slider", width = "75%",min = Min-0.0000001, max = Max+0.0000001, value = c(Min+0.25*(Max-Min),Min+0.75*(Max-Min)), label = "Probability", step = 0.001, ticks=FALSE)
    }
  })
  
  output$simulate_showSimulation_hist_probability_text <- renderText({
    if(length(simulation_hist$values)!=0 & !is.null(input$simulate_showSimulation_hist_probability_slider)){
      val <- as.numeric(simulation_hist$values)
      paste(as.character(100*sum(ifelse(val>=input$simulate_showSimulation_hist_probability_slider[1] & val<=input$simulate_showSimulation_hist_probability_slider[2],1,0))/length(val)),"%")
    }
  })

  
  ###Save Trajectory Button
  output$simulate_showSimulation_button_saveTrajectory <- {
    dataDownload_traj <- reactive({
      id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
      yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory
    })
    downloadHandler(
      filename = function() {
        paste(input$simulate_showSimulation_simID, ".txt", sep="")
      },
      content = function(file) {
        write.zoo(dataDownload_traj(), file, row.names = TRUE, col.names = FALSE, quote = TRUE)
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
  
  

}




                                                                                                                                                                                                                                                                                            