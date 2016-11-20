options(shiny.maxRequestSize = 9*1024^2)
 

server <- function(input, output, session) {

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
    switch(class, "Diffusion process" = NA, "Fractional process" = NA,"Compound Poisson" = jumps, "COGARCH"=NA, "CARMA" = NA)
  }
  
  ### Home
  output$video_intro <- renderUI({
    HTML('<iframe width="90%" height="250px" src="https://www.youtube.com/embed/XX_bmCrI_gc?rel=0" frameborder="0" allowfullscreen></iframe>')
  })
  
  output$certificates <- renderUI({
    div(align = "center",
      HTML('<div id="certificate1" style="display: inline-block;"><img src="seville2016.png" class="thumbnail" height="100" width="100" /></div>
            <div style="display: inline-block;"><img src="oviedo2016.png" class="thumbnail" height="100" width="100" /></div>')
    )
  })
  
  
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
      for(i in names(usr_models$model))
        if (usr_models$model[[i]]$class==input$modelClass) {
          if(input$modelClass!="Diffusion process") choices <- c(i, choices)
          else if (length(setModelByName(name = i)@parameter@all)!=0) choices <- c(i, choices)
        }
    return (selectInput("model",label = "Model Name", choices = choices, multiple = TRUE))
  })
  
  output$jumps <- renderUI({
    if (input$modelClass=="Compound Poisson")
      return(selectInput("jumps",label = "Jumps", choices = defaultJumps))
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
  })

  observeEvent(input$usr_model_button_save, {
    entered <- FALSE
    switch(input$usr_modelClass,
           "Diffusion process" = {
             if (input$usr_model_name!="" & (input$usr_model_coeff_drift!="" | input$usr_model_coeff_diff!="")){
               mod <- try(setModel(drift = tolower(input$usr_model_coeff_drift), diffusion = tolower(input$usr_model_coeff_diff), solve.variable = "x"))
               if(class(mod)!="try-error") usr_models$model[[input$usr_model_name]] <<- list(object=mod, class=input$usr_modelClass)
               entered <- TRUE
             }
           },
           "Fractional process" = {
             if (input$usr_model_name!="" & (input$usr_model_coeff_drift!="" | input$usr_model_coeff_diff!="")){
               mod <- try(setModel(drift = tolower(input$usr_model_coeff_drift), diffusion = tolower(input$usr_model_coeff_diff), hurst = NA, solve.variable = "x"))
               if(class(mod)!="try-error") usr_models$model[[input$usr_model_name]] <<- list(object=mod, class=input$usr_modelClass)
               entered <- TRUE
             }
           },
           "Compound Poisson" = {
             if (input$usr_model_name!="" & (input$usr_model_coeff_intensity!="")){
               mod <- try(setPoisson(intensity = tolower(input$usr_model_coeff_intensity), df = "", solve.variable = "x"))
               if(class(mod)!="try-error") usr_models$model[[input$usr_model_name]] <<- list(intensity=tolower(input$usr_model_coeff_intensity), class=input$usr_modelClass)
               entered <- TRUE
             }
           } 
          )
    if (entered){
      estimateSettings[[input$usr_model_name]] <<- list()
      closeAlert(session, "alert_savingModels")
      if(class(mod)!="try-error") createAlert(session = session, anchorId = "panel_set_model_alert", alertId = "alert_savingModels", style = "success", content = "Model saved successfully")
      else createAlert(session = session, anchorId = "panel_set_model_alert", alertId = "alert_savingModels", style = "error", content = "Model is not correctly specified")
    }
  })

  observe({
    for(mod in names(estimateSettings))
      if (!(mod %in% c(names(usr_models$model), names(defaultModels))))
        estimateSettings[[mod]] <<- list()
  })
  
  output$usr_model_saved <- renderUI({
    if (length(names(usr_models$model))!=0)
      selectInput("usr_model_saved", label = "Saved Models", choices = names(usr_models$model), selected = tail(names(usr_models$model),1))
  })

  output$usr_model_saved_latex <- renderUI({
    input$usr_model_button_save
    if (!is.null(input$usr_model_saved)) if (input$usr_model_saved %in% names(usr_models$model))
      withMathJax(printModelLatex(input$usr_model_saved, process = usr_models$model[[input$usr_model_saved]]$class))
  })

  observeEvent(input$usr_model_button_delete, {
    for (i in input$usr_model_saved)
      usr_models$model[i] <<- NULL
  })


  ###Display available data
  output$database3 <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"Please load some data first (section Data I/O)"=NA, check.names = FALSE)
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
      NoData <- data.frame("Symb"=NA,"Select some data from the table beside"=NA, check.names = FALSE)
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
      if (is.null(deltaSettings[[symb]])) deltaSettings[[symb]] <<- 0.01
      if (is.null(toLogSettings[[symb]])) toLogSettings[[symb]] <<- FALSE
      data <- na.omit(as.numeric(getData(symb)))
      if (toLogSettings[[symb]]==TRUE) data <- log(data)
      for (modName in input$model){
        if (class(try(setModelByName(modName, jumps = jumps_shortcut(class = class, jumps = input$jumps), AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA))))!="try-error"){
          if (is.null(estimateSettings[[modName]]))
            estimateSettings[[modName]] <<- list()
          if (is.null(estimateSettings[[modName]][[symb]]))
            estimateSettings[[modName]][[symb]] <<- list()
          if (is.null(estimateSettings[[modName]][[symb]][["fixed"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            estimateSettings[[modName]][[symb]][["fixed"]] <<- list()
          if (is.null(estimateSettings[[modName]][[symb]][["start"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            estimateSettings[[modName]][[symb]][["start"]] <<- list()
          
          startMinMax <- defaultBounds(name = modName, 
                                       jumps = jumps_shortcut(class = class, jumps = input$jumps), 
                                       AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), 
                                       MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA), 
                                       strict = FALSE,
                                       data = data,
                                       delta = deltaSettings[[symb]])
          upperLower <- defaultBounds(name = modName, 
                                      jumps = jumps_shortcut(class = class, jumps = input$jumps), 
                                      AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), 
                                      MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA),
                                      strict = TRUE,
                                      data = data,
                                      delta = deltaSettings[[symb]])
          
          if (is.null(estimateSettings[[modName]][[symb]][["startMin"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            estimateSettings[[modName]][[symb]][["startMin"]] <<- startMinMax$lower
          if (is.null(estimateSettings[[modName]][[symb]][["startMax"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            estimateSettings[[modName]][[symb]][["startMax"]] <<- startMinMax$upper
          if (is.null(estimateSettings[[modName]][[symb]][["upper"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            estimateSettings[[modName]][[symb]][["upper"]] <<- upperLower$upper
          if (is.null(estimateSettings[[modName]][[symb]][["lower"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
            estimateSettings[[modName]][[symb]][["lower"]] <<- upperLower$lower
          if (is.null(estimateSettings[[modName]][[symb]][["method"]])){
            if(class=="COGARCH" | class=="CARMA") estimateSettings[[modName]][[symb]][["method"]] <<- "SANN"
            else estimateSettings[[modName]][[symb]][["method"]] <<- "L-BFGS-B"
          }
          if (is.null(estimateSettings[[modName]][[symb]][["trials"]]))
            estimateSettings[[modName]][[symb]][["trials"]] <<- 1
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
    }
    prev_buttonDelta <<- input$advancedSettingsButtonApplyDelta
    prev_buttonAllDelta <<- input$advancedSettingsButtonApplyAllDelta
  })
  
  observe({
    valid <- TRUE
    if (nrow(seriesToEstimate$table)==0 | is.null(input$model)) valid <- FALSE
    else for(mod in input$model) if  (class(try(setModelByName(mod, jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps), AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))))=="try-error")  valid <- FALSE
    shinyjs::toggle(id="advancedSettingsAll", condition = valid)
    shinyjs::toggle(id="advancedSettingsErrorMessage", condition = !valid)
  })
  output$advancedSettingsSeries <- renderUI({
    if (nrow(seriesToEstimate$table)!=0)
      selectInput(inputId = "advancedSettingsSeries", label = "Series", choices = rownames(seriesToEstimate$table))
  })
  output$advancedSettingsDelta <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      return (numericInput("advancedSettingsDelta", label = paste("delta", input$advancedSettingsSeries), value = deltaSettings[[input$advancedSettingsSeries]]))
  })
  output$advancedSettingsToLog <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries)){
      choices <- FALSE
      if (all(getData(input$advancedSettingsSeries)>0)) choices <- c(FALSE, TRUE)
      return (selectInput("advancedSettingsToLog", label = "Convert to log", choices = choices, selected = toLogSettings[[input$advancedSettingsSeries]]))
    }
  })
  output$advancedSettingsModel <- renderUI({
    if(!is.null(input$model))
      selectInput(inputId = "advancedSettingsModel", label = "Model", choices = input$model)
  })
  output$advancedSettingsParameter <- renderUI({
    if (!is.null(input$model))
      if (!is.null(input$advancedSettingsModel)){
        parL <- setModelByName(input$advancedSettingsModel, jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps), AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))@parameter
        par <- parL@all
        if (input$modelClass=="COGARCH") par <- unique(c(parL@drift, parL@xinit))
        if (input$modelClass=="CARMA") par <- parL@drift
        selectInput(inputId = "advancedSettingsParameter", label = "Parameter", choices = par)
      }
  })
  #REMOVE# output$advancedSettingsFixed <- renderUI({
  #REMOVE#  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
  #REMOVE#    numericInput(inputId = "advancedSettingsFixed", label = "fixed", value = ifelse(is.null(estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]]),NA,estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]]))
  #REMOVE#})
  output$advancedSettingsStart <- renderUI({
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      #REMOVE# if (is.na(input$advancedSettingsFixed))
        numericInput(inputId = "advancedSettingsStart", label = "start", value = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["start"]][[input$advancedSettingsParameter]])
  })
  output$advancedSettingsStartMin <- renderUI({
    input$advancedSettingsButtonApplyDelta
    input$advancedSettingsButtonApplyAllDelta
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsStart) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      if (#REMOVE# is.na(input$advancedSettingsFixed) & 
        is.na(input$advancedSettingsStart))
        numericInput(inputId = "advancedSettingsStartMin", label = "start: Min", value = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMin"]][[input$advancedSettingsParameter]])
  })
  output$advancedSettingsStartMax <- renderUI({
    input$advancedSettingsButtonApplyDelta
    input$advancedSettingsButtonApplyAllDelta
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsStart) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      if (#REMOVE# is.na(input$advancedSettingsFixed) & 
        is.na(input$advancedSettingsStart))
        numericInput(inputId = "advancedSettingsStartMax", label = "start: Max", value = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMax"]][[input$advancedSettingsParameter]])
  })
  output$advancedSettingsLower <- renderUI({
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      #REMOVE# if (is.na(input$advancedSettingsFixed))
        if (input$advancedSettingsMethod=="L-BFGS-B" | input$advancedSettingsMethod=="Brent")
          numericInput("advancedSettingsLower", label = "lower", value = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["lower"]][[input$advancedSettingsParameter]])
  })
  output$advancedSettingsUpper <- renderUI({
    if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
      !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
      #REMOVE# if (is.na(input$advancedSettingsFixed))
        if (input$advancedSettingsMethod=="L-BFGS-B" | input$advancedSettingsMethod=="Brent")
          numericInput("advancedSettingsUpper", label = "upper", value = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["upper"]][[input$advancedSettingsParameter]])
  })
  #REMOVE# output$advancedSettingsJoint <- renderUI({
  #REMOVE#   if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
  #REMOVE#     selectInput("advancedSettingsJoint", label = "joint", choices = c(FALSE, TRUE), selected = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["joint"]])
  #REMOVE# })
  output$advancedSettingsMethod <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      selectInput("advancedSettingsMethod", label = "method", choices = c("L-BFGS-B", "Nelder-Mead", "BFGS", "CG", "SANN", "Brent"), selected = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]])
  })
  #REMOVE# output$advancedSettingsAggregation <- renderUI({
  #REMOVE#   if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
  #REMOVE#     selectInput("advancedSettingsAggregation", label = "aggregation", choices = c(TRUE, FALSE), selected = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["aggregation"]])
  #REMOVE# })
  #REMOVE# output$advancedSettingsThreshold <- renderUI({
  #REMOVE#   if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
  #REMOVE#     numericInput("advancedSettingsThreshold", label = "threshold", value = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["threshold"]])
  #REMOVE# })
  output$advancedSettingsTrials <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsMethod))
      numericInput("advancedSettingsTrials", label = "trials", min = 1, value = ifelse(input$advancedSettingsMethod=="SANN" & estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]]!="SANN",1,estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["trials"]]))
  })
  output$advancedSettingsSeed <- renderUI({
    if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
      numericInput("advancedSettingsSeed", label = "seed", min = 1, value = estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["seed"]])
  })



  observeEvent(input$advancedSettingsButtonApplyDelta, {
      deltaSettings[[input$advancedSettingsSeries]] <<- input$advancedSettingsDelta
      toLogSettings[[input$advancedSettingsSeries]] <<- input$advancedSettingsToLog
  })
  observeEvent(input$advancedSettingsButtonApplyAllDelta, {
    for (symb in rownames(seriesToEstimate$table)){
      deltaSettings[[symb]] <<- input$advancedSettingsDelta
      if (input$advancedSettingsToLog==FALSE) toLogSettings[[symb]] <<- input$advancedSettingsToLog
      else if (all(getData(symb)>0)) toLogSettings[[symb]] <<- input$advancedSettingsToLog
    }
  })
  observeEvent(input$advancedSettingsButtonApplyModel,{
    #REMOVE# estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsFixed
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["start"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStart
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMin"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMin
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMax"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMax
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["lower"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsLower
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["upper"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsUpper
  })
  observeEvent(input$advancedSettingsButtonApplyAllModel,{
    for (symb in rownames(seriesToEstimate$table)){
      #REMOVE# estimateSettings[[input$advancedSettingsModel]][[symb]][["fixed"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsFixed
      estimateSettings[[input$advancedSettingsModel]][[symb]][["start"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStart
      estimateSettings[[input$advancedSettingsModel]][[symb]][["startMin"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMin
      estimateSettings[[input$advancedSettingsModel]][[symb]][["startMax"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMax
      estimateSettings[[input$advancedSettingsModel]][[symb]][["lower"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsLower
      estimateSettings[[input$advancedSettingsModel]][[symb]][["upper"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsUpper
    }
  })
  observeEvent(input$advancedSettingsButtonApplyGeneral,{
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]] <<- input$advancedSettingsMethod
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["trials"]] <<- input$advancedSettingsTrials
    estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["seed"]] <<- input$advancedSettingsSeed
    #REMOVE# estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["joint"]] <<- input$advancedSettingsJoint
    #REMOVE# estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["aggregation"]] <<- input$advancedSettingsAggregation
    #REMOVE# estimateSettings[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["threshold"]] <<- input$advancedSettingsThreshold
  })
  observeEvent(input$advancedSettingsButtonApplyAllModelGeneral,{
    for (symb in rownames(seriesToEstimate$table)){
      estimateSettings[[input$advancedSettingsModel]][[symb]][["method"]] <<- input$advancedSettingsMethod
      estimateSettings[[input$advancedSettingsModel]][[symb]][["trials"]] <<- input$advancedSettingsTrials
      estimateSettings[[input$advancedSettingsModel]][[symb]][["seed"]] <<- input$advancedSettingsSeed
      #REMOVE# estimateSettings[[input$advancedSettingsModel]][[symb]][["joint"]] <<- input$advancedSettingsJoint
      #REMOVE# estimateSettings[[input$advancedSettingsModel]][[symb]][["aggregation"]] <<- input$advancedSettingsAggregation
      #REMOVE# estimateSettings[[input$advancedSettingsModel]][[symb]][["threshold"]] <<- input$advancedSettingsThreshold
    }
  })
  observeEvent(input$advancedSettingsButtonApplyAllGeneral,{
    for (mod in input$model){
      for (symb in rownames(seriesToEstimate$table)){
        estimateSettings[[mod]][[symb]][["method"]] <<- input$advancedSettingsMethod
        estimateSettings[[mod]][[symb]][["trials"]] <<- input$advancedSettingsTrials
        estimateSettings[[mod]][[symb]][["seed"]] <<- input$advancedSettingsSeed
        #REMOVE# estimateSettings[[mod]][[symb]][["joint"]] <<- input$advancedSettingsJoint
        #REMOVE# estimateSettings[[mod]][[symb]][["aggregation"]] <<- input$advancedSettingsAggregation
        #REMOVE# estimateSettings[[mod]][[symb]][["threshold"]] <<- input$advancedSettingsThreshold
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
    else for(mod in input$model) if (class(try(setModelByName(mod, jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps), AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))))=="try-error")  valid <- FALSE
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
              AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), 
              MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA),
              jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps),
              symbName = symb,
              data = data,
              delta = deltaSettings[[symb]],
              toLog = toLogSettings[[symb]],
              start = estimateSettings[[modName]][[symb]][["start"]],
              startMin = estimateSettings[[modName]][[symb]][["startMin"]],
              startMax = estimateSettings[[modName]][[symb]][["startMax"]],
              method=estimateSettings[[modName]][[symb]][["method"]],
              trials=estimateSettings[[modName]][[symb]][["trials"]],
              seed = estimateSettings[[modName]][[symb]][["seed"]],
              fixed = estimateSettings[[modName]][[symb]][["fixed"]],
              lower = estimateSettings[[modName]][[symb]][["lower"]],
              upper = estimateSettings[[modName]][[symb]][["upper"]],
              joint = estimateSettings[[modName]][[symb]][["joint"]],
              aggregation = estimateSettings[[modName]][[symb]][["aggregation"]],
              threshold = estimateSettings[[modName]][[symb]][["threshold"]],
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
      h3(id[1], " - " , info$modName),
      h4(
        em("delta:"), info$delta, br(),
        em("series to log:"), info$toLog, br(),
        em("method:"), info$method, br(),
        em("trials:"), info$trials, br(),
        em("seed:"), info$seed, br()
        #REMOVE# em("joint:"), info$joint, br(),
        #REMOVE# em("aggregation:"), info$aggregation, br(),
        #REMOVE# em("threshold:"), info$threshold
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
    shinyjs::toggle("usr_model_saved_div", condition = length(names(usr_models$model))!=0)
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
      NoData <- data.frame("Symb"=NA,"Please estimate some models first (section Modelling)"=NA, check.names = FALSE)
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
    for(i in names(usr_models$model))
      if (usr_models$model[[i]]$class==input$simulate_model_usr_selectClass)
        choices <- c(i, choices)
    selectInput("simulate_model_usr_selectModel", label = "Model Name", choices = choices, selected = sel)
  })

  output$simulate_model_usr_selectJumps <- renderUI({
    if(input$simulate_model_usr_selectClass=="Compound Poisson")
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
      if (is.null(usr_models$simulation[[id]])){
        usr_models$simulation[[id]] <<- list()
      }
      usr_models$simulation[[id]][["Class"]] <<- input$simulate_model_usr_selectClass
      usr_models$simulation[[id]][["Model"]] <<- input$simulate_model_usr_selectModel
      usr_models$simulation[[id]][["Jumps"]] <<- input$simulate_model_usr_selectJumps
      if (is.null(usr_models$simulation[[id]][["true.param"]])){
        usr_models$simulation[[id]][["true.param"]] <<- list()
      }
      allparam <- setModelByName(input$simulate_model_usr_selectModel, jumps = input$simulate_model_usr_selectJumps)@parameter@all
      if (input$simulate_model_usr_selectClass=="Fractional process") allparam <- c(allparam, "hurst")
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
      if (!is.null(usr_models$simulation[[id]])){
        valid <- TRUE
        if(usr_models$simulation[[id]][["Model"]]!=input$simulate_model_usr_selectModel | input$simulate_model_usr_selectParam=="") 
          valid <- FALSE
        else if (usr_models$simulation[[id]][["Class"]]=="Compound Poisson") if (usr_models$simulation[[id]][["Jumps"]]!=input$simulate_model_usr_selectJumps)  
          valid <- FALSE
        if (valid)
          usr_models$simulation[[id]][["true.param"]][[input$simulate_model_usr_selectParam]] <- ifelse(is.na(input$simulate_model_usr_param),"MISSING",input$simulate_model_usr_param)
      }
    }
  })

  observe({
    for(i in names(usr_models$simulation))
      if (!(usr_models$simulation[[i]]$Model %in% c(defaultModels, names(usr_models$model))))
          usr_models$simulation[i] <<- NULL
  })

  output$simulate_model_usr_table <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollX=TRUE, scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "multiple",{
    table <- data.frame()
    for (i in names(usr_models$simulation)){
      newRow <- as.data.frame(usr_models$simulation[[i]])
      colnames(newRow) <- gsub(pattern = "true.param.", x = colnames(newRow), replacement = "")
      table <- rbind.fill(table, newRow)
    }
    if (length(table)==0){
      NoData <- data.frame("Model"=NA, "Parameters"=NA)
      return(NoData[-1,])
    }
    return (data.frame(table, row.names = names(usr_models$simulation)))
  })

  observeEvent(input$simulate_model_usr_button_select, {
    if (!is.null(input$simulate_model_usr_table_rows_selected)){
      table <- data.frame()
      for (i in names(usr_models$simulation)[input$simulate_model_usr_table_rows_selected]){
        if ("MISSING" %in% usr_models$simulation[[i]][["true.param"]]){
          createAlert(session = session, anchorId = "panel_simulate_equation_alert", alertId = "simulate_alert_usr_button_select", content = "There are still missing values in selected models", style = "error")
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
      for (i in names(usr_models$simulation)[input$simulate_model_usr_table_rows_all]){
        if ("MISSING" %in% usr_models$simulation[[i]][["true.param"]]){
          createAlert(session = session, anchorId = "panel_simulate_equation_alert", alertId = "simulate_alert_usr_button_select", content = "There are still missing values in selected models", style = "error")
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

  simulateSettings <- list()
  observe({
    for (modID in rownames(modelsToSimulate$table)[input$simulate_selectedModels_rows_all]){
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
        if (is.null(simulateSettings[[modID]][["xinit"]])){
          xinit <- as.numeric(tail(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data,1))[1] 
          toLog <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$toLog
          if(toLog==TRUE) xinit <- exp(xinit)
          simulateSettings[[modID]][["xinit"]] <<- xinit 
        }
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
    selectInput("simulate_modelID", label = "Simulation ID", choices = rownames(modelsToSimulate$table))
  })

  output$simulate_advancedSettings_modelID <- renderUI({
    selectInput("simulate_advancedSettings_modelID", label = "Simulation ID", choices = rownames(modelsToSimulate$table))
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
    for (modID in rownames(modelsToSimulate$table)){
      simulateSettings[[modID]][["seed"]] <<- input$simulate_seed
      simulateSettings[[modID]][["traj"]] <<- input$simulate_traj
    }
  })
  observeEvent(input$simulate_button_apply_nsim, {
    simulateSettings[[input$simulate_modelID]][["nsim"]] <<- input$simulate_nsim
    simulateSettings[[input$simulate_modelID]][["nstep"]] <<- input$simulate_nstep
  })
  observeEvent(input$simulate_button_applyAll_nsim, {
    for (modID in rownames(modelsToSimulate$table)){
      simulateSettings[[modID]][["nsim"]] <<- input$simulate_nsim
      simulateSettings[[modID]][["nstep"]] <<- input$simulate_nstep
    }
  })
  observeEvent(input$simulate_button_apply_xinit, {
    simulateSettings[[input$simulate_modelID]][["xinit"]] <<- input$simulate_xinit
  })
  observeEvent(input$simulate_button_applyAll_xinit, {
    for (modID in rownames(modelsToSimulate$table))
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
    for (modID in rownames(modelsToSimulate$table)){
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
  
  observe({
    if (!is.null(modelsToSimulate$table)) if (nrow(modelsToSimulate$table)!=0) {
      closeAlert(session, alertId = "simulate_alert_buttonEstimate1")
      closeAlert(session, alertId = "simulate_alert_buttonEstimate2")
    }
  })

  observeEvent(input$simulate_simulateModels, {
    if (is.null(modelsToSimulate$table)) {
      if (input$panel_simulations=="Simulate model") createAlert(session = session, anchorId = "panel_simulate_model_alert", alertId = "simulate_alert_buttonEstimate1", content = "Table 'Selected Models' is empty", style = "warning")
      if (input$panel_simulations=="Simulate equation") createAlert(session = session, anchorId = "panel_simulate_equation_alert", alertId = "simulate_alert_buttonEstimate2", content = "Table 'Selected Models' is empty", style = "warning")
    } else if (nrow(modelsToSimulate$table)==0) {
      if (input$panel_simulations=="Simulate model") createAlert(session = session, anchorId = "panel_simulate_model_alert", alertId = "simulate_alert_buttonEstimate1", content = "Table 'Selected Models' is empty", style = "warning")
      if (input$panel_simulations=="Simulate equation") createAlert(session = session, anchorId = "panel_simulate_equation_alert", alertId = "simulate_alert_buttonEstimate2", content = "Table 'Selected Models' is empty", style = "warning")
    }
    else{
      withProgress(message = 'Simulating: ', value = 0, {
        for (modID in rownames(modelsToSimulate$table)){
          if(modID %in% names(usr_models$simulation)){
            incProgress(1/nrow(modelsToSimulate$table), detail = paste(modID,"-",usr_models$simulation[[modID]][["Model"]]))
            info <- list(
              "class" = usr_models$simulation[[modID]][["Class"]],
              "model" = usr_models$simulation[[modID]][["Model"]],
              "jumps" = ifelse(is.null(usr_models$simulation[[modID]][["Jumps"]]),NA, usr_models$simulation[[modID]][["Jumps"]]),
              "estimate.from" = NA,
              "estimate.to" = NA,
              "simulate.from" = as.numeric(simulateSettings[[modID]][["t0"]]),
              "simulate.to" = as.numeric(simulateSettings[[modID]][["t1"]]))
            Initial <- simulateSettings[[modID]][["t0"]]
            Terminal <- simulateSettings[[modID]][["t1"]]
            n <- ifelse(is.na(simulateSettings[[modID]][["nstep"]]),(Terminal-Initial)/0.01,simulateSettings[[modID]][["nstep"]])
            addSimulation(
              modelYuima = setYuima(model = setModelByName(name = info$model, jumps = info$jumps)),
              true.parameter = usr_models$simulation[[modID]][["true.param"]],
              symbName = modID,
              info = info,
              xinit = simulateSettings[[modID]][["xinit"]],
              nsim = simulateSettings[[modID]][["nsim"]],
              sampling = setSampling(Initial = Initial, Terminal = Terminal, n=n, delta = NA),
              saveTraj = simulateSettings[[modID]][["traj"]],
              seed = simulateSettings[[modID]][["seed"]],
              session = session,
              anchorId = "panel_simulations_alert"
            )
          }
          else if(modID %in% rownames(yuimaGUItable$model)){
            id <- unlist(strsplit(modID, split = " "))
            incProgress(1/nrow(modelsToSimulate$table), detail = paste(id[1],"-",yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$modName))
            data <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data
            toLog <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$toLog
            if(class(index(data))=="Date"){
              info <- list(
                "class" = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$class,
                "model" = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$modName,
                "jumps" = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$jumps,
                "estimate.from" = as.Date(start(data)),
                "estimate.to" = as.Date(end(data)),
                "simulate.from" = simulateSettings[[modID]][["t0"]],
                "simulate.to" = simulateSettings[[modID]][["t1"]])
              Initial <- as.numeric(simulateSettings[[modID]][["t0"]]-start(data))/as.numeric(end(data)-start(data))*(length(data)-1)*yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@sampling@delta
              Terminal <- as.numeric(simulateSettings[[modID]][["t1"]]-start(data))/as.numeric(end(data)-start(data))*(length(data)-1)*yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@sampling@delta
              n <- ifelse(is.na(simulateSettings[[modID]][["nstep"]]),as.numeric(simulateSettings[[modID]][["t1"]]-simulateSettings[[modID]][["t0"]])/as.numeric(end(data)-start(data))*(length(data)-1),simulateSettings[[modID]][["nstep"]])
            }
            if(is.numeric(index(data))){
              info <- list(
                "class" = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$class,
                "model" = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$modName,
                "jumps" = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$jumps,
                "estimate.from" = as.numeric(start(data)),
                "estimate.to" = as.numeric(end(data)),
                "simulate.from" = as.numeric(simulateSettings[[modID]][["t0"]]),
                "simulate.to" = as.numeric(simulateSettings[[modID]][["t1"]]))
              Initial <- simulateSettings[[modID]][["t0"]]/(as.numeric(end(data))-as.numeric(start(data)))*(length(data)-1)*yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@sampling@delta
              Terminal <- simulateSettings[[modID]][["t1"]]/(as.numeric(end(data))-as.numeric(start(data)))*(length(data)-1)*yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@sampling@delta
              n <- ifelse(is.na(simulateSettings[[modID]][["nstep"]]),as.numeric(simulateSettings[[modID]][["t1"]]-simulateSettings[[modID]][["t0"]])/(as.numeric(end(data))-as.numeric(start(data)))*(length(data)-1),simulateSettings[[modID]][["nstep"]])
            }
            if (yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$class=="Fractional process") true.parameter <- as.list(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$qmle["Estimate",])
            else true.parameter <- as.list(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$qmle@coef)
            addSimulation(
              modelYuima = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model,
              true.parameter = true.parameter,
              symbName = id[1],
              info = info,
              toLog = toLog,
              xinit = simulateSettings[[modID]][["xinit"]],
              nsim = simulateSettings[[modID]][["nsim"]],
              sampling = setSampling(Initial = Initial, Terminal = Terminal, n=n, delta = NA),
              saveTraj = simulateSettings[[modID]][["traj"]],
              seed = simulateSettings[[modID]][["seed"]],
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


  output$simulate_showSimulation_hist_probability_slider <- renderUI({
    if(length(simulation_hist$values)!=0){
      Min <- min(simulation_hist$values)
      Max <- max(simulation_hist$values)
      sliderInput("simulate_showSimulation_hist_probability_slider", width = "75%",min = Min-0.01, max = Max+0.01, value = c(Min+0.25*(Max-Min),Min+0.75*(Max-Min)), label = "Mean & Probability", step = 0.01, ticks=FALSE, round = -2)
    }
  })

  output$simulate_showSimulation_hist_probability_text <- renderText({
    if(length(simulation_hist$values)!=0 & !is.null(input$simulate_showSimulation_hist_probability_slider)){
      val <- as.numeric(simulation_hist$values)
      paste("Probability: ",as.character(100*sum(ifelse(val>=input$simulate_showSimulation_hist_probability_slider[1] & val<=input$simulate_showSimulation_hist_probability_slider[2],1,0))/length(val)),"%")
    }
  })
  output$simulate_showSimulation_hist_mean_text <- renderText({
    if(length(simulation_hist$values)!=0 & !is.null(input$simulate_showSimulation_hist_probability_slider)){
      val <- as.numeric(simulation_hist$values)
      val <- val[val>=input$simulate_showSimulation_hist_probability_slider[1] & val<=input$simulate_showSimulation_hist_probability_slider[2]]
      paste("Mean: ",mean(val))
    }
  })


  ###Save Trajectory Button
  output$simulate_showSimulation_button_saveTrajectory <- {
    dataDownload_traj <- reactive({
      id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
      x <- yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory
      data.frame(x, row.names = index(x))
    })
    downloadHandler(
      filename = function() {
        paste(input$simulate_showSimulation_simID, ".txt", sep="")
      },
      content = function(file) {
        write.table(dataDownload_traj(), file, row.names = TRUE, col.names = FALSE, quote = TRUE)
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
      NoData <- data.frame("Symb"=NA,"Please load some data first (section Data I/O)"=NA, check.names = FALSE)
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
      NoData <- data.frame("Symb"=NA,"Select some data from the table beside"=NA, check.names = FALSE)
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
        "MOdist" = try(MOdist(na.omit(x))),
        "MYdist_perc" = try(MYdist(x, percentage = TRUE)),
        "MYdist_ass" = try(MYdist(x, percentage = FALSE)),
        "euclidean" = try(dist(t(as.data.frame(x)), method = "euclidean")),
        "maximum" = try(dist(t(as.data.frame(x)), method = "maximum")),
        "manhattan" = try(dist(t(as.data.frame(x)), method = "manhattan")),
        "canberra" = try(dist(t(as.data.frame(x)), method = "canberra")),
        "minkowski" = try(dist(t(as.data.frame(x)), method = "minkowski", p = input$cluster_distance_minkowskiPower))
      )
      shinyjs::toggle("cluster_charts", condition = (class(d)!="try-error"))
      if (class(d)=="try-error")
        createAlert(session, anchorId = "cluster_alert", alertId = "cluster_alert_dist", content = "Error in clustering", style = "error")
      else{
        hc <- hclust(d, method = input$cluster_linkage)
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
    })}
  })
  
  
  
  ########################Nonparametric Change Point
  ########################
  ########################
  
  ###Display available data
  output$changepoint_table_select <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
    if (length(yuimaGUItable$series)==0){
      NoData <- data.frame("Symb"=NA,"Please load some data first (section Data I/O)"=NA, check.names = FALSE)
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
      NoData <- data.frame("Symb"=NA,"Select some data from the table beside"=NA, check.names = FALSE)
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
    selectInput("changepoint_symb", "Symbol", choices = sort(names(yuimaGUIdata$cp)))  
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
        h3(input$changepoint_symb),
        h4(
          em(switch(info$method, "KSdiff"="Increments Distriution", "KSperc"="Percentage Increments Distriution")), br()
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
      NoData <- data.frame("Symb"=NA,"Please load some data first (section Data I/O)"=NA, check.names = FALSE)
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
      NoData <- data.frame("Symb"=NA,"Select some data from the table beside"=NA, check.names = FALSE)
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
    for(i in names(usr_models$model))
      if (usr_models$model[[i]]$class=="Diffusion process") choices <- c(i, choices)
    selectInput("parametric_changepoint_model", label = "Model", choices = choices, multiple = FALSE, selected = sel)
  })
  
  
  
  ### Estimation Settings
  parametric_modal_prev_buttonDelta <- 0
  parametric_modal_prev_buttonAllDelta <- 0
  observe({
    for (symb in rownames(parametric_seriesToChangePoint$table)){
      if (is.null(deltaSettings[[symb]])) deltaSettings[[symb]] <<- 0.01
      if (is.null(toLogSettings[[symb]])) toLogSettings[[symb]] <<- FALSE
      data <- na.omit(as.numeric(getData(symb)))
      if (toLogSettings[[symb]]==TRUE) data <- log(data)
      for (modName in input$parametric_changepoint_model){
        if (class(try(setModelByName(modName, jumps = NA, AR_C = NA, MA_C = NA)))!="try-error"){
          if (is.null(estimateSettings[[modName]]))
            estimateSettings[[modName]] <<- list()
          if (is.null(estimateSettings[[modName]][[symb]]))
            estimateSettings[[modName]][[symb]] <<- list()
          if (is.null(estimateSettings[[modName]][[symb]][["fixed"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            estimateSettings[[modName]][[symb]][["fixed"]] <<- list()
          if (is.null(estimateSettings[[modName]][[symb]][["start"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            estimateSettings[[modName]][[symb]][["start"]] <<- list()
          
          startMinMax <- defaultBounds(name = modName, 
                                       jumps = NA, 
                                       AR_C = NA, 
                                       MA_C = NA, 
                                       strict = FALSE,
                                       data = data,
                                       delta = deltaSettings[[symb]])
          upperLower <- defaultBounds(name = modName, 
                                      jumps = NA, 
                                      AR_C = NA, 
                                      MA_C = NA, 
                                      strict = TRUE,
                                      data = data,
                                      delta = deltaSettings[[symb]])
          
          if (is.null(estimateSettings[[modName]][[symb]][["startMin"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            estimateSettings[[modName]][[symb]][["startMin"]] <<- startMinMax$lower
          if (is.null(estimateSettings[[modName]][[symb]][["startMax"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            estimateSettings[[modName]][[symb]][["startMax"]] <<- startMinMax$upper
          if (is.null(estimateSettings[[modName]][[symb]][["upper"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            estimateSettings[[modName]][[symb]][["upper"]] <<- upperLower$upper
          if (is.null(estimateSettings[[modName]][[symb]][["lower"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
            estimateSettings[[modName]][[symb]][["lower"]] <<- upperLower$lower
          if (is.null(estimateSettings[[modName]][[symb]][["method"]])){
            estimateSettings[[modName]][[symb]][["method"]] <<- "L-BFGS-B"
          }
          if (is.null(estimateSettings[[modName]][[symb]][["trials"]]))
            estimateSettings[[modName]][[symb]][["trials"]] <<- 1
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
      return (numericInput("parametric_modal_delta", label = paste("delta", input$parametric_modal_series), value = deltaSettings[[input$parametric_modal_series]]))
  })
  output$parametric_modal_toLog <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series)){
      choices <- FALSE
      if (all(getData(input$parametric_modal_series)>0)) choices <- c(FALSE, TRUE)
      return (selectInput("parametric_modal_toLog", label = "Convert to log", choices = choices, selected = toLogSettings[[input$parametric_modal_series]]))
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
      numericInput(inputId = "parametric_modal_start", label = "start", value = estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["start"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_startMin <- renderUI({
    input$parametric_modal_button_applyDelta
    input$parametric_modal_button_applyAllDelta
    if (!is.null(input$parametric_modal_start) & !is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
      if (is.na(input$parametric_modal_start))
        numericInput(inputId = "parametric_modal_startMin", label = "start: Min", value = estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMin"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_startMax <- renderUI({
    input$parametric_modal_button_applyDelta
    input$parametric_modal_button_applyAllDelta
    if (!is.null(input$parametric_modal_start) & !is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
      if (is.na(input$parametric_modal_start))
        numericInput(inputId = "parametric_modal_startMax", label = "start: Max", value = estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMax"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_lower <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
      if (input$parametric_modal_method=="L-BFGS-B" | input$parametric_modal_method=="Brent")
        numericInput("parametric_modal_lower", label = "lower", value = estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["lower"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_upper <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
      if (input$parametric_modal_method=="L-BFGS-B" | input$parametric_modal_method=="Brent")
        numericInput("parametric_modal_upper", label = "upper", value = estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["upper"]][[input$parametric_modal_parameter]])
  })
  output$parametric_modal_method <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series))
      selectInput("parametric_modal_method", label = "method", choices = c("L-BFGS-B"
                                                                           #, "Nelder-Mead", "BFGS", "CG", "SANN", "Brent"
                                                                           ), selected = estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["method"]])
  })
  output$parametric_modal_trials <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_method))
      numericInput("parametric_modal_trials", label = "trials", min = 1, value = ifelse(input$parametric_modal_method=="SANN" & estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["method"]]!="SANN",1,estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["trials"]]))
  })
  output$parametric_modal_seed <- renderUI({
    if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series))
      numericInput("parametric_modal_seed", label = "seed", min = 1, value = estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["seed"]])
  })
  output$parametric_modal_range <- renderUI({
    if(!is.null(input$parametric_modal_series)){
      series <- getData(input$parametric_modal_series)
      type <- class(index(series)[1])
      if(type=="Date") return(column(12,dateRangeInput("parametric_modal_range_date", label = "Range", start = start(series), end = end(series))))
      else return(div(
        column(6,numericInput("parametric_modal_range_numeric_t0", label = "From", value = start(series))),
        column(6,numericInput("parametric_modal_range_numeric_t1", label = "To", value = end(series)))
      ))      
    }
  })
  
  
  
  observeEvent(input$parametric_modal_button_applyDelta, {
    deltaSettings[[input$parametric_modal_series]] <<- input$parametric_modal_delta
    toLogSettings[[input$parametric_modal_series]] <<- input$parametric_modal_toLog
    type <- class(index(getData(input$parametric_modal_series))[1])
    if(type=="Date"){
      from <- input$parametric_modal_range_date[1]
      to <- input$parametric_modal_range_date[2]
    } else {
      from <- input$parametric_modal_range_numeric_t0
      to <- input$parametric_modal_range_numeric_t1
    }
    levels(parametric_seriesToChangePoint$table[,"From"]) <- c(levels(parametric_seriesToChangePoint$table[,"From"]), as.character(from))
    levels(parametric_seriesToChangePoint$table[,"To"]) <- c(levels(parametric_seriesToChangePoint$table[,"To"]), as.character(to))
    parametric_seriesToChangePoint$table[input$parametric_modal_series,"From"] <<- as.character(from)
    parametric_seriesToChangePoint$table[input$parametric_modal_series,"To"] <<- as.character(to)
  })
  observeEvent(input$parametric_modal_button_applyAllDelta, {
    type <- class(index(getData(input$parametric_modal_series))[1])
    if(type=="Date"){
      from <- input$parametric_modal_range_date[1]
      to <- input$parametric_modal_range_date[2]
    } else {
      from <- input$parametric_modal_range_numeric_t0
      to <- input$parametric_modal_range_numeric_t1
    }
    levels(parametric_seriesToChangePoint$table[,"From"]) <- c(levels(parametric_seriesToChangePoint$table[,"From"]), as.character(from))
    levels(parametric_seriesToChangePoint$table[,"To"]) <- c(levels(parametric_seriesToChangePoint$table[,"To"]), as.character(to))
    for (symb in rownames(parametric_seriesToChangePoint$table)){
      deltaSettings[[symb]] <<- input$parametric_modal_delta
      if (input$parametric_modal_toLog==FALSE) toLogSettings[[symb]] <<- input$parametric_modal_toLog
      else if (all(getData(symb)>0)) toLogSettings[[symb]] <<- input$parametric_modal_toLog
      type_symb <- class(index(getData(symb))[1])
      if(type_symb==type){
        parametric_seriesToChangePoint$table[symb,"From"] <<- as.character(from)
        parametric_seriesToChangePoint$table[symb,"To"] <<- as.character(to)
      }
    }
  })
  observeEvent(input$parametric_modal_button_applyModel,{
    estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["start"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_start
    estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMin"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMin
    estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMax"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMax
    estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["lower"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_lower
    estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["upper"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_upper
  })
  observeEvent(input$parametric_modal_button_applyAllModel,{
    for (symb in rownames(parametric_seriesToChangePoint$table)){
      estimateSettings[[input$parametric_modal_model]][[symb]][["start"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_start
      estimateSettings[[input$parametric_modal_model]][[symb]][["startMin"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMin
      estimateSettings[[input$parametric_modal_model]][[symb]][["startMax"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMax
      estimateSettings[[input$parametric_modal_model]][[symb]][["lower"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_lower
      estimateSettings[[input$parametric_modal_model]][[symb]][["upper"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_upper
    }
  })
  observeEvent(input$parametric_modal_button_applyGeneral,{
    estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["method"]] <<- input$parametric_modal_method
    estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["trials"]] <<- input$parametric_modal_trials
    estimateSettings[[input$parametric_modal_model]][[input$parametric_modal_series]][["seed"]] <<- input$parametric_modal_seed
  })
  observeEvent(input$parametric_modal_button_applyAllModelGeneral,{
    for (symb in rownames(parametric_seriesToChangePoint$table)){
      estimateSettings[[input$parametric_modal_model]][[symb]][["method"]] <<- input$parametric_modal_method
      estimateSettings[[input$parametric_modal_model]][[symb]][["trials"]] <<- input$parametric_modal_trials
      estimateSettings[[input$parametric_modal_model]][[symb]][["seed"]] <<- input$parametric_modal_seed
    }
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
                        delta = deltaSettings[[symb]],
                        toLog = toLogSettings[[symb]],
                        start = estimateSettings[[input$parametric_changepoint_model]][[symb]][["start"]],
                        startMin = estimateSettings[[input$parametric_changepoint_model]][[symb]][["startMin"]],
                        startMax = estimateSettings[[input$parametric_changepoint_model]][[symb]][["startMax"]],
                        method = estimateSettings[[input$parametric_changepoint_model]][[symb]][["method"]],
                        trials = estimateSettings[[input$parametric_changepoint_model]][[symb]][["trials"]],
                        seed = estimateSettings[[input$parametric_changepoint_model]][[symb]][["seed"]],
                        lower = estimateSettings[[input$parametric_changepoint_model]][[symb]][["lower"]],
                        upper = estimateSettings[[input$parametric_changepoint_model]][[symb]][["upper"]]))
          if(class(test)=="try-error") 
            errors <- c(errors, symb)
        }
        if (!is.null(errors))
          createAlert(session = session, anchorId = "parametric_changepoint_alert", alertId = "parametric_changepoint_alert_err", style = "error", dismiss = TRUE, content = paste("Unable to estimate Change Point of:", paste(errors, collapse = " ")))
      })
  })

  output$parametric_changepoint_symb <- renderUI({
    selectInput("parametric_changepoint_symb", "Symbol", choices = sort(names(yuimaGUIdata$cpYuima)))  
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
        align="center", style="color:#CDCECD"
      )
    }
  })
  
  output$parametric_changepoint_modal_info_text <- renderUI({
    info <- yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]]$info
    div(
      h3(input$parametric_changepoint_symb, " - " , info$model),
      h4(
        em("series to log:"), info$toLog, br(),
        em("method:"), info$method, br(),
        em("trials:"), info$trials, br(),
        em("seed:"), info$seed, br()
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
      NoData <- data.frame("Symb"=NA,"Please load some data first (section Data I/O)"=NA, check.names = FALSE)
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
      NoData <- data.frame("Symb"=NA,"Select some data from the table beside"=NA, check.names = FALSE)
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
          if(type=="Date") data <- window(data, start = input$llag_range_date[1], end = input$llag_range_date[2])
          else data <- window(data, start = input$llag_range_numeric1, end = input$llag_range_numeric2)
          delta <- 0.01
          if(is.regular(data)){
            yuimaData <- setDataGUI(data, delta = delta)
            res <- try(llag(yuimaData, ci=TRUE, plot=FALSE, grid = seq(from = -input$llag_maxLag*delta, to = input$llag_maxLag*delta, by = delta)))
            if (class(res)=="try-error")
              createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Error in computing lead-lag", style = "error")
            else {
              LeadLag <- res$lagcce
              mode <- function(x) {
                ux <- unique(x)
                ux[which.max(tabulate(match(x, ux)))]
              }
              LeadLag <- LeadLag/delta*mode(na.omit(diff(index(data))))
              if(all(LeadLag==0)){
                shinyjs::hide("llag_plot_body")
                shinyjs::show("llag_plot_Text")
              } else{
                shinyjs::hide("llag_plot_Text")
                shinyjs::show("llag_plot_body")
                col1 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7","#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
                col2 <- colorRampPalette(c("#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
                col3 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7","#FFFFFF"))
                output$llag_plot <- renderPlot({
                  corrplot(LeadLag, p.mat = res$p.values, sig.level = input$llag_plot_confidence, is.corr = FALSE, method = input$llag_plot_type, type = "lower", cl.pos = "b", tl.pos = "ld", tl.srt = 60, col=get(input$llag_plot_cols)(100), outline=TRUE, bg = "grey10", order = "alphabet", tl.col = "black") 
                })
              }
              shinyjs::show("llag_button_showResults")
              toggleModal(session = session, modalId = "llag_modal_plot", toggle = "open")
            }
          }
          else{
            createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Cannot compute Lead-Lag for non-regular grid of observations", style = "error")
          }
        })
      }
    }
  })
  
  
  
  
  
  
  
  ########################Hedging
  ########################
  ########################
  
  output$hedging_databaseModels <- DT::renderDataTable(options=list(scrollY = 200, scrollX = TRUE, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
    if (length(yuimaGUItable$model)==0){
      NoData <- data.frame("Symb"=NA,"Please estimate some models first (section Modelling)"=NA, check.names = FALSE)
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
        data <- yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data
        info = list(
          "model" = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$modName,
          "estimate.from" = start(data),
          "estimate.to" = end(data),
          "maturity"= input$hedging_maturity,
          "strike"=input$hedging_strike, 
          "type"=input$hedging_type, 
          "optPrice"=input$hedging_optMarketPrice, 
          "optLotMult"=input$hedging_lotMult,
          "optLotCost"=input$hedging_lotCostOpt,
          "assPrice"=input$hedging_assMarketPrice,
          "assPercCost"=input$hedging_percCostAss/100,
          "assMinCost"=input$hedging_minCostAss,
          "assRateShortSelling"=input$hedging_rateShort/100)
        Initial <- 0
        Terminal <- as.numeric(input$hedging_maturity-info$estimate.to)/as.numeric(info$estimate.to-info$estimate.from)*length(data)*yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@sampling@delta
        n <- as.numeric(input$hedging_maturity-info$estimate.to)/as.numeric(info$estimate.to-info$estimate.from)*length(data)
        if (yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$info$class=="Fractional process") true.parameter <- as.list(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$qmle["Estimate",])
        else true.parameter <- as.list(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$qmle@coef)
        addHedging(
          model = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]],
          true.parameter = true.parameter,
          symbName = id[1],
          info = info,
          xinit = input$hedging_assMarketPrice,
          nsim = input$hedging_nSim,
          sampling = setSampling(Initial = Initial, Terminal = Terminal, n=n, delta = NA),
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
  
  
  hedging_values <- reactiveValues(profits=NULL, symb=NULL, model=NULL, id.changed=FALSE)
  
  output$hedging_nOptLot_hedge <- renderUI({
    if (!is.null(input$hedging_table_results_row_last_clicked)){
      info <- isolate({yuimaGUIdata$hedging[[input$hedging_table_results_row_last_clicked]]$info})
      nMax <- as.integer(input$hedging_maxCapital/(info$optLotMult*info$optPrice+input$hedging_lotCostOpt))
      isolate({hedging_values$id.changed <- TRUE})
      sliderInput("hedging_nOptLot_hedge", label = "Option - number of Lots", min = 0, max = nMax, value = info$LotsToBuy, step = 1, ticks = FALSE)
    }
  })
  
  output$hedging_nAss_hedge <- renderUI({
    if (!is.null(input$hedging_table_results_row_last_clicked)){
      if (!is.null(input$hedging_nOptLot_hedge)){
        info <- isolate({yuimaGUIdata$hedging[[input$hedging_table_results_row_last_clicked]]$info})
        assCapital <- input$hedging_maxCapital-input$hedging_nOptLot_hedge*(info$optLotMult*info$optPrice+input$hedging_lotCostOpt)
        nMax <- as.integer(assCapital/(info$assPrice*(1+input$hedging_percCostAss/100)))
        val <- min(nMax, isolate({input$hedging_nAss_hedge}))
        isolate({
          if (hedging_values$id.changed == TRUE){
            val <- switch (info$type,
              "call" = info$sell,
              "put" = info$buy
            )
            hedging_values$id.changed <- FALSE
          }
        })
        type <- info$type
        if (input$hedging_type2!="default")
          type <- input$hedging_type2
        lab <- paste("Number of Assets to", ifelse(type=="call", "Sell", "Buy"))
        sliderInput("hedging_nAss_hedge", label = lab, min = 0, max = nMax, value = val, step = 1, ticks = FALSE)
      }
    }
  })
  
  
  observe({
    if (!is.null(input$hedging_table_results_row_last_clicked)){
      if(isolate({length(yuimaGUIdata$hedging)})>=input$hedging_table_results_row_last_clicked & !is.null(input$hedging_nOptLot_hedge) & !is.null(input$hedging_nAss_hedge)){
        id <- input$hedging_table_results_row_last_clicked
        info <- isolate({yuimaGUIdata$hedging[[id]]$info})
        profits <- profit_distribution(nOpt=input$hedging_nOptLot_hedge*info$optLotMult, 
                                       nAss=input$hedging_nAss_hedge, 
                                       type=ifelse(is.na(input$hedging_type2) | input$hedging_type2=="default", info$type, input$hedging_type2), 
                                       strike=ifelse(is.na(input$hedging_strike2), info$strike, input$hedging_strike2),
                                       priceAtMaturity=isolate({yuimaGUIdata$hedging[[id]]$hist}), 
                                       optMarketPrice=ifelse(is.na(input$hedging_optMarketPrice2), info$optPrice, input$hedging_optMarketPrice2),
                                       assMarketPrice=info$assPrice, 
                                       percCostAss=input$hedging_percCostAss/100, 
                                       minCostAss=input$hedging_minCostAss, 
                                       lotCostOpt=input$hedging_lotCostOpt, 
                                       lotMultiplier=info$optLotMult, 
                                       shortCostPerYear=input$hedging_rateShort/100, 
                                       t0=info$estimate.to, 
                                       maturity=info$maturity)
        hedging_values$profits <- profits
        hedging_values$symb <- isolate({yuimaGUIdata$hedging[[id]]$symb})
        hedging_values$model <- isolate({yuimaGUIdata$hedging[[id]]$info$model})
      }
    }
  })
  
  output$hedging_plot_distribution <- renderPlot({
    par(bg="black")
    if (!is.null(hedging_values$profits) & !is.null(hedging_values$model) & !is.null(hedging_values$symb))
      hist(hedging_values$profits, main = paste(hedging_values$symb,"-",hedging_values$model), xlab = "", breaks = input$hedging_slider_nBin, col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
    grid()
  })
  output$hedging_slider_rangeHist <- renderUI({
    if (!is.null(hedging_values$profits)){
      Min <- min(hedging_values$profits)
      Max <- max(hedging_values$profits)
      val <- c(round(Min-1), round(Max+1))
      sliderInput("hedging_slider_rangeHist", width = "75%", min = round(Min-1), max = round(Max+1), value = val, label = "Mean & Probability", step = 1, ticks=FALSE, round = -2)
    }
  })
  output$hedging_probability_text <- renderText({
    if(!is.null(input$hedging_slider_rangeHist) & !is.null(hedging_values$profits)){
      binary <- ifelse(hedging_values$profits>=input$hedging_slider_rangeHist[1] & hedging_values$profits<=input$hedging_slider_rangeHist[2],1,0)
      prob <- mean(binary)
      sdErr <- sd(binary)/sqrt(length(binary))
      paste("Probability: ",round(100*prob,2),"%", " ± ", round(100*sdErr,2), "%")
    }
  })
  output$hedging_mean_text <- renderText({
    if(!is.null(input$hedging_slider_rangeHist) & !is.null(hedging_values$profits)){
      val <- hedging_values$profits
      val <- val[val>=input$hedging_slider_rangeHist[1] & val<=input$hedging_slider_rangeHist[2]]
      paste("Mean: ",round(mean(val),0), " ± ", round(sd(val)/sqrt(length(val)),0))
    }
  })
  output$hedging_capital_text <- renderText({
    if (!is.null(input$hedging_table_results_row_last_clicked)){
      id <- input$hedging_table_results_row_last_clicked
      info <- isolate({yuimaGUIdata$hedging[[id]]$info})
      optPrice <- ifelse(is.na(input$hedging_optMarketPrice2), info$optPrice, input$hedging_optMarketPrice2)
      cap <- input$hedging_nOptLot_hedge*(info$optLotMult*optPrice+input$hedging_lotCostOpt)+input$hedging_nAss_hedge*info$assPrice + ifelse(input$hedging_nAss_hedge>0,max(input$hedging_nAss_hedge*info$assPrice*input$hedging_percCostAss/100,input$hedging_minCostAss),0)
      paste("Invested Capitalt: ", round(cap,0))
    }
  })
  output$hedging_meanPerc_text <- renderText({
    if (!is.null(input$hedging_table_results_row_last_clicked) & !is.null(hedging_values$profits)){
      id <- input$hedging_table_results_row_last_clicked
      info <- isolate({yuimaGUIdata$hedging[[id]]$info})
      optPrice <- ifelse(is.na(input$hedging_optMarketPrice2), info$optPrice, input$hedging_optMarketPrice2)
      cap <- input$hedging_nOptLot_hedge*(info$optLotMult*optPrice+input$hedging_lotCostOpt)+input$hedging_nAss_hedge*info$assPrice + ifelse(input$hedging_nAss_hedge>0,max(input$hedging_nAss_hedge*info$assPrice*input$hedging_percCostAss/100,input$hedging_minCostAss),0)
      val <- hedging_values$profits
      val <- val[val>=input$hedging_slider_rangeHist[1] & val<=input$hedging_slider_rangeHist[2]]
      paste("Return on Capital: ", round(mean(val)/cap*100,2), "%", " ± ", round(sd(val)/cap*100/sqrt(length(val)),2)," %")
    }
  })
  
  observeEvent(input$hedging_button_saveHedging, {
    id <- input$hedging_table_results_row_last_clicked
    yuimaGUIdata$hedging[[id]]$info$assPercCost <<- input$hedging_percCostAss/100
    yuimaGUIdata$hedging[[id]]$info$assMinCost <<- input$hedging_minCostAss
    yuimaGUIdata$hedging[[id]]$info$assRateShortSelling <<- input$hedging_rateShort/100
    yuimaGUIdata$hedging[[id]]$info$optLotCost <<- input$hedging_lotCostOpt
    if (!is.na(input$hedging_type2) & input$hedging_type2!="default")
      yuimaGUIdata$hedging[[id]]$info$type <<- input$hedging_type2
    if (yuimaGUIdata$hedging[[id]]$info$type=="put"){
      yuimaGUIdata$hedging[[id]]$info$buy <<- input$hedging_nAss_hedge
      yuimaGUIdata$hedging[[id]]$info$sell <<- NA
    }
    if (yuimaGUIdata$hedging[[id]]$info$type=="call"){
      yuimaGUIdata$hedging[[id]]$info$sell <<- input$hedging_nAss_hedge
      yuimaGUIdata$hedging[[id]]$info$buy <<- NA
    }
    yuimaGUIdata$hedging[[id]]$info$LotsToBuy <<- input$hedging_nOptLot_hedge
    if (!is.na(input$hedging_strike2))
      yuimaGUIdata$hedging[[id]]$info$strike <<- input$hedging_strike2
    if (!is.na(input$hedging_optMarketPrice2))
      yuimaGUIdata$hedging[[id]]$info$optPrice <<- input$hedging_optMarketPrice2
    yuimaGUIdata$hedging[[id]]$info$profit <<- mean(hedging_values$profits)/(input$hedging_nOptLot_hedge*(yuimaGUIdata$hedging[[id]]$info$optLotMult*yuimaGUIdata$hedging[[id]]$info$optPrice+input$hedging_lotCostOpt)+input$hedging_nAss_hedge*yuimaGUIdata$hedging[[id]]$info$assPrice + ifelse(input$hedging_nAss_hedge>0,max(input$hedging_nAss_hedge*yuimaGUIdata$hedging[[id]]$info$assPrice*input$hedging_percCostAss/100,input$hedging_minCostAss),0))
    yuimaGUIdata$hedging[[id]]$info$stdErr <<- sd(hedging_values$profits)/sqrt(length(hedging_values$profits))/(input$hedging_nOptLot_hedge*(yuimaGUIdata$hedging[[id]]$info$optLotMult*yuimaGUIdata$hedging[[id]]$info$optPrice+input$hedging_lotCostOpt)+input$hedging_nAss_hedge*yuimaGUIdata$hedging[[id]]$info$assPrice + ifelse(input$hedging_nAss_hedge>0,max(input$hedging_nAss_hedge*yuimaGUIdata$hedging[[id]]$info$assPrice*input$hedging_percCostAss/100,input$hedging_minCostAss),0))
  })

  observe({
    shinyjs::toggle("hedging_alert_selectRow", condition = (input$panel_hedging=="Start simulations"))
    valid <- FALSE
    if (!is.null(input$hedging_table_results_row_last_clicked) & !is.null(input$hedging_table_results_rows_selected))
      if (input$hedging_table_results_row_last_clicked %in% input$hedging_table_results_rows_selected)
        valid <- TRUE
    shinyjs::toggle("hedging_body", condition = valid)
  })
  
  ###Delete Hedging
  observeEvent(input$hedging_button_delete, priority = 1, {
    if(!is.null(input$hedging_table_results_rows_selected) & !is.null(input$hedging_table_results_row_last_clicked)){
      if(input$hedging_table_results_row_last_clicked %in% input$hedging_table_results_rows_selected){
        delHedging(n=input$hedging_table_results_row_last_clicked)
      }
    }
    shinyjs::hide("hedging_body")
  })
  
  ###DeleteAll Hedging
  observeEvent(input$hedging_button_deleteAll, priority = 1, {
    if(!is.null(input$hedging_table_results_rows_all))
      delHedging(n=input$hedging_table_results_rows_all)
    shinyjs::hide("hedging_body")
  })
  
  
}




