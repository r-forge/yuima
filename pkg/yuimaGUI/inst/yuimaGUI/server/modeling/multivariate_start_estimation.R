###Model Input depending on Class Input
output$multi_model <- renderUI({
  choices <- as.vector(defaultModels[names(defaultModels)==input$multi_modelClass])
  if(input$multi_modelClass!="Fractional process")
    for(i in names(yuimaGUIdata$usr_model))
      if (yuimaGUIdata$usr_model[[i]]$class==input$multi_modelClass) {
        if(input$multi_modelClass!="Diffusion process") choices <- c(i, choices)
        else if (length(setModelByName(name = i)@parameter@all)!=0) choices <- c(i, choices)
      }
  return (selectInput("multi_model",label = "Model Name", choices = choices, multiple = TRUE))
})

output$multi_jumps <- renderUI({
  if (input$multi_modelClass=="Compound Poisson")
    return(selectInput("multi_jumps",label = "Jumps", choices = defaultJumps))
  if (input$multi_modelClass=="Levy process"){
    jump_choices <- defaultJumps
    jump_sel <- NULL
    if(!is.null(input$multi_model)){
      if(input$multi_model=="Geometric Brownian Motion with Jumps") jump_sel <- "Gaussian"
    }
    return(div(
      column(6,selectInput("model_levy_intensity", label = "Intensity", choices = c(#"None",
        "Constant"="lambda"))),
      column(6,selectInput("multi_jumps",label = "Jumps", choices = jump_choices, selected = jump_sel)))
    )
  }
  
})

output$multi_pq_C <- renderUI({
  if (input$multi_modelClass=="CARMA")
    return(div(
      column(6,numericInput("AR_C",label = "AR degree (p)", value = 2, min = 1, step = 1)),
      column(6,numericInput("MA_C",label = "MA degree (q)", value = 1, min = 1, step = 1))
    ))
  if (input$multi_modelClass=="COGARCH")
    return(div(
      column(6,numericInput("AR_C",label = "AR degree (p)", value = 1, min = 1, step = 1)),
      column(6,numericInput("MA_C",label = "MA degree (q)", value = 1, min = 1, step = 1))
    ))
})

###Print last selected multi_model in Latex
output$multi_PrintModelLatex <- renderUI({
  shinyjs::hide("multi_titlePrintModelLatex")
  if (!is.null(input$multi_model)){
    shinyjs::show("multi_titlePrintModelLatex")
    class <- isolate({input$multi_modelClass})
    return(withMathJax(printModelLatex(names = input$multi_model, process = class, jumps = jumps_shortcut(class = class, jumps = input$multi_jumps))))
  }
})


###Display available data
output$multi_database3 <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
  if (length(yuimaGUItable$series)==0){
    NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
    return(NoData[-1,])
  }
  return (yuimaGUItable$series)
})

###Table of selected data to multi_model
multi_seriesToEstimate <- reactiveValues(table=data.frame())

###Select Button
observeEvent(input$multi_buttonSelect_models_Univariate, priority = 1, {
  multi_seriesToEstimate$table <<- rbind(multi_seriesToEstimate$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% rownames(yuimaGUItable$series)[input$multi_database3_rows_selected]) & !(rownames(yuimaGUItable$series) %in% rownames(multi_seriesToEstimate$table)),])
})

###SelectAll Button
observeEvent(input$multi_buttonSelectAll_models_Univariate, priority = 1, {
  multi_seriesToEstimate$table <<- rbind(multi_seriesToEstimate$table, yuimaGUItable$series[(rownames(yuimaGUItable$series) %in% rownames(yuimaGUItable$series)[input$multi_database3_rows_all]) & !(rownames(yuimaGUItable$series) %in% rownames(multi_seriesToEstimate$table)),])
})

###Display Selected Data
output$multi_database4 <- DT::renderDataTable(options=list(order = list(1, 'desc'), scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = FALSE, selection = "multiple",{
  if (nrow(multi_seriesToEstimate$table)==0){
    NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
    return(NoData[-1,])
  }
  return (multi_seriesToEstimate$table)
})


###Control selected data to be in yuimaGUIdata$series
observe({
  if(length(multi_seriesToEstimate$table)!=0){
    if (length(yuimaGUItable$series)==0)
      multi_seriesToEstimate$table <<- data.frame()
    else
      multi_seriesToEstimate$table <<- multi_seriesToEstimate$table[which(as.character(multi_seriesToEstimate$table[,"Symb"]) %in% as.character(yuimaGUItable$series[,"Symb"])),]
  }
})

###Delete Button
observeEvent(input$multi_buttonDelete_models_Univariate, priority = 1,{
  if (!is.null(input$multi_database4_rows_selected))
    multi_seriesToEstimate$table <<- multi_seriesToEstimate$table[-input$multi_database4_rows_selected,]
})

###DeleteAll Button
observeEvent(input$multi_buttonDeleteAll_models_Univariate, priority = 1,{
  if (!is.null(input$multi_database4_rows_all))
    multi_seriesToEstimate$table <<- multi_seriesToEstimate$table[-input$multi_database4_rows_all,]
})

###Interactive range of multi_selectRange chart
range_selectRange <- reactiveValues(x=NULL, y=NULL)
observe({
  if (!is.null(input$multi_selectRange_brush) & !is.null(input$multi_plotsRangeSeries)){
    data <- getData(input$multi_plotsRangeSeries)
    test <- (length(index(window(data, start = input$multi_selectRange_brush$xmin, end = input$multi_selectRange_brush$xmax))) > 3)
    if (test==TRUE){
      range_selectRange$x <- c(as.Date(input$multi_selectRange_brush$xmin), as.Date(input$multi_selectRange_brush$xmax))
      range_selectRange$y <- c(input$multi_selectRange_brush$ymin, input$multi_selectRange_brush$ymax)
    }
  }
})


observe({
  shinyjs::toggle(id="multi_plotsRangeErrorMessage", condition = nrow(multi_seriesToEstimate$table)==0)
  shinyjs::toggle(id="multi_plotsRangeAll", condition = nrow(multi_seriesToEstimate$table)!=0)
})

###Display charts: series and its increments
observe({
  symb <- input$multi_plotsRangeSeries
  if(!is.null(symb))
    if (symb %in% rownames(yuimaGUItable$series)){
      data <- getData(symb)
      incr <- na.omit(Delt(data, type = "arithmetic"))
      condition <- all(is.finite(incr))
      shinyjs::toggle("multi_selectRangeReturns", condition = condition)
      range_selectRange$x <- NULL
      range_selectRange$y <- NULL
      start <- as.character(multi_seriesToEstimate$table[input$multi_plotsRangeSeries,"From"])
      end <- as.character(multi_seriesToEstimate$table[input$multi_plotsRangeSeries,"To"])
      if(class(index(data))=="numeric"){
        start <- as.numeric(start)
        end <- as.numeric(end)
      }
      output$multi_selectRange <- renderPlot({
        if ((symb %in% rownames(yuimaGUItable$series) & (symb %in% rownames(multi_seriesToEstimate$table)))){
          par(bg="black")
          plot.zoo(window(data, start = range_selectRange$x[1], end = range_selectRange$x[2]), main=symb, xlab="Index", ylab=NA, log=switch(input$multi_scale_selectRange,"Linear"="","Logarithmic (Y)"="y", "Logarithmic (X)"="x", "Logarithmic (XY)"="xy"), col="grey", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
          lines(window(data, start = start, end = end), col = "green")
          grid(col="grey")
        }
      })
      output$multi_selectRangeReturns <- renderPlot({
        if (symb %in% rownames(yuimaGUItable$series) & (symb %in% rownames(multi_seriesToEstimate$table)) & condition){
          par(bg="black")
          plot.zoo( window(incr, start = range_selectRange$x[1], end = range_selectRange$x[2]), main=paste(symb, " - Percentage Increments"), xlab="Index", ylab=NA, log=switch(input$multi_scale_selectRange,"Linear"="","Logarithmic (Y)"="", "Logarithmic (X)"="x", "Logarithmic (XY)"="x"), col="grey", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
          lines(window(incr, start = start,  end = end), col = "green")
          grid(col="grey")
        }
      })
    }
})


output$multi_plotsRangeSeries <- renderUI({
  selectInput("multi_plotsRangeSeries", label = "Series", choices = rownames(multi_seriesToEstimate$table), selected = input$multi_plotsRangeSeries)
})

###Choose Range input set to "Select range from charts" if charts have been brushed
output$multi_chooseRange <- renderUI({
  sel <- "full"
  if (!is.null(range_selectRange$x)) sel <- "selected"
  selectInput("multi_chooseRange", label = "Range", choices = c("Full Range" = "full", "Select Range from Charts" = "selected", "Specify Range" = "specify"), selected = sel)
})

output$multi_chooseRange_specify <- renderUI({
  if(!is.null(input$multi_plotsRangeSeries)) {
    data <- getData(input$multi_plotsRangeSeries)
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
  shinyjs::toggle(id = "multi_chooseRange_specify", condition = (input$multi_chooseRange)=="specify")
})

###Function to update data range to use to estimate models
updateRange_multi_seriesToEstimate <- function(symb, range = c("full","selected","specify"), type = c("Date", "numeric")){
  for (i in symb){
    data <- getData(i)
    if (range == "full"){
      levels(multi_seriesToEstimate$table[,"From"]) <- c(levels(multi_seriesToEstimate$table[,"From"]), as.character(start(data)))
      levels(multi_seriesToEstimate$table[,"To"]) <- c(levels(multi_seriesToEstimate$table[,"To"]), as.character(end(data)))
      multi_seriesToEstimate$table[i,"From"] <<- as.character(start(data))
      multi_seriesToEstimate$table[i,"To"] <<- as.character(end(data))
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
        levels(multi_seriesToEstimate$table[,"From"]) <- c(levels(multi_seriesToEstimate$table[,"From"]), as.character(start))
        levels(multi_seriesToEstimate$table[,"To"]) <- c(levels(multi_seriesToEstimate$table[,"To"]), as.character(end))
        multi_seriesToEstimate$table[i,"From"] <<- as.character(start)
        multi_seriesToEstimate$table[i,"To"] <<- as.character(end)
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
        levels(multi_seriesToEstimate$table[,"From"]) <- c(levels(multi_seriesToEstimate$table[,"From"]), as.character(start))
        levels(multi_seriesToEstimate$table[,"To"]) <- c(levels(multi_seriesToEstimate$table[,"To"]), as.character(end))
        multi_seriesToEstimate$table[i,"From"] <<- as.character(start)
        multi_seriesToEstimate$table[i,"To"] <<- as.character(end)
      }
    }
  }
}

###Apply selected range by double click
observeEvent(input$multi_selectRange_dbclick, priority = 1, {
  updateRange_multi_seriesToEstimate(input$multi_plotsRangeSeries, range = "selected", type = class(index(getData(input$multi_plotsRangeSeries))))
})

###Apply selected range
observeEvent(input$multi_buttonApplyRange, priority = 1, {
  updateRange_multi_seriesToEstimate(input$multi_plotsRangeSeries, range = input$multi_chooseRange, type = class(index(getData(input$multi_plotsRangeSeries))))
})

###ApplyAll selected range
observeEvent(input$multi_buttonApplyAllRange, priority = 1, {
  updateRange_multi_seriesToEstimate(rownames(multi_seriesToEstimate$table), range = input$multi_chooseRange, type = class(index(getData(input$multi_plotsRangeSeries))))
})


prev_buttonDelta <- 0
prev_buttonAllDelta <- 0
observe({
  class <- isolate({input$multi_modelClass})
  for (symb in rownames(multi_seriesToEstimate$table)){
    if (is.null(yuimaGUIsettings$delta[[symb]])) {
      i <- index(getData(symb))
      if(is.numeric(i)) yuimaGUIsettings$delta[[symb]] <<- mode(diff(i))
      else yuimaGUIsettings$delta[[symb]] <<- 0.01
    }
    if (is.null(yuimaGUIsettings$toLog[[symb]])) yuimaGUIsettings$toLog[[symb]] <<- FALSE
    data <- na.omit(as.numeric(getData(symb)))
    if (yuimaGUIsettings$toLog[[symb]]==TRUE) data <- log(data)
    for (modName in input$multi_model){
      if (class(try(setModelByName(modName, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = class, jumps = input$multi_jumps), AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA))))!="try-error"){
        if (is.null(yuimaGUIsettings$estimation[[modName]]))
          yuimaGUIsettings$estimation[[modName]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]]))
          yuimaGUIsettings$estimation[[modName]][[symb]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$multi_advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$multi_advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["start"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$multi_advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$multi_advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["start"]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]] <<- setThreshold(class = class, data = data)
        
        startMinMax <- defaultBounds(name = modName, 
                                     jumps = jumps_shortcut(class = class, jumps = input$multi_jumps), 
                                     intensity = input$model_levy_intensity,
                                     threshold = yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]],
                                     AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), 
                                     MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA), 
                                     strict = FALSE,
                                     data = data,
                                     delta = yuimaGUIsettings$delta[[symb]])
        upperLower <- defaultBounds(name = modName, 
                                    jumps = jumps_shortcut(class = class, jumps = input$multi_jumps), 
                                    intensity = input$model_levy_intensity,
                                    threshold = yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]],
                                    AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), 
                                    MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA),
                                    strict = TRUE,
                                    data = data,
                                    delta = yuimaGUIsettings$delta[[symb]])
        
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$multi_advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$multi_advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]] <<- startMinMax$lower
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$multi_advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$multi_advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]] <<- startMinMax$upper
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$multi_advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$multi_advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]] <<- upperLower$upper
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$multi_advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$multi_advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]] <<- upperLower$lower
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["method"]])){
          if(class=="COGARCH" | class=="CARMA") yuimaGUIsettings$estimation[[modName]][[symb]][["method"]] <<- "SANN"
          else yuimaGUIsettings$estimation[[modName]][[symb]][["method"]] <<- "L-BFGS-B"
        }
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]] <<- 1
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]] <<- NA
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]] <<- FALSE
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]] <<- TRUE
      }
    }
  }
  prev_buttonDelta <<- input$multi_advancedSettingsButtonApplyDelta
  prev_buttonAllDelta <<- input$multi_advancedSettingsButtonApplyAllDelta
})

observe({
  valid <- TRUE
  if (nrow(multi_seriesToEstimate$table)==0 | is.null(input$multi_model)) valid <- FALSE
  else for(mod in input$multi_model) if  (class(try(setModelByName(mod, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = input$multi_modelClass, jumps = input$multi_jumps), AR_C = ifelse(input$multi_modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$multi_modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))))=="try-error")  valid <- FALSE
  shinyjs::toggle(id="multi_advancedSettingsAll", condition = valid)
  shinyjs::toggle(id="multi_advancedSettingsErrorMessage", condition = !valid)
})
output$multi_advancedSettingsSeries <- renderUI({
  if (nrow(multi_seriesToEstimate$table)!=0)
    selectInput(inputId = "multi_advancedSettingsSeries", label = "Series", choices = rownames(multi_seriesToEstimate$table))
})
output$multi_advancedSettingsDelta <- renderUI({
  if (!is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries))
    return (numericInput("multi_advancedSettingsDelta", label = paste("delta", input$multi_advancedSettingsSeries), value = yuimaGUIsettings$delta[[input$multi_advancedSettingsSeries]]))
})
output$multi_advancedSettingsToLog <- renderUI({
  if (!is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries)){
    choices <- FALSE
    if (all(getData(input$multi_advancedSettingsSeries)>0)) choices <- c(FALSE, TRUE)
    return (selectInput("multi_advancedSettingsToLog", label = "Convert to log", choices = choices, selected = yuimaGUIsettings$toLog[[input$multi_advancedSettingsSeries]]))
  }
})
output$multi_advancedSettingsModel <- renderUI({
  if(!is.null(input$multi_model))
    selectInput(inputId = "multi_advancedSettingsModel", label = "Model", choices = input$multi_model)
})
output$multi_advancedSettingsParameter <- renderUI({
  if (!is.null(input$multi_model))
    if (!is.null(input$multi_advancedSettingsModel)){
      parL <- setModelByName(input$multi_advancedSettingsModel, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = input$multi_modelClass, jumps = input$multi_jumps), AR_C = ifelse(input$multi_modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$multi_modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))@parameter
      par <- parL@all
      if (input$multi_modelClass=="COGARCH") par <- unique(c(parL@drift, parL@xinit))
      if (input$multi_modelClass=="CARMA") par <- parL@drift
      selectInput(inputId = "multi_advancedSettingsParameter", label = "Parameter", choices = par)
    }
})
#REMOVE# output$multi_advancedSettingsFixed <- renderUI({
#REMOVE#  if (!is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries) & !is.null(input$multi_advancedSettingsParameter))
#REMOVE#    numericInput(inputId = "multi_advancedSettingsFixed", label = "fixed", value = ifelse(is.null(yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["fixed"]][[input$multi_advancedSettingsParameter]]),NA,yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["fixed"]][[input$multi_advancedSettingsParameter]]))
#REMOVE#})
output$multi_advancedSettingsStart <- renderUI({
  if (#REMOVE# !is.null(input$multi_advancedSettingsFixed) & 
    !is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries) & !is.null(input$multi_advancedSettingsParameter))
    #REMOVE# if (is.na(input$multi_advancedSettingsFixed))
    numericInput(inputId = "multi_advancedSettingsStart", label = "start", value = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["start"]][[input$multi_advancedSettingsParameter]])
})
output$multi_advancedSettingsStartMin <- renderUI({
  input$multi_advancedSettingsButtonApplyDelta
  input$multi_advancedSettingsButtonApplyAllDelta
  if (#REMOVE# !is.null(input$multi_advancedSettingsFixed) & 
    !is.null(input$multi_advancedSettingsStart) & !is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries) & !is.null(input$multi_advancedSettingsParameter))
    if (#REMOVE# is.na(input$multi_advancedSettingsFixed) & 
      is.na(input$multi_advancedSettingsStart))
      numericInput(inputId = "multi_advancedSettingsStartMin", label = "start: Min", value = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["startMin"]][[input$multi_advancedSettingsParameter]])
})
output$multi_advancedSettingsStartMax <- renderUI({
  input$multi_advancedSettingsButtonApplyDelta
  input$multi_advancedSettingsButtonApplyAllDelta
  if (#REMOVE# !is.null(input$multi_advancedSettingsFixed) & 
    !is.null(input$multi_advancedSettingsStart) & !is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries) & !is.null(input$multi_advancedSettingsParameter))
    if (#REMOVE# is.na(input$multi_advancedSettingsFixed) & 
      is.na(input$multi_advancedSettingsStart))
      numericInput(inputId = "multi_advancedSettingsStartMax", label = "start: Max", value = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["startMax"]][[input$multi_advancedSettingsParameter]])
})
output$multi_advancedSettingsLower <- renderUI({
  if (#REMOVE# !is.null(input$multi_advancedSettingsFixed) & 
    !is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries) & !is.null(input$multi_advancedSettingsParameter))
    #REMOVE# if (is.na(input$multi_advancedSettingsFixed))
    if (input$multi_advancedSettingsMethod=="L-BFGS-B" | input$multi_advancedSettingsMethod=="Brent")
      numericInput("multi_advancedSettingsLower", label = "lower", value = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["lower"]][[input$multi_advancedSettingsParameter]])
})
output$multi_advancedSettingsUpper <- renderUI({
  if (#REMOVE# !is.null(input$multi_advancedSettingsFixed) & 
    !is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries) & !is.null(input$multi_advancedSettingsParameter))
    #REMOVE# if (is.na(input$multi_advancedSettingsFixed))
    if (input$multi_advancedSettingsMethod=="L-BFGS-B" | input$multi_advancedSettingsMethod=="Brent")
      numericInput("multi_advancedSettingsUpper", label = "upper", value = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["upper"]][[input$multi_advancedSettingsParameter]])
})
#REMOVE# output$multi_advancedSettingsJoint <- renderUI({
#REMOVE#   if (!is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries))
#REMOVE#     selectInput("multi_advancedSettingsJoint", label = "joint", choices = c(FALSE, TRUE), selected = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["joint"]])
#REMOVE# })
output$multi_advancedSettingsMethod <- renderUI({
  if (!is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries))
    selectInput("multi_advancedSettingsMethod", label = "method", choices = c("L-BFGS-B", "Nelder-Mead", "BFGS", "CG", "SANN", "Brent"), selected = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["method"]])
})
#REMOVE# output$multi_advancedSettingsAggregation <- renderUI({
#REMOVE#   if (!is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries))
#REMOVE#     selectInput("multi_advancedSettingsAggregation", label = "aggregation", choices = c(TRUE, FALSE), selected = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["aggregation"]])
#REMOVE# })
output$multi_advancedSettingsThreshold <- renderUI({
  if (!is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries)) if(isolate({input$multi_modelClass})=="Levy process")
    numericInput("multi_advancedSettingsThreshold", label = "threshold", value = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["threshold"]])
})
output$multi_advancedSettingsTrials <- renderUI({
  if (!is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries) & !is.null(input$multi_advancedSettingsMethod))
    numericInput("multi_advancedSettingsTrials", label = "trials", min = 1, value = ifelse(input$multi_advancedSettingsMethod=="SANN" & yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["method"]]!="SANN",1,yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["trials"]]))
})
output$multi_advancedSettingsSeed <- renderUI({
  if (!is.null(input$multi_advancedSettingsModel) & !is.null(input$multi_advancedSettingsSeries))
    numericInput("multi_advancedSettingsSeed", label = "seed", min = 1, value = yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["seed"]])
})



observeEvent(input$multi_advancedSettingsButtonApplyDelta, {
  yuimaGUIsettings$delta[[input$multi_advancedSettingsSeries]] <<- input$multi_advancedSettingsDelta
  yuimaGUIsettings$toLog[[input$multi_advancedSettingsSeries]] <<- input$multi_advancedSettingsToLog
})
observeEvent(input$multi_advancedSettingsButtonApplyAllDelta, {
  for (symb in rownames(multi_seriesToEstimate$table)){
    yuimaGUIsettings$delta[[symb]] <<- input$multi_advancedSettingsDelta
    if (input$multi_advancedSettingsToLog==FALSE) yuimaGUIsettings$toLog[[symb]] <<- input$multi_advancedSettingsToLog
    else if (all(getData(symb)>0)) yuimaGUIsettings$toLog[[symb]] <<- input$multi_advancedSettingsToLog
  }
})
observeEvent(input$multi_advancedSettingsButtonApplyModel,{
  #REMOVE# yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["fixed"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsFixed
  yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["start"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsStart
  yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["startMin"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsStartMin
  yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["startMax"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsStartMax
  yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["lower"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsLower
  yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["upper"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsUpper
})
observeEvent(input$multi_advancedSettingsButtonApplyAllModel,{
  for (symb in rownames(multi_seriesToEstimate$table)){
    #REMOVE# yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["fixed"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsFixed
    yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["start"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsStart
    yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["startMin"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsStartMin
    yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["startMax"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsStartMax
    yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["lower"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsLower
    yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["upper"]][[input$multi_advancedSettingsParameter]] <<- input$multi_advancedSettingsUpper
  }
})
observeEvent(input$multi_advancedSettingsButtonApplyGeneral,{
  yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["method"]] <<- input$multi_advancedSettingsMethod
  yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["trials"]] <<- input$multi_advancedSettingsTrials
  yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["seed"]] <<- input$multi_advancedSettingsSeed
  #REMOVE# yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["joint"]] <<- input$multi_advancedSettingsJoint
  #REMOVE# yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["aggregation"]] <<- input$multi_advancedSettingsAggregation
  yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[input$multi_advancedSettingsSeries]][["threshold"]] <<- input$multi_advancedSettingsThreshold
})
observeEvent(input$multi_advancedSettingsButtonApplyAllModelGeneral,{
  for (symb in rownames(multi_seriesToEstimate$table)){
    yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["method"]] <<- input$multi_advancedSettingsMethod
    yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["trials"]] <<- input$multi_advancedSettingsTrials
    yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["seed"]] <<- input$multi_advancedSettingsSeed
    #REMOVE# yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["joint"]] <<- input$multi_advancedSettingsJoint
    #REMOVE# yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["aggregation"]] <<- input$multi_advancedSettingsAggregation
    yuimaGUIsettings$estimation[[input$multi_advancedSettingsModel]][[symb]][["threshold"]] <<- input$multi_advancedSettingsThreshold
  }
})
observeEvent(input$multi_advancedSettingsButtonApplyAllGeneral,{
  for (mod in input$multi_model){
    for (symb in rownames(multi_seriesToEstimate$table)){
      yuimaGUIsettings$estimation[[mod]][[symb]][["method"]] <<- input$multi_advancedSettingsMethod
      yuimaGUIsettings$estimation[[mod]][[symb]][["trials"]] <<- input$multi_advancedSettingsTrials
      yuimaGUIsettings$estimation[[mod]][[symb]][["seed"]] <<- input$multi_advancedSettingsSeed
      #REMOVE# yuimaGUIsettings$estimation[[mod]][[symb]][["joint"]] <<- input$multi_advancedSettingsJoint
      #REMOVE# yuimaGUIsettings$estimation[[mod]][[symb]][["aggregation"]] <<- input$multi_advancedSettingsAggregation
      yuimaGUIsettings$estimation[[mod]][[symb]][["threshold"]] <<- input$multi_advancedSettingsThreshold
    }
  }
})

observe({
  closeAlert(session = session, alertId = "CARMA_COGARCH_err")
  if(!is.null(input$multi_modelClass)) if(input$multi_modelClass=="CARMA" ) if(!is.null(input$AR_C)) if(!is.null(input$MA_C)) if(!is.na(input$AR_C) & !is.na(input$MA_C)) {
    if(input$AR_C<=input$MA_C)
      createAlert(session = session, anchorId = "multi_panel_run_estimation_alert", alertId = "CARMA_COGARCH_err", style = "error", content = "AR degree (p) must be greater than MA degree (q)")
    if(input$AR_C== 0 | input$MA_C==0)
      createAlert(session = session, anchorId = "multi_panel_run_estimation_alert", alertId = "CARMA_COGARCH_err", style = "error", content = "AR and MA degree (p,q) must be positive")
  }
  if(!is.null(input$multi_modelClass)) if(input$multi_modelClass=="COGARCH" ) if(!is.null(input$AR_C)) if(!is.null(input$MA_C)) if(!is.na(input$AR_C) & !is.na(input$MA_C)) {
    if(input$AR_C<input$MA_C)
      createAlert(session = session, anchorId = "multi_panel_run_estimation_alert", alertId = "CARMA_COGARCH_err", style = "error", content = "AR degree (p) must be greater than or equal to MA degree (q)")
    if(input$AR_C== 0 | input$MA_C==0)
      createAlert(session = session, anchorId = "multi_panel_run_estimation_alert", alertId = "CARMA_COGARCH_err", style = "error", content = "AR and MA degree (p,q) must be positive")
  }  
})


###Estimate models
observeEvent(input$multi_EstimateModels,{
  closeAlert(session = session, alertId = "modelsErr")
  valid <- TRUE
  if(is.null(input$multi_model) | nrow(multi_seriesToEstimate$table)==0) valid <- FALSE
  else if (input$multi_modelClass=="Compound Poisson" & is.null(input$multi_jumps)) valid <- FALSE
  else for(mod in input$multi_model) if (class(try(setModelByName(mod, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = input$multi_modelClass, jumps = input$multi_jumps), AR_C = ifelse(input$multi_modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$multi_modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))))=="try-error")  valid <- FALSE
  if(!valid){
    createAlert(session = session, anchorId = "multi_panel_run_estimation_alert", alertId = "modelsAlert_err", content = "Select some series and (valid) models to estimate", style = "warning")
  }
  if(valid){
    withProgress(message = 'Estimating: ',{
      for (modName in input$multi_model){
        for (i in rownames(multi_seriesToEstimate$table)){
          symb <- as.character(multi_seriesToEstimate$table[i,"Symb"])
          incProgress(1/(length(input$multi_model)*nrow(multi_seriesToEstimate$table)), detail = paste(symb,"-",modName))
          data <- getData(symb)
          start <- as.character(multi_seriesToEstimate$table[i,"From"])
          end <- as.character(multi_seriesToEstimate$table[i,"To"])
          times <- index(data)
          if (class(times)=="numeric")
            data <- data[(times >= as.numeric(start)) & (times <= as.numeric(end)), , drop = FALSE]
          else
            data <- data[(times >= start) & (times <= end), , drop = FALSE]
          addModel(
            modName = modName,
            modClass = input$multi_modelClass,
            intensity_levy = input$model_levy_intensity,
            AR_C = ifelse(input$multi_modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), 
            MA_C = ifelse(input$multi_modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA),
            jumps = jumps_shortcut(class = input$multi_modelClass, jumps = input$multi_jumps),
            symbName = symb,
            data = data,
            delta = yuimaGUIsettings$delta[[symb]],
            toLog = yuimaGUIsettings$toLog[[symb]],
            start = yuimaGUIsettings$estimation[[modName]][[symb]][["start"]],
            startMin = yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]],
            startMax = yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]],
            method=yuimaGUIsettings$estimation[[modName]][[symb]][["method"]],
            trials=yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]],
            seed = yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]],
            fixed = yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]],
            lower = yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]],
            upper = yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]],
            joint = yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]],
            aggregation = yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]],
            threshold = yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]],
            session = session,
            anchorId = "multi_panel_estimates_alert",
            alertId = NULL
          )
        }
      }
    })
    updateTabsetPanel(session = session,  inputId = "multi_panel_estimates", selected = "Estimates")
  }
})

observe({
  valid <- TRUE
  if(is.null(input$multi_model) | nrow(multi_seriesToEstimate$table)==0) valid <- FALSE
  else if (input$multi_modelClass=="Compound Poisson" & is.null(input$multi_jumps)) valid <- FALSE
  if(valid) closeAlert(session, alertId = "modelsAlert_err")
})




