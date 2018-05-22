###Model Input depending on Class Input
output$model <- renderUI({
  choices <- as.vector(defaultModels[names(defaultModels)==input$modelClass])
  if(input$modelClass!="Fractional process")
    for(i in names(yuimaGUIdata$usr_model))
      if (yuimaGUIdata$usr_model[[i]]$class==input$modelClass) {
        if(input$modelClass!="Diffusion process") choices <- c(i, choices)
        else if (length(getAllParams(mod = setModelByName(name = i), class = input$modelClass))!=0) choices <- c(i, choices)
      }
  return (selectInput("model",label = "Model Name", choices = choices, multiple = TRUE))
})

output$jumps <- renderUI({
  if (input$modelClass=="Compound Poisson")
    return(selectInput("jumps",label = "Jumps", choices = defaultJumps))
  if (input$modelClass=="Levy process"){
    jump_choices <- defaultJumps
    jump_sel <- NULL
    if(!is.null(input$model)){
      if(input$model=="Geometric Brownian Motion with Jumps") jump_sel <- "Gaussian"
    }
    return(div(
      column(6,selectInput("model_levy_intensity", label = "Intensity", choices = c(#"None",
        "Constant"="lambda"))),
      column(6,selectInput("jumps",label = "Jumps", choices = jump_choices, selected = jump_sel)))
    )
  }
  
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


###Display available data
output$database3 <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
  if (length(yuimaGUItable$series)==0){
    NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
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
    NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
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
    if (is.null(yuimaGUIsettings$delta[[symb]])) {
      i <- index(getData(symb))
      if(is.numeric(i)) yuimaGUIsettings$delta[[symb]] <<- mode(diff(i))
      else yuimaGUIsettings$delta[[symb]] <<- 0.01
    }
    if (is.null(yuimaGUIsettings$toLog[[symb]])) yuimaGUIsettings$toLog[[symb]] <<- FALSE
    data <- getData(symb)
    if (yuimaGUIsettings$toLog[[symb]]==TRUE) data <- log(data)
    for (modName in input$model){
      if (class(try(setModelByName(modName, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = class, jumps = input$jumps), AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA))))!="try-error"){
        if (is.null(yuimaGUIsettings$estimation[[modName]]))
          yuimaGUIsettings$estimation[[modName]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]]))
          yuimaGUIsettings$estimation[[modName]][[symb]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["start"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["start"]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]] <<- setThreshold(class = class, data = data)
        
        startMinMax <- defaultBounds(name = modName, 
                                     jumps = jumps_shortcut(class = class, jumps = input$jumps), 
                                     intensity = input$model_levy_intensity,
                                     threshold = yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]],
                                     AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), 
                                     MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA), 
                                     strict = FALSE,
                                     data = data,
                                     delta = yuimaGUIsettings$delta[[symb]])
        upperLower <- defaultBounds(name = modName, 
                                    jumps = jumps_shortcut(class = class, jumps = input$jumps), 
                                    intensity = input$model_levy_intensity,
                                    threshold = yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]],
                                    AR_C = ifelse(class %in% c("CARMA","COGARCH"), input$AR_C, NA), 
                                    MA_C = ifelse(class %in% c("CARMA","COGARCH"), input$MA_C, NA),
                                    strict = TRUE,
                                    data = data,
                                    delta = yuimaGUIsettings$delta[[symb]])
        
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]] <<- startMinMax$lower
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]] <<- startMinMax$upper
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]] <<- upperLower$upper
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]]) | !(class %in% c("Diffusion process", "Fractional process")) | prev_buttonDelta!=input$advancedSettingsButtonApplyDelta | prev_buttonAllDelta!=input$advancedSettingsButtonApplyAllDelta)
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
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["timeout"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["timeout"]] <<- Inf
      }
    }
  }
  prev_buttonDelta <<- input$advancedSettingsButtonApplyDelta
  prev_buttonAllDelta <<- input$advancedSettingsButtonApplyAllDelta
})

observe({
  valid <- TRUE
  if (nrow(seriesToEstimate$table)==0 | is.null(input$model)) valid <- FALSE
  else for(mod in input$model) if  (class(try(setModelByName(mod, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps), AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))))=="try-error")  valid <- FALSE
  shinyjs::toggle(id="advancedSettingsAll", condition = valid)
  shinyjs::toggle(id="advancedSettingsErrorMessage", condition = !valid)
})
output$advancedSettingsSeries <- renderUI({
  if (nrow(seriesToEstimate$table)!=0)
    selectInput(inputId = "advancedSettingsSeries", label = "Series", choices = rownames(seriesToEstimate$table))
})
output$advancedSettingsDelta <- renderUI({
  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
    return (numericInput("advancedSettingsDelta", label = paste("delta", input$advancedSettingsSeries), value = yuimaGUIsettings$delta[[input$advancedSettingsSeries]], min = 0))
})
output$advancedSettingsToLog <- renderUI({
  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries)){
    choices <- FALSE
    if (all(getData(input$advancedSettingsSeries)>0)) choices <- c(FALSE, TRUE)
    return (selectInput("advancedSettingsToLog", label = "Convert to log", choices = choices, selected = yuimaGUIsettings$toLog[[input$advancedSettingsSeries]]))
  }
})
output$advancedSettingsModel <- renderUI({
  if(!is.null(input$model))
    selectInput(inputId = "advancedSettingsModel", label = "Model", choices = input$model)
})
output$advancedSettingsParameter <- renderUI({
  if (!is.null(input$model))
    if (!is.null(input$advancedSettingsModel)){
      mod <- setModelByName(input$advancedSettingsModel, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps), AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))
	  par <- getAllParams(mod, input$modelClass)
      selectInput(inputId = "advancedSettingsParameter", label = "Parameter", choices = par)
    }
})
#REMOVE# output$advancedSettingsFixed <- renderUI({
#REMOVE#  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
#REMOVE#    numericInput(inputId = "advancedSettingsFixed", label = "fixed", value = ifelse(is.null(yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]]),NA,yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]]))
#REMOVE#})
output$advancedSettingsStart <- renderUI({
  if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
    !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
    #REMOVE# if (is.na(input$advancedSettingsFixed))
    numericInput(inputId = "advancedSettingsStart", label = "start", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["start"]][[input$advancedSettingsParameter]])
})
output$advancedSettingsStartMin <- renderUI({
  input$advancedSettingsButtonApplyDelta
  input$advancedSettingsButtonApplyAllDelta
  if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
    !is.null(input$advancedSettingsStart) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
    if (#REMOVE# is.na(input$advancedSettingsFixed) & 
      is.na(input$advancedSettingsStart))
      numericInput(inputId = "advancedSettingsStartMin", label = "start: Min", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMin"]][[input$advancedSettingsParameter]])
})
output$advancedSettingsStartMax <- renderUI({
  input$advancedSettingsButtonApplyDelta
  input$advancedSettingsButtonApplyAllDelta
  if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
    !is.null(input$advancedSettingsStart) & !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
    if (#REMOVE# is.na(input$advancedSettingsFixed) & 
      is.na(input$advancedSettingsStart))
      numericInput(inputId = "advancedSettingsStartMax", label = "start: Max", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMax"]][[input$advancedSettingsParameter]])
})
output$advancedSettingsLower <- renderUI({
  if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
    !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
    #REMOVE# if (is.na(input$advancedSettingsFixed))
    if (input$advancedSettingsMethod=="L-BFGS-B" | input$advancedSettingsMethod=="Brent")
      numericInput("advancedSettingsLower", label = "lower", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["lower"]][[input$advancedSettingsParameter]])
})
output$advancedSettingsUpper <- renderUI({
  if (#REMOVE# !is.null(input$advancedSettingsFixed) & 
    !is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsParameter))
    #REMOVE# if (is.na(input$advancedSettingsFixed))
    if (input$advancedSettingsMethod=="L-BFGS-B" | input$advancedSettingsMethod=="Brent")
      numericInput("advancedSettingsUpper", label = "upper", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["upper"]][[input$advancedSettingsParameter]])
})
#REMOVE# output$advancedSettingsJoint <- renderUI({
#REMOVE#   if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
#REMOVE#     selectInput("advancedSettingsJoint", label = "joint", choices = c(FALSE, TRUE), selected = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["joint"]])
#REMOVE# })
output$advancedSettingsMethod <- renderUI({
  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
    selectInput("advancedSettingsMethod", label = "method", choices = c("L-BFGS-B", "Nelder-Mead", "BFGS", "CG", "SANN", "Brent"), selected = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]])
})
#REMOVE# output$advancedSettingsAggregation <- renderUI({
#REMOVE#   if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
#REMOVE#     selectInput("advancedSettingsAggregation", label = "aggregation", choices = c(TRUE, FALSE), selected = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["aggregation"]])
#REMOVE# })
output$advancedSettingsThreshold <- renderUI({
  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries)) if(isolate({input$modelClass})=="Levy process")
    numericInput("advancedSettingsThreshold", label = "threshold", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["threshold"]])
})
output$advancedSettingsTrials <- renderUI({
  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries) & !is.null(input$advancedSettingsMethod))
    numericInput("advancedSettingsTrials", label = "trials", min = 1, value = ifelse(input$advancedSettingsMethod=="SANN" & yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]]!="SANN",1,yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["trials"]]))
})
output$advancedSettingsSeed <- renderUI({
  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
    numericInput("advancedSettingsSeed", label = "seed", min = 1, value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["seed"]])
})
output$advancedSettingsTimeout <- renderUI({
  if (!is.null(input$advancedSettingsModel) & !is.null(input$advancedSettingsSeries))
    numericInput("advancedSettingsTimeout", label = "timeout (s)", value = yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["timeout"]])
})


observeEvent(input$advancedSettingsButtonApplyDelta, {
  yuimaGUIsettings$delta[[input$advancedSettingsSeries]] <<- input$advancedSettingsDelta
  yuimaGUIsettings$toLog[[input$advancedSettingsSeries]] <<- input$advancedSettingsToLog
})
observeEvent(input$advancedSettingsButtonApplyAllDelta, {
  for (symb in rownames(seriesToEstimate$table)){
    yuimaGUIsettings$delta[[symb]] <<- input$advancedSettingsDelta
    if (input$advancedSettingsToLog==FALSE) yuimaGUIsettings$toLog[[symb]] <<- input$advancedSettingsToLog
    else if (all(getData(symb)>0)) yuimaGUIsettings$toLog[[symb]] <<- input$advancedSettingsToLog
  }
})
observeEvent(input$advancedSettingsButtonApplyModel,{
  #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["fixed"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsFixed
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["start"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStart
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMin"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMin
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["startMax"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMax
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["lower"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsLower
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["upper"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsUpper
})
observeEvent(input$advancedSettingsButtonApplyAllModel,{
  for (symb in rownames(seriesToEstimate$table)){
    #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["fixed"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsFixed
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["start"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStart
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["startMin"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMin
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["startMax"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsStartMax
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["lower"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsLower
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["upper"]][[input$advancedSettingsParameter]] <<- input$advancedSettingsUpper
  }
})
observeEvent(input$advancedSettingsButtonApplyGeneral,{
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["method"]] <<- input$advancedSettingsMethod
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["trials"]] <<- input$advancedSettingsTrials
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["seed"]] <<- input$advancedSettingsSeed
  #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["joint"]] <<- input$advancedSettingsJoint
  #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["aggregation"]] <<- input$advancedSettingsAggregation
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["threshold"]] <<- input$advancedSettingsThreshold
  yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[input$advancedSettingsSeries]][["timeout"]] <<- input$advancedSettingsTimeout
})
observeEvent(input$advancedSettingsButtonApplyAllModelGeneral,{
  for (symb in rownames(seriesToEstimate$table)){
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["method"]] <<- input$advancedSettingsMethod
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["trials"]] <<- input$advancedSettingsTrials
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["seed"]] <<- input$advancedSettingsSeed
    #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["joint"]] <<- input$advancedSettingsJoint
    #REMOVE# yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["aggregation"]] <<- input$advancedSettingsAggregation
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["threshold"]] <<- input$advancedSettingsThreshold
    yuimaGUIsettings$estimation[[input$advancedSettingsModel]][[symb]][["timeout"]] <<- input$advancedSettingsTimeout
  }
})
observeEvent(input$advancedSettingsButtonApplyAllGeneral,{
  for (mod in input$model){
    for (symb in rownames(seriesToEstimate$table)){
      yuimaGUIsettings$estimation[[mod]][[symb]][["method"]] <<- input$advancedSettingsMethod
      yuimaGUIsettings$estimation[[mod]][[symb]][["trials"]] <<- input$advancedSettingsTrials
      yuimaGUIsettings$estimation[[mod]][[symb]][["seed"]] <<- input$advancedSettingsSeed
      #REMOVE# yuimaGUIsettings$estimation[[mod]][[symb]][["joint"]] <<- input$advancedSettingsJoint
      #REMOVE# yuimaGUIsettings$estimation[[mod]][[symb]][["aggregation"]] <<- input$advancedSettingsAggregation
      yuimaGUIsettings$estimation[[mod]][[symb]][["threshold"]] <<- input$advancedSettingsThreshold
      yuimaGUIsettings$estimation[[mod]][[symb]][["timeout"]] <<- input$advancedSettingsTimeout
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
  else for(mod in input$model) if (class(try(setModelByName(mod, intensity = input$model_levy_intensity, jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps), AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA))))=="try-error")  valid <- FALSE
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
            timeout = ifelse(is.na(yuimaGUIsettings$estimation[[modName]][[symb]][["timeout"]]), Inf, yuimaGUIsettings$estimation[[modName]][[symb]][["timeout"]]),
            modName = modName,
            modClass = input$modelClass,
            intensity_levy = input$model_levy_intensity,
            AR_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$AR_C, NA), 
            MA_C = ifelse(input$modelClass %in% c("CARMA","COGARCH"), input$MA_C, NA),
            jumps = jumps_shortcut(class = input$modelClass, jumps = input$jumps),
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




