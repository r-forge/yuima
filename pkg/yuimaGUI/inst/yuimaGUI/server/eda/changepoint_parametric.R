###Display available data
output$parametric_changepoint_table_select <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
  if (length(yuimaGUItable$series)==0){
    NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
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
    NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
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
  for(i in names(yuimaGUIdata$usr_model))
    if (yuimaGUIdata$usr_model[[i]]$class=="Diffusion process") choices <- c(i, choices)
  selectInput("parametric_changepoint_model", label = "Model", choices = choices, multiple = FALSE, selected = sel)
})


###Interactive range of selectRange chart
parametric_range_selectRange <- reactiveValues(x=NULL, y=NULL)
observe({
  if (!is.null(input$parametric_selectRange_brush) & !is.null(input$parametric_plotsRangeSeries)){
    data <- getData(input$parametric_plotsRangeSeries)
    test <- (length(index(window(data, start = input$parametric_selectRange_brush$xmin, end = input$parametric_selectRange_brush$xmax))) > 3)
    if (test==TRUE){
      parametric_range_selectRange$x <- c(as.Date(input$parametric_selectRange_brush$xmin), as.Date(input$parametric_selectRange_brush$xmax))
      parametric_range_selectRange$y <- c(input$parametric_selectRange_brush$ymin, input$parametric_selectRange_brush$ymax)
    }
  }
})


observe({
  shinyjs::toggle(id="parametric_plotsRangeErrorMessage", condition = nrow(parametric_seriesToChangePoint$table)==0)
  shinyjs::toggle(id="parametric_plotsRangeAll", condition = nrow(parametric_seriesToChangePoint$table)!=0)
})

###Display charts: series and its increments
observe({
  symb <- input$parametric_plotsRangeSeries
  if(!is.null(symb))
    if (symb %in% rownames(yuimaGUItable$series)){
      data <- getData(symb)
      incr <- na.omit(Delt(data, type = "arithmetic"))
      condition <- all(is.finite(incr))
      shinyjs::toggle("parametric_selectRangeReturns", condition = condition)
      parametric_range_selectRange$x <- NULL
      parametric_range_selectRange$y <- NULL
      start <- as.character(parametric_seriesToChangePoint$table[input$parametric_plotsRangeSeries,"From"])
      end <- as.character(parametric_seriesToChangePoint$table[input$parametric_plotsRangeSeries,"To"])
      if(class(index(data))=="numeric"){
        start <- as.numeric(start)
        end <- as.numeric(end)
      }
      output$parametric_selectRange <- renderPlot({
        if ((symb %in% rownames(yuimaGUItable$series) & (symb %in% rownames(parametric_seriesToChangePoint$table)))){
          par(bg="black")
          plot.zoo(window(data, start = parametric_range_selectRange$x[1], end = parametric_range_selectRange$x[2]), main=symb, xlab="Index", ylab=NA, log=switch(input$parametric_scale_selectRange,"Linear"="","Logarithmic (Y)"="y", "Logarithmic (X)"="x", "Logarithmic (XY)"="xy"), col="grey", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
          lines(window(data, start = start, end = end), col = "green")
          grid(col="grey")
        }
      })
      output$parametric_selectRangeReturns <- renderPlot({
        if (symb %in% rownames(yuimaGUItable$series) & (symb %in% rownames(parametric_seriesToChangePoint$table)) & condition){
          par(bg="black")
          plot.zoo( window(incr, start = parametric_range_selectRange$x[1], end = parametric_range_selectRange$x[2]), main=paste(symb, " - Percentage Increments"), xlab="Index", ylab=NA, log=switch(input$parametric_scale_selectRange,"Linear"="","Logarithmic (Y)"="", "Logarithmic (X)"="x", "Logarithmic (XY)"="x"), col="grey", col.axis="grey", col.lab="grey", col.main="grey", fg="black")
          lines(window(incr, start = start,  end = end), col = "green")
          grid(col="grey")
        }
      })
    }
})


output$parametric_plotsRangeSeries <- renderUI({
  selectInput("parametric_plotsRangeSeries", label = "Series", choices = rownames(parametric_seriesToChangePoint$table), selected = input$parametric_plotsRangeSeries)
})

###Choose Range input set to "Select range from charts" if charts have been brushed
output$parametric_chooseRange <- renderUI({
  sel <- "full"
  if (!is.null(parametric_range_selectRange$x)) sel <- "selected"
  selectInput("parametric_chooseRange", label = "Range", choices = c("Full Range" = "full", "Select Range from Charts" = "selected", "Specify Range" = "specify"), selected = sel)
})

output$parametric_chooseRange_specify <- renderUI({
  if(!is.null(input$parametric_plotsRangeSeries)) {
    data <- getData(input$parametric_plotsRangeSeries)
    if(class(index(data))=="numeric") 
      return(div(
        column(6,numericInput("parametric_chooseRange_specify_t0", label = "From", min = start(data), max = end(data), value = start(data))),
        column(6,numericInput("parametric_chooseRange_specify_t1", label = "To", min = start(data), max = end(data), value = end(data)))
      ))
    if(class(index(data))=="Date")
      return(dateRangeInput("parametric_chooseRange_specify_date", start = start(data), end = end(data), label = "Specify Range"))
  }
})


observe({
  shinyjs::toggle(id = "parametric_chooseRange_specify", condition = (input$parametric_chooseRange)=="specify")
})

###Function to update data range to use to estimate models
updateRange_parametric_seriesToChangePoint <- function(symb, range = c("full","selected","specify"), type = c("Date", "numeric")){
  for (i in symb){
    data <- getData(i)
    if (range == "full"){
      levels(parametric_seriesToChangePoint$table[,"From"]) <- c(levels(parametric_seriesToChangePoint$table[,"From"]), as.character(start(data)))
      levels(parametric_seriesToChangePoint$table[,"To"]) <- c(levels(parametric_seriesToChangePoint$table[,"To"]), as.character(end(data)))
      parametric_seriesToChangePoint$table[i,"From"] <<- as.character(start(data))
      parametric_seriesToChangePoint$table[i,"To"] <<- as.character(end(data))
    }
    if (range == "selected"){
      if(!is.null(parametric_range_selectRange$x) & class(index(data))==type){
        start <- parametric_range_selectRange$x[1]
        end <- parametric_range_selectRange$x[2]
        if(class(index(data))=="numeric"){
          start <- as.numeric(start)
          end <- as.numeric(end)
        }
        start <- max(start(data),start)
        end <- min(end(data), end)
        levels(parametric_seriesToChangePoint$table[,"From"]) <- c(levels(parametric_seriesToChangePoint$table[,"From"]), as.character(start))
        levels(parametric_seriesToChangePoint$table[,"To"]) <- c(levels(parametric_seriesToChangePoint$table[,"To"]), as.character(end))
        parametric_seriesToChangePoint$table[i,"From"] <<- as.character(start)
        parametric_seriesToChangePoint$table[i,"To"] <<- as.character(end)
      }
    }
    if (range == "specify"){
      if(class(index(data))==type){
        if(class(index(data))=="Date"){
          start <- input$parametric_chooseRange_specify_date[1]
          end <- input$parametric_chooseRange_specify_date[2]
        }
        if(class(index(data))=="numeric"){
          start <- input$parametric_chooseRange_specify_t0
          end <- input$parametric_chooseRange_specify_t1
        }
        start <- max(start(data),start)
        end <- min(end(data), end)
        levels(parametric_seriesToChangePoint$table[,"From"]) <- c(levels(parametric_seriesToChangePoint$table[,"From"]), as.character(start))
        levels(parametric_seriesToChangePoint$table[,"To"]) <- c(levels(parametric_seriesToChangePoint$table[,"To"]), as.character(end))
        parametric_seriesToChangePoint$table[i,"From"] <<- as.character(start)
        parametric_seriesToChangePoint$table[i,"To"] <<- as.character(end)
      }
    }
  }
}

###Apply selected range by double click
observeEvent(input$parametric_selectRange_dbclick, priority = 1, {
  updateRange_parametric_seriesToChangePoint(input$parametric_plotsRangeSeries, range = "selected", type = class(index(getData(input$parametric_plotsRangeSeries))))
})

###Apply selected range
observeEvent(input$parametric_buttonApplyRange, priority = 1, {
  updateRange_parametric_seriesToChangePoint(input$parametric_plotsRangeSeries, range = input$parametric_chooseRange, type = class(index(getData(input$parametric_plotsRangeSeries))))
})

###ApplyAll selected range
observeEvent(input$parametric_buttonApplyAllRange, priority = 1, {
  updateRange_parametric_seriesToChangePoint(rownames(parametric_seriesToChangePoint$table), range = input$parametric_chooseRange, type = class(index(getData(input$parametric_plotsRangeSeries))))
})


### Estimation Settings
parametric_modal_prev_buttonDelta <- 0
parametric_modal_prev_buttonAllDelta <- 0
observe({
  for (symb in rownames(parametric_seriesToChangePoint$table)){
    if (is.null(yuimaGUIsettings$delta[[symb]])) yuimaGUIsettings$delta[[symb]] <<- 0.01
    if (is.null(yuimaGUIsettings$toLog[[symb]])) yuimaGUIsettings$toLog[[symb]] <<- FALSE
    data <- na.omit(as.numeric(getData(symb)))
    if (yuimaGUIsettings$toLog[[symb]]==TRUE) data <- log(data)
    for (modName in input$parametric_changepoint_model){
      if (class(try(setModelByName(modName, jumps = NA, AR_C = NA, MA_C = NA)))!="try-error"){
        if (is.null(yuimaGUIsettings$estimation[[modName]]))
          yuimaGUIsettings$estimation[[modName]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]]))
          yuimaGUIsettings$estimation[[modName]][[symb]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["fixed"]] <<- list()
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["start"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["start"]] <<- list()
        
        startMinMax <- defaultBounds(name = modName, 
                                     jumps = NA, 
                                     AR_C = NA, 
                                     MA_C = NA, 
                                     strict = FALSE,
                                     data = data,
                                     delta = yuimaGUIsettings$delta[[symb]])
        upperLower <- defaultBounds(name = modName, 
                                    jumps = NA, 
                                    AR_C = NA, 
                                    MA_C = NA, 
                                    strict = TRUE,
                                    data = data,
                                    delta = yuimaGUIsettings$delta[[symb]])
        
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["startMin"]] <<- startMinMax$lower
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["startMax"]] <<- startMinMax$upper
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["upper"]] <<- upperLower$upper
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]]) | parametric_modal_prev_buttonDelta!=input$parametric_modal_button_applyDelta | parametric_modal_prev_buttonAllDelta!=input$parametric_modal_button_applyAllDelta)
          yuimaGUIsettings$estimation[[modName]][[symb]][["lower"]] <<- upperLower$lower
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["method"]])){
          yuimaGUIsettings$estimation[[modName]][[symb]][["method"]] <<- "L-BFGS-B"
        }
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["trials"]] <<- 1
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["seed"]] <<- NA
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["joint"]] <<- FALSE
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["aggregation"]] <<- TRUE
        if (is.null(yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]]))
          yuimaGUIsettings$estimation[[modName]][[symb]][["threshold"]] <<- NA
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
    return (numericInput("parametric_modal_delta", label = paste("delta", input$parametric_modal_series), value = yuimaGUIsettings$delta[[input$parametric_modal_series]]))
})
output$parametric_modal_toLog <- renderUI({
  if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series)){
    choices <- FALSE
    if (all(getData(input$parametric_modal_series)>0)) choices <- c(FALSE, TRUE)
    return (selectInput("parametric_modal_toLog", label = "Convert to log", choices = choices, selected = yuimaGUIsettings$toLog[[input$parametric_modal_series]]))
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
    numericInput(inputId = "parametric_modal_start", label = "start", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["start"]][[input$parametric_modal_parameter]])
})
output$parametric_modal_startMin <- renderUI({
  input$parametric_modal_button_applyDelta
  input$parametric_modal_button_applyAllDelta
  if (!is.null(input$parametric_modal_start) & !is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
    if (is.na(input$parametric_modal_start))
      numericInput(inputId = "parametric_modal_startMin", label = "start: Min", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMin"]][[input$parametric_modal_parameter]])
})
output$parametric_modal_startMax <- renderUI({
  input$parametric_modal_button_applyDelta
  input$parametric_modal_button_applyAllDelta
  if (!is.null(input$parametric_modal_start) & !is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
    if (is.na(input$parametric_modal_start))
      numericInput(inputId = "parametric_modal_startMax", label = "start: Max", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMax"]][[input$parametric_modal_parameter]])
})
output$parametric_modal_lower <- renderUI({
  if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
    if (input$parametric_modal_method=="L-BFGS-B" | input$parametric_modal_method=="Brent")
      numericInput("parametric_modal_lower", label = "lower", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["lower"]][[input$parametric_modal_parameter]])
})
output$parametric_modal_upper <- renderUI({
  if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_parameter))
    if (input$parametric_modal_method=="L-BFGS-B" | input$parametric_modal_method=="Brent")
      numericInput("parametric_modal_upper", label = "upper", value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["upper"]][[input$parametric_modal_parameter]])
})
output$parametric_modal_method <- renderUI({
  if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series))
    selectInput("parametric_modal_method", label = "method", choices = c("L-BFGS-B", "Nelder-Mead", "BFGS", "CG", "SANN", "Brent"), selected = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["method"]])
})
output$parametric_modal_trials <- renderUI({
  if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series) & !is.null(input$parametric_modal_method))
    numericInput("parametric_modal_trials", label = "trials", min = 1, value = ifelse(input$parametric_modal_method=="SANN" & yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["method"]]!="SANN",1,yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["trials"]]))
})
output$parametric_modal_seed <- renderUI({
  if (!is.null(input$parametric_modal_model) & !is.null(input$parametric_modal_series))
    numericInput("parametric_modal_seed", label = "seed", min = 1, value = yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["seed"]])
})



observeEvent(input$parametric_modal_button_applyDelta, {
  yuimaGUIsettings$delta[[input$parametric_modal_series]] <<- input$parametric_modal_delta
  yuimaGUIsettings$toLog[[input$parametric_modal_series]] <<- input$parametric_modal_toLog
})
observeEvent(input$parametric_modal_button_applyAllDelta, {
  for (symb in rownames(parametric_seriesToChangePoint$table)){
    yuimaGUIsettings$delta[[symb]] <<- input$parametric_modal_delta
    if (input$parametric_modal_toLog==FALSE) yuimaGUIsettings$toLog[[symb]] <<- input$parametric_modal_toLog
    else if (all(getData(symb)>0)) yuimaGUIsettings$toLog[[symb]] <<- input$parametric_modal_toLog
  }
})
observeEvent(input$parametric_modal_button_applyModel,{
  yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["start"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_start
  yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMin"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMin
  yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["startMax"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMax
  yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["lower"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_lower
  yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["upper"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_upper
})
observeEvent(input$parametric_modal_button_applyAllModel,{
  for (symb in rownames(parametric_seriesToChangePoint$table)){
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["start"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_start
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["startMin"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMin
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["startMax"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_startMax
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["lower"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_lower
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["upper"]][[input$parametric_modal_parameter]] <<- input$parametric_modal_upper
  }
})
observeEvent(input$parametric_modal_button_applyGeneral,{
  yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["method"]] <<- input$parametric_modal_method
  yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["trials"]] <<- input$parametric_modal_trials
  yuimaGUIsettings$estimation[[input$parametric_modal_model]][[input$parametric_modal_series]][["seed"]] <<- input$parametric_modal_seed
})
observeEvent(input$parametric_modal_button_applyAllModelGeneral,{
  for (symb in rownames(parametric_seriesToChangePoint$table)){
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["method"]] <<- input$parametric_modal_method
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["trials"]] <<- input$parametric_modal_trials
    yuimaGUIsettings$estimation[[input$parametric_modal_model]][[symb]][["seed"]] <<- input$parametric_modal_seed
  }
})

output$parametric_changepoint_symb <- renderUI({
  n <- names(yuimaGUIdata$cpYuima)
  if(length(n)!=0)
    selectInput("parametric_changepoint_symb", "Symbol", choices = sort(n), selected = last(n))  
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
                            delta = yuimaGUIsettings$delta[[symb]],
                            toLog = yuimaGUIsettings$toLog[[symb]],
                            start = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["start"]],
                            startMin = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["startMin"]],
                            startMax = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["startMax"]],
                            method = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["method"]],
                            trials = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["trials"]],
                            seed = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["seed"]],
                            lower = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["lower"]],
                            upper = yuimaGUIsettings$estimation[[input$parametric_changepoint_model]][[symb]][["upper"]]))
      if(class(test)=="try-error") 
        errors <- c(errors, symb)
    }
    if (!is.null(errors))
      createAlert(session = session, anchorId = "parametric_changepoint_alert", alertId = "parametric_changepoint_alert_err", style = "error", dismiss = TRUE, content = paste("Unable to estimate Change Point of:", paste(errors, collapse = " ")))
  })
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
      align="center"
    )
  }
})

output$parametric_changepoint_modal_info_text <- renderUI({
  info <- yuimaGUIdata$cpYuima[[input$parametric_changepoint_symb]]$info
  div(
    h3(input$parametric_changepoint_symb, " - " , info$model, class = "hModal"),
    h4(
      em("series to log:"), info$toLog, br(),
      em("method:"), info$method, br(),
      em("trials:"), info$trials, br(),
      em("seed:"), info$seed, br(), class = "hModal"
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
