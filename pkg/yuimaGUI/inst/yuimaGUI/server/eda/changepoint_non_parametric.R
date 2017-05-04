###Display available data
output$changepoint_table_select <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
  if (length(yuimaGUItable$series)==0){
    NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
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
    NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
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
  n <- names(yuimaGUIdata$cp)
  if(length(n)!=0)
    selectInput("changepoint_symb", "Symbol", choices = sort(n), selected = last(n))  
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
      h3(input$changepoint_symb, class = "hModal"),
      h4(
        em(switch(info$method, "KSdiff"="Increments Distriution", "KSperc"="Percentage Increments Distriution")), br(),
        class = "hModal"
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

