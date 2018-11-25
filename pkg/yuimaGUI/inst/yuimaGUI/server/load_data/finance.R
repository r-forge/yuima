###Download data and display message
observeEvent(input$finDataGo, priority = 1, {
  if (input$symb!=""){
    closeAlert(session, "finDataAlert_err")
    closeAlert(session, "finDataAlert_warn")
    closeAlert(session, "finDataAlert_succ")
    symb <- unlist(strsplit(input$symb, split = "[, ]+" , fixed = FALSE))
    err <- c()
    already_in <- c()
    withProgress(message = 'Loading: ', value = 0, {
      for (i in symb){
        incProgress(1/length(symb), detail = i)
        x <- try(window(getSymbols(i, src = input$sources ,auto.assign = FALSE), start = input$dR[1], end = input$dR[2]))
        if (class(x)[1]=="try-error")
          err <- cbind(err,i)
        else {
          info <- addData(x, typeIndex = "%Y-%m-%d")
          err <- c(err, info$err)
          already_in <- c(already_in, info$already_in)
        }
      }
    })
    if(!is.null(err))
      createAlert(session = session, anchorId = "finDataAlert", alertId = "finDataAlert_err", content = paste("Unable to load following symbols:", paste(err,collapse = " ")), style = "error")
    if(!is.null(already_in))
      createAlert(session = session, anchorId = "finDataAlert", alertId = "finDataAlert_warn", content = paste("WARNING! Following symbols already loaded:", paste(already_in,collapse = " ")), style = "warning")
    if(is.null(err) & is.null(already_in))
      createAlert(session = session, anchorId = "finDataAlert", alertId = "finDataAlert_succ", content = paste("All symbols loaded successfully"), style = "success")
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
