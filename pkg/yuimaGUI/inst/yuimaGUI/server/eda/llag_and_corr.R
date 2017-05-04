###Display available data
output$llag_table_select <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
  if (length(yuimaGUItable$series)==0){
    NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
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
    NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
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

observe({
  shinyjs::toggle("llag_maxLag", condition = input$llag_type=="llag")
  shinyjs::toggle("llag_corr_method", condition = input$llag_type=="corr")
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
        if(type=="Date") {
          start <- input$llag_range_date[1]
          end <- input$llag_range_date[2]
        } else {
          start <- input$llag_range_numeric1
          end <- input$llag_range_numeric2
        }
        data <- window(data, start = start, end = end)
        if(is.regular(data)){
          delta <- mode(na.omit(diff(index(data))))
          yuimaData <- setDataGUI(data, delta = delta)
          if(input$llag_type=="llag"){
            res <- try(llag(yuimaData, ci=TRUE, plot=FALSE, grid = seq(from = -input$llag_maxLag, to = input$llag_maxLag, by = delta)))
            if (class(res)=="try-error")
              createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Error in computing lead-lag", style = "error")
            else {
              i <- 1
              id <- "Lead-Lag Analysis"
              repeat {
                if(id %in% names(yuimaGUIdata$llag)){
                  id <- paste("Lead-Lag Analysis", i)
                  i <- i+1
                } else break
              }
              yuimaGUIdata$llag[[id]] <<- list(type = "llag", maxLag = input$llag_maxLag, delta = delta, llag = res$lagcce, p.values = res$p.values, start = start, end = end)
            }
          }
          if(input$llag_type=="corr"){
            if(input$llag_corr_method %in% c("pearson", "kendall", "spearman")){
              x <- as.matrix(yuimaData@original.data)
              res <- try(cor(x, method = input$llag_corr_method, use = "pairwise.complete.obs"))
            } 
            else 
              res <- try(cce(x = yuimaData, method = input$llag_corr_method)$cormat)
            if (class(res)=="try-error")
              createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Error in computing the correlation matrix", style = "error")
            else {
              i <- 1
              id <- "Correlation Analysis"
              repeat {
                if(id %in% names(yuimaGUIdata$llag)){
                  id <- paste("Correlation Analysis", i)
                  i <- i+1
                } else break
              }
              yuimaGUIdata$llag[[id]] <<- list(type = "corr", cormat = res, method = input$llag_corr_method, start = start, end = end)
            }
          }
        } else{
          createAlert(session, anchorId = "llag_alert", alertId = "llag_alert_select", content = "Cannot analyze non-regular grid of observations", style = "error")
        }
      })
    }
  }
})

observe({
  shinyjs::toggle("llag_plot_body", condition = length(names(yuimaGUIdata$llag))!=0)
})

output$llag_analysis_id <- renderUI({
  n <- names(yuimaGUIdata$llag)
  if(length(n)!=0)
    selectInput("llag_analysis_id", label = "Analysis ID", choices = sort(n), selected = last(n))
})

output$llag_plot_corr_method <- renderUI({
  if(!is.null(input$llag_analysis_id)) if (input$llag_analysis_id %in% names(isolate({yuimaGUIdata$llag}))){
    info <- isolate({yuimaGUIdata$llag})[[input$llag_analysis_id]]
    if (info$type=="corr"){
      method <- switch(info$method,
                       "HY"="Hayashi-Yoshida", 
                       "PHY"="Pre-averaged Hayashi-Yoshida", 
                       "MRC"="Modulated Realized Covariance", 
                       "TSCV"="Two Scales realized CoVariance", 
                       "GME"="Generalized Multiscale Estimator", 
                       "RK"="Realized Kernel", 
                       "QMLE"="Quasi Maximum Likelihood Estimator", 
                       "SIML"="Separating Information Maximum Likelihood", 
                       "THY"="Truncated Hayashi-Yoshida", 
                       "PTHY"="Pre-averaged Truncated Hayashi-Yoshida", 
                       "SRC"="Subsampled Realized Covariance", 
                       "SBPC"="Subsampled realized BiPower Covariation")
      return(HTML(paste("<div><h4>&nbsp &nbsp Method:", method, "</h4></div>")))
    }
  }
})

observe({
  if(!is.null(input$llag_analysis_id)) if (input$llag_analysis_id %in% isolate({names(yuimaGUIdata$llag)})) {
    type <- isolate({yuimaGUIdata$llag})[[input$llag_analysis_id]]$type
    shinyjs::toggle("llag_plot_confidence", condition = type=="llag")
    shinyjs::toggle("llag_plot_corr_method", condition = type=="corr")   
    shinyjs::toggle("llag_plot_howToRead", condition = type=="llag")
  }
})

output$llag_plot <- renderPlot({
  if(!is.null(input$llag_analysis_id) & !is.null(input$llag_plot_confidence)) if (input$llag_analysis_id %in% isolate({names(yuimaGUIdata$llag)})) {
    info <- isolate({yuimaGUIdata$llag[[input$llag_analysis_id]]})
    if(info$type=="llag"){
      co <- ifelse(info$p.values > input$llag_plot_confidence | is.na(info$p.values), 0, info$llag)
      co<-melt(t(co))
      digits <- 1+as.integer(abs(log10(info$delta)))
    }
    if(info$type=="corr"){
      co <- info$cormat
      co<-melt(t(co))
      digits <- 2
    }
    fillColor <- switch(getOption("yuimaGUItheme"), "black"="#282828", "white"="#f0f4f5")
    textColor <- switch(getOption("yuimaGUItheme"), "black"="#CDCECD", "white"="black")
    ggplot(co, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
      geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
      geom_text(aes(label = round(co$value, digits))) + # write the values
      scale_fill_gradient2(low = "#ffa500", 
                           mid = switch(getOption("yuimaGUItheme"), "black"="gray30", "white"="#C7E2DF"), 
                           high = "#74d600", 
                           midpoint = 0) + # determine the colour
      theme(panel.grid.major.x=element_blank(), #no gridlines
            panel.grid.minor.x=element_blank(), 
            panel.grid.major.y=element_blank(), 
            panel.grid.minor.y=element_blank(),
            panel.background=element_rect(fill=fillColor), # background=white
            plot.background = element_rect(fill = fillColor, linetype = 0, color = fillColor),
            axis.text.x = element_text(angle=90,hjust = 1, size = 12,face = "bold", colour = textColor),
            plot.title = element_text(size=20,face="bold", colour = textColor, hjust = 0.5),
            axis.text.y = element_text(size = 12,face = "bold",  colour = textColor)) + 
      ggtitle(paste("Analyzed data from", info$start, "to", info$end)) + 
      theme(legend.title=element_text(face="bold", size=14)) + 
      scale_x_discrete(name="") +
      scale_y_discrete(name="") +
      labs(fill="")
  }
})

observeEvent(input$llag_delete_analysis, {
  yuimaGUIdata$llag[[input$llag_analysis_id]] <<- NULL
})

observeEvent(input$llag_deleteAll_analysis, {
  yuimaGUIdata$llag <<- list()
})
