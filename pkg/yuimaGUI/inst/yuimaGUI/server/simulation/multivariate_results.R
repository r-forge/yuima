###Create simulations table
output$multi_simulate_monitor_table <- DT::renderDataTable(options=list(scrollY = 200, scrollX=TRUE, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
  if (length(yuimaGUItable$multisimulation)==0){
    NoData <- data.frame("Symb"=NA,"Here will be stored simulations you run in the previous tabs"=NA, check.names = FALSE)
    return(NoData[-1,])
  }
  return (yuimaGUItable$multisimulation)
})

observe({
  shinyjs::toggle("multi_simulate_monitor_button_showSimulation", condition = (length(names(yuimaGUIdata$multisimulation))!=0))
})

###Delete Simulation
observeEvent(input$multi_simulate_monitor_button_delete, priority = 1, {
  if(!is.null(input$multi_simulate_monitor_table_rows_selected) & !is.null(input$multi_simulate_monitor_table_row_last_clicked)){
    if(input$multi_simulate_monitor_table_row_last_clicked %in% input$multi_simulate_monitor_table_rows_selected){
      rowname <- unlist(strsplit(rownames(yuimaGUItable$multisimulation)[input$multi_simulate_monitor_table_row_last_clicked], split = " " , fixed = FALSE))
      delSimulation(symb=rowname[1], n=rowname[2], multi = TRUE)
    }
  }
})

###DeleteAll Simulation
observeEvent(input$multi_simulate_monitor_button_deleteAll, priority = 1, {
  if(!is.null(input$multi_simulate_monitor_table_rows_all)){
    rowname <- unlist(strsplit(rownames(yuimaGUItable$multisimulation)[input$multi_simulate_monitor_table_rows_all], split = " " , fixed = FALSE))
    delSimulation(symb=rowname[seq(1,length(rowname),2)], n=rowname[seq(2,length(rowname),2)], multi = TRUE)
  }
})

output$multi_simulate_showSimulation_simID <- renderUI({
  selectInput(inputId = "multi_simulate_showSimulation_simID", label = "Simulation ID", choices = rownames(yuimaGUItable$multisimulation))
})

multi_observationTime <- reactiveValues(x = numeric())
observeEvent(input$multi_simulate_showSimulation_simID,{
  id <- unlist(strsplit(input$multi_simulate_showSimulation_simID, split = " "))
  if(!is.na(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory[[1]]))
    multi_observationTime$x <<- as.numeric(end(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory))
})
observe({
  if (!is.null(input$multi_simulate_showSimulation_plot_click$x) & !is.null(input$multi_simulate_showSimulation_simID)){
    id <- unlist(strsplit(input$multi_simulate_showSimulation_simID, split = " "))
    if(!is.na(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory[[1]]))
      multi_observationTime$x <<- input$multi_simulate_showSimulation_plot_click$x
  }
})


params_multi_simulate_showSimulation_plot <- reactiveValues(id =  NULL, y = NULL, z = NULL)
output$multi_simulate_showSimulation_plot_series1 <- renderUI({
  if(!is.null(input$multi_simulate_showSimulation_simID))
    if(input$multi_simulate_showSimulation_simID %in% rownames(yuimaGUItable$multisimulation)){
      id <- unlist(strsplit(input$multi_simulate_showSimulation_simID, split = " "))
      shinyjs::toggle("multi_simulate_showSimulation_plot_div", condition = !is.na(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory[1]))
      choices <- yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$model$info$symb
      params_multi_simulate_showSimulation_plot$id <<- input$multi_simulate_showSimulation_simID
      params_multi_simulate_showSimulation_plot$y <<- choices[1]
      selectInput("multi_simulate_showSimulation_plot_series1", label="y-Axis", choices = choices, selected = choices[1])
    }
})
output$multi_simulate_showSimulation_plot_series2 <- renderUI({
  if(!is.null(input$multi_simulate_showSimulation_simID))
    if(input$multi_simulate_showSimulation_simID %in% rownames(yuimaGUItable$multisimulation)){
      id <- unlist(strsplit(input$multi_simulate_showSimulation_simID, split = " "))
      choices <- yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$model$info$symb
      params_multi_simulate_showSimulation_plot$z <<- last(choices)
      selectInput("multi_simulate_showSimulation_plot_series2", label="z-Axis", choices = choices, selected = last(choices))
    }
})


observe({
  params_multi_simulate_showSimulation_plot$y <<- input$multi_simulate_showSimulation_plot_series1
  params_multi_simulate_showSimulation_plot$z <<- input$multi_simulate_showSimulation_plot_series2
})


observeEvent(params_multi_simulate_showSimulation_plot$id,{
  if(!is.null(params_multi_simulate_showSimulation_plot$id) & !is.null(params_multi_simulate_showSimulation_plot$y) & !is.null(params_multi_simulate_showSimulation_plot$z) ){
    id <- unlist(strsplit(params_multi_simulate_showSimulation_plot$id, split = " "))
    if(!is.na(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory[[1]])){ 
      filtered.colnames <- gsub(colnames(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory), pattern = "_sim.*", replacement = "")
      seriesnames <- unique(filtered.colnames)
      d <- data.frame(sapply(seriesnames, function(x) {matrix(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory[, filtered.colnames==x])}))
      x <- index(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory)
      n.x <- length(x)
      n.row.d <- nrow(d)
      d$x6428347364932 <- rep(x, n.row.d/n.x) #avoid index name matching a real seriesname
      d$ID432793740239 <- rep(seq(1:(n.row.d/n.x)), each = n.x) #avoid id name matching a real seriesname
      
      output$multi_simulate_showSimulation_plot <- renderPlotly({
          y <- params_multi_simulate_showSimulation_plot$y
          z <- params_multi_simulate_showSimulation_plot$z
          if(y!=z)
            plot_ly(d, x=~x6428347364932, y=d[,y], z=d[,z], split = ~ID432793740239, type="scatter3d", mode="lines", source = "multi_simulate_showSimulation_plot") %>%
              layout(
                showlegend = FALSE,
                title = paste("ID:", isolate({params_multi_simulate_showSimulation_plot$id})),
                scene = list (
                  xaxis = list(
                    title = ""
                  ),
                  yaxis = list(
                    title = y
                  ),
                  zaxis = list(
                    title = z
                  )
                )
              )
          else
            plot_ly(d, x=~x6428347364932, y=d[,y], split = ~ID432793740239, type="scatter", mode="lines", source = "multi_simulate_showSimulation_plot") %>%
            layout(
              showlegend = FALSE,
              title = paste("ID:", isolate({params_multi_simulate_showSimulation_plot$id})),
              xaxis = list(
                title = ""
              ),
              yaxis = list(
                title = y
              )
            )
            
      })
    }    
  }
})





multi_simulation_hist <- reactiveValues(values=NULL, x=NULL, y=NULL, z=NULL)
observe({
  if(!is.null(params_multi_simulate_showSimulation_plot$id) & !is.null(params_multi_simulate_showSimulation_plot$y) & !is.null(params_multi_simulate_showSimulation_plot$z) ){
    id <- unlist(strsplit(params_multi_simulate_showSimulation_plot$id, split = " "))
    if(id[1] %in% names(yuimaGUIdata$multisimulation)) if(length(yuimaGUIdata$multisimulation[[id[1]]])>=as.numeric(id[2])) if(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$info$nsim > 1){
      if(is.na(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$hist[1])){
        shinyjs::toggle("multi_simulate_showSimulation_hist_div", condition = yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$info$nsim > 1)
        
        filtered.colnames <- gsub(colnames(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory), pattern = "_sim.*", replacement = "")
        seriesnames <- unique(filtered.colnames)
        x <- index(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory)
       
        click <- event_data(event = "plotly_click", source = "multi_simulate_showSimulation_plot")
        if (!is.null(click)) extract.x <- as.character(click$x[1])
        else  extract.x <- as.character(last(x))
        extract.index <- which(as.character(index(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory))==extract.x)
        d <- data.frame(sapply(seriesnames, function(x) {matrix(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory[extract.index, filtered.colnames==x])}))
        
        multi_simulation_hist$x <<- extract.x
        multi_simulation_hist$values <<- d
      }
      else {
        multi_simulation_hist$x <<- yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$info$simulate.to
        multi_simulation_hist$values <<- t(yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$hist)
      }
      multi_simulation_hist$y <<- params_multi_simulate_showSimulation_plot$y
      multi_simulation_hist$z <<- params_multi_simulate_showSimulation_plot$z
    }
  }
})




observeEvent(multi_simulation_hist$values, {
  output$multi_simulate_showSimulation_hist <- renderPlotly({
    if(multi_simulation_hist$y!=multi_simulation_hist$z){
      plot_ly(x=multi_simulation_hist$values[,multi_simulation_hist$y], y=multi_simulation_hist$values[,multi_simulation_hist$z], type = "histogram2d") %>%
        layout(
          title = paste("Distribution at index =", multi_simulation_hist$x), 
          xaxis = list(
            title = multi_simulation_hist$y
          ),
          yaxis = list(
            title = multi_simulation_hist$z
          )
        )
    }
    else
      plot_ly(x=multi_simulation_hist$values[,multi_simulation_hist$y], type = "histogram") %>%
        layout(
          title = paste("Distribution at index =", multi_simulation_hist$x), 
          xaxis = list(
            title = multi_simulation_hist$y
          ),
          yaxis = list(
            title = "Frequency"
          )
        )
  })
}, once = TRUE)



output$multi_simulate_showSimulation_hist_text <- renderUI({
  if(length(multi_simulation_hist$values)!=0 & !is.null(input$multi_simulate_showSimulation_hist_probability_slider)){
    val1 <- as.numeric(multi_simulation_hist$values[,multi_simulation_hist$y])
    qq1 <- quantile(val1, probs = input$multi_simulate_showSimulation_hist_probability_slider/100)
    val2 <- as.numeric(multi_simulation_hist$values[,multi_simulation_hist$z])
    qq2 <- quantile(val2, probs = input$multi_simulate_showSimulation_hist_probability_slider/100)
    HTML(paste("<div>", multi_simulation_hist$y , "<br/>", "Lower:", qq1[1],"<br/>", "Upper: ", qq1[2], "<br/>", "Mean: ", mean(val1[val1>=qq1[1] & val1<=qq1[2]]), "</div>",
               "<br/>",
               "<div>", multi_simulation_hist$z , "<br/>", "Lower:", qq2[1],"<br/>", "Upper: ", qq2[2], "<br/>", "Mean: ", mean(val2[val2>=qq2[1] & val2<=qq2[2]]), "</div>"))
  }
})

###Save Trajectory Button
output$multi_simulate_showSimulation_button_saveTrajectory <- {
  dataDownload_multi_traj <- reactive({
    id <- unlist(strsplit(input$multi_simulate_showSimulation_simID, split = " "))
    x <- yuimaGUIdata$multisimulation[[id[1]]][[as.numeric(id[2])]]$trajectory
    d <- data.frame(x, row.names = index(x))
    return(d)
  })
  downloadHandler(
    filename = function() {
      paste(input$multi_simulate_showSimulation_simID, ".txt", sep="")
    },
    content = function(file) {
      write.table(dataDownload_multi_traj(), file, row.names = TRUE, col.names = TRUE, quote = TRUE)
    }
  )
}

###Save Histogram Button
output$multi_simulate_showSimulation_button_saveHist <- {
  dataDownload_multi_hist <- reactive({
    multi_simulation_hist$values
  })
  downloadHandler(
    filename = function() {
      paste(input$multi_simulate_showSimulation_simID, "_hist",".txt", sep="")
    },
    content = function(file) {
      write.table(dataDownload_multi_hist(), file, row.names = FALSE, col.names = TRUE)
    }
  )
}


