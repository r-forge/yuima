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

simulation_hist <- reactiveValues(distribution=list(), values=vector())
observe({
  if(!is.null(input$simulate_showSimulation_simID)){
    if(input$simulate_showSimulation_simID %in% rownames(yuimaGUItable$simulation)) {
      id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
      shinyjs::toggle("simulate_showSimulation_hist_div", condition = yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$info$nsim > 1)
      if(yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$info$nsim > 1){
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

output$simulate_showSimulation_hist_text <- renderUI({
  if(length(simulation_hist$values)!=0 & !is.null(input$simulate_showSimulation_hist_probability_slider)){
    val <- as.numeric(simulation_hist$values)
    qq <- quantile(val, probs = input$simulate_showSimulation_hist_probability_slider/100)
    HTML(paste("<div>", "Lower:", qq[1],"<br/>", "Upper: ", qq[2], "<br/>", "Mean: ", mean(val[val>=qq[1] & val<=qq[2]]), "</div>"))
  }
})

###Save Trajectory Button
output$simulate_showSimulation_button_saveTrajectory <- {
  dataDownload_traj <- reactive({
    id <- unlist(strsplit(input$simulate_showSimulation_simID, split = " "))
    x <- yuimaGUIdata$simulation[[id[1]]][[as.numeric(id[2])]]$trajectory
    d <- data.frame(x, row.names = index(x))
    colnames(d) <- paste(id[1],id[2],"_",seq(1, ncol(d)), sep = "")
    return(d)
  })
  downloadHandler(
    filename = function() {
      paste(input$simulate_showSimulation_simID, ".txt", sep="")
    },
    content = function(file) {
      write.table(dataDownload_traj(), file, row.names = TRUE, col.names = TRUE, quote = TRUE)
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


