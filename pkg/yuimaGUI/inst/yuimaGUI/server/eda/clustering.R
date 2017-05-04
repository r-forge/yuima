###Display available data
output$cluster_table_select <- DT::renderDataTable(options=list(scrollY = 150, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
  if (length(yuimaGUItable$series)==0){
    NoData <- data.frame("Symb"=NA,"Please load some data first"=NA, check.names = FALSE)
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
    NoData <- data.frame("Symb"=NA,"Select from table beside"=NA, check.names = FALSE)
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
      "MOdist" = try(sde::MOdist(na.omit(x))),
      "MYdist_perc" = try(MYdist(x, percentage = TRUE)),
      "MYdist_ass" = try(MYdist(x, percentage = FALSE)),
      "euclidean" = try(dist(t(as.data.frame(x)), method = "euclidean")),
      "maximum" = try(dist(t(as.data.frame(x)), method = "maximum")),
      "manhattan" = try(dist(t(as.data.frame(x)), method = "manhattan")),
      "canberra" = try(dist(t(as.data.frame(x)), method = "canberra")),
      "minkowski" = try(dist(t(as.data.frame(x)), method = "minkowski", p = input$cluster_distance_minkowskiPower))
    )
    if (class(d)=="try-error")
      createAlert(session, anchorId = "cluster_alert", alertId = "cluster_alert_dist", content = "Error in clustering", style = "error")
    else{
      hc <- hclust(d, method = input$cluster_linkage)
      i <- 1
      id <- "Clustering"
      repeat {
        if(id %in% names(yuimaGUIdata$cluster)){
          id <- paste("Clustering", i)
          i <- i+1
        } else break
      }
      yuimaGUIdata$cluster[[id]] <<- list(d = d, linkage = input$cluster_linkage, distance = input$cluster_distance, power = input$cluster_distance_minkowskiPower)
    }
  })}
})

output$cluster_analysis_id <- renderUI({
  n <- names(yuimaGUIdata$cluster)
  if(length(n)!=0)
    selectInput("cluster_analysis_id", label = "Clustering ID", choices = sort(n), selected = last(n))
})

observeEvent(input$cluster_analysis_id, {
  if(!is.null(input$cluster_analysis_id)) if (input$cluster_analysis_id %in% names(yuimaGUIdata$cluster)){
    d <- yuimaGUIdata$cluster[[input$cluster_analysis_id]]$d
    hc <- hclust(d, method = yuimaGUIdata$cluster[[input$cluster_analysis_id]]$linkage)
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
})

output$cluster_moreInfo <- renderUI({
  if(!is.null(input$cluster_analysis_id)) if (input$cluster_analysis_id %in% names(isolate({yuimaGUIdata$cluster}))){
    info <- isolate({yuimaGUIdata$cluster[[input$cluster_analysis_id]]})
    dist <- switch(info$distance, 
                   "MOdist"="Markov Operator", 
                   "MYdist_perc"="Percentage Increments Distribution", 
                   "MYdist_ass"="Increments Distribution", 
                   "euclidean"="Euclidean", 
                   "maximum"="Maximum", 
                   "manhattan"="Manhattan", 
                   "canberra"="Canberra", 
                   "minkowski"="Minkowski")
    linkage <- switch(info$linkage,
                      "complete"="Complete", 
                      "single"="Single", 
                      "average"="Average", 
                      "ward.D"="Ward", 
                      "ward.D2"="Ward squared", 
                      "mcquitty"="McQuitty", 
                      "Median"="median", 
                      "centroid"="Centroid")
    if (dist=="Minkowski") dist <- paste(dist, " (", info$power,")", sep = "")
    return(HTML(paste("<div><h4>&nbsp &nbsp Linkage:",linkage, " &nbsp &nbsp &nbsp &nbsp Distance:", dist, "</h4></div>")))
  }
})

observeEvent(input$cluster_button_delete_analysis, {
  yuimaGUIdata$cluster[[input$cluster_analysis_id]] <<- NULL
})

observeEvent(input$cluster_button_deleteAll_analysis, {
  yuimaGUIdata$cluster <<- list()
})

observe({
  shinyjs::toggle("cluster_charts", condition = length(names(yuimaGUIdata$cluster))!=0)
})
