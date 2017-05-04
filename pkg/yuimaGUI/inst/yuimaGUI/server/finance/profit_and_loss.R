
hedging_databaseModels_table <- data.frame()
output$hedging_databaseModels <- DT::renderDataTable(options=list(scrollY = 200, scrollX = TRUE, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', rownames = TRUE, selection = "single",{
  if (length(yuimaGUItable$model)==0){
    NoData <- data.frame("Symb"=NA,"Please estimate some models first"=NA, check.names = FALSE)
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

output$hedging_strike <- renderUI({
  if (is.null(input$hedging_databaseModels_rows_selected))
    numericInput("hedging_strike", label="Strike Price:", value=0, min = 0, max = NA, step = NA, width = NULL)
  else {
    if(input$hedging_databaseModels_row_last_clicked %in% input$hedging_databaseModels_rows_selected){
      id <- unlist(strsplit(rownames(hedging_databaseModels_table)[input$hedging_databaseModels_row_last_clicked], split = " "))
      numericInput("hedging_strike", label="Strike Price:", value=as.numeric(tail(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data,1)), min = 0, max = NA, step = NA, width = NULL)
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
      info = list(
        "maturity"= input$hedging_maturity,
        "strike"=input$hedging_strike, 
        "type"=input$hedging_type, 
        "optPrice"=input$hedging_optMarketPrice, 
        "optLotMult"=input$hedging_lotMult,
        "optLotCost"= ifelse(is.na(input$hedging_lotCostOpt), 0, input$hedging_lotCostOpt),
        "assPrice"=input$hedging_assMarketPrice,
        "assPercCost"= ifelse(is.na(input$hedging_percCostAss), 0, input$hedging_percCostAss/100),
        "assMinCost"= ifelse(is.na(input$hedging_minCostAss), 0, input$hedging_minCostAss),
        "assRateShortSelling"= ifelse(is.na(input$hedging_rateShort), 0, input$hedging_rateShort/100))
      addHedging(
        symbName = id[1],
        modelYuimaGUI = yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]],
        info = info,
        xinit = input$hedging_assMarketPrice,
        nsim = input$hedging_nSim,
        nstep = NA,
        simulate.from = end(yuimaGUIdata$model[[id[1]]][[as.numeric(id[2])]]$model@data@original.data),
        simulate.to = input$hedging_maturity,
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





hedging_values <- reactiveValues(profits=NULL, symb=NULL, model=NULL, return = NA)
hedging_values2 <- reactiveValues(number_of_ids = 0)
na_zero <- function(x){ifelse(is.na(x), 0, x)}

observe({
  hedging_values2$number_of_ids <<- length(yuimaGUIdata$hedging)
  shinyjs::toggle(id = "hedging_button_show", condition = hedging_values2$number_of_ids>0)
})

output$hedging_modal_id <- renderUI({
  if(hedging_values2$number_of_ids>0) 
    selectInput("hedging_modal_id", label = "ID", choices = seq(1, hedging_values2$number_of_ids))
})

output$hedging_modal_id_hidden <- renderUI({
  selectInput("hedging_modal_id_hidden", label = "ID", choices = input$hedging_modal_id, selected = input$hedging_modal_id)
})
output$hedging_nAss_hedge <- renderUI({
  if (!is.null(input$hedging_modal_id)){
    id <- as.integer(input$hedging_modal_id)
    if (hedging_values2$number_of_ids>=id){
      info <- yuimaGUIdata$hedging[[id]]$info
      val <- switch (info$type, "call" = info$sell, "put" = info$buy)
      lab <- paste("Number of Assets to", ifelse(info$type=="call", "Sell", "Buy"))
      numericInput("hedging_nAss_hedge", label = lab, min = 0, value = val, step = 1)
    }
  }
})
output$hedging_nOptLot_hedge <- renderUI({
  if (!is.null(input$hedging_modal_id)){
    id <- as.integer(input$hedging_modal_id)
    if (hedging_values2$number_of_ids>=id){
      info <- yuimaGUIdata$hedging[[id]]$info
      nOpt <- info$LotsToBuy
      numericInput("hedging_nOptLot_hedge", label = "Option - number of Lots", min = 0, value = nOpt, step = 1)
    }
  }
})
output$hedging_type2 <- renderUI({
  if (!is.null(input$hedging_modal_id)){
    id <- as.integer(input$hedging_modal_id)
    if (hedging_values2$number_of_ids>=id){
      type <- yuimaGUIdata$hedging[[id]]$info$type
      selectInput("hedging_type2", width = "75%", label="Modify Type", c(Call="call", Put="put"), selected = type)
    }
  }
})
output$hedging_strike2 <- renderUI({
  if (!is.null(input$hedging_modal_id)){
    id <- as.integer(input$hedging_modal_id)
    if (hedging_values2$number_of_ids>=id){
      strike <- yuimaGUIdata$hedging[[id]]$info$strike
      numericInput("hedging_strike2", width = "75%", label = "Modify Strike", min = 0, value = strike)
    }
  }
})
output$hedging_optMarketPrice2 <- renderUI({
  if (!is.null(input$hedging_modal_id)){
    id <- as.integer(input$hedging_modal_id)
    if (hedging_values2$number_of_ids>=id){
      optPrice <- yuimaGUIdata$hedging[[id]]$info$optPrice
      numericInput("hedging_optMarketPrice2", width = "75%", label = "Modify Market Price", min = 0, value = optPrice)
    }
  }
})

observe({
  id <- input$hedging_modal_id_hidden
  if (!is.null(id) & !is.null(input$hedging_strike2) & !is.null(input$hedging_nAss_hedge)){
    id <- as.integer(id)
    if(hedging_values2$number_of_ids>=id){
      info <- yuimaGUIdata$hedging[[id]]$info
      profits <- profit_distribution(nOpt= na_zero(input$hedging_nOptLot_hedge)*info$optLotMult, 
                                     nAss= na_zero(input$hedging_nAss_hedge), 
                                     type=input$hedging_type2,
                                     strike=ifelse(is.na(input$hedging_strike2), info$strike, input$hedging_strike2),
                                     priceAtMaturity=yuimaGUIdata$hedging[[id]]$hist, 
                                     optMarketPrice=ifelse(is.na(input$hedging_optMarketPrice2), info$optPrice, input$hedging_optMarketPrice2),
                                     assMarketPrice=info$assPrice, 
                                     percCostAss=na_zero(input$hedging_percCostAss)/100, 
                                     minCostAss=na_zero(input$hedging_minCostAss), 
                                     lotCostOpt=na_zero(input$hedging_lotCostOpt), 
                                     lotMultiplier=info$optLotMult, 
                                     shortCostPerYear=na_zero(input$hedging_rateShort)/100, 
                                     t0=info$today, 
                                     maturity=info$maturity)
      hedging_values$profits <- profits
      hedging_values$symb <- yuimaGUIdata$hedging[[id]]$symb
      hedging_values$model <- yuimaGUIdata$hedging[[id]]$model$info$modName
    }
  }
})

output$hedging_plot_distribution <- renderPlot({
  par(bg="black")
  if (!is.null(hedging_values$profits) & !is.null(hedging_values$model) & !is.null(hedging_values$symb))
    hist(hedging_values$profits, main = paste(hedging_values$symb,"-",hedging_values$model), xlab = "Profit & Loss", breaks = input$hedging_slider_nBin, col="green", col.axis="grey", col.lab="grey", col.main="grey", fg="black", right = FALSE)
  grid()
})
output$hedging_quantiles_text <- renderUI({
  if(!is.null(input$hedging_slider_rangeHist) & !is.null(hedging_values$profits)){
    val <- hedging_values$profits
    qq <- quantile(val, probs = input$hedging_slider_rangeHist/100)
    HTML(paste("<div>", "Lower:", round(qq[1],0),"<br/>", "Upper: ", round(qq[2],0), "<br/>", "Mean: ", round(mean(val[val>=qq[1] & val<=qq[2]]),0), "</div>"))
  }
})
output$hedging_capital_text <- renderUI({
  if (!is.null(input$hedging_modal_id) & !is.null(hedging_values$profits)){
    id <- as.integer(input$hedging_modal_id)
    if (hedging_values2$number_of_ids>=id){
      info <- isolate({yuimaGUIdata$hedging[[id]]$info})
      optPrice <- ifelse(is.na(input$hedging_optMarketPrice2), info$optPrice, input$hedging_optMarketPrice2)
      percCostAss <- na_zero(input$hedging_percCostAss)/100
      minCostAss <- na_zero(input$hedging_minCostAss)
      lotCostOpt <- na_zero(input$hedging_lotCostOpt)
      nOptLot <- na_zero(input$hedging_nOptLot_hedge)
      nAss <- na_zero(input$hedging_nAss_hedge)
      cap <- nOptLot*(info$optLotMult*optPrice+lotCostOpt)+nAss*info$assPrice + ifelse(nAss>0,max(nAss*info$assPrice*percCostAss,minCostAss),0)
      val <- hedging_values$profits
      ret <- mean(val)/cap
      hedging_values$return <- ret
      HTML(paste("Invested Capital: ", round(cap,0), "<br/>", "Average Return: ", round(ret*100,2), "%"))
    }
  }
})


observeEvent(input$hedging_button_saveHedging, {
  id <- as.integer(input$hedging_modal_id)
  yuimaGUIdata$hedging[[id]]$info$assPercCost <<- ifelse(is.na(input$hedging_percCostAss), 0, input$hedging_percCostAss/100)
  yuimaGUIdata$hedging[[id]]$info$assMinCost <<- ifelse(is.na(input$hedging_minCostAss), 0, input$hedging_minCostAss)
  yuimaGUIdata$hedging[[id]]$info$assRateShortSelling <<- ifelse(is.na(input$hedging_rateShort), 0, input$hedging_rateShort/100)
  yuimaGUIdata$hedging[[id]]$info$optLotCost <<- ifelse(is.na(input$hedging_lotCostOpt), 0, input$hedging_lotCostOpt)
  yuimaGUIdata$hedging[[id]]$info$type <<- input$hedging_type2
  if (yuimaGUIdata$hedging[[id]]$info$type=="put"){
    yuimaGUIdata$hedging[[id]]$info$buy <<- na_zero(input$hedging_nAss_hedge)
    yuimaGUIdata$hedging[[id]]$info$sell <<- NA
  }
  if (yuimaGUIdata$hedging[[id]]$info$type=="call"){
    yuimaGUIdata$hedging[[id]]$info$sell <<- na_zero(input$hedging_nAss_hedge)
    yuimaGUIdata$hedging[[id]]$info$buy <<- NA
  }
  yuimaGUIdata$hedging[[id]]$info$LotsToBuy <<- na_zero(input$hedging_nOptLot_hedge)
  if (!is.na(input$hedging_strike2))
    yuimaGUIdata$hedging[[id]]$info$strike <<- input$hedging_strike2
  if (!is.na(input$hedging_optMarketPrice2))
    yuimaGUIdata$hedging[[id]]$info$optPrice <<- input$hedging_optMarketPrice2
  yuimaGUIdata$hedging[[id]]$info$profit <<- hedging_values$return
})

observe({
  shinyjs::toggle("hedging_alert_selectRow", condition = (input$panel_hedging=="Start simulations"))
})

###Delete Hedging
observeEvent(input$hedging_button_delete, priority = 1, {
  if(!is.null(input$hedging_table_results_rows_selected) & !is.null(input$hedging_modal_id)){
    if(input$hedging_table_results_row_last_clicked %in% input$hedging_table_results_rows_selected){
      delHedging(n=input$hedging_table_results_row_last_clicked)
    }
  }
})

###DeleteAll Hedging
observeEvent(input$hedging_button_deleteAll, priority = 1, {
  if(!is.null(input$hedging_table_results_rows_all))
    delHedging(n=input$hedging_table_results_rows_all)
})
