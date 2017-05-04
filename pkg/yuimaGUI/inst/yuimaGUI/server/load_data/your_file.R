###Read file
fileUp_O <- reactive({
  if (!is.null(input$yourFile$datapath)){
    sep <- input$yourFileSep
    if(input$yourFileSep=="default") sep <- ""
    skip <- input$yourFileLine-1
    if(is.na(skip)) skip <- 0
    dec <- input$yourFileDec
    if(input$yourFileDec=="") dec <- "."
    if(input$yourFileHeader=="Only rows")
      z <- read.csv(input$yourFile$datapath ,sep = sep, header = FALSE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
    if(input$yourFileHeader=="Only columns"){
      z <- read.csv(input$yourFile$datapath, sep = sep, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
      z <- data.frame(t(z), row.names = 1, check.names = FALSE)
      z <- data.frame(t(z), check.names = FALSE)
    }
    if (input$yourFileHeader=="Both")
      z <- read.csv(input$yourFile$datapath, sep = sep, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
    if (input$yourFileHeader=="None")
      z <- read.csv(input$yourFile$datapath, sep = sep, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
    if (input$yourFileHeader=="Default")
      z <- read.csv(input$yourFile$datapath, sep = sep, check.names = FALSE, stringsAsFactors = FALSE, dec = dec, na.strings = input$yourFileNA, skip = skip)
    if (input$yourFileHeader=="Only rows" | identical(colnames(z),paste("V",seq(1,length(colnames(z))),sep="")))
      colnames(z) <- paste("X",seq(1,length(colnames(z))),"_",make.names(input$yourFile$name),sep="")
    dec <- isolate({ifelse(input$yourFileDec=="", ".", input$yourFileDec)})
    if(dec==".") dec <- "\\."
    thnd <- input$yourFileThnd
    if(thnd==".") thnd <- "\\."
    zz <- data.frame(row.names = rownames(z), x = apply(z, 2, function(x) gsub(pattern =  dec, replacement =  ".", x = gsub(pattern =  thnd, replacement =  "", x = as.character(x)))))
    colnames(zz) <- colnames(z)
    return(zz)
  }
})

###Display Index choices: columns of file or transposed file
output$yourFileIndex <- renderUI({
  temp <- try(colnames(fileUp_O()))
  if (input$yourFileSwitch==TRUE){
    temp <- try(rownames(fileUp_O()))
    if(class(temp)!="try-error")      
      if (input$yourFileHeader=="Only columns" | identical(temp,paste("V",seq(1,length(temp)),sep="")))
        temp <- paste("X",seq(1,length(temp)),"_",make.names(input$yourFile$name),sep="")
  }
  if (class(temp)=="try-error")
    return(selectInput("yourFileIndex",label = "Index", width = "60%", choices = c("Row Headers"="default","Numeric"="numeric"), selected = "default"))
  if (class(temp)!="try-error")
    return(selectInput("yourFileIndex",label = "Index", width = "60%", choices = c("Row Headers"="default","Numeric"="numeric",temp), selected = "default"))
})


###File to upload
fileUp <- reactive({
  if (!is.null(input$yourFile$datapath)){
    z <- fileUp_O()
    if (input$yourFileSwitch==TRUE) {
      z <- as.data.frame(t(z), check.names = FALSE)
      if (identical(colnames(z), as.character(seq(1,length(colnames(z))))))
        colnames(z) <- paste("X",seq(1,length(colnames(z))),"_",make.names(input$yourFile$name),sep="")
    }
    ###Display choices for Index Type and set to "numeric" if Index is "numeric"
    output$yourFileFUN <- renderUI({
      if (!is.null(input$yourFileIndex)){
        sel <- "%Y-%m-%d"
        if (input$yourFileIndex=="numeric" | !all(is.na(as.numeric(as.character(rownames(z))))) )
          sel <- "numeric"
        selectInput("yourFileFUN", label = "Index Format", width = "60%", choices = c("Numeric"="numeric", "Year-Month-Day    (yyyy-mm-dd)"="%Y-%m-%d", "Month-Day-Year    (mm-dd-yyyy)"="%m-%d-%Y", "Month-Day-Year    (mm-dd-yy)"="%m-%d-%y", "Day-Month-Year    (dd-mm-yyyy)"="%d-%m-%Y", "Day-Month-Year    (dd-mm-yy)"="%d-%m-%y", "Year/Month/Day    (yyyy/mm/dd)"="%Y/%m/%d", "Month/Day/Year    (mm/dd/yyyy)"="%m/%d/%Y", "Month/Day/Year    (mm/dd/yy)"="%m/%d/%y", "Day/Month/Year    (dd/mm/yyyy)"="%d/%m/%Y", "Day/Month/Year    (dd/mm/yy)"="%d/%m/%y"), selected = sel)
      }
    })
    if(input$yourFileIndex!="default" & input$yourFileIndex!="numeric")
      z <- data.frame(z, row.names = which(colnames(z)==input$yourFileIndex), check.names = FALSE)
    if(input$yourFileIndex=="numeric")
      z <- data.frame(z, row.names = seq(1,length(rownames(z))), check.names = FALSE)
    return (z)
  }
})

###Display Upload Button
output$yourFileButton <- renderUI ({
  if (!is.null(input$yourFile$datapath))
    return(tags$button(type="button", id="yourFileGo", class = "action-button", em("Load data")))
})

observe({
  shinyjs::toggle("yourFileButton", condition = "try-error"!=(class(try(fileUp()))))
})

###Display text "Preview"
output$yourFilePreviewText <- renderText ({
  if (!is.null(input$yourFile$datapath))
    return("Preview")
})

###Display Preview of file to upload
output$yourFilePreview <- DT::renderDataTable(options=list(scrollX=TRUE, scrollY = 250, scrollCollapse = FALSE, deferRender = TRUE, dom = 'frtiS'), extensions = 'Scroller', selection = "none", rownames = TRUE, {
  if (!is.null(input$yourFile$datapath))
    return (fileUp())
})

###Upload file
observeEvent(input$yourFileGo, priority = 1, {
  closeAlert(session, "yourDataAlert_err")
  closeAlert(session, "yourDataAlert_warn")
  closeAlert(session, "yourDataAlert_succ")
  info <- addData(fileUp(), typeIndex = input$yourFileFUN)
  if(!is.null(info$err))
    createAlert(session = session, anchorId = "yourDataAlert", alertId = "yourDataAlert_err", content = paste("Unable to load following symbols:", paste(info$err,collapse = " ")), style = "error")
  if(!is.null(info$already_in))
    createAlert(session = session, anchorId = "yourDataAlert", alertId = "yourDataAlert_warn", content = paste("WARNING! Following symbols already loaded:", paste(info$already_in,collapse = " ")), style = "warning")
  if(is.null(info$err) & is.null(info$already_in))
    createAlert(session = session, anchorId = "yourDataAlert", alertId = "yourDataAlert_succ", content = paste("All symbols loaded successfully"), style = "success")
})

###Display data available
output$database2 <- DT::renderDataTable(options=list(scrollY = 200, scrollCollapse = FALSE, deferRender = FALSE, dom = 'frtS'), extensions = 'Scroller', selection = "multiple", rownames = FALSE,{
  if (length(yuimaGUItable$series)!=0)
    return (yuimaGUItable$series)
})

###Delete Button
observeEvent(input$yourFileDelete, priority = 1,{
  delData(yuimaGUItable$series$Symb[input$database2_rows_selected])
})

###DeleteAll Button
observeEvent(input$yourFileDeleteAll, priority = 1,{
  delData(yuimaGUItable$series$Symb[input$database2_rows_all])
})

###Save Button
output$yourFileSave <- {
  saveData()
}

observe({
  shinyjs::toggle("buttons_DataIO_file", condition = length(yuimaGUIdata$series)!=0)
  shinyjs::toggle("buttons_DataIO_fin", condition = length(yuimaGUIdata$series)!=0)
})
