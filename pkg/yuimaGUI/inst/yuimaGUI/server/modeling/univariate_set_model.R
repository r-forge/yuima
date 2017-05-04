
output$usr_modelClass_latex <- renderUI({
  if (input$usr_modelClass=="Diffusion process")
    return(withMathJax("$$dX=a(t,X,\\theta)\\;dt\\;+\\;b(t,X,\\theta)\\;dW$$"))
  if (input$usr_modelClass=="Fractional process")
    return(withMathJax("$$dX=a(t,X,\\theta)\\;dt\\;+\\;b(t,X,\\theta)\\;dW^H$$"))
  if (input$usr_modelClass=="Compound Poisson")
    return(withMathJax("$$X_t = X_0+\\sum_{i=0}^{N_t} Y_i \\; : \\;\\;\\;  N_t \\sim Poi\\Bigl(\\int_0^t \\lambda(t)dt\\Bigl)$$"))
  if (input$usr_modelClass=="Levy process")
    return(withMathJax("$$dX_t = \\mu X_t \\; dt + \\sigma X_t \\; dW_t + X_t \\; dZ_t$$"))
})

observe({
  if (input$usr_modelClass=="Fractional process") createAlert(session = session, anchorId = "panel_set_model_alert", alertId = "alert_fracinfo", style = "info", content = "Fractional process you set here will be available for simulation purposes, but not for estimation.")
  else closeAlert(session = session, alertId = "alert_fracinfo")
})

output$usr_model_coeff <- renderUI({
  if (input$usr_modelClass=="Diffusion process")
    return(
      div(align="center", 
          column(6, textInput("usr_model_coeff_drift", width = "70%", label = withMathJax("$$a(t,X,\\theta)$$"))),
          column(6, textInput("usr_model_coeff_diff", width = "70%", label = withMathJax("$$b(t,X,\\theta)$$")))
      )
    )
  if (input$usr_modelClass=="Fractional process")
    return(
      div(align="center", 
          column(6, textInput("usr_model_coeff_drift", width = "70%", label = withMathJax("$$a(t,X,\\theta)$$"))),
          column(6, textInput("usr_model_coeff_diff", width = "70%", label = withMathJax("$$b(t,X,\\theta)$$")))
      )
    )
  if (input$usr_modelClass=="Compound Poisson")
    return(
      div(align="center",
          textInput("usr_model_coeff_intensity", width = "45%", label = withMathJax("$$\\lambda(t)$$"))
      )
    )
  if (input$usr_modelClass=="Levy process")
    return(
      div(align="center",
          fluidRow(column(12,textInput("usr_model_coeff_intensity", width = "45%", label = withMathJax("$$\\lambda(t)$$")))),
          fluidRow(
            column(6, textInput("usr_model_coeff_drift", width = "70%", label = withMathJax("$$a(t,X,\\theta)$$"))),
            column(6, textInput("usr_model_coeff_diff", width = "70%", label = withMathJax("$$b(t,X,\\theta)$$")))
          )
      )
    )
})

observeEvent(input$usr_model_button_save, {
  entered <- FALSE
  switch(input$usr_modelClass,
         "Diffusion process" = {
           if (input$usr_model_name!="" & (input$usr_model_coeff_drift!="" | input$usr_model_coeff_diff!="")){
             mod <- try(setModel(drift = tolower(input$usr_model_coeff_drift), diffusion = tolower(input$usr_model_coeff_diff), solve.variable = "x"))
             if(class(mod)!="try-error") yuimaGUIdata$usr_model[[input$usr_model_name]] <<- list(object=mod, class=input$usr_modelClass)
             entered <- TRUE
           }
         },
         "Fractional process" = {
           if (input$usr_model_name!="" & (input$usr_model_coeff_drift!="" | input$usr_model_coeff_diff!="")){
             mod <- try(setModel(drift = tolower(input$usr_model_coeff_drift), diffusion = tolower(input$usr_model_coeff_diff), hurst = NA, solve.variable = "x"))
             if(class(mod)!="try-error") yuimaGUIdata$usr_model[[input$usr_model_name]] <<- list(object=mod, class=input$usr_modelClass)
             entered <- TRUE
           }
         },
         "Compound Poisson" = {
           if (input$usr_model_name!="" & (input$usr_model_coeff_intensity!="")){
             mod <- try(setPoisson(intensity = tolower(input$usr_model_coeff_intensity), df = "", solve.variable = "x"))
             if(class(mod)!="try-error") yuimaGUIdata$usr_model[[input$usr_model_name]] <<- list(intensity=tolower(input$usr_model_coeff_intensity), class=input$usr_modelClass)
             entered <- TRUE
           }
         },
         "Levy process" = {
           if (input$usr_model_name!=""){
             mod <- try(setModel(drift=input$usr_model_coeff_drift, diffusion=input$usr_model_coeff_diff, measure.type = ifelse(is.na(input$usr_model_coeff_intensity), "code", "CP"), measure = list(intensity = input$usr_model_coeff_intensity, df = ""), solve.variable = "x"))
             if(class(mod)!="try-error") yuimaGUIdata$usr_model[[input$usr_model_name]] <<- list(intensity=tolower(input$usr_model_coeff_intensity), drift = input$usr_model_coeff_drift, diffusion = input$usr_model_coeff_diff, class=input$usr_modelClass)
             entered <- TRUE
           }
         } 
  )
  if (entered){
    yuimaGUIsettings$estimation[[input$usr_model_name]] <<- list()
    closeAlert(session, "alert_savingModels")
    if(class(mod)!="try-error") createAlert(session = session, anchorId = "panel_set_model_alert", alertId = "alert_savingModels", style = "success", content = "Model saved successfully")
    else createAlert(session = session, anchorId = "panel_set_model_alert", alertId = "alert_savingModels", style = "error", content = "Model is not correctly specified")
  }
})


observe({
  for(mod in names(yuimaGUIsettings$estimation))
    if (!(mod %in% c(names(yuimaGUIdata$usr_model), names(defaultModels))))
      yuimaGUIsettings$estimation[[mod]] <<- list()
})


output$usr_model_saved <- renderUI({
  if (length(names(yuimaGUIdata$usr_model))!=0)
    selectInput("usr_model_saved", label = "Saved Models", choices = names(yuimaGUIdata$usr_model), selected = tail(names(yuimaGUIdata$usr_model),1))
})

output$usr_model_saved_latex <- renderUI({
  input$usr_model_button_save
  if (!is.null(input$usr_model_saved)) if (input$usr_model_saved %in% names(yuimaGUIdata$usr_model))
    withMathJax(printModelLatex(input$usr_model_saved, process = yuimaGUIdata$usr_model[[input$usr_model_saved]]$class))
})

observeEvent(input$usr_model_button_delete, {
  for (i in input$usr_model_saved)
    yuimaGUIdata$usr_model[i] <<- NULL
})

observe({
  shinyjs::toggle("usr_model_saved_div", condition = length(names(yuimaGUIdata$usr_model))!=0)
})
