yuimaGUI <- function(theme = NULL) {

  print("Please wait while loading...")
  addr <- system.file("yuimaGUI", package = "yuimaGUI")
  
  if(!is.null(theme)){
    addr_from <- paste(addr, "/www/", theme, ".css", sep = "")
    addr_to <- paste(addr, "/www/custom.css", sep = "")
    if(file.exists(addr_from))
      file.copy(from = addr_from, to = addr_to, overwrite = TRUE)
    else print("Theme not supported. Using default.")
  }
  
  
  utils::capture.output(
    suppressWarnings(
      shiny::runApp(addr)
    )
  )
  
}
