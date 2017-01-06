yuimaGUI <- function(theme = "last") {

  print("Please wait while loading...")
  addr <- system.file("yuimaGUI", package = "yuimaGUI")
  
  if(theme!="last"){
    addr_from <- paste(addr, "/www/", theme, ".css", sep = "")
    addr_to <- paste(addr, "/www/custom.css", sep = "")
    if(file.exists(addr_from))
      file.copy(from = addr_from, to = addr_to, overwrite = TRUE)
    else print("Theme not supported.")
  }
  
  utils::capture.output(
    suppressWarnings(
      shiny::runApp(addr)
    )
  )
  
}
