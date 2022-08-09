#' User-friendly Interface for the yuima package
#' 
#' Runs yuima Graphical User Interface
#' 
#' @param theme GUI theme: "black" or "white".
#' 
#' @return Starts yuima GUI
#' 
#' @author The YUIMA Project Team
#' 
#' @examples 
#' \dontrun{
#' yuimaGUI()
#' }
#' 
#' @export
#' 
yuimaGUI <- function(theme = "black") {

  if(!(theme %in% c("black", "white"))) stop ("Theme not supported. Only 'black' or 'white' themes are available.")
  print("Please wait while loading...")
  
  options(yuimaGUItheme = theme)
  
  utils::capture.output(
    suppressWarnings(
      shiny::runApp(system.file("yuimaGUI", package = "yuimaGUI"))
    )
  )
  
}
