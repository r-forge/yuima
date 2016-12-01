yuimaGUI <- function() {
  invisible(shiny::runApp(
    system.file(
      "yuimaGUI",
      package = "yuimaGUI"
    )
  ))
}
