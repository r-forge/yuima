yuimaGUI <- function() {
  suppressWarnings(
    shiny::runApp(
      system.file(
        "yuimaGUI",
        package = "yuimaGUI"
      )
    )
  )
}
