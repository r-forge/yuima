yuimaGUI <- function() {
  utils::capture.output(
    suppressWarnings(
      shiny::runApp(
        system.file(
          "yuimaGUI",
          package = "yuimaGUI"
        )
      )
    )
  )
}
