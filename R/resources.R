.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = "custom-assets",
    directoryPath = system.file("www", package = "fofdash")
  )
}