box::use(
  shiny[bootstrapPage, div, moduleServer, NS,fluidPage]
)

box::use(
  app/view/app
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    app$ui(ns("app"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    app$server("app")
  })
}

