box::use(
  shiny[bootstrapPage, div, moduleServer, NS,fluidPage]
)

box::use(
  app/logic/graph,
  app/view[courseChecks,curGraph,recommend]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    # app$ui(ns("app"))
    courseChecks$ui(ns("courseChecks")),
    curGraph$ui(ns("curGraph")),
    recommend$ui(ns("recommend"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # app$server("app")
    courseChecks$server("courseChecks")
    curGraph$server("curGraph")
    recommend$server("recommend")
  })
}

