box::use(
    shiny[column,wellPanel,h4],
    visNetwork[visNetworkOutput]
)


#' @export 
ui <- function(id){
    ns <- shiny::NS(id)
    column(4,
                         wellPanel(
                           h4("Interactive Graph"),
                           visNetworkOutput("network")
                         ))
}

#' @export 
server <- function(id){
  shiny::moduleServer(id, function(input, output, session){


    })
}