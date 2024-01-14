box::use(
    shiny[column,wellPanel,checkboxGroupInput,verbatimTextOutput,h4]
)


#' @export 
ui <- function(id){
    ns <- shiny::NS(id)
      column(4,
                         wellPanel(
                           h4("Node Control"),
                           checkboxGroupInput(
                             "checkboxes",
                             "Select Nodes",
                             choices = c("Topics", "Predicted Grade", "Course Similarity")
                           ),
                           verbatimTextOutput("selectedNodes")
                         ))
}

#' @export 
server <- function(id){
  shiny::moduleServer(id, function(input, output, session){


    })
}