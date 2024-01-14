box::use(
    shiny[fluidPage,titlePanel,fluidRow,column,wellPanel,selectInput,uiOutput,actionButton,h4,checkboxGroupInput,verbatimTextOutput],
    visNetwork[visNetworkOutput]
)

box::use(
    app/logic/graph
)

#' @export 
ui <- function(id){
    ns <- shiny::NS(id)
     fluidPage(titlePanel("Data Science Curriculum Explorer"),
                fluidRow(
                  column(4,
                         wellPanel(
                           h4("Courses Information"),
                           selectInput(
                             "dropdown",
                             "Select Option",
                             choices = c("Year 1", "Year 2", "Year 3", "Year 4")
                           ),
                           uiOutput("coursestaken"),
                           actionButton("resetButton", "Reset"),
                           h4("Legend"),
                           uiOutput("formattedText")
                         )),
                  column(4,
                         wellPanel(
                           h4("Interactive Graph"),
                           visNetworkOutput("network")
                         )),
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
                ))
}

#' @export 
server <- function(id){
    shiny$moduleServer(id, function(input, output, session){

    })
}