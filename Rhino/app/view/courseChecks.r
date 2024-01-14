box::use(
    shiny[column,wellPanel,h4,selectInput,uiOutput,actionButton,reactiveValues,reactive,renderUI,checkboxGroupInput,observeEvent,HTML,updateCheckboxGroupInput]
)


#' @export 
ui <- function(id){
    ns <- shiny::NS(id)
       column(4,
                         wellPanel(
                           h4("Courses Information"),
                           selectInput(
                             ns("dropdown"),
                             "Select Option",
                             choices = c("Year 1", "Year 2", "Year 3", "Year 4")
                           ),
                           uiOutput(ns("coursestaken")),
                           actionButton(ns("resetButton"), "Reset"),
                           h4("Legend"),
                           uiOutput(ns("formattedText"))
                         ))
}

#' @export 
server <- function(id){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session){
    valuesYear1 <- reactiveValues(selected = character(0))
    valuesYear2 <- reactiveValues(selected = character(0))
    valuesYear3 <- reactiveValues(selected = character(0))
    valuesYear4 <- reactiveValues(selected = character(0))

    output$coursestaken <- renderUI({
    if (input$dropdown == "Year 1") {
      checkboxGroupInput(
        ns("checkboxes1"),
        "Choose Options",
        choices = c("DATA 101", "MATH 100", "MATH 101")
      )
    } else if (input$dropdown == "Year 2") {
      checkboxGroupInput(
        ns("checkboxes2"),
        "Choose Options",
        choices = c("STAT 230", "MATH 200", "COSC 221")
      )
    } else if (input$dropdown == "Year 3") {
      checkboxGroupInput(
        ns("checkboxes3"),
        "Choose Options",
        choices = c("DATA 301", "DATA 311", "STAT 303")
      )
    } else if (input$dropdown == "Year 4") {
      checkboxGroupInput(
        ns("checkboxes4"),
        "Choose Options",
        choices = c("DATA 405", "DATA 407", "DATA 410")
      )
    }
  })

      observeEvent(input$checkboxes1, {
    valuesYear1$selected <- input$checkboxes1

    if (length(input$checkboxes1) %in% 0:4) {
      cat(paste(input$checkboxes1,sep = " "))
      cat(paste("\nNumber of elements selected: ",length(valuesYear1$selected)))
      cat("\n------------------\n")
    }
  })

  observeEvent(input$checkboxes2, {
    valuesYear2$selected <- input$checkboxes2

    
  })

  observeEvent(input$checkboxes3, {
    valuesYear3$selected <- input$checkboxes3
  })

  observeEvent(input$checkboxes4, {
    valuesYear4$selected <- input$checkboxes4
  })

  output$formattedText <- renderUI({
    HTML(
      "
    df: Delay Factor - measures the degree to which failing this course will delay graduation.<br>
    bf: Blocking Factor - measures the degree to which failing this course will block access to future courses.<br>
    cf: Centrality - measures the degree to which a course is central to a curriculum and therefore contains important concepts for the entirety of the program.<br>
    sc: Structural Complexity - measures the degree to which the course or curriculum add to the difficutly of completing the program.<br>
    "
    )
  })


    # This does nothing atm
    observeEvent(input$resetButton, {
      updateCheckboxGroupInput(session, ns("checkboxes1"), selected = character(0))
      updateCheckboxGroupInput(session, ns("checkboxes2"), selected = character(0))
      updateCheckboxGroupInput(session, ns("checkboxes3"), selected = character(0))
      updateCheckboxGroupInput(session, ns("checkboxes4"), selected = character(0))

  })

    })
}