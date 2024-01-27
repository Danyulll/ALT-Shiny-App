library(CurricularAnalytics)
library(visNetwork)
library(shiny)

source("functions.R")

ui <- fluidPage(titlePanel("Data Science Curriculum Explorer"),
                fluidRow(
                  column(
                    4,
                    wellPanel(
                      h4("Courses Information"),
                      selectInput(
                        "dropdownYear",
                        "Select Year",
                        choices = c("Year 1", "Year 2", "Year 3", "Year 4")
                      ), selectInput(
                        "dropdownCourseCode",
                        "Select Code",
                        choices = c("MATH", "DATA", "BIOL", "CHEM","EESC","PHYS","COSC","ENGL","APSC","STAT","PSYO","PHIL")
                      ),
                      uiOutput("coursestaken"),
                      actionButton("resetButton", "Reset"),
                      h4("Legend"),
                      uiOutput("formattedText")
                    )
                  ),
                  column(4,
                         wellPanel(
                           h4("Interactive Graph"),
                           visNetworkOutput("network")
                         )),
                  column(4,
                         wellPanel(
                           checkboxGroupInput(
                             "checkboxes",
                             "Select Nodes",
                             choices = c("Topics", "Predicted Grade", "Course Similarity")
                           ),
                           verbatimTextOutput("selectedNodes")
                         ))
                ))


server <- function(input, output, session) {
  valuesYear1 <- reactiveValues(selected = character(0))
  valuesYear2 <- reactiveValues(selected = character(0))
  valuesYear3 <- reactiveValues(selected = character(0))
  valuesYear4 <- reactiveValues(selected = character(0))


  output$network <- renderVisNetwork({
    if( all(
      length(valuesYear1$selected) == 0,
      length(valuesYear2$selected) == 0,
      length(valuesYear3$selected) == 0,
      length(valuesYear4$selected) == 0
    )){
      empty_nodes <- data.frame(id = numeric(0), label = character(0))
      empty_edges <- data.frame(from = numeric(0), to = numeric(0))
      visNetwork(empty_nodes, empty_edges)
    }else{
      plot_graph(
        c(
          valuesYear1$selected,
          valuesYear2$selected,
          valuesYear3$selected,
          valuesYear4$selected
        ),
        "..\\data\\Example-Curriculum.csv"
      )
    }

  })

  output$coursestaken <- renderUI({
    if (input$dropdownYear == "Year 1") {
      checkboxGroupInput(
        "checkboxes1",
        "Choose Options",
        choices = getCourses(1, "..\\data\\Example-Curriculum.csv",input$dropdownCourseCode),
        selected = isolate(valuesYear1$selected)
      )
    } else if (input$dropdownYear == "Year 2") {
      checkboxGroupInput(
        "checkboxes2",
        "Choose Options",
        choices = getCourses(2, "..\\data\\Example-Curriculum.csv",input$dropdownCourseCode),
        selected = isolate(valuesYear2$selected)
      )
    } else if (input$dropdownYear == "Year 3") {
      checkboxGroupInput(
        "checkboxes3",
        "Choose Options",
        choices = getCourses(3, "..\\data\\Example-Curriculum.csv",input$dropdownCourseCode),
        selected = isolate(valuesYear3$selected)
      )
    } else if (input$dropdownYear == "Year 4") {
      checkboxGroupInput(
        "checkboxes4",
        "Choose Options",
        choices = getCourses(4, "..\\data\\Example-Curriculum.csv",input$dropdownCourseCode),
        selected = isolate(valuesYear4$selected)
      )
    }
  })

  observeEvent(input$checkboxes1,
               {
                 c(valuesYear1$selected,input$checkboxes1) |> unique() -> valuesYear1$selected
                 test <- c(
                   isolate(valuesYear1$selected),
                   isolate(valuesYear2$selected),
                   isolate(valuesYear3$selected),
                   isolate(valuesYear4$selected)
                 )
                 cat(paste(
                   "Courses selected: ",
                   paste(test, collapse = ", "),
                   "\nTotal elements: ",
                   length(test),
                   "\n"
                 ))

               })

  observeEvent(input$checkboxes2, {
    c(input$checkboxes2,valuesYear2$selected) |> unique() -> valuesYear2$selected
    test <- c(
      isolate(valuesYear1$selected),
      isolate(valuesYear2$selected),
      isolate(valuesYear3$selected),
      isolate(valuesYear4$selected)
    )
    cat(paste(
      "Courses selected: ",
      paste(test, collapse = ", "),
      "\nTotal elements: ",
      length(test),
      "\n"
    ))
  })

  observeEvent(input$checkboxes3, {
    c(input$checkboxes3,valuesYear3$selected) |> unique() -> valuesYear3$selected
    test <- c(
      isolate(valuesYear1$selected),
      isolate(valuesYear2$selected),
      isolate(valuesYear3$selected),
      isolate(valuesYear4$selected)
    )
    cat(paste(
      "Courses selected: ",
      paste(test, collapse = ", "),
      "\nTotal elements: ",
      length(test),
      "\n"
    ))
  })

  observeEvent(input$checkboxes4, {
    c(input$checkboxes4,valuesYear4$selected) |> unique() -> valuesYear4$selected
    test <- c(
      isolate(valuesYear1$selected),
      isolate(valuesYear2$selected),
      isolate(valuesYear3$selected),
      isolate(valuesYear4$selected)
    )
    cat(paste(
      "Courses selected: ",
      paste(test, collapse = ", "),
      "\nTotal elements: ",
      length(test),
      "\n"
    ))
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

  observeEvent(input$resetButton, {
    updateCheckboxGroupInput(session, "checkboxes1", selected = character(0))
    updateCheckboxGroupInput(session, "checkboxes2", selected = character(0))
    updateCheckboxGroupInput(session, "checkboxes3", selected = character(0))
    updateCheckboxGroupInput(session, "checkboxes4", selected = character(0))

    valuesYear1$selected <- character(0)
    valuesYear2$selected <- character(0)
    valuesYear3$selected <- character(0)
    valuesYear4$selected <- character(0)

  })
}





shinyApp(ui, server)
