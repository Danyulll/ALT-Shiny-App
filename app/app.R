library(CurricularAnalytics)
library(visNetwork)
library(shiny)

ui <- fluidPage(titlePanel("Data Science Curriculum Explorer"),
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

server <- function(input, output, session) {
  # Values for courses taken
  valuesYear1 <- reactiveValues(selected = character(0))
  valuesYear2 <- reactiveValues(selected = character(0))
  valuesYear3 <- reactiveValues(selected = character(0))
  valuesYear4 <- reactiveValues(selected = character(0))





  # Interactive visNetwork graph
  output$network <- renderVisNetwork({
    selectedOptions <-
      c(
        valuesYear1$selected,
        valuesYear2$selected,
        valuesYear3$selected,
        valuesYear4$selected
      )
    if (length(selectedOptions) %in% 0:1) { # Currently if this condition just checks 0 when you uncheck all boxes it will still display 1 node despite the checkboxes vector being 0. This is because of how selectedOptions is updated, its length stays at 1. Need to learn how to force update SelecredOptions so that it goes to length 0 when all checkboxes are unchecked.
      nodes <- data.frame(id = numeric(0), label = character(0))
      edges <- data.frame(from = numeric(0), to = numeric(0))
      visNetwork(nodes, edges)
    } else{
      node_list <-
        data.frame(
          "id" = 1:length(selectedOptions) ,
          "label" = selectedOptions
        )

      df <-
        utils::read.csv(
          "C:\\Users\\danie\\Desktop\\School\\ALT-Shiny-App\\data\\Example-Curriculum.csv" #TODO make this not a hard path
        )

      edge_list <- data.frame(from = NA, to = NA)
      terms <- c()
      for (node_id in node_list$id) {
        terms <- subset(df, id == node_id)$term
        node_reqs <- subset(df, id == node_id)$requisites
        req_ids <- ""
        if (node_reqs != "") {
          req_ids <- as.numeric(strsplit(node_reqs, ";")[[1]])

          from <- rownames(subset(df, id == node_id))
          to <- rownames(subset(df, id %in% req_ids))
          if (length(to) > 1) {
            for (id in to) {
              edge_list <- rbind(edge_list, data.frame(from = id, to = from))
            }
          } else {
            edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
          }
        }
      }
      edge_list <- stats::na.omit(edge_list)
      node_list$term <- terms

      C <- curriculum_graph_from_list(node_list, edge_list)
      visNetwork::visNetwork(
        C$node_list,
        C$edge_list,
        submain = paste(
          "Total Structural Complexity:",
          C$sc_total,
          "Total Blocking Factor:",
          C$bf_total,
          "Total Delay Factor:",
          C$df_total
        )
      ) %>%
        visNetwork::visEdges(arrows = "to")

    }

  })

  # Output for selected nodes
  output$selectedNodes <- renderText({
    paste("Selected Nodes:",  c(
        valuesYear1$selected,
        valuesYear2$selected,
        valuesYear3$selected,
        valuesYear4$selected
      ))
  })


  output$coursestaken <- renderUI({
    if (input$dropdown == "Year 1") {
      checkboxGroupInput(
        "checkboxes1",
        "Choose Options",
        choices = c("DATA 101", "MATH 100", "MATH 101")
      )
    } else if (input$dropdown == "Year 2") {
      checkboxGroupInput(
        "checkboxes2",
        "Choose Options",
        choices = c("STAT 230", "MATH 200", "COSC 221")
      )
    } else if (input$dropdown == "Year 3") {
      checkboxGroupInput(
        "checkboxes3",
        "Choose Options",
        choices = c("DATA 301", "DATA 311", "STAT 303")
      )
    } else if (input$dropdown == "Year 4") {
      checkboxGroupInput(
        "checkboxes4",
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

  observeEvent(input$resetButton, {

    cat(length(selectedOptions))
    selectedYear <- input$dropdown
      updateCheckboxGroupInput(session, "checkboxes1", selected = character(0))
      updateCheckboxGroupInput(session, "checkboxes2", selected = character(0))
      updateCheckboxGroupInput(session, "checkboxes3", selected = character(0))
      updateCheckboxGroupInput(session, "checkboxes4", selected = character(0))

  })

}

shinyApp(ui, server)
