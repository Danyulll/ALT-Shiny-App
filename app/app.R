library(CurricularAnalytics)
library(visNetwork)
library(shiny)

ui <- fluidPage(
  titlePanel("Data Science Curriculum Explorer"),
  fluidRow(
    column(4,
           wellPanel(
             h4("Courses Information"),
             selectInput("dropdown", "Select Option", choices = c("Year 1", "Year 2", "Year 3", "Year 4")),
             uiOutput("coursestaken"),
             h4("Legend"),
             uiOutput("formattedText")
           )
    ),
    column(4,
           wellPanel(
             h4("Interactive Graph"),
             visNetworkOutput("network")
           )
    ),
    column(4,
           wellPanel(
             h4("Node Control"),
             checkboxGroupInput("checkboxes", "Select Nodes", choices = c("Topics", "Predicted Grade","Course Similarity")),
             verbatimTextOutput("selectedNodes")
           )
    )
  )
)

server <- function(input, output) {
  # Values for courses taken
  checkboxValues <- reactiveValues(selected = character(0))



  # Sample text produced from code
  output$textOutput <- renderText({
    # Insert your code here to generate text
    "This is sample text. Replace with code output."
  })

  # Interactive visNetwork graph
  output$network <- renderVisNetwork({
    selectedOptions <- input$coursestaken
    if (is.null(selectedOptions)) {
      nodes <- data.frame(id = numeric(0), label = character(0))
      edges <- data.frame(from = numeric(0), to = numeric(0))
      visNetwork(nodes, edges)
    } else{
      node_list <- data.frame("id" = 1:length(input$coursestaken) ,"label" = input$coursestaken)

      df <- utils::read.csv("C:\\Users\\danie\\Desktop\\School\\ALT-Shiny-App\\data\\Example-Curriculum.csv")

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
      plot(C, width="50%")


    }

  })

  # Output for selected nodes
  output$selectedNodes <- renderText({
    paste("Selected Nodes:", input$checkboxes)
  })


  output$coursestaken <- renderUI({
    if(input$dropdown == "Year 1") {
      checkboxGroupInput("checkboxes1", "Choose Options", choices = c("DATA 101", "MATH 100", "MATH 101"), selected = checkboxValues)
    } else if(input$dropdown == "Year 2") {
      checkboxGroupInput("checkboxes2", "Choose Options", choices = c("STAT 230", "MATH 200", "COSC 221"), selected = checkboxValues)
    }else if(input$dropdown == "Year 3") {
      checkboxGroupInput("checkboxes3", "Choose Options", choices = c("DATA 301", "DATA 311", "STAT 303"), selected = checkboxValues)
    }
    else if(input$dropdown == "Year 4") {
      checkboxGroupInput("checkboxes4", "Choose Options", choices = c("DATA 405", "DATA 407", "DATA 410"), selected = checkboxValues)
    }
  })

  observeEvent(input$checkboxes, {
    checkboxValues$selected <- c(input$checkboxes1,input$checkboxes2,input$checkboxes3,input$checkboxes4)
  })

  output$formattedText <- renderUI({
   HTML( "
    df: Delay Factor - measures the degree to which failing this course will delay graduation.<br>
    bf: Blocking Factor - measures the degree to which failing this course will block access to future courses.<br>
    cf: Centrality - measures the degree to which a course is central to a curriculum and therefore contains important concepts for the entirety of the program.<br>
    sc: Structural Complexity - measures the degree to which the course or curriculum add to the difficutly of completing the program.<br>
    ")
  })

}

shinyApp(ui, server)
