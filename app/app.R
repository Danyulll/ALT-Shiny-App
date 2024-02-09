library(CurricularAnalytics)
library(visNetwork)
library(shiny)
library(reticulate)
library(stringr)
library(dplyr)
library(readr)

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
                           tabsetPanel(
                             id = "course_recommendations",
                             tabPanel("Topics",
                                      # Add content for Topics here
                             ),
                             tabPanel("Predicted Grade",
                                      # Add content for Predicted Grade here
                             ),
                             tabPanel("Course Similarity",
                                      textAreaInput("text_input", "Enter Text", value = "", rows = 1),
                                      selectInput(
                                        "sugYear",
                                        "Select Year",
                                        choices = c("1", "2", "3", "4","Any")
                                      ),
                                      actionButton("submit_button", "Submit"),
                                      uiOutput("courseRecSim")
                             )
                           ),

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
        )
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


  reactive_data <- eventReactive(input$submit_button, {
    ti <- input$text_input
    sg <- input$sugYear

    if(!is.null(ti) & !is.null(sg) & nchar(ti) > 0){
      # Call your function to generate the table data
      use_or_create_env()
      df <- read_csv("C:\\Users\\danie\\Desktop\\School\\ALT-Shiny-App\\data\\UBCO_Course_Calendar.csv", locale = locale(encoding = "ISO-8859-1")) %>%
        filter(!is.na(`Course Description`))


      cat("Staring doc sim\n")
      np <- import("numpy")
      pd <- import("pandas")

      nltk_corpus <- import("nltk.corpus")
      stopwords <- nltk_corpus$stopwords

      nltk_tokenize <- import("nltk.tokenize")
      RegexpTokenizer <- nltk_tokenize$RegexpTokenizer

      nltk_stem <- import("nltk.stem")
      PorterStemmer <- nltk_stem$PorterStemmer

      nltk <- import("nltk")
      FreqDist <- nltk$FreqDist
      bigrams <- nltk$bigrams

      collections <- import("collections")
      Counter <- collections$Counter

      sklearn_decomposition <- import("sklearn.decomposition")
      TruncatedSVD <- sklearn_decomposition$TruncatedSVD

      sklearn_feature_extraction_text <- import("sklearn.feature_extraction.text")
      TfidfTransformer <- sklearn_feature_extraction_text$TfidfTransformer
      CountVectorizer <- sklearn_feature_extraction_text$CountVectorizer

      sklearn_preprocessing <- import("sklearn.preprocessing")
      Normalizer <- sklearn_preprocessing$Normalizer

      cat("Packages loaded\n")


      # You may think this looks bad, and I'd agree but I challenge you to get reticulate to work when these imports are in functions.R
      # TODO seriosuly tho fix this
      lsaDocSim(
        ti,
        ifelse(is.na(as.numeric(sg)), 0, as.numeric(sg)),
        df,
        np,
        pd,
        nltk_corpus,
        stopwords,
        nltk_tokenize,
        RegexpTokenizer ,
        nltk_stem,
        PorterStemmer,
        nltk,
        FreqDist,
        bigrams,
        collections,
        Counter,
        sklearn_decomposition,
        TruncatedSVD,
        sklearn_feature_extraction_text,
        TfidfTransformer,
        CountVectorizer,
        sklearn_preprocessing,
        Normalizer
      )
    }
  }, ignoreNULL = FALSE)

  output$courseRecSim <- renderUI({
    data <- reactive_data()
    if(!is.null(data)){
      tagList(
        tableOutput("table_view"),
        checkboxInput("checkbox1rec", paste(data$Course.Name[1],data$Course.Code[1]), value = FALSE),
        checkboxInput("checkbox2rec", paste(data$Course.Name[2],data$Course.Code[2]), value = FALSE),
        checkboxInput("checkbox3rec", paste(data$Course.Name[3],data$Course.Code[3]), value = FALSE)
      )
    }
  })

  output$table_view <- renderTable({
    reactive_data()
  })

  observeEvent(input$checkbox1rec, {
    data <- reactive_data()
    course_id <- paste(data$Course.Name[1], data$Course.Code[1])

    if (input$checkbox1rec) {
      # If the checkbox is checked, add the course
      if (!(course_id %in% valuesYear1$selected)) {
        valuesYear1$selected <- unique(c(valuesYear1$selected, course_id))
      }
    } else {
      # If the checkbox is unchecked, remove the course
      valuesYear1$selected <- setdiff(valuesYear1$selected, course_id)
    }
  })

  observeEvent(input$checkbox2rec, {
    data <- reactive_data()
    course_id <- paste(data$Course.Name[2], data$Course.Code[2])

    if (input$checkbox2rec) {
      # If the checkbox is checked, add the course
      if (!(course_id %in% valuesYear1$selected)) {
        valuesYear1$selected <- unique(c(valuesYear1$selected, course_id))
      }
    } else {
      # If the checkbox is unchecked, remove the course
      valuesYear1$selected <- setdiff(valuesYear1$selected, course_id)
    }
  })

  observeEvent(input$checkbox3rec, {
    data <- reactive_data()
    course_id <- paste(data$Course.Name[3], data$Course.Code[3])

    if (input$checkbox3rec) {
      # If the checkbox is checked, add the course
      if (!(course_id %in% valuesYear1$selected)) {
        valuesYear1$selected <- unique(c(valuesYear1$selected, course_id))
      }
    } else {
      # If the checkbox is unchecked, remove the course
      valuesYear1$selected <- setdiff(valuesYear1$selected, course_id)
    }
  })


}





shinyApp(ui, server)
