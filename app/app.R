##########################################################################################
# App Name: Data Science Curriculum Explorer
# Author: Daniel Krasnov
# Last Updated: 2024-02-10
#
# Description:
# This R Shiny application allows users to explore various data science courses offered
# across different academic years. Users can select courses based on their year and
# subject code to visualize course dependencies, access detailed course information,
# and receive personalized course recommendations. The app integrates with a SQLite
# database for persistent storage of user selections and utilizes various R packages
# for data manipulation, visualization, and interaction.
#
# The application aims to assist students and academic advisers in planning and
# optimizing their data science curriculum by providing insights into course
# prerequisites, recommendations based on text analysis, and an interactive graph
# representation of the course structure.
#
# Documentation for how the app works and how to change things is available in the Developer Manual.
#
# Just a couple of notes. The grade prediction functionality is a placeholder for a real model that could be added.
# As it is, it only works for one course and was just added to see what the planned app would look like.
# In this version of the app a topic model is fitted in Python in real time and the document embeddings are retrieved.
# Then the cosine similarity is taken to decide which courses to select. The model implemented is Latent Semantic Analysis.
# Using a R topic model like Latent Dirichlet Allocation would make much more sense here and remove the Python dependency.
# To overhaul this all that would have to be done is to remove all the Python code and add some logic reading in a csv of document
# embeddings and taking the distance between them.
##########################################################################################


# Load necessary libraries
library(CurricularAnalytics) # For curriculum analysis tools
library(visNetwork)          # For creating interactive network graphs
library(shiny)               # For building interactive web apps
library(reticulate)          # For interfacing with Python
library(stringr)             # For string manipulation
library(dplyr)               # For data manipulation
library(readr)               # For reading CSV files
library(DBI)                 # Database Interface for communication with databases
library(RSQLite)             # SQLite interface for R
library(randomForest)        # Random forest for placeholder model


# Source external helper functions
source("functions.R")

# Define the user interface of the Shiny app
ui <- navbarPage("DCADS V0.1",
                 tabPanel("Legend",
                          titlePanel("About the App"),
                          fluidRow(
                            column(12,
                                   h4("Welcome to the Data-driven Curriculum Analytics and Design System (DCADS)!"),
                                   p("This R Shiny application allows users to explore various data science courses offered across different academic years."),
                                   p("You can select courses based on their year and subject code to visualize course dependencies, access detailed course information, and receive personalized course recommendations."),
                                   p("Navigate to the 'Explorer' tab to start using the application."),
                                   h2("Legend"),
                                   p("This app makes use of a variety of metrics to help assess curicula in a data-driven way. At the top of the curriculum graph, and when nodes are selected, these metrics will be printed. Read below to learn more:"),
                                   HTML(
                                     "<ul>
    <li><strong>Delay Factor (df)</strong> - Measures the degree to which failing this course will delay graduation.</li>
    <li><strong>Blocking Factor (bf)</strong> - Measures the degree to which failing this course will block access to future courses.</li>
    <li><strong>Centrality (cf)</strong> - Measures the degree to which a course is central to a curriculum and therefore contains important concepts for the entirety of the program.</li>
    <li><strong>Structural Complexity (sc)</strong> - Measures the degree to which the course or curriculum adds to the difficulty of completing the program.</li>
  </ul>"
                                   )


                            )
                          )
                 ),
                 tabPanel("Explorer",
                          titlePanel("Data Science Curriculum Explorer"),
                          fluidRow(
                            column(4,
                                   # Side panel for inputs and information
                                   wellPanel(
                                     h4("Courses Information"),
                                     selectInput("dropdownYear", "Select Year", choices = c("Year 1", "Year 2", "Year 3", "Year 4")),
                                     selectInput("dropdownCourseCode", "Select Code", choices = c("MATH", "DATA", "BIOL", "CHEM", "EESC", "PHYS", "COSC", "ENGL", "APSC", "STAT", "PSYO", "PHIL")),
                                     uiOutput("coursestaken"),
                                     actionButton("resetButton", "Reset")
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
                                     tabsetPanel(
                                       tabPanel("Predicted Grade",
                                                textAreaInput("response_input", "Response Course", value = "DATA.311", rows = 1),
                                       textAreaInput("predictor_input", "Predictor Course(s)", value = "STAT.230,MATH.101,MATH.100,COSC.111", rows = 1),
                                       textAreaInput("predictor_grade_input", "Predictor Course(s) Grade(s)", value = "70,85,75,90", rows = 1),
                                       actionButton("submit_button_pred_course_grad", "Submit"),
                                       uiOutput("pred_grad_output")),
                                       tabPanel("Course Similarity",
                                                textAreaInput("text_input", "Enter Text", value = "", rows = 1),
                                                selectInput("sugYear", "Select Year", choices = c("1", "2", "3", "4", "Any")),
                                                actionButton("submit_button", "Submit"),
                                                uiOutput("courseRecSim")
                                       )
                                     )
                                   )
                            )
                          )
                 )
)

server <- function(input, output, session) {

  ############# DATABASE SECTION ##################

  # Create reactive element to check changes in database contents
  databaseContents <- reactivePoll(
    50,
    session,
    checkFunc = function() {
      file.info("my_courses_db2.sqlite")$mtime
    },
    valueFunc = function() {
      dbGetQuery(db, "SELECT * FROM selected_courses")
    }
  )

  # Connect to database
  db <-
    dbConnect(RSQLite::SQLite(), "my_courses_db2.sqlite")

  # If table does not exist create it
  query <-
    "CREATE TABLE IF NOT EXISTS selected_courses (
    id INTEGER PRIMARY KEY,
    course_name TEXT,
    year INTEGER,
    course_code TEXT)"
  dbExecute(db, query)

  # Helper function to update database based on checkbox changes
  updateDatabaseBasedOnCheckbox <-
    function(inputId, year, courseCode) {
      # Get current selections from the input
      selectedCourses <- input[[inputId]]

      # Fetch current selections from the database for this year and course code
      currentSelections <-
        dbGetQuery(
          db,
          sprintf(
            "SELECT course_name FROM selected_courses WHERE year = %d AND course_code = '%s'",
            year,
            courseCode
          )
        )

      # Determine courses to add or remove
      coursesToAdd <-
        setdiff(selectedCourses, currentSelections$course_name)
      coursesToRemove <-
        setdiff(currentSelections$course_name, selectedCourses)

      # Insert new selections
      sapply(coursesToAdd, function(course) {
        dbExecute(
          db,
          "INSERT INTO selected_courses (course_name, year, course_code) VALUES (?, ?, ?)",
          params = list(course, year, courseCode)
        )
      })

      # Remove deselected courses
      sapply(coursesToRemove, function(course) {
        dbExecute(
          db,
          "DELETE FROM selected_courses WHERE course_name = ? AND year = ? AND course_code = ?",
          params = list(course, year, courseCode)
        )
      })

      currentCourses <-
        dbGetQuery(db, "SELECT course_name FROM selected_courses WHERE year = 1")
      cat(
        "Courses selected for Year 1:",
        paste(currentCourses$course_name, collapse = ", "),
        "\n"
      )
    }

  # Observe changes to checkboxes and update the database accordingly
  observe({
    updateDatabaseBasedOnCheckbox("checkboxes1", 1, input$dropdownCourseCode)
  })

  observe({
    updateDatabaseBasedOnCheckbox("checkboxes2", 2, input$dropdownCourseCode)
  })

  observe({
    updateDatabaseBasedOnCheckbox("checkboxes3", 3, input$dropdownCourseCode)
  })

  observe({
    updateDatabaseBasedOnCheckbox("checkboxes4", 4, input$dropdownCourseCode)
  })

  # Logic for reset button to remove all courses from database
  observeEvent(input$resetButton, {
    # Reset the UI elements for all checkboxes to be unselected
    updateCheckboxGroupInput(session, "checkboxes1", selected = character(0))
    updateCheckboxGroupInput(session, "checkboxes2", selected = character(0))
    updateCheckboxGroupInput(session, "checkboxes3", selected = character(0))
    updateCheckboxGroupInput(session, "checkboxes4", selected = character(0))

    # Execute a query to delete all records from the 'selected_courses' table in the database
    dbExecute(db, "DELETE FROM selected_courses")

  })

  # We want each launch of the app to be fresh so delete all stored courses
  # If accounts are implemented this will be removed and log ins will be tracked
  onStop(function() {
    dbDisconnect(db)
    file.remove("my_courses_db2.sqlite")
  })

  ############# GRAPHING SECTION ##################
  # Render curriculum graph
  output$network <- renderVisNetwork({
    predictor_input <-
      databaseContents() # Courses are taken from database

    cat("Selected courses in graph:\n")
    cat(paste(predictor_input$course_name,sep = ","))
    cat("\n")

    # If no courses selected, display an empty graph
    if (nrow(predictor_input) == 0) {
      empty_nodes <-
        data.frame(id = numeric(0), label = character(0))
      empty_edges <-
        data.frame(from = numeric(0), to = numeric(0))
      visNetwork(empty_nodes, empty_edges)
    } else {
      courseNames <- predictor_input$course_name
      plot_graph(courseNames)
    }
  })


  ############# COURSE NAVIGATION SECTION ##################

  # Render course selection UI
  output$coursestaken <- renderUI({
    # Get the year number for the selected Year
    yearNum <-
      as.numeric(gsub("Year ", "", input$dropdownYear))

    # Fetch selected courses from the database for the current year
    selectedCourses <-
      dbGetQuery(db,
                 paste0(
                   "SELECT course_name FROM selected_courses WHERE year = ",
                   yearNum
                 ))

    # Extract the course names to a vector
    selectedCourseNames <-
      selectedCourses$course_name

    # Now use selectedCourseNames for the 'selected' parameter to maintain previosuly selected courses
    if (input$dropdownYear == "Year 1") {
      checkboxGroupInput(
        "checkboxes1",
        "Choose Options",
        choices = getCourses(
          1,
          "../data/Example-Curriculum.csv",
          input$dropdownCourseCode
        ),
        selected = selectedCourseNames
      )
    } else if (input$dropdownYear == "Year 2") {
      checkboxGroupInput(
        "checkboxes2",
        "Choose Options",
        choices = getCourses(
          2,
          "../data/Example-Curriculum.csv",
          input$dropdownCourseCode
        ),
        selected = selectedCourseNames
      )
    } else if (input$dropdownYear == "Year 3") {
      checkboxGroupInput(
        "checkboxes3",
        "Choose Options",
        choices = getCourses(
          3,
          "../data/Example-Curriculum.csv",
          input$dropdownCourseCode
        ),
        selected = selectedCourseNames
      )
    } else if (input$dropdownYear == "Year 4") {
      checkboxGroupInput(
        "checkboxes4",
        "Choose Options",
        choices = getCourses(
          4,
          "../data/Example-Curriculum.csv",
          input$dropdownCourseCode
        ),
        selected = selectedCourseNames
      )
    }
  })

  ############# COURSE PREDICTION SECTION ##################

  course_pred_data <- eventReactive(input$submit_button_pred_course_grad,{ df <-
    read.csv("..\\data\\student-data.csv")
  df_clean <- df[,c(1,12,13,15,16)]
  df_clean <- na.omit(df_clean)
  # Do some cleaning on chr columns
  df_clean$STUD_NO_ANONYMOUS <- trimws(df_clean$STUD_NO_ANONYMOUS)
  df_clean$CRS_DPT_CD <- trimws(df_clean$CRS_DPT_CD)
  df_clean$HDR_CRS_LTTR_GRD <- trimws(df_clean$HDR_CRS_LTTR_GRD)

  # Factor grades column
  grades <-
    c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F")
  df_clean$HDR_CRS_LTTR_GRD <-
    factor(df_clean$HDR_CRS_LTTR_GRD, levels = grades)

  # Create course code column
  df_clean$COURSE_CODE <- paste(df_clean$CRS_DPT_CD, df_clean$CRS_NO, sep = ".")
  df_clean <- df_clean[,-(2:3)]
  df <- subset(df, HDR_CRS_PCT_GRD < 999)
  df_clean <- subset(df_clean, HDR_CRS_PCT_GRD < 999)
  df_clean})

  # Initialize a reactive value to control UI display
  values <- reactiveValues(ready = FALSE)

  observeEvent(input$submit_button_pred_course_grad, {
    cat("Loading data for grade prediction.\n")
    data <- course_pred_data()  # Loading your data
    cat("Data loaded.\n")

    # Handling user inputs
    response <- input$response_input
    predictor_input <- input$predictor_input
    predictor_input <- trimws(unlist(strsplit(predictor_input, ",")))
    predictor_input <- c(predictor_input, response)
    cat("Here is the predictor input:\n")
    cat(predictor_input)
    cat("\n")

    cat("Here is the response input:\n")
    cat(response)
    cat("\n")

    # Preparing data for prediction
    empty <- setNames(as.data.frame(matrix(nrow = 1, ncol = length(predictor_input), data = NA)), predictor_input)

    # This is just for the conference, change this later
    if(sum(c("DATA.311","STAT.230","MATH.101","COSC.111","MATH.100") %in% predictor_input) == 5){
      load("../data/grade.RData")
      preds <- format_data_no_fail_handling.out
      cat("Empty loaded sucessfully\n")
    }else{
      preds <- format_data_no_fail_handling(empty, predictor_input, data)
    }

    # Running the random forest model
    cat("Starting rf\n")
    rf.out <- randomForest(as.formula(paste0(response,"~.")), data=preds)

    # Extracting importance and determining the most important course
    impor <- importance(rf.out)
    most_important_course <- rownames(impor)[which.max(impor)]

    # Preparing prediction input data
    pred_courses_scores <- input$predictor_grade_input
    pred_courses_scores <- as.numeric(unlist(strsplit(pred_courses_scores, ",")))
    out_mat <- matrix(pred_courses_scores)
    rownames(out_mat) <- predictor_input[predictor_input != response]
    out_mat <- t(out_mat)
    out_df <- as.data.frame(out_mat)

    # Predicting course grades
    predict.out <- predict(rf.out, newdata=out_df)

    # Update reactive values
    output$predicted_grades <- renderTable({
      data.frame(Course = names(predict.out), Predicted_Grade = as.numeric(predict.out))
    })

    output$most_important_course_output <- renderText({
      most_important_course
    })

    # Set reactive value to TRUE to indicate that processing is complete and UI should update
    values$ready <- TRUE
  })

  # Render UI components conditionally based on the reactive value
  output$pred_grad_output <- renderUI({
    if (values$ready) {
      tagList(
        h4("Predicted Course Grades:"),
        tableOutput("predicted_grades"),
        h4("Most Important Course for Prediction:"),
        textOutput("most_important_course_output")
      )
    }
  })



  ############# COURSE RECOMMENDATION SECTION ##################

  # Create reactive course suggestion data for topic model
  reactive_data <-
    eventReactive(input$submit_button, {
      ti <- input$text_input
      sg <- input$sugYear

      if (!is.null(ti) &
          !is.null(sg) & nchar(ti) > 0) {
        cat("Attempting to create Python environment.\n")
        use_or_create_env()

        df <-
          read_csv(
            "..\\data\\UBCO_Course_Calendar.csv",
            locale = locale(encoding = "ISO-8859-1")
          ) |>
          filter(!is.na(`Course Description`))


        cat("Staring doc sim.\n")
        lsaDocSim.out <- lsaDocSim(ti, sg, df)
        # cat("Doc sim finished.\n")
        lsaDocSim.out
      }
    }, ignoreNULL = FALSE)

  # Render the course recommendation UI
  output$courseRecSim <- renderUI({
    cat("Loading course suggestion data for display.\n")
    data <- reactive_data()
    cat("Data loaded.\n")
    if (!is.null(data)) {
      tagList(
        tableOutput("table_view"),
        checkboxInput(
          "checkbox1rec",
          paste(data$Course.Name[1], data$Course.Code[1]),
          value = FALSE
        ),
        checkboxInput(
          "checkbox2rec",
          paste(data$Course.Name[2], data$Course.Code[2]),
          value = FALSE
        ),
        checkboxInput(
          "checkbox3rec",
          paste(data$Course.Name[3], data$Course.Code[3]),
          value = FALSE
        )
      )
    }
  })




  # Dataframe to otuput for course sugesstion topic model
  output$table_view <- renderTable({
    reactive_data()
  })


  # Logic to handle adding suggested courses from topic model
  observeEvent(input$checkbox1rec, {
    data <- reactive_data()
    course_name <- data$Course.Name[1]
    course_code <- data$Course.Code[1]
    year <- data$Course.Code |> substr(1, 1) |> as.numeric()

    cat("This is what wants to be added and deleted from the database thanks to suggestions 1\n")
    cat("\n")
    cat(paste(course_name, course_code))
    cat("\n")
    cat(course_code)
    cat("\n")
    cat(year[1])
    cat("\n")

    if (input$checkbox1rec) {
      # Insert the course into the database if checked
      query <-
        sprintf(
          "INSERT INTO selected_courses (course_name, course_code, year) VALUES ('%s', '%s', %d)",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
      dbExecute(db, query)
    } else {
      # Remove the course from the database if unchecked
      query <-
        sprintf(
          "DELETE FROM selected_courses WHERE course_name = '%s' AND course_code = '%s' AND year = %d",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
      dbExecute(db, query)
    }
  })

  observeEvent(input$checkbox2rec, {
    data <- reactive_data()
    course_name <- data$Course.Name[2]
    course_code <- data$Course.Code[2]
    year <- data$Course.Code |> substr(1, 1) |> as.numeric()

    cat("This is what wants to be added and deleted from the database thanks to suggestions 2\n")
    cat("\n")
    cat(paste(course_name, course_code))
    cat("\n")
    cat(course_code)
    cat("\n")
    cat(year[1])
    cat("\n")

    if (input$checkbox2rec) {
      # Insert the course into the database if checked
      query <-
        sprintf(
          "INSERT INTO selected_courses (course_name, course_code, year) VALUES ('%s', '%s', %d)",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
      dbExecute(db, query)
    } else {
      # Remove the course from the database if unchecked
      query <-
        sprintf(
          "DELETE FROM selected_courses WHERE course_name = '%s' AND course_code = '%s' AND year = %d",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
      dbExecute(db, query)
    }
  })

  observeEvent(input$checkbox3rec, {
    data <- reactive_data()
    course_name <- data$Course.Name[3]
    course_code <- data$Course.Code[3]
    year <- data$Course.Code |> substr(1, 1) |> as.numeric()

    cat("This is what wants to be added and deleted from the database thanks to suggestions 1\n")
    cat("\n")
    cat(paste(course_name, course_code))
    cat("\n")
    cat(course_code)
    cat("\n")
    cat(year[1])
    cat("\n")

    if (input$checkbox3rec) {
      # Insert the course into the database if checked
      query <-
        sprintf(
          "INSERT INTO selected_courses (course_name, course_code, year) VALUES ('%s', '%s', %d)",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
      dbExecute(db, query)
    } else {
      # Remove the course from the database if unchecked
      query <-
        sprintf(
          "DELETE FROM selected_courses WHERE course_name = '%s' AND course_code = '%s' AND year = %d",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
      dbExecute(db, query)
    }
  })


}


# Launch shiny app
shinyApp(ui, server)

