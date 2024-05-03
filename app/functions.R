library(CurricularAnalytics)
library(visNetwork)
library(reticulate)
library(stringr)
library(dplyr)
library(readr)
library(igraph)

plot_graph <- function(selectedOptions) {
  path <-
    "..\\data\\Example-Curriculum.csv"
  df <-
    utils::read.csv(path)

  df <- df[which(df$label %in% selectedOptions), ]

  if(is.null(selectedOptions)) warning("selectedOptions is null (plot_graph)")

  if (length(selectedOptions) %in% 0) {
    # Currently if this condition just checks 0 when you uncheck all boxes it will still display 1 node despite the checkboxes vector being 0. This is because of how selectedOptions is updated, its length stays at 1. Need to learn how to force update SelecredOptions so that it goes to length 0 when all checkboxes are unchecked.
    nodes <- data.frame(id = numeric(0), label = character(0))
    edges <- data.frame(from = numeric(0), to = numeric(0))
    visNetwork(nodes, edges)
  } else if (length(selectedOptions) %in% 1) {
    node_list <-
      data.frame(id = 1,
                 label = df$label,
                 term = df$term)
    edge_list <- data.frame(from = integer(), to = integer())
    c <-
      curriculum_graph_from_list(node_list = node_list, edge_list = edge_list)
    plot(c)

  } else{

    # cat(paste0("SelectedOptions contents in else of plot_graph: ",paste0(selectedOptions,collapse = ",")))

    # df[is.na(df)] <- ""



    node_list <-
      data.frame(id = df$id,
                 label = df$label,
                 term = df$term)

    db_ids <- df[which(df$label %in% selectedOptions), ]$id
    app_ids <- 1:length(db_ids)

    map <- setNames(object = app_ids, nm = db_ids)


    edge_list <- data.frame(from = NA, to = NA)
    for (node_id in node_list$id) {
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

    for (i in 1:nrow(edge_list)) {
     if(length(edge_list$from[i] %in% names(map))==0){
       next()
     }else{
       if(is.null(edge_list$to[i] %in% names(map))) cat("Error 1 something is null\n")
       if(length(edge_list$from[i] %in% names(map))==0) cat("Error 2 0 length argument\n")

       if (edge_list$from[i] %in% names(map)) {
         edge_list$from[i] <-  map[which(names(map) %in%  edge_list$from[i])]
       }
     }
    }

    for (i in 1:nrow(edge_list)) {
      cat(length(edge_list$to[i] %in% names(map) == 0))
      cat("\n")
      if (length(edge_list$to[i] %in% names(map) == 0) == 0) {
        next
      } else{
        if (edge_list$to[i] %in% names(map)) {
          edge_list$to[i] <-  map[which(names(map) %in%  edge_list$to[i])]
        }
      }

    }


    node_list$id <- rownames(node_list) |> as.numeric()
    c <-
      curriculum_graph_from_list(node_list = node_list, edge_list = edge_list)
    plot(c)

  }
}

getCourses <- function(year, path, subject) {
  df <-
    utils::read.csv(path)

  res <- grep(paste("\\b[", year, "]\\d{2}\\b"),  df$label, value = TRUE)
  res <- res[substr(res, 1, 4) == subject]
  res
}

plot.curriculum_graph <- function(curriculum_graph, width = "100%", height = 500) {
  curriculum_graph$node_list <-
    curriculum_graph$node_list[order(curriculum_graph$node_list$term),]
  # coords <- generate_coords(curriculum_graph)
  visNetwork::visNetwork(
    curriculum_graph$node_list,
    curriculum_graph$edge_list,
    width = width,
    height = height,
    submain = paste(
      "Total Structural Complexity:",
      curriculum_graph$sc_total,
      "Total Blocking Factor:",
      curriculum_graph$bf_total,
      "Total Delay Factor:",
      curriculum_graph$df_total
    )
  ) %>%
    visNetwork::visEdges(arrows = "to") |>
    visNetwork::visEvents(
      selectNode = "function(properties) {
      alert(' sc: ' + this.body.data.nodes.get(properties.nodes[0]).sc + ' cf: ' + this.body.data.nodes.get(properties.nodes[0]).cf + ' bf: ' + this.body.data.nodes.get(properties.nodes[0]).bf + ' df: ' + this.body.data.nodes.get(properties.nodes[0]).df);}"
    )
}


################
# Testing area for reticulate


# Define the nltk_pipeline function in R
nltk_pipeline <- function(input_string) {
  # Create tokenizer and stemmer instances
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

  tokenizer <- RegexpTokenizer('\\w+')
  nltk_stem <- import("nltk.stem")
  PorterStemmer <- nltk_stem$PorterStemmer
  stemmer <- PorterStemmer()

  # cat("stuff loaded again")

  # Tokenize input string and remove punctuation
  tokens <- tokenizer$tokenize(input_string)
  # cat("tokenize")
  # Remove tokens containing numbers
  tokens <- Filter(function(token) !grepl('\\d', token), tokens)
  # cat("filter tokens")
  #Convert tokens to lowercase and remove stopwords
  stop_words_set <- stopwords$words("english")
  custom_stop_words_set <- unique(c("ii", "i", "e", "g", "official", "calendar", "first", "year", "second", "third", "fourth", "fall", "spring", "summer", "winter", "credit", "granted"))
  stop_words_combined <- union(stop_words_set, custom_stop_words_set)
  filtered_tokens <- Filter(function(token) !(tolower(token) %in% stop_words_combined), tokens)
  # cat("filter out stop words")
    # Perform stemming on filtered tokens
  stemmed_tokens <- sapply(filtered_tokens, function(token) stemmer$stem(token))
  # cat("stem")
  bigram_freq <- FreqDist(bigrams(stemmed_tokens))

  # I had to remove the bigram functionality for the topic model due to werid Python to R conversions that crash the app. It would be nice to fix this.
  # names(which(unlist(bigram_freq) >= 2)) |> str_extract_all( "\\b\\w+\\b") -> frequent_bigrams
  # frequent_bigrams <- paste(unlist(frequent_bigrams),collapse = "_")
  # cat("bigram stuff")

  final_tokens <- c(stemmed_tokens) #, frequent_bigrams[[1]]
  token_counts <- Counter(final_tokens)
  filtered_final_tokens <- Filter(function(token) token_counts[[token]] > 0, final_tokens)
  return(paste(filtered_final_tokens, collapse=' '))
}

# Define the lsaDocSim function in R
lsaDocSim <- function(query_course, year,df) {

  # Clean data
  clean_text <- sapply(df$`Course Description`, nltk_pipeline)

  cat("Data cleaned")

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

  tokenizer <- RegexpTokenizer('\\w+')
  nltk_stem <- import("nltk.stem")
  PorterStemmer <- nltk_stem$PorterStemmer
  stemmer <- PorterStemmer()

  vectorizer <- CountVectorizer(min_df=1L)
  dtm <- vectorizer$fit_transform(clean_text)

  tfidf_transformer <- TfidfTransformer()
  tfidf_dtm <- tfidf_transformer$fit_transform(dtm)


  lsa <- TruncatedSVD(4L, algorithm='randomized')
  dtm_lsa <- lsa$fit_transform(tfidf_dtm)

  dtm_lsa <- Normalizer()$fit_transform(dtm_lsa)

  doc_embeddings <- np$asmatrix(pd$DataFrame(dtm_lsa))
  other_info <- np$asmatrix(df[7:ncol(df)])

  row_norms <- np$linalg$norm(other_info, axis=1L)
  normalized_matrix <- sweep(other_info, 1, row_norms, FUN="/")

  mat <- np$concatenate(list(normalized_matrix, doc_embeddings), axis=1L)
  doc_sim <- mat %*% t(mat)

  cat("doc_sim made\n")

  df$ID <- seq_len(nrow(df))
  # map <- df[,c('Course Code','ID')]$set_index('Course Code')$to_dict(orient='dict')['ID']
  # TODO check that map is converted right
  map <- df %>%
    select(`Course Code`, ID) %>%
    distinct() %>%
    # Convert to a simple data frame
    as.data.frame(stringsAsFactors = FALSE)

  # Create a named vector that maps 'Course Code' to 'ID'
  map_vector <- setNames(map$ID, map$`Course Code`)

  index <- map_vector[names(map_vector)==query_course]

  sorted_indices <- order(doc_sim[index,])[nrow(doc_sim):1]
  sorted_values <- as.array(doc_sim[index, sorted_indices])
  sorted_values <- sorted_values / max(sorted_values)



  df_out <- data.frame(
    'Course Code' = substr(names(map_vector[sorted_indices]),6,9),
    'Course Name' = substr(names(map_vector[sorted_indices]),1,4),
    'Similarity' = sorted_values
  )

  if(year %in% 1:4){
    indices <- which(df_out$Course.Code |> substr(1,1) |>as.numeric() == year)
    df_out <- df_out[indices,]
  }

  cat("Returning result\n")
  return(df_out[2:4, ])
}

create_python_env <- function(env_name="shiny_app") {
  # Load reticulate library
  library(reticulate)

  # Create a new virtual environment
  virtualenv_create(env_name)

  # Install necessary Python packages in the created environment
  packages_to_install <- c("numpy", "pandas", "nltk", "scikit-learn")
  py_install(packages=packages_to_install, envname=env_name)

  # Download additional NLTK data (stopwords, etc.)
  py_run_string("import nltk; nltk.download('stopwords'); nltk.download('punkt')")

  # Activate the environment
  use_virtualenv(env_name, required = TRUE)

  cat("Python environment", env_name, "is set up and ready to use.")
}

use_or_create_env <- function(env_name="shiny_app") {
  tryCatch(
    {
      # Try to use the virtual environment
      reticulate::use_virtualenv(env_name, required = TRUE)
      cat("Successfully activated the Python environment:", env_name, "\n")
    },
    error = function(e) {
      if(is_python_installed()){
        cat("Installed already")
      }else{
        reticulate::install_python()
      }
      # If an error occurs, create and activate the environment
      cat("Error in activating environment. Creating a new environment:", env_name, "\n")
      create_python_env(env_name)
      reticulate::use_virtualenv(env_name, required = TRUE)
      cat("Successfully created and activated the Python environment:", env_name, "\n")
    }
  )
}

is_python_installed <- function() {
  py_config <- tryCatch({
    py_discover_config()
  }, error = function(e) {
    return(NULL)
  })

  return(!is.null(py_config$python))
}

format_data_no_fail_handling <- function(empty, courses, df_whole) {

  df_whole |> filter(COURSE_CODE %in% courses) |> pull(STUD_NO_ANONYMOUS) |> unique() -> ids

  df_filtered <-
    data.frame(
      "STUD_NO_ANONYMOUS" = NA, "HDR_CRS_PCT_GRD" = NA,   "HDR_CRS_LTTR_GRD" = NA,  "COURSE_CODE" = NA
    )

  for (id in ids) {
    courses_taken <- subset(df_whole, STUD_NO_ANONYMOUS == id)$COURSE_CODE
    if (all(courses %in% courses_taken)) {
      for (course in courses) {
        df_filtered <-
          rbind(df_filtered,
                subset(df_whole, STUD_NO_ANONYMOUS == id &
                         COURSE_CODE == course))
      }
    }
  }
  df_filtered <- na.omit(df_filtered)
  df_filtered$STUD_NO_ANONYMOUS |> unique() -> filtered_ids

  length(filtered_ids)

  for (id in filtered_ids) {
    maj <-
      subset(df_whole, STUD_NO_ANONYMOUS == id)$CURR_SPEC_PRIM_SUBJECT_1[length(subset(df_whole, STUD_NO_ANONYMOUS == id)$CURR_SPEC_PRIM_SUBJECT_1)]

    df_filt <-
      subset(df_whole, STUD_NO_ANONYMOUS == id)[, c("HDR_CRS_PCT_GRD", "COURSE_CODE")]
    df_filt <- df_filt[df_filt$COURSE_CODE %in% courses,]
    df_filt <- data.frame(t(df_filt))

    # Take highest grade for repeat courses
    if (any(duplicated(as.vector(df_filt[2,])))) {
      dups_indicies <- numeric(2)
    } else{
      dups_indicies <- numeric(0)
    }

    while (length(dups_indicies) > 1) {
      dups_indicies <- which(duplicated(as.vector(df_filt[2,])))
      index <- dups_indicies[1]

      # Get repeated course
      repeat_course <- df_filt[2, index]
      repeat_course_df <-
        df_filt[, which(df_filt[2,] == repeat_course)]

      # Get best grade of repeated course
      best_idx <-
        which.max(repeat_course_df[1,])
      best_grade <- repeat_course_df[, best_idx]

      # drop repeated course in df
      filter <- (as.vector(df_filt[2,]) == repeat_course)
      trues <- which(filter == TRUE)
      df_filt <- df_filt[,-trues]

      # attach best score of repeated course
      df_filt <- cbind(df_filt, best_grade)
    }

    cols <- df_filt[2,]

    if (nrow(data.frame(df_filt[1, ])) > 1) {
      df_filt <- t(data.frame(df_filt[1, ]))
    } else{
      df_filt <- data.frame(df_filt[1, ])
    }

    colnames(df_filt) <- cols

    rownames(df_filt) <- ""

    if (sum(colnames(empty) %in% colnames(df_filt)) == ncol(empty)) {
      empty <- rbind(empty, df_filt)
    }
  }

  empty <- na.omit(empty)
  empty <- data.frame(sapply(empty, as.numeric))
  empty
}

