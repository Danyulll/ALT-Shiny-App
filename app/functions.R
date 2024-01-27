library(CurricularAnalytics)
library(visNetwork)


plot_graph <- function(selectedOptions, path){

  if (length(selectedOptions) %in% 0) { # Currently if this condition just checks 0 when you uncheck all boxes it will still display 1 node despite the checkboxes vector being 0. This is because of how selectedOptions is updated, its length stays at 1. Need to learn how to force update SelecredOptions so that it goes to length 0 when all checkboxes are unchecked.
    nodes <- data.frame(id = numeric(0), label = character(0))
    edges <- data.frame(from = numeric(0), to = numeric(0))
    visNetwork(nodes, edges)
  }else{

    node_list <-
      data.frame(
        "id" = 1:length(selectedOptions) ,
        "label" = selectedOptions
      )

    df <-
      utils::read.csv(
        path
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
    plot.curriculum_graph(C)
    # visNetwork::visNetwork(
    #   C$node_list,
    #   C$edge_list,
    #   submain = paste(
    #     "Total Structural Complexity:",
    #     C$sc_total,
    #     "Total Blocking Factor:",
    #     C$bf_total,
    #     "Total Delay Factor:",
    #     C$df_total
    #   )
    # ) |>
    #   visNetwork::visEdges(arrows = "to") |>
    #   visNetwork::visEvents(
    #     selectNode = "function(properties) {
    #   alert(' sc: ' + this.body.data.nodes.get(properties.nodes[0]).sc + ' cf: ' + this.body.data.nodes.get(properties.nodes[0]).cf + ' bf: ' + this.body.data.nodes.get(properties.nodes[0]).bf + ' df: ' + this.body.data.nodes.get(properties.nodes[0]).df);}"
    #   )

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
library(reticulate)
library(stringr)
library(dplyr)
library(readr)
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

np <- import("numpy")
pd <- import("pandas")

# Define the nltk_pipeline function in R
nltk_pipeline <- function(input_string) {
  # Create tokenizer and stemmer instances
  tokenizer <- RegexpTokenizer('\\w+')
  stemmer <- PorterStemmer()

  # Tokenize input string and remove punctuation
  tokens <- tokenizer$tokenize(input_string)
  # Remove tokens containing numbers
  tokens <- Filter(function(token) !grepl('\\d', token), tokens)
  # Convert tokens to lowercase and remove stopwords
  stop_words_set <- stopwords$words("english")
  custom_stop_words_set <- unique(c("ii", "i", "e", "g", "official", "calendar", "first", "year", "second", "third", "fourth", "fall", "spring", "summer", "winter", "credit", "granted"))
  stop_words_combined <- union(stop_words_set, custom_stop_words_set)
  filtered_tokens <- Filter(function(token) !(tolower(token) %in% stop_words_combined), tokens)
  # Perform stemming on filtered tokens
  stemmed_tokens <- sapply(filtered_tokens, function(token) stemmer$stem(token))
  bigram_freq <- FreqDist(bigrams(stemmed_tokens))
  names(which(unlist(bigram_freq) >= 2)) |> str_extract_all( "\\b\\w+\\b") -> frequent_bigrams
  frequent_bigrams <- paste(unlist(frequent_bigrams),collapse = "_")
  final_tokens <- c(stemmed_tokens, frequent_bigrams[[1]])
  token_counts <- Counter(final_tokens)
  filtered_final_tokens <- Filter(function(token) token_counts[[token]] > 0, final_tokens)
  return(paste(filtered_final_tokens, collapse=' '))
}

# Define the lsaDocSim function in R
lsaDocSim <- function(query_course, year) {
  df <- read_csv("./data/UBCO_Course_Calendar.csv", locale = locale(encoding = "ISO-8859-1")) %>%
    filter(!is.na(`Course Description`))

  # Clean data
  clean_text <- sapply(df$`Course Description`, nltk_pipeline)

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

  return(df_out[1:3, ])
}


