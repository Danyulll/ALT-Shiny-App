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
    ) |>
      visNetwork::visEdges(arrows = "to")

  }
}

getCourses <- function(year, path) {
  df <-
    utils::read.csv(path)

  grep(paste("\\b[", year, "]\\d{2}\\b"),  df$label, value = TRUE)
}


