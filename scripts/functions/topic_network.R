#' Topic Network Visualization
#'
#' Generates a network visualization representing the relationships between
#' topics based on word distributions
#'
#' Adapted from code by André Bittermann: https://github.com/abitter/PTOS
#'
#' @param model  A topic model object containing the topic distributions
#'               (theta and phi)
#' @param thresh A numeric threshold to filter small correlations between
#'               topics. Default is 0.15
#'
#' @return       A network visualization using the visNetwork package,
#'               illustrating the relationships between topics based on the word
#'               distributions and correlations between them
#'
#' @examples
#' # Example Usage:
#' # topic_network(my_lda_model, thresh = 0.2)
#'
#' @details
#' The function extracts information from the provided topic model, including
#' document-topic probabilities (theta),' word-topic probabilities (phi), and
#' top words for each topic. It then builds a network representation of the
#' relationships between topics, considering correlations between their word
#' distributions. The resulting visualization includes nodes representing topics,
#' edges representing correlations, and community detection for better
#' interpretation.
#'
#' @seealso
#' \code{\link{topWords}}, \code{\link{cluster_louvain}}, \code{\link{visNetwork}}
#'
#' @importFrom sentopics topWords
#' @importFrom igraph graph.adjacency, cluster_louvain
#' @importFrom visNetwork visNetwork, visIgraphLayout, visNodes, visEdges,
#'             visOptions
#'
#' @export

topic_network <- function(model, thresh = 0.15){

  # The selected model and number of topics
  lda <- model
  K   <- lda$K

  # theta (document-topic probabilities)
  theta <- lda$theta
  colnames(theta) <- NULL

  # beta (word-topic probabilities) a.k.a. phi
  beta <- lda$phi
  colnames(beta) <- NULL

  # Extract the most representative words in each topic
  topwords <- sentopics::topWords(model, method = "probability", nWords = 10) %>%
    select(topic, word) %>%
    pivot_wider(names_from  = "topic",
                values_from = "word",
                values_fn   = list) %>%
    unnest(everything())

  topwords <- as.matrix(topwords)
  colnames(topwords) <- NULL
  topwords <- apply(topwords, 2, paste, collapse = ", ")

  prevalence <- colMeans(theta)

  # number of docs with theta > .5 per topic
  n_docs <- apply(theta, 2, function(x){unname(table(x > 0.5)[2])})

  # use first two top terms as initial topic labels
  label <- sapply(topwords, function(x) {paste(strsplit(x, ", ")[[1]][1:2], collapse = " ")})
  label <- unname(label)

  # add ID for disambiguation
  for (i in 1:K) {
    label[i] <- paste0("T", i, ": ", label[i])
  }

  # topic correlations
  cor_mat <- cor(beta)

  # omit small correlations to improve graph readability
  cor_mat[cor_mat < thresh] <- 0
  diag(cor_mat) <- 0 # needed for network plot

  # network of the word distributions over topics (topic relation)
  graph <- graph.adjacency(cor_mat, weighted = TRUE, mode = "lower")

  # edge labels
  edge_attr(graph, "name")  <- round(E(graph)$weight, 2)
  edge_attr(graph, "label") <- E(graph)$name

  # line thickness
  E(graph)$edge.width <- E(graph)$weight * 20

  # labels
  V(graph)$label <- label
  V(graph)$size  <- 10

  # Detect communities within the graph and add membership as vertex attribute
  # multi-level modularity optimization algorithm for finding community structure
  cd <- cluster_louvain(graph)
  V(graph)$community <- cd$membership

  # nodes
  nodes        <- as.data.frame(1:K)
  names(nodes) <- "id"
  nodes$label  <- label
  nodes$size   <- colMeans(theta)*100 # size by topic prevalence
  nodes$group  <- V(graph)$community

  # edges
  edges           <- get.data.frame(graph)
  names(edges)[3] <- "width"
  edges$label     <- as.character(round(edges$width, 2))
  edges$label     <- ifelse(nchar(edges$label) == 3, paste0(edges$label, "0"), edges$label)
  edges$width     <- edges$width * 4 # improve visibility in plot
  rbPal           <- colorRampPalette(c("grey", "cornflowerblue"))
  edges$color     <- rbPal(10)[as.numeric(cut(edges$width, breaks = 10))] # color edge label according to width

  set.seed(1234)

  visNetwork(nodes, edges) %>%
    visIgraphLayout(layout = "layout.fruchterman.reingold", physics = TRUE, smooth = TRUE) %>%

    visNodes(
      shape = "dot",
      font = list(size = 10, background = "white"),
      color = list(
        background = "#0085AF",
        border     = "#013848",
        highlight  = "#FF8000"
      ),
      shadow = list(enabled = TRUE, size = 10)
    ) %>%

    visEdges(
      label  = edges$label,
      font   = list(color = "slategray", size = 10),
      smooth = list(enabled = TRUE, type = "diagonalCross"),
      shadow = FALSE,
      color = list(color = "#0085AF", highlight = "#C62F4B", opacity = .5)
    ) %>%

    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE))
}
