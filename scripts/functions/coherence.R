#' Topic Coherence Calculation
#'
#' Calculates the coherence of topics based on the method proposed by Mimno et
#' al. (2011).
#'
#' Modified from "coherence_LDAproto function" by A. Bitterman.
#'
#' Based on Mimno, D., Wallach, H. M., Talley, E., Leenders, M., & McCallum, A.
# (2011). Optimizing semantic coherence in topic models. In: Proceedings of
# the Conference on Empirical Methods in Natural Language Processing
# (pp. 262-272). Association for Computational Linguistics. Chicago
#'
#' @param model A topic model object containing the topic distributions (phi).
#' @param DTM A Document-Term Matrix (DTM) or equivalent in Matrix or slam format.
#' @param N An integer specifying the number of top words to consider for
#' coherence calculation. Default is 10.
#'
#' @return A numeric vector representing the coherence scores for each topic.
#'
#' @examples
#' # Example Usage:
#' # coherence(my_lda_model, my_dtm, N = 10)
#'
#' @details
#' The function calculates the coherence of topics based on the method proposed
#'  by Mimno et al. (2011). It considers the co-occurrence of words within
#'  topics to measure the quality and interpretability of the topics.
#'
#' @seealso
#' \code{\link{lda}}, \code{\link{colSums}}, \code{\link{log}},
#' \code{\link{sentopics::topWords}}
#'
#' @importFrom Matrix sparseMatrix
#' @importFrom slam is.simple_triplet_matrix
#' @importFrom sentopics topWords, pivot_wider, unnest
#'
#' @export

coherence <- function(model, DTM, N = 10) {

  # Ensure matrix or Matrix-format (convert if slam)
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(DTM)) {
    DTM <- sparseMatrix(i = DTM$i, j = DTM$j, x = DTM$v,
                        dims = c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }

  K <- model$K

  DTMBIN <- DTM > 0

  documentFrequency <- colSums(DTMBIN)
  names(documentFrequency) <- colnames(DTMBIN)

  topNtermsPerTopic <- sentopics::topWords(model, nWords = 10) %>%
    select(topic,word) %>%
    pivot_wider(names_from  = "topic",
                values_from = "word",
                values_fn   = list) %>%
    unnest(everything())

  topNtermsPerTopic <- as.matrix(topNtermsPerTopic)
  allTopicModelTerms <- unique(as.vector(topNtermsPerTopic))

  DTMBIN     <- DTMBIN[, allTopicModelTerms]
  DTMBINCooc <- t(DTMBIN) %*% DTMBIN
  DTMBINCooc <- t((DTMBINCooc + 1) / colSums(DTMBIN))
  DTMBINCooc <- log(DTMBINCooc)
  DTMBINCooc <- as.matrix(DTMBINCooc)

  coherence  <- rep(0, K)
  pb <- txtProgressBar(max = K)

  for (topicIdx in 1:K) {
    setTxtProgressBar(pb, topicIdx)
    topWordsOfTopic <- topNtermsPerTopic[,topicIdx]

    coherence[topicIdx] <- 0
    for (m in 2:length(topWordsOfTopic)) {
      for (l in 1:(m - 1)) {
        mTerm <- as.character(topWordsOfTopic[m])
        lTerm <- as.character(topWordsOfTopic[l])
        coherence[topicIdx] <- coherence[topicIdx] + DTMBINCooc[mTerm, lTerm]
      }
    }
  }
  close(pb)

  return(coherence)
}
