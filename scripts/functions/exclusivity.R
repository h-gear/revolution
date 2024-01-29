#' Topic Exclusivity Calculation
#'
#' Calculates the exclusivity of topics using the LDAvis relevance score with
#' lambda = 0, putting emphasis on exclusivity. See Sievert, C., Shirley, K.E.
#' (2014). LDAvis: A method for visualizing and interpreting topics
#'
#' Modified from A. Bitterman
#'
#' @param lda A topic model object containing the topic distributions (phi).
#' @param dfm A Document-Frequency Matrix (DFM) object
#' @param lambda A numeric parameter controlling the trade-off between the
#' informativeness and exclusivity of topics. Default is 0
#' @param num.words An integer specifying the number of top words to consider
#' for exclusivity calculation. Default is 0 (consider all words)
#'
#' @return A numeric vector representing the exclusivity scores for each topic.
#'
#' @examples
#' # Example Usage:
#' # exclusivity(my_lda_model, my_dfm, lambda = 0, num.words = 0)
#' # exclusivity(my_lda_model, my_dfm, lambda = 0.6, num.words = 10)
#' # exclusivity(my_lda_model, my_dfm, lambda = 0.6, num.words = 20)
#'
#' @details
#' The function calculates the exclusivity of topics based on the LDAvis
#' relevance score, considering the trade-off parameter lambda.
#' Higher lambda values put more emphasis on exclusivity, and the num.words
#' parameter controls the number of top words considered for the calculation
#'
#' @seealso
#' \code{\link{lda}}, \code{\link{colSums}}, \code{\link{log}}
#'
#' @export

exclusivity <- function(lda, dfm, lambda = 0, num.words = 0){

  if (num.words == 0)
    num.words = dim(dfm)[2]

  pwt <- t(as.matrix(lda$phi))
  pw  <- colSums(dfm)/sum(dfm)
  res <- apply(pwt, 1, function(x, num.words, pw, lambda) {
    x <- lambda * log(x) + (1 - lambda) * log(x/pw)
    return((sort(x, decreasing = TRUE)[1:num.words]))
  }, num.words, pw, lambda)

  return(colMeans(res))
}
