#' Get Top Documents
#'
#' Retrieves the top documents based on the topic distribution from a given model
#'
#' Adapted from code by André Bittermann: https://github.com/abitter/PTOS
#'
#' @param model   A topic model object containing the topic distribution (theta)
#' @param texts   A data frame representing the text data
#' @param n       An integer specifying the number of top documents to retrieve
#'                for each topic. Default is 10
#' @param text_ID A character vector representing the unique identifiers for
#'                the documents. Default is rownames of the texts
#'
#' @return        A list of data frames, each containing the top documents and
#'                their corresponding theta probabilities for each topic
#'
#' @examples
#' # topdocs <- get_topdocs(my_lda_model, my_text_data, n = 5)
#'
#' @details
#' The function extracts the topic distribution (theta) from the given model and
#' identifies the top documents for each topic based on the highest theta
#' probabilities. The result is a list of data frames, where each data frame
#' corresponds to a topic and contains the top documents and their associated
#' theta probabilities
#'
#' @export

get_topdocs <- function(model, texts, n = 10, text_ID = rownames(texts)) {

  theta <- model$theta
  K     <- lda$K

  # 10 most representative docs
  theta_tmp    <- as.data.frame(theta)
  theta_tmp$ID <- text_ID # for matching theta with texts

  topdocs <- list()

  for (i in 1:K) {
    # get row indices of top n
    tmp <- theta_tmp[order(-theta_tmp[,i]),]
    ids <- tmp$ID[1:n]

    topdocs_tmp <- texts[text_ID == ids[1],]
    for (j in 2:length(ids)) {
      topdocs_tmp <- rbind(topdocs_tmp, texts[text_ID == ids[j],])
    }

    # add theta probabilities
    topdocs_tmp$prob <- tmp[1:n,i]
    topdocs[[i]] <- topdocs_tmp
  }

  return(topdocs) # a list
}
