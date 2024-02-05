#' Part-of-Speech tagging
#'
#' @description
#'`pos_tag` is a function that takes a vector of texts and
#' returns a vector of texts with part-of-speech tags. The default
#' is to return only nouns.
#'
#' The function is modified from: https://tm4ss.github.io/docs/Tutorial_8_NER_POS.html
#' and adapted from code by André Bittermann: https://github.com/abitter/PTOS
#'
#' The function is designed to apply part-of-speech tagging to a corpus using
#' the openNLP package. It constructs a pipeline of annotators, including
#' sentence tokenization, word tokenization, and part-of-speech tagging.
#' The function then filters the results based on the specified part-of-speech
#' tags (pos_filter).
#'
#' @section details:
#' The openNLP package relies on the rjava package. For this to work properly,
#' you need a version of Java installed (e.g. open-jdk) which matches your
#' R-version, either the 32- or 64-bit installation.

#' Also the JAVA_HOME environment variable needs to be set, pointing to your
#' Java installation directory. This can be done in R using the following
#' command: Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_121') (replace
#' the path with the path to your Java installation directory). Alternatively,
#' you can set the JAVA_HOME environment variable in your operating system.
#'
#' @param x this function expects x as a vector of texts
#' @param pos_filter a vector of part-of-speech tags to filter for
#'  nouns are default
#' @returns description a vector of texts with part-of-speech tags, based on the
#' selection of the pos_filter argument (e.g., nouns only)
#' @examples
#' sentence <- c("I am a sentence.", "I am another sentence.")
#' pos_tag(x = sentence, pos_filter = c("NNP"))
#' @export
#'
pos_tag <- function(x, pos_filter = c("NNP", "NNPS", "NN", "NNS")) {

  options(stringsAsFactors = FALSE)
  library(quanteda)
  library(NLP)

  # Create corpus object
  text_corpus <- corpus(x)

  require(openNLP)
  require(openNLPdata)

  # openNLP annotator objects
  sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator    <- Maxent_POS_Tag_Annotator()
  annotator_pipeline   <- Annotator_Pipeline(
    sent_token_annotator,
    word_token_annotator,
    pos_tag_annotator
  )

  # Function for annotation
  annotateDocuments <- function(doc, pos_filter = NULL) {
    doc <- as.String(doc)

    tryCatch({
      doc_with_annotations <- NLP::annotate(doc, annotator_pipeline)

      tags   <- sapply(subset(doc_with_annotations, type == "word")$features, `[[`, "POS")
      tokens <- doc[subset(doc_with_annotations, type == "word")]

      if (!is.null(pos_filter)) {
        res <- tokens[tags %in% pos_filter]
      } else {
        res <- paste0(tokens, "_", tags)
      }
      res <- paste(res, collapse = " ")
      return(res)
    }, error = function(e) {
      # Print the error and the row where it occurred
      cat("Error in row:", which(as.character(text_corpus) == doc), "\n")
      stop(e)
    })
  }

  # Return filtered corpus
  return(sapply(as.character(text_corpus), annotateDocuments, pos_filter = pos_filter))
}
