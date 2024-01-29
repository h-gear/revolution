#' Document-Frequency Matrix Trim Plot
#'
#' Generates a plot similar to plotRemoved in the stm package, illustrating the
#' impact of different minimal document or term frequencies on vocabulary size
#' and the number of documents dropped.
#'
#' Adapted from code by André Bittermann: https://github.com/abitter/PTOS
#'
#' @param DFM A Document-Frequency Matrix (DFM) object
#' @param s1 The starting value for minimal frequency
#' @param s2 The ending value for minimal frequency
#' @param s3 The step size for minimal frequency
#' @param min_freq A character indicating whether to use minimal document
#'      frequency ("doc") or minimal term frequency ("term"). Default is "doc"
#'
#' @return The function generates a plot with two subplots: one showing the
#' change in vocabulary size and another showing the number of documents dropped
#' based on the specified minimal document or term frequency
#'
#' @examples
#' # Example Usage:
#' # dfm_trim_plot(my_dfm, 1, 10, 1, min_freq = "doc")
#'
#' @details
#' The function trims the Document-Frequency Matrix (DFM) based on the specified
#' minimal document or term frequency and generates a plot illustrating the
#' impact on vocabulary size and the number of documents dropped. It provides
#' insights into the trade-off between vocabulary size and document retention
#' for different frequency thresholds.
#'
#' @seealso
#' \code{\link{dfm_trim}}, \code{\link{plotRemoved}}
#'
#' @export

dfm_trim_plot <- function(DFM, s1, s2, s3, min_freq = "doc"){
  voc_list <- list()
  doc_list <- list()

  if (min_freq == "doc") {
    for (i in seq(s1, s2, s3)) {
      trim <- quanteda::dfm_trim(DFM, min_docfreq = i)
      voc_list[[i]] <- dim(trim)[2]
      doc_list[[i]] <- sum(rowSums(trim) == 0)
    }

    par(mfrow = c(1, 2))
    plot(unlist(voc_list), type = "l", ylab = "voc size", xlab = "Minimal doc frequency", main = "Change in vocabulary size", xaxt = "n")
    axis(1, at = 1:length(seq(s1, s2, s3)), labels = seq(s1, s2, s3))
    abline(h = round(dim(DFM)[2]/2, 0), col = "orange", lty = "dashed")
    text((s2 - s1)/2, dim(DFM)[2]/75 + round(dim(DFM)[2]/2, 0), "half of max voc size", col = "orange")
    plot(unlist(doc_list), type = "l", ylab = "docs dropped", xlab = "Minimal doc frequency", main = "Number of docs dropped", xaxt = "n")
    axis(1, at = 1:length(seq(s1, s2, s3)), labels = seq(s1, s2, s3))

  }

  if (min_freq == "term") {
    for (i in seq(s1, s2, s3)) {
      trim <- quanteda::dfm_trim(DFM, min_termfreq = i)
      voc_list[[i]] <- dim(trim)[2]
      doc_list[[i]] <- sum(rowSums(trim) == 0)
    }

    par(mfrow = c(1, 2))
    plot(unlist(voc_list), type = "l", ylab = "voc size", xlab = "Minimal term frequency", main = "Change in vocabulary size", xaxt = "n")
    axis(1, at = 1:length(seq(s1, s2, s3)), labels = seq(s1, s2, s3))
    abline(h = round(dim(DFM)[2]/2, 0), col = "orange", lty = "dashed")
    text((s2 - s1)/2, dim(DFM)[2]/75 + round(dim(DFM)[2]/2, 0), "half of max voc size", col = "orange")
    plot(unlist(doc_list), type = "l", ylab = "docs dropped", xlab = "Minimal term frequency", main = "Number of docs dropped", xaxt = "n")
    axis(1, at = 1:length(seq(s1, s2, s3)), labels = seq(s1, s2, s3))
  }

}
