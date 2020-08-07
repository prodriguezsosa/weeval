#' Computes the average similarity vector between a set of cues and the full vocab over multiple embedding models
#'
#' @param embeds_list list of trained word embedding models (each is a V by D matrix)
#' @param cues a character vector of cue words for which the similarity vector will be computed
#' @param method a `param` of `text2vec`'s `sim2`: character, the similarity measure to be used. One of c("cosine","jaccard").
#' @param norm a `param` of `text2vec`'s `sim2`: character = c("l2","none") - how to scale input matrices. If they already scaled - use "none"
#' @return a matrix of similarity vectors, one row for each cue
#'
#' @note embedding models should not be averaged directly, instead, users should average
#' the similarity vectors. This function makes this easier. See Rodriguez & Spirling.
#'
#' @export
avg_cos_similarity <- function(embeds_list, cues, method = "cosine", norm = 'l2'){
  cos_sim <- lapply(embeds_list, function(embeds) sim2(x = embeds[cues, , drop = FALSE], y = embeds, method = method, norm = norm)) # compute cosine similarity vectors
  out <- Reduce('+', cos_sim)/length(cos_sim) # take mean over all initializations
  return(out)
}
