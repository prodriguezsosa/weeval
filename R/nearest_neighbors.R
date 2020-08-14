#' Find the nearest neighbors of a cue given a trained word embedding model
#'
#' @param embeds matrix of trained embeddings
#' @param cue cue word for which we want nearest neighbors
#' @param N number of nearest neighbors to return
#' @param method a `param` of `text2vec`'s `sim2`: character, the similarity measure to be used. One of c("cosine","jaccard").
#' @param norm a `param` of `text2vec`'s `sim2`: character = c("l2","none") - how to scale input matrices. If they already scaled - use "none"
#' @param drop_cue drop nearest neighbor in top N count
#' @return a character vector of nearest neighbors of length N
#'
#' @note `context` the first nearest neighbor will be the cue itself.
#'
#' @export
nearest_neighbors <- function(cue, embeds, N = 5, method = "cosine", norm = "l2", drop_cue = TRUE){
  cos_sim <- sim2(x = embeds, y = embeds[cue, , drop = FALSE], method = method, norm = norm)
  nn <- cos_sim[order(-cos_sim),]
  if(drop_cue){return(names(nn)[2:(N + 1)])}else{
    return(names(nn)[1:N])}
}
