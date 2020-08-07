#' Compute similarity vector between a cue word and all words in the vocabulary of an embeddings model
#'
#' @param embeds embeddings model (V by D matrix)
#' @param cue cue word for which similarity vector will be computed
#' @param method a `param` of `text2vec`'s `sim2`: character, the similarity measure to be used. One of c("cosine","jaccard").
#' @param norm a `param` of `text2vec`'s `sim2`: character = c("l2","none") - how to scale input matrices. If they already scaled - use "none"
#' @param rank logical value indicating whether to return a vector of similarities or ranks
#' @return a numeric vector of cosine similarities (or ranks if indicated) between all words in the vocabulary and the cue word
#'
#' @export
cue_sim <- function(cue, embeds, method = "cosine", norm = "l2", rank = FALSE){
  cos_sim <- sim2(x = embeds, y = embeds[cue, , drop = FALSE], method = method, norm = norm)
  if(rank){return(rank((-1)*cos_sim[,1], ties.method = "random"))}else{  # mult by -1 b/c the base rank fcn. works in increasing order
    return(cos_sim[,1])
  }
}
