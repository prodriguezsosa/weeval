#' Compute the correlation of a given cue's similarity vectors for two embedding models
#'
#' @param embeds1 embeddings model 1 (V by D matrix)
#' @param embeds2 embeddings model 2 (V by D matrix)
#' @param cue cue word for which the correlation will be computed
#' @param type `pearson` or `rank` correlation
#' @param method a `param` of `text2vec`'s `sim2`: character, the similarity measure to be used. One of c("cosine","jaccard").
#' @param norm a `param` of `text2vec`'s `sim2`: character = c("l2","none") - how to scale input matrices. If they already scaled - use "none"
#' @return a numeric value between -1 and 1
#'
#' @note embedding models will be subsetted to common vocabulary
#'
#' @export
corr_embeds <- function(embeds1, embeds2, cue, type = "pearson", method = "cosine", norm = "l2"){

  # identify common vocabulary (to be added: option to compare non-fully-overlapping vocabs)
  common_vocab <- intersect(rownames(embeds1), rownames(embeds2))

  # subset embeddings to common vocabulary
  embeds1 <- embeds1[common_vocab,]
  embeds2 <- embeds2[common_vocab,]

  # compute cosine distances (to cue) for each set of embeddings
  rank <- ifelse(type == "pearson", FALSE, TRUE)
  cos_dist <- lapply(list(embeds1, embeds2), function(x) cue_sim(cue, x, method = method, norm = norm, rank = rank))

  # bind vectors of cosine distances
  cos_dist <- do.call(cbind, cos_dist)

  # compute correlation
  if(type == "pearson"){
    corr_value <- cor(cos_dist, use = "complete.obs", method = "pearson")}else{
      corr_value <- cor.fk(cos_dist, y = NULL)
    }

  # keep only the diagonal value
  output <- corr_value[upper.tri(corr_value, diag = FALSE)]

  # return output
  return(output)
}
