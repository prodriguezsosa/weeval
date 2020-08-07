#' Compute correlations between two matrices of similarity vectors
#'
#' @param sims1 matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)
#' @param sims2 matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)
#' @param type `pearson` or `rank` correlation
#' @return a tibble with mean and standard error of correlation overt set of cues
#'
#' @note embedding models will be subsetted to common vocabulary
#'
#' @export
corr_sims <- function(sims1, sims2, type = "pearson"){


  # identify common vaocabulary (to be added: option to compare non-fully-overlapping vocabs)
  common_vocab <- intersect(colnames(sims1), colnames(sims2))

  # subset embeddings to common vocabulary
  sims1 <- sims1[,common_vocab]
  sims2 <- sims2[,common_vocab]

  # check cues are the same
  if(!all(nrow(sims1) == nrow(sims2) & all(rownames(sims1) %in% rownames(sims2)))){

    # identify common cues (to be added: option to compare non-fully-overlapping vocabs)
    common_cues <- intersect(rownames(sims1), rownames(sims2))

    # subset similarities to common cues
    sims1 <- sims1[common_cues,]
    sims2 <- sims2[common_cues,]}

  # compute correlation
  if(type == "pearson"){
    corr_value <- lapply(1:nrow(sims1), function(y) cor(sims1[y, ], sims2[y, ], use = "complete.obs", method = "pearson"))}else{
      corr_value <- lapply(1:nrow(sims1), function(y) cor.fk(sims1[y, ], sims2[y, ]))
    }

  # unlist
  output <- unlist(corr_value)

  # output
  if(length(output) == 1){
    return(tibble(mean = output, se = NA))}else{
    return(tibble(mean = mean(output), se = sd(output)/sqrt(length(output))))}
}
