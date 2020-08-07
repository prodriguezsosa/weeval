#' Compute jaccard index of two embeddings models for N nearest neighbors
#'
#' @param sims1 matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)
#' @param sims2 matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)
#' @param N number of nearest neighbors with which to compute jaccard index
#' @param common_vocab whether to use a common vocabulary to compute jaccard index
#' @return a tibble with mean and standard error of jaccard index overt set of cues
#'
#' @export
jaccard_sims <- function(sims1, sims2, N = 10, common_vocab = FALSE){

  # check N is less than or equal to smallest vocab
  if(N > min(ncol(sims1), ncol(sims2))) stop("N must be smaller than or equal to the smallest vocabulary")

  # check cues are the same
  if(!all(nrow(sims1) == nrow(sims2) & all(rownames(sims1) %in% rownames(sims2)))) stop("similarity matrices have different rownames (cues)")

  # if common_vocab == TRUE, subset to common vocab
  if(common_vocab){

    # identify common vocabulary
    common_vocab <- intersect(colnames(sims1), colnames(sims2))

    # subset embeddings to common vocabulary
    sims1 <- sims1[,common_vocab]
    sims2 <- sims2[,common_vocab]}

  # compute jaccard index
  jaccard_value <- lapply(1:nrow(sims1), function(x){
    nn1 <- sims1[x,][order(-sims1[x,])] %>% names() %>% .[2:(N + 1)] # exclude cue, always nearest neighbor
    nn2 <- sims2[x,][order(-sims2[x,])] %>% names() %>% .[2:(N + 1)] # exclude cue, always nearest neighbor
    return(length(intersect(nn1, nn2))/length(unique(nn1, nn2)))
  })

  # unlist
  output <- unlist(jaccard_value)

  # output
  if(length(output) == 1){
    return(tibble(mean = output, se = NA))}else{
    return(tibble(mean = mean(output), se = sd(output)/sqrt(length(output))))}
}
