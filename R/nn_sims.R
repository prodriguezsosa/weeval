#' Return nearest neighbors based on cosine similarity vector
#'
#' @param cue cue word for which nearest neighbors are required
#' @param sims matrix of similariteis (with cues as rownames)
#' @param N number of nearest neighbors to return
#' @return a character vector of nearest neighbors to cue word
#'
#' @note cue is always the nearest neighbor hence can request to drop
#'
#' @export
#'
nn_sims <- function(cue, sims, N = 5, drop_cue = TRUE){
  cos_sim <- sims[cue,]
  nn <- cos_sim <- cos_sim[order(-cos_sim)]
  if(drop_cue){return(names(nn)[2:(N + 1)])}else{
  return(names(nn)[1:N])}
}
