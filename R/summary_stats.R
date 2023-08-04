#' @export summary_stats
summary_stats <- function(phy) {
  balance <- treestats::j_one(phy)
  gamma <- treestats::gamma_statistic(phy)
  pd <- treestats::phylogenetic_diversity(phy)
  mpd <- treestats::mean_pair_dist(phy, normalization = "tips")
  rogers <- treestats::rogers(phy, normalization = "tips")
  
  return(data.frame(
    balance = balance,
    gamma = gamma,
    pd = pd,
    mpd = mpd,
    rogers = rogers
  ))
}


#' @export summary_result
summary_result <- function(data) {
  stats <- lapply(data, function(x) {
    sub_stats <- lapply(x, summary_stats)
    sub_stats <- dplyr::bind_rows(sub_stats)
    return(sub_stats)
  })
  
  return(stats)
}
