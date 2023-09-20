#' @export summary_stats
summary_stats <- function(phy) {
  balance <- treestats::j_one(phy)
  mpd <- treestats::mean_pair_dist(phy, normalization = "none")
  pd <- sum(phy$edge.length)
  sr <- treestats::number_of_lineages(phy)
  cherries <- treestats::cherries(phy, normalization = "none")
  rogers <- treestats::rogers(phy, normalization = "tips")
  
  return(data.frame(
    balance = balance,
    mpd = mpd,
    pd = pd,
    sr = sr,
    cherries = cherries,
    rogers = rogers
  ))
}


#' @export give_extreme_stats
give_extreme_stats <- function() {
  stats <-
    data.frame(
      balance = 0,
      mpd = 0,
      pd = 0,
      sr = 3,
      cherries = 0,
      rogers = 0
    )
  return(stats)
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


#' @export summarize_simulated_target
summarize_simulated_target <- function(data, method = "median") {
  target <- lapply(data, FUN = function(x){
    x <- x[complete.cases(x), ]
    out <- data.frame()
    if (method == "mean") {
      out <- colMeans(x)
    } else if (method == "median") {
      out <- sapply(x, median)
    }
    return(out)
  })
  
  return(target)
}
