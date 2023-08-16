process_abc_result <- function(data, stats) {
  if (length(stats) < 1) {
    stop("Invalid statistics names")
  }
  
  if (!all(sapply(stats, is.character))) {
    stop("Invalid statistics names")
  }
  
  colnames(data$param) <- c("lambda", "mu", "beta_n", "beta_phi")
  colnames(data$stats) <- stats
  
  return(data)
}


process_simulated_result <- function(data, stats) {
  if (length(stats) < 1) {
    stop("Invalid statistics names")
  }
  
  if (!all(sapply(stats, is.character))) {
    stop("Invalid statistics names")
  }
  
  colnames(data$stats) <- stats
  
  return(data)
}