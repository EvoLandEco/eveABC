#' @export do_edd_sim
do_edd_sim <- function(sample, rep = 100, drop_extinct = FALSE) {
  if(!inherits(sample, "prior sample")) {
    stop("Prior sample must be created by the create_prior_sample() function")
  }
  
  trees <- lapply(sample, edd_sim_sample_rep, drop_extinct = drop_extinct)
  
  return(trees)
}


#' @export edd_sim_sample_rep
edd_sim_sample_rep <- function(sample, rep = 100, drop_extinct = FALSE) {
  result <- replicate(rep, edd_sim_sample(sample = sample, drop_extinct = drop_extinct))
}


edd_sim_sample <- function(sample, drop_extinct = FALSE) {
  result <- eve::edd_sim(pars = c(sample$lambda, sample$mu, sample$beta_n,sample$beta_phi),
               age = sample$age,
               model = sample$model,
               metric = sample$metric,
               offset= sample$offset,
               history = FALSE,
               verbose = FALSE)
  
  if(drop_extinct == TRUE) {
    return(result$tes)
  } else {
    return(result$tas)
  }
}