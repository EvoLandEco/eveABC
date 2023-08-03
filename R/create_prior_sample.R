create_prior_sample <- function(dist, ...) {
  stopifnot(is.character(dist))
  pars <- list(...)

  if(dist=="norm"){
    
  }
  if (dist=="unif"){
    pars_names <- c("n",
                    "la_min",
                    "la_max",
                    "mu_min",
                    "mu_max",
                    "beta_n_min",
                    "beta_n_max",
                    "beta_phi_min",
                    "beta_phi_max",
                    "age",
                    "model",
                    "metric",
                    "offset")
    pars_identical <- setequal(sort(names(pars)), sort(pars_names))
    pars_legal <- all(lengths(my_list) == 1)
    if (!pars_identical) stop(paste0("Insufficient parameters, requiring ", paste(pars_names, collapse = ", ")))
    if (!pars_legal) stop("Illegal parameter(s)")
    
    lambda <- runif(n=pars$n,min = pars$la_min,max=pars$la_max)
    mu <- runif(n=pars$n,min = pars$mu_min,max=pars$mu_max)
    beta_n <- runif(n=pars$n,min = pars$beta_n_min,max=pars$beta_n_max)
    beta_phi <- runif(n=pars$n,min = pars$beta_phi_min,max=pars$beta_phi_max)
    age <- rep(pars$age, times = pars$n)
    model <- rep(pars$model, times = pars$n)
    metric <- rep(pars$metric, times = pars$n)
    offset <- rep(pars$offset, times = pars$n)
    
    prior <- data.frame(lambda = lambda,
                        mu = mu,
                        beta_n = beta_n,
                        beta_phi = beta_phi,
                        age = age,
                        model = model,
                        metric = metric,
                        offset = offset)
  }
  
  return(prior)
}


as.prior.sample <- function(x) {
  if(!inherits(x, "data.frame")) stop("Must be a data frame")
  
  x <- as.list(x)
  
  x <- lapply(seq_len(length(x[[1]])), function(i) 
    lapply(x, `[[`, i))
  
  class(x) <- c("prior sample", class(x))
  
  return(x)
}