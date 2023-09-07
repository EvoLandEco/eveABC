#' @export do_edd_sim
do_edd_sim <- function(sample,
                       rep = 100,
                       drop_extinct = FALSE) {
  if (!inherits(sample, "prior sample")) {
    stop("Prior sample must be created by the create_prior_sample() function")
  }
  
  trees <-
    lapply(sample, edd_sim_sample_rep, drop_extinct = drop_extinct)
  
  return(trees)
}


#' @export edd_sim_sample_rep
edd_sim_sample_rep <-
  function(sample,
           rep = 100,
           drop_extinct = FALSE) {
    result <-
      replicate(rep,
                edd_sim_sample(sample = sample, drop_extinct = drop_extinct))
  }


edd_sim_sample <- function(sample, drop_extinct = FALSE) {
  result <-
    eve::edd_sim(
      pars = c(sample$lambda, sample$mu, sample$beta_n, sample$beta_phi),
      age = sample$age,
      model = sample$model,
      metric = sample$metric,
      offset = sample$offset,
      history = FALSE,
      verbose = FALSE
    )
  
  if (drop_extinct == TRUE) {
    return(result$tes)
  } else {
    return(result$tas)
  }
}


#' @export create_simulated_target
create_simulated_target <- function(combo = NULL, nrep = 1000){
  target_result <- eve::edd_sim_rep(
    combo = combo,
    history = FALSE,
    verbose = FALSE,
    nrep = nrep
  )
  
  target_stats <- list(stats_tas = dplyr::bind_rows(lapply(target_result$tas, summary_stats)),
  stats_tes = dplyr::bind_rows(lapply(target_result$tes, summary_stats)))
  
  return(target_stats)
}


#' @export edd_sim_ABCSMC_pd_tas_cluster
edd_sim_ABCSMC_pd_tas_cluster <- function(pars) {
  set.seed(pars[1])
  
  done <- FALSE
  complete <- FALSE
  
  stats <- data.frame()
  
  result <- list()
  
  while (!done) {
    tryCatch(
      expr = {
        R.utils::withTimeout({
          result <- eve::edd_sim(
            pars = c(pars[2], pars[3], pars[4], pars[5]),
            age = 65,
            model = "dsce2",
            metric = "pd",
            offset = "simtime",
            history = FALSE,
            verbose = FALSE
          )
        }, timeout = 60)
        complete <- TRUE
      },
      TimeoutException = function(ex) {
        cat("Simulation timed out. Returning impossible statistics\n")
      },
      error = function(e) {
        if (grepl("reached elapsed time limit", e$message)) {
          cat("Simulation timed out. Returning impossible statistics\n")
        } else {
          stop(e)  # Rethrow any other error
        }
      }
    )
    
    if (complete == TRUE) {
      stats <- eveABC::summary_stats(result$tas)
    } else {
      stats <-
        data.frame(
          balance = 0,
          gamma = 0,
          pd = 0,
          sr = 3,
          cherries = 0,
          rogers = 0
        )
    }
    
    if (stats$sr > 2) {
      done <- TRUE
    }
  }
  
  return(unlist(stats))
}


#' @export edd_sim_ABCSMC_ed_tas_cluster
edd_sim_ABCSMC_ed_tas_cluster <- function(pars) {
  set.seed(pars[1])
  
  done <- FALSE
  complete <- FALSE
  
  stats <- data.frame()
  
  result <- list()
  
  while (!done) {
    tryCatch(
      expr = {
        R.utils::withTimeout({
          result <- eve::edd_sim(
            pars = c(pars[2], pars[3], pars[4], pars[5]),
            age = 65,
            model = "dsce2",
            metric = "ed",
            offset = "none",
            history = FALSE,
            verbose = FALSE
          )
        }, timeout = 60)
        complete <- TRUE
      },
      TimeoutException = function(ex) {
        cat("Simulation timed out. Returning impossible statistics\n")
      },
      error = function(e) {
        if (grepl("reached elapsed time limit", e$message)) {
          cat("Simulation timed out. Returning impossible statistics\n")
        } else {
          stop(e)  # Rethrow any other error
        }
      }
    )
    
    if (complete == TRUE) {
      stats <- eveABC::summary_stats(result$tas)
    } else {
      stats <-
        data.frame(
          balance = 0,
          gamma = 0,
          pd = 0,
          sr = 3,
          cherries = 0,
          rogers = 0
        )
    }
    
    if (stats$sr > 2) {
      done <- TRUE
    }
  }
  
  return(unlist(stats))
}


#' @export edd_sim_ABCSMC_nnd_tas_cluster
edd_sim_ABCSMC_nnd_tas_cluster <- function(pars) {
  set.seed(pars[1])
  
  done <- FALSE
  complete <- FALSE
  
  stats <- data.frame()
  
  result <- list()
  
  while (!done) {
    tryCatch(
      expr = {
        R.utils::withTimeout({
          result <- eve::edd_sim(
            pars = c(pars[2], pars[3], pars[4], pars[5]),
            age = 65,
            model = "dsce2",
            metric = "nnd",
            offset = "none",
            history = FALSE,
            verbose = FALSE
          )
        }, timeout = 60)
        complete <- TRUE
      },
      TimeoutException = function(ex) {
        cat("Simulation timed out. Returning impossible statistics\n")
      },
      error = function(e) {
        if (grepl("reached elapsed time limit", e$message)) {
          cat("Simulation timed out. Returning impossible statistics\n")
        } else {
          stop(e)  # Rethrow any other error
        }
      }
    )
    
    if (complete == TRUE) {
      stats <- eveABC::summary_stats(result$tas)
    } else {
      stats <-
        data.frame(
          balance = 0,
          gamma = 0,
          pd = 0,
          sr = 3,
          cherries = 0,
          rogers = 0
        )
    }
    
    if (stats$sr > 2) {
      done <- TRUE
    }
  }
  
  return(unlist(stats))
}


#' @export edd_sim_ABCSMC_pd_tes_cluster
edd_sim_ABCSMC_pd_tes_cluster <- function(pars) {
  set.seed(pars[1])
  
  done <- FALSE
  complete <- FALSE
  
  stats <- data.frame()
  
  result <- list()
  
  while (!done) {
    tryCatch(
      expr = {
        R.utils::withTimeout({
          result <- eve::edd_sim(
            pars = c(pars[2], pars[3], pars[4], pars[5]),
            age = 20,
            model = "dsce2",
            metric = "pd",
            offset = "simtime",
            history = FALSE,
            verbose = FALSE
          )
        }, timeout = 60)
        complete <- TRUE
      },
      TimeoutException = function(ex) {
        cat("Simulation timed out. Returning impossible statistics\n")
      },
      error = function(e) {
        if (grepl("reached elapsed time limit", e$message)) {
          cat("Simulation timed out. Returning impossible statistics\n")
        } else {
          stop(e)  # Rethrow any other error
        }
      }
    )
    
    if (complete == TRUE) {
      stats <- eveABC::summary_stats(result$tes)
    } else {
      stats <-
        data.frame(
          balance = 0,
          gamma = 0,
          pd = 0,
          sr = 3,
          cherries = 0,
          rogers = 0
        )
    }
    
    if (stats$sr > 2) {
      done <- TRUE
    }
  }
  
  return(unlist(stats))
}


#' @export edd_sim_ABCSMC_ed_tes_cluster
edd_sim_ABCSMC_ed_tes_cluster <- function(pars) {
  set.seed(pars[1])
  
  done <- FALSE
  complete <- FALSE
  
  stats <- data.frame()
  
  result <- list()
  
  while (!done) {
    tryCatch(
      expr = {
        R.utils::withTimeout({
          result <- eve::edd_sim(
            pars = c(pars[2], pars[3], pars[4], pars[5]),
            age = 20,
            model = "dsce2",
            metric = "ed",
            offset = "none",
            history = FALSE,
            verbose = FALSE
          )
        }, timeout = 60)
        complete <- TRUE
      },
      TimeoutException = function(ex) {
        cat("Simulation timed out. Returning impossible statistics\n")
      },
      error = function(e) {
        if (grepl("reached elapsed time limit", e$message)) {
          cat("Simulation timed out. Returning impossible statistics\n")
        } else {
          stop(e)  # Rethrow any other error
        }
      }
    )
    
    if (complete == TRUE) {
      stats <- eveABC::summary_stats(result$tes)
    } else {
      stats <-
        data.frame(
          balance = 0,
          gamma = 0,
          pd = 0,
          sr = 3,
          cherries = 0,
          rogers = 0
        )
    }
    
    if (stats$sr > 2) {
      done <- TRUE
    }
  }
  
  return(unlist(stats))
}


#' @export edd_sim_ABCSMC_nnd_tes_cluster
edd_sim_ABCSMC_nnd_tes_cluster <- function(pars) {
  set.seed(pars[1])
  
  done <- FALSE
  complete <- FALSE
  
  stats <- data.frame()
  
  result <- list()
  
  while (!done) {
    tryCatch(
      expr = {
        R.utils::withTimeout({
          result <- eve::edd_sim(
            pars = c(pars[2], pars[3], pars[4], pars[5]),
            age = 20,
            model = "dsce2",
            metric = "nnd",
            offset = "none",
            history = FALSE,
            verbose = FALSE
          )
        }, timeout = 60)
        complete <- TRUE
      },
      TimeoutException = function(ex) {
        cat("Simulation timed out. Returning impossible statistics\n")
      },
      error = function(e) {
        if (grepl("reached elapsed time limit", e$message)) {
          cat("Simulation timed out. Returning impossible statistics\n")
        } else {
          stop(e)  # Rethrow any other error
        }
      }
    )
    
    if (complete == TRUE) {
      stats <- eveABC::summary_stats(result$tes)
    } else {
      stats <-
        data.frame(
          balance = 0,
          gamma = 0,
          pd = 0,
          sr = 3,
          cherries = 0,
          rogers = 0
        )
    }
    
    if (stats$sr > 2) {
      done <- TRUE
    }
  }
  
  return(unlist(stats))
}