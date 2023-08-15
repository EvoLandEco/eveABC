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


#' @export edd_sim_ABCSMC_pd_nr_cluster
edd_sim_ABCSMC_pd_nr_cluster <- function(pars) {
  set.seed(pars[1])
  
  done <- FALSE
  complete <- FALSE
  
  stats <- data.frame()
  
  result <- list()
  
  while (!done) {
    tryCatch(
      expr = {
        R.utils::withTimeout({
          result <- eve::edd_sim_nr(
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
          gamma = 100,
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


#' @export edd_sim_ABCSMC_ed_nr_cluster
edd_sim_ABCSMC_ed_nr_cluster <- function(pars) {
  set.seed(pars[1])
  
  done <- FALSE
  complete <- FALSE
  
  stats <- data.frame()
  
  result <- list()
  
  while (!done) {
    tryCatch(
      expr = {
        R.utils::withTimeout({
          result <- eve::edd_sim_nr(
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
          gamma = 100,
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


#' @export edd_sim_ABCSMC_nnd_nr_cluster
edd_sim_ABCSMC_nnd_nr_cluster <- function(pars) {
  set.seed(pars[1])
  
  done <- FALSE
  complete <- FALSE
  
  stats <- data.frame()
  
  result <- list()
  
  while (!done) {
    tryCatch(
      expr = {
        R.utils::withTimeout({
          result <- eve::edd_sim_nr(
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
          gamma = 10,
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


#' @export edd_sim_ABCSMC_pd_cluster
edd_sim_ABCSMC_pd_cluster <- function(pars) {
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
          gamma = 100,
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


#' @export edd_sim_ABCSMC_ed_cluster
edd_sim_ABCSMC_ed_cluster <- function(pars) {
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
          gamma = 100,
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


#' @export edd_sim_ABCSMC_nnd_cluster
edd_sim_ABCSMC_nnd_cluster <- function(pars) {
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
          gamma = 10,
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