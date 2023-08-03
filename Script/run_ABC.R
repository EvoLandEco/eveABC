args <- commandArgs(TRUE)

name <- args[1]
set <- as.numeric(args[2])
nrep <- as.numeric(args[3])

prior_sample_1 <-  eveABC::create_prior_sample(dist = "unif",
                                       n = 50,
                                       la_min = 0.6,
                                       la_max = 1.2,
                                       mu_min = 0,
                                       mu_max = 0.2,
                                       beta_n_min = -0.02,
                                       beta_n_max = 0,
                                       beta_phi_min = -0.02,
                                       beta_phi_max = 0.002,
                                       age = 9.9,
                                       model = "dsce2",
                                       metric = "pd",
                                       offset = "simtime")

prior_sample_2 <-  eveABC::create_prior_sample(dist = "unif",
                                       n = 50,
                                       la_min = 0.6,
                                       la_max = 1.2,
                                       mu_min = 0,
                                       mu_max = 0.2,
                                       beta_n_min = -0.02,
                                       beta_n_max = 0,
                                       beta_phi_min = -0.02,
                                       beta_phi_max = 0.002,
                                       age = 9.9,
                                       model = "dsce2",
                                       metric = "ed",
                                       offset = "none")

prior_sample_3 <-  eveABC::create_prior_sample(dist = "unif",
                                       n = 50,
                                       la_min = 0.6,
                                       la_max = 1.2,
                                       mu_min = 0,
                                       mu_max = 0.2,
                                       beta_n_min = -0.02,
                                       beta_n_max = 0,
                                       beta_phi_min = -0.02,
                                       beta_phi_max = 0.002,
                                       age = 9.9,
                                       model = "dsce2",
                                       metric = "nnd",
                                       offset = "none")

prior_sample <- rbind(prior_sample_1, prior_sample_2, prior_sample_3)

prior_sample <- eveABC::as.prior.sample(prior_sample)

out <- eveABC::edd_sim_sample_rep(prior_sample[[set]], rep = nrep, drop_extinct = FALSE)

eve:::check_folder(name)
eve:::save_result(out, name, set)