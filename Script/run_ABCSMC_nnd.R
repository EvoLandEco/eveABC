args <- commandArgs(TRUE)
name <- args[1]

stats_target <- c(balance = 0.491350742542373, gamma = -9.73491404802578, pd = 54.8700000000001, 
                  mpd = 5.87076256058496, rogers = 0.741839762611276)

prior_ABCSMC <- eveABC::create_prior_ABCSMC(
  "unif",
  la_min = 0.6,
  la_max = 1.2,
  mu_min = 0,
  mu_max = 0.2,
  beta_n_min = -0.02,
  beta_n_max = 0,
  beta_phi_min = -0.02,
  beta_phi_max = 0.002
)

ABC_result <- EasyABC::ABC_sequential(
  method = "Lenormand",
  model = eveABC::edd_sim_ABCSMC_nnd_cluster,
  prior = prior_ABCSMC,
  nb_simul = 10000,
  summary_stat_target = stats_target,
  p_acc_min = 0.4,
  use_seed = TRUE,
  n_cluster = 16
)

save.image(file = paste0(name, "_nnd", ".RData"))