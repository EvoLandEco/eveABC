args <- commandArgs(TRUE)
name <- args[1]

stats_target_accu_pd_tas <-
  c(
    balance = 0.752322,
    gamma = -4.565912,
    pd = 57.529837,
    sr = 35,
    cherries = 10,
    rogers = 0.700000
  )

stats_target_accu_pd_tes <-
  c(
    balance = 0.9123397,
    gamma = 0.9389810,
    pd = 57.9741213,
    sr = 5,
    cherries = 2,
    rogers = 0.7500000
  )

prior_ABCSMC_accu_pd_tas <- eveABC::create_prior_ABCSMC(
  "unif",
  la_min = 0.5,
  la_max = 4,
  mu_min = 0,
  mu_max = 0.8,
  beta_n_min = -0.3,
  beta_n_max = 0.05,
  beta_phi_min = -0.2,
  beta_phi_max = 0.1
)

prior_ABCSMC_accu_pd_tes <- eveABC::create_prior_ABCSMC(
  "unif",
  la_min = 0.5,
  la_max = 4,
  mu_min = 0,
  mu_max = 0.8,
  beta_n_min = -0.3,
  beta_n_max = 0.05,
  beta_phi_min = -0.2,
  beta_phi_max = 0.1
)

ABC_result_accu_pd_tas <- EasyABC::ABC_sequential(
  method = "Lenormand",
  model = eveABC::edd_sim_ABCSMC_pd_cluster,
  prior = prior_ABCSMC_accu_pd_tas,
  nb_simul = 1000,
  summary_stat_target = stats_target_accu_pd_tas,
  p_acc_min = 0.02,
  use_seed = TRUE,
  n_cluster = 64,
  inside_prior = TRUE
)

ABC_result_accu_pd_tes <- EasyABC::ABC_sequential(
  method = "Lenormand",
  model = eveABC::edd_sim_ABCSMC_pd_cluster,
  prior = prior_ABCSMC_accu_pd_tes,
  nb_simul = 1000,
  summary_stat_target = stats_target_accu_pd_tes,
  p_acc_min = 0.02,
  use_seed = TRUE,
  n_cluster = 64,
  inside_prior = TRUE
)

save.image(file = paste0(name, "_accu_pd", ".RData"))
