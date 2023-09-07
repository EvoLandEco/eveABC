args <- commandArgs(TRUE)
name <- args[1]

stats_target_accu_nnd_tas <-
  c(
    balance = 0.6055318,
    gamma = -39.7715814,
    pd = 224.6290047,
    sr = 144,
    cherries = 41,
    rogers = 0.6875000
  )

prior_ABCSMC_accu_nnd_tas <- eveABC::create_prior_ABCSMC(
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

ABC_result_accu_nnd_tas <- EasyABC::ABC_sequential(
  method = "Lenormand",
  model = eveABC::edd_sim_ABCSMC_nnd_tas_cluster,
  prior = prior_ABCSMC_accu_nnd_tas,
  nb_simul = 1000,
  summary_stat_target = stats_target_accu_nnd_tas,
  p_acc_min = 0.02,
  use_seed = TRUE,
  n_cluster = 16,
  inside_prior = TRUE
)

save.image(file = paste0(name, "_accu_nnd_tas", ".RData"))
