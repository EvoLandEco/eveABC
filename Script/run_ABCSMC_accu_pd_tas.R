args <- commandArgs(TRUE)
name <- args[1]

stats_target_accu_pd_tas <-
  c(
    balance = 0.7606298,
    mpd = 18.0725775,
    pd = 155.6657107,
    sr = 35,
    cherries = 10,
    rogers = 0.6956522
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

ABC_result_accu_pd_tas <- EasyABC::ABC_sequential(
  method = "Lenormand",
  model = eveABC::edd_sim_ABCSMC_pd_tas_cluster,
  prior = prior_ABCSMC_accu_pd_tas,
  nb_simul = 100,
  summary_stat_target = stats_target_accu_pd_tas,
  p_acc_min = 0.05,
  use_seed = TRUE,
  n_cluster = 16,
  inside_prior = TRUE
)

save.image(file = paste0(name, "_accu_pd_tas", ".RData"))
