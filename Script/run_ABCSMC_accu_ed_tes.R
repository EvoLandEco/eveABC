args <- commandArgs(TRUE)
name <- args[1]

stats_target_accu_ed_tes <-
  c(
    balance = 0.7924813,
    mpd = 31.1108627,
    pd = 134.6469078,
    sr = 13,
    cherries = 4,
    rogers = 0.6666667 
  )

prior_ABCSMC_accu_ed_tes <- eveABC::create_prior_ABCSMC(
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

ABC_result_accu_ed_tes <- EasyABC::ABC_sequential(
  method = "Lenormand",
  model = eveABC::edd_sim_ABCSMC_ed_tes_cluster,
  prior = prior_ABCSMC_accu_ed_tes,
  nb_simul = 1000,
  summary_stat_target = stats_target_accu_ed_tes,
  p_acc_min = 0.05,
  use_seed = TRUE,
  n_cluster = 16,
  inside_prior = TRUE
)

save.image(file = paste0(name, "_accu_ed_tes", ".RData"))
