args <- commandArgs(TRUE)
name <- args[1]

stats_target <-
  c(
    balance = 0.491350742542373,
    mpd = 68.40613,
    pd = 2474.57,
    sr = 339,
    cherries = 81,
    rogers = 0.741839762611276
  )

prior_ABCSMC_ed <- eveABC::create_prior_ABCSMC(
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

ABC_result_ed <- EasyABC::ABC_sequential(
  method = "Lenormand",
  model = eveABC::edd_sim_ABCSMC_ed_tas_cluster,
  prior = prior_ABCSMC_ed,
  nb_simul = 1000,
  summary_stat_target = stats_target,
  p_acc_min = 0.05,
  use_seed = TRUE,
  n_cluster = 24,
  inside_prior = TRUE
)

save.image(file = paste0(name, "_ed", ".RData"))
