args <- commandArgs(TRUE)
name <- args[1]
metric <- args[2]
set <- args[3]
nsim <- args[4]

target_combo_pd <- eve::edd_combo_maker(
  la = c(0.6),
  mu = c(0.1),
  beta_n = c(-0.04, 0),
  beta_phi = c(-0.04, 0),
  age = c(10),
  model = "dsce2",
  metric = "pd",
  offset = "simtime"
)

target_combo_ed <- eve::edd_combo_maker(
  la = c(0.6),
  mu = c(0.1),
  beta_n = c(-0.04, 0),
  beta_phi = c(-0.04, 0, 0.001),
  age = c(10),
  model = "dsce2",
  metric = "ed",
  offset = "none"
)

target_combo_nnd <- eve::edd_combo_maker(
  la = c(0.6),
  mu = c(0.1),
  beta_n = c(-0.04, 0),
  beta_phi = c(-0.04, 0, 0.001),
  age = c(10),
  model = "dsce2",
  metric = "nnd",
  offset = "none"
)

stats_target_name <- paste0(name, "_", metric, "_", set, "_target")
prior_name <- paste0(name, "_", metric, "_", set, "_prior")
eval(parse(text = paste0(stats_target_name, " <- 3")))
eval(parse(text = paste0(stats_target_name)))

stats_target_accu_ed_tas <-
  c(
    balance = 0.6888722,
    mpd = 20.1759778,
    pd = 380.8391195,
    sr = 87,
    cherries = 25,
    rogers = 0.6842105
  )

prior_ABCSMC_accu_ed_tas <- eveABC::create_prior_ABCSMC(
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

ABC_result_accu_ed_tas <- EasyABC::ABC_sequential(
  method = "Lenormand",
  model = eveABC::edd_sim_ABCSMC_ed_tas_cluster,
  prior = eval(parse(text = paste0(prior_name))),
  nb_simul = nsim,
  summary_stat_target = eval(parse(text = paste0(stats_target_name))),
  p_acc_min = 0.05,
  use_seed = TRUE,
  n_cluster = 16,
  inside_prior = TRUE
)

save.image(file = paste0(name, "_accu_ed_tas", ".RData"))

