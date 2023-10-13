args <- commandArgs(TRUE)
nrep <- args[1]

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
  beta_phi = c(-0.04, 0),
  age = c(10),
  model = "dsce2",
  metric = "ed",
  offset = "none"
)

target_combo_nnd <- eve::edd_combo_maker(
  la = c(0.6),
  mu = c(0.1),
  beta_n = c(-0.04, 0),
  beta_phi = c(-0.04, 0),
  age = c(10),
  model = "dsce2",
  metric = "nnd",
  offset = "none"
)

target_pd <- create_simulated_target_batch(target_combo_pd, nrep, "multisession", 16)

saveRDS(target_pd, "target_pd.rds")

target_ed <- create_simulated_target_batch(target_combo_ed, nrep, "multisession", 16)

saveRDS(target_ed, "target_ed.rds")

target_nnd <- create_simulated_target_batch(target_combo_nnd, nrep, "multisession", 16)

saveRDS(target_nnd, "target_nnd.rds")