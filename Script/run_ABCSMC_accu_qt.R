args <- commandArgs(TRUE)
name <- as.character(args[1])
nsim <- as.numeric(as.character(args[2]))
metric <- as.character(args[3])
set <- as.numeric(as.character(args[4]))

# Run for complete trees
type <- "tas"

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

target_stats <-
  lapply(readRDS(paste0("target_", metric , ".rds")), eveABC::summarize_simulated_target)
combo <-
  eval(parse(text = paste0("target_combo_", metric, "[[set]]")))
target <- target_stats[[set]]$stats_tas
prior <- eveABC::create_prior_ABCSMC(
  "unif",
  la_min = 0,
  la_max = 2 * combo$pars[[1]][1],
  mu_min = 0,
  mu_max = 2 * combo$pars[[1]][2],
  beta_n_min = -0.1,
  beta_n_max = 0,
  beta_phi_min = -0.1,
  beta_phi_max = 0.005
)

print(combo)
print(target)
print(paste0(name, ": ", nsim, " simulations, ", metric, ", parameter set: ", set))

ABC_result <- EasyABC::ABC_sequential(
  method = "Lenormand",
  model =   eval(parse(
    text = paste0("eveABC::edd_sim_ABCSMC_", metric, "_", type , "_cluster")
  )),
  prior = prior,
  nb_simul = nsim,
  summary_stat_target = target,
  p_acc_min = 0.05,
  use_seed = TRUE,
  n_cluster = 16,
  inside_prior = TRUE
)

message("ABCSMC finished")

ABC_bundle <- list(ABC_result <- ABC_result,
                   combo <- combo,
                   target <- target,
                   prior <- prior)

message(paste0("Saving result to ",name, "_qt_", metric, "_", type, ".rds"))
saveRDS(ABC_bundle, file = paste0(name, "_qt_", metric, "_", type, ".rds"))
