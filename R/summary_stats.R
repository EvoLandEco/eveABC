summary_stats <- function(phy) {
  balance <- treestats::j_one(phy)
  gamma <- treestats::gamma_statistic(phy)
  pd <- treestats::phylogenetic_diversity(phy)
  mpd <- treestats::mean_pair_dist(phy, normalization = "tips")
  rogers <- treestats::rogers(phy, normalization = "tips")
  
  return(data.frame(balance = balance,
                    gamma = gamma,
                    pd = pd,
                    mpd = mpd,
                    rogers = rogers))
}