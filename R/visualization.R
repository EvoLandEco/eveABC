hist_param <- function(data) {
  plot_data <-
    as.data.frame(data$param) %>% tidyr::gather(key = "Param", value = "Value")
  
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_histogram(
      ggplot2::aes(Value, fill = Param, alpha = ..count..),
      color = "black",
      bins = 30
    ) +
    ggplot2::facet_wrap(. ~ Param, scales = "free") +
    ggplot2::theme(legend.position = "none",
                   aspect.ratio = 4/5)
}


density_param <- function(data) {
  plot_data <-
    as.data.frame(data$param) %>% tidyr::gather(key = "Param", value = "Value")
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_density(
      ggplot2::aes(Value),
      color = "black"
    ) +
    ggplot2::facet_wrap(. ~ Param, scales = "free")
}


hist_stats <- function(data, target) {
  plot_data <-
    as.data.frame(data$stats) %>% tidyr::gather(key = "Stats", value = "Value")
  
  stats_target <- tidyr::gather(data.frame(as.list(target)), key = "Stats", value = "Value")
  
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_histogram(
      ggplot2::aes(Value, fill = Stats, alpha = ..count..),
      color = "black",
      bins = 30
    ) +
    ggplot2::facet_wrap(. ~ Stats, scales = "free") +
    ggplot2::geom_vline(data = stats_target, aes(xintercept = Value)) +
    ggplot2::theme(legend.position = "none",
                   aspect.ratio = 4/5)
}