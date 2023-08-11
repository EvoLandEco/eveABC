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
                   aspect.ratio = 4 / 5)
}


density_param <- function(data) {
  plot_data <-
    as.data.frame(data$param) %>% tidyr::gather(key = "Param", value = "Value")
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_density(ggplot2::aes(Value),
                          color = "black") +
    ggplot2::facet_wrap(. ~ Param, scales = "free")
}


hist_stats <- function(data, target) {
  plot_data <-
    as.data.frame(data$stats) %>% tidyr::gather(key = "Stats", value = "Value")
  
  stats_target <-
    tidyr::gather(data.frame(as.list(target)), key = "Stats", value = "Value")
  
  
  stats_median <-
    plot_data %>% dplyr::group_by(Stats) %>% dplyr::summarize(median = median(Value), height = max(hist(Value, breaks=30, plot=FALSE)$counts) / 2)
  
  stats_median <- left_join(stats_median, tibble::enframe(target, name = "Stats", value = "target"))
  
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_histogram(
      ggplot2::aes(Value, fill = Stats, alpha = ..count..),
      color = "black",
      bins = 30,
      right=TRUE
    ) +
    ggplot2::facet_wrap(. ~ Stats, scales = "free") +
    ggplot2::geom_vline(
      data = stats_target,
      ggplot2::aes(xintercept = Value),
      color = "red",
      linetype = "dotdash",
      linewidth = 1
    ) +
    ggplot2::geom_vline(
      data = stats_median,
      ggplot2::aes(xintercept = median),
      color = "black",
      linetype = "dotdash",
      linewidth = 1
    ) +
    ggplot2::geom_segment(
      data = stats_median,
      ggplot2::aes(
        x = median,
        xend = target,
        y = height,
        yend = height
      ),
      arrow = arrow(type = "closed", length = unit(0.05, "inches"), ends = "both")
    ) +
    ggplot2::theme(legend.position = "none",aspect.ratio = 4 / 5) +
    ggplot2::labs(x = "Parameter value",
                  y = "Frequency") +
    ggplot2::ggtitle("Precision estimates")
  #guide_area() +
  #plot_layout(guides = 'collect')
}