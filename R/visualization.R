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
    ggplot2::scale_alpha_continuous(range = c(0.05, 0.8)) +
    ggplot2::theme(legend.position = "none",
                   aspect.ratio = 4 / 5) +
    ggplot2::labs(x = "Parameter value",
                  y = "Frequency")
}


density_param <- function(data) {
  plot_data <-
    as.data.frame(data$param) %>% tidyr::gather(key = "Param", value = "Value")
  
  stats_median <-
    plot_data %>% dplyr::group_by(Param) %>% dplyr::summarize(median = median(Value))
  
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_density(ggplot2::aes(Value, fill = Param),
                          color = "black",
                          alpha = 0.3) +
    ggplot2::geom_vline(
      data = stats_median,
      ggplot2::aes(xintercept = median),
      linetype = "dotdash",
      linewidth = 1
    ) +
    ggplot2::facet_wrap(. ~ Param, scales = "free") +
    ggplot2::theme(aspect.ratio = 3 / 4) +
    ggplot2::labs(x = "Parameter value",
                  y = "Density")
}


grouped_violin_param <- function(data) {
  params <- list()
  
  for (i in 1:length(data)) {
    params[[i]] <-
      as.data.frame(data[[i]]$param) %>% tidyr::gather(key = "Param", value = "Value") %>%
      dplyr::mutate(Model = names(data)[i])
  }
  
  plot_data <- dplyr::bind_rows(params)
  
  plot_data <- plot_data %>% dplyr::group_by(Param, Model) %>%
    dplyr::mutate(Bin = cut(Value, breaks = 30, labels = FALSE)) %>%
    dplyr::ungroup()
  
  ggplot2::ggplot(plot_data) +
    see::geom_violinhalf(ggplot2::aes(x = Model, y = Value, fill = Model),
                         scale = "width") +
    ggplot2::facet_wrap(. ~ Param, nrow = 1, scales = "free") +
    ggplot2::theme(aspect.ratio = 2 / 1) +
    ggplot2::labs(x = "Parameter value",
                  y = "Half violin")
}


grouped_density_param <-
  function(data, stat = "density", color = "C") {
    if (stat != "density" & stat != "binline") {
      stop("Stat must be either 'density' or 'binline'")
    }
    
    params <- list()
    
    for (i in 1:length(data)) {
      params[[i]] <-
        as.data.frame(data[[i]]$param) %>% tidyr::gather(key = "Param", value = "Value") %>%
        dplyr::mutate(Model = names(data)[i]) %>% 
        dplyr::mutate(Type = if_else(grepl("\\d", names(data)[i]), "HR", "NR"))
    }
    
    plot_data <- dplyr::bind_rows(params)
    
    plot_data$Model <- toupper(plot_data$Model)
    
    levels(plot_data$Model) <- c("PD", "ED", "NND")
    
    plots <- list()
    
    if (stat == "density") {
      plots <- plot_data %>% dplyr::group_split(Param) %>% purrr::map(
        ~ ggplot2::ggplot(.) +
          ggridges::geom_density_ridges_gradient(
            ggplot2::aes(
              x = Value,
              y = Model,
              group = Model,
              fill = Type
            ),
            quantile_lines = TRUE
          ) +
          scale_fill_manual(values = c("HR" = "#AEC7E8",
                                "NR" = "#FFBB78")) +
          ggplot2::theme(aspect.ratio = 3 / 4,
                         legend.position = "none") +
          ggplot2::labs(x = NULL,
                        y = NULL) +
          ggplot2::ggtitle(parameter_to_expression(.$Param[1]))
      )
    } else {
      plots <- plot_data %>% dplyr::group_split(Param) %>% purrr::map(
        ~ ggplot2::ggplot(.) +
          ggridges::geom_density_ridges_gradient(
            ggplot2::aes(
              x = Value,
              y = Model,
              group = Model,
              fill = Type
            ),
            stat = stat,
            bins = 40
          ) +
          #viridis::scale_fill_viridis(option = color) +
          scale_fill_manual(values = c("HR" = "#AEC7E8",
                                       "NR" = "#FFBB78")) +
          ggplot2::theme(aspect.ratio = 3 / 4,
                         legend.position = "none") +
          ggplot2::labs(x = NULL,
                        y = NULL) +
          ggplot2::ggtitle(parameter_to_expression(.$Param[1]))
      )
    }
    
    return(
      patchwork::wrap_plots(plots) +
        patchwork::plot_annotation(
          title = 'Parameter posteriors',
          caption = 'Parameter value',
          theme = theme(
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)
          )
        )
    )
  }


hist_stats <- function(data, target, model = NULL) {
  plot_data <-
    as.data.frame(data$stats) %>% tidyr::gather(key = "Stats", value = "Value")
  
  stats_target <-
    tidyr::gather(data.frame(as.list(target)), key = "Stats", value = "Value")
  
  
  stats_median <-
    plot_data %>% dplyr::group_by(Stats) %>% dplyr::summarize(median = median(Value),
                                                              height = max(hist(
                                                                Value, breaks = 30, plot = FALSE
                                                              )$counts) / 2)
  
  stats_median <-
    suppressMessages(left_join(
      stats_median,
      tibble::enframe(target, name = "Stats", value = "target")
    ))
  
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_histogram(
      ggplot2::aes(Value, fill = Stats, alpha = ..count..),
      color = "black",
      bins = 30,
      right = TRUE
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
      arrow = arrow(
        type = "closed",
        length = unit(0.05, "inches"),
        ends = "both"
      )
    ) +
    ggplot2::theme(legend.position = "none", aspect.ratio = 4 / 5) +
    ggplot2::labs(x = "Parameter value",
                  y = "Frequency") +
    ggplot2::ggtitle(bquote("Precision estimates " ~ .(paste0("(",model,") ")) ~ epsilon ~ "=" ~  .(data$epsilon)))
}