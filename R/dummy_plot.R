#!/usr/bin/env Rscript

create_dummy_plot <- function(..., colors = c(), levels = c()) {
    data <- do.call(rbind, lapply(list(...), \(df) df["Algorithm"]))
    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels)
    }

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = 0, y = 1, color = Algorithm)) +
        ggplot2::geom_line(linewidth = 1.0) +
        ggplot2::theme(legend.position = "bottom")

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(name = "Algorithm", values = colors)
    }

    return(plot)
}
