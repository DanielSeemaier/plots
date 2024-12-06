create_dummy_plot <- function(..., colors = c(), levels = c()) {
    datasets <- list(...)
    data <- data.frame()
    for (dataset in datasets) {
        dataset <- dataset %>% dplyr::mutate(Ith = 1:nrow(dataset))
        data <- rbind(data, dataset)
    }

    data$Algorithm <- factor(data$Algorithm, levels = levels)

    p <- ggplot(data, aes(x = Ith, y = AvgTime, color = Algorithm)) + 
        geom_line(linewidth = 1.0)
    if (length(colors) > 0) {
        p <- p + scale_color_manual(name = "Algorithm", values = colors)
    }

    return (p)
}

