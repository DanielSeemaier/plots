create_running_time_breakdown_plot <- function(
    data,
    cols,
    column.graph = "Graph",
    column.total_time = "AvgTime",
    column.remaining_time = "AvgTimeRemaining"
) {
    if (is.null(column.total_time)) {
        column.total_time <- ".AvgTime"
        data <- data %>% dplyr::mutate(!!column.total_time := rowSums(dplyr::accross(all_of(cols))))
    }

    data <- data %>% dplyr::mutate(
        !!column.remaining_time := (!!rlang::sym(column.total_time) - rowSums(dplyr::across(all_of(cols)))) / 
            !!rlang::sym(column.total_time)
    )

    for (col in cols) {
        data <- data %>% dplyr::mutate(!!col := !!rlang::sym(col) / !!rlang::sym(column.total_time))
    }

    data <- data %>%
        dplyr::select(all_of(c(column.graph, column.remaining_time, cols))) %>%
        tidyr::pivot_longer(
            cols = all_of(c(column.remaining_time, cols)),
            names_to = "Metric",
            values_to = "Value"
        )

    ggplot2::ggplot(data, ggplot2::aes(x = Graph, y = Value, fill = Metric)) +
        ggplot2::geom_col(width = 0.9, position = "stack") +
        #ggplot2::scale_y_continuous(
            #expand = c(0, 0),
            #breaks = seq(0, 1, by = 0.1)
            #labels = c("0.0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1.0")
        #) +
        ggplot2::labs(
            x = "Graph",
            y = "Time Fraction",
            fill = NULL
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            legend.position = "bottom",
            legend.title = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank()
        )
}
