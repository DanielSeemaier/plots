create_slowdown_plot <- function(
    ...,
    baseline,
    primary_key = c("Graph", "K"),
    column.time = "AvgTime",
    column.timeout = "Timeout",
    column.algorithm = "Algorithm",
    column.failed = "Failed",
    colors = c(),
    factor.levels = c(),
    color.baseline = "black",
    namer = identity
) {
    all_datasets <- list(...)
    stopifnot(length(all_datasets) > 0)

    baseline <- baseline %>% dplyr::arrange_at(primary_key)
    for (i in 1:length(all_datasets)) {
        all_datasets[[i]] <- all_datasets[[i]] %>% dplyr::arrange_at(primary_key)
    }

    # Check for consistent data
    for (dataset in all_datasets) {
        stopifnot(column.time %in% colnames(dataset))
        stopifnot(column.algorithm %in% colnames(dataset))
        stopifnot(column.timeout %in% colnames(dataset))
        stopifnot(column.failed %in% colnames(dataset))

        stopifnot(!(NA %in% dataset[[column.time]]))
        stopifnot(!(NaN %in% dataset[[column.time]]))
        stopifnot(!(-Inf %in% dataset[[column.time]]))
        stopifnot(!(0 %in% dataset[[column.time]]))
        stopifnot(!any(dataset[[column.time]] <= 0))

        stopifnot(dataset[, primary_key] == baseline[, primary_key])
    }

    baseline_data <- baseline %>% 
        dplyr::select(
            Algorithm = rlang::sym(column.algorithm),
            Time = rlang::sym(column.time),
            Timeout = rlang::sym(column.timeout)
        ) %>%
        dplyr::mutate(Algorithm = namer(Algorithm))

    data <- data.frame()

    for (i in 1:length(all_datasets)) {
        df <- all_datasets[[i]]

        df <- df %>% 
            dplyr::select(
                Algorithm = rlang::sym(column.algorithm),
                Time = rlang::sym(column.time),
                Timeout = rlang::sym(column.timeout),
                Failed
            ) %>%
            dplyr::mutate(TimeRatio = Time / baseline_data$Time) %>%
            dplyr::arrange(TimeRatio) %>%
            dplyr::group_by(Failed) %>%
            dplyr::filter(row_number() == 1 | !Failed) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Ith = 1:n())

        data <- rbind(data, df)
    }

    #data %>% dplyr::filter(Failed) %>% print()
    data <- data %>% dplyr::mutate(TimeRatio = ifelse(TimeRatio > 10^3, Inf, TimeRatio))

    #data %>% dplyr::filter(TimeRatio > 10^3) %>% print(n = Inf)

    if (length(factor.levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = factor.levels)
    }

    p <- ggplot(data, aes(x = Ith, y = TimeRatio, color = Algorithm)) +
        geom_line(linewidth = 1.0) +
        xlab("Number of Instances") +
        ylab("Rel. Slowdown") +
        geom_hline(linewidth = 1.0, yintercept = 1, linetype = "solid", color = color.baseline) +
        #scale_x_continuous(breaks = c(seq(0, nrow(baseline_data) - 50, by = 100), nrow(baseline_data)))
        scale_x_continuous(breaks = c(seq(0, nrow(baseline_data), by = 100)))

    if (length(colors) > 0) {
        p <- p + scale_color_manual(name = "Algorithm", values = colors)
    }

    return(p)
}

