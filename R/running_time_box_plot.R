#!/usr/bin/env Rscript
show_timeout <- \(df, option) option == "always" || (option == "auto" && any(df$Timeout))
show_imbalanced <- \(df, option) option == "always" || (option == "auto" && any(df$Imbalanced)) 
show_failed <- \(df, option) option == "always" || (option == "auto" && any(df$Failed))

create_running_time_box_plot <- function(
    ...,
    column.time = "AvgTime", 
    column.algorithm = "Algorithm", 
    column.timeout = "Timeout",
    column.imbalanced = "Imbalanced",
    column.failed = "Failed",
    primary_key = c("Graph", "K"),
    tick.timeout = "auto", # always, auto, never
    tick.imbalanced = "auto", # always, auto, never
    tick.failed = "auto", # always, auto, never
    tick.errors.space_below = 0.8,
    tick.errors.space_between = 0.8,
    tex = FALSE,
    pdf.label.timeout = PDF_LABEL_TIMEOUT,
    pdf.label.imbalanced = PDF_LABEL_IMBALANCED,
    pdf.label.failed = PDF_LABEL_FAILED,
    tex.label.timeout = TEX_LABEL_TIMEOUT,
    tex.label.imbalanced = TEX_LABEL_IMBALANCED,
    tex.label.failed = TEX_LABEL_FAILED,
    colors = c(),
    levels = c(),
    position.y = "left"
) {
    all_dfs <- list(...)
    if (length(all_dfs) == 0) {
        cli::cli_abort("Need at least one data frame for plotting running times.")
    }

    for (i in 1:length(all_dfs)) {
        # Sort the data; replace 0 by 1 to ensure that zero-cuts do not crash our code
        df <- all_dfs[[i]] %>% dplyr::arrange_at(primary_key) 
        all_dfs[[i]] <- df

        # Sanity checks: crash early with helpful error messages
        if (!(column.algorithm %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.algorithm}} missing from data frame no. {.val {i}}.")
            quit()
        }

        algorithms <- df %>% dplyr::pull(rlang::sym(column.algorithm))
        algorithm <- algorithms[1]
        if (!all(algorithms == algorithm)) {
            cli::cli_abort("Rows for multiple algorithms in the same data frame no. {.val {i}}: {.val {unique(algorithms)}}")
        }

        if (!(column.time %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.time}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!(column.timeout %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.timeout}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!(column.imbalanced %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.imbalanced}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!column.failed %in% colnames(df)) {
            cli::cli_abort("Column {.field {column.failed}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }

        for (illegal_value in list(NaN, NA, -Inf, 0)) {
            if (illegal_value %in% df[[column.time]]) {
                cli::cli_abort("Column {.field {column.time}} of data frame no. {.val {i}} (algorithm {.val {algorithm}}) contains illegal value {.val {illegal_value}}.")
                quit()
            }
        }

        if (nrow(df[, primary_key]) != nrow(all_dfs[[1]][, primary_key])) {
            cli::cli_abort("Number of rows for the primary keys in data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match the number of rows for the primary keys in the first data frame.")
            quit()
        }
        if (!all.equal(df[, primary_key], all_dfs[[1]][, primary_key])) {
            cli::cli_abort("Primary keys of data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match for all rows with the primary keys of the first data frame.")
            quit()
        }
    }

    data <- purrr::map_dfr(all_dfs, \(df) df %>% 
        dplyr::select(
            Algorithm = rlang::sym(column.algorithm),
            Time = rlang::sym(column.time),
            Timeout = rlang::sym(column.timeout),
            Imbalanced = rlang::sym(column.imbalanced),
            Failed = rlang::sym(column.failed)
        ) %>% 
        dplyr::mutate(
            PK = dplyr::row_number(),
            JitterTime = Time
        )
    )

    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels, ordered = TRUE)
    }

    # Find max time
    min_max_time <- data %>% 
        dplyr::filter(!Timeout & !Imbalanced & !Failed) %>%
        dplyr::summarize(Max = max(Time), Min = min(Time))
    max_time_log10 <- ceiling(log10(min_max_time$Max))
    min_time_log10 <- -1
    max_time_exp10 <- 10 ^ max_time_log10
    min_time_exp10 <- 10 ^ min_time_log10

    # Create ticks 
    y_breaks <- 10 ^ seq(min_time_log10, max_time_log10, by = 1)
    if (tex) {
        y_labels <- sapply(y_breaks, \(val) paste0("$10^{", log10(val), "}$"))
    } else {
        y_labels <- sapply(y_breaks, \(val) paste0("1e", log10(val)))
    }

    # Remap imbalanced solutions, timeouts and failed runs
    label.imbalanced <- ifelse(tex, tex.label.imbalanced, pdf.label.imbalanced)
    label.timeout <- ifelse(tex, tex.label.timeout, pdf.label.timeout)
    label.failed <- ifelse(tex, tex.label.failed, pdf.label.failed)

    show_imbalanced_tick <- show_imbalanced(data, tick.imbalanced)
    show_timeout_tick <- show_timeout(data, tick.timeout)
    show_failed_tick <- show_failed(data, tick.failed)
    show_error_ticks <- show_imbalanced_tick || show_timeout_tick || show_failed_tick 

    offset <- tick.errors.space_below - tick.errors.space_below
    if (show_imbalanced(data, tick.imbalanced)) {
        offset <- offset + tick.errors.space_between
        y_breaks <- c(y_breaks, 10 ^ (max_time_log10 + offset))
        y_labels <- c(y_labels, label.imbalanced)
    }

    data <- data %>% dplyr::mutate(
        JitterTime = ifelse(Imbalanced & !Timeout, 10 ^ (max_time_log10 + offset), JitterTime),
        Time = ifelse(Imbalanced & !Timeout, NA, Time)
    )

    if (show_timeout(data, tick.timeout)) {
        offset <- offset + tick.errors.space_between
        y_breaks <- c(y_breaks, 10 ^ (max_time_log10 + offset))
        y_labels <- c(y_labels, label.timeout)
    }

    data <- data %>% dplyr::mutate(
        JitterTime = ifelse(Timeout, 10 ^ (max_time_log10 + offset), JitterTime)
    )

    if (show_failed(data, tick.failed)) {
        offset <- offset + tick.errors.space_between
        y_breaks <- c(y_breaks, 10 ^ (max_time_log10 + offset))
        y_labels <- c(y_labels, label.failed)
    }

    data <- data %>% dplyr::mutate(
        JitterTime = ifelse(Failed & !Timeout & !Imbalanced, 10 ^ (max_time_log10 + offset), JitterTime),
        Time = ifelse(Failed & !Timeout & !Imbalanced, NA, Time)
    )

    # Compute stats across instances for which all Algorithms produced a meaningful result
    data <- data %>%
        dplyr::group_by(PK) %>%
        dplyr::mutate(.AllOk = all(!is.na(Time))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ComparableTime = ifelse(.AllOk, Time, NA)) %>%
        dplyr::select(-.AllOk)
    annotation <- data %>%
        dplyr::group_by(Algorithm) %>%
        dplyr::summarize(
            Num = dplyr::n(),
            NumFeasibles = sum(!is.na(Time)),
            NumCommon = sum(!is.na(ComparableTime)),
            GmeanFeasibles = exp(mean(log(Time), na.rm = TRUE)),
            GmeanCommon = exp(mean(log(ComparableTime), na.rm = TRUE)), 
            .groups = "drop"
        )

    p <- ggplot2::ggplot(data, ggplot2::aes(x = Algorithm, y = JitterTime)) +
        ggplot2::geom_jitter(
            ggplot2::aes(color = Algorithm, fill = Algorithm), 
            size = 0.75, 
            alpha = 0.33, 
            pch = 21, 
            width = 0.3
        ) +
        ggplot2::stat_boxplot(
            ggplot2::aes(y = ComparableTime, color = Algorithm), 
            geom = "errorbar", 
            width = 0.6, 
            na.rm = TRUE
        ) +
        ggplot2::geom_boxplot(
            ggplot2::aes(y = ComparableTime, color = Algorithm), 
            outlier.shape = NA, 
            alpha = 0.5, 
            na.rm = TRUE
        ) +
        ggplot2::scale_y_continuous(
            trans = "log10", 
            breaks = y_breaks, 
            labels = y_labels, 
            position = position.y
        ) +
        ggplot2::geom_text(
            ggplot2::aes(
                x = Algorithm, 
                y = min(data$JitterTime),
                label = sprintf(
                    "Cmp: %.2fs (%d), All: %.2fs (%d)",
                    GmeanCommon, 
                    NumCommon,
                    GmeanFeasibles,
                    NumFeasibles
                ),
                vjust = 1.5
            ), 
            annotation, 
            size = 2.5
        )

    if (show_error_ticks) {
        p <- p + ggplot2::geom_hline(yintercept = 10 ^ (max_time_log10 + tick.errors.space_below / 2))
    }

    # Set colors
    if (length(colors) > 0) {
        p <- p + 
            ggplot2::scale_color_manual(name = "Algorithm", values = colors) +
            ggplot2::scale_fill_manual(name = "Algorithm", values = colors)
    }

    p
}

