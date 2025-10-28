#!/usr/bin/env Rscript
create_speedup_plot <- function(
    ...,
    baseline,
    colors,
    fails.show = TRUE,
    primary_key = c("Graph", "K"),
    column.time = "AvgTime",
    column.timeout = "Timeout",
    column.algorithm = "Algorithm",
    column.failed = "Failed",
    levels = c(),
    x_step = 100) {
    #
    all_dfs <- list(...)

    if (length(all_dfs) == 0) {
        cli::cli_abort("Need at least one data frames for plotting a speedup profile.")
        quit()
    }

    baseline <- baseline %>% dplyr::arrange_at(primary_key)
    all_dfs <- purrr::map(all_dfs, ~ .x %>% dplyr::arrange_at(primary_key))

    purrr::walk2(all_dfs, seq_along(all_dfs), function(df, i) {
        algorithms <- df %>% dplyr::pull(rlang::sym(column.algorithm))
        algorithm <- algorithms[1]

        if (nrow(df) != nrow(baseline)) {
            cli::cli_abort("Number of rows in data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match the number of rows in the first data frame.")
        }
        if (!all.equal(df[, primary_key], baseline[, primary_key])) {
            cli::cli_abort("Primary keys of data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match for all rows with the primary keys of the first data frame.")
        }
    })


    baseline <- baseline %>%
        dplyr::select(
            Algorithm = rlang::sym(column.algorithm),
            Time = rlang::sym(column.time),
            Timeout = rlang::sym(column.timeout)
        )
    baseline_color <- colors[[first(baseline$Algorithm)]]

    if (NA %in% baseline$Time || Inf %in% baseline$Time || 0 %in% baseline$Time) {
        cli::cli_abort("Baseline contains {.val NA}, {.val Inf} or {.val 0} values in column {.field {column.time}}, which is not allowed")
        quit()
    }

    fail_markers <- data.frame()

    data <- purrr::map_dfr(all_dfs, function(df) {
        df <- df %>%
            dplyr::select(
                Algorithm = rlang::sym(column.algorithm),
                Time = rlang::sym(column.time),
                Timeout = rlang::sym(column.timeout)
            ) %>%
            dplyr::mutate(TimeRatio = baseline$Time / Time) %>%
            dplyr::arrange(TimeRatio) %>%
            dplyr::mutate(Ith = dplyr::row_number()) %>%
            dplyr::filter(!is.na(TimeRatio))

        if (nrow(df) < nrow(baseline)) {
            fail_markers <<- fail_markers %>% dplyr::bind_rows(data.frame(
                Algorithm = dplyr::first(df$Algorithm),
                Ith = max(df$Ith),
                TimeRatio = min(df$TimeRatio)
            ))
        }

        df
    })

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = Ith, y = TimeRatio, color = Algorithm)) +
        ggplot2::geom_line(linewidth = 1.5) +
        ggplot2::xlab("Number of Instances") +
        ggplot2::ylab("Relative Speedup") +
        ggplot2::geom_hline(
            linewidth = 1.5,
            yintercept = 1,
            linetype = "solid",
            color = baseline_color
        ) +
        ggplot2::scale_x_continuous(
            breaks = c(seq(0, nrow(baseline), by = x_step))
        ) +
        ggplot2::scale_color_manual(
            name = "Algorithm",
            values = colors
        )

    if (fails.show && nrow(fail_markers) > 0) {
        plot <- plot + ggplot2::geom_point(
            data = fail_markers,
            shape = 4,
            size = 2,
            show.legend = FALSE
        )
    }

    return(plot)
}
