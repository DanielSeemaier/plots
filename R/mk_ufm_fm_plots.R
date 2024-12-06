#!/usr/bin/env Rscript 
TEX <- TRUE

source("R/common.R")
source("R/instances.R")
source("R/throughput_plot.R")
source("R/performance_profile.R")

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

create_memory_plot <- function(
    baseline,
    via_heap = list(),
    via_rss = list(),
    primary_key = c("Graph", "K"),
    column.memory.heap = "PeakMemory",
    column.memory.rss = "MaxRSS",
    column.algorithm = "Algorithm",
    column.failed = "Failed",
    column.arrange_by = "M",
    column.keep = "KeepDots",
    color.baseline = "black",
    colors = c(),
    namer = identity,
    window_size = 50
) {
    baseline_data <- baseline %>% 
        dplyr::arrange_at(primary_key) %>%
        dplyr::select(
            Algorithm = rlang::sym(column.algorithm),
            Failed = rlang::sym(column.failed),
            HeapMemory = rlang::sym(column.memory.heap),
            RSSMemory = rlang::sym(column.memory.rss),
            M = rlang::sym(column.arrange_by),
            KeepDots = rlang::sym(column.keep)
        ) %>%
        dplyr::mutate(Algorithm = namer(Algorithm))

    prepare_data <- function(datasets, baseline_memory, key) {
        data <- data.frame()

        for (df in datasets) {
            stopifnot(df[, primary_key] == baseline[, primary_key])

            df <- df %>%
                dplyr::arrange_at(primary_key) %>%
                dplyr::select(
                    Algorithm = rlang::sym(column.algorithm),
                    Failed = rlang::sym(column.failed),
                    Memory = rlang::sym(key),
                    M = rlang::sym(column.arrange_by),
                    KeepDots = rlang::sym(column.keep)
                ) %>%
                dplyr::mutate(MemoryRatio = Memory / baseline_memory) %>%
                dplyr::arrange(M) %>%
                dplyr::mutate(Ith = 1:nrow(df)) %>%
                dplyr::mutate(Curve = rollapply(MemoryRatio, window_size, geometric.mean, partial = TRUE, align = "right")) %>%
                dplyr::mutate(MemoryRatio = ifelse(KeepDots, MemoryRatio, NA))

            data <- rbind(data, df)
        }

        return(data)
    }

    data <- rbind(
        prepare_data(via_heap, baseline_data$HeapMemory, column.memory.heap),
        prepare_data(via_rss, baseline_data$RSSMemory, column.memory.rss)
    )

    p <- ggplot(data, aes(x = M, y = MemoryRatio, color = Algorithm)) +
        geom_point(size = 0.4, alpha = 1 / 4) +
        geom_line(aes(y = Curve), linewidth = 1.0) +
        xlab("Number of Edges") +
        ylab("Rel. Peak Memory") +
        geom_hline(linewidth = 1.0, yintercept = 1, linetype = "solid", color = color.baseline) #+
        #scale_x_continuous(breaks = c(seq(0, nrow(baseline_data) - 50, by = 100), nrow(baseline_data)))

    if (length(colors) > 0) {
        p <- p + scale_color_manual(name = "Algorithm", values = colors)
    }

    return(p)
}

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

compute_relative_gmean_slowdown <- function(partial_x, complete_y) { 
    successful_x <- partial_x[partial_x$Failed == FALSE,]
    matching_y <- dplyr::semi_join(complete_y, successful_x, by = c('Graph', 'K'))

    gmean_x <- exp(mean(log(successful_x$AvgTime)))
    gmean_y <- exp(mean(log(matching_y$AvgTime)))

    return (gmean_x / gmean_y)
}

MAX_K <- 10000

terapart_fm_name <- paste0(terapart_name, "-FM")
terapart_lp_name <- paste0(terapart_name, "-LP")

relative_mtmetis_slowdown <- compute_relative_gmean_slowdown(mtmetis_ufm_bal, kaminpar_ufm)

terapart_ufm$Algorithm <- terapart_lp_name
terapart_ufm_hp$Algorithm <- terapart_lp_name

fm_ufm_compact$Algorithm <- terapart_fm_name
fm_ufm_compact_hp$Algorithm <- terapart_fm_name

colors_fm[terapart_fm_name] <- colors_fm[fm_compact_name]
colors_fm[terapart_lp_name] <- colors_fm[terapart_name]

terapart_ufm <- terapart_ufm %>% dplyr::filter(K <= MAX_K)
terapart_ufm_hp <- terapart_ufm_hp %>% dplyr::filter(K <= MAX_K)
fm_ufm_compact_hp <- fm_ufm_compact_hp %>% dplyr::filter(K <= MAX_K)
fm_ufm_compact <- fm_ufm_compact %>% dplyr::filter(K <= MAX_K)
fm_ufm_dense_hp <- fm_ufm_dense_hp %>% dplyr::filter(K <= MAX_K)
fm_ufm_dense <- fm_ufm_dense %>% dplyr::filter(K <= MAX_K)
fm_ufm_on_the_fly_hp <- fm_ufm_on_the_fly_hp %>% dplyr::filter(K <= MAX_K)
fm_ufm_on_the_fly <- fm_ufm_on_the_fly %>% dplyr::filter(K <= MAX_K)

mtmetis_ufm_bal <- mtmetis_ufm_bal %>% 
    dplyr::mutate(Infeasible = FALSE) %>%
    dplyr::filter(K <= MAX_K)

set_failed_memory <- function(df, max_mib) {
    cat(paste0("Number of failed ", df$Algorithm[1], " runs: ", nrow(df %>% dplyr::filter(Failed)), "\n"))
    cat(paste0("Replacing memory usage to ", max_mib / 1024 / 1024, " TiB\n"))
    df %>% dplyr::mutate(
        PeakMemory = ifelse(Failed, max_mib, PeakMemory),
        MaxRSS = ifelse(Failed, max_mib * 1024, MaxRSS)
    )
}

max_memory_mib <- 24 * 64 * 1024
fm_ufm_dense_hp <- set_failed_memory(fm_ufm_dense_hp, max_memory_mib)
mtmetis_ufm_bal <- set_failed_memory(mtmetis_ufm_bal, max_memory_mib)

set_failed_time <- function(df, max_s) {
    cat(paste0("Number of failed ", df$Algorithm[1], " runs: ", nrow(df %>% dplyr::filter(Failed)), "\n"))
    cat(paste0("Replacing avg time with ", max_s / 60, " min\n"))
    df %>% dplyr::mutate(
        AvgTime = ifelse(Failed, max_s, AvgTime)
    )
}

fm_ufm_on_the_fly_hp <- fm_ufm_on_the_fly_hp %>%
    dplyr::mutate(Failed = Failed | AvgTime > 1000)
fm_ufm_on_the_fly <- fm_ufm_on_the_fly %>%
    dplyr::mutate(Failed = Failed | AvgTime > 1000)
fm_ufm_dense_hp <- set_failed_time(fm_ufm_dense_hp, Inf)
fm_ufm_dense <- set_failed_time(fm_ufm_dense, Inf)
mtmetis_ufm_bal <- set_failed_time(mtmetis_ufm_bal, Inf)
fm_ufm_on_the_fly_hp <- set_failed_time(fm_ufm_on_the_fly_hp, Inf)
fm_ufm_on_the_fly <- set_failed_time(fm_ufm_on_the_fly, Inf)

prepare_mem <- function(df, keep) {
    df <- statify(df)
    df$KeepDots <- keep
    df$M <- df$M / 2
    df
}

time_plot <- create_slowdown_plot(
        mtmetis_ufm_bal,
        fm_ufm_compact,
        fm_ufm_dense,
        fm_ufm_on_the_fly,
        baseline = terapart_ufm,
        color.baseline = colors_fm[terapart_name],
        factor.levels = c(
            fm_ufm_dense$Algorithm[1],
            fm_ufm_compact$Algorithm[1],
            fm_ufm_on_the_fly$Algorithm[1],
            mtmetis_ufm_bal$Algorithm[1]
        ), 
        colors = colors_fm
    )+
    theme_bw() +
    create_theme() +
    scale_y_continuous(
        trans = "log10",
        breaks = c(1 / 10, 1.0, 10, 100, 1000),
        labels = c("$10^{-1}$", "$10^{0}$", "$10^{1}$", "$10^{2}$", "$10^{3}$"),
        limits = c(1 / 10, 1000)
    ) +
    coord_cartesian(ylim = c(1 / 10, 1000)) +
    theme(
        plot.margin = margin(0.0, 0.05, 0.0, 0.05, "cm"),
        legend.position = "none"
    )

memory_plot <- create_memory_plot(
        via_heap = list(
            prepare_mem(fm_ufm_dense_hp, FALSE), 
            #prepare_mem(terapart_ufm_hp, FALSE),
            prepare_mem(fm_ufm_compact_hp, TRUE)
        ),
        via_rss = list(prepare_mem(mtmetis_ufm_bal, FALSE)),
        #baseline = prepare_mem(fm_ufm_dense_hp, FALSE),
        baseline = prepare_mem(terapart_ufm_hp, FALSE),
        colors = colors_fm,
        color.baseline = colors_fm[terapart_name]
        #color.baseline = colors_fm[fm_dense_name]
    ) +
    theme_bw() +
    create_theme() +
    theme(
        legend.position = "none"
    ) +
    scale_y_continuous(
        trans = "log2",
        breaks = c(1, 2, 4, 8, 16),
        labels = c("$2^{0}$", "$2^{1}$", "$2^{2}$", "$2^{3}$", "$2^{4}$"),
        #limits = c(1 / 128, 4.0)
    ) +
    scale_x_continuous(
        trans = "log2",
        breaks = c(2^23, 2^25, 2^27, 2^29, 2^31),
        labels = c("$2^{23}$", "$2^{25}$", "$2^{27}$", "$2^{29}$", "$2^{31}$")
    ) #+
    ##expand_limits(y = c(0.0, 16.0)) 

performance_plot <- create_performance_profile(
        terapart_ufm, fm_ufm_compact_hp, mtmetis_ufm_bal,
        # fm_ufm_dense_hp, fm_ufm_on_the_fly_hp,
        tex = TEX,
        tiny = TRUE,
        colors = colors_fm,
        segments = list(
            list(
                trans = "identity", 
                to = 1.1, 
                width = 3,
                breaks = seq(1.0, 1.1, by = 0.01),
                labels = c("1.0", "", "", "", "", "1.05", "", "", "", "", "1.1")
            ),
            list(
                trans = "identity", 
                to = 2, 
                width = 2,
                breaks = c(1.25, 1.5, 1.75, 2),
                labels = c("", "1.5", "", "2")
            ),
            list(trans = "log10", 
                to = 100,
                width = 1,
                breaks = c(10, 100),
                labels = c("", "10^2")
            )
        )
    ) +
    theme_bw() +
    create_theme() +
    theme(
        legend.position = "none"
    )

open_dev("ufm_fm", width = 7, tex = TEX)
egg::ggarrange(
    time_plot, memory_plot, performance_plot,
    nrow = 1,
    padding = unit(0.0, "line")
)
dev_off()

dummy_legend_plot <- create_dummy_plot(
        terapart_ufm, fm_ufm_on_the_fly_hp, fm_ufm_compact_hp, mtmetis_ufm_bal, fm_ufm_dense_hp,
        colors = colors_fm,
        levels = c(terapart_lp_name, fm_on_the_fly_name, fm_dense_name, terapart_fm_name, mtmetis_name)
    ) +
    create_theme() +
    theme_bw() +
    theme(
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.spacing.x = unit(0.0, "line"),
    )# +
    #guides(color = guide_legend(ncol = 3, bycol = TRUE))

legend <- ggpubr::get_legend(dummy_legend_plot, position = "bottom")
open_dev("ufm_fm_legend", width = 3.5, tex = TEX)
egg::ggarrange(
    ggpubr::as_ggplot(legend),
    heights = 1,
    padding = unit(0.0, "line")
)
dev_off()
