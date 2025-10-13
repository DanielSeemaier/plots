#!/usr/bin/env Rscript
TEX <- FALSE

source("R/common.R")
source("R/performance_profile.R")
source("R/running_time_boxplot.R")

source("instances.R")

example_performance_plot <- create_performance_profile(
        kaminpar_fm, mtmetis,
        # ... add more datasets here ...
        tex = TEX,
        tiny = FALSE,
        # Remove the next line if you do not want to use custom colors
        colors = colors
    ) +
    ggplot2::theme_bw() +
    create_theme() +
    ggplot2::xlab("Ratio") +
    ggplot2::ylab("Fraction of Instances") +
    ggplot2::theme(
        legend.position = "bottom"
    )

example_running_time_plot <- create_running_time_boxplot(
        kaminpar_fm, mtmetis,
        # ... add more datasets here ...
        tex = TEX,
        tiny = FALSE,
        # Remove the next line if you do not want to use custom colors
        colors = colors
    ) +
    ggplot2::theme_bw() +
    create_theme() +
    ggplot2::xlab("Algorithm") +
    ggplot2::ylab("Running Time [s]") +
    ggplot2::theme(
        legend.position = "none"
    )

open_dev("examples", tex = TEX)
print(example_performance_plot)
print(example_running_time_plot)
dev_off()
