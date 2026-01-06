#!/usr/bin/env Rscript
TEX <- FALSE

source("R/common.R")
source("R/performance_profile_plot.R")
source("R/running_time_box_plot.R")
source("R/slowdown_plot.R")
source("R/speedup_plot.R")

source("instances.R")

always_best <- load_data("Always Best", "/data/tests/AlwaysBest.csv")
never_best <- load_data("Never Best", "/data/tests/NeverBest.csv")
sometimes_best_a <- load_data("Sometimes Best A", "/data/tests/SometimesBest-A.csv")
sometimes_best_b <- load_data("Sometimes Best B", "/data/tests/SometimesBest-B.csv")

always_imbalanced <- load_data("Always Imbalanced", "/data/tests/AlwaysImbalanced.csv")

pp_default <- create_performance_profile_plot(
        always_best,
        never_best,
        sometimes_best_a,
        sometimes_best_b
    ) +
    ggplot2::labs(title = "Performance Profile") +
    ggplot2::theme_bw() +
    create_theme() +
    ggplot2::xlab("Ratio") +
    ggplot2::ylab("Fraction of Instances") +
    ggplot2::theme(legend.position = "bottom")

pp_with_imbalanced <- create_performance_profile_plot(
        sometimes_best_a,
        sometimes_best_b,
        always_imbalanced
    ) +
    ggplot2::labs(title = "Performance Profile") +
    ggplot2::theme_bw() +
    create_theme() +
    ggplot2::xlab("Ratio") +
    ggplot2::ylab("Fraction of Instances") +
    ggplot2::theme(legend.position = "bottom")

open_pdf("tests")
print(pp_default)
print(pp_with_imbalanced)
dev_off()
