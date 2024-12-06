#!/usr/bin/env Rscript
TEX <- TRUE

source("R/common.R")
source("R/performance_profile.R")
source("instances.R")

example_performance_plot <- create_performance_profile(
        kaminpar_fm, mtmetis,
        # ... add more datasets here ...
        tex = TEX,
        tiny = TRUE,
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

open_dev("performance_profile.example", width = 4.5, tex = TEX)
print(example_performance_plot)
dev_off()
