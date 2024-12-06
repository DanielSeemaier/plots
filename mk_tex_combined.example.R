#!/usr/bin/env Rscript
TEX <- TRUE

source("R/common.R")
source("R/performance_profile.R")
source("R/slowdown_plot.R")
source("R/dummy_plot.R")
source("instances.R")

performance_plot <- create_performance_profile(
        kaminpar_fm, mtmetis,
        tex = TEX,
        tiny = TRUE,
        colors = colors,
        # Use custom segments:
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
    ggplot2::ggtitle("b)") +
    ggplot2::theme_bw() +
    create_theme() +
    ggplot2::theme(
        legend.position = "none"
    )

time_plot <- create_slowdown_plot(
        mtmetis, 
        # ... add more datasets here ...
        baseline = kaminpar_fm,
        color.baseline = colors[kaminpar_fm_name],
        # Control the order in which the curves will be plotted
        # (useful if curves overlap and you want to control which one is on top)
        factor.levels = c(
            mtmetis$Algorithm[1],
            kaminpar_fm$Algorithm[1]
        ), 
        colors = colors
    )+
    ggplot2::ggtitle("a)") +
    ggplot2::theme_bw() +
    create_theme() +
    ggplot2::scale_y_continuous(
        trans = "log10",
        breaks = c(1 / 10, 1.0, 10, 100, 1000),
        labels = c("$10^{-1}$", "$10^{0}$", "$10^{1}$", "$10^{2}$", "$10^{3}$"),
        limits = c(1 / 10, 1000)
    ) +
    ggplot2::coord_cartesian(ylim = c(1 / 10, 1000)) +
    ggplot2::theme(
        plot.margin = margin(0.0, 0.05, 0.0, 0.05, "cm"),
        legend.position = "none"
    )

dummy_legend_plot <- create_dummy_plot(
        kaminpar_fm, mtmetis,
        colors = colors,
        # Control the order of the legend entries
        levels = c(kaminpar_fm_name, mtmetis_name)
    ) +
    create_theme() +
    ggplot2::theme_bw() +
    ggplot2::theme(
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.spacing.x = unit(0.0, "line"),
    )# +
    #guides(color = guide_legend(ncol = 3, bycol = TRUE))
    # ^ if you want legend entries in multiple rows ^


open_dev("combined_plot.example", width = 4.5, tex = TEX)
egg::ggarrange(
    time_plot, performance_plot,
    nrow = 1,
    padding = unit(0.0, "line")
)
dev_off()

legend <- ggpubr::get_legend(dummy_legend_plot, position = "bottom")
open_dev("combined_plot.legend.example", width = 4.5, tex = TEX)
egg::ggarrange(
    ggpubr::as_ggplot(legend),
    heights = 1,
    padding = unit(0.0, "line")
)
dev_off()

