#!/usr/bin/env Rscript
TEX <- FALSE

source("R/common.R")
source("R/running_time_breakdown_plot.R")

aggreate_running_times <- function(df) {
    df %>% default_aggregator() %>% dplyr::mutate(
        AvgCoarseningTime = mean(df$TimeCoarsening),
        AvgInitialPartitioningTime = mean(df$TimeInitialPartitioning),
        AvgUncoarseningTime = mean(df$TimeUncoarsening),
        AvgRefinementTime = mean(df$TimeRefinement)
    )
}


kaminpar_timings <- load_data(
    name = "KaMinPar", 
    file = "data/KaMinPar-Timings.csv", 
    aggregator = aggreate_running_times
)
kaminpar_k16 <- kaminpar_timings %>% dplyr::filter(K == 16)

example_normalized_breakdown_plot <- create_running_time_breakdown_plot(
    data = kaminpar_k16,
    cols = c("AvgCoarseningTime", "AvgInitialPartitioningTime", "AvgUncoarseningTime", "AvgRefinementTime")
)

example_absolute_breakdown_plot <- create_running_time_breakdown_plot(
    data = kaminpar_k16,
    cols = c("AvgCoarseningTime", "AvgInitialPartitioningTime", "AvgUncoarseningTime", "AvgRefinementTime"),
    normalize = FALSE
)

open_dev("breakdown", tex = TEX)
print(example_normalized_breakdown_plot)
print(example_absolute_breakdown_plot)
dev_off()
