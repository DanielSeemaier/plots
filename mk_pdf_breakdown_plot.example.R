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

#print(colnames(kaminpar_timings))
#print(kaminpar_timings)
#quit()

example_breakdown_plot <- create_running_time_breakdown_plot(
    data = kaminpar_timings,
    cols = c("AvgCoarseningTime", "AvgInitialPartitioningTime", "AvgUncoarseningTime", "AvgRefinementTime")
)

open_dev("breakdown", tex = TEX)
print(example_breakdown_plot)
dev_off()
