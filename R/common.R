options(show.error.locations = TRUE)
options(error = traceback)
options(tidyverse.quiet = TRUE)

library(tidyverse, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(tikzDevice, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(egg, warn.conflicts = FALSE)
library(zoo, warn.conflicts = FALSE)
library(psych, warn.conflicts = FALSE)
library(cli, warn.conflicts = FALSE)

TEX_MACROS <- "/tex/preamble.tex"
TEX_OUTPUT <- "/tikz/"
PDF_OUTPUT <- "/pdf/"

PDF_LABEL_TIMEOUT <- "T"
PDF_LABEL_IMBALANCED <- "I"
PDF_LABEL_FAILED <- "F"

# Define din TEX_MACROS
TEX_LABEL_TIMEOUT <- "\\SymbTimeout"
TEX_LABEL_IMBALANCED <- "\\SymbImbalanced"
TEX_LABEL_FAILED <- "\\SymbFailed"

default_aggregator <- \(df) data.frame(
    # ... add custom columns that you want to aggregate here ...
    MinRealCut = ifelse(all(is.na(df$RealCut)), NA, min(df$RealCut, na.rm = TRUE)),
    AvgRealCut = ifelse(all(is.na(df$RealCut)), NA, mean(df$RealCut, na.rm = TRUE)),
    MaxRealCut = ifelse(all(is.na(df$RealCut)), NA, max(df$RealCut, na.rm = TRUE)),
    MinCut = ifelse(all(is.na(df$Cut)), NA, min(df$Cut, na.rm = TRUE)),
    AvgCut = ifelse(all(is.na(df$Cut)), NA, mean(df$Cut, na.rm = TRUE)),
    MaxCut = ifelse(all(is.na(df$Cut)), NA, max(df$Cut, na.rm = TRUE)),
    MinImbalance = ifelse(all(is.na(df$Imbalance)), NA, min(df$Imbalance, na.rm = TRUE)),
    AvgImbalance = ifelse(all(is.na(df$Imbalance)), NA, mean(df$Imbalance, na.rm = TRUE)),
    MaxImbalance = ifelse(all(is.na(df$Imbalance)), NA, max(df$Imbalance, na.rm = TRUE)),
    MinTime = ifelse(all(is.na(df$Time)), NA, min(df$Time, na.rm = TRUE)),
    AvgTime = ifelse(all(is.na(df$Time)), NA, mean(df$Time, na.rm = TRUE)),
    MaxTime = ifelse(all(is.na(df$Time)), NA, max(df$Time, na.rm = TRUE)),
    Timeout = all(df$Timeout),
    Failed = all(df$Failed)
)

aggregate_data <- \(df, timelimit, aggregator) df %>% 
    ##############################################################
    # Step 1: Pre-aggregation -- Invalidate individual runs that 
    # crashed, timed out or violate the balance constraint
    ##############################################################
    # Set the cut to NA if the run timed out or crashed
    dplyr::mutate(Cut = ifelse(Timeout | Failed, NA, Cut)) %>%
    dplyr::mutate(Imbalance = ifelse(Timeout | Failed, NA, Imbalance)) %>%
    # Set the time to the timelimit if the run timed out
    dplyr::mutate(Time = ifelse(Timeout, timelimit, Time)) %>%
    # Set the time to NA if the run failed
    dplyr::mutate(Time = ifelse(Failed & !Timeout, NA, Time)) %>%
    ##############################################################
    # Step 2: Aggregation -- Use arithmetic means to aggregate 
    # multiple seeds of the same instance
    ##############################################################
    plyr::ddply(
        # These are the columns that we want to group by
        c("Algorithm", "Graph", "K", "Epsilon", "Threads"),
        # This is the function that we want to apply to each group
        aggregator
    )  %>%
    ##############################################################
    # Step 3: Post-aggregation -- Invalid the aggregated data if 
    # all repetitions of an instance failed
    ##############################################################
    # If all repetitions of a run failed, set the cut and time to Inf 
    dplyr::mutate(AvgCut = ifelse(is.na(AvgCut), Inf, AvgCut)) %>%
    dplyr::mutate(MinCut = ifelse(is.na(MinCut), Inf, MinCut)) %>%
    dplyr::mutate(AvgTime = ifelse(is.na(AvgTime), Inf, AvgTime)) %>%
    dplyr::mutate(MinTime = ifelse(is.na(MinTime), Inf, MinTime)) %>%
    # If all repetitions of a run failed to fulfill the balance constraint, mark it as Infeasible
    dplyr::mutate(Imbalanced = !Failed & !Timeout & MinImbalance > 0.03 + .Machine$double.eps) %>%
    # ... otherwise, mark it as feasible (the aggregated cut / time columns will exclude the infeasible repetitions)
    dplyr::mutate(Feasible = !Failed & !Timeout & !Imbalanced) %>%
    # If all repetitions crashed, ran out of time or produced an imbalanced cut, mark it as invalid
    dplyr::mutate(Invalid = Failed | Timeout | Imbalanced)

create_optional_columns <- \(df) df %>%
    { if (!"Timeout" %in% colnames(.)) dplyr::mutate(., Timeout = FALSE) else . } %>%
    { if (!"Failed" %in% colnames(.)) dplyr::mutate(., Failed = FALSE) else . } %>%
    { if (!"MaxRSS" %in% colnames(.)) dplyr::mutate(., MaxRSS = -1) else . } %>%
    { if (!"Epsilon" %in% colnames(.)) dplyr::mutate(., Epsilon = 0.03) else . } %>%
    { if (!"Threads" %in% colnames(.)) dplyr::mutate(., Threads = 1) else . } %>%
    { if (!"Seed" %in% colnames(.)) dplyr::mutate(., Seed = -1) else . }

normalize_graph_names <- \(df) df %>%
    dplyr::mutate(Graph = sub("\\.metis|\\.bgf|\\.mtx|\\.mtx.hgr|\\.hgr|\\.graph|\\.scotch", "", Graph))

load_data <- \(name, file, aggregator = default_aggregator, timelimit = 3600) read.csv(file) %>%
    normalize_graph_names() %>%
    create_optional_columns() %>%
    dplyr::mutate(Algorithm = name) %>%
    aggregate_data(timelimit, aggregator) %>% 
    dplyr::arrange(Graph, K)

load_mtkahypar_data <- \(file, aggregator = default_aggregator, timelimit = 3600) read.csv(file) %>%
    dplyr::rename(
        Algorithm = algorithm,
        Graph = graph,
        K = k,
        Seed = seed,
        Epsilon = epsilon,
        Threads = num_threads,
        Imbalance = imbalance,
        Time = totalPartitionTime,
        Cut = km1,
        Failed = failed,
        Timeout = timeout
    ) %>% dplyr::mutate(
        Failed = ifelse(Failed == "no", FALSE, TRUE),
        Timeout = ifelse(Timeout == "no", FALSE, TRUE)
    ) %>%
    normalize_graph_names() %>%
    create_optional_columns() %>%
    aggregate_data(timelimit, aggregator) %>%
    dplyr::arrange(Graph, K)

create_theme <- \(aspect_ratio = 2 / (1 + sqrt(5)) / 1.25) theme(
    aspect.ratio = aspect_ratio,
    legend.background = element_blank(),
    legend.title = element_text(hjust = 0.5),
    legend.box.spacing = unit(0.1, "cm"),
    legend.text = element_text(size = 8, color = "black"), 
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.major = element_line(linetype = "11", linewidth = 0.5, color = "grey"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.2, color = "black"),
    axis.title.y = element_text(size = 8, vjust = 1.5, color = "black"),
    axis.title.x = element_text(size = 8, vjust = 1.5, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black")
)

if (!exists("tikzDeviceLoaded")) {
    options(tikzLatexPackages = c(getOption('tikzLatexPackages'), paste0("\\input{", getwd(), "/", TEX_MACROS, "}")))
    options(tikzMetricsDictionary = ".tikzdevicecache.db")
    tikzDeviceLoaded <- T
}

current_device_file <- ""
current_device_file_is_tikz <- FALSE

open_pdf <- \(file, width = 6) {
    current_device_file <<- paste0(PDF_OUTPUT, "/", file, ".pdf") 
    current_device_file_is_tikz <<- FALSE
    pdf(current_device_file, width = width)
}

open_tex <- \(file, width = 6) {
    current_device_file <<- paste0(TEX_OUTPUT, "/", file, ".tex")
    current_device_file_is_tikz <<- TRUE
    if (width == 7) {
        tikz(current_device_file, pointsize = 12, timestamp = FALSE)
    } else {
        tikz(current_device_file, width = width, pointsize = 12, timestamp = FALSE)
    }
}

dev_off <- \() {
    dev.off()
    if (current_device_file_is_tikz) {
        lines <- readLines(con = current_device_file)
        lines <- lines[-which(grepl("\\path\\[clip\\]*", lines, perl = F))]
        lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines, perl = F))]
        writeLines(lines, con = current_device_file)
    }
}

