#!/usr/bin/env Rscript

pkgs <- c(
    "tidyverse",
    "egg",
    "ggpubr",
    "gridExtra",
    "plyr",
    "cli",
    "zoo",
    "RColorBrewer",
    "tikzDevice",
    "cumstats",
    "ggh4x",
    "psych",
    "mnormt",
    "cli"
)

need <- setdiff(pkgs, rownames(installed.packages()))
if (length(need) > 0) {
    install.packages(need)
}

