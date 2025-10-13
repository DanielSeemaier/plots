if (TEX) {
    # In TeX mode, you can use LaTeX commands to name your algorithms.
    # For instance, you could decide to use \textsc{} to typeset the names of the algorithms.
    kaminpar_fm_name <- "\\textsc{KaMinPar-FM}"
    mtmetis_name <- "\\textsc{MtMetis}"
    kaminpar_timings_name <- "\\textsc{KaMinPar}"
} else {
    kaminpar_fm_name <- "KaMinPar-FM"
    mtmetis_name <- "MtMetis"
    kaminpar_timings_name <- "KaMinPar"
}

results <- "data/"

kaminpar_fm <- load_data(kaminpar_fm_name, paste0(results, "KaMinPar-FM.csv")) %>%
    dplyr::filter(K %in% c(8, 37, 64, 91, 128))
mtmetis <- load_data(mtmetis_name, paste0(results, "MtMetis.csv")) %>%
    dplyr::filter(K %in% c(8, 37, 64, 91, 128))

# This step is optional: for the final thesis / paper, it is generally a good idea to 
# fix the colors of the algorithms to ensure consistency across all figures. Thus, we 
# define a "colors" array here, which we can then use in the plotting functions.
set1 <- brewer.pal(n = 9, name = "Set1")

colors <- c()
colors[kaminpar_fm_name] = set1[[1]]
colors[mtmetis_name] = set1[[2]]
